## Libraries
library(DBI)          # Database Connection
library(tidyverse)    # Core Libraries - dplyr, ggplot2
library(dbplyr)       # Database backend for dplyr
library(knitr)        # Report Graphics
library(fuzzyjoin)
library(spatialsample)
library(cmap4r)
library(RPostgreSQL)
library(getPass)
library(tidyverse)
library(tidymodels)
library(lubridate)
library(h2o)
library(recipes)
library(readxl)
library(tidyquant)
library(stringr)
library(forcats)
library(cowplot)
library(fs)
library(glue)
#############################################################################################################
# The following set of function depends on the data acquired from CMAP using this code
# /home/alakob/Documents/skill_development/practices/machine_learning/R/marine/00_scripts/getCmap_data.R
#############################################################################################################

#####################################
#  Configs
#####################################
basepath <- "/home/alakob/Documents/skill_development/practices/data"
coord_info_path <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwin_coords_depth.rds"
darwinSyn_clim_path <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwinSyn_clim.rds"
darwinPro_clim_path <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwinPro_clim.rds"
darwinNO3_clim_path <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwinNO3_clim.rds"
darwinPO4_clim_path <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwinPO4_clim.rds"
darwinDOP_clim_path  <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwinDOP_clim.rds"
darwinFe_clim_path <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwinFe_clim.rds"
longhurst_path  <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/longhurst.rds"

########################################################
#      RETRIEVE DATA FROM BACKEND DATABASE
########################################################
pgdrv <- dbDriver(drvName = "PostgreSQL")

connection <-DBI::dbConnect(pgdrv,
                            dbname="marine_db",
                            host="localhost", port=5432,
                            user = 'postgres',
                            password = getPass("Enter PostgreSQL DB Password:"))


###########################################################
#   Extract primary tables
###########################################################
family_summary <- tbl(connection, "family_bracken_summary")
family_results <- tbl(connection, "family_bracken_results")

species_summary <- tbl(connection, "species_bracken_summary")
species_results <- tbl(connection, "species_bracken_results")

metadata <- tbl(connection, "experiment_metadata")
mapping <- tbl(connection, "id_mapping")
samplesheet <- tbl(connection, "samplesheet")
software_versions <- tbl(connection, "software_versions") %>%  collect()

########################################################
# Pull out the metadata
########################################################
dbget_metadata <- function(){
    meta_tbl <- metadata %>% 
        collect() %>%
        dplyr::select(library_id,
               latitude_decimal_deg_n,
               longitude_decimal_deg_w,
               iso_datetime_utc,
               depth_m,
               temperature_deg_c,
               phosphate_bottle_umol_l,
               nitrate_nitrite_bottle_umol_l,
               ctd_station_no,
               sample_method,
               platform_strategy,
               sra_accession_number) %>% 
        dplyr::rename(lat = latitude_decimal_deg_n,
               lon = longitude_decimal_deg_w,
               depth = depth_m,
               temperature =  temperature_deg_c,
               phosphate = phosphate_bottle_umol_l,
               nitrate_nitrite = nitrate_nitrite_bottle_umol_l ) %>% 
        tidyr::separate(iso_datetime_utc, 
                 into=c("collection_date", "rest"), 
                 sep="T",
                 remove = TRUE) %>%
        dplyr::mutate(collection_date = ymd(collection_date))
    
    return(meta_tbl)
    
}

################################
# Load CMAP data from fileSystem
################################
coord_info     <- readRDS(coord_info_path)
darwinSyn_clim <- readRDS(darwinSyn_clim_path)
darwinPro_clim <- readRDS(darwinPro_clim_path)
darwinNO3_clim <- readRDS(darwinNO3_clim_path)
darwinPO4_clim <- readRDS(darwinPO4_clim_path)
darwinDOP_clim <- readRDS(darwinDOP_clim_path)
darwinFe_clim  <- readRDS(darwinFe_clim_path)
longhust       <- readRDS(longhurst_path)

darwin_bound_wide <- bind_rows(darwinSyn_clim %>%  dplyr::select(library_id, value, variable),
                               darwinPro_clim,
                               darwinNO3_clim,
                               darwinPO4_clim,
                               darwinDOP_clim,
                               darwinFe_clim) %>%
                    pivot_wider(names_from = "variable", 
                                values_from = "value")


darwin_bound_wide_longhurst <-  darwin_bound_wide %>%  dplyr::left_join(longhust)

darwin_bound_wide_longhurst_meta <- darwin_bound_wide_longhurst %>% 
    dplyr::select(-c(lat, lon)) %>% 
    dplyr::left_join(dbget_metadata(), by=c("library_id"="library_id"))

########################################################################
# Collate taxonomic profiles from DB to the metadata..
# Construct query for dumpy family or species level taxonomic
# profiling
########################################################################

concatenate_from_db <- function(family = TRUE){
    marine_df <- 
        metadata %>%
        dplyr::select(library_id,
               lat = latitude_decimal_deg_n,
               lon = longitude_decimal_deg_w,
               col_date =iso_datetime_utc ,
               depth = depth_m,
               section_id,
               temperature = temperature_deg_c,
               phosphate = phosphate_bottle_umol_l,
               nitrate = nitrate_nitrite_bottle_umol_l,
               ctd_station = ctd_station_no,
               sample_method,
               platform_strategy,
               sra_accession_number) %>% 
        dplyr::left_join(
            dplyr::select(mapping,
                   experiment_accession,
                   run_accession,
                   sample_accession,
                   description),
            by = c("sra_accession_number" = "sample_accession")
        ) %>%
        dplyr::left_join(
            # Count number of family per library
            dplyr::select(family_summary ,
                   family_name = name,
                   family_fraction_total_reads = fraction_total_reads,
                   experiment_id,
                   family_sample_id = sample_id) %>% 
                dplyr::group_by (experiment_id) %>% 
                dplyr::summarize(family_number=n()) %>% 
                dplyr::ungroup() ,
            by = c("experiment_accession" = "experiment_id")
        ) %>% 
        dplyr::left_join(
            # Count number of species per library
            dplyr::select(species_summary ,
                   specie_name = name,
                   species_fraction_total_reads = fraction_total_reads,
                   experiment_id,
                   species_sample_id = sample_id) %>% 
                dplyr::group_by (experiment_id) %>% 
                dplyr::summarize(species_number=n()) %>% 
                ungroup() ,
            by = c("experiment_accession" = "experiment_id")
        ) %>% 
        dplyr::left_join(
            # Return the family with the most coverage
            dplyr::select(family_summary,
                   family_name = name, 
                   family_fraction_total_reads = fraction_total_reads,
                   experiment_id,
                   family_sample_id = sample_id) %>% 
                dplyr::filter(!family_name == 'Hominidae') %>% 
                dplyr::group_by(experiment_id) %>% 
                window_order(desc(family_fraction_total_reads)) %>% 
                dplyr::filter(family_fraction_total_reads == max(family_fraction_total_reads)) %>% 
                ungroup() %>% 
                dplyr::select(top1_family_name = family_name, 
                       top1_family_fraction_total_reads = family_fraction_total_reads, 
                       experiment_id),
            by = c("experiment_accession" = "experiment_id")
            
        ) %>% 
        dplyr::left_join(
            # Return the Species with the most coverage
            dplyr::select(species_summary,
                   species_name = name, 
                   species_fraction_total_reads = fraction_total_reads,
                   experiment_id,
                   species_sample_id = sample_id) %>% 
                dplyr::filter(!species_name == 'Homo sapiens') %>% 
                dplyr::group_by(experiment_id) %>% 
                window_order(desc(species_fraction_total_reads)) %>% 
                dplyr::filter(species_fraction_total_reads == max(species_fraction_total_reads)) %>% 
                ungroup() %>% 
                dplyr::select(top1_species_name = species_name, 
                       top1_species_fraction_total_reads = species_fraction_total_reads, 
                       experiment_id),
            by = c("experiment_accession" = "experiment_id")
            
        )
    if (family) {
        marine_df <- marine_df %>% 
            dplyr::left_join(
                # Return topn family per library_id with the most coverage
                dplyr::select(family_summary,
                       taxon_name = name,
                       fraction_total_reads,
                       experiment_id,
                       sample_id) %>%
                    dplyr::filter(!taxon_name == 'Hominidae') %>%
                    dplyr::group_by(experiment_id) %>% arrange(desc(fraction_total_reads)) %>%
                    # slice_min(n=10, order_by=desc(family_fraction_total_reads)) %>%
                    ungroup() %>%
                    dplyr::select(taxon_name,
                           fraction_total_reads,
                           experiment_id, sample_id),
                by = c("experiment_accession" = "experiment_id")
                
            ) 
    }
    else{
        marine_df <-  marine_df %>% 
            dplyr::left_join(
                # Return topn species per library_id with the most coverage
                dplyr::select(species_summary,
                       taxon_name = name,
                       fraction_total_reads,
                       experiment_id,
                       sample_id) %>%
                    dplyr::filter(!taxon_name == 'Homo sapiens') %>%
                    dplyr::group_by(experiment_id) %>% arrange(desc(fraction_total_reads)) %>%
                    # slice_min(n=10, order_by=desc(family_fraction_total_reads)) %>%
                    ungroup() %>%
                    dplyr::select(taxon_name,
                           fraction_total_reads,
                           experiment_id,sample_id),
                by = c("experiment_accession" = "experiment_id")
                
            )
        
    }
    marine_df <- marine_df %>%     
        dplyr::left_join(
        # Append EBI-Samplesheet metadata to metadata 
            dplyr::select(samplesheet,
               samplesheet_experiment_accession = experiment_accession,
               accession,
               run_accession,
               study_accession,
               instrument_model,
               read_count,
               fastq_bytes),
        by = c("experiment_accession"="samplesheet_experiment_accession")
    )
    return (marine_df)
    
}
#####################################################################
#  This is and except of the data to be pull from the DB
#  use concatenate_from_db(family=TRUE) %>%  show_query() to print the corresponding backend 
#  SQL command
#####################################################################
# concatenate_from_db(family=TRUE) %>%  show_query()

#####################################################################
# Merge the above collation with CMAP merged data
##################################################################### 

pull_from_db <- function(family=TRUE, save=FALSE){
    marine_df <- concatenate_from_db(family=family) %>% 
        collect() %>% 
        separate(col_date,into = c("col_date","rest"), sep="T") %>% 
        dplyr::select(-rest) %>% 
        dplyr::mutate(col_date=ymd(col_date))%>%
        dplyr::left_join( darwin_bound_wide_longhurst,
                   by=c('library_id' = 'library_id')) %>% 
        dplyr::rename(lat = lat.x, lon=lon.x,
               lat_longhurst = lat.y, 
               lon_longhurst = lon.y,
               run_accession = run_accession.x,
               experiment_description = description.x,
               longhurst_description = description.y,
               biosample_id = sra_accession_number) %>% 
        dplyr::select(-c(run_accession.y, accession))
    if (family){
        if (save){
            outfile = str_glue("{basepath}/marine_composite_allfam.csv")
            write_csv(marine_df, 
                      file=outfile, 
                      append = FALSE,
                      col_names = TRUE)    
        }
    }
    else{
        if(save){
            outfile = str_glue("{basepath}/marine_composite_allspecies.csv")
            write_csv(darwin_bound_wide_longhurst, 
                      file=outfile, 
                      append = FALSE,
                      col_names = TRUE)    
        }
    }
    if(save){
        write_csv(darwin_bound_wide_longhurst, 
                  file=str_glue("{basepath}/darwin_bound_wide_longhurst.csv"), 
                  append = FALSE,
                  col_names = TRUE)    
    }
    return(marine_df)
}

pull_software_version <- function(){
    return(software_versions)
} 


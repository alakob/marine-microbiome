
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


########################################################
#      RETRIEVE DATA FROM BACKEND DATABASE
########################################################
pgdrv <- dbDriver(drvName = "PostgreSQL")

connection <-DBI::dbConnect(pgdrv,
                            dbname="marine_db",
                            host="localhost", port=5432,
                            user = 'postgres',
                            password = getPass("Enter Password:"))


dbListTables(connection)

dbListTables(connection ) %>%
    map(.f = function(x) tbl(connection, x)) %>%
    set_names(dbListTables(connection))

tbl(connection, "species_bracken_results") %>%
    filter(fraction_total_reads > 0.50) %>%
    arrange(desc(fraction_total_reads))
#show_query()

tbl(connection, "species_bracken_results") %>%  count(name)
tbl(connection, "species_bracken_results") %>%  count(name) %>%
    arrange(desc(n)) %>%
    filter(name=='Francisella hispaniensis') %>%
    show_query()
tbl(connection, "species_bracken_results") %>%  count(name)



## Explore the following tables: family_bracken_summary experiment_metadata id_mapping
## - skimr
## - use julia silge geolocation clustering and ploting
## - Use matt dancho  correleation analysis
## - barplot of species by cruise/Years
## -


family_summary <- tbl(connection, "family_bracken_summary")
family_results <- tbl(connection, "family_bracken_results")

species_summary <- tbl(connection, "species_bracken_summary")
species_results <- tbl(connection, "species_bracken_results")

metadata <- tbl(connection, "experiment_metadata")
mapping <- tbl(connection, "id_mapping")
samplesheet <- tbl(connection, "samplesheet")

family_summary
family_results
species_results
species_summary

# TO DO ...
# append biogeography to biogeochemistry to hydrographic data to
# Metadata. Collate these data from the CMAP repository using the 
# Cmap4r packages.
# Reference: 
# https://github.com/slhogle/tara_oceans_metadata/blob/v1.0/bin/formatting_pipeline.md
# Here we use the cmap4r package to get the Darwin model nutrient
# climatology. We will use this modeled data since it is
# highly complete and represents more of an “time-averaged” 
# picture rather than the point source observations we have 
# from the GEOTRACES samples.
#
# We will download modeled: - 
# - Synechococcus (mmol C/L) = prokaryote_c01_darwin_clim 
# - Prochlorococcus (mmol C/L) = prokaryote_c02_darwin_clim 
# - Dissolved Nitrate (mmol N/L) = NO3_darwin_clim 
# - Dissolved Phosphate (mmol P/L) = PO4_darwin_clim 
# - Dissolved Organic Phosphate (mmol P/L) = DOP_darwin_clim 
# - Dissolved Total Fe (mmol Fe/L) = FeT_darwin_clim
# BIO-G-SHIP repeat hydrography transects Metadata

meta_tbl <- metadata %>% 
    collect() %>%
    select(library_id,
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
    rename(lat = latitude_decimal_deg_n,
           lon = longitude_decimal_deg_w,
           depth = depth_m,
           temperature =  temperature_deg_c,
           phosphate = phosphate_bottle_umol_l,
           nitrate_nitrite = nitrate_nitrite_bottle_umol_l ) %>% 
   separate(iso_datetime_utc, 
          into=c("collection_date", "rest"), 
          sep="T",
          remove = TRUE) %>%
    mutate(collection_date = ymd(collection_date))
    
meta_tbl
# Columns of interest (library_id, latitude_decimal_deg_n, longitude_decimal_deg_w,
# iso_datetime_utc, depth_m, temperature_deg_c,  phosphate_bottle_umol_l,nitrate_nitrite_bottle_umol_l, ctd_station_no,sample_method,platform_strategy, sra_accession_number  )

################################
# Load CMAP data
################################
coord_info_path <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwin_coords_depth.rds"
darwinSyn_clim_path <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwinSyn_clim.rds"
darwinPro_clim_path <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwinPro_clim.rds"
darwinNO3_clim_path <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwinNO3_clim.rds"
darwinPO4_clim_path <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwinPO4_clim.rds"
darwinDOP_clim_path  <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwinDOP_clim.rds"
darwinFe_clim_path <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwinFe_clim.rds"
longhurst_path  <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/longhurst.rds"

coord_info     <- readRDS(coord_info_path)
darwinSyn_clim <- readRDS(darwinSyn_clim_path)
darwinPro_clim <- readRDS(darwinPro_clim_path)
darwinNO3_clim <- readRDS(darwinNO3_clim_path)
darwinPO4_clim <- readRDS(darwinPO4_clim_path)
darwinDOP_clim <- readRDS(darwinDOP_clim_path)
darwinFe_clim  <- readRDS(darwinFe_clim_path)
longhust       <- readRDS(longhurst_path)

darwin_bound_wide <- bind_rows(darwinSyn_clim %>%  select(library_id, value, variable),
                               darwinPro_clim,
                               darwinNO3_clim,
                               darwinPO4_clim,
                               darwinDOP_clim,
                               darwinFe_clim) %>%
    pivot_wider(names_from = "variable", 
                values_from = "value")

darwin_bound_wide


darwin_bound_wide_longhurst <-  darwin_bound_wide %>%  left_join(longhust)

darwin_bound_wide_longhurst_meta <- darwin_bound_wide_longhurst %>% 
    select(-c(lat, lon)) %>% 
    left_join(meta_tbl, by=c("library_id"="library_id"))


# saveRDS(darwin_bound_wide_longhurst_meta, 
#        file="/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_RDS/darwin_bound_wide_longhurst_meta.rds")




########################################################################
# Collate taxonomic profiles from DB to the metadata..
########################################################################
# Collect marine data family-wide

marine_df <- 
    metadata %>%
    select(library_id,
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
    left_join(
        select(mapping,
               experiment_accession,
               run_accession,
               sample_accession,
               description),
        by = c("sra_accession_number" = "sample_accession")
    ) %>%
    left_join(
        # Count number of family per library
        select(family_summary ,
               family_name = name,
               family_fraction_total_reads = fraction_total_reads,
               experiment_id,
               family_sample_id = sample_id) %>% 
            group_by (experiment_id) %>% 
            dplyr::summarize(family_number=n()) %>% 
            ungroup() ,
        by = c("experiment_accession" = "experiment_id")
    ) %>% 
    left_join(
        # Count number of species per library
        select(species_summary ,
               specie_name = name,
               species_fraction_total_reads = fraction_total_reads,
               experiment_id,
               species_sample_id = sample_id) %>% 
            group_by (experiment_id) %>% 
            dplyr::summarize(species_number=n()) %>% 
            ungroup() ,
        by = c("experiment_accession" = "experiment_id")
    ) %>% 
    left_join(
        # Return the family with the most coverage
        select(family_summary,
               family_name = name, 
               family_fraction_total_reads = fraction_total_reads,
               experiment_id,
               family_sample_id = sample_id) %>% 
            filter(!family_name == 'Hominidae') %>% 
            group_by(experiment_id) %>% 
            window_order(desc(family_fraction_total_reads)) %>% 
            filter(family_fraction_total_reads == max(family_fraction_total_reads)) %>% 
            ungroup() %>% 
            select(top1_family_name = family_name, 
                   top1_family_fraction_total_reads = family_fraction_total_reads, 
                   experiment_id),
        by = c("experiment_accession" = "experiment_id")
        
    ) %>% 
    left_join(
        # Return the Species with the most coverage
        select(species_summary,
               species_name = name, 
               species_fraction_total_reads = fraction_total_reads,
               experiment_id,
               species_sample_id = sample_id) %>% 
            filter(!species_name == 'Homo sapiens') %>% 
            group_by(experiment_id) %>% 
            window_order(desc(species_fraction_total_reads)) %>% 
            filter(species_fraction_total_reads == max(species_fraction_total_reads)) %>% 
            ungroup() %>% 
            select(top1_species_name = species_name, 
                   top1_species_fraction_total_reads = species_fraction_total_reads, 
                   experiment_id),
        by = c("experiment_accession" = "experiment_id")
        
    ) %>% 
    left_join(
        # Return topn family per library_id with the most coverage
        select(family_summary,
               family_name = name,
               family_fraction_total_reads = fraction_total_reads,
               experiment_id,
               family_sample_id = sample_id) %>%
            filter(!family_name == 'Hominidae') %>%
            group_by(experiment_id) %>% arrange(desc(family_fraction_total_reads)) %>%
            # slice_min(n=10, order_by=desc(family_fraction_total_reads)) %>%
            ungroup() %>%
            select(topn_family_name = family_name,
                   topn_family_fraction_total_reads = family_fraction_total_reads,
                   experiment_id, family_sample_id),
        by = c("experiment_accession" = "experiment_id")

    ) %>%
    # left_join(
    #     # Return topn species per library_id with the most coverage
    #     select(species_summary,
    #            species_name = name, 
    #            species_fraction_total_reads = fraction_total_reads,
    #            experiment_id,
    #            species_sample_id = sample_id) %>% 
    #         filter(!species_name == 'Homo sapiens') %>% 
    #         group_by(experiment_id) %>% arrange(desc(species_fraction_total_reads)) %>%  
    #         # slice_min(n=10, order_by=desc(family_fraction_total_reads)) %>%  
    #         ungroup() %>% 
    #         select(topn_species_name = species_name, 
    #                topn_species_fraction_total_reads = species_fraction_total_reads, 
    #                experiment_id, species_sample_id),
    #     by = c("experiment_accession" = "experiment_id")
    #     
    #) %>%
    left_join(
        # Append EBI-Samplesheet metadata to metadata 
        select(samplesheet,
               samplesheet_experiment_accession = experiment_accession,
               accession,
               run_accession,
               study_accession,
               instrument_model,
               read_count,
               fastq_bytes),
        by = c("experiment_accession"="samplesheet_experiment_accession")
    )

    
marine_df %>%  glimpse()
marine_df %>%  show_query()

species_summary
#marine_df <- marine_fam
    
marine_df <- marine_df %>% 
    collect() %>% 
    separate(col_date,into = c("col_date","rest"), sep="T") %>% 
    select(-rest) %>% 
    mutate(col_date=ymd(col_date)) #%>% 
    mutate(tax_presence=1)


marine_df %>% select(topn_family_name) %>%  ggplot(aes(topn_family_name)) + geom_bar() + coord_flip()

marine_df %>%  summarize(min_ = min(topn_family_fraction_total_reads)*100, max_ = max(topn_family_fraction_total_reads)*100)

marine_df %>%  group_by(topn_family_name) %>%  summarise(family_perc = 100* sum(topn_family_fraction_total_reads)/n()) %>%  ungroup() %>%  
    arrange(desc(family_perc)) %>%  ggplot(aes(topn_family_name, family_perc)) + geom_col() + coord_flip()
#####################################################################
# Merge the above collation with CMAP merged data
##################################################################### 

marine_df <- marine_df %>%  left_join(
    darwin_bound_wide_longhurst,
    by=c('library_id' = 'library_id')) %>% 
    rename(lat = lat.x, lon=lon.x,
           lat_longhurst = lat.y, 
           lon_longhurst = lon.y,
           run_accession = run_accession.x,
           experiment_description = description.x,
           longhurst_description = description.y,
           biosample_id = sra_accession_number) %>% 
    select(-c(run_accession.y, accession))

marine_df %>%  
    glimpse()

# Write combine data to be stored in DB table
#write_csv(marine_df, file="/home/alakob/Documents/skill_development/practices/data/marine_composite_allspecies.csv", append = FALSE,col_names = TRUE)
#write_csv(darwin_bound_wide_longhurst, file="/home/alakob/Documents/skill_development/practices/data/darwin_bound_wide_longhurst.csv", append = FALSE,col_names = TRUE)

#marine_df %>% View()

#########################################
# Clean column names and transforms some
# Values in the Marine data
#########################################

marine_tbl <- marine_df %>% 
    select(library_id,
           lat, 
           lon,
           col_date,
           section_id,
           depth,
           temperature,
           phosphate,
           nitrate,
           ctd_station,
           sample_method,
           platform_strategy,
           biosample_id,
           experiment_accession,
           run_accession,
           experiment_description,
           family_number,
           species_number,
           top1_family_name,
           top1_family_fraction_total_reads,
           top1_species_name,
           top1_species_fraction_total_reads,
           topn_family_name, 
           topn_family_fraction_total_reads,
           study_accession,
           instrument_model,
           read_count,
           fastq_bytes,
           synDarwin = prokaryote_c01_darwin_clim, #_umolC_kg
           proDarwin = prokaryote_c02_darwin_clim, #_umolC_kg
           nitrateDarwin = NO3_darwin_clim,        #_dissolved_umol.kg
           phosphateDarwin = PO4_darwin_clim,      #_dissolved_umol.kg
           DOPDarwin = DOP_darwin_clim,            #_dissolved_umol.kg
           FeT_darwin_clim, 
           lat_longhurst,
           lon_longhurst,
           code #Pelagibacteraceae:Coprothermobacteraceae
           ) %>% # Top 3 taxon per sample_id Pelagibacteraceae:Hexamitidae
    mutate(ironDarwin  = FeT_darwin_clim*1000,      #_dissolved_nmol.kg
           synDarwin   = ifelse(synDarwin < 0, 0, synDarwin),
           proDarwin   = ifelse(proDarwin < 0, 0, proDarwin)
    ) %>%
    select(-FeT_darwin_clim)

marine_tbl %>%  glimpse()
marine_tbl %>%  dplyr::count(code, top1_family_name, sort = TRUE) #%>%  pull()
marine_tbl %>%  dplyr::count(top1_family_name, sort=TRUE)
marine_tbl %>%  dplyr::count(top1_species_name, sort = TRUE)

#########################################################################
#   INITIAL distribution plots of station over the map
#########################################################################

library(hrbrthemes)

#################################
#  microbial biodiversity map
#################################

#################################
#  microbial biodiversity map
#################################

plot_world_map <- function(interactive = TRUE){
    
    # DATA MANIPULATION
    world <- map_data("world")
    marine_tbl <- marine_tbl %>% group_by(biosample_id) %>% 
        arrange(desc(fraction_total_reads)) %>% 
        slice_min(n=3, order_by = desc(fraction_total_reads))  %>% 
        mutate(label_text = str_c(top1_family_name,": ", 
                                  paste0(round(top1_family_fraction_total_reads, digits=2 ),"%"))) %>% 
        ungroup() %>% 
        select(-c(taxon_name, fraction_total_reads)) %>% 
        distinct()
    
    # VISUALIZATION
    g <- marine_tbl %>% ggplot() +
        geom_map(
            data = world, map = world,
            aes(long, lat, map_id = region),
            color = "white", fill = "gray50", size = 0.05, alpha = 0.2
        )  + 
        stat_summary_hex(aes(lon,lat, z = species_number), alpha = 0.6, bins=35) +
        geom_jitter(aes(lon,lat,  text = label_text,
                        color = top1_family_name), alpha = .6, width = 2) +
        #geom_text(data = marine_tbl, aes(label=label_text, text = label_text), size = 3) +
        coord_fixed() +
        scale_fill_gradientn(colours = rainbow(2.5)) +
        scale_color_discrete() +
        labs(fill ="Number of Species",
             color="Top1 Family",
             x = "",
             y = "") +
        theme_ipsum()
    
    # INTERACTIVE VS STATIC
    if(interactive){
        return(ggplotly(g, tooltip ="text"))
    } else{
        return(g)
    }
}

plot_world_map(interactive = FALSE)



world <- map_data("world")

ggplot() +
    geom_map(
        data = world, map = world,
        aes(long, lat, map_id = region),
        color = "white", fill = "gray50", size = 0.05, alpha = 0.2
    )  + 
    stat_summary_hex(data = marine_tbl %>%  
                         select(-c(topn_family_name,
                                   topn_family_fraction_total_reads)) %>% 
                         distinct(),
                     aes(lon,lat, z = species_number), alpha = 0.6, bins=35) +
    geom_jitter(data= marine_tbl,
                aes(lon,lat,
                    color = top1_family_name), alpha = .6, width = 2) +
    coord_fixed() +
    #scale_fill_viridis_c(alpha=.9) +
    scale_fill_gradientn(colours = rainbow(2.5)) +
    # scale_fill_gradientn(colours = terrain.colors(10))
    #scale_fill_gradient(low = palette_light()[[2]], 
    #                    # mid = "white",
    #                     high = palette_light()[[1]])+
    scale_color_discrete() +
    labs(fill ="Number of Species", color="Top1 Family") +
    theme_ipsum()
####################################
# Families present in datasets
####################################
marine_tbl %>% select(-c(topn_family_name,
                         topn_family_fraction_total_reads)) %>% 
    distinct() %>% 
    ggplot(aes(x=top1_family_fraction_total_reads,
               group=top1_family_name,
               fill=top1_family_name,
               color=top1_family_name)) +
    geom_density( alpha=.5) +
    facet_wrap(~section_id, scales = "free") +
    #labs(fill="Family") +
    theme_ipsum()

######################################
# Top1 Species present in datasets
######################################
marine_tbl %>% select(-c(topn_family_name,
                         topn_family_fraction_total_reads)) %>% 
    distinct() %>% 
    ggplot(aes(x=top1_species_fraction_total_reads,
               group=top1_species_name,
               fill=top1_species_name,
               color=top1_species_name)) +
    geom_density( alpha=.5) +
    facet_wrap(~section_id, scales = "free") +
    #labs(fill="Family") +
    theme_ipsum()

#######################################################
# Categorical values relationships to families
# factor reordering reorder
#######################################################

p1 <- marine_tbl %>% 
    select(top1_family_name, section_id, depth, code, lat) %>%
    arrange(desc(depth)) %>% #pull(depth)
    mutate(depth = factor(depth) ,
           code = as_factor(code) %>%  fct_reorder(lat),
           section_id = as_factor(section_id) %>% fct_reorder( lat),
           top1_family_name = as_factor(top1_family_name)
           ) %>% 
    select(tax_name=top1_family_name, section_id,depth,code) %>% 
    pivot_longer(section_id:code)  %>% 
    ggplot(aes(y = value, fill = tax_name )) +
    geom_bar(position = "fill") + 
    scale_fill_viridis_d(alpha=.9) +
    facet_wrap(vars(name), scales= "free", ncol=3) +
    labs(x=NULL, y=NULL, fill=NULL) + #+ theme_void()
    theme_ipsum()

#######################################################
# Categorical values relationships to families
# No factor reordering reorder
#######################################################


p2 <- marine_tbl %>% 
    select(top1_family_name, section_id, depth, code, lat) %>%
    arrange(desc(depth)) %>% #pull(depth)
    mutate(depth = as_factor(depth) %>% fct_rev())%>% 
    select(tax_name=top1_family_name, section_id,depth,code) %>% 
    pivot_longer(section_id:code)  %>% 
    ggplot(aes(y = value, fill = tax_name )) +
    geom_bar(position = "fill") + 
    scale_fill_viridis_d(alpha=.9) +
    facet_wrap(vars(name), scales= "free", ncol=3) +
    labs(x=NULL, y=NULL, fill=NULL) + #+ theme_void()
    theme_ipsum()

p2
library(cowplot)
library(tidyquant)

cowplot::plot_grid(p1,p2)

################
# EDA
################
# - Visualize
#  -- plot UMAP (see get_customer_segments functions) cluster of station given
#  -- family and fraction of reads proportions.
#  -- See marine_viz_functions.R in (practices/machine_learning/R/marine/00_scripts/marine_viz_functions.R)


#########################################
# features to remove before modelling:
#########################################

# Pivot wider feature for modeling:

marine_ml_tbl <- marine_tbl %>%  
    pivot_wider(names_from=topn_family_name, 
                values_from = topn_family_fraction_total_reads,
                values_fill = 0) %>%
    pivot_wider(names_from = top1_species_name, 
                values_from = top1_species_fraction_total_reads, 
                values_fill = 0)  %>%  
    select(-c(library_id,
              ctd_station, 
              biosample_id,
              experiment_accession,
              run_accession,
              experiment_description, 
              study_accession,
              col_date,
              fastq_bytes,
              lon_longhurst,
              lat_longhurst))
marine_ml_tbl %>%  glimpse() %>%  head()




#select(-c(topn_family_name,
#          topn_family_fraction_total_reads)) %>% 
#    distinct()

############################################
# Correlation of feature and top1_family
############################################
bind_cols(marine_ml_tbl %>% 
              select(top1_family_name) ,
              select_if(marine_ml_tbl, is.numeric)) %>% 
    plot_cor(temperature)

marine_ml_tbl %>% plot_cor(family_number)
marine_ml_tbl %>% plot_cor(species_number)
marine_ml_tbl %>% plot_cor(depth)
marine_ml_tbl %>% 
    glimpse()


#############################################
# 1- REGRESSION TASK:
#   Predict temperature given all other fts.
#############################################
marine_ml_tbl %>% plot_cor(temperature)

###########################################
#   AutoML H2O
###########################################
marine_reg_tbl <- marine_ml_tbl %>% 
    mutate(across(where(is.character), as_factor)) #%>%  glimpse()

marine_reg_tbl %>%  glimpse()

# 1- Preprocessing .

##############################################
#    Skewed features
##############################################

skewed_feature_names <-
    marine_reg_tbl %>% # glimpse()
    select(-temperature)  %>%  # Depth is the outcome to predict in the regression step
    select_if(is.numeric) %>%
    map_df(skewness) %>% #pivot_longer(cols=names(.),names_to = key)
    gather(factor_key = T) %>%
    arrange(desc(value)) %>%
    filter(value > .8) %>%
    pull(key) %>%
    as.character()
skewed_feature_names


#########################################
# Split data into training and testing.
#########################################
h2o.init()

split_h2o <- h2o.splitFrame(as.h2o(marine_reg_tbl), 
                            ratios=c(0.85), seed=1234)

train_h2o <- split_h2o[[1]]
test_h2o <- as.data.frame(split_h2o[[2]])

#######################################
# Preprocessing --> recipe
#######################################
library(themis)
# See https://juliasilge.com/blog/multinomial-volcano-eruptions/
recipe_marine <- recipe(temperature ~., data = as.data.frame(train_h2o)) %>% 
    update_role(library_id, new_role="Id") %>% 
    step_other(top1_family_name) %>% 
    step_other(top1_species_name) %>% 
    step_other(code) %>% 
    step_other(section_id) %>% 
    step_impute_knn(all_numeric_predictors()) %>% 
    step_YeoJohnson(all_of(skewed_feature_names)) %>% 
    step_zv(all_predictors()) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_dummy(all_nominal_predictors()) %>%
    prep(retain=TRUE)

recipe_obj <- recipe(temperature ~., data = as.data.frame(train_h2o)) %>% 
    step_impute_knn(all_outcomes()) %>% 
    step_impute_knn(all_numeric_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    prep(retain=TRUE)


marine_prep  <- recipe_obj %>% bake(new_data=NULL)
marine_prep %>%  skimr::skim()

# 4- Evaluate model

####################################
# Modeling
####################################
y   <- "temperature" # tax_name
x   <- setdiff(names(train_h2o),y)

automl_models_h2o <- h2o.automl(
    x                 = x,
    y                 = y,
    training_frame    = train_h2o,
    # validation_frame  = valid_h2o,
    # leaderboard_frame = test_h2o,
    max_runtime_secs  = 900,
    keep_cross_validation_predictions = TRUE,
    keep_cross_validation_models = TRUE,
    nfolds = 10
)



# Investigate the content of the automl_models_h2o
automl_models_h2o %>%  typeof()
slotNames(automl_models_h2o)

#######################################
# Visualize H2o model performance/Matt
#######################################


######################################
# DALEX explain model
######################################

library(DALEXtra)
library(modelStudio)
model <- automl_models_h2o@leader
model
# create an explainer for the model
explainer <- explain_h2o(model,
                         data = test_h2o,
                         y = test_h2o$temperature,
                         label = "h2o")

# pick observations
new_observation <- test_h2o[1:10,]
rownames(new_observation) <- paste0("id", 1:10)

# make a studio for the model
modelStudio(explainer, new_observation, parallel=TRUE, viewer = "internal")


extract_h2o_model_name <- function (h2o_leaderboard, n = 1, verbose = FALSE){
    model_name <- h2o_leaderboard %>% 
        as_tibble() %>% 
        slice(n) %>% 
        pull(model_id)
    if(verbose) message(model_name)
    return(model_name)
}



# Save and load models
basedir <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/models/top3family_presence_absence"

for(i in 1:6){
    model_id <- automl_models_h2o@leaderboard %>%  
        extract_h2o_model_name(n=i, verbose=FALSE)
    model_h2o <- h2o.getModel(model_id)
    h2o.saveModel(model_h2o, path = basedir)
}





#############################################
# 1-  CLASSIFICATION TASK
#     Predict top families/species given fts.
#############################################
# marine_ml_tbl %>% plot_cor(top1_family_name)
marine_tbl %>%  
    pivot_wider(names_from=topn_family_name, 
                values_from = topn_family_fraction_total_reads,
                values_fill = 0) %>%
    pivot_wider(names_from = top1_species_name, 
                values_from = top1_species_fraction_total_reads, 
                values_fill = 0)  %>%  
    select(-c(library_id,
              ctd_station, 
              biosample_id,
              experiment_accession,
              run_accession,
              experiment_description, 
              study_accession,
              col_date,
              fastq_bytes,
              lon_longhurst,
              lat_longhurst))

marine_class_tbl <- marine_tbl %>% 
    mutate(taxon = case_when(
        top1_family_name == "Prochloraceae" ~ "Prochloraceae",
        TRUE ~ "Other")) %>%  
    pivot_wider(names_from=topn_family_name, 
                values_from = topn_family_fraction_total_reads,
                values_fill = 0) %>%
    pivot_wider(names_from = top1_species_name, 
                values_from = top1_species_fraction_total_reads, 
                values_fill = 0)  %>%  
    select(-c(library_id,
              top1_family_name,
              top1_family_fraction_total_reads,
              ctd_station, 
              biosample_id,
              experiment_accession,
              run_accession,
              experiment_description, 
              study_accession,
              col_date,
              fastq_bytes,
              lon_longhurst,
              lat_longhurst)) 


marine_class_tbl <- marine_class_tbl %>% janitor::clean_names()

# Preprocessing step with recipe.


skewed_feature_names <-
    marine_class_tbl %>% # glimpse()
    select(-taxon)  %>%  # Depth is the outcome to predict in the regression step
    select_if(is.numeric) %>%
    map_df(skewness) %>% #pivot_longer(cols=names(.),names_to = key)
    gather(factor_key = T) %>%
    arrange(desc(value)) %>%
    filter(value > .8) %>%
    pull(key) %>%
    as.character()
skewed_feature_names


#########################################
# Split data into training and testing.
#########################################

#######################################
# Preprocessing --> recipe
#######################################
library(themis)
# See https://juliasilge.com/blog/multinomial-volcano-eruptions/

recipe_obj <- recipe(taxon ~., data = as.data.frame(train_h2o)) %>% 
    step_impute_knn(all_numeric_predictors()) %>%
    #step_YeoJohnson(all_of(skewed_feature_names)) %>% 
    step_zv(all_predictors()) %>% 
    step_normalize(all_numeric_predictors()) %>% 
    step_dummy(all_nominal_predictors()) %>%
    step_smote(taxon) %>% 
    prep(retain=TRUE)


marine_class_prep  <- recipe_obj %>% bake(new_data=NULL)
marine_class_prep %>%  skimr::skim()
marine_class_prep %>%  plot_cor(taxon)


h2o.init()

split_h2o <- h2o.splitFrame(as.h2o(marine_class_prep), 
                            ratios=c(0.85), seed=1234)

train_h2o <- split_h2o[[1]]
test_h2o <- as.data.frame(split_h2o[[2]])




####################################
# Modeling
####################################
y   <- "taxon" # tax_name
x   <- setdiff(names(train_h2o),y)

automl_models_h2o <- h2o.automl(
    x                 = x,
    y                 = y,
    training_frame    = train_h2o,
    # validation_frame  = valid_h2o,
    # leaderboard_frame = test_h2o,
    max_runtime_secs  = 18000,
    keep_cross_validation_predictions = TRUE,
    keep_cross_validation_models = TRUE,
    nfolds = 10
)

#
# Save models:
#
path ="/home/alakob/Documents/development/skill_development/practices/machine_learning/R/models/classification/"


for(i in 1:20){
    print(i)
    model_id <- automl_models_h2o@leaderboard %>%  
        extract_h2o_model_name(n=i, verbose=FALSE)
    model_h2o <- h2o.getModel(model_id)
    h2o.saveModel(model_h2o, path = path)
}

# Investigate the content of the automl_models_h2o
automl_models_h2o %>%  typeof()
slotNames(automl_models_h2o)

#######################################
# Visualize H2o model performance/Matt
#######################################
plot_h2o_leaderboard_tidy(automl_models_h2o@leaderboard)


######################################
# DALEX explain model
######################################

library(DALEXtra)
library(modelStudio)
model <- automl_models_h2o@leader
model
# create an explainer for the model
explainer <- explain_h2o(model,
                         data = test_h2o,
                         y = test_h2o$temperature,
                         label = "h2o")

# pick observations
new_observation <- test_h2o[1:10,]
rownames(new_observation) <- paste0("id", 1:10)


# 4- Evaluate model
# make a studio for the model
modelStudio(explainer, new_observation, parallel=TRUE, viewer = "internal")





####################################
# 6- Grid optimization
####################################

# h2o_split <- h2o.splitFrame(as.h2o(data))
# train <- h2o_split[[1]]
# test <- as.data.frame(h2o_split[[2]])

# h2o automl takes target as factor
# train$survived <- as.factor(train$survived)

# fit a model
# automl <- h2o.automl(y = "survived", training_frame = train, max_runtime_secs = 30)
# model <- automl@leader
# model %>%  typeof()

# create an explainer for the model
# explainer <- explain_h2o(model,
#                         data = test,
#                         y = test$survived,
#                         label = "h2o")
#
# pick observations
# new_observation <- test[1:2,]
# rownames(new_observation) <- c("id1", "id2")

# make a studio for the model
# modelStudio(explainer, new_observation, parallel=TRUE,
#            B = 5)
# modelStudio(explainer, test, parallel=TRUE)

# marine_reg_split <- initial_split(marine_reg_tbl, strata = temperature)
# marine_reg_train <- training(marine_reg_split)
# marine_reg_test  <- testing(marine_reg_split)

# marine_reg_train %>%  glimpse()
# set.seed(2022)
# marine_reg_train_folds <- spatial_clustering_cv(
#    marine_reg_train,
#    coords = c('lon','lat'),
#    v = 10
#)
#marine_reg_train_folds
# marine_reg_train %>%  glimpse()
# marine_reg_test %>%  glimpse()



#####################################
#####################################

# Make an explainer H2O
explainer_h2o <- DALEX::explain(
    model = model_h2o,
    data  = train_tbl,
    y     = train_tbl$temperature,
    label ="XGBOOST"
)

modelStudio::modelStudio(explainer = explainer_h2o)




# Get the best models
h2o.getModel('XGBoost_1_AutoML_1_20220223_190130')

glm_reg <- automl_models_h2o@leaderboard %>% 
    as_tibble() %>% 
    slice(1) %>% 
    pull(model_id) %>% 
    h2o.getModel()


extract_h2o_model_name <- function (h2o_leaderboard, n = 1, verbose = FALSE){
    model_name <- h2o_leaderboard %>% 
        as_tibble() %>% 
        slice(n) %>% 
        pull(model_id)
    if(verbose) message(model_name)
    return(model_name)
}

# Make prediction
h2o_prediction <- h2o.predict(xgboost_reg, newdata = as.h2o(test_tbl)) %>% 
    as_tibble()

h2o_prediction


#######################################
# Explain the model with modelStudio
#######################################

library(modelStudio)
library(DALEX)

# Make an explainer


# Model H2O ----
model_h2o <- automl_models_h2o@leaderboard %>% 
    as_tibble() %>% 
    slice(1) %>% 
    pull(model_id) %>% 
    h2o.getModel()

# Make an explainer H2O
explainer_h2o <- DALEX::explain(
    model = model_h2o,
    data  = train_tbl,
    y     = train_tbl$temperature,
    label ="XGBOOST"
)

modelStudio::modelStudio(explainer = explainer_h2o)


# Tidymodel ----
fit_xgboost <- boost_tree(learn_rate = .3) %>% 
    set_mode("regression") %>% 
    set_engine("xgboost") %>% 
    fit(temperature ~., data= train_tbl)
# Make an explainer Tidy
explainer_tidy <- DALEX::explain(
    model = fit_xgboost,
    data  = train_tbl,
    y     = train_tbl$temperature,
    label ="XGBOOST"
)

modelStudio::modelStudio(explainer = explainer_tidy)





############################################################
#test_tbl <- recipe_obj %>%  bake(new_data=marine_reg_test)
# Failed to transform testing sets. with the following error --> INVESTIGATE
# Error in gower_work(x = x, y = y, pair_x = pair_x, pair_y = pair_y, n = n,  : 
#                        Column 6 of x is of class character while matching column 6 of y is of class factor
#
# train_tbl %>%  glimpse()
# test_tbl %>%  glimpse()

###############################################
# Save a copy of the dataset to be processed
# with pycaret2.3.6
###############################################
# marine_data <- recipe_obj %>% bake(new_data = marine_ml_tbl)
# write_csv(marine_data, file ="/home/alakob/Documents/skill_development/practices/machine_learning/Python/pycaret/data/marine.csv")
# Explore the distribution of the data after transformation
# train_tbl %>% plot_hist_facet()

###################################
##### modelStudio demo ####
####################################
# Library
library(tidyverse)
library(tidymodels)
library(DALEX)
library(modelStudio)

# Data
data_tbl <- mpg %>% 
    select(hwy, manufacturer:drv, fl, class)

# Model

fit_xgboost <- boost_tree(learn_rate = .3) %>% 
    set_mode("regression") %>% 
    set_engine("xgboost") %>% 
    fit(hwy~., data= data_tbl)
# Explain model

explainer <- DALEX::explain(fit_xgboost, 
                            data  = data_tbl, 
                            y     = data_tbl$hwy,
                            label = "xgboost")

# Run modelstudio

modelStudio::modelStudio(explainer=explainer)



fit_xgboost %>%  str()





#############################################
# 1-  CLASSIFICATION TASK
#     Predict top families/species given fts.
#############################################
marine_ml_tbl %>% plot_cor(top1_family_name)













###########################################
# EDA for numeric predictors
###########################################
library(GGally)
marine_ml_tbl  %>%  
    select(-c(code, section_id)) %>% 
    ggpairs(columns = 2:14, aes(color=tax_name, alpha=0.5))
###########################################
#  EDA for categorical predictors
###########################################

x <- marine_sub_tbl %>% 
    select(tax_name, section_id, depth, code) %>% 
    mutate(depth = depth %>%  as.factor() %>%  fct_rev() ,
           code = code %>%  as.character())

marine_sub_tbl %>% 
    select(tax_name, section_id, depth, code) %>% 
    mutate(depth = as.factor(depth),
           code = code %>%  as.character()) %>% #glimpse() 
    pivot_longer(section_id:code) %>% 
    ggplot(aes(y = value,fill=tax_name )) +
    geom_bar(position = "fill") + 
    facet_wrap(vars(name), scales= "free", ncol=3) +
    labs(x=NULL, y=NULL, fill=NULL) + 
    theme_ipsum()

###########################################
#    BUILD A MODEL/ Silge
###########################################


library(tidymodels)
library(spatialsample)

set.seed(123)
marine_folds <- spatial_clustering_cv(
    marine_sub_tbl,
    coords = c('lon','lat'),
    v = 10
)
marine_folds

marine_metrics <- metric_set(mn_log_loss, accuracy, sensitivity, specificity)

marine_rec <- recipe(tax_name ~., data = marine_sub_tbl) %>% 
    step_novel(all_nominal_predictors()) %>% 
    step_other(all_nominal_predictors(), threshold = 0.01) %>% 
    step_unknown(all_nominal_predictors()) %>% 
    step_impute_knn(all_numeric_predictors()) %>% 
    step_zv(all_predictors())


marine_rec

library(baguette)
bag_spec <- bag_tree(min_n = 10) %>% 
    set_engine("rpart", times = 25) %>% 
    set_mode("classification")

imb_wf <- workflow() %>% 
    add_recipe(marine_rec) %>% 
    add_model(bag_spec)

imb_fit <- fit(imb_wf, data = marine_sub_tbl)
imb_fit

########################################
#   RESAMPLE AND COMPARE MODELS /Silge
########################################

#doParallel::registerDoParallel()
set.seed(123)
imb_rs <- 
    fit_resamples(
        imb_wf,
        resamples = marine_folds,
        metrics = marine_metrics
    )

collect_metrics(imb_rs)

# Change the preprocessing recipe to account for the 
# Class imbalance
library(themis)
bal_rec  <- marine_rec %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_smote(tax_name)

bal_wf <- workflow() %>% 
    add_recipe(bal_rec) %>% 
    add_model(bag_spec)
set.seed(2345)

bal_rs <- fit_resamples(
    bal_wf,
    resamples = marine_folds,
    metrics = marine_metrics
)

collect_metrics(bal_rs)

# compare the log loss and accuracy and sensitivity, 
# high loss is bad high accuracy is good, high sensitivity is good
# in real-world case, we are often interested in identifying the 
# minority class.
# In real world use several performance metric to see if they do agree
# If evaluating on log loss, it is advised to use an imbalance data
#
#test_df <- test_ray
###########################################
#   AutoML H2O
###########################################
#glm_spec <- logistic_reg()

marine_sub_tbl <- marine_sub_tbl %>% 
    mutate(across(where(is.character), as_factor)) 

marine_sub_tbl %>%  glimpse()

marine_sub_tbl %>%  filter(code %in% lregion )

marine_sub_tbl %>% 
    count(tax_name)

marine_sub_tbl %>% 
    count(code)

marine_split <- initial_split(marine_sub_tbl, strata = code) # tax_name
marine_train <- training(marine_split)
marine_test <- testing(marine_split)

marine_train %>%  count(code) # tax_name
marine_test %>%  count(code)  # tax_name
set.seed(123)
marine_train_folds <- spatial_clustering_cv(
    marine_train,
    coords = c('lon','lat'),
    v = 10
)
marine_folds

# Steps
# 1- preprocess
# 2- split/ cross validation
# 3- Model

# Prediction tasks predict region given sepecies and attributes
# top 25 regions for highy represented species c("MONS","ISSG","SATL","NATR","NASW","WTRA","SPSG")
# Predict species/family given attributes and classes

# 4- Evaluate model
# 5- Visualize
#  -- plot UMAP (see get_customer_segments functions) cluster of station given
#  -- family and fraction of reads proportions.
#  -- See marine_viz_functions.R in (practices/machine_learning/R/marine/00_scripts/marine_viz_functions.R)
# 6- Grid optimization

# 1- Preprocessing .

##############################################
#    Skewed features
##############################################

skewed_feature_names <-
    marine_sub_tbl %>%
    select_if(is.numeric) %>%
    map_df(skewness) %>% #pivot_longer(cols=names(.),names_to = key)
    gather(factor_key = T) %>%
    arrange(desc(value)) %>%
    filter(value > .8) %>%
    pull(key) %>%
    as.character()
skewed_feature_names

marine_sub_tbl %>%  glimpse()
# Preprocessing --> recipe
recipe_obj <- recipe(code ~., data = marine_train) %>% 
    step_zv(all_predictors()) %>% 
    #step_novel(all_nominal_predictors()) %>% 
    #step_other(all_nominal_predictors(), threshold = 0.01) %>% 
    #step_unknown(all_nominal_predictors()) %>% 
    step_impute_knn(all_numeric_predictors()) %>% 
    step_YeoJohnson(all_of(skewed_feature_names)) %>% 
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_dummy(all_nominal(), -code) %>%
    step_smote(code, over_ratio = 0.5) %>% 
    prep(retain=TRUE)

train_tbl <- recipe_obj %>% bake(new_data=NULL)
test_tbl <- recipe_obj %>%  bake(new_data=marine_test)
train_tbl %>%  count(code)
test_tbl %>%  count(code)

marine_data <- recipe_obj %>% bake(new_data = marine_sub_tbl)
write_csv(marine_data, file ="/home/alakob/Documents/skill_development/practices/machine_learning/Python/pycaret/data/marine.csv")
# Explore the distribution of the data after transformation
#
train_tbl %>% plot_hist_facet()
marine_train %>% plot_hist_facet()

# Setup ----
library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)
library(cowplot)
library(fs)
library(glue)

# 2. Modelling
h2o.init()
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), 
                            ratios=c(0.85), seed=1234)

split_h2o   %>%  names(.)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o <- as.h2o(test_tbl)

y <- "code" # tax_name
x <- setdiff(names(train_h2o),y)

automl_models_h2o <- h2o.automl(
    x                 = x,
    y                 = y,
    training_frame    = train_h2o,
    validation_frame  = valid_h2o,
    leaderboard_frame = test_h2o,
    max_runtime_secs  = 180,
    keep_cross_validation_predictions = TRUE,
    keep_cross_validation_models = TRUE,
    nfolds = 10
)

# Investigate the content of the automl_models_h2o
automl_models_h2o %>%  typeof()
slotNames(automl_models_h2o)

automl_models_h2o@leaderboard
automl_models_h2o@project_name
automl_models_h2o@event_log %>%  as_tibble()
automl_models_h2o@leader %>%  as_tibble()
automl_models_h2o@modeling_steps
automl_models_h2o@training_info

# Get the best models
h2o.getModel('XGBoost_1_AutoML_10_20220222_135507')

automl_models_h2o@leaderboard %>% 
    as_tibble() %>% 
    slice(1) %>% 
    pull(model_id)


extract_h2o_model_name <- function (h2o_leaderboard, n = 1, verbose = FALSE){
    model_name <- h2o_leaderboard %>% 
        as_tibble() %>% 
        slice(n) %>% 
        pull(model_id)
    if(verbose) message(model_name)
    return(model_name)
}

automl_models_h2o@leaderboard %>% 
    extract_h2o_model_name(n=1:2) %>% 
    h2o.getModel()

# Save and load models
basedir <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/models/22feb2022/predict_region/top50species_top25region"

for(i in 1:3){
    model_id <- automl_models_h2o@leaderboard %>%  
        extract_h2o_model_name(n=i, verbose=FALSE)
    model_h2o <- h2o.getModel(model_id)
    h2o.saveModel(model_h2o, path = basedir)
}
# Make prediction

xgboost <-  automl_models_h2o@leaderboard %>% 
    extract_h2o_model_name(n=1) %>% 
    h2o.getModel()
h2o.saveModel(xgboost, path=basedir)

# Or we could load the saved model with h2o.loadModel(path)
# Make prediction
h2o_prediction <- h2o.predict(xgboost, newdata = as.h2o(test_tbl)) %>% 
    as_tibble()

h2o_prediction


#####################################
#  Explain models
#####################################
library(modelStudio)
library(DALEX)
library(tidymodels)
library(tidyverse)

xgboost %>%  typeof()

explainer <- DALEX::explain(
    model = xgboost,
    data = train_tbl,
    y = train_tbl$code,
    label = "XGBoost"
)

# MODEL STUDIO ----
modelStudio::modelStudio(explainer)

# Get the parameters used for gbm 
xgboost@parameters
xgboost@allparameters

#Getting cross validation models
h2o.cross_validation_models(xgboost)

automl_models_h2o@leaderboard %>% 
    plot_h2o_leaderboard_tidy(mean)
# 3 Visualizing a leaderboard
#data <- automl_models_h2o@leaderboard
#data 
plot_h2o_leaderboard <- function(data, 
                                 fct_reorder = FALSE, 
                                 fct_rev=FALSE,
                                 add_label = TRUE,
                                 metrics =c("rmse","logloss")){
    data_processed <- data %>%  
        as_tibble() %>%
        separate(model_id, into = "model_type",sep="_", remove = F) %>% 
        pivot_longer(cols=c(mean_per_class_error ,rmse  ,mse) ,
                     names_to = "metric", values_to = "value") %>% 
        filter(metric %in% metrics  ) %>% 
        rownames_to_column() %>% 
        mutate(model_type = as_factor(model_type), metric = as_factor(metric)) #%>% 
    #mutate(model_id = paste0(rowname,". ", as.character(model_id)) %>%  as_factor())
    
    if(fct_reorder){
        data_processed <- data_processed %>% 
            group_by(metric) %>% 
            mutate(model_id = as_factor(model_id) %>% fct_reorder(value)) %>% 
            arrange(value) %>% 
            ungroup()
    }
    if (fct_rev) {
        data_processed <- data_processed %>% 
            group_by(metric) %>% 
            mutate(model_id = as_factor(model_id) %>% fct_rev()) %>% 
            arrange(value) %>% 
            ungroup()
    }
    
    # Visualization 
    g <- data_processed %>% 
        ggplot(aes(value, model_id, color = model_type)) + 
        geom_point() +
        facet_wrap(~ metric, scales = "free_x") +
        theme_tq() +
        scale_color_tq() +
        labs(title = "H2O Leaderboard metrics",
             subtitle = "Order by auc",
             x ="",
             y ="Model Position, Model ID") +
        theme(legend.position = "bottom",
              plot.title = element_text(face = "bold"))
    
    if (add_label){
        g <- g +  geom_label(aes(label = round(value,2)), hjust = "inward")
        
    }
    return(g)
    
}

automl_models_h2o@leaderboard %>% 
    plot_h2o_leaderboard(fct_reorder = T, fct_rev = F, metrics = c("rmse", "logloss")) #logloss","rmse","mse"))

plot_h2o_leaderboard_tidy <- function (h2o_leaderboard, order_by,
                                       n_max = 20, size = 4, include_lbl = TRUE){
    # Setup Inputs
    #h2o_leaderboard <- data
    #order_by <- "mean_per_class_error"
    order_by <- enquo(order_by)
    leaderboard_tbl <- h2o_leaderboard %>% 
        as_tibble() %>% 
        mutate(model_type = str_split(model_id, "_", simplify = T)[,1]) %>% 
        rownames_to_column() %>% 
        mutate(model_id = paste0(rowname, ". ", as.character(model_id)) %>%  as_factor())
    # Transformation
    if( quo_name(order_by) %in%  (colnames(leaderboard_tbl))) {
        data_transformed_tbl <- leaderboard_tbl %>% 
            slice(1:n_max) %>% 
            mutate(
                model_id = as_factor(model_id) %>% reorder(!!order_by),
                model_type = as_factor(model_type)
            ) %>% 
            pivot_longer(cols=c(mean_per_class_error ,logloss,rmse  ,mse) ,
                         names_to = "key", values_to = "value") #auc,logloss,aucpr,
        
    } else {
        stop(paste0("Order_by = '", order_by, "' is not a permitted option."))
    }
    # Visualisation
    g <- data_transformed_tbl %>% 
        ggplot(aes(value, model_id, color= model_type)) +
        geom_point(size = size) +
        facet_wrap(~ key, scales="free_x") +
        theme_tq() +
        scale_color_tq() +
        labs(title = "Leaderboad metrics",
             subtitle = paste0("Order by: ", toupper(quo_name(order_by))),
             y = "Model Position , Model ID", x="")
    if (include_lbl) g <- g + geom_label(aes(label = round(value, 2)), hjust = "inward")
    return(g)
}

automl_models_h2o@leaderboard %>% 
    plot_h2o_leaderboard_tidy(logloss)

# Getting cross validation models
# Visualizing a leaderboard
# Data manipulation



######################################
# Correlation Analysis ----
######################################
# Data Preprocessing With Recipes ----
# Plan: Correlation Analysis
# 1. Zero Variance Features ----
# 2. Transformations ---- 
# 3. Center/Scaling -----
# 4. Dummy Variables ----
# Final Recipe -----
#####################################

train_tbl %>% 
    plot_hist_facet(bin=10, ncol=5, fct_rev = F)

train_tbl %>%
    get_cor(tax_name, fct_reorder = T, fct_rev = T)

train_tbl %>%
    plot_cor(target = tax_name, fct_reorder = T, fct_rev = F)


##############################################
#    Skewed features
##############################################

skewed_feature_names <-
    top_tax_marine_df %>%
    select_if(is.numeric) %>%
    map_df(skewness) %>% #pivot_longer(cols=names(.),names_to = key)
    gather(factor_key = T) %>%
    arrange(desc(value)) %>%
    filter(abs(value) >= 0.5) %>%
    pull(key) %>%
    as.character()


########################################
# Final Recipe -----
########################################
recipe_obj <- recipe(tax_name ~ ., data = marine_train) %>%
    step_zv(all_predictors()) %>%
    step_YeoJohnson(all_of(skewed_feature_names)) %>%
    step_string2factor(c(tax_name,code)) %>% 
    #step_other(library_id) %>% 
    step_impute_knn(temperature) %>% 
    #step_num2factor(factor_names) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_dummy(all_nominal(), -tax_name) %>%
    prep()

recipe_obj

train_tbl <- recipe_obj %>% bake( marine_train)

test_tbl <- recipe_obj %>% bake(marine_test)

##############################################################
# Distribution of features in train_tbl after applying recipe
##############################################################
train_tbl %>%  plot_hist_facet()

################# END H2O Modelling ########################

###########################################
# Fit and evaluate model
###########################################
# Apply Julia Silge blog sliced aircraft


library(tidymodels)
library(spatialsample)

set.seed(123)
good_folds <- spatial_clustering_cv(
    top_tax_marine_df,
    coords = c('lon','lat'),
    v = 5
)
good_folds

set.seed(123)
bad_folds <- vfold_cv(
    top_tax_marine_df, 
    v =5, 
    strata=tax_name
)
bad_folds

# Loop through the split list and plot the splits
walk(good_folds$splits, plot_splits)

walk(bad_folds$splits, plot_splits)


top_tax_marine_tbl<- top_tax_marine_df %>%  
    mutate(class = case_when(
        tax_name =='Prochloraceae' ~ 'TRUE',
        TRUE ~ 'FALSE'
    )) 

top_tax_marine_tbl <-
    top_tax_marine_tbl %>%  mutate(class=class %>% as_factor())


top_max_marine_tbl <- top_tax_marine_tbl %>% 
    select(
        class,
        lat,
        lon,
        temperature,
        proDarwin_umolC.kg,
        proDarwin_umolC.kg,
        nitrateDarwin_dissolved_umol.kg,
        phosphateDarwin_dissolved_umol.kg,
        DOPDarwin_dissolved_umol.kg,
        fraction_total_reads,
        read_count,
        base_count,
        code,
        ironDarwin_dissolved_nmol.kg      
    ) %>% 
    mutate(code = code %>% as_factor)
    #glimpse()
set.seed(123)
good_folds <- spatial_clustering_cv(
    top_tax_marine_tbl,
    coords = c('lon','lat'),
    v = 10
)
good_folds

set.seed(123)
bad_folds <- vfold_cv(
    top_tax_marine_tbl, 
    v =5, 
    strata=tax_name
)
bad_folds

    
    
glm_spec <- logistic_reg()
specie_form <- class ~ lat + lon + temperature + proDarwin_umolC.kg + proDarwin_umolC.kg + nitrateDarwin_dissolved_umol.kg + phosphateDarwin_dissolved_umol.kg + DOPDarwin_dissolved_umol.kg + fraction_total_reads + read_count + base_count + ironDarwin_dissolved_nmol.kg      
specie_wf <- workflow(specie_form, glm_spec)

doParallel::registerDoParallel()
set.seed(2022)
regular_rs <- fit_resamples(specie_wf, bad_folds)
set.seed(2022)
spatial_rs <- fit_resamples(specie_wf, good_folds)

spatial_rs
collect_metrics(regular_rs)
collect_metrics(spatial_rs)
###################################################
# Reproduce silge spatial clusting ands modeling
###################################################
data('lsl', package='spDataLarge')
landslide <- as_tibble(lsl)
landslide



ggplot(landslide, aes(x,y)) + 
    stat_summary_hex(aes(z=elev), alpha = 0.6, bins=12) + 
    geom_point(aes(color=lslpts), alpha = 0.7) +
    coord_fixed() +
    scale_fill_viridis_c() +
    scale_color_manual(values= c("gray90","midnightblue")) +
    labs(fill = "Elevation", color="Landslide?")

library(tidymodels)
library(spatialsample)
set.seed(123)
good_folds <- spatial_clustering_cv(landslide, coords=c("x","y"), v = 5)
good_folds

set.seed(234)
bad_folds <- vfold_cv(landslide, v =5, strata = lslpts)
bad_folds

plot_splits <- function(split){
    p <- bind_rows(
        analysis(split) %>% 
            mutate(analysis = "Analysis"),
        assessment(split) %>%  
            mutate(analysis ="Assessment")
    ) %>% 
        ggplot(aes(x, y, color = analysis)) +
        geom_point(size = 1.5, alpha = 0.8) +
        coord_fixed() +
        labs(col = NULL)
    print(p)
}

walk(good_folds$splits, plot_splits)
walk(bad_folds$splits, plot_splits)

glm_spec <- logistic_reg()
lsl_form <- lslpts ~ slope + cplan + cprof + elev + log10_carea
lsl_wf <- workflow(lsl_form, glm_spec)

doParallel::registerDoParallel()
set.seed(2021)
regular_rs <- fit_resamples(lsl_wf, bad_folds)

spatial_rs <- fit_resamples(lsl_wf, good_folds)

collect_metrics(regular_rs)
collect_metrics(spatial_rs)


############################################################
top_tax_marine_df %>% 
    mutate(depth=as_factor(depth))%>%
    plot_cor(code)
# Missing values ...
top_tax_marine_df %>% 
    map_dfr( ~ sum(is.na(.)) /length(.)) %>% 
    gather() %>%  nest() %>%  unnest()


############################################################

library(hrbrthemes)

top_tax_marine_df %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate_if(is.factor, as.numeric) %>% 
    #select(-library_id) %>% 
    gather(key=key, value = value, factor_key = TRUE) %>% 
    ggplot(aes(x=value, group=key)) +
    geom_histogram(bins=10, fill="blue",color="white") +
    facet_wrap(~key, ncol=5, scale="free") +
    theme_ipsum()
    glimpse()
    top_tax_marine_df 


set.seed(123)
marine_folds <- spatial_clustering_cv(
    marine_train,
    coords = c('lon','lat'),
    v = 10
)

marine_folds
library(themis)

multi_spec <- multinom_reg(penalty = tune(),
                           mixture = 1) %>% 
    set_mode("classification") %>% 
    set_engine("glmnet")

multi_spec


marine_wf <- workflow() %>% 
    add_recipe(recipe_obj) %>% 
    add_model(multi_spec)

marine_wf

members_metrics <- metric_set(roc_auc, accuracy, sensitivity, specificity)


marine_grid <- grid_regular(
    penalty(range=c(-5,0)), levels=20)


doParallel::registerDoParallel()
set.seed(2022)

marine_rs <- tune_grid(
    marine_wf,
    marine_folds,
    grid = marine_grid
)

marine_rs

autoplot(marine_rs)


show_best(marine_rs)

# Choose and evaluate a final model

final_penalty <- marine_rs %>% 
    select_by_one_std_err(metric = "roc_auc", desc(penalty))

final_penalty_best <- marine_rs %>% 
    select_best(metric = "roc_auc", desc(penalty))
final_penalty_best


final_rs <- 
    marine_wf %>% 
    finalize_workflow(final_penalty) %>% 
    last_fit(marine_split)

final_rs


collect_metrics(final_rs)

collect_predictions(final_rs) %>% 
    conf_mat(tax_name, .pred_class) %>% 
    autoplot()


# Visualize the ROC curve for each classes

collect_predictions(final_rs) %>% 
    roc_curve(truth = tax_name, .pred_Finance:.pred_Micro) %>% 
    ggplot(aes(1- specificity, sensitivity, color = .level)) +
    geom_abline(slope = 1, color = "gray50", lty =2, alpha = .8) +
    geom_path(size = 1.5, alpha =.7) +
    labs(color = NULL) +
    coord_fixed()

# Draw a conclusion on the plot results


regular_rs <- fit_resamples(tax_wf, bad_folds)
set.seed(2022)
spatial_rs <- fit_resamples(tax_wf, good_folds)

# How did we perform?
collect_metrics(regular_rs)
collect_metrics(spatial_rs)

#%>%
#samplesheet %>%  colnames()

# "experiment_accession",
# "accession",
# "run_accession", 
# "study_accession",
# "instrument_model",
# "base_count",
# "read_count",
# "fastq_bytes"








#########################################################################
family_summary %>%
    select(name,
           fraction_total_reads,
           experiment_id,
           sample_id)
# Column of interest (name, fraction_total_reads,
# experiment_id, sample_id- run_accession)

mapping %>%
    select(experiment_accession,
           run_accession,
           sample_accession,
           description)
# Columns of interest experiment_accession, run_accession, sample_accession--sra_accession_number, description


marine_df <- metadata %>%
    select(library_id,
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
    left_join(
        select(mapping,
               experiment_accession,
               run_accession,
               sample_accession,
               description),
        by = c("sra_accession_number" = "sample_accession")
    ) %>%
    left_join(
        select(family_summary ,
               name,
               fraction_total_reads,
               experiment_id,
               sample_id),
        by = c("experiment_accession" ="experiment_id")
    )

marine_df %>%  show_query()
marine_df <- marine_df %>%
    collect()
# Note: phosphate and nitrate+nitrate are measure in bottle/umol/liter

marine_df <- marine_df %>%
    rename(longitude =longitude_decimal_deg_w,
           latitude=latitude_decimal_deg_n,
           depth = depth_m,
           phosphate = phosphate_bottle_umol_l,
           nitrate_nitrite = nitrate_nitrite_bottle_umol_l,
           tax_name = name )

mapping %>%  select(experiment_accession,
                    run_accession,
                    sample_accession,
                    description)

marine_df %>%  colnames()

library(skimr)

marine_df %>%  skim()

meta_tbl <- meta_tbl %>%  collect()
meta_tbl %>%  View()
#

# Barplot
#ggplot(data, aes(x=name, y=value)) +
#    geom_bar(stat = "identity") +
#    coord_flip()

# Density plot
#data %>%
#    filter( price<300 ) %>%
#    ggplot( aes(x=price)) +
#    geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
#    ggtitle("Night price distribution of Airbnb appartements") +
#    theme_ipsum()

# Distribution of taxon per depth

marine_df %>%
    filter(fraction_total_reads > .2) %>%
    select(tax_name, 
           longitude, 
           latitude,
           depth,
           temperature_deg_c,
           phosphate,
           nitrate_nitrite ) %>%
    group_by(depth) %>%
    count(tax_name) %>%
    mutate(percent = n/sum(n) * 100) %>%
    mutate(depth=as.factor(depth)) %>%
    ungroup() %>%
    ggplot(aes(depth, percent, fill=tax_name)) +
    geom_bar(stat="identity") +
    theme_ipsum()

# Distribution of taxon per temperature
marine_df %>%
    filter(fraction_total_reads > .2) %>%
    ggplot(aes(x=temperature_deg_c)) +
    stat_boxplot()+
    coord_flip() +
    facet_wrap(~tax_name)

boxplot_num <- function(df=df, col= 'temperature_deg_c', threshold=0.01){
    df %>%
        filter(fraction_total_reads > threshold) %>%
        filter(!tax_name=='Hominidae')%>%
        ggplot(aes(x= tax_name, y=col)) +
        stat_boxplot() +
        coord_flip() +
        theme_ipsum()


}

boxplot_num(df = marine_df, col='phosphate')
boxplot_num(df = marine_df, col='nitrate_nitrite')
boxplot_num(df = marine_df, col='temperature_deg_c')
boxplot_num(df = marine_df, col='phosphate')


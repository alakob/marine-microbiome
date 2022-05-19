
## Libraries
library(tidyverse)    # Core Libraries - dplyr, ggplot2
library(dbplyr)       # Database backend for dplyr
library(fuzzyjoin)
library(spatialsample)
library(tidymodels)
library(lubridate)
library(h2o)
library(recipes)
library(stringr)
library(forcats)
library(cowplot)
library(fs)
library(glue)
# Error: An error has occurred. Check your logs or contact the app author for clarification.
########################################################
#      RETRIEVE DATA FROM BACK-END DATABASE
########################################################
#~/Documents/development/skill_development/practices/machine_learning/R/00_Scripts/eda_ml_performance_plots.R
basedir <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/"
marine_dump_cmd <- str_c(basedir,"marine_dump_from_db.R", sep = "")
eda_ml_performance_plots <- str_c(basedir, "00_Scripts/eda_ml_performance_plots.R", sep="")
source (marine_dump_cmd)
source (eda_ml_performance_plots)

################################################
## Finally dump concatenated tables
################################################
marine_tbl <- pull_from_db(family = TRUE, save = FALSE)


marine_tbl %>%  glimpse()

# 1.0 ORGANIZED THE DATA WIDEN TAXON ---
marine_top3_tbl <- marine_tbl %>%
    group_by(biosample_id) %>%
    arrange(desc(fraction_total_reads)) %>%
    slice_min(n=3, order_by = desc(fraction_total_reads)) %>%
    ungroup() %>%
    dplyr::select(-c(library_id,
                     sample_id,
                     instrument_model,
                     read_count,
                     top1_family_name,
                     top1_family_fraction_total_reads,
                     ctd_station,
                     experiment_accession,
                     run_accession,
                     experiment_description,
                     study_accession,
                     col_date,
                     fastq_bytes,
                     lon_longhurst,
                     lat_longhurst))
######################################
#    Distribution of all features
#  Categorical + numeric
######################################

# Plot raw distribution of features
# To show the shape, spread and data
####################################
feature_distribution_plot <- marine_top3_tbl %>%
    plot_hist_facet()

feature_distribution_plot


# Report after preprocessing

###############################
# tbl for Regression
###############################

marine_reg <- marine_top3_tbl %>%
    select(-biosample_id) %>%
    pivot_wider(names_from = taxon_name,
                values_from = fraction_total_reads,
                values_fill = 0) %>%
    pivot_wider(names_from = top1_species_name,
                values_from = top1_species_fraction_total_reads,
                values_fill = 0) %>%
    janitor::clean_names()

# Report before preprocessing
library(DataExplorer)
marine_reg %>%
    create_report(output_file = "marine_reg_raw_eda_dataexplorer",
                  output_dir = "marine_eda",
                  y = "temperature")

###############################
# tbl for classification
###############################
marine_class <-
    marine_top3_tbl %>%  #select(taxon_name) %>%  count(taxon_name) %>% arrange(desc(n))
    group_by(biosample_id) %>%
    arrange(desc(fraction_total_reads)) %>%
    slice_min(n=1, order_by = desc(fraction_total_reads)) %>%
    ungroup() %>%
    #filter(!str_detect('Pelagibacteraceae', taxon_name)) %>%
    mutate(target = case_when(
        str_detect('Prochloraceae', taxon_name) ~ 'Prochloraceae',
        TRUE ~ "Other"
    )) %>% # select(taxon_name) %>%  count() %>% arrange(desc(freq)) #glimpse()

    #marine_top3_tbl %>%
    pivot_wider(names_from = taxon_name,
                values_from = fraction_total_reads,
                values_fill = 0) %>%
    pivot_wider(names_from = top1_species_name,
                values_from = top1_species_fraction_total_reads,
                values_fill = 0) %>%
    janitor::clean_names() %>% select(-c(prochloraceae, biosample_id))
marine_class %>%     glimpse()


marine_class %>%
    create_report(output_file = "marine_class_raw_eda_dataexplorer",
                  output_dir = "marine_eda",
                  y = "target")

######################################
#   Widen marine data with species and
#   families
######################################
#marine_wide_tbl %>%  plot_hist_facet()

# 2.0 MISSING DATA ---

missing_names_to_remove <- function(df, missing_thresh = 0.2){
    missing_thresh <- missing_thresh
    report <- df %>%
    DataExplorer::profile_missing() %>%
        arrange(desc(pct_missing)) %>%
        filter(pct_missing > missing_thresh) %>%
        pull(feature) %>%
        as.character()
    return(report)
}

missing_names_to_impute <- function(df, missing_name){
    report <- df %>%
        dplyr::select(-all_of(missing_name)) %>%
        DataExplorer::profile_missing() %>%
        arrange(desc(pct_missing)) %>%
        filter(pct_missing > 0) %>%
        pull(feature) %>%
        as.character()
    return(report)
}
get_skewed_feature_names <- function(df, target, missing){
    target_expr <- enquo(target)
    target_name <- quo_name(target_expr)
    missing_expr <- enquo(missing)
    missing_name <- quo_name(missing_expr)
    df %>% # glimpse()
        dplyr::select( -!! target_expr)  %>%  # temperature is the outcome to predict in the regression step
        dplyr::select(-all_of(!!missing_expr)) %>%
        dplyr::select_if(is.numeric) %>%
        map_df(skewness) %>% #pivot_longer(cols=names(.),names_to = key)
        gather(factor_key = T) %>%
        arrange(desc(value)) %>%
        filter(value > .8) %>%
        pull(key)  %>%
        as.character()

}

# Missing features for regression
missing_names_to_remove_lst <- missing_names_to_remove(marine_reg)
# Missing feature to impute
missing_names_to_impute_lst <-
    marine_reg %>% missing_names_to_impute(missing_names_to_remove_lst)


# Data transformation, for skewed data
skewed_feature_reg <-
    get_skewed_feature_names(marine_reg,
                             temperature,
                             missing_names_to_remove_lst)
skewed_feature_class <-
    get_skewed_feature_names(marine_class,
                             target,
                             missing_names_to_remove_lst)

skewed_feature_reg %in%  skewed_feature_class %>%  table()

skewed_feature_class %in%  skewed_feature_reg %>%  table()

# 3.0 DATA PREPROCESSING ---
##################################################
# PREPROCESSING RECEIPE FOR REGRESSION
#################################################

rec_obj_cor_reg <-
    recipe(temperature ~ ., data = marine_reg) %>%
    step_rm(all_of(missing_names_to_remove_lst)) %>%
    step_impute_bag(all_outcomes()) %>%
    step_impute_knn(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_YeoJohnson(all_of(skewed_feature_reg)) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_nzv(all_predictors()) %>%
    prep()

# rec_obj_cor_reg %>%
#     bake(new_data=NULL) %>%
#     plot_hist_facet()

# Get highly correlated feature for exploration
###################################################################
feature_high_temperature_cor_reg <- rec_obj_cor_reg %>%  bake(new_data=NULL) %>%
    get_cor(temperature, remove_ties = TRUE) %>%
    filter(abs(temperature) > 0.09) %>%  # Only Consider correlation > 0.09
    arrange(desc(temperature))  %>%
    pull(feature) %>%
    as.character()
feature_high_temperature_cor_reg

top5_important_features <- c('lat','section_id_P18','dop_darwin_clim','dop_darwin_clim','section_id_AMT28')

# Plot correlation of highly correlated features to target
# Features
#######################################################

temperature_corr_plot_reg <-  rec_obj_cor_reg %>%  bake(new_data=NULL) %>%
    dplyr::select(all_of(feature_high_temperature_cor_reg), temperature)  %>%
    plot_cor(temperature, remove_ties=TRUE)

temperature_corr_plot_reg +
    labs(title = "Feature Correlation for Regression",
         subtitle = "Target: Temperature",
         caption = date())

temperature_corr_plot_reg_sub <-  rec_obj_cor_reg %>%  bake(new_data=NULL) %>%
    dplyr::select(all_of(top5_important_features), temperature)  %>%
    plot_cor(temperature, remove_ties=TRUE)

temperature_corr_plot_reg_sub +
    labs(title = "Feature Correlation for Regression",
         subtitle = "Target: Temperature",
         caption = date())
##################################################
# Model only on highly correlated features
##################################################
marine_clean_reg <- rec_obj_cor_reg %>%  bake(new_data=NULL) %>%
dplyr::select(all_of(feature_high_temperature_cor_reg), temperature)


marine_clean_reg %>%
    create_report(output_file = "marine_reg_clean_eda_dataexplorer",
                  output_dir = "marine_eda",
                  y = "temperature")

marine_clean_reg_sub <- rec_obj_cor_reg %>%  bake(new_data=NULL) %>%
    dplyr::select(all_of(top5_important_features), temperature)

##################################################
# Save Model to filesystem
##################################################

# SAVE and load Models ----
extract_h2o_model_name <- function (h2o_leaderboard, n = 1, verbose = FALSE){
    model_name <- h2o_leaderboard %>%
        as_tibble() %>%
        slice(n) %>%
        pull(model_id)
    if(verbose) message(model_name)
    return(model_name)
}



# Split the data into train and test sets
#########################################

h2o.init()
h2o.show_progress()
split_h2o_reg <- h2o.splitFrame(as.h2o(marine_clean_reg),
                            ratios=c(0.85), seed=1234)
train_h2o_reg <- split_h2o_reg[[1]]
test_h2o_reg <- as.data.frame(split_h2o_reg[[2]])

split_h2o_reg_sub <- h2o.splitFrame(as.h2o(marine_clean_reg_sub),
                                ratios=c(0.85), seed=1234)
train_h2o_reg_sub <- split_h2o_reg_sub[[1]]
test_h2o_reg_sub <- as.data.frame(split_h2o_reg_sub[[2]])


#############################
# 5.0 Fit a model -----
#############################
y   <- "temperature"
x   <- setdiff(names(train_h2o_reg),y)

automl_models_h2o_reg <- h2o.automl(
    x                 = x,
    y                 = y,
    training_frame    = train_h2o_reg,
    max_runtime_secs  = 180,
    keep_cross_validation_predictions = TRUE,
    keep_cross_validation_models = TRUE,
    nfolds = 10
)

y.sub   <- "temperature"
x.sub   <- setdiff(names(train_h2o_reg_sub),y.sub)
automl_models_h2o_reg_sub <- h2o.automl(
    x                 = x.sub,
    y                 = y.sub,
    training_frame    = train_h2o_reg_sub,
    max_runtime_secs  = 600,
    keep_cross_validation_predictions = TRUE,
    keep_cross_validation_models = TRUE,
    nfolds = 10
)
automl_models_h2o_reg@leaderboard %>%
    extract_h2o_model_name(n=1:2) %>%
    h2o.getModel()


automl_models_h2o_reg@leaderboard
#######################################
#  Save models to filesystem
#######################################
nmodel_reg <- automl_models_h2o_reg@leaderboard %>%  dim()
for (i in 1:15){
    model_id <- automl_models_h2o_reg@leaderboard %>%
        extract_h2o_model_name(n=i, verbose = FALSE)
    model_h2o<- h2o.getModel(model_id)
    #h2o.saveModel(model_h2o, path = "00_models/h20_models/regression")
    h2o.save_mojo(model_h2o, path ="00_models/h20_models/regression/mojo")
    print(model_id)
}

######################################
#


#####################################
# Load model
#####################################
model_gbm <- h2o.loadModel("/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_dashboard/dashboard_marine/00_models/h20_models/regression/GBM_grid_1_AutoML_7_20220406_153624_model_351")
model_gbm

new_data <- test_h2o_reg %>%  as_tibble() %>%  slice(1) %>%  select(-temperature)
new_data %>%  select(prochloraceae:depth)

#h2o.no_progress()
#'lat','section_id_P18','dop_darwin_clim','dop_darwin_clim','section_id_AMT28'
new_data <- new_data  %>% select(prochloraceae:depth)
new_data <- new_data  %>%  select(lat, section_id_P18, dop_darwin_clim, dop_darwin_clim, section_id_AMT28)

model_gbm %>%  h2o.predict( newdata = as.h2o(new_data)) %>% as_tibble () %>%
    bind_cols(new_data) %>%  rename(temperature = predict) %>%  pivot_longer(cols=everything())

# Verify prediction
#test_h2o_reg %>%  as_tibble() %>%  slice(1) %>%  select(temperature)

# 6.0 EXPLAIN THE MODEL. ----

model <- automl_models_h2o_reg@leader
model

library(DALEXtra)
library(modelStudio)

# create an explainer for the model
explainer <- explain_h2o(model,
                         data = test_h2o_reg,
                         y = test_h2o_reg$temperature,
                         label = "h2o Model Temperature")

# pick observations
new_observation_reg <- test_h2o_reg[1:20,]
rownames(new_observation_reg) <- paste0("id", 1:20)

# make a studio for the model
modelStudio(explainer, new_observation_reg,
            parallel=TRUE,
            B = 10)

#############################################################
# 6.1 EXPLAIN THE MODEL. SUBS--top5_important_features
#############################################################
model_sub <- automl_models_h2o_reg_sub@leader
model_sub

library(DALEXtra)
library(modelStudio)

# create an explainer for the model
explainer_sub <- explain_h2o(model_sub,
                         data = test_h2o_reg_sub,
                         y = test_h2o_reg_sub$temperature,
                         label = "h2o Model Temperature")

# pick observations
new_observation_reg_sub <- test_h2o_reg_sub[1:2,]
rownames(new_observation_reg_sub) <- paste0("id", 1:2)

# make a studio for the model
modelStudio(explainer_sub, new_observation_reg_sub,
            parallel=TRUE,
            B = 5)




##################################################
# PREPROCESSING RECEIPE FOR CLASSIFICATION For later
#################################################
rec_obj_cor_class <-
    recipe(target ~ ., data = marine_class) %>%
    step_rm(all_of(missing_names_to_remove_lst)) %>%
    step_impute_bag(all_outcomes()) %>%
    step_impute_knn(all_numeric_predictors()) %>%
    step_impute_mode(all_nominal_predictors()) %>%
    step_YeoJohnson(all_of(skewed_feature_class)) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_nzv(all_predictors()) %>%
    prep()

# Get highly correlated feature for exploration
###################################################################
feature_high_taxon_cor_class <- rec_obj_cor_class %>%  bake(new_data=NULL) %>%
    get_cor(target, remove_ties = TRUE) %>%
    filter(abs(target) > 0.09) %>%
    arrange(desc(target))  %>%
    pull(feature) %>%
    as.character()
feature_high_taxon_cor_class

# Plot correlation of highly correlated features to target
# Features
#######################################################

taxon_corr_plot_class <-  rec_obj_cor_class %>%  bake(new_data=NULL) %>%
    dplyr::select(all_of(feature_high_taxon_cor_class), target)  %>%
    plot_cor(target, remove_ties=TRUE)

taxon_corr_plot_class +
    labs(title = "Feature Correlation for Classification",
         subtitle = "Targets: Prochloroaceae and Non-Prochloroaceae",
         caption = date())

##################################################
# Model only on highly correlated features
##################################################
marine_clean_class <- rec_obj_cor_class %>%  bake(new_data=NULL) %>%
    dplyr::select(all_of(feature_high_taxon_cor_class), target)


marine_clean_class %>%
    create_report(output_file = "marine_class_clean_eda_dataexplorer",
                  output_dir = "marine_eda",
                  y = "target")


# Split the data into train and test sets
#########################################
split_h2o_class <- h2o.splitFrame(as.h2o(marine_clean_class),
                                ratios=c(0.85), seed=1234)
train_h2o_class <- split_h2o_class[[1]]
test_h2o_class <- as.data.frame(split_h2o_class[[2]])

#train_h2o_class$target <- as.factor(train_h2o_class$target)

# Modelling: 5.0 Fit a model ----
y   <- "target" # tax_name
x   <- setdiff(names(train_h2o_class),y)

automl_models_h2o_class <- h2o.automl(
    x                 = x,
    y                 = y,
    training_frame    = train_h2o_class,
    max_runtime_secs  = 18000,
    keep_cross_validation_predictions = TRUE,
    keep_cross_validation_models = TRUE,
    nfolds = 10
)

#################################
#  Save the model on filesystem
#################################
nmodel_class <- automl_models_h2o_class@leaderboard %>%  dim()
for (i in 1:15){
    model_id <- automl_models_h2o_class@leaderboard %>%
        extract_h2o_model_name(n=i, verbose = FALSE)
    model_h2o<- h2o.getModel(model_id)
    h2o.saveModel(model_h2o, path = "00_models/h20_models/classification")
    print(paste0("Saving ", model_id, " To disk"))
}


################################
#  Plot model performances
################################
plot_h2o_leaderboard(automl_models_h2o_class@leaderboard)
plot_h2o_performance(automl_models_h2o_class@leaderboard, test_h2o_class, size = 1, max_models = 3)



##################################################
# Alternatively save marine_clean_reg and
# marine_clean_class as CSV for pycaret2.3.6
##################################################

write_csv(marine_clean_reg, file="00_data/marine_clean_reg.csv")
write_csv(marine_clean_class, file="00_data/marine_clean_class.csv")


################################
# 6.0 EXPLAIN THE MODEL.
################################
library(DALEXtra)
library(modelStudio)


model_class <- automl_models_h2o_class@leader
model_class

# create an explainer for the model
explainer <- explain_h2o(model_class,
                         data = test_h2o_class,
                         y = test_h2o_class$target,
                         label = "h2oModel classification")

# pick observations
# x<- test_h2o_class %>%  filter(target=='Prochloraceae')
# y<- test_h2o_class %>%  filter(!target=='Prochloraceae')
# new_observation_class <- bind_rows(head(x, n=5), head(y, n=5))
# #new_observation_class$target <- as.factor(new_observation_class$target)
# #new_observation_class <- test_h2o_class[1:2,]
# rownames(new_observation_class) <- paste0("id", 1:10)
# pick observations
new_observation_class <- test_h2o_class[1:10,]
rownames(new_observation_class) <- paste0("id", 1:10)


# make a studio for the model
modelStudio(explainer, new_observation_class,
            parallel=TRUE,
            B = 10)

################################
# Make prediction
################################





# #################################################
# #   Funnel plot
# #################################################
#
#
# rec_obj_funnel <-
#     recipe(temperature ~ ., data = marine_top3_tbl) %>%
#     step_rm(all_of(missing_names_to_remove)) %>%
#     step_impute_bag(all_outcomes()) %>%
#     step_impute_knn(all_numeric_predictors()) %>%
#     step_impute_mode(all_nominal_predictors()) %>%
#     #step_YeoJohnson(all_of(skewed_feature_names)) %>%
#     #step_normalize(all_numeric_predictors()) %>%
#     #step_dummy(all_nominal_predictors()) %>%
#     step_nzv(all_predictors()) %>%
#     prep()
#
#
#
# library(correlationfunnel)
#
# rec_obj_funnel %>%  bake(new_data=NULL) %>%
#     #get_cor(temperature, remove_ties = TRUE) %>%
#     #filter(abs(temperature) > 0.09) %>%
#     #arrange(desc(temperature))
#     binarize(n_bins = 4, thresh_infreq = 0.01) %>% glimpse()
#         correlate(target = temperature__27.207_29.697) %>%
#         plot_correlation_funnel()


# 4.0 Modelling ---

tic()


# 3. Visualizing a leaderboard ----
# ---------------------------------

#############################################
# 1-  CLASSIFICATION TASK
#     Predict top families/species given fts.
#############################################
# marine_ml_tbl %>% plot_cor(top1_family_name)

        # top1_family_name == "Prochloraceae" ~ "Prochloraceae",
        # TRUE ~ "Other")) %>%

marine_wide_tbl <- marine_top3_tbl %>%
    pivot_wider(names_from = taxon_name,
                values_from = fraction_total_reads,
                values_fill = 0) %>%
    pivot_wider(names_from = top1_species_name,
                values_from = top1_species_fraction_total_reads,
                values_fill = 0) %>%
    janitor::clean_names()



#
# source("llpro_bonus_machine_learning.R")
# ```
#
# ```{r, paged.print = FALSE}
# rec_obj
# ```
#
# ```{r}
# model_xgb
# ```
#
# ```{r}
# prediction_tbl <- predict.model_fit(model_xgb, new_data = train_processed_tbl, type = "prob") %>%
#     bind_cols(train_processed_tbl)
#
# prediction_tbl
# ```
#
# ```{r}
# prediction_tbl %>%
#     select(.pred_1, TARGET) %>%
#     yardstick::roc_auc(TARGET, .pred_1)
# ```
#
#

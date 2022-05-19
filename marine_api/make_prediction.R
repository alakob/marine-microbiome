library(h2o)
library(tidyverse)


format_table <- function(data){
    data %>%  rename(temperature = predict) %>% 
        pivot_longer(everything())
}


generate_prediction <- function(model, new_data ){
    predictions <-
        h2o.predict(model, newdata = as.h2o(new_data)) %>% as_tibble() %>%  
        bind_cols(new_data) %>% 
        format_table()
    
}


h2o.init()

# Load the model 
#gbm_model <- h2o.loadModel("00_models/regression/GBM_grid_1_AutoML_7_20220406_153624_model_351")
gbm_model <- h2o.import_mojo("/GBM_grid_1_AutoML_1_20220428_141256_model_16.zip")

# gbm_model


# Construct input dataframe
# new_data <- tibble(
#     prochloraceae             = -2.20991863,
#     prochlorococcus_marinus   = -2.15944647,
#     code_MONS                 =  0.00000000,
#     lon                       = -0.70838125,
#     lat                       =  2.37503551,
#     prokaryote_c01_darwin_clim= -0.58931670,
#     sample_method_underway    =  0.00000000,
#     section_id_I09N           =  0.00000000,
#     fe_t_darwin_clim          =  0.26547914,
#     section_id_I07N           =  0.00000000,
#     depth                     = -0.47300962,
#     myoviridae                = -0.37538445,
#     code_WTRA                 =  0.00000000,
#     species_number            = -0.36934772,
#     section_id_C13.5.A13.5    =  0.00000000,
#     code_NASW                 =  0.00000000,
#     family_number             = -0.20876634,
#     code_SATL                 =  0.00000000,
#     section_id_AMT28          =  0.00000000,
#     pelagibacteraceae         =  2.10109562,
#     no3_darwin_clim           = -0.03044877,
#     sample_method_CTD         =  1.00000000,
#     section_id_P18            =  0.00000000,
#     dop_darwin_clim           =  2.49989234,
#     flavobacteriaceae         =  2.56736587,
#     po4_darwin_clim           =  1.23587714,
# )

# make prediction

# prediction <- generate_prediction(gbm_model, new_data)
# prediction
# 


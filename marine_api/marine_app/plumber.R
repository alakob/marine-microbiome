#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(tidyverse)
#library(tidymodels)
library(h2o)

source("../make_prediction.R")

# Load the Regression H2o model


#* @apiTitle Marine temperature prediction API
#* @apiDescription Endpoints for predicting marine temperature given some features

#* Predict marine temperature given features
#* @param lat: latitude coordinates                       
#* @param section_id_AMT28: Found at Cruise with ID AMT28 (0: No, 1:Yes)
#* @param section_id_P18: Found at Cruise with ID P18 (0: No, 1:Yes)
#* @param dop_darwin_clim: Disolved Oxygen Phosphate value
#* @get /predict
function(lat = 2.37, section_id_AMT28  = 0, section_id_P18 = 0, dop_darwin_clim=2.5) {
    
    tibble(
        lat         = as.numeric(lat),
        section_id_AMT28 = as.numeric(section_id_AMT28),
        section_id_P18     = as.numeric(section_id_P18),
        dop_darwin_clim = as.numeric(dop_darwin_clim)
    ) %>%
        generate_prediction(gbm_model, new_data = .)
}

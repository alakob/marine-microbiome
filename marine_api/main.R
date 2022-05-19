library(plumber)
r <- plumb("marine_app/plumber.R")

r$run(port = 8800, host = "0.0.0.0")


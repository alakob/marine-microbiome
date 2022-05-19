FROM rocker/shiny-verse:latest

RUN apt-get update -qq \
  && apt-get -y --no-install-recommends install \
  libtool \
  libsodium-dev \
  libssl-dev \
  libcurl4-gnutls-dev \
    default-jdk \
    default-jre \
  && R CMD javareconf \
  && install2.r --error \
    --repos 'http://cran.rstudio.com' \
    tidymodels \
    h2o 

## Install R Packages
RUN install2.r --error --deps TRUE \
    plumber

# copy everything from the current directory into the container
COPY . .

# open port 8000 to traffic
EXPOSE 8000

# When the container starts, start the main.R script
ENTRYPOINT ["Rscript", "main.R"]



###### INSTRUCTIONS ######

# 1. Create Image:
# sudo docker build . -t alakob/h2o-marine-plumber

# 2. Run Image Locally
# docker run --rm -p 8000:8000 alakob/h2o-marine-plumber

# 3. Test Your API
# http://localhost:8888/predict

# 4. Shutdown
# Ctrl + C

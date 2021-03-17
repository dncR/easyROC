# get shiny serves plus tidyverse packages image
FROM rocker/shiny:4.0.4

# Working directory within container.
WORKDIR /home
# COPY . /home/shinyapps

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 

# install R packages required 
# (change it dependeing on the packages you need)
# Below packages are mandatory for shiny installation.
RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_version("shiny", version = "0.10.1", repos = "http://cran.us.r-project.org")"
# RUN R -e "install.packages('shiny')"
# RUN R -e "install.packages('shinydashboard')"


# Use "installDependencies.R" file to add required packages for this shiny application
RUN R -e "source('installDependencies.R')"

# copy the app to the image
COPY /app/. /srv/shiny-server/

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
CMD ["/usr/bin/shiny-server.sh"]

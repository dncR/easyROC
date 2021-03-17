# get shiny serves plus tidyverse packages image
FROM rocker/shiny:4.0.4

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
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('devtools')"

# Use "installDependencies.R" file to add required packages for this shiny application
RUN R -e "source('installDependencies.R')"

# copy the app to the image
COPY project.Rproj /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY R /srv/shiny-server/R
COPY data /srv/shiny-server/data

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
CMD ["/usr/bin/shiny-server.sh"]

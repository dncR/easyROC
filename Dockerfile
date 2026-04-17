FROM rocker/r-ver:4.5.2

ENV DEBIAN_FRONTEND=noninteractive
WORKDIR /opt/easyroc

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    ca-certificates \
    git \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
  && rm -rf /var/lib/apt/lists/*

# Install dependencies from lockfile first for better layer caching.
COPY renv.lock renv.lock
COPY renv/activate.R renv/settings.json renv/

RUN Rscript --vanilla -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
RUN RENV_CONFIG_CACHE_ENABLED=FALSE Rscript --vanilla -e "renv::restore(lockfile = 'renv.lock', prompt = FALSE)"

COPY . .

RUN useradd --create-home --shell /bin/bash appuser \
  && chown -R appuser:appuser /opt/easyroc

USER appuser

ENV RENV_PROJECT=/opt/easyroc \
    RENV_CONFIG_CACHE_ENABLED=FALSE \
    SHINY_HOST=0.0.0.0 \
    SHINY_PORT=3838 \
    R_CONFIG_ACTIVE=production

EXPOSE 3838

CMD ["Rscript", "--vanilla", "-e", "source('renv/activate.R'); shiny::runApp('app.R', host = Sys.getenv('SHINY_HOST', '0.0.0.0'), port = as.integer(Sys.getenv('SHINY_PORT', '3838')), launch.browser = FALSE)"]


FROM rocker/geospatial:4.4.1
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/* \
    && R -q -e 'install.packages(c("here", "sf", "ggspatial", "jsonlite", "cowplot", "patchwork", "quarto","ggfortify"))' \
    && R -q -e 'devtools::install_github("cboettig/minioclient")' \
    && R -q -e 'library(minioclient); install_mc()'

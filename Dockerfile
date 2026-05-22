FROM rocker/shiny:4.3.3

# ── System dependencies ────────────────────────────────────────
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libuv1-dev \
    cmake \
  && rm -rf /var/lib/apt/lists/*

# ── Install R packages ─────────────────────────────────────────
RUN R -e "install.packages(c('shinyWidgets','plotly','bslib','dplyr','tidyr','stringr','scales'), repos='https://cloud.r-project.org')"

# ── Deploy apps ────────────────────────────────────────────────
RUN rm -rf /srv/shiny-server/*
COPY apps/ /srv/shiny-server/

EXPOSE 3838

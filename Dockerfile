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

# ── Install renv ───────────────────────────────────────────────
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# ── Restore packages ───────────────────────────────────────────
# Copy renv files first so Docker can cache this layer.
# Packages only reinstall when renv.lock actually changes.
WORKDIR /build
COPY renv.lock  renv.lock
COPY renv/      renv/
COPY .Rprofile  .Rprofile

# Install into the standard R site library so Shiny Server
# finds packages without any .libPaths() configuration.
ENV RENV_PATHS_LIBRARY=/usr/local/lib/R/site-library
RUN R -e "renv::restore(prompt = FALSE)"

# ── Deploy apps ────────────────────────────────────────────────
# Clear rocker's default placeholder content, then copy your apps.
# Each subfolder of apps/ becomes a route: domain.com/<foldername>/
RUN rm -rf /srv/shiny-server/*
COPY apps/ /srv/shiny-server/

EXPOSE 3838

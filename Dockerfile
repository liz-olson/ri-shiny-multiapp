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
#  Pinned to a dated Posit Package Manager snapshot so every build installs
#  the same versions. 2026-06-15 is the date of the last production build
#  before pinning (it pulled the newest CRAN versions that day), so this
#  freezes what was already live. rocker/shiny:4.3.3 is Ubuntu 22.04
#  ("jammy"), hence the jammy binaries. To upgrade packages deliberately,
#  bump the date and check every app.
#
#  The build fails if any package didn't install or can't load, instead of
#  deploying an app that errors on first visit.
ENV PKG_SNAPSHOT="https://p3m.dev/cran/__linux__/jammy/2026-06-15"
RUN R -e "pkgs <- c('shinyWidgets','plotly','bslib','dplyr','tidyr','stringr','scales'); \
  install.packages(pkgs, repos = Sys.getenv('PKG_SNAPSHOT')); \
  missing <- setdiff(pkgs, rownames(installed.packages())); \
  if (length(missing)) stop('Failed to install: ', paste(missing, collapse = ', ')); \
  for (p in pkgs) loadNamespace(p)"

# ── Deploy apps ────────────────────────────────────────────────
RUN rm -rf /srv/shiny-server/*
COPY apps/ /srv/shiny-server/

EXPOSE 3838

# ri-shiny-multiapp

Hosts the Roosevelt Institute's public Shiny apps. Every app in `apps/` is served by one [Shiny Server](https://posit.co/products/open-source/shiny-server/) container, built from the `Dockerfile` here and deployed on Railway.

New apps are built from the [`ri-shiny-template`](https://github.com/liz-olson/ri-shiny-template) repo, which holds the shared brand helpers, CSS, and layout templates. This repo just holds the finished apps and the container that serves them.

---

## Repo structure

```
ri-shiny-multiapp/
├── apps/                          One folder per app — each folder is served at /<folder>/
│   ├── mortgage-calculator/
│   ├── mortgage-refi-calculator/
│   └── state-of-the-labor-force/
├── Dockerfile                     The production container: R + packages + apps/
├── renv.lock, renv/, .Rprofile    Local development environment only (not used by the Docker build — see "Packages")
└── README.md
```

---

## How deploys work

1. A push to `master` triggers a Railway build. (The Railway service itself — build trigger, domain, environment — is configured in the Railway dashboard, not in this repo.)
2. Railway builds the `Dockerfile`:
   - starts from `rocker/shiny:4.3.3` (R 4.3.3 + Shiny Server),
   - installs the R packages listed in the Dockerfile's `install.packages()` line,
   - copies everything in `apps/` into `/srv/shiny-server/`.
3. Shiny Server serves each folder in `apps/` as its own app on port 3838.

**Every push to `master` redeploys every app.** Test locally first, and do experimental work on a branch.

**Live URL pattern:** `https://ri-shiny-multiapp-production.up.railway.app/<folder-name>/`

**WordPress embed:**
```html
<iframe
  src="https://ri-shiny-multiapp-production.up.railway.app/your-app-name/"
  width="100%"
  height="900px"
  frameborder="0">
</iframe>
```

---

## Adding a new app

1. Build the app from a template in `ri-shiny-template` (see that repo's README). Its template folders are self-contained — each carries its own `R/` helpers and `www/` assets — so the finished folder runs on its own.
2. Copy the finished app folder into `apps/`. The folder name becomes the URL path, so use lowercase-and-hyphens (e.g. `apps/state-of-the-labor-force/`).
3. If the app reads pre-processed data (`app_data.rds`), commit that file inside the app folder — the container only has what's in `apps/`. (`ri-shiny-template`'s `.gitignore` ignores `.rds` files, so data files only get committed here.)
4. If the app uses an R package that isn't already in the Dockerfile's `install.packages()` line, add it there.
5. Run the app locally (see below), then push to `master`.

---

## Packages

**Production uses only the Dockerfile's package list.** `renv.lock` is not read by the Docker build.

- The Dockerfile installs `shinyWidgets`, `plotly`, `bslib`, `dplyr`, `tidyr`, `stringr`, and `scales` from CRAN (`repos='https://cloud.r-project.org'`), so each build gets **whatever version is newest on CRAN at build time**. `shiny` and `rmarkdown` — plus their dependencies, such as `htmltools` and `jsonlite` — come preinstalled in `rocker/shiny:4.3.3` at older, fixed versions, and `install.packages()` doesn't upgrade them when a newer package needs a newer version.
- `renv.lock` records the environment used for local development, which differs from production: it pins **R 4.5.1** and specific package versions (e.g. `shiny` 1.13.0, `plotly` 4.12.0), while the container runs **R 4.3.3**.

What that means in practice:

- A rebuild can change package versions even if no code changed, so a push that only adds a new app can change how existing apps behave.
- An app can work locally and behave differently in production.
- If a package fails to install, `install.packages()` only prints a warning — the Docker build still succeeds, and the failure only shows up when the app loads. If an app breaks right after a deploy, check the Railway build log for install warnings.

---

## Local development

Opening this project in R activates `renv` (via `.Rprofile`). The first time, restore the locked packages:

```r
renv::restore()
```

Then run any app:

```r
shiny::runApp("apps/state-of-the-labor-force")
```

This runs against `renv.lock`'s versions, not production's — see "Packages" above.

---

## Current apps

| Folder | Notes |
|---|---|
| `mortgage-calculator/` | Carries its own `R/components.R` + `R/plotting.R` and `www/roosevelt.css` |
| `mortgage-refi-calculator/` | Same structure as `mortgage-calculator/` |
| `state-of-the-labor-force/` | Styles and helpers defined inline in `app.R`; reads `app_data.rds` |

These apps predate the current `ri-shiny-template` and carry older copies of its helpers and CSS. Updating the template does **not** update apps already deployed here — each app keeps the copy it was built with until someone updates that app's files.

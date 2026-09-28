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
│   ├── state-of-the-labor-force/
│   └── test-dashboard/            Template test app (every layout, chart type & UX feature) — sample data only
├── Dockerfile                     The production container: R + packages + apps/
├── renv.lock, renv/, .Rprofile    Local development environment only (not used by the Docker build — see "Packages")
└── README.md
```

---

## How deploys work

1. A push to `master` triggers a Railway build. (The Railway service itself — build trigger, domain, environment — is configured in the Railway dashboard, not in this repo.)
2. Railway builds the `Dockerfile`:
   - starts from `rocker/shiny:4.3.3` (R 4.3.3 + Shiny Server),
   - installs the R packages listed in the Dockerfile (pinned to a dated snapshot — see "Packages"),
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
4. If the app uses an R package that isn't already in the Dockerfile's `pkgs` list, add it there.
5. Run the app locally (see below), then push to `master`.

---

## Packages

**Production uses only the Dockerfile's package list.** `renv.lock` is not read by the Docker build.

- The Dockerfile installs `shinyWidgets`, `plotly`, `bslib`, `dplyr`, `tidyr`, `stringr`, and `scales` from a **dated package snapshot** (`PKG_SNAPSHOT` in the Dockerfile — currently 2026-06-15, the date of the last build before versions were pinned), so every build installs the same versions.
- `shiny` and `rmarkdown` — plus their dependencies, such as `htmltools` and `jsonlite` — come preinstalled in `rocker/shiny:4.3.3` from an older snapshot (2024-04-23), and `install.packages()` doesn't upgrade them.
- The build **fails** if any listed package doesn't install or can't load (e.g. because a dependency is too old), rather than deploying an app that errors on its first visit. If a deploy fails, the Railway build log shows which package and why.
- `renv.lock` records the environment used for local development, which differs from production: it pins **R 4.5.1** and specific package versions (e.g. `shiny` 1.13.0, `plotly` 4.12.0), while the container runs **R 4.3.3**. So an app can work locally and behave differently in production — test anything version-sensitive against production's versions.

**Adding a package:** add it to the `pkgs` list in the Dockerfile. It installs at the snapshot date's version.

**Upgrading packages:** change the date in `PKG_SNAPSHOT` on a branch, rebuild, and check every app before merging — a new date upgrades all the listed packages at once.

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

These apps predate the current `ri-shiny-template` and carry older copies of its helpers and CSS. Updating the template does **not** update apps already deployed here — each app keeps the copy it was built with. They're being left as-is: they use Bootstrap 3 styling, so don't copy the current template's `roosevelt.css` or `R/` helpers into them. Template updates apply to new apps built from it.

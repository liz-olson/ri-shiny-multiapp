# ============================================================
#  TEST DASHBOARD — CONFIG
#  Page settings for the kitchen-sink test app. Everything else
#  (sample data, tabs, charts) lives in app.R.
# ============================================================

# ── App identity ──────────────────────────────────────────────
app_title    <- "Template Test Dashboard"
app_subtitle <- "Roosevelt Institute · every template, chart type & UX feature"

# ── Compare tab: filter row ───────────────────────────────────
#  Choices come from the sample data's industries (see app.R),
#  so only the label and width live here.
selector_label <- "Industry:"
selector_width <- "320px"

# ── Explain tab: opt-in KPI strip ─────────────────────────────
#  None of the templates include a stat strip by default — this
#  dashboard adds one (per snippets/kpi-strip.R) only so the
#  feature gets exercised. Values are filled in from the sample
#  data in app.R.

# ── Footer ────────────────────────────────────────────────────
footer_text <- "Roosevelt Institute · rooseveltinstitute.org · Sample data is randomly generated — for testing the template only"

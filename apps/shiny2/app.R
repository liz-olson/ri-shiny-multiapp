# ============================================================
#  State of the Labor Force  —  Roosevelt Institute
#  Shiny Dashboard — Plotly version
# ============================================================

library(shiny)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(bslib)

d <- readRDS("app_data.rds")
list2env(d, envir = .GlobalEnv)
rm(d)

# ── Brand colors ──────────────────────────────────────────────
ri_green       <- "#58A618"
ri_green_dark  <- "#2D5A0B"
ri_green_light <- "#C4E2B7"
ri_violet      <- "#4C12A1"
ri_magenta     <- "#AA0061"
ri_grey        <- "#5E6A71"
ri_green_70    <- "#8CCA79"

# ── Education level order + color palette ─────────────────────
educ_order <- c(
  "No formal educational credential",
  "High school diploma or equivalent",
  "Some college, no degree",
  "Postsecondary nondegree award",
  "Associate's degree",
  "Bachelor's degree",
  "Master's degree",
  "Doctoral or professional degree"
)
educ_palette <- c(
  "No formal educational credential"  = "#2D5A0B",
  "High school diploma or equivalent" = "#58A618",
  "Some college, no degree"           = "#8CCA79",
  "Postsecondary nondegree award"     = "#C4E2B7",
  "Associate's degree"                = "#C4B2E0",
  "Bachelor's degree"                 = "#9B7FCB",
  "Master's degree"                   = "#6B45B8",
  "Doctoral or professional degree"   = "#4C12A1"
)

# ── Logo ──────────────────────────────────────────────────────
logo_el <- tags$img(
  src   = "ri_logo.png",
  alt   = "Roosevelt Institute",
  style = "height:36px;width:auto;display:block;flex-shrink:0;"
)

# ── Plotly helpers ────────────────────────────────────────────
FONT_SIZE   <- 11
FONT_FAMILY <- "Montserrat, sans-serif"

ax <- function(title = "", ...) {
  list(
    title    = list(text = title, font = list(size = FONT_SIZE, color = ri_grey)),
    tickfont = list(size = FONT_SIZE, family = FONT_FAMILY, color = "#333333"),
    gridcolor = "#EBEBEB", zeroline = FALSE, showline = FALSE, ...
  )
}
ax_blank <- function(...) {
  list(title = "", showticklabels = FALSE, showgrid = FALSE,
       zeroline = FALSE, showline = FALSE, ...)
}

chip <- function(color, label) {
  tagList(
    span(style = paste0(
      "display:inline-block;width:10px;height:10px;border-radius:2px;",
      "background:", color, ";margin-right:4px;vertical-align:middle;"
    )),
    span(label, style = "vertical-align:middle;")
  )
}

# ── Shared helpers ────────────────────────────────────────────
fmt_change <- function(x) {
  fmt <- number(abs(x) * 1000, scale_cut = cut_short_scale(), accuracy = 1)
  ifelse(x > 0, paste0("+", fmt), ifelse(x < 0, paste0("−", fmt), paste0("+", fmt)))
}

# ── CSS ───────────────────────────────────────────────────────
app_css <- paste0("
  @import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@300;400;600;700&display=swap');
  *, *::before, *::after { box-sizing: border-box; }
  body { margin: 0; background: #fff; font-family: 'Montserrat', sans-serif; color: #1A1A1A; }

  /* ── Header ── */
  .ri-header {
    background: #fff;
    padding: 10px 20px 10px;
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 10px;
    border-bottom: 3px solid ", ri_green, ";
  }
  .ri-hd-text  { line-height: 1.25; }
  .ri-hd-title { font-size: 17px; font-weight: 700; color: #1A1A1A; }
  .ri-hd-sub   { font-size: 8px; font-weight: 700; text-transform: uppercase;
                 letter-spacing: 1px; color: ", ri_green, "; margin-top: 1px; }

  /* ── Nav wrapper ── */
  .nav-wrapper { background: #fff; }
  .nav-wrapper .tab-content { background: transparent; border: none; padding: 16px 20px 28px; }

  /* ── Tab pills — green; works at render position and in header after JS move ── */
  .nav-pills {
    display: flex !important;
    gap: 6px !important;
    align-items: center;
    list-style: none;
    margin: 0 !important;
    padding: 8px 20px !important;
    background: #fff;
    border: none !important;
    flex-shrink: 0;
  }
  .nav-pills > li > a {
    font-family: 'Montserrat', sans-serif !important;
    font-size: 10px !important;
    font-weight: 700 !important;
    letter-spacing: .5px !important;
    text-transform: uppercase !important;
    border-radius: 20px !important;
    padding: 5px 14px !important;
    border: 1.5px solid ", ri_green, " !important;
    color: ", ri_green, " !important;
    background: #fff !important;
    text-decoration: none !important;
    white-space: nowrap;
    transition: all .15s;
  }
  .nav-pills > li > a:hover {
    background: #f0fae8 !important;
    color: ", ri_green_dark, " !important;
    border-color: ", ri_green_dark, " !important;
  }
  .nav-pills > li.active > a,
  .nav-pills > li.active > a:focus,
  .nav-pills > li.active > a:hover {
    background: ", ri_green, " !important;
    color: #fff !important;
    border-color: ", ri_green, " !important;
  }
  /* When JS moves pills into header, remove nav-wrapper padding */
  .ri-header .nav-pills { padding: 0 !important; }

  /* ── Stat strip — green numbers, no card boxes ── */
  .stat-strip {
    display: flex;
    gap: 0;
    margin-bottom: 18px;
    border-bottom: 1px solid #EBEBEB;
    padding-bottom: 14px;
    flex-wrap: wrap;
  }
  .stat-card {
    flex: 1;
    min-width: 90px;
    padding: 4px 16px 4px 0;
    border-right: 1px solid #EBEBEB;
    margin-right: 16px;
  }
  .stat-card:last-child { border-right: none; margin-right: 0; }
  .stat-num { font-size: 26px; font-weight: 700; line-height: 1; color: ", ri_green, "; }
  .stat-lbl { font-size: 8px; text-transform: uppercase; letter-spacing: .6px;
               color: ", ri_grey, "; font-weight: 600; margin-top: 3px; }

  /* ── Chart cards — minimal border, no shadow ── */
  .ch-card {
    background: #fff;
    border: 1px solid #EBEBEB;
    border-radius: 4px;
    padding: 14px 16px 10px;
    margin-bottom: 12px;
  }
  .ch-card h3 {
    margin: 0 0 2px;
    font-size: 11px;
    font-weight: 700;
    color: #1A1A1A;
    border-bottom: none;
    padding-bottom: 0;
    text-transform: uppercase;
    letter-spacing: .8px;
  }
  .ch-card .sublabel { font-size: 9px; color: ", ri_grey, "; margin-bottom: 8px; }

  /* Plotly fills its card */
  .ch-card { display: flex; flex-direction: column; }
  .ch-card .plotly { flex: 1 1 auto; }
  .ch-card .js-plotly-plot { height: 100% !important; }

  /* bslib column gap */
  .bslib-grid { gap: 12px !important; }

  /* Equal height cards within each grid row */
  .bslib-grid > .bslib-grid-item { display: flex; flex-direction: column; }
  .bslib-grid > .bslib-grid-item > .ch-card { flex: 1; }

  /* ── Mobile ── */
  @media (max-width: 640px) {
    /* Stack logo row, then nav pills on next row */
    .ri-header {
      flex-wrap: wrap;
      padding-bottom: 8px;
      gap: 0;
    }
    .ri-header > div:first-child {
      flex: 0 0 100%;
      margin-bottom: 4px;
    }
    .ri-header .nav-pills {
      display: block !important;
      width: 100% !important;
      padding: 0 0 6px !important;
    }
    .ri-header .nav-pills > li {
      display: block !important;
      float: none !important;
      margin-bottom: 4px !important;
    }
    /* Stack explore filters above chart */
    .explore-wrap { flex-direction: column; }
    .explore-sidebar { width: 100% !important; }
    /* Prevent bslib grid from forcing equal heights when cards stack */
    .bslib-grid { grid-auto-rows: auto !important; }
    .bslib-grid > * { height: auto !important; min-height: 0 !important; }
  }

  /* ── Legend row ── */
  .legend-row {
    display: flex;
    gap: 12px;
    align-items: center;
    flex-wrap: wrap;
    font-size: 9px;
    color: ", ri_grey, ";
    margin-bottom: 8px;
  }

  /* ── Industry sector selector — prominent green pill, left-aligned ── */
  .ind-selector {
    display: flex;
    align-items: center;
    justify-content: flex-start;
    gap: 10px;
    margin-bottom: 16px;
  }
  .sector-label {
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 1px;
    color: ", ri_grey, ";
    white-space: nowrap;
  }
  .ind-selector .selectize-input {
    font-family: 'Montserrat', sans-serif !important;
    border-radius: 24px !important;
    border: 2px solid ", ri_green, " !important;
    font-size: 13px !important;
    font-weight: 700 !important;
    padding: 7px 18px !important;
    box-shadow: none !important;
    color: ", ri_green, " !important;
  }
  .ind-selector .selectize-input .item { color: ", ri_green, " !important; }
  .ind-selector .selectize-input.focus {
    border-color: ", ri_green_dark, " !important;
    box-shadow: none !important;
  }
  .ind-selector .form-group { margin-bottom: 0; }

  /* ── Explore tab: filter sidebar ── */
  .explore-wrap { display: flex; gap: 20px; align-items: flex-start; }
  .explore-sidebar {
    width: 200px;
    flex-shrink: 0;
    font-family: 'Montserrat', sans-serif;
  }
  .explore-sidebar .filter-label {
    font-size: 9px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: .8px;
    color: #1A1A1A;
    border-bottom: 1.5px solid ", ri_green, ";
    padding-bottom: 3px;
    margin-top: 16px;
    margin-bottom: 8px;
  }
  .explore-sidebar .filter-label:first-child { margin-top: 0; }
  .explore-sidebar .form-group { margin-bottom: 4px; }
  .explore-sidebar .checkbox label,
  .explore-sidebar .checkbox-inline label { font-size: 10px !important; font-family: 'Montserrat', sans-serif !important; }
  .explore-sidebar input[type='text'] {
    font-family: 'Montserrat', sans-serif !important;
    font-size: 11px !important;
    border-radius: 6px !important;
    border: 1px solid #DDD !important;
    width: 100% !important;
    padding: 5px 10px !important;
  }
  .explore-sidebar .selectize-input { font-size: 11px !important; border-radius: 6px !important; }
  .explore-sidebar .irs--shiny .irs-bar { background: ", ri_green, " !important; border-color: ", ri_green, " !important; }
  .explore-sidebar .irs--shiny .irs-handle { border-color: ", ri_green, " !important; }
  .explore-sidebar .irs--shiny .irs-from, .explore-sidebar .irs--shiny .irs-to,
  .explore-sidebar .irs--shiny .irs-single { background: ", ri_green, " !important; font-size: 9px !important; }

  /* ── Filter tooltip icon ── */
  .tip-icon {
    display: inline-block;
    width: 12px; height: 12px;
    border-radius: 50%;
    background: #aaa;
    color: #fff;
    font-size: 8px; font-weight: 700;
    text-align: center; line-height: 12px;
    cursor: default;
    margin-left: 4px;
    vertical-align: middle;
    position: relative;
    text-transform: none;
    letter-spacing: 0;
  }
  .tip-icon::after {
    content: attr(data-tip);
    position: absolute;
    left: 16px; top: -4px;
    background: #333; color: #fff;
    padding: 6px 8px;
    border-radius: 4px;
    font-size: 9px; font-weight: 400;
    width: 190px;
    white-space: normal; line-height: 1.5;
    display: none;
    z-index: 9999;
    font-family: 'Montserrat', sans-serif;
    text-transform: none; letter-spacing: 0;
    pointer-events: none;
  }
  .tip-icon:hover::after { display: block; }

  /* ── Footer ── */
  .ri-footer {
    text-align: center;
    padding: 10px;
    font-size: 9px;
    color: ", ri_grey, ";
    border-top: 1px solid #EBEBEB;
    background: #fff;
    margin-top: 4px;
  }

  /* ── Workforce composition table ── */
  .ri-demo-tbl {
    font-family: 'Montserrat', sans-serif;
    font-size: 11px;
    width: 100%;
    border-collapse: collapse;
    margin-top: 4px;
  }
  .ri-demo-tbl tbody tr td {
    padding: 9px 8px;
    color: #1A1A1A;
    text-align: left;
    border-bottom: 1px solid #F4F4F4;
  }
  .ri-demo-tbl tbody tr td:last-child { text-align: right; font-weight: 600; }
  .ri-demo-tbl tbody tr:hover td { background: #f0fae8 !important; cursor: default; }
  .ri-demo-tbl tbody tr.tbl-section-hdr td {
    padding: 10px 8px 3px !important;
    font-size: 9px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: .5px;
    color: #1A1A1A !important;
    background: #fff !important;
    border-bottom: 1.5px solid ", ri_green, " !important;
    text-align: left;
  }
")

# ============================================================
#  UI
# ============================================================
ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),

  div(class = "ri-header",
    div(style = "display:flex; align-items:center; gap:10px;",
      logo_el,
      div(class = "ri-hd-text",
        div(class = "ri-hd-title", "Roosevelt Institute"),
        div(class = "ri-hd-sub",  "State of the Labor Force")
      )
    )
  ),

  tags$script(HTML("
    $(document).ready(function() {
      // Move pills into header
      var attempts = 0;
      function moveTabs() {
        var $ul = $('#main_tabs > ul.nav-pills');
        var already = $('.ri-header').find('ul.nav-pills').length;
        if ($ul.length && !already) {
          $('.ri-header').append($ul);
          $('.ri-header').css('align-items', 'center');
        } else if (!already && attempts < 30) {
          attempts++;
          setTimeout(moveTabs, 100);
        }
      }
      setTimeout(moveTabs, 100);

      // Force Plotly resize on tab switch via Shiny message
      Shiny.addCustomMessageHandler('trigger_resize', function(msg) {
        [50, 200, 400, 800].forEach(function(d) {
          setTimeout(function() {
            document.querySelectorAll('.js-plotly-plot').forEach(function(el) {
              Plotly.Plots.resize(el);
            });
          }, d);
        });
      });
    });
  ")),

  div(class = "nav-wrapper",
    tabsetPanel(id = "main_tabs", type = "pills",

      # ── OVERVIEW ─────────────────────────────────────────────
      tabPanel("Overview",

        div(class = "stat-strip",
          div(class = "stat-card",
            div(class = "stat-num", "160M"),
            div(class = "stat-lbl", "Total Workers, 2024")),
          div(class = "stat-card",
            div(class = "stat-num", "55%"),
            div(class = "stat-lbl", "Workforce with less than BA, 2024")),
          div(class = "stat-card",
            div(class = "stat-num", "6%"),
            div(class = "stat-lbl", "Workforce with less than HS, 2024")),
          div(class = "stat-card",
            div(class = "stat-num", "$35.30"),
            div(class = "stat-lbl", "Avg. civilian wage, 2024"))
        ),

        layout_column_wrap(width = "320px", min_height = "400px", gap = "12px",
          div(class = "ch-card",
            tags$h3("Share of workforce by level of education"),
            div(class = "sublabel", "Civilian workers age 25+, 2024"),
            plotlyOutput("edu_donut", width = "100%", height = "340px"),
            div(class = "sublabel",
                "Source: CPS Basic 2022–2025")
          ),
          div(class = "ch-card",
            tags$h3("Average Wages by Race/Ethnicity and Gender"),
            div(class = "sublabel", "Civilian workers age 25+, 2025"),
            div(class = "legend-row",
              chip(ri_green,  "Male"),
              chip(ri_violet, "Female"),
              div(style = "margin-left:auto;",
                selectInput("wage_edu_filter", label = NULL,
                            choices  = c("All workers" = "all",
                                         "BA or higher" = "BA or higher",
                                         "Less than BA" = "Less than BA"),
                            selected = "all", width = "150px"))
            ),
            plotlyOutput("wages_bar", width = "100%", height = "240px"),
            div(class = "sublabel", "Source: CPS ORG 2025.")
          )
        ),

        layout_column_wrap(width = "320px", gap = "12px",
          div(class = "ch-card",
            tags$h3("Industries with Most Projected Growth"),
            div(class = "sublabel", "Employment change, 2024–34"),
            div(class = "legend-row",
              chip(ri_green,  "Increase"),
              chip(ri_violet, "Decrease")),
            plotlyOutput("bar_proj", width = "100%", height = "380px"),
            div(class = "sublabel", "Source: BLS Employment Projections 2024–34.")
          ),
          div(class = "ch-card",
            tags$h3("Education Share Within Industry"),
            div(class = "sublabel", "Civilian workers age 25+"),
            div(class = "legend-row",
              chip(ri_violet, "BA or higher"),
              chip(ri_green,  "Less than BA")),
            plotlyOutput("bar_edu", width = "100%", height = "380px"),
            div(class = "sublabel", "Source: CPS Basic 2022–2025.")
          )
        )
      ),

      # ── INDUSTRY ─────────────────────────────────────────────
      tabPanel("Sector Trends",

        div(class = "ind-selector",
          selectInput("selected_industry", label = NULL,
                      choices = focus_industries, selected = focus_industries[1],
                      width = "380px")
        ),

        layout_column_wrap(width = "320px", gap = "12px",
          div(class = "ch-card",
            uiOutput("occ_edu_bar_title"),
            div(class = "sublabel", "Top 10 by projected growth"),
            div(class = "legend-row",
              chip(ri_violet, "BA or higher"),
              chip(ri_green,  "Less than BA")),
            plotlyOutput("occ_edu_bar", width = "100%", height = "320px"),
            div(class = "sublabel", "Source: CPS Basic 2022–2025")
          ),
          div(class = "ch-card",
            uiOutput("occ_scatter_title"),
            div(class = "sublabel",
                "Mean wages in 2025, hover for labels"),
            plotlyOutput("occ_scatter", width = "100%", height = "320px"),
            div(class = "sublabel", "Source: CPS Basic 2022–2025")
          )
        ),

        layout_column_wrap(width = "320px", gap = "12px",
          div(class = "ch-card",
            uiOutput("ind_demo_title"),
            div(class = "sublabel",
                "2025"),
            uiOutput("ind_demo_table"),
            div(class = "sublabel", style = "margin-top:auto;", "Source: CPS Basic 2022–2025")
          ),
          div(class = "ch-card",
            uiOutput("demo_bar_title"),
            div(class = "sublabel",
                "Share of sector with less than BA"),
            div(class = "legend-row",
              chip(ri_green,  "Male"),
              chip(ri_violet, "Female")),
            plotlyOutput("demo_grouped_bar", width = "100%", height = "185px"),
            div(class = "sublabel",
                "Source: CPS Basic 2022–2025")
          )
        )
      ),

      # ── EXPLORE OCCUPATIONS ───────────────────────────────────
      tabPanel("Explore Occupations",
        div(class = "explore-wrap",

          # ── Filter sidebar ──
          div(class = "explore-sidebar",
            div(class = "filter-label", "Search Occupation"),
            textInput("occ_search", label = NULL,
                      placeholder = "e.g. Nurse, Manager..."),

            div(class = "filter-label", "Occupational Group"),
            selectInput("occ_group", label = NULL,
                        choices = c("All occupational groups" = "all",
                                    setNames(sort(unique(occ_explore$occ_group)),
                                             sort(unique(occ_explore$occ_group)))),
                        selected = "all"),

            div(class = "filter-label", "Education Level",
              tags$span(class = "tip-icon",
                `data-tip` = "Education level that is typically required for workers to enter occupation.",
                "?")),
            selectInput("occ_educ", label = NULL,
                        choices  = c("All levels" = "all", setNames(educ_order, educ_order)),
                        selected = "all", multiple = FALSE),

            div(class = "filter-label", "Work Experience",
              tags$span(class = "tip-icon",
                `data-tip` = "Years of experience in a related occupation typically needed for entry into a given occupation.",
                "?")),
            checkboxGroupInput("occ_work_exp", label = NULL,
                               choices  = c("None", "Less than 5 years", "5 years or more"),
                               selected = c("None", "Less than 5 years", "5 years or more")),

            div(class = "filter-label", "Annual Wage"),
            sliderInput("occ_wage", label = NULL,
                        min = 30, max = 240, value = c(30, 240),
                        step = 5, post = "K")
          ),

          # ── Chart ──
          div(style = "flex:1; min-width:0;",
            div(class = "ch-card",
              tags$h3("Wages vs. Projected Growth by Occupation"),
              div(class = "sublabel",
                  "Median annual wage vs. employment change 2024–34 · dashed line = group median wage"),
              plotlyOutput("occ_explore_plot", width = "100%", height = "520px"),
              div(class = "sublabel",
                  "Source: BLS Occupational Employment Projections 2024–34.")
            )
          )
        )
      )
    )
  ),

  div(class = "ri-footer",
    "Roosevelt Institute · rooseveltinstitute.org",
    " · Sources: BLS Employment Projections 2024–34 · CPS Basic & ORG 2022–2025"
  )
)

# ============================================================
#  SERVER  (unchanged)
# ============================================================
server <- function(input, output, session) {

  # ── Overview: education share donut ─────────────────────────
  output$edu_donut <- renderPlotly({
    labels <- c("BA and above", "High school/\nsome college", "Less than high school")
    values <- c(45, 49, 6)
    colors <- c(ri_violet, ri_green, ri_green_dark)

    plot_ly(
      labels = labels, values = values,
      type = "pie", hole = 0.5,
      marker = list(colors = colors, line = list(color = "#fff", width = 2)),
      customdata = colors,
      textposition = "outside",
      texttemplate = "<b>%{label}</b><br>%{percent:.0%}",
      outsidetextfont = list(family = FONT_FAMILY, size = 10, color = "#555555"),
      hovertemplate = "<b style='font-size:9px;color:#1A1A1A;'>%{label}</b><br><b style='font-size:11px;color:%{customdata};'>%{percent:.0%}</b><br><span style='font-size:9px;color:#888;'>of 25+ civilian workforce</span><extra></extra>",
      showlegend = FALSE
    ) %>%
      layout(
        paper_bgcolor = "white",
        font   = list(family = FONT_FAMILY, size = FONT_SIZE),
        margin = list(l = 80, r = 120, t = 20, b = 80),
        hoverlabel = list(bgcolor = "#fff",
                          font = list(family = FONT_FAMILY, size = 9, color = "#1A1A1A"),
                          bordercolor = ri_green)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── Overview: wages by race × gender ────────────────────────
  output$wages_bar <- renderPlotly({
    edu_sel <- input$wage_edu_filter
    race_lvls <- c("Asian","Black","Hispanic","Multiple races","Native American","White")
    df <- wages_data %>% filter(edu == edu_sel) %>%
      mutate(
        race  = factor(str_wrap(race, 9), levels = str_wrap(race_lvls, 9)),
        color = if_else(sex == "Male", ri_green, ri_violet)
      ) %>%
      arrange(race, sex)

    males   <- df %>% filter(sex == "Male")
    females <- df %>% filter(sex == "Female")
    y_max   <- max(wages_data$wage) * 1.15

    plot_ly() %>%
      add_bars(data = males, x = ~race, y = ~wage,
               name = "Male", marker = list(color = ri_green),
               text = ~paste0("$", round(wage, 0)),
               textposition = "outside",
               textfont = list(family = FONT_FAMILY, size = FONT_SIZE, color = ri_grey),
               hovertemplate = "<b style='font-size:9px;color:#1A1A1A;'>%{x} — Male</b><br><b style='font-size:11px;color:#58A618;'>$%{y:.0f}/hr</b><br><span style='font-size:9px;color:#888;'>avg. hourly wage</span><extra></extra>",
               offsetgroup = "1") %>%
      add_bars(data = females, x = ~race, y = ~wage,
               name = "Female", marker = list(color = ri_violet),
               text = ~paste0("$", round(wage, 0)),
               textposition = "outside",
               textfont = list(family = FONT_FAMILY, size = FONT_SIZE, color = ri_grey),
               hovertemplate = "<b style='font-size:9px;color:#1A1A1A;'>%{x} — Female</b><br><b style='font-size:11px;color:#4C12A1;'>$%{y:.0f}/hr</b><br><span style='font-size:9px;color:#888;'>avg. hourly wage</span><extra></extra>",
               offsetgroup = "2") %>%
      layout(
        barmode = "group",
        xaxis   = ax("", showgrid = FALSE, tickangle = 0),
        yaxis   = ax("", showticklabels = FALSE, showgrid = FALSE, range = c(0, y_max)),
        paper_bgcolor = "white", plot_bgcolor = "white",
        font   = list(family = FONT_FAMILY, size = FONT_SIZE),
        margin = list(l = 10, r = 20, t = 10, b = 60),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "#fff",
                          font = list(family = FONT_FAMILY, size = 9, color = "#1A1A1A"),
                          bordercolor = ri_green)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── Overview: industry projected growth ──────────────────────
  output$bar_proj <- renderPlotly({
    df <- industry_proj %>%
      arrange(change) %>%
      mutate(
        ind_wrap = str_wrap(industry, 22),
        color    = if_else(change >= 0, ri_green, ri_violet),
        label    = fmt_change(change),
        anchor   = "left",
        tx       = if_else(change >= 0,
                           change + max(abs(change)) * 0.02,
                           max(abs(change)) * 0.02)
      )

    pos <- df %>% filter(change >= 0)
    neg <- df %>% filter(change <  0)
    y_order <- reorder(df$ind_wrap, df$change)

    plot_ly() %>%
      add_bars(data = pos, x = ~change, y = ~reorder(ind_wrap, change),
               orientation = "h", marker = list(color = ri_green),
               customdata = ~label,
               hovertemplate = "<b style='font-size:9px;color:#1A1A1A;'>%{y}</b><br><b style='font-size:11px;color:#58A618;'>%{customdata}</b><br><span style='font-size:9px;color:#888;'>projected employment change</span><extra></extra>",
               showlegend = FALSE) %>%
      add_bars(data = neg, x = ~change, y = ~reorder(ind_wrap, change),
               orientation = "h", marker = list(color = ri_violet),
               customdata = ~label,
               hovertemplate = "<b style='font-size:9px;color:#1A1A1A;'>%{y}</b><br><b style='font-size:11px;color:#4C12A1;'>%{customdata}</b><br><span style='font-size:9px;color:#888;'>projected employment change</span><extra></extra>",
               showlegend = FALSE) %>%
      add_annotations(
        data = df,
        x = ~tx, y = ~ind_wrap,
        text = ~label, xanchor = ~anchor, showarrow = FALSE,
        font = list(family = FONT_FAMILY, size = FONT_SIZE, color = "#333333"),
        inherit = FALSE
      ) %>%
      layout(
        xaxis = ax_blank(),
        yaxis = ax("", tickfont = list(size = FONT_SIZE, family = FONT_FAMILY,
                                        color = "#333333"),
                   showgrid = FALSE, automargin = TRUE,
                   categoryorder = "array",
                   categoryarray = levels(reorder(df$ind_wrap, df$change))),
        paper_bgcolor = "white", plot_bgcolor = "white",
        font   = list(family = FONT_FAMILY, size = FONT_SIZE),
        margin = list(l = 10, r = 80, t = 10, b = 20),
        hoverlabel = list(bgcolor = "#fff",
                          font = list(family = FONT_FAMILY, size = 9, color = "#1A1A1A"),
                          bordercolor = ri_green)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── Overview: education share within industry ────────────────
  output$bar_edu <- renderPlotly({
    proj_order         <- industry_proj %>% arrange(change) %>% pull(industry)
    proj_order_wrapped <- str_wrap(proj_order, 22)

    df <- edu_industry %>%
      filter(!is.na(industry)) %>%
      mutate(
        ind_wrap = str_wrap(industry, 22),
        ind_wrap = factor(ind_wrap, levels = proj_order_wrapped)
      ) %>%
      filter(!is.na(ind_wrap))

    ba  <- df %>% filter(edu_group == "ba_higher")
    lba <- df %>% filter(edu_group == "less_than_ba")

    plot_ly() %>%
      add_bars(data = lba, x = ~share, y = ~ind_wrap, name = "Less than BA",
               orientation = "h", marker = list(color = ri_green),
               text = ~ifelse(share >= 8, paste0(round(share), "%"), ""),
               textposition = "inside", insidetextanchor = "middle",
               textfont = list(family = FONT_FAMILY, size = FONT_SIZE, color = "white"),
               hovertemplate = "<b style='font-size:9px;color:#1A1A1A;'>%{y}</b><br><b style='font-size:11px;color:#58A618;'>%{x:.0f}%</b><br><span style='font-size:9px;color:#888;'>less than BA</span><extra></extra>") %>%
      add_bars(data = ba, x = ~share, y = ~ind_wrap, name = "BA or higher",
               orientation = "h", marker = list(color = ri_violet),
               text = ~ifelse(share >= 8, paste0(round(share), "%"), ""),
               textposition = "inside", insidetextanchor = "middle",
               textfont = list(family = FONT_FAMILY, size = FONT_SIZE, color = "white"),
               hovertemplate = "<b style='font-size:9px;color:#1A1A1A;'>%{y}</b><br><b style='font-size:11px;color:#4C12A1;'>%{x:.0f}%</b><br><span style='font-size:9px;color:#888;'>BA or higher</span><extra></extra>") %>%
      layout(
        barmode = "stack",
        xaxis   = ax("", showticklabels = FALSE, showgrid = FALSE, range = c(-4, 104)),
        yaxis   = ax("", tickfont = list(size = FONT_SIZE, family = FONT_FAMILY,
                                          color = "#333333"),
                     showgrid = FALSE, tickpad = 10, automargin = TRUE,
                     categoryorder = "array", categoryarray = proj_order_wrapped),
        paper_bgcolor = "white", plot_bgcolor = "white",
        font   = list(family = FONT_FAMILY, size = FONT_SIZE),
        margin = list(l = 10, r = 20, t = 10, b = 10),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "#fff",
                          font = list(family = FONT_FAMILY, size = 9, color = "#1A1A1A"),
                          bordercolor = ri_green)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── Trigger Plotly resize on any tab switch ───────────────────
  observeEvent(input$main_tabs, {
    session$sendCustomMessage("trigger_resize", list())
  })

  # ── Industry: reactive ────────────────────────────────────────
  sel_ind <- reactive({ req(input$selected_industry); input$selected_industry })

  # ── Industry: chart titles with sector name ──────────────────
  ind_title <- function(base) tags$h3(paste0(base, " — ", sel_ind()))
  output$occ_edu_bar_title <- renderUI({ ind_title("Education Share by Occupation") })
  output$occ_scatter_title <- renderUI({ ind_title("Mean Wages vs. Projected Growth") })
  output$ind_demo_title    <- renderUI({ ind_title("Workforce Composition") })
  output$demo_bar_title    <- renderUI({ ind_title("Less-than-BA Workers by Race & Gender") })

  # ── Industry: stat cards (kept for reference, not shown in UI) ───
  output$ind_stat_strip <- renderUI({
    s <- industry_stats[[sel_ind()]]
    div(class = "stat-strip",
      div(class = "stat-card",
        div(class = "stat-num", s$total),
        div(class = "stat-lbl", "Total Workers, 2024")),
      div(class = "stat-card",
        div(class = "stat-num", s$pct_no_ba),
        div(class = "stat-lbl", "Workforce without BA")),
      div(class = "stat-card",
        div(class = "stat-num", s$jobs_added),
        div(class = "stat-lbl", "Projected Job Gains, 2024–34"))
    )
  })

  # ── Industry: occupations by education share ─────────────────
  output$occ_edu_bar <- renderPlotly({
    df <- occ_data %>%
      filter(industry == sel_ind()) %>%
      arrange(ba_share) %>%
      mutate(
        occupation = case_when(
          str_detect(occupation, "First.Line Supervisors of Construction Trades") ~
            "First Line Supervisors of Construction Trades",
          str_detect(occupation, "Heating.*Air Conditioning.*Refrigeration") ~
            "Heating and Refrigeration Mechanics",
          TRUE ~ occupation
        ),
        occ_wrap = str_wrap(occupation, 28)
      )
    req(nrow(df) > 0)

    lba_df <- df %>% mutate(share_pct = less_ba_share * 100,
                             label = ifelse(share_pct >= 8,
                                            paste0(round(share_pct), "%"), ""))
    ba_df  <- df %>% mutate(share_pct = ba_share * 100,
                             label = ifelse(share_pct >= 8,
                                            paste0(round(share_pct), "%"), ""))

    plot_ly() %>%
      add_bars(data = lba_df, x = ~share_pct, y = ~occ_wrap,
               name = "Less than BA", orientation = "h",
               marker = list(color = ri_green),
               text = ~label, textposition = "inside", insidetextanchor = "middle",
               textfont = list(family = FONT_FAMILY, size = FONT_SIZE, color = "white"),
               hovertemplate = "<b style='font-size:9px;color:#1A1A1A;'>%{y}</b><br><b style='font-size:11px;color:#58A618;'>%{x:.0f}%</b><br><span style='font-size:9px;color:#888;'>less than BA</span><extra></extra>") %>%
      add_bars(data = ba_df, x = ~share_pct, y = ~occ_wrap,
               name = "BA or higher", orientation = "h",
               marker = list(color = ri_violet),
               text = ~label, textposition = "inside", insidetextanchor = "middle",
               textfont = list(family = FONT_FAMILY, size = FONT_SIZE, color = "white"),
               hovertemplate = "<b style='font-size:9px;color:#1A1A1A;'>%{y}</b><br><b style='font-size:11px;color:#4C12A1;'>%{x:.0f}%</b><br><span style='font-size:9px;color:#888;'>BA or higher</span><extra></extra>") %>%
      layout(
        barmode = "stack",
        xaxis   = ax("", showticklabels = FALSE, showgrid = FALSE, range = c(-4, 104)),
        yaxis   = ax("", tickfont = list(size = FONT_SIZE, family = FONT_FAMILY,
                                          color = "#333333"),
                     showgrid = FALSE, tickpad = 10, automargin = TRUE,
                     categoryorder = "array", categoryarray = df$occ_wrap),
        uniformtext = list(mode = "hide", minsize = 8),
        paper_bgcolor = "white", plot_bgcolor = "white",
        font   = list(family = FONT_FAMILY, size = FONT_SIZE),
        margin = list(l = 10, r = 20, t = 10, b = 10),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "#fff",
                          font = list(family = FONT_FAMILY, size = 9, color = "#1A1A1A"),
                          bordercolor = ri_green)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── Industry: mean wages vs log growth scatter ───────────────
  output$occ_scatter <- renderPlotly({
    df <- occ_data %>%
      filter(industry == sel_ind(), !is.na(mean_wage)) %>%
      mutate(
        tip = paste0(
          "<b style='font-size:9px;color:#1A1A1A;'>",
          str_replace_all(str_wrap(occupation, 35), "\n", "<br>"),
          "</b><br>",
          "<b style='font-size:11px;color:#58A618;'>$", round(mean_wage, 0), "/hr</b><br>",
          "<span style='font-size:9px;color:#888;'>mean wage, 2025</span><br>",
          "<span style='font-size:9px;color:#888;'>Log growth: </span><span style='font-size:9px;color:#1A1A1A;'>", round(log_growth, 2), "</span>",
          " &nbsp; <span style='font-size:9px;color:#888;'>BA share: </span><span style='font-size:9px;color:#1A1A1A;'>", round(ba_share * 100, 0), "%</span>")
      )
    req(nrow(df) > 0)

    plot_ly(df, x = ~mean_wage, y = ~log_growth,
            type = "scatter", mode = "markers",
            marker = list(
              size    = 10,
              color   = ~ba_share,
              cmin    = 0, cmax = 1,
              colorscale = list(
                list(0,   ri_green),
                list(0.5, "#ffffff"),
                list(1,   ri_violet)
              ),
              showscale = TRUE,
              colorbar  = list(
                title     = list(text = "BA share",
                                 font = list(size = 9, family = FONT_FAMILY,
                                             color = ri_grey)),
                thickness = 10,
                len       = 0.5,
                tickformat = ".0%",
                tickfont  = list(size = 9, family = FONT_FAMILY, color = "#333333"),
                outlinewidth = 0
              ),
              opacity = 0.85,
              line    = list(width = 0.5, color = "#333")
            ),
            text = ~tip, hoverinfo = "text") %>%
      layout(
        xaxis = ax("Mean wage, 2025", tickprefix = "$", gridcolor = "#EBEBEB"),
        yaxis = ax("Log projected employment growth",  gridcolor = "#EBEBEB"),
        paper_bgcolor = "white", plot_bgcolor = "white",
        font   = list(family = FONT_FAMILY, size = FONT_SIZE),
        margin = list(l = 60, r = 20, t = 10, b = 60),
        hoverlabel = list(bgcolor = "#fff",
                          font = list(family = FONT_FAMILY, size = 9, color = "#1A1A1A"),
                          bordercolor = ri_green)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── Industry: workforce composition table ────────────────────
  output$ind_demo_table <- renderUI({
    ind <- sel_ind()

    edu_rows <- edu_industry %>%
      filter(industry == ind) %>%
      transmute(label = edu_label, share = paste0(share, "%"))

    gender_rows <- demo_data %>%
      filter(industry == ind, group_type == "all") %>%
      group_by(sex) %>%
      summarise(share_pct = sum(share_pct, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(share_pct)) %>%
      transmute(label = sex, share = paste0(round(share_pct, 1), "%"))

    race_rows <- demo_data %>%
      filter(industry == ind, group_type == "all") %>%
      group_by(race) %>%
      summarise(share_pct = sum(share_pct, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(share_pct)) %>%
      transmute(label = race, share = paste0(round(share_pct, 1), "%"))

    sec_hdr <- function(label)
      tags$tr(class = "tbl-section-hdr", tags$td(colspan = "2", label))
    data_rows <- function(df)
      lapply(seq_len(nrow(df)), function(i) {
        pct_num <- suppressWarnings(as.numeric(gsub("%", "", df$share[i])))
        bar_pct <- if (!is.na(pct_num)) pct_num else 0
        tags$tr(
          tags$td(df$label[i]),
          tags$td(style = "text-align:right; padding-right:4px;",
            div(style = "display:flex; align-items:center; justify-content:flex-end; gap:6px;",
              div(style = "width:120px; height:4px; background:#EBEBEB; border-radius:2px; flex-shrink:0; position:relative;",
                div(style = paste0(
                  "position:absolute; left:0; top:0; height:100%; border-radius:2px; ",
                  "width:", bar_pct, "%; background:", ri_green_dark, ";"
                ))
              ),
              span(df$share[i],
                   style = "font-weight:600; min-width:34px; text-align:right; display:inline-block;")
            )
          )
        )
      })

    tags$table(class = "ri-demo-tbl",
      do.call(tags$tbody, c(
        list(sec_hdr("Education")),        data_rows(edu_rows),
        list(sec_hdr("Gender")),           data_rows(gender_rows),
        list(sec_hdr("Race / Ethnicity")), data_rows(race_rows)
      ))
    )
  })

  # ── Industry: demographics grouped bar ───────────────────────
  output$demo_grouped_bar <- renderPlotly({
    ind <- sel_ind()

    race_order <- demo_data %>%
      filter(industry == ind, group_type == "less_ba") %>%
      group_by(race) %>%
      summarise(tot = sum(share_pct, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(tot)) %>% pull(race)

    race_order_wrap <- str_wrap(race_order, 9)
    df <- demo_data %>%
      filter(industry == ind, group_type == "less_ba", share_pct > 0) %>%
      mutate(race = factor(str_wrap(race, 9), levels = race_order_wrap))

    males   <- df %>% filter(sex == "Male")
    females <- df %>% filter(sex == "Female")

    plot_ly() %>%
      add_bars(data = males, x = ~race, y = ~share_pct,
               name = "Male", marker = list(color = ri_green),
               text = ~ifelse(share_pct < 1, "<1%", paste0(round(share_pct), "%")),
               textposition = "outside",
               textfont = list(family = FONT_FAMILY, size = FONT_SIZE, color = ri_grey),
               hovertemplate = "<b style='font-size:9px;color:#1A1A1A;'>%{x} — Male</b><br><b style='font-size:11px;color:#58A618;'>%{y}</b><br><span style='font-size:9px;color:#888;'>of sub-BA workforce</span><extra></extra>",
               offsetgroup = "1") %>%
      add_bars(data = females, x = ~race, y = ~share_pct,
               name = "Female", marker = list(color = ri_violet),
               text = ~ifelse(share_pct < 1, "<1%", paste0(round(share_pct), "%")),
               textposition = "outside",
               textfont = list(family = FONT_FAMILY, size = FONT_SIZE, color = ri_grey),
               hovertemplate = "<b style='font-size:9px;color:#1A1A1A;'>%{x} — Female</b><br><b style='font-size:11px;color:#4C12A1;'>%{y}</b><br><span style='font-size:9px;color:#888;'>of sub-BA workforce</span><extra></extra>",
               offsetgroup = "2") %>%
      layout(
        barmode = "group",
        xaxis   = ax("", tickfont = list(size = FONT_SIZE, family = FONT_FAMILY,
                                          color = "#333333"),
                     showgrid = FALSE, automargin = TRUE, tickangle = 0),
        yaxis   = ax("", ticksuffix = "%", showticklabels = FALSE,
                     showgrid = FALSE),
        paper_bgcolor = "white", plot_bgcolor = "white",
        font   = list(family = FONT_FAMILY, size = FONT_SIZE),
        margin = list(l = 10, r = 20, t = 10, b = 60),
        showlegend = FALSE,
        hoverlabel = list(bgcolor = "#fff",
                          font = list(family = FONT_FAMILY, size = 9, color = "#1A1A1A"),
                          bordercolor = ri_green)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ── Explore Occupations scatter ──────────────────────────────
  output$occ_explore_plot <- renderPlotly({
    plot_width <- session$clientData$output_occ_explore_plot_width
    is_mobile  <- !is.null(plot_width) && plot_width > 0 && plot_width < 480

    df <- occ_explore

    # Apply filters
    if (input$occ_group != "all")
      df <- df %>% filter(occ_group == input$occ_group)
    if (nchar(trimws(input$occ_search)) > 0)
      df <- df %>% filter(str_detect(occupation, regex(trimws(input$occ_search), ignore_case = TRUE)))
    if (!is.null(input$occ_educ) && input$occ_educ != "all")
      df <- df %>% filter(educ_needed == input$occ_educ)
    if (length(input$occ_work_exp) > 0)
      df <- df %>% filter(work_exp %in% input$occ_work_exp)
    df <- df %>% filter(median_wage >= input$occ_wage[1] * 1000,
                        median_wage <= input$occ_wage[2] * 1000)

    req(nrow(df) > 0)

    df <- df %>%
      mutate(
        educ_needed = factor(educ_needed, levels = educ_order),
        tip = paste0(
          "<b style='font-size:13px;color:#1A1A1A;'>", occupation, "</b><br><br>",
          "<b style='font-size:18px;color:#2D5A0B;'>$",
          format(round(median_wage), big.mark = ","),
          "</b>",
          " &nbsp;<span style='color:#ccc;'>│</span>&nbsp; ",
          "<b style='font-size:18px;color:#4C12A1;'>",
          ifelse(emp_change >= 0, "+", "−"),
          number(abs(emp_change) * 1000, scale_cut = cut_short_scale(), accuracy = 1),
          "</b><br>",
          "<span style='font-size:9px;color:#888;'>Annual wage",
          "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Job change ‘24–34</span><br>",
          "<span style='font-size:9px;color:#ccc;'>──────────────────────</span><br>",
          "<span style='font-size:9px;color:#aaa;'>Group&nbsp;&nbsp;</span>",
          "<span style='font-size:9px;color:#1A1A1A;'>", occ_group, "</span><br>",
          "<span style='font-size:9px;color:#aaa;'>Education&nbsp;&nbsp;</span>",
          "<span style='font-size:9px;color:#1A1A1A;'>", as.character(educ_needed), "</span><br>",
          "<span style='font-size:9px;color:#aaa;'>Experience&nbsp;&nbsp;</span>",
          "<span style='font-size:9px;color:#1A1A1A;'>", work_exp, "</span>"
        )
      )

    median_line <- median(df$median_wage, na.rm = TRUE)

    plot_ly(df, x = ~median_wage, y = ~emp_change,
            type = "scatter", mode = "markers",
            color = ~educ_needed,
            colors = educ_palette[levels(df$educ_needed)],
            marker = list(size = 9, opacity = 0.8,
                          line = list(width = 0.3, color = "#333")),
            text = ~tip, hoverinfo = "text",
            legendgroup = ~educ_needed
    ) %>%
      add_segments(
        x = median_line, xend = median_line,
        y = min(df$emp_change, na.rm = TRUE) * 1.1,
        yend = max(df$emp_change, na.rm = TRUE) * 1.1,
        line = list(color = "#555555", width = 1.5, dash = "dash"),
        showlegend = FALSE, hoverinfo = "none", inherit = FALSE
      ) %>%
      add_annotations(
        x = median_line, y = 1, yref = "paper",
        text = paste0("Median: $", format(round(median_line / 1000), big.mark = ","), "K"),
        showarrow = FALSE, xanchor = "left", yanchor = "top",
        font = list(size = 9, family = FONT_FAMILY, color = "#555555")
      ) %>%
      layout(
        xaxis = ax("Median annual wage",
                   tickprefix = "$", tickformat = ",.0f", gridcolor = "#EBEBEB"),
        yaxis = ax("Employment change 2024–34 (thousands)", gridcolor = "#EBEBEB"),
        paper_bgcolor = "white", plot_bgcolor = "white",
        font   = list(family = FONT_FAMILY, size = FONT_SIZE),
        margin = if (is_mobile)
          list(l = 40, r = 10, t = 20, b = 160)
        else
          list(l = 60, r = 20, t = 20, b = 60),
        legend = if (is_mobile) list(
          title = list(text = "<b>Education</b>",
                       font = list(size = 9, family = FONT_FAMILY)),
          font  = list(size = 8, family = FONT_FAMILY),
          orientation = "h", x = 0, y = -0.4,
          bgcolor = "rgba(255,255,255,0.9)",
          bordercolor = "#EBEBEB", borderwidth = 1
        ) else list(
          title = list(text = "<b>Education</b>",
                       font = list(size = 9, family = FONT_FAMILY)),
          font  = list(size = 9, family = FONT_FAMILY),
          orientation = "v", x = 1.01, y = 1,
          bgcolor = "rgba(255,255,255,0.9)",
          bordercolor = "#EBEBEB", borderwidth = 1
        ),
        hoverlabel = list(bgcolor = "#fff",
                          font = list(family = FONT_FAMILY, size = 11, color = "#1A1A1A"),
                          bordercolor = ri_green,
                          align = "left")
      ) %>%
      config(displayModeBar = FALSE)
  })
}

# ============================================================
shinyApp(ui, server)

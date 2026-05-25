# ============================================================
#  Roosevelt Institute — Shiny UI Component Helpers
#  Source this file at the top of app.R:
#    source("R/components.R")
#
#  These functions return Shiny HTML objects. You use them
#  exactly like any other Shiny UI element — just call them
#  inside your ui definition.
# ============================================================

# ── Brand colors (available throughout the app) ───────────────
ri_green       <- "#58A618"
ri_green_dark  <- "#2D5A0B"
ri_green_light <- "#C4E2B7"
ri_green_70    <- "#8CCA79"
ri_violet      <- "#4C12A1"
ri_magenta     <- "#AA0061"
ri_grey        <- "#5E6A71"

# ── Education level order + palette ───────────────────────────
#  Use these any time you need education as a categorical variable.
#  educ_order:   character vector in logical order (no credential → doctoral)
#  educ_palette: named color vector, pass directly to Plotly `colors =`
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


# ── ri_header() ───────────────────────────────────────────────
#  The page header: Roosevelt logo + app title + subtitle.
#  Tab pills are automatically moved here via JavaScript (see
#  the script block in app.R).
#
#  Arguments:
#    title    App title shown in large text (17px bold)
#    subtitle Smaller all-caps label below the title
#
#  Example:
#    ri_header("State of the Labor Force", "An Interactive Dashboard")
ri_header <- function(title, subtitle = "Roosevelt Institute") {
  logo_el <- tags$img(
    src   = "ri_logo.png",
    alt   = "Roosevelt Institute",
    style = "height:36px; width:auto; display:block; flex-shrink:0;"
  )

  div(class = "ri-header",
    div(style = "display:flex; align-items:center; gap:10px;",
      logo_el,
      div(class = "ri-hd-text",
        div(class = "ri-hd-title", title),
        div(class = "ri-hd-sub",  subtitle)
      )
    )
  )
}


# ── ri_tab_nav_script() ───────────────────────────────────────
#  JavaScript that moves the tab pills into the header after the
#  page loads, and triggers Plotly resize on tab switch.
#  Include this once in your ui, right after ri_header().
#
#  Arguments:
#    tabs_id  The `id` you gave your tabsetPanel (default "main_tabs")
#
#  Example:
#    ri_tab_nav_script("main_tabs")
ri_tab_nav_script <- function(tabs_id = "main_tabs") {
  tags$script(HTML(paste0("
    $(document).ready(function() {
      var attempts = 0;
      function moveTabs() {
        var $ul = $('#", tabs_id, " > ul.nav-pills');
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
  ")))
}


# ── ri_stat_card() ────────────────────────────────────────────
#  A single stat: big green number + small uppercase label.
#  Use inside ri_stat_strip().
#
#  Arguments:
#    value  The number or text to display large (e.g. "160M", "55%")
#    label  Descriptive label shown below in small caps
#
#  Example:
#    ri_stat_card("160M", "Total Workers, 2024")
ri_stat_card <- function(value, label) {
  div(class = "stat-card",
    div(class = "stat-num", value),
    div(class = "stat-lbl", label)
  )
}


# ── ri_stat_strip() ───────────────────────────────────────────
#  A horizontal row of stat cards separated by thin grey lines.
#  Pass any number of ri_stat_card() calls as arguments.
#
#  Example:
#    ri_stat_strip(
#      ri_stat_card("160M", "Total Workers, 2024"),
#      ri_stat_card("55%",  "Without BA"),
#      ri_stat_card("$35",  "Avg. Hourly Wage")
#    )
ri_stat_strip <- function(...) {
  div(class = "stat-strip", ...)
}


# ── ri_chart_card() ───────────────────────────────────────────
#  A bordered card that holds one chart. Gives the chart a title,
#  an optional subtitle, the Plotly output, and an optional source note.
#
#  Arguments:
#    title        Chart title (displayed uppercase, bold, 11px)
#    output_id    The outputId of your renderPlotly() in the server
#    height       Height of the chart area (default "300px")
#    subtitle     Optional text shown below the title (grey, 9px)
#    source_note  Optional source citation shown at the bottom
#    ...          Any additional HTML you want inside the card
#                 (e.g. a legend row or a filter dropdown)
#
#  Example:
#    ri_chart_card(
#      title       = "Employment by Sector",
#      output_id   = "sector_bar",
#      height      = "350px",
#      subtitle    = "Civilian workers age 25+, 2024",
#      source_note = "Source: CPS Basic 2022–2025"
#    )
ri_chart_card <- function(title,
                           output_id,
                           height      = "300px",
                           subtitle    = NULL,
                           source_note = NULL,
                           ...) {
  div(class = "ch-card",
    tags$h3(title),
    if (!is.null(subtitle)) div(class = "sublabel", subtitle),
    ...,
    plotlyOutput(output_id, width = "100%", height = height),
    if (!is.null(source_note)) div(class = "sublabel", style = "margin-top:6px;", source_note)
  )
}


# ── ri_legend_chip() ──────────────────────────────────────────
#  A small colored square + label, used in chart legends.
#  Combine several inside ri_legend_row().
#
#  Example:
#    ri_legend_chip(ri_green,  "Male")
#    ri_legend_chip(ri_violet, "Female")
ri_legend_chip <- function(color, label) {
  tagList(
    tags$span(style = paste0(
      "display:inline-block; width:10px; height:10px; border-radius:2px; ",
      "background:", color, "; margin-right:4px; vertical-align:middle;"
    )),
    tags$span(label, style = "vertical-align:middle;")
  )
}


# ── ri_legend_row() ───────────────────────────────────────────
#  Horizontal row of legend chips. Put inside ri_chart_card()
#  using the `...` argument, before the plotlyOutput.
#
#  Example:
#    ri_chart_card(
#      title    = "Wages by Gender",
#      output_id = "wages_plot",
#      ri_legend_row(
#        ri_legend_chip(ri_green,  "Male"),
#        ri_legend_chip(ri_violet, "Female")
#      )
#    )
ri_legend_row <- function(...) {
  div(class = "legend-row", ...)
}


# ── ri_filter_sidebar() ───────────────────────────────────────
#  A 200px sidebar for filters, designed for use alongside a chart.
#  Wrap it + the chart together in ri_explore_layout().
#
#  Use ri_filter_label() for section headings inside the sidebar.
#  Standard Shiny inputs (selectInput, checkboxGroupInput, sliderInput,
#  textInput) go directly inside and will be styled automatically.
#
#  Example:
#    ri_filter_sidebar(
#      ri_filter_label("Industry"),
#      selectInput("ind", NULL, choices = c("All", "Healthcare")),
#      ri_filter_label("Education"),
#      checkboxGroupInput("edu", NULL,
#        choices = c("Less than BA", "BA or higher"),
#        selected = c("Less than BA", "BA or higher"))
#    )
ri_filter_sidebar <- function(...) {
  div(class = "explore-sidebar", ...)
}


# ── ri_filter_label() ─────────────────────────────────────────
#  A section heading inside a filter sidebar: small caps, green underline.
#
#  Arguments:
#    label    The filter section name (e.g. "Industry", "Education Level")
#    tooltip  Optional hover tooltip text (adds a grey ? icon)
#
#  Example:
#    ri_filter_label("Education Level",
#      tooltip = "Typical entry-level education required for the occupation")
ri_filter_label <- function(label, tooltip = NULL) {
  tip_el <- if (!is.null(tooltip)) {
    tags$span(
      class    = "tip-icon",
      `data-tip` = tooltip,
      "?"
    )
  }
  div(class = "filter-label", label, tip_el)
}


# ── ri_explore_layout() ───────────────────────────────────────
#  Side-by-side layout: filter sidebar on the left, chart on the right.
#  Stacks vertically on mobile.
#
#  Arguments:
#    sidebar  Result of ri_filter_sidebar(...)
#    chart    Result of ri_chart_card(...) or any other div
#
#  Example:
#    ri_explore_layout(
#      sidebar = ri_filter_sidebar(...),
#      chart   = ri_chart_card(...)
#    )
ri_explore_layout <- function(sidebar, chart) {
  div(class = "explore-wrap",
    sidebar,
    div(style = "flex:1; min-width:0;", chart)
  )
}


# ── ri_footer() ───────────────────────────────────────────────
#  Centered footer with source attribution.
#
#  Arguments:
#    ...  One or more text strings or HTML elements
#
#  Example:
#    ri_footer("Roosevelt Institute · rooseveltinstitute.org · Source: CPS 2024")
ri_footer <- function(...) {
  div(class = "ri-footer", ...)
}

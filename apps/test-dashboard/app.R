# ============================================================
#  TEST DASHBOARD
#  A kitchen-sink app that exercises the whole template in one
#  place: all three layouts, every common chart type, every
#  palette rule from CLAUDE.md, and the UI helpers — so a change
#  to R/components.R, R/plotting.R, or roosevelt.css can be
#  checked against everything at once.
#
#  This is a test harness, NOT a starting point for a real app —
#  start from one of templates/explore, compare, or explain.
#  Things real templates leave out on purpose (tabs, a KPI
#  strip) are switched on here only so they get exercised.
#
#  Tabs:
#    Explain      explain layout: opt-in KPI strip + hero chart
#                 + supporting pair
#    Compare      compare layout: pill filter row + 2x2 grid
#    Explore      explore layout: filter sidebar + one chart
#    Chart types  one card per chart type, each labeled with
#                 the color rule it demonstrates
#    Color & UX   palette swatches, >5 categories, cross-chart
#                 color consistency, highlight-on-interaction,
#                 and the styled data table
#
#  Run from the repo root:
#    Rscript sync-assets.R            # refresh R/ + www/ copies
#    shiny::runApp("test-dashboard")
#
#  All sample data below is randomly generated.
# ============================================================

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(stringr)

source("R/components.R")
source("R/plotting.R")
source("config.R")


# ============================================================
#  SAMPLE DATA
# ============================================================
set.seed(2026)   # keeps the sample data consistent across launches

years <- 2014:2025

# Ten industries — more than 5 on purpose, so the Color & UX tab can
# exercise ri_categorical_group()'s top-5 + "Other" fallback.
industries <- c(
  "Health care", "Retail trade", "Leisure & hospitality", "Manufacturing",
  "Professional services", "Education", "Construction", "Transportation",
  "Finance", "Public administration"
)
ind_size   <- c(22, 16, 14, 13, 12, 9, 8, 6, 5, 4)   # millions of workers, 2025
ind_growth <- runif(length(industries), -0.01, 0.03)  # annual growth rate

# ── Employment by industry and year ───────────────────────────
ind_trend <- expand.grid(year = years, industry = industries,
                         stringsAsFactors = FALSE)
i <- match(ind_trend$industry, industries)
ind_trend$employment <- round(
  ind_size[i] * (1 + ind_growth[i])^(ind_trend$year - 2025) *
    ifelse(ind_trend$year == 2020, 0.9, 1) *
    runif(nrow(ind_trend), 0.98, 1.02),
  2
)
ind_trend <- ind_trend %>%
  group_by(industry) %>%
  mutate(
    index      = 100 * employment / employment[year == 2014],
    chg_vs2019 = 100 * (employment / employment[year == 2019] - 1)
  ) %>%
  ungroup()

# ── Low-wage share (explain tab) ──────────────────────────────
low_wage_trend <- data.frame(
  year  = years,
  share = c(35.2, 34.9, 34.6, 34.1, 33.9, 33.6, 34.3, 34.0, 33.8, 33.6, 33.3, 33.1)
)
low_wage_by_ind <- data.frame(
  industry = industries,
  share    = c(31, 48, 62, 22, 14, 27, 19, 29, 11, 9)
)
wage_by_educ <- data.frame(
  year  = rep(years, times = 2),
  index = c(100, 101, 101, 102, 103, 104, 106, 105, 104, 105, 106, 107,
            100, 102, 104, 105, 107, 109, 113, 113, 112, 114, 116, 118),
  group = rep(c("Less than BA", "BA or higher"), each = length(years))
)

# ── Wages by race x gender x industry (compare tab) ───────────
races <- c("Black", "Hispanic", "Asian", "White")
wages <- expand.grid(industry = industries, race = races,
                     gender = c("Men", "Women"), stringsAsFactors = FALSE)
wages$wage <- round(
  runif(length(industries), 18, 42)[match(wages$industry, industries)] *
    c(Black = 0.82, Hispanic = 0.80, Asian = 1.12, White = 1)[wages$race] *
    ifelse(wages$gender == "Women", 0.84, 1) *
    runif(nrow(wages), 0.96, 1.04),
  2
)

# ── Education composition by industry and year (compare tab) ──
ed_comp <- expand.grid(industry = industries, year = 2021:2025,
                       stringsAsFactors = FALSE)
ed_comp$ba_share <- round(
  runif(length(industries), 18, 55)[match(ed_comp$industry, industries)] +
    0.6 * (ed_comp$year - 2021), 1
)
ed_comp <- bind_rows(
  ed_comp %>% transmute(industry, year, group = "Less than BA", share = 100 - ba_share),
  ed_comp %>% transmute(industry, year, group = "BA or higher", share = ba_share)
)

# ── Occupations (explore tab + highlight demo) ────────────────
n_occ <- 80
occ <- data.frame(
  occupation = sprintf("Occupation %02d", seq_len(n_occ)),
  industry   = sample(industries, n_occ, replace = TRUE, prob = ind_size),
  edu        = sample(educ_order, n_occ, replace = TRUE,
                      prob = c(10, 30, 8, 10, 8, 22, 8, 4)),
  stringsAsFactors = FALSE
)
occ$sector <- dplyr::case_when(
  occ$industry %in% c("Health care", "Education")        ~ "Care & education",
  occ$industry %in% c("Manufacturing", "Construction")   ~ "Goods-producing",
  TRUE                                                   ~ "Other services"
)
occ$wage       <- round(26000 + 9500 * match(occ$edu, educ_order) + rnorm(n_occ, 0, 9000), -2)
occ$growth     <- round(rnorm(n_occ, 3, 4), 1)
occ$employment <- round(rlnorm(n_occ, log(400), 0.8))   # thousands
# Bubble diameter in px, scaled once on the full data so a bubble keeps
# its size whatever the filters. (Mapping plot_ly(size = ~employment)
# instead works too, but plotly-R warns about line.width on every render.)
occ$bubble_px  <- 8 + 30 * sqrt(occ$employment / max(occ$employment))

# ── Color mappings computed ONCE, from the full data ──────────
#  (CLAUDE.md: cross-chart consistency. Every chart below that shows
#  one of these variables reuses the mapping, whatever it's filtered to.)

# Sector: 3 categories, ranked by total employment so the biggest gets green.
sector_rank   <- names(sort(tapply(occ$employment, occ$sector, sum), decreasing = TRUE))
sector_colors <- setNames(ri_pal()(length(sector_rank)), sector_rank)
occ$sector    <- factor(occ$sector, levels = sector_rank)
occ$edu       <- factor(occ$edu, levels = educ_order)

# Industry: 10 categories -> top 5 by 2025 employment + a grey "Other".
ind_2025       <- ind_trend %>% filter(year == 2025)
industry_group <- ri_categorical_group(ind_2025$industry, weight = ind_2025$employment)
group_industry <- function(x) {
  factor(ifelse(x %in% industry_group$kept, x, "Other"),
         levels = names(industry_group$colors))
}

# Binary variables: the 2-category default (green + blue). Less than BA
# is the bigger group, so it takes green.
edu_binary_colors    <- setNames(ri_palettes$green_blue_binary, c("Less than BA", "BA or higher"))
gender_binary_colors <- setNames(ri_palettes$green_blue_binary, c("Men", "Women"))


# ============================================================
#  SMALL CHART HELPERS (local to this app)
# ============================================================

# Category axis for horizontal bars: no gridlines, labels breathe.
hbar_yaxis <- function(...) {
  ri_axis("", showgrid = FALSE, automargin = TRUE, ticklen = 8,
          ticks = "outside", tickcolor = "transparent", ...)
}

# A compact horizontal legend above the plot area, for charts that use
# Plotly's own legend rather than ri_legend_row() chips.
top_legend <- function(title = NULL) {
  list(
    orientation = "h", x = 0, y = 1.02, xanchor = "left", yanchor = "bottom",
    font  = list(size = 9, family = RI_FONT_FAMILY, color = ri_text),
    traceorder = "normal",   # stacked traces otherwise list in reverse
    title = list(text = if (is.null(title)) "" else paste0("<b>", title, "</b>  "),
                 font = list(size = 9, family = RI_FONT_FAMILY))
  )
}

# Turns an ri_palettes ordinal/diverging ramp into a Plotly colorscale,
# for continuous color (heatmaps, gradient-filled bars).
ri_colorscale <- function(palette, n = 11, reverse = FALSE) {
  cols <- ri_pal(palette, reverse = reverse)(n)
  lapply(seq_len(n), function(k) list((k - 1) / (n - 1), cols[k]))
}

# A plain bordered card for non-chart content (swatches, tables).
info_card <- function(title, subtitle = NULL, ...) {
  div(class = "ch-card",
    tags$h3(title),
    if (!is.null(subtitle)) div(class = "sublabel", subtitle),
    ...
  )
}


# ============================================================
#  UI
# ============================================================
ui <- fluidPage(

  theme = bslib::bs_theme(
    version    = 5,
    primary    = ri_green,
    secondary  = ri_violet,
    base_font  = bslib::font_google("Montserrat", wght = c(300, 400, 600, 700))
  ),

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "roosevelt.css")
  ),

  ri_header(title = app_title, subtitle = app_subtitle),

  # Tabs, per snippets/new-tab.R: this replaces the templates'
  # nav-wrapper/padding block, and the script moves the pills into the header.
  ri_tab_nav_script("main_tabs"),
  div(class = "nav-wrapper",
    tabsetPanel(id = "main_tabs", type = "pills",

      # ── EXPLAIN ──────────────────────────────────────────────
      tabPanel("Explain",

        # Opt-in KPI strip (snippets/kpi-strip.R) — not in the explain
        # template by default; here only to exercise the helper.
        ri_stat_strip(
          ri_stat_card(sprintf("%.1f%%", tail(low_wage_trend$share, 1)), "Low-wage share, 2025"),
          ri_stat_card(sprintf("%.0fM", sum(ind_2025$employment)), "Workers, 10 industries"),
          ri_stat_card(sprintf("%d pts", max(low_wage_by_ind$share) - min(low_wage_by_ind$share)),
                       "Gap across industries"),
          ri_stat_card(sprintf("+%d%%", tail(wage_by_educ$index, 1) - 100), "BA wage growth since 2014")
        ),

        ri_chart_card(
          title       = "The low-wage share is falling — slowly",
          output_id   = "hero_chart",
          height      = "400px",
          subtitle    = "Share of workers earning under two-thirds of the median wage · 2020 shaded",
          source_note = "Source: sample data"
        ),

        layout_column_wrap(width = "320px", gap = "12px",
          ri_chart_card(
            title       = "Where low wages concentrate",
            output_id   = "support_chart1",
            height      = "300px",
            subtitle    = "Low-wage share by industry, 2025 · biggest on top",
            source_note = "Source: sample data"
          ),
          ri_chart_card(
            title       = "Not everyone shares in the gains",
            output_id   = "support_chart2",
            height      = "300px",
            subtitle    = "Median wage index (2014 = 100) by education",
            ri_legend_row(
              ri_legend_chip(edu_binary_colors[["Less than BA"]], "Less than BA"),
              ri_legend_chip(edu_binary_colors[["BA or higher"]], "BA or higher")
            ),
            source_note = "Source: sample data"
          )
        )
      ),

      # ── COMPARE ──────────────────────────────────────────────
      tabPanel("Compare",

        div(class = "ind-selector",
          div(class = "sector-label", selector_label),
          selectInput("selector", label = NULL, choices = industries,
                      selected = industries[1], width = selector_width)
        ),

        layout_column_wrap(width = "320px", gap = "12px",
          ri_chart_card(
            title       = "Median hourly wage by race and gender",
            output_id   = "cmp_chart1",
            height      = "300px",
            subtitle    = "Grouped bar · 2 categories → green + blue",
            ri_legend_row(
              ri_legend_chip(gender_binary_colors[["Men"]],   "Men"),
              ri_legend_chip(gender_binary_colors[["Women"]], "Women")
            ),
            source_note = "Source: sample data"
          ),
          ri_chart_card(
            title       = "Occupations: wage vs. growth",
            output_id   = "cmp_chart2",
            height      = "300px",
            subtitle    = "Scatter, not colored by group → plain green",
            source_note = "Source: sample data"
          )
        ),

        layout_column_wrap(width = "320px", gap = "12px",
          ri_chart_card(
            title       = "Employment index",
            output_id   = "cmp_chart3",
            height      = "280px",
            subtitle    = "2014 = 100 · dotted line marks the baseline",
            source_note = "Source: sample data"
          ),
          ri_chart_card(
            title       = "Education composition",
            output_id   = "cmp_chart4",
            height      = "280px",
            subtitle    = "Stacked bar · share of workers, %",
            ri_legend_row(
              ri_legend_chip(edu_binary_colors[["Less than BA"]], "Less than BA"),
              ri_legend_chip(edu_binary_colors[["BA or higher"]], "BA or higher")
            ),
            source_note = "Source: sample data"
          )
        )
      ),

      # ── EXPLORE ──────────────────────────────────────────────
      tabPanel("Explore",
        ri_explore_layout(
          sidebar = ri_filter_sidebar(
            ri_filter_label("Search"),
            textInput("search_text", label = NULL, placeholder = "e.g. Occupation 1"),

            ri_filter_label("Sector"),
            selectInput("filter_sector", label = NULL,
                        choices = c("All sectors" = "all", setNames(sector_rank, sector_rank)),
                        selected = "all"),

            ri_filter_label("Education",
                            tooltip = "Typical entry-level education required for the occupation"),
            checkboxGroupInput("filter_educ", label = NULL,
                               choices = educ_order, selected = educ_order),

            ri_filter_label("Median wage"),
            sliderInput("filter_wage", label = NULL,
                        min = 0, max = 150000, value = c(0, 150000),
                        step = 5000, pre = "$", sep = ","),

            ri_filter_label("Color points by",
                            tooltip = "Sector is nominal (3 categories); education uses educ_palette"),
            radioButtons("color_by", label = NULL,
                         choices = c("Sector" = "sector", "Education" = "edu"),
                         selected = "sector"),

            actionButton("reset_filters", "Reset filters", class = "btn-sm btn-outline-primary")
          ),

          chart = ri_chart_card(
            title       = "Occupations by wage and projected growth",
            output_id   = "explore_plot",
            height      = "560px",
            subtitle    = "Bubble size = employment · hover a bubble for details",
            div(class = "sublabel", textOutput("explore_count", inline = TRUE)),
            source_note = "Source: sample data"
          )
        )
      ),

      # ── CHART TYPES ──────────────────────────────────────────
      tabPanel("Chart types",
        layout_column_wrap(width = "320px", gap = "12px",
          ri_chart_card("Multi-line", "g_multiline", "280px",
                        subtitle = "3 categories → ri_pal()(3), ranked so the biggest is green"),
          ri_chart_card("Vertical bar (ri_bar_chart)", "g_vbar", "280px",
                        subtitle = "Single series → green · text labels via text_col"),
          ri_chart_card("Stacked bar", "g_stacked", "280px",
                        subtitle = "4 nominal categories → ri_pal()(4), biggest region green"),
          ri_chart_card("100% stacked, ordinal", "g_ordinal", "280px",
                        subtitle = "5 ordered levels → ri_pal(\"ordinal_green\")(5)"),
          ri_chart_card("Donut", "g_donut", "280px",
                        subtitle = "4 categories, sorted biggest-first → green on the biggest slice"),
          ri_chart_card("Stacked area", "g_area", "280px",
                        subtitle = "3 categories → ri_pal()(3)"),
          ri_chart_card("Scatter", "g_scatter", "280px",
                        subtitle = "Not colored by group → green"),
          ri_chart_card("Histogram", "g_hist", "280px",
                        subtitle = "Single series → green"),
          ri_chart_card("Box plot, ordinal", "g_box", "280px",
                        subtitle = "Age brackets → ri_pal(\"ordinal_green\")(4)"),
          ri_chart_card("Dumbbell (before / after)", "g_dumbbell", "280px",
                        subtitle = "2 categories → opt-in green_violet_binary"),
          ri_chart_card("Diverging bar", "g_diverging", "280px",
                        subtitle = "Wage vs. national average → diverging_green_white_violet"),
          ri_chart_card("Heatmap", "g_heatmap", "280px",
                        subtitle = "Employment change vs. 2019, % → diverging, centered on 0")
        )
      ),

      # ── COLOR & UX ───────────────────────────────────────────
      tabPanel("Color & UX",

        info_card(
          "Palettes",
          "Every ri_palettes entry as defined, plus how ri_pal() handles other counts",
          uiOutput("palette_swatches")
        ),

        layout_column_wrap(width = "320px", gap = "12px",
          ri_chart_card(
            "More than 5 categories", "ux_other", "300px",
            subtitle    = "10 industries → ri_categorical_group(): top 5 by employment + grey \"Other\"",
            source_note = "Source: sample data"
          ),
          ri_chart_card(
            "Same colors, filtered", "ux_consistent", "300px",
            subtitle    = "Uses the same kept/colors as the chart beside it — change the year, colors don't move",
            source_note = "Source: sample data",
            selectInput("ux_year", label = NULL, choices = rev(years), selected = 2025, width = "120px")
          )
        ),

        layout_column_wrap(width = "320px", gap = "12px",
          ri_chart_card(
            "Highlight on interaction", "ux_highlight", "340px",
            subtitle    = "Click a point to highlight its industry · double-click to clear",
            source_note = "Source: sample data",
            selectInput("hl_industry", label = NULL,
                        choices = c("No highlight" = "none", setNames(industries, industries)),
                        selected = "none", width = "220px")
          ),
          info_card(
            "Workforce composition",
            "Styled data table (.ri-demo-tbl) with section headers",
            selectInput("tbl_industry", label = NULL, choices = industries,
                        selected = industries[1], width = "220px"),
            uiOutput("demo_table")
          )
        )
      )
    )
  ),

  ri_footer(footer_text)
)


# ============================================================
#  SERVER
# ============================================================
server <- function(input, output, session) {

  # ══ EXPLAIN ═══════════════════════════════════════════════

  output$hero_chart <- renderPlotly({
    last <- tail(low_wage_trend, 1)
    plot_ly(
      low_wage_trend, x = ~year, y = ~share,
      type = "scatter", mode = "lines+markers",
      line   = list(color = ri_green, width = 3),
      marker = list(color = ri_green, size = 8),
      hovertemplate = ri_hover_template("%{x}", "%{y:.1f}%", note = "of workers earn low wages")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 50, r = 70, t = 10, b = 40)),
        list(
          xaxis  = ri_axis("", showgrid = FALSE, tickformat = "d"),
          yaxis  = ri_axis("Low-wage share", ticksuffix = "%"),
          shapes = list(list(
            type = "rect", xref = "x", yref = "paper",
            x0 = 2019.5, x1 = 2020.5, y0 = 0, y1 = 1,
            fillcolor = RI_GRID_COLOR, line = list(width = 0), layer = "below"
          )),
          annotations = list(list(
            x = last$year, y = last$share, xanchor = "left", xshift = 8,
            text = sprintf("<b>%.1f%%</b>", last$share), showarrow = FALSE,
            font = list(size = 12, color = ri_green, family = RI_FONT_FAMILY)
          ))
        )
      )) %>%
      ri_config()
  })

  output$support_chart1 <- renderPlotly({
    df <- low_wage_by_ind %>%
      mutate(industry = factor(industry, levels = industry[order(share)]))   # biggest on top
    plot_ly(
      df, x = ~share, y = ~industry, type = "bar", orientation = "h",
      marker = list(color = ri_green),
      text = ~paste0(share, "%"), textposition = "outside", cliponaxis = FALSE,
      textfont = list(size = RI_FONT_SIZE, color = ri_grey),
      hovertemplate = ri_hover_template("%{y}", "%{x}%", note = "low-wage share")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 10, r = 40, t = 10, b = 10)),
        list(xaxis = ri_axis_blank(), yaxis = hbar_yaxis())
      )) %>%
      ri_config()
  })

  output$support_chart2 <- renderPlotly({
    plot_ly(
      wage_by_educ, x = ~year, y = ~index,
      color = ~group, colors = edu_binary_colors,
      type = "scatter", mode = "lines+markers",
      hovertemplate = ri_hover_template("%{x}", "%{y}", note = "wage index")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 50, r = 20, t = 10, b = 40)),
        list(
          showlegend = FALSE,
          xaxis = ri_axis("", showgrid = FALSE, tickformat = "d"),
          yaxis = ri_axis("Index")
        )
      )) %>%
      ri_config()
  })


  # ══ COMPARE ═══════════════════════════════════════════════

  sel <- reactive({
    req(input$selector)
    input$selector
  })

  output$cmp_chart1 <- renderPlotly({
    df <- wages %>% filter(industry == sel())
    race_order <- df %>% group_by(race) %>% summarise(w = mean(wage)) %>% arrange(w) %>% pull(race)
    df$race   <- factor(df$race, levels = race_order)   # highest-paid on top
    df$gender <- factor(df$gender, levels = names(gender_binary_colors))

    # Every race has both genders, so "group" mode keeps bars centered.
    plot_ly(
      df, x = ~wage, y = ~race, color = ~gender, colors = gender_binary_colors,
      type = "bar", orientation = "h",
      hovertemplate = ri_hover_template("%{y}", "$%{x:.2f}", note = "median hourly wage")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 10, r = 20, t = 10, b = 30)),
        list(
          barmode = "group", showlegend = FALSE,
          xaxis = ri_axis("", tickprefix = "$"),
          yaxis = hbar_yaxis()
        )
      )) %>%
      ri_config()
  })

  output$cmp_chart2 <- renderPlotly({
    df <- occ %>% filter(industry == sel())
    validate(need(nrow(df) > 0, "No occupations in this industry."))
    plot_ly(
      df, x = ~wage, y = ~growth, text = ~occupation,
      type = "scatter", mode = "markers",
      marker = list(color = ri_green, size = 10, opacity = 0.85,
                    line = list(width = 0.5, color = "white")),
      hovertemplate = ri_hover_template("%{text}", "$%{x:,.0f}", note = "%{y:.1f}% projected growth")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 50, r = 20, t = 10, b = 50)),
        list(
          xaxis = ri_axis("Median annual wage", tickprefix = "$"),
          yaxis = ri_axis("Projected growth", ticksuffix = "%")
        )
      )) %>%
      ri_config()
  })

  output$cmp_chart3 <- renderPlotly({
    df <- ind_trend %>% filter(industry == sel())
    plot_ly(
      df, x = ~year, y = ~index, type = "scatter", mode = "lines",
      line = list(color = ri_green, width = 2.5),
      hovertemplate = ri_hover_template("%{x}", "%{y:.1f}", note = "employment index")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 50, r = 20, t = 10, b = 40)),
        list(
          xaxis  = ri_axis("", showgrid = FALSE, tickformat = "d"),
          yaxis  = ri_axis("Index"),
          shapes = list(list(
            type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 100, y1 = 100,
            line = list(color = ri_lightest_grey, width = 1.5, dash = "dot")
          ))
        )
      )) %>%
      ri_config()
  })

  output$cmp_chart4 <- renderPlotly({
    df <- ed_comp %>%
      filter(industry == sel()) %>%
      mutate(group = factor(group, levels = names(edu_binary_colors)))
    plot_ly(
      df, x = ~factor(year), y = ~share, color = ~group, colors = edu_binary_colors,
      type = "bar",
      hovertemplate = ri_hover_template("%{x}", "%{y:.1f}%")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 50, r = 20, t = 10, b = 40)),
        list(
          barmode = "stack", showlegend = FALSE,
          xaxis = ri_axis("", showgrid = FALSE),
          yaxis = ri_axis("Share of workers", ticksuffix = "%", range = c(0, 100))
        )
      )) %>%
      ri_config()
  })


  # ══ EXPLORE ═══════════════════════════════════════════════

  observeEvent(input$reset_filters, {
    updateTextInput(session, "search_text", value = "")
    updateSelectInput(session, "filter_sector", selected = "all")
    updateCheckboxGroupInput(session, "filter_educ", selected = educ_order)
    updateSliderInput(session, "filter_wage", value = c(0, 150000))
  })

  filtered_occ <- reactive({
    df <- occ
    q  <- trimws(input$search_text)
    if (nchar(q) > 0)
      df <- df %>% filter(str_detect(occupation, regex(q, ignore_case = TRUE)))
    if (input$filter_sector != "all")
      df <- df %>% filter(sector == input$filter_sector)
    df %>% filter(
      edu %in% input$filter_educ,
      wage >= input$filter_wage[1], wage <= input$filter_wage[2]
    )
  })

  output$explore_count <- renderText({
    sprintf("Showing %d of %d occupations", nrow(filtered_occ()), nrow(occ))
  })

  output$explore_plot <- renderPlotly({
    df <- filtered_occ()
    validate(need(nrow(df) > 0, "No occupations match these filters — try widening them."))

    by_edu <- identical(input$color_by, "edu")
    # Colors come from the full-data mappings, so a category keeps its
    # color whatever the filters leave on screen.
    colors <- if (by_edu) educ_palette[educ_order] else sector_colors

    plot_ly(
      df, x = ~wage, y = ~growth,
      color = if (by_edu) ~edu else ~sector, colors = colors,
      text = ~occupation,
      type = "scatter", mode = "markers",
      marker = list(size = ~bubble_px, opacity = 0.85,
                    line = list(width = 0.5, color = "white")),
      hovertemplate = ri_hover_template("%{text}", "$%{x:,.0f}", note = "%{y:.1f}% projected growth")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 60, r = 20, t = 40, b = 60)),
        list(
          xaxis  = ri_axis("Median annual wage", tickprefix = "$"),
          yaxis  = ri_axis("Projected growth, 2025–35", ticksuffix = "%"),
          legend = top_legend(if (by_edu) "Education" else "Sector")
        )
      )) %>%
      ri_config()
  })


  # ══ CHART TYPES ═══════════════════════════════════════════

  output$g_multiline <- renderPlotly({
    picks <- c("Health care", "Retail trade", "Manufacturing")
    df    <- ind_trend %>% filter(industry %in% picks)
    rank  <- df %>% filter(year == 2025) %>% arrange(desc(employment)) %>% pull(industry)
    df$industry <- factor(df$industry, levels = rank)
    plot_ly(
      df, x = ~year, y = ~employment, color = ~industry,
      colors = setNames(ri_pal()(3), rank),
      type = "scatter", mode = "lines",
      line = list(width = 2.5),
      hovertemplate = ri_hover_template("%{x}", "%{y:.1f}M", note = "workers")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 45, r = 10, t = 45, b = 30)),
        list(
          legend = top_legend(),
          xaxis  = ri_axis("", showgrid = FALSE, tickformat = "d"),
          yaxis  = ri_axis("Workers (M)")
        )
      )) %>%
      ri_config()
  })

  output$g_vbar <- renderPlotly({
    df <- data.frame(
      industry = c("Public admin.", "Education", "Transportation", "Construction",
                   "Manufacturing", "Health care"),
      rate     = c(31.2, 32.9, 16.1, 10.3, 7.8, 6.9)
    )
    df$industry <- factor(df$industry, levels = df$industry[order(-df$rate)])
    df$label    <- paste0(df$rate, "%")
    ri_bar_chart(df, x = "industry", y = "rate", text_col = "label") %>%
      style(marker = list(color = ri_green),
            hovertemplate = ri_hover_template("%{x}", "%{y}%", note = "union membership")) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 40, r = 10, t = 20, b = 60)),
        list(
          xaxis = ri_axis("", showgrid = FALSE),
          yaxis = ri_axis("", ticksuffix = "%", range = c(0, 40))
        )
      )) %>%
      ri_config()
  })

  output$g_stacked <- renderPlotly({
    regions <- c("Northeast", "Midwest", "South", "West")
    df <- expand.grid(industry = industries[1:5], region = regions, stringsAsFactors = FALSE)
    df$workers <- round(ind_size[match(df$industry, industries)] *
                          c(Northeast = 0.18, Midwest = 0.21, South = 0.38, West = 0.23)[df$region] *
                          runif(nrow(df), 0.9, 1.1), 2)
    rank <- names(sort(tapply(df$workers, df$region, sum), decreasing = TRUE))
    df$region   <- factor(df$region, levels = rank)
    df$industry <- factor(df$industry, levels = industries[5:1])
    plot_ly(
      df, x = ~workers, y = ~industry, color = ~region,
      colors = setNames(ri_pal()(4), rank),
      type = "bar", orientation = "h",
      hovertemplate = ri_hover_template("%{y}", "%{x:.1f}M", note = "workers")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 10, r = 10, t = 50, b = 30)),
        list(
          barmode = "stack", legend = top_legend(),
          xaxis   = ri_axis("", ticksuffix = "M"),
          yaxis   = hbar_yaxis()
        )
      )) %>%
      ri_config()
  })

  output$g_ordinal <- renderPlotly({
    levels_sat <- c("Very dissatisfied", "Dissatisfied", "Neutral", "Satisfied", "Very satisfied")
    jobs <- c("Hourly, part-time", "Hourly, full-time", "Salaried")
    shares <- rbind(
      c(14, 22, 26, 25, 13),
      c(9, 17, 24, 32, 18),
      c(5, 11, 20, 38, 26)
    )
    df <- data.frame(
      job   = factor(rep(jobs, each = 5), levels = rev(jobs)),
      level = factor(rep(levels_sat, times = 3), levels = levels_sat),
      share = as.vector(t(shares))
    )
    plot_ly(
      df, x = ~share, y = ~job, color = ~level,
      colors = setNames(ri_pal("ordinal_green")(5), levels_sat),
      type = "bar", orientation = "h",
      hovertemplate = ri_hover_template("%{y}", "%{x}%", note = "%{fullData.name}")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 10, r = 10, t = 75, b = 30)),
        list(
          barmode = "stack", legend = top_legend(),
          xaxis   = ri_axis("", ticksuffix = "%", range = c(0, 100)),
          yaxis   = hbar_yaxis()
        )
      )) %>%
      ri_config()
  })

  output$g_donut <- renderPlotly({
    df <- data.frame(
      class   = c("Government", "Private for-profit", "Self-employed", "Private nonprofit"),
      workers = c(23.1, 112.4, 9.8, 13.6)
    ) %>% arrange(desc(workers))   # biggest first -> green
    plot_ly(
      df, labels = ~class, values = ~workers, type = "pie", hole = 0.55,
      sort = FALSE, direction = "clockwise",
      marker = list(colors = ri_pal()(nrow(df)), line = list(color = "white", width = 2)),
      textinfo = "percent", textfont = list(family = RI_FONT_FAMILY, size = 10, color = "white"),
      hovertemplate = ri_hover_template("%{label}", "%{value:.1f}M", note = "%{percent} of workers")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 10, r = 10, t = 10, b = 10)),
        list(legend = list(font = list(size = 9, family = RI_FONT_FAMILY)))
      )) %>%
      ri_config()
  })

  output$g_area <- renderPlotly({
    picks <- c("Professional services", "Education", "Construction")
    df    <- ind_trend %>% filter(industry %in% picks)
    rank  <- df %>% group_by(industry) %>% summarise(t = sum(employment)) %>%
      arrange(desc(t)) %>% pull(industry)
    cols  <- setNames(ri_pal()(3), rank)
    p <- plot_ly()
    for (ind in rank) {   # one trace per series so fill color is set explicitly
      d <- df %>% filter(industry == ind)
      p <- add_trace(p, data = d, x = ~year, y = ~employment, name = ind,
                     type = "scatter", mode = "lines", stackgroup = "one",
                     line = list(color = cols[[ind]], width = 1),
                     fillcolor = cols[[ind]],
                     hovertemplate = ri_hover_template("%{x}", "%{y:.1f}M", note = ind))
    }
    p %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 45, r = 10, t = 45, b = 30)),
        list(
          legend = top_legend(),
          xaxis  = ri_axis("", showgrid = FALSE, tickformat = "d"),
          yaxis  = ri_axis("Workers (M)")
        )
      )) %>%
      ri_config()
  })

  output$g_scatter <- renderPlotly({
    df <- ind_2025 %>% left_join(low_wage_by_ind, by = "industry") %>%
      left_join(wages %>% group_by(industry) %>% summarise(wage = mean(wage)), by = "industry")
    plot_ly(
      df, x = ~wage, y = ~share, text = ~industry,
      type = "scatter", mode = "markers",
      marker = list(color = ri_green, size = 11, opacity = 0.85,
                    line = list(width = 0.5, color = "white")),
      hovertemplate = ri_hover_template("%{text}", "%{y}%", note = "low-wage share")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 50, r = 10, t = 10, b = 45)),
        list(
          xaxis = ri_axis("Mean hourly wage", tickprefix = "$"),
          yaxis = ri_axis("Low-wage share", ticksuffix = "%")
        )
      )) %>%
      ri_config()
  })

  output$g_hist <- renderPlotly({
    plot_ly(
      occ, x = ~wage, type = "histogram", nbinsx = 20,
      marker = list(color = ri_green, line = list(color = "white", width = 1)),
      hovertemplate = ri_hover_template("%{x}", "%{y}", note = "occupations")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 45, r = 10, t = 10, b = 45)),
        list(
          bargap = 0.02,
          xaxis  = ri_axis("Median annual wage", tickprefix = "$", showgrid = FALSE),
          yaxis  = ri_axis("Occupations")
        )
      )) %>%
      ri_config()
  })

  output$g_box <- renderPlotly({
    ages <- c("16–24", "25–34", "35–54", "55+")
    df <- data.frame(
      age  = factor(rep(ages, each = 60), levels = ages),
      wage = round(c(rnorm(60, 15, 3), rnorm(60, 23, 6), rnorm(60, 29, 8), rnorm(60, 27, 8)), 2)
    )
    plot_ly(
      df, x = ~age, y = ~wage, color = ~age,
      colors = setNames(ri_pal("ordinal_green")(4), ages),
      type = "box", boxpoints = FALSE
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 45, r = 10, t = 10, b = 40)),
        list(
          showlegend = FALSE,
          xaxis = ri_axis("", showgrid = FALSE),
          yaxis = ri_axis("Hourly wage", tickprefix = "$")
        )
      )) %>%
      ri_config()
  })

  output$g_dumbbell <- renderPlotly({
    # Opt-in violet: green vs. violet stays distinct for every type of
    # color vision within a 2-category chart (see CLAUDE.md) — picked
    # knowingly here to exercise the alternative, not as the default.
    cols <- setNames(ri_palettes$green_violet_binary, c("2025", "2019"))
    df <- ind_trend %>%
      filter(year %in% c(2019, 2025), industry %in% industries[1:6]) %>%
      select(industry, year, employment) %>%
      tidyr_wide()
    df$industry <- factor(df$industry, levels = df$industry[order(df$y2025)])
    plot_ly(df) %>%
      add_segments(x = ~y2019, xend = ~y2025, y = ~industry, yend = ~industry,
                   line = list(color = ri_lightest_grey, width = 3),
                   hoverinfo = "none", showlegend = FALSE) %>%
      add_markers(x = ~y2019, y = ~industry, name = "2019",
                  marker = list(color = cols[["2019"]], size = 11),
                  hovertemplate = ri_hover_template("%{y}", "%{x:.1f}M", note = "2019")) %>%
      add_markers(x = ~y2025, y = ~industry, name = "2025",
                  marker = list(color = cols[["2025"]], size = 11),
                  hovertemplate = ri_hover_template("%{y}", "%{x:.1f}M", note = "2025")) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 10, r = 10, t = 35, b = 30)),
        list(
          legend = top_legend(),
          xaxis  = ri_axis("", ticksuffix = "M"),
          yaxis  = hbar_yaxis()
        )
      )) %>%
      ri_config()
  })

  output$g_diverging <- renderPlotly({
    df <- data.frame(
      division = c("New England", "Middle Atlantic", "East North Central",
                   "West North Central", "South Atlantic", "East South Central",
                   "West South Central", "Mountain", "Pacific"),
      diff = c(14, 11, -2, -6, -1, -13, -5, -3, 12)
    )
    df$division <- factor(df$division, levels = df$division[order(df$diff)])
    m <- max(abs(df$diff))
    plot_ly(
      df, x = ~diff, y = ~division, type = "bar", orientation = "h",
      # reverse = TRUE puts green on the "above average" side
      marker = list(color = df$diff, cmin = -m, cmax = m, showscale = FALSE,
                    colorscale = ri_colorscale("diverging_green_white_violet", reverse = TRUE),
                    line = list(color = RI_GRID_COLOR, width = 1)),
      hovertemplate = ri_hover_template("%{y}", "%{x}%", note = "vs. national median wage")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 10, r = 10, t = 10, b = 30)),
        list(
          xaxis = ri_axis("", ticksuffix = "%", zeroline = TRUE,
                          zerolinecolor = ri_lightest_grey, zerolinewidth = 1.5),
          yaxis = hbar_yaxis()
        )
      )) %>%
      ri_config()
  })

  output$g_heatmap <- renderPlotly({
    df  <- ind_trend %>% filter(year >= 2019)
    mat <- tapply(df$chg_vs2019, list(df$industry, df$year), identity)
    mat <- mat[rev(industries), , drop = FALSE]
    m   <- max(abs(mat))
    plot_ly(
      x = colnames(mat), y = rownames(mat), z = mat, type = "heatmap",
      colorscale = ri_colorscale("diverging_green_white_violet", reverse = TRUE),
      zmin = -m, zmax = m, xgap = 1, ygap = 1,
      colorbar = list(thickness = 8, len = 0.8, ticksuffix = "%",
                      tickfont = list(size = 9, family = RI_FONT_FAMILY, color = ri_grey)),
      hovertemplate = ri_hover_template("%{y}, %{x}", "%{z:.1f}%", note = "vs. 2019")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 10, r = 10, t = 10, b = 30)),
        list(
          xaxis = ri_axis("", showgrid = FALSE, type = "category"),
          yaxis = hbar_yaxis()
        )
      )) %>%
      ri_config()
  })


  # ══ COLOR & UX ════════════════════════════════════════════

  output$palette_swatches <- renderUI({
    swatch <- function(hex) {
      tags$span(title = hex, style = paste0(
        "display:inline-block; width:26px; height:18px; margin-right:2px; ",
        "border-radius:2px; border:1px solid ", RI_GRID_COLOR, "; background:", hex, ";"))
    }
    row <- function(name, cols, note = NULL) {
      div(style = "display:flex; align-items:center; gap:10px; margin-bottom:5px;",
        div(style = "width:230px; font-size:10px; font-weight:600;", name,
            if (!is.null(note)) tags$span(style = paste0("font-weight:400; color:", ri_grey, ";"),
                                          paste0(" — ", note))),
        div(lapply(cols, swatch))
      )
    }
    defaults <- c(categorical_cvd_safe = "default, 3–5 categories",
                  green_blue_binary    = "default, 2 categories",
                  ordinal_green        = "default ordinal",
                  diverging_green_white_violet = "default diverging")
    tagList(
      layout_column_wrap(width = "380px", gap = "12px",
        div(lapply(names(ri_palettes), function(nm)
          row(nm, ri_palettes[[nm]], if (nm %in% names(defaults)) defaults[[nm]]))),
        div(
          div(class = "sublabel", style = "font-weight:700;", "ri_pal() at other counts"),
          row("ri_pal()(2)", ri_pal()(2), "exact swatches"),
          row("ri_pal()(3)", ri_pal()(3), "exact swatches"),
          row("ri_pal(\"ordinal_green\")(8)", ri_pal("ordinal_green")(8), "interpolated"),
          row("ri_pal(\"diverging_…\")(9)", ri_pal("diverging_green_white_violet")(9), "interpolated"),
          row("educ_palette", unname(educ_palette[educ_order])),
          div(class = "sublabel", style = "font-weight:700; margin-top:10px;", "Neutrals"),
          row("ri_text / grey / light / lightest / grid",
              c(ri_text, ri_grey, ri_light_grey, ri_lightest_grey, RI_GRID_COLOR))
        )
      )
    )
  })

  output$ux_other <- renderPlotly({
    df <- ind_trend %>%
      filter(year >= 2019) %>%
      mutate(group = group_industry(industry)) %>%
      group_by(year, group) %>%
      summarise(employment = sum(employment), .groups = "drop")
    plot_ly(
      df, x = ~factor(year), y = ~employment, color = ~group,
      colors = industry_group$colors, type = "bar",
      hovertemplate = ri_hover_template("%{x}", "%{y:.1f}M", note = "%{fullData.name}")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 45, r = 10, t = 10, b = 30)),
        list(
          barmode = "stack",
          legend  = list(font = list(size = 9, family = RI_FONT_FAMILY), traceorder = "normal"),
          xaxis   = ri_axis("", showgrid = FALSE),
          yaxis   = ri_axis("Workers (M)")
        )
      )) %>%
      ri_config()
  })

  output$ux_consistent <- renderPlotly({
    df <- ind_trend %>%
      filter(year == as.integer(input$ux_year)) %>%
      mutate(group = group_industry(industry)) %>%
      group_by(group) %>%
      summarise(employment = sum(employment), .groups = "drop") %>%
      arrange(group)
    plot_ly(
      df, labels = ~group, values = ~employment, type = "pie", hole = 0.55,
      sort = FALSE, direction = "clockwise",
      marker = list(colors = unname(industry_group$colors[as.character(df$group)]),
                    line = list(color = "white", width = 2)),
      textinfo = "none",
      hovertemplate = ri_hover_template("%{label}", "%{value:.1f}M", note = "%{percent} of workers")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 10, r = 10, t = 10, b = 10)),
        list(legend = list(font = list(size = 9, family = RI_FONT_FAMILY)))
      )) %>%
      ri_config()
  })

  # Highlight-on-interaction: one trace with an explicit color per point,
  # so only the chosen industry is saturated and everything else is grey.
  # Clicks come back through a small onRender() handler rather than
  # event_data(), which warns at startup while the chart sits unrendered
  # on a hidden tab.
  observeEvent(input$hl_click, {
    updateSelectInput(session, "hl_industry",
                      selected = if (identical(input$hl_click, "none")) "none" else input$hl_click)
  })

  output$ux_highlight <- renderPlotly({
    hl <- input$hl_industry
    df <- occ %>%
      mutate(on = industry == hl) %>%
      arrange(on)   # highlighted points drawn last, on top
    plot_ly(
      df, x = ~wage, y = ~growth, customdata = ~industry, text = ~occupation,
      type = "scatter", mode = "markers",
      marker = list(color = ifelse(df$on, ri_green, ri_lightest_grey),
                    size = ifelse(df$on, 12, 9), opacity = 0.9,
                    line = list(width = 0.5, color = "white")),
      hovertemplate = ri_hover_template("%{text}", "%{customdata}",
                                        note = "$%{x:,.0f} · %{y:.1f}%")
    ) %>%
      ri_apply_layout(modifyList(
        ri_layout(margin = list(l = 50, r = 10, t = 10, b = 45)),
        list(
          xaxis = ri_axis("Median annual wage", tickprefix = "$"),
          yaxis = ri_axis("Projected growth", ticksuffix = "%")
        )
      )) %>%
      ri_config() %>%
      htmlwidgets::onRender("
        function(el) {
          el.on('plotly_click', function(ev) {
            Shiny.setInputValue('hl_click', ev.points[0].customdata, {priority: 'event'});
          });
          el.on('plotly_doubleclick', function() {
            Shiny.setInputValue('hl_click', 'none', {priority: 'event'});
          });
        }
      ")
  })

  output$demo_table <- renderUI({
    ind <- input$tbl_industry
    w   <- wages %>% filter(industry == ind)
    e   <- ed_comp %>% filter(industry == ind, year == 2025)
    section <- function(label) tags$tr(class = "tbl-section-hdr", tags$td(colspan = 2, label))
    line    <- function(label, value) tags$tr(tags$td(label), tags$td(value))
    tags$table(class = "ri-demo-tbl",
      tags$tbody(
        section("Workers"),
        line("Employment, 2025", sprintf("%.1fM", ind_2025$employment[ind_2025$industry == ind])),
        line("Low-wage share", paste0(low_wage_by_ind$share[low_wage_by_ind$industry == ind], "%")),
        section("Education"),
        lapply(seq_len(nrow(e)), function(k) line(e$group[k], sprintf("%.1f%%", e$share[k]))),
        section("Median hourly wage"),
        lapply(c("Men", "Women"), function(g)
          line(g, sprintf("$%.2f", mean(w$wage[w$gender == g]))))
      )
    )
  })
}

# Long -> wide for the dumbbell chart (base R, so tidyr isn't required).
tidyr_wide <- function(df) {
  out <- reshape(as.data.frame(df), idvar = "industry", timevar = "year", direction = "wide")
  names(out) <- sub("^employment\\.", "y", names(out))
  out
}

shinyApp(ui, server)

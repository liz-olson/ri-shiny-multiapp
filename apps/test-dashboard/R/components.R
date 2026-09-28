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
#  Greens follow the RI style guide's green scale, light -> dark:
#  35% tint, 70% tint, Roosevelt green, 35% shade, 70% shade.
ri_green_light    <- "#C4E2B7"   # style guide "35% tint"
ri_green_70       <- "#8CCA79"   # style guide "70% tint"
ri_green          <- "#58A618"   # Roosevelt Green
ri_dark_green     <- "#396C10"   # style guide "35% shade"
ri_darkest_green  <- "#1A3207"   # style guide "70% shade"
ri_violet      <- "#4C12A1"
ri_magenta     <- "#AA0061"
# Greys follow the style guide (Roosevelt Grey, Roosevelt Light Grey) and
# roosevelttheme's names ("grey", "light grey"); ri_lightest_grey is
# Roosevelt Grey at a 35% tint. Together with ri_text and RI_GRID_COLOR
# (R/plotting.R) these are the only neutrals the template uses — reach for
# one of them rather than a new hex. www/roosevelt.css mirrors them as
# CSS variables (--ri-text, --ri-grey, ...).
ri_text          <- "#1A1A1A"   # near-black: body text, titles, tick labels, tooltip text
ri_grey          <- "#5E6A71"   # Roosevelt Grey (Pantone 431)
ri_light_grey    <- "#7B868C"   # Roosevelt Light Grey (Pantone 430)
ri_lightest_grey <- "#C7CCCF"   # Roosevelt Grey, 35% tint
ri_blue        <- "#0067AC"
ri_orange      <- "#FF8200"
ri_red_tint    <- "#F47854"
ri_teal        <- "#008C95"
ri_teal_tint   <- "#38AEB8"

# ── Named chart palettes ───────────────────────────────────────
#  Each is a plain character vector of hex codes. ri_pal() is the usual
#  way in (see CLAUDE.md for which palette fits which chart), but you can
#  also pass one straight to plot_ly(), e.g. `colors = ri_palettes$hot`.
#
#  Defaults: categorical_cvd_safe is ri_pal()'s and ri_categorical_group()'s
#  default (3-5 categories); green_blue_binary is the 2-category default
#  (the same first two colors). ordinal_green and
#  diverging_green_white_violet are the go-to ordinal / diverging ramps.
#
#  Relationship to roosevelttheme::roosevelt_palettes
#  (github.com/liz-olson/roosevelttheme), the ggplot package — these
#  started from it but no longer mirror it exactly:
#    - Identical to the package: main, diverging_orange_blue,
#      diverging_green_violet, diverging_green_white_violet, hot, cool,
#      mixed, green_violet_binary.
#    - Same name, different colors: greens (uses the style guide's 35%
#      shade, ri_dark_green, instead of the package's darker "dark green",
#      which isn't in the style guide).
#    - Template only: categorical_cvd_safe, green_blue_binary,
#      green_violet_tint_binary, green_dark_binary, ordinal_green (and
#      the teal-tint / red-tint colors they use).
#    - Package only: light_green, green (single-color "palettes" — use
#      ri_green_light / ri_green directly instead).
#  ri_pal() also behaves differently from roosevelt_pal(): it defaults to
#  categorical_cvd_safe rather than main, and returns categorical
#  palettes' exact swatches instead of interpolating. So ggplot charts
#  made with the package's defaults won't match these colors.
.ri_theme_colors <- c(
  green             = ri_green,
  teal              = ri_teal,
  violet            = ri_violet,
  magenta           = ri_magenta,
  blue              = ri_blue,
  orange            = ri_orange,
  dark_blue         = "#10069F",
  red               = "#DA291C",
  grey              = ri_grey,
  light_grey        = ri_light_grey,
  dark_green        = ri_dark_green,
  darkest_green     = ri_darkest_green,
  green_tint        = ri_green_70,
  light_green       = ri_green_light,
  lightest_green    = "#F6FBF4",
  violet_tint_70    = "#766DB2",
  teal_tint_70      = ri_teal_tint,
  red_tint_70       = ri_red_tint
)

ri_palettes <- list(
  # Color-vision-deficiency-safe categorical palette — the default for
  # ri_pal() and any chart that doesn't ask for a specific palette by name.
  # Order matters here (see ri_pal(): categorical palettes are NOT
  # interpolated, so with N categories you get exactly the first N of these,
  # in this order): green, blue, magenta, teal-tint (ri_teal_tint — not
  # ri_teal), red-tint (ri_red_tint — not ri_orange). Red-tint is last
  # deliberately — it's not a core Roosevelt brand color, so it should
  # only show up once a chart genuinely needs all 5 categories, not
  # appear by default at 3 or 4.
  categorical_cvd_safe          = unname(.ri_theme_colors[c("green", "blue", "magenta", "teal_tint_70", "red_tint_70")]),
  main                          = unname(.ri_theme_colors[c("green", "teal", "blue", "violet", "magenta")]),
  diverging_orange_blue         = unname(.ri_theme_colors[c("orange", "dark_blue")]),
  diverging_green_violet        = unname(.ri_theme_colors[c("green", "teal", "violet")]),
  diverging_green_white_violet  = c(unname(.ri_theme_colors["green"]), "#FFFFFF", unname(.ri_theme_colors["violet"])),
  hot                           = unname(.ri_theme_colors[c("magenta", "orange", "red")]),
  cool                          = unname(.ri_theme_colors[c("violet", "dark_blue", "teal", "green")]),
  mixed                         = unname(.ri_theme_colors[c("magenta", "violet", "blue", "green", "orange")]),
  greens                        = unname(.ri_theme_colors[c("dark_green", "green", "light_green")]),
  # green_blue_binary (Roosevelt green + Roosevelt blue) is the default for
  # exactly 2 categories — the same first two colors as categorical_cvd_safe,
  # so 2-category and 3-5-category charts share colors (see CLAUDE.md for
  # why blue rather than violet). The others are opt-in alternatives:
  # full-saturation violet or violet at 70% tint when a chart wants
  # violet's look, dark green for two shades of one hue.
  green_blue_binary             = unname(.ri_theme_colors[c("green", "blue")]),
  green_violet_binary           = unname(.ri_theme_colors[c("green", "violet")]),
  green_violet_tint_binary      = unname(.ri_theme_colors[c("green", "violet_tint_70")]),
  green_dark_binary             = unname(.ri_theme_colors[c("green", "dark_green")]),
  # Single-hue light -> dark ramp for ORDINAL data (ordered levels, e.g.
  # an age bracket): the style guide's full green scale, 35% tint ->
  # 70% tint -> Roosevelt green -> 35% shade -> 70% shade. With 5 levels
  # you get exactly these 5; other counts interpolate between them. See
  # CLAUDE.md for when a diverging palette is the better fit.
  ordinal_green                 = unname(.ri_theme_colors[c("light_green", "green_tint", "green", "dark_green", "darkest_green")])
)

# Palettes for NOMINAL/categorical use (no inherent order) — ri_pal() does
# NOT interpolate these. Everything else in ri_palettes is ordinal or
# diverging (has a real low->high or two-sided direction), where blending
# between anchors is the correct thing to do and ri_pal() still
# interpolates via colorRampPalette().
.ri_categorical_palettes <- c(
  "categorical_cvd_safe", "main", "hot", "cool", "mixed",
  "green_violet_tint_binary", "green_dark_binary", "green_violet_binary", "green_blue_binary"
)

# ── ri_pal() ──────────────────────────────────────────────────
#  Returns a function(n) that gives you n colors from a named palette.
#
#  For CATEGORICAL palettes (unordered — see .ri_categorical_palettes
#  above) this returns the first n colors AS DEFINED, in order — no
#  blending — as long as n is within the palette's length. A palette is a
#  deliberately chosen, ordered sequence of distinct swatches;
#  interpolating between them for n <= length(palette) would replace real
#  anchor colors with blends nobody chose (e.g. asking categorical_cvd_safe
#  for 3 colors gives you green/blue/magenta exactly as listed, not some
#  blended stand-in). If n EXCEEDS the palette's length, it falls back to
#  interpolating across the whole palette (colorRampPalette) so you at
#  least get distinguishable colors instead of literal repeats — but
#  that's still a fallback, not a recommendation: prefer
#  ri_categorical_group() (top 5 + a grey "Other" bucket) for genuinely
#  more than 5 categories — see CLAUDE.md.
#
#  For ORDINAL/DIVERGING palettes (a real low->high or two-sided
#  direction, e.g. ordinal_green, diverging_green_white_violet) this DOES
#  interpolate, via colorRampPalette() — that's the correct way to fit an
#  arbitrary number of ordered steps onto a fixed set of anchor colors.
#
#  Arguments:
#    palette  Name of a palette in ri_palettes
#             (default "categorical_cvd_safe" — color-vision-deficiency-safe)
#    reverse  Reverse the palette before selecting/interpolating? (default FALSE)
#
#  Example — colors always match however many categories are really in df:
#    cats <- sort(unique(df$category))
#    my_colors <- setNames(ri_pal()(length(cats)), cats)
ri_pal <- function(palette = "categorical_cvd_safe", reverse = FALSE) {
  pal <- ri_palettes[[palette]]
  if (is.null(pal)) {
    stop(sprintf("'%s' is not a valid ri_palettes name. Choose from: %s",
                 palette, paste(names(ri_palettes), collapse = ", ")),
         call. = FALSE)
  }
  if (reverse) pal <- rev(pal)

  if (palette %in% .ri_categorical_palettes) {
    return(function(n) {
      if (n <= length(pal)) return(pal[seq_len(n)])   # exact swatches, no blending
      # More categories than this palette has real swatches. Prefer
      # ri_categorical_group() instead (top 5 + a grey "Other" bucket) —
      # this interpolated fallback exists for direct ri_pal() calls that
      # skip that, so you get distinguishable colors rather than literal
      # repeats, but it's still worse than not having >5 categories at all.
      warning(sprintf(
        "ri_pal(\"%s\") only has %d colors — interpolating to fill %d categories. See CLAUDE.md/ri_categorical_group() for handling more than 5 categories.",
        palette, length(pal), n), call. = FALSE)
      grDevices::colorRampPalette(pal)(n)
    })
  }

  grDevices::colorRampPalette(pal)
}

# ── ri_categorical_group() ───────────────────────────────────────
#  Implements the "cut to what matters + Other" fallback from the color
#  guidance above (see CLAUDE.md): keeps the `max_categories` biggest
#  categories with their own colors, and folds everything else into a
#  single grey "Other" bucket. "Biggest" is by total `weight` (e.g. sum of
#  employment, row count, bar length) — NOT alphabetical or first-seen
#  order — so the categories that actually matter in your data are the
#  ones that keep a distinct color. Ranking biggest-first also means the
#  biggest category gets `ri_green` by design, since it's the default
#  palette's first anchor color — see CLAUDE.md's "Roosevelt green on the
#  biggest category" rule.
#
#  If this variable (e.g. "industry") appears in more than one chart on the
#  same dashboard, call this ONCE against the full/combined dataset and
#  reuse the same `colors`/`kept` in every chart — see CLAUDE.md's
#  cross-chart consistency rule, which takes precedence over green landing
#  on whichever category is biggest within any one chart's local view.
#
#  Arguments:
#    categories      Character/factor vector of raw category values, one
#                     per row of your data (same length as weight).
#    weight          Numeric vector, same length as categories, used to
#                     rank which categories are "biggest" (e.g. the column
#                     a bar's length is drawn from). Defaults to 1 per row,
#                     i.e. ranks by row count if you don't have a better
#                     magnitude to rank by.
#    max_categories  How many to keep before folding the rest into "Other"
#                     (default 5, matching the color guidance above)
#    palette         Name of an ri_palettes entry for the kept categories
#                     (default "categorical_cvd_safe" — so 2 categories get
#                     green + blue, matching green_blue_binary)
#    other_label     Label for the folded-together bucket (default "Other")
#    other_color     Color for that bucket (default ri_lightest_grey, #C7CCCF —
#                     deliberately much lighter than ri_grey/ri_light_grey
#                     so "Other" reads as muted background noise, not a
#                     category competing for attention)
#
#  Returns a list:
#    labels  categories, recoded so anything outside the top `max_categories`
#            (by total weight) becomes `other_label` — same length/order as
#            the `categories` you passed in, so you can assign it straight
#            back onto your data frame. Returned as a factor ordered biggest
#            → smallest, with `other_label` always last.
#            Plotly's legend order follows these factor levels (not row
#            order), so "Other" ends up last on screen automatically —
#            no need to sort your data frame.
#    colors  named color vector covering every value in `labels`, ready to
#            pass directly to Plotly `colors =`
#    kept    character vector of the categories that got their own color
#            (everything else was folded into `other_label`) — reapply the
#            same grouping to a filtered subset with
#            `ifelse(x %in% kept, x, other_label)` so colors stay stable
#            no matter what the active filters are
#
#  Example — rank industries by total employment, not alphabetically:
#    grouped <- ri_categorical_group(df$industry, weight = df$employment)
#    df$industry_grouped <- grouped$labels
#    plot_ly(df, color = ~industry_grouped, colors = grouped$colors, ...)
ri_categorical_group <- function(categories,
                                  weight         = NULL,
                                  max_categories = 5,
                                  palette        = "categorical_cvd_safe",
                                  other_label    = "Other",
                                  other_color    = ri_lightest_grey) {
  categories <- as.character(categories)
  if (is.null(weight)) weight <- rep(1, length(categories))

  totals <- tapply(weight, categories, sum)
  ranked <- names(sort(totals, decreasing = TRUE))

  if (length(ranked) <= max_categories) {
    kept   <- ranked
    labels <- factor(categories, levels = kept)
  } else {
    kept   <- ranked[seq_len(max_categories)]
    labels <- factor(ifelse(categories %in% kept, categories, other_label),
                      levels = c(kept, other_label))   # Other always last
  }

  kept_colors <- setNames(ri_pal(palette)(length(kept)), kept)
  colors <- if (length(kept) < length(ranked)) {
    c(kept_colors, setNames(other_color, other_label))
  } else {
    kept_colors
  }

  list(labels = labels, colors = colors, kept = kept)
}

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
  "No formal educational credential"  = "#396C10",   # ri_dark_green
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
#  If the page uses tabs, ri_tab_nav_script() moves the tab pills
#  into this header (see snippets/new-tab.R).
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
#  JavaScript that moves the tab pills into the header bar after the
#  page loads. Include it once in your ui, right after ri_header(),
#  when the page uses a tabsetPanel() — see snippets/new-tab.R.
#
#  Shiny puts the tabsetPanel's id on the pill list itself
#  (<ul id="main_tabs" class="nav nav-pills">), so that's what the
#  script looks for. Plotly charts on other tabs resize themselves
#  when their tab is shown, so no resize handling is needed here.
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
        var $ul = $('ul#", tabs_id, ".nav-pills');
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
#    ri_legend_chip(ri_blue,   "Female")
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
#        ri_legend_chip(ri_blue,   "Female")
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

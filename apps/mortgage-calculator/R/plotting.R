# ============================================================
#  Roosevelt Institute — Plotly Helpers
#  Source this file at the top of app.R:
#    source("R/plotting.R")
#
#  These functions standardize the look of every Plotly chart:
#  Montserrat font, RI brand colors, consistent hover labels,
#  no mode bar, and clean gridlines.
# ============================================================

# ── Global Plotly defaults ─────────────────────────────────────
#  These are used internally by the helper functions.
#  You can reference RI_FONT and RI_FONT_SIZE in your own
#  plot_ly() calls to stay consistent.
RI_FONT_SIZE   <- 11
RI_FONT_FAMILY <- "Montserrat, sans-serif"
RI_GRID_COLOR  <- "#EBEBEB"


# ── ri_axis() ─────────────────────────────────────────────────
#  Returns a styled Plotly axis configuration list.
#  Pass the result to xaxis = or yaxis = inside layout().
#
#  Arguments:
#    title  Axis title text (default: no title)
#    ...    Any additional Plotly axis properties
#           (e.g. tickprefix = "$", range = c(0, 100))
#
#  Example:
#    layout(
#      xaxis = ri_axis("Median Annual Wage", tickprefix = "$"),
#      yaxis = ri_axis("Employment Change")
#    )
ri_axis <- function(title = "", ...) {
  list(
    title     = list(
      text = title,
      font = list(size = RI_FONT_SIZE, color = ri_grey)
    ),
    tickfont  = list(
      size   = RI_FONT_SIZE,
      family = RI_FONT_FAMILY,
      color  = "#333333"
    ),
    gridcolor = RI_GRID_COLOR,
    zeroline  = FALSE,
    showline  = FALSE,
    ...
  )
}


# ── ri_axis_blank() ───────────────────────────────────────────
#  A blank axis — no title, no labels, no gridlines.
#  Useful for axes where you're placing text annotations instead.
#
#  Example:
#    layout(xaxis = ri_axis_blank())
ri_axis_blank <- function(...) {
  list(
    title          = "",
    showticklabels = FALSE,
    showgrid       = FALSE,
    zeroline       = FALSE,
    showline       = FALSE,
    ...
  )
}


# ── ri_layout() ───────────────────────────────────────────────
#  Returns standard RI layout settings for layout().
#  Covers background, font, hover label styling, and margins.
#  Merge with your own layout() call using modifyList() if you
#  need to override individual settings.
#
#  Arguments:
#    margin  Named list with l/r/t/b values (default: l=60,r=20,t=20,b=60)
#    ...     Any additional layout properties to add or override
#
#  Basic usage — pass directly to layout():
#    plot_ly(...) %>% layout(ri_layout())
#
#  With custom margins:
#    plot_ly(...) %>% layout(ri_layout(margin = list(l=10, r=20, t=10, b=40)))
#
#  Merging with extra settings:
#    my_layout <- modifyList(ri_layout(), list(barmode = "stack", showlegend = FALSE))
#    plot_ly(...) %>% layout(my_layout)
ri_layout <- function(margin = list(l = 60, r = 20, t = 20, b = 60), ...) {
  base <- list(
    paper_bgcolor = "white",
    plot_bgcolor  = "white",
    font          = list(family = RI_FONT_FAMILY, size = RI_FONT_SIZE),
    margin        = margin,
    hoverlabel    = list(
      bgcolor     = "#fff",
      font        = list(
        family = RI_FONT_FAMILY,
        size   = 9,
        color  = "#1A1A1A"
      ),
      bordercolor = ri_green
    )
  )
  modifyList(base, list(...))
}


# ── ri_config() ───────────────────────────────────────────────
#  Applies standard Plotly config: hides the mode bar.
#  Chain this at the end of every plot_ly() pipeline.
#
#  Example:
#    plot_ly(...) %>% layout(...) %>% ri_config()
ri_config <- function(p) {
  plotly::config(p, displayModeBar = FALSE)
}


# ── ri_hover_template() ───────────────────────────────────────
#  Builds a consistent hovertemplate string with RI styling.
#  Uses inline HTML that Plotly renders in the tooltip.
#
#  Arguments:
#    label_expr   Plotly expression for the hover label (e.g. "%{y}")
#    value_expr   Plotly expression for the main value (e.g. "%{x:.0f}%")
#    value_color  Hex color for the value text (default: ri_green)
#    note         Optional small grey note below the value
#
#  Example — percentage bar chart:
#    hovertemplate = ri_hover_template(
#      label_expr  = "%{y}",
#      value_expr  = "%{x:.0f}%",
#      value_color = ri_green,
#      note        = "share of workforce"
#    )
#
#  Example — dollar value:
#    hovertemplate = ri_hover_template(
#      label_expr  = "%{x}",
#      value_expr  = "$%{y:.0f}",
#      value_color = ri_violet,
#      note        = "median annual wage"
#    )
ri_hover_template <- function(label_expr,
                               value_expr,
                               value_color = ri_green,
                               note        = NULL) {
  note_html <- if (!is.null(note)) {
    paste0("<br><span style='font-size:9px;color:#888;'>", note, "</span>")
  } else ""

  paste0(
    "<b style='font-size:9px;color:#1A1A1A;'>", label_expr, "</b><br>",
    "<b style='font-size:11px;color:", value_color, ";'>", value_expr, "</b>",
    note_html,
    "<extra></extra>"
  )
}


# ── ri_bar_chart() ────────────────────────────────────────────
#  A convenience wrapper for the most common chart pattern:
#  a grouped or stacked horizontal or vertical bar chart.
#  Returns a plot_ly object — chain layout() and ri_config() after.
#
#  Arguments:
#    df           Data frame
#    x, y         Column names (as strings) for x and y axes
#    color_col    Column name for the color grouping (optional)
#    colors       Named vector mapping color_col values to hex colors
#    orientation  "v" (vertical, default) or "h" (horizontal)
#    text_col     Column name for bar label text (optional)
#
#  Example — vertical grouped bar:
#    ri_bar_chart(
#      df          = wages_df,
#      x           = "race",
#      y           = "wage",
#      color_col   = "sex",
#      colors      = c(Male = ri_green, Female = ri_violet)
#    ) %>%
#    layout(modifyList(ri_layout(), list(barmode = "group"))) %>%
#    ri_config()
#
#  Example — horizontal stacked bar:
#    ri_bar_chart(
#      df          = edu_df,
#      x           = "share",
#      y           = "industry",
#      color_col   = "edu_group",
#      colors      = c(less_than_ba = ri_green, ba_higher = ri_violet),
#      orientation = "h"
#    ) %>%
#    layout(modifyList(ri_layout(), list(barmode = "stack"))) %>%
#    ri_config()
ri_bar_chart <- function(df, x, y,
                          color_col   = NULL,
                          colors      = NULL,
                          orientation = "v",
                          text_col    = NULL) {

  # Build base mapping
  mapping <- list(
    x           = as.formula(paste0("~", x)),
    y           = as.formula(paste0("~", y)),
    type        = "bar",
    orientation = orientation
  )

  if (!is.null(color_col)) mapping$color  <- as.formula(paste0("~", color_col))
  if (!is.null(colors))    mapping$colors <- colors
  if (!is.null(text_col))  {
    mapping$text          <- as.formula(paste0("~", text_col))
    mapping$textposition  <- "outside"
    mapping$textfont      <- list(
      family = RI_FONT_FAMILY,
      size   = RI_FONT_SIZE,
      color  = ri_grey
    )
  }

  do.call(plotly::plot_ly, c(list(df), mapping))
}

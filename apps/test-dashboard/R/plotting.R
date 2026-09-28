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
#  You can reference RI_FONT_FAMILY and RI_FONT_SIZE in your own
#  plot_ly() calls to stay consistent.
RI_FONT_SIZE   <- 11
RI_FONT_FAMILY <- "Montserrat, sans-serif"
RI_GRID_COLOR  <- "#EBEBEB"


# ── ri_axis() ─────────────────────────────────────────────────
#  Returns a styled Plotly axis configuration list.
#  Pass the result to xaxis = or yaxis = inside the list you give
#  ri_apply_layout().
#
#  Arguments:
#    title  Axis title text (default: no title)
#    ...    Any additional Plotly axis properties
#           (e.g. tickprefix = "$", range = c(0, 100))
#
#  Example:
#    ri_apply_layout(p, modifyList(ri_layout(), list(
#      xaxis = ri_axis("Median Annual Wage", tickprefix = "$"),
#      yaxis = ri_axis("Employment Change")
#    )))
ri_axis <- function(title = "", ...) {
  list(
    title     = list(
      text     = title,
      font     = list(size = RI_FONT_SIZE, color = ri_grey),
      standoff = 15
    ),
    tickfont  = list(
      size   = RI_FONT_SIZE,
      family = RI_FONT_FAMILY,
      color  = ri_text
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
#    ri_apply_layout(p, modifyList(ri_layout(), list(xaxis = ri_axis_blank())))
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
#  Returns standard RI layout settings as a list — it doesn't apply
#  anything by itself. Covers background, font, hover label styling,
#  and margins. Apply it with ri_apply_layout(), NOT layout():
#  plotly::layout() silently ignores a list passed positionally, so
#  layout(ri_layout()) falls back to Plotly's defaults (see
#  ri_apply_layout() below and CLAUDE.md).
#
#  Arguments:
#    margin  Named list with l/r/t/b values (default: l=60,r=20,t=20,b=60)
#    ...     Any additional layout properties to add or override
#
#  Basic usage:
#    plot_ly(...) %>% ri_apply_layout(ri_layout())
#
#  With custom margins:
#    plot_ly(...) %>% ri_apply_layout(ri_layout(margin = list(l=10, r=20, t=10, b=40)))
#
#  Merging with extra settings:
#    my_layout <- modifyList(ri_layout(), list(barmode = "stack", showlegend = FALSE))
#    plot_ly(...) %>% ri_apply_layout(my_layout)
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
        color  = ri_text
      ),
      bordercolor = ri_lightest_grey
    )
  )
  modifyList(base, list(...))
}


# ── ri_config() ───────────────────────────────────────────────
#  Applies standard Plotly config: hides the mode bar.
#  Chain this at the end of every plot_ly() pipeline.
#
#  Example:
#    plot_ly(...) %>% ri_apply_layout(ri_layout()) %>% ri_config()
ri_config <- function(p) {
  plotly::config(p, displayModeBar = FALSE)
}


# ── ri_apply_layout() ───────────────────────────────────────────
#  Applies a merged layout list (e.g. from modifyList(ri_layout(...),
#  list(xaxis = ..., yaxis = ...))) to a plot.
#
#  Use this instead of calling layout() directly with a merged list.
#  plotly::layout() only reads settings from named ... arguments — a
#  single list passed positionally (`layout(modifyList(...))`) is
#  silently ignored, so the chart falls back to Plotly's own defaults
#  instead of the RI styling. do.call() spreads the merged list into
#  real named arguments so it actually takes effect. Verify what a
#  layout call is really producing with plotly::plotly_build(p)$x$layout
#  if a chart looks like it's ignoring ri_layout()/ri_axis().
#
#  Arguments:
#    p            A plotly object (from plot_ly() or a %>% chain)
#    layout_args  A named list of layout settings — typically
#                 modifyList(ri_layout(...), list(xaxis = ..., ...))
#
#  Example:
#    plot_ly(...) %>%
#      ri_apply_layout(modifyList(
#        ri_layout(margin = list(l = 60, r = 20, t = 20, b = 60)),
#        list(
#          xaxis = ri_axis("X label"),
#          yaxis = ri_axis("Y label")
#        )
#      )) %>%
#      ri_config()
ri_apply_layout <- function(p, layout_args) {
  do.call(plotly::layout, c(list(p), layout_args))
}


# ── ri_hover_template() ───────────────────────────────────────
#  Builds a consistent hovertemplate string with RI styling.
#  Uses inline HTML that Plotly renders in the tooltip.
#  Reads top to bottom as: value / note / label.
#
#  Arguments:
#    label_expr   Plotly expression for the hover label (e.g. "%{y}")
#    value_expr   Plotly expression for the main value (e.g. "%{x:.0f}%")
#    value_color  Hex color for the value text (default: ri_text — the
#                 same near-black as the label, so every line reads as one
#                 consistent color instead of trying to match per-point
#                 marker colors, which Plotly only substitutes correctly
#                 for pie/donut traces, not bar/scatter — see CLAUDE.md)
#    note         Optional small note below the label, in ri_light_grey
#
#  Example — dollar value in a different color than the default:
#    hovertemplate = ri_hover_template(
#      label_expr  = "%{x}",
#      value_expr  = "$%{y:.0f}",
#      value_color = ri_violet,
#      note        = "median annual wage"
#    )
ri_hover_template <- function(label_expr,
                               value_expr,
                               value_color = ri_text,
                               note        = NULL) {
  # Plotly hover labels are drawn as SVG text, which has no CSS box model —
  # `padding` has no effect here. The inset instead comes from baking
  # whitespace into the template itself: non-breaking spaces for left/right,
  # and a thin blank line for top/bottom.
  pad_h <- "   "
  pad_v <- "<span style='font-size:4px;line-height:4px;'>&nbsp;</span><br>"

  label_size <- 9
  value_size <- label_size * 1.5   # value text stays proportionally bigger than the label

  # pad_h is wrapped at a fixed label_size font, not each line's own font
  # size, so the padding renders the same width on every line regardless
  # of how big that line's own text is (otherwise the 1.5x-larger value
  # text would get visibly wider — and lopsided-looking — padding).
  pad_span <- paste0("<span style='font-size:", label_size, "px;'>", pad_h, "</span>")

  note_html <- if (!is.null(note)) {
    paste0(pad_span, "<span style='font-size:", label_size, "px;color:", ri_light_grey, ";'>", note, "</span>", pad_span, "<br>")
  } else ""

  paste0(
    pad_v,
    pad_span, "<b style='font-size:", value_size, "px;color:", value_color, ";'>", value_expr, "</b>", pad_span, "<br>",
    note_html,
    pad_span, "<b style='font-size:", label_size, "px;color:", ri_text, ";'>", label_expr, "</b>", pad_span,
    "<br>", pad_v,
    "<extra></extra>"
  )
}


# ── ri_bar_chart() ────────────────────────────────────────────
#  A convenience wrapper for the most common chart pattern:
#  a grouped or stacked horizontal or vertical bar chart.
#  Returns a plot_ly object — chain ri_apply_layout() and ri_config() after.
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
#      colors      = c(Male = ri_green, Female = ri_blue)
#    ) %>%
#    ri_apply_layout(modifyList(ri_layout(), list(barmode = "group"))) %>%
#    ri_config()
#
#  Example — horizontal stacked bar:
#    ri_bar_chart(
#      df          = edu_df,
#      x           = "share",
#      y           = "industry",
#      color_col   = "edu_group",
#      colors      = c(less_than_ba = ri_green, ba_higher = ri_blue),
#      orientation = "h"
#    ) %>%
#    ri_apply_layout(modifyList(ri_layout(), list(barmode = "stack"))) %>%
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

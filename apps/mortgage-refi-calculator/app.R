library(shiny)
library(plotly)

source("R/components.R")
source("R/plotting.R")

# ── Helpers ───────────────────────────────────────────────────
fmt_dollar <- function(x) {
  paste0("$", formatC(round(x), format = "d", big.mark = ","))
}

monthly_pi <- function(loan, annual_rate_pct, term_yrs) {
  n <- term_yrs * 12
  r <- annual_rate_pct / 100 / 12
  if (r == 0) return(loan / n)
  loan * r * (1 + r)^n / ((1 + r)^n - 1)
}

amort_by_year <- function(loan, r, n, pi_payment) {
  years  <- ceiling(n / 12)
  result <- data.frame(year = seq_len(years), interest = 0, principal = 0, balance = 0)
  bal    <- loan
  for (i in seq_len(n)) {
    yr     <- ceiling(i / 12)
    int_i  <- bal * r
    prin_i <- min(max(pi_payment - int_i, 0), bal)
    bal    <- max(bal - prin_i, 0)
    result$interest[yr]  <- result$interest[yr]  + int_i
    result$principal[yr] <- result$principal[yr] + prin_i
    result$balance[yr]   <- bal
  }
  result$cum_interest  <- cumsum(result$interest)
  result$cum_principal <- cumsum(result$principal)
  result
}

fmt_months <- function(m) {
  if (!is.finite(m)) return("Never")
  m <- round(m)
  if (m < 12) return(paste0(m, " mo"))
  yrs <- m %/% 12
  mos <- m %%  12
  if (mos == 0) paste0(yrs, " yr") else paste0(yrs, " yr ", mos, " mo")
}

# ── UI ────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "roosevelt.css"),
    tags$style(HTML("
      html, body { max-width: 100%; overflow-x: hidden; }
      .ch-card { overflow: hidden; }

      .calc-wrap {
        display: flex;
        gap: 36px;
        padding: 16px;
        align-items: flex-start;
        flex-wrap: wrap;
        box-sizing: border-box;
        width: 100%;
      }
      .calc-panel {
        width: 260px;
        flex-shrink: 0;
        font-family: 'Montserrat', sans-serif;
      }
      .calc-results { flex: 1 1 280px; min-width: 0; }
      .stat-card { min-width: 100px; }
      .stat-strip { margin-bottom: 14px; }
      .stat-num { font-size: 20px; }

      .calc-panel .filter-label {
        font-size: 9px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: .8px;
        color: #1A1A1A;
        border-bottom: 1.5px solid #58A618;
        padding-bottom: 3px;
        margin-top: 18px;
        margin-bottom: 10px;
      }
      .calc-panel .filter-label:first-child { margin-top: 0; }
      .calc-panel .form-group { margin-bottom: 10px; }
      .calc-panel label {
        font-size: 10px !important;
        font-weight: 700 !important;
        color: #5E6A71 !important;
        font-family: 'Montserrat', sans-serif !important;
        margin-bottom: 3px !important;
        text-transform: uppercase;
        letter-spacing: .5px;
      }
      .calc-panel input[type='number'] {
        font-family: 'Montserrat', sans-serif !important;
        font-size: 14px !important;
        font-weight: 600 !important;
        border-radius: 6px !important;
        border: 1.5px solid #DDD !important;
        width: 100% !important;
        padding: 8px 10px !important;
        color: #1A1A1A !important;
        transition: border-color .15s;
      }
      .calc-panel input[type='number']:focus {
        border-color: #58A618 !important;
        outline: none !important;
        box-shadow: 0 0 0 2px rgba(88,166,24,.15) !important;
      }
      .calc-panel .selectize-input {
        font-family: 'Montserrat', sans-serif !important;
        font-size: 14px !important;
        font-weight: 600 !important;
        border-radius: 6px !important;
        border: 1.5px solid #DDD !important;
        padding: 8px 10px !important;
        box-shadow: none !important;
      }
      .calc-panel .selectize-input.focus {
        border-color: #58A618 !important;
        box-shadow: 0 0 0 2px rgba(88,166,24,.15) !important;
      }

      .refi-notice {
        border-left: 3px solid #F9A825;
        background: #FFF8E1;
        border-radius: 0 4px 4px 0;
        padding: 9px 12px;
        font-size: 10px;
        color: #5E6A71;
        margin-bottom: 14px;
        line-height: 1.6;
      }

      /* Three-block payment comparison */
      .cmp-row {
        display: flex;
        gap: 10px;
        margin-bottom: 14px;
        flex-wrap: wrap;
      }
      .cmp-block {
        flex: 1;
        min-width: 110px;
        padding: 10px 12px;
        border: 1px solid #EBEBEB;
        border-radius: 4px;
      }
      .cmp-lbl {
        font-size: 9px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: .5px;
        color: #5E6A71;
        margin-bottom: 6px;
      }
      .cmp-val  { font-size: 15px; font-weight: 700; }
      .cmp-sub  { font-size: 9px; color: #5E6A71; margin-top: 2px; }

      @media (max-width: 620px) {
        .calc-panel { width: 100%; }
        .calc-results { min-width: 0; width: 100%; }
        .calc-wrap { padding: 12px; gap: 12px; }
        .stat-num { font-size: 18px; }
        .stat-card { min-width: 70px; }
      }
    "))
  ),

  ri_header("Mortgage Refinance Calculator", "Roosevelt Institute"),

  div(class = "calc-wrap",

    # ── LEFT: Inputs ──────────────────────────────────────────
    div(class = "calc-panel",

      div(class = "filter-label", "Current Mortgage"),

      numericInput("cur_balance", "Remaining Balance ($)",
        value = 199000, min = 1000, step = 5000),

      numericInput("cur_rate", "Current Rate (%)",
        value = 6.875, min = 0, max = 25, step = 0.05),

      selectInput("yrs_rem", "Years Remaining",
        choices  = setNames(1:30, paste(1:30, "years")),
        selected = 28),

      div(class = "filter-label", "Refinance To"),

      numericInput("new_rate", "New Rate (%)",
        value = 6.125, min = 0, max = 25, step = 0.05),

      selectInput("new_term", "New Term",
        choices  = c("10 years" = 10, "15 years" = 15,
                     "20 years" = 20, "25 years" = 25, "30 years" = 30),
        selected = 30),

      numericInput("closing_costs", "Closing Costs ($)",
        value = 0, min = 0, step = 500),

      uiOutput("left_calcs_ui")
    ),

    # ── RIGHT: Results ────────────────────────────────────────
    div(class = "calc-results",

      uiOutput("notice_ui"),

      div(class = "ch-card",
        tags$h3("Monthly Savings by Rate"),
        div(class = "sublabel",
            "The lower the rate you can lock in, the more you save each month"),
        plotlyOutput("savings_curve_chart", width = "100%", height = "260px")
      )
    )
  ),

  ri_footer("Roosevelt Institute · rooseveltinstitute.org")
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {

  calc <- reactive({
    req(isTruthy(input$cur_balance), isTruthy(input$cur_rate),
        isTruthy(input$new_rate), isTruthy(input$closing_costs),
        isTruthy(input$yrs_rem), isTruthy(input$new_term))

    bal      <- max(input$cur_balance, 1)
    cur_rate <- max(input$cur_rate, 0)
    yrs_rem  <- as.numeric(input$yrs_rem)
    new_rate <- max(input$new_rate, 0)
    new_term <- as.numeric(input$new_term)
    closing  <- max(input$closing_costs, 0)

    cur_pi  <- monthly_pi(bal, cur_rate, yrs_rem)
    new_pi  <- monthly_pi(bal, new_rate, new_term)
    savings <- cur_pi - new_pi

    be_months <- if (savings > 0) closing / savings else Inf

    r_cur  <- cur_rate / 100 / 12
    df_cur <- amort_by_year(bal, r_cur, yrs_rem * 12, cur_pi)

    r_new  <- new_rate / 100 / 12
    df_new <- amort_by_year(bal, r_new, new_term * 12, new_pi)

    cur_total_int  <- sum(df_cur$interest)
    new_total_int  <- sum(df_new$interest)
    interest_saved <- cur_total_int - new_total_int
    net_savings    <- interest_saved - closing

    list(
      bal = bal, cur_rate = cur_rate, yrs_rem = yrs_rem,
      new_rate = new_rate, new_term = new_term, closing = closing,
      cur_pi = cur_pi, new_pi = new_pi, savings = savings,
      be_months = be_months,
      df_cur = df_cur, df_new = df_new,
      cur_total_int = cur_total_int, new_total_int = new_total_int,
      interest_saved = interest_saved, net_savings = net_savings
    )
  })

  # ── Left-panel calculations ───────────────────────────────
  output$left_calcs_ui <- renderUI({
    cv      <- calc()
    ann_col <- if (cv$savings    >= 0) "#1A1A1A" else "#AA0061"
    tot_col <- if (cv$net_savings >= 0) "#58A618"  else "#AA0061"

    pair <- function(...) {
      div(style = "display:flex; gap:14px; margin-bottom:6px;", ...)
    }
    stat <- function(val, lbl, col) {
      div(style = "flex:1;",
        div(class = "stat-num", style = paste0("color:", col, "; font-size:18px;"), val),
        div(class = "stat-lbl", lbl)
      )
    }

    tagList(
      div(class = "filter-label", "Monthly Payments"),
      pair(
        stat(fmt_dollar(cv$cur_pi),
             paste0("Current at ", cv$cur_rate, "%"),
             "#1A1A1A"),
        stat(fmt_dollar(cv$new_pi),
             paste0("New at ", cv$new_rate, "%"),
             "#1A1A1A")
      ),
      div(class = "filter-label", style = "margin-top:16px;", "Savings"),
      pair(
        stat(fmt_dollar(abs(cv$savings) * 12),
             if (cv$savings >= 0) "Annual Savings" else "Annual Increase",
             ann_col),
        stat(fmt_dollar(abs(cv$net_savings)),
             if (cv$net_savings >= 0) "Net Savings" else "Net Extra Cost",
             tot_col)
      )
    )
  })

  # ── Contextual notices ────────────────────────────────────
  output$notice_ui <- renderUI({
    cv <- calc()
    msg <- if (cv$savings <= 0) {
      tagList(tags$b("Higher monthly payment: "),
        "The new loan results in a higher monthly P&I than your current loan.")
    } else if (!is.finite(cv$be_months)) {
      tagList(tags$b("Closing costs not recovered: "),
        "Your monthly savings are too small to offset closing costs.")
    } else if (cv$be_months > cv$yrs_rem * 12) {
      tagList(tags$b("Break-even after payoff: "),
        paste0("You'd break even in ", fmt_months(cv$be_months),
               ", but only ", fmt_months(cv$yrs_rem * 12), " remain on your current loan."))
    } else if (cv$net_savings < 0) {
      tagList(tags$b("More interest overall: "),
        "Extending the term means you'll pay more total interest even at the lower rate.")
    }
    if (!is.null(msg)) div(class = "refi-notice", msg)
  })

  # ── Savings vs. new rate chart ────────────────────────────
  output$savings_curve_chart <- renderPlotly({
    cv    <- calc()
    rates <- seq(0, 10, by = 0.05)
    sav   <- sapply(rates, function(r) cv$cur_pi - monthly_pi(cv$bal, r, cv$new_term))

    # Separate positive / negative regions (NA stops fill at zero)
    sav_pos <- ifelse(sav >= 0, sav, NA)
    sav_neg <- ifelse(sav <= 0, sav, NA)

    # Break-even rate (where savings cross zero)
    be_rate <- tryCatch(
      uniroot(function(r) monthly_pi(cv$bal, r, cv$new_term) - cv$cur_pi,
              interval = c(0.001, 20))$root,
      error = function(e) NA_real_
    )
    be_in_range <- !is.na(be_rate) && be_rate >= 0 && be_rate <= 10

    dot_y    <- cv$savings
    dot_col  <- if (dot_y >= 0) "#58A618" else "#AA0061"

    # Dotted vertical line from selected dot to x-axis
    shapes <- list(list(
      type = "line",
      x0 = cv$new_rate, x1 = cv$new_rate,
      y0 = 0, y1 = dot_y,
      xref = "x", yref = "y",
      line = list(color = dot_col, width = 1.5, dash = "dot")
    ))

    # Annotations
    dot_lbl <- if (dot_y >= 0) {
      paste0("At <b>", cv$new_rate, "%</b> you save <b>", fmt_dollar(round(dot_y)), "</b>/mo")
    } else {
      paste0("At <b>", cv$new_rate, "%</b> costs <b>", fmt_dollar(round(abs(dot_y))), "</b> more/mo")
    }

    anns <- list(list(
      x = cv$new_rate, y = dot_y, xref = "x", yref = "y",
      text = dot_lbl,
      xanchor = if (cv$new_rate > 7) "right" else "left",
      yanchor = "middle",
      xshift  = if (cv$new_rate > 7) -12 else 12,
      yshift  = 0,
      showarrow = FALSE,
      font = list(size = 9, color = dot_col, family = RI_FONT_FAMILY)
    ))

    if (be_in_range) {
      anns <- c(anns, list(list(
        x = be_rate, y = 0, xref = "x", yref = "y",
        text = paste0("Break-even: <b>", round(be_rate, 2), "%</b>"),
        xanchor = "left", yanchor = "bottom",
        xshift = 0, yshift = 8,
        showarrow = FALSE,
        font = list(size = 9, color = "#5E6A71", family = RI_FONT_FAMILY)
      )))
    }

    p <- plot_ly() %>%
      # Green fill — positive savings region
      add_trace(
        x = rates, y = sav_pos,
        type = "scatter", mode = "lines",
        fill = "tozeroy", fillcolor = "rgba(88,166,24,0.12)",
        line = list(color = "transparent", width = 0),
        showlegend = FALSE, hoverinfo = "none", connectgaps = FALSE
      ) %>%
      # Magenta fill — negative savings (loss) region
      add_trace(
        x = rates, y = sav_neg,
        type = "scatter", mode = "lines",
        fill = "tozeroy", fillcolor = "rgba(170,0,97,0.12)",
        line = list(color = "transparent", width = 0),
        showlegend = FALSE, hoverinfo = "none", connectgaps = FALSE
      ) %>%
      # Black savings curve
      add_trace(
        x = rates, y = sav,
        type = "scatter", mode = "lines",
        line = list(color = "#1A1A1A", width = 2),
        showlegend = FALSE,
        hovertemplate = "New Rate: %{x:.2f}%<br><b>$%{y:,.0f}/mo saved</b><extra></extra>"
      ) %>%
      # Colored dot at selected new rate
      add_trace(
        x = cv$new_rate, y = dot_y,
        type = "scatter", mode = "markers",
        marker = list(color = dot_col, size = 9),
        showlegend = FALSE, hoverinfo = "none"
      )

    # Black dot at break-even
    if (be_in_range) {
      p <- p %>% add_trace(
        x = be_rate, y = 0,
        type = "scatter", mode = "markers",
        marker = list(color = "#1A1A1A", size = 8),
        showlegend = FALSE, hoverinfo = "none"
      )
    }

    p %>% layout(
      paper_bgcolor = "white", plot_bgcolor = "white",
      font    = list(family = RI_FONT_FAMILY, size = RI_FONT_SIZE),
      margin  = list(l = 60, r = 20, t = 10, b = 45),
      showlegend = FALSE,
      hovermode  = "x",
      hoverlabel = list(bgcolor = "#fff", bordercolor = "#EBEBEB",
                        font = list(family = RI_FONT_FAMILY, size = 9, color = "#1A1A1A")),
      shapes      = shapes,
      annotations = anns,
      xaxis = list(
        title     = list(text = "New Interest Rate (%)",
                         font = list(size = RI_FONT_SIZE, color = "#5E6A71")),
        tickfont  = list(size = RI_FONT_SIZE, family = RI_FONT_FAMILY, color = "#333"),
        gridcolor = RI_GRID_COLOR, zeroline = FALSE, showline = FALSE,
        ticksuffix = "%", range = c(0, 10)
      ),
      yaxis = list(
        title     = list(text = "Monthly Savings",
                         font = list(size = RI_FONT_SIZE, color = "#5E6A71")),
        tickfont  = list(size = RI_FONT_SIZE, family = RI_FONT_FAMILY, color = "#333"),
        gridcolor = RI_GRID_COLOR,
        zeroline = TRUE, zerolinecolor = "#BBBBBB", zerolinewidth = 1,
        showline = FALSE,
        tickprefix = "$", tickformat = "~s"
      )
    ) %>%
    ri_config()
  })

}

shinyApp(ui, server)

library(shiny)
library(plotly)

source("R/components.R")
source("R/plotting.R")

ri_green_dark <- "#008C95"

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

payoff_label <- function(term_yrs) {
  d <- Sys.Date()
  format(seq(d, by = "month", length.out = term_yrs * 12 + 1)[term_yrs * 12 + 1], "%b %Y")
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
        width: 220px;
        flex-shrink: 0;
        font-family: 'Montserrat', sans-serif;
      }
      .calc-results { flex: 1 1 280px; min-width: 0; }
      .stat-card { min-width: 100px; }

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

      .down-row { display: flex; gap: 10px; }
      .down-row .form-group:first-child { flex: 2; }
      .down-row .form-group:last-child  { flex: 1; }

      .stat-strip { margin-bottom: 14px; }
      .stat-num { font-size: 20px; }

      .pmi-notice {
        background: #FFF8E1;
        border-left: 3px solid #F9A825;
        border-radius: 0 4px 4px 0;
        padding: 9px 12px;
        font-size: 10px;
        color: #5E6A71;
        margin-top: 10px;
        line-height: 1.6;
      }

      .chart-hover-wrap {
        display: flex;
        flex-direction: column;
        gap: 8px;
        margin-top: 4px;
      }
      /* Hover panel sits below chart as a horizontal strip */
      .hover-panel {
        width: 100%;
        border: 1px solid #EBEBEB;
        border-radius: 4px;
        overflow: hidden;
        font-family: 'Montserrat', sans-serif;
        display: flex;
        flex-wrap: wrap;
      }
      .hover-row {
        flex: 1;
        display: flex;
        flex-direction: column;
        align-items: flex-start;
        padding: 8px 12px;
        border-bottom: none;
        border-right: 1px solid #F4F4F4;
        gap: 3px;
      }
      .hover-row:last-child { border-right: none; }
      .hover-label { font-size: 9px; font-weight: 700; color: #1A1A1A; text-transform: uppercase; letter-spacing: .5px; }
      .hover-value { font-size: 15px; font-weight: 700; }

      .amort-wrap { max-height: 320px; overflow-y: auto; }
      .amort-tbl {
        font-family: 'Montserrat', sans-serif;
        font-size: 11px;
        width: 100%;
        border-collapse: collapse;
      }
      .amort-tbl th {
        position: sticky; top: 0; background: #fff;
        font-size: 9px; font-weight: 700; text-transform: uppercase;
        letter-spacing: .5px; color: #5E6A71;
        padding: 8px 10px; border-bottom: 1.5px solid #58A618;
        text-align: right; z-index: 1;
      }
      .amort-tbl th:first-child { text-align: left; }
      .amort-tbl td {
        padding: 7px 10px; border-bottom: 1px solid #F4F4F4;
        text-align: right; color: #1A1A1A;
      }
      .amort-tbl td:first-child { text-align: left; font-weight: 600; }
      .amort-tbl tbody tr:hover td { background: #f0fae8; }
      .amort-tbl .amort-total td {
        font-weight: 700; border-top: 1.5px solid #EBEBEB; background: #F9F9F9;
      }

      .hoverlayer .spikeline text {
        font-size: 11px !important;
        font-family: 'Montserrat', sans-serif !important;
      }

      @media (max-width: 620px) {
        .calc-panel { width: 100%; }
        .calc-results { min-width: 0; width: 100%; }
        .calc-wrap { padding: 12px; gap: 12px; }
        .stat-num { font-size: 20px; }
        .stat-card { min-width: 70px; }
      }
      @media (max-width: 480px) {
        .stat-num { font-size: 17px; }
        .totals-row .stat-num { font-size: 14px; }
        .stat-card { min-width: 60px; padding-right: 10px; margin-right: 10px; }
      }
    ")),

    # ── Client-side hover handler ──────────────────────────────
    tags$script(HTML("
      var _amort   = null;
      var _boundGd = null;

      function _fmt(x) {
        return '$' + Math.round(x).toLocaleString('en-US');
      }

      function _updatePanel(offset) {
        if (!_amort) return;
        var i = Math.max(0, Math.min(offset, _amort.n - 1));
        document.getElementById('hp-title-year').textContent = _amort.cal[i];
        document.getElementById('hp-balance').textContent    = _fmt(_amort.balance[i]);
        document.getElementById('hp-principal').textContent  = _fmt(_amort.principal[i]);
        document.getElementById('hp-interest').textContent   = _fmt(_amort.interest[i]);
      }

      // Plotly initialises on the output div itself (#amort_chart), not on a child.
      // Wait for _fullLayout to confirm Plotly is fully ready.
      function _getGd() {
        var el = document.getElementById('amort_chart');
        if (!el) return null;
        if (el._fullLayout) return el;
        var child = el.querySelector('.js-plotly-plot');
        return (child && child._fullLayout) ? child : null;
      }

      // Resize gd to exactly the wrapper's constrained offsetWidth.
      // We measure the wrapper (not gd itself) because Plotly may have set an
      // explicit pixel width on the graph div, making gd.clientWidth a stale
      // circular measurement that Plotly.Plots.resize() would just echo back.
      function _relayoutToWrap(gd) {
        var wrap = document.getElementById('amort_chart_wrap');
        var w = wrap ? wrap.offsetWidth : 0;
        if (w > 0) Plotly.relayout(gd, {width: w});
      }

      function _bindHover() {
        var gd = _getGd();
        if (!gd) { setTimeout(_bindHover, 200); return; }
        if (gd === _boundGd) return;

        _relayoutToWrap(gd);

        gd.on('plotly_hover', function(ev) {
          if (!_amort || !ev.points || !ev.points.length) return;
          _updatePanel(Math.round(ev.points[0].x) - _amort.cal[0]);
        });

        gd.on('plotly_afterplot', function() {
          var wrap = document.getElementById('amort_chart_wrap');
          if (!wrap) return;
          var need = wrap.offsetWidth;
          if (need > 0 && Math.abs((gd._fullLayout.width || 0) - need) > 2) {
            Plotly.relayout(gd, {width: need});
          }
        });

        _boundGd = gd;
      }

      function _resizeChart() {
        var gd = _getGd();
        if (gd) _relayoutToWrap(gd);
      }

      Shiny.addCustomMessageHandler('amortData', function(d) {
        _amort = d;
        _updatePanel(1);
        _boundGd = null;  // force rebind after every server re-render
        setTimeout(_bindHover, 50);
      });

      window.addEventListener('resize', _resizeChart);
    "))
  ),

  ri_header("Mortgage Calculator", "Roosevelt Institute"),

  div(class = "calc-wrap",

    # ── LEFT: Inputs ────────────────────────────────────────
    div(class = "calc-panel",

      div(class = "filter-label", "Loan Details"),

      numericInput("home_price", "Home Price ($)",
        value = 400000, min = 10000, step = 5000),

      div(class = "down-row",
        numericInput("down_amount", "Down Payment ($)",
          value = 80000, min = 0, step = 5000),
        numericInput("down_pct", "Down (%)",
          value = 20, min = 0, max = 100, step = 0.5)
      ),

      selectInput("term", "Loan Term",
        choices  = c("30 years" = 30, "20 years" = 20,
                     "15 years" = 15, "10 years" = 10),
        selected = 30),

      numericInput("rate", "Interest Rate (%)",
        value = 6.5, min = 0, max = 25, step = 0.05),

      numericInput("rate2", "Compare Rate (%)",
        value = 7.0, min = 0, max = 25, step = 0.05),

      div(class = "filter-label", "Monthly Costs"),

      numericInput("annual_tax", "Property Taxes (monthly $)",
        value = 367, min = 0, step = 10),

      numericInput("annual_insurance", "Home Insurance (monthly $)",
        value = 125, min = 0, step = 10),

      numericInput("hoa", "HOA Fees (monthly $)",
        value = 0, min = 0, step = 50),

      uiOutput("pmi_notice")
    ),

    # ── RIGHT: Results ──────────────────────────────────────
    div(class = "calc-results",

      uiOutput("stats_ui"),

      div(class = "ch-card",
        tags$h3(HTML('Balance as of <span id="hp-title-year">—</span>')),
        div(class = "sublabel", "Hover over the chart to see values at any point in the loan"),
        div(class = "chart-hover-wrap",
          # Panel sits above chart — values written by JS
          div(class = "hover-panel",
            div(class = "hover-row",
              div(class = "hover-label", "Loan balance"),
              div(class = "hover-value",
                  style = paste0("color:", ri_green, ";"), id = "hp-balance", "—")
            ),
            div(class = "hover-row",
              div(class = "hover-label", "Principal paid"),
              div(class = "hover-value",
                  style = paste0("color:", ri_green_dark, ";"), id = "hp-principal", "—")
            ),
            div(class = "hover-row",
              div(class = "hover-label", "Interest paid"),
              div(class = "hover-value",
                  style = paste0("color:", ri_violet, ";"), id = "hp-interest", "—")
            )
          ),
          div(id = "amort_chart_wrap", style = "width:100%; min-width:0; overflow:hidden;",
            plotlyOutput("amort_chart", width = "100%", height = "300px")
          )
        )
      ),

      div(class = "ch-card",
        tags$h3("Rate Comparison"),
        div(class = "sublabel",
          "Same home and loan term — different interest rates"),
        uiOutput("compare_strip_ui"),
        plotlyOutput("rate_compare_chart", width = "100%", height = "240px")
      ),

      div(class = "ch-card",
        tags$h3("Monthly Payment by Rate"),
        div(class = "sublabel",
          "How your P&I payment changes across interest rates (0–20%)"),
        plotlyOutput("rate_curve_chart", width = "100%", height = "220px")
      ),

      div(class = "ch-card",
        tags$h3("Amortization Schedule"),
        div(class = "sublabel",
          "Annual principal paid, interest paid, and remaining balance"),
        div(class = "amort-wrap", uiOutput("amort_table"))
      )
    )
  ),

  ri_footer("Roosevelt Institute · rooseveltinstitute.org")
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Down payment sync ──────────────────────────────────────
  observeEvent(input$home_price, {
    req(isTruthy(input$home_price), isTruthy(input$down_pct))
    new_amt <- round(input$home_price * input$down_pct / 100)
    if (!isTRUE(all.equal(new_amt, input$down_amount)))
      updateNumericInput(session, "down_amount", value = new_amt)
  }, ignoreInit = TRUE)

  observeEvent(input$down_pct, {
    req(isTruthy(input$home_price), input$home_price > 0)
    new_amt <- round(input$home_price * input$down_pct / 100)
    if (!isTRUE(all.equal(new_amt, input$down_amount)))
      updateNumericInput(session, "down_amount", value = new_amt)
  }, ignoreInit = TRUE)

  observeEvent(input$down_amount, {
    req(isTruthy(input$home_price), input$home_price > 0, isTruthy(input$down_amount))
    new_pct <- round(input$down_amount / input$home_price * 100, 1)
    if (!isTRUE(all.equal(new_pct, input$down_pct)))
      updateNumericInput(session, "down_pct", value = new_pct)
  }, ignoreInit = TRUE)

  # ── Core calculation ────────────────────────────────────────
  calc <- reactive({
    req(isTruthy(input$home_price), isTruthy(input$down_pct),
        isTruthy(input$rate), isTruthy(input$term),
        isTruthy(input$annual_tax), isTruthy(input$annual_insurance),
        isTruthy(input$hoa))

    home_price <- max(input$home_price, 1)
    down_pct   <- max(min(input$down_pct, 100), 0)
    loan       <- home_price * (1 - down_pct / 100)
    term_yrs   <- as.numeric(input$term)
    rate       <- max(input$rate, 0)
    n          <- term_yrs * 12
    r          <- rate / 100 / 12

    pi    <- monthly_pi(loan, rate, term_yrs)
    tax   <- max(input$annual_tax, 0)
    ins   <- max(input$annual_insurance, 0)
    pmi   <- if (down_pct < 20) loan * 0.005 / 12 else 0
    hoa   <- max(input$hoa, 0)
    total <- pi + tax + ins + pmi + hoa

    df         <- amort_by_year(loan, r, n, pi)
    total_int  <- sum(df$interest)
    total_cost <- loan + total_int

    list(pi = pi, tax = tax, ins = ins, pmi = pmi, hoa = hoa,
         total = total, down_pct = down_pct,
         loan = loan, n = n, r = r, term_yrs = term_yrs,
         rate = rate, home_price = home_price,
         df = df, total_int = total_int, total_cost = total_cost)
  })

  # Send amort data to JS whenever inputs change
  observeEvent(calc(), {
    cv  <- calc()
    df  <- cv$df
    yr0 <- as.integer(format(Sys.Date(), "%Y"))
    cal <- yr0 + c(0, df$year)
    session$sendCustomMessage("amortData", list(
      cal       = as.list(cal),
      balance   = as.list(c(cv$loan,  df$balance)),
      principal = as.list(c(0,        df$cum_principal)),
      interest  = as.list(c(0,        df$cum_interest)),
      n         = length(cal)
    ))
  })

  # ── PMI notice ──────────────────────────────────────────────
  output$pmi_notice <- renderUI({
    cv <- calc()
    if (cv$down_pct < 20) {
      div(class = "pmi-notice",
        tags$b("PMI included:"),
        paste0(" With less than 20% down, an estimated PMI of ",
               fmt_dollar(cv$pmi), "/mo (0.5% annual rate) is added. ",
               "PMI can typically be removed once you reach 20% equity.")
      )
    }
  })

  # ── Key stats strip ─────────────────────────────────────────
  output$stats_ui <- renderUI({
    cv <- calc()
    ri_stat_strip(
      ri_stat_card(fmt_dollar(cv$total),      "Est. Monthly Payment"),
      ri_stat_card(fmt_dollar(cv$loan),       "Loan Amount"),
      ri_stat_card(fmt_dollar(cv$total_int),  "Total Interest Paid"),
      ri_stat_card(fmt_dollar(cv$total_cost), "Total Cost of Loan")
    )
  })

  # ── Amortization line chart ──────────────────────────────────
  output$amort_chart <- renderPlotly({
    cv  <- calc()
    df  <- cv$df
    yr0 <- as.integer(format(Sys.Date(), "%Y"))
    cal <- yr0 + c(0, df$year)

    balance  <- c(cv$loan, df$balance)
    cum_prin <- c(0, df$cum_principal)
    cum_int  <- c(0, df$cum_interest)

    plot_ly(x = cal, y = balance, type = "scatter", mode = "lines",
            name = "Loan balance",
            line = list(color = ri_green, width = 2.5),
            hovertemplate = paste0("<b style='color:", ri_green, ";'>$%{y:,.0f}</b><extra></extra>"),
            showlegend = FALSE) %>%
      add_trace(y = cum_prin, name = "Principal paid",
                type = "scatter", mode = "lines",
                line = list(color = ri_green_dark, width = 2.5),
                hovertemplate = paste0("<b style='color:", ri_green_dark, ";'>$%{y:,.0f}</b><extra></extra>"),
                showlegend = FALSE) %>%
      add_trace(y = cum_int, name = "Interest paid",
                type = "scatter", mode = "lines",
                line = list(color = ri_violet, width = 2.5),
                hovertemplate = paste0("<b style='color:", ri_violet, ";'>$%{y:,.0f}</b><extra></extra>"),
                showlegend = FALSE) %>%
      layout(
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        font          = list(family = RI_FONT_FAMILY, size = RI_FONT_SIZE),
        margin        = list(l = 55, r = 25, t = 10, b = 40),
        showlegend    = FALSE,
        hovermode     = "x",
        hoverlabel    = list(
          bgcolor     = "#fff",
          bordercolor = "#EBEBEB",
          font        = list(family = RI_FONT_FAMILY, size = 9, color = "#1A1A1A")
        ),
        xaxis = list(
          tickfont    = list(size = RI_FONT_SIZE, family = RI_FONT_FAMILY, color = "#333"),
          gridcolor   = RI_GRID_COLOR,
          zeroline    = FALSE,
          showline    = FALSE,
          dtick       = 5,
          tickformat  = "d",
          hoverformat = " ",
          showspikes  = TRUE,
          spikemode   = "across",
          spikecolor  = "#888",
          spikethickness = 1,
          spikedash   = "solid"
        ),
        yaxis = list(
          tickfont    = list(size = RI_FONT_SIZE, family = RI_FONT_FAMILY, color = "#333"),
          gridcolor   = RI_GRID_COLOR,
          zeroline    = FALSE,
          showline    = FALSE,
          tickprefix  = "$",
          tickformat  = "~s"
        )
      ) %>%
      ri_config()
  })

  # ── Second-rate scenario ────────────────────────────────────
  calc2 <- reactive({
    cv <- calc()
    req(isTruthy(input$rate2))
    rate2 <- max(input$rate2, 0)
    n2    <- cv$term_yrs * 12
    r2    <- rate2 / 100 / 12
    pi2   <- monthly_pi(cv$loan, rate2, cv$term_yrs)
    df2   <- amort_by_year(cv$loan, r2, n2, pi2)
    list(rate = rate2, pi = pi2, df = df2, total_int = sum(df2$interest))
  })

  # ── Rate comparison stat strip ───────────────────────────────
  output$compare_strip_ui <- renderUI({
    cv  <- calc()
    cv2 <- calc2()
    pi_diff  <- cv2$pi       - cv$pi
    int_diff <- cv2$total_int - cv$total_int

    sign_fmt <- function(x) paste0(if (x >= 0) "+" else "−", fmt_dollar(abs(x)))
    hi_col   <- function(x) if (x > 0) "#AA0061" else "#58A618"

    mk_block <- function(lbl, pi_val, int_val, col) {
      div(style = paste0(
            "flex:1; min-width:110px; padding:10px 12px;",
            " border:1px solid #EBEBEB; border-top:3px solid ", col, ";",
            " border-radius:4px;"),
        div(style = "font-size:9px; font-weight:700; text-transform:uppercase;
                     letter-spacing:.5px; color:#5E6A71; margin-bottom:6px;", lbl),
        div(style = paste0("font-size:15px; font-weight:700; color:", col, ";"), pi_val),
        div(style = "font-size:9px; color:#5E6A71; margin-bottom:6px;", "monthly P&I"),
        div(style = paste0("font-size:15px; font-weight:700; color:", col, ";"), int_val),
        div(style = "font-size:9px; color:#5E6A71;", "total interest")
      )
    }

    div(style = "display:flex; gap:10px; margin-bottom:14px; flex-wrap:wrap;",
      mk_block(paste0(cv$rate,  "%"), fmt_dollar(cv$pi),  fmt_dollar(cv$total_int),  "#008C95"),
      mk_block(paste0(cv2$rate, "%"), fmt_dollar(cv2$pi), fmt_dollar(cv2$total_int), "#AA0061"),
      div(style = "flex:1; min-width:110px; padding:10px 12px; border:1px solid #EBEBEB;
                   border-top:3px solid #EBEBEB; border-radius:4px; background:#F9F9F9;",
        div(style = "font-size:9px; font-weight:700; text-transform:uppercase;
                     letter-spacing:.5px; color:#5E6A71; margin-bottom:6px;", "Difference"),
        div(style = paste0("font-size:15px; font-weight:700; color:", hi_col(pi_diff), ";"),
            sign_fmt(pi_diff)),
        div(style = "font-size:9px; color:#5E6A71; margin-bottom:6px;", "per month"),
        div(style = paste0("font-size:15px; font-weight:700; color:", hi_col(int_diff), ";"),
            sign_fmt(int_diff)),
        div(style = "font-size:9px; color:#5E6A71;", "total interest")
      )
    )
  })

  # ── Balance comparison chart ─────────────────────────────────
  output$rate_compare_chart <- renderPlotly({
    cv  <- calc()
    cv2 <- calc2()
    yr0 <- as.integer(format(Sys.Date(), "%Y"))
    cal1 <- yr0 + c(0, cv$df$year);  bal1 <- c(cv$loan, cv$df$balance)
    cal2 <- yr0 + c(0, cv2$df$year); bal2 <- c(cv$loan, cv2$df$balance)
    l1 <- paste0(cv$rate, "%");  l2 <- paste0(cv2$rate, "%")

    plot_ly(x = cal1, y = bal1, type = "scatter", mode = "lines", name = l1,
            line = list(color = "#008C95", width = 2.5),
            hovertemplate = paste0("<b style='color:#008C95;'>$%{y:,.0f}</b><extra>", l1, "</extra>")) %>%
      add_trace(x = cal2, y = bal2, name = l2, type = "scatter", mode = "lines",
                line = list(color = "#AA0061", width = 2.5),
                hovertemplate = paste0("<b style='color:#AA0061;'>$%{y:,.0f}</b><extra>", l2, "</extra>")) %>%
      layout(
        paper_bgcolor = "white", plot_bgcolor = "white",
        font      = list(family = RI_FONT_FAMILY, size = RI_FONT_SIZE),
        margin    = list(l = 55, r = 20, t = 30, b = 40),
        showlegend = TRUE,
        legend    = list(orientation = "h", x = 0, y = 1.18,
                         font = list(family = RI_FONT_FAMILY, size = 9)),
        hovermode = "x",
        hoverlabel = list(bgcolor = "#fff", bordercolor = "#EBEBEB",
                          font = list(family = RI_FONT_FAMILY, size = 9, color = "#1A1A1A")),
        xaxis = list(tickfont = list(size = RI_FONT_SIZE, family = RI_FONT_FAMILY, color = "#333"),
                     gridcolor = RI_GRID_COLOR, zeroline = FALSE, showline = FALSE,
                     dtick = 5, tickformat = "d"),
        yaxis = list(tickfont = list(size = RI_FONT_SIZE, family = RI_FONT_FAMILY, color = "#333"),
                     gridcolor = RI_GRID_COLOR, zeroline = FALSE, showline = FALSE,
                     tickprefix = "$", tickformat = "~s")
      ) %>%
      ri_config()
  })

  # ── Monthly payment curve (0–20%) ───────────────────────────
  output$rate_curve_chart <- renderPlotly({
    cv     <- calc()
    rates  <- seq(0, 20, by = 0.1)
    pi_vals <- sapply(rates, function(r) monthly_pi(cv$loan, r, cv$term_yrs))
    cur_pi  <- cv$pi

    plot_ly() %>%
      add_trace(x = rates, y = pi_vals, type = "scatter", mode = "lines",
                line = list(color = "#008C95", width = 2),
                showlegend = FALSE,
                hovertemplate = "Rate: %{x:.1f}%<br><b style='color:#008C95;'>$%{y:,.0f}/mo</b><extra></extra>") %>%
      add_trace(x = c(cv$rate, cv$rate), y = c(0, cur_pi),
                type = "scatter", mode = "lines",
                line = list(color = "#5E6A71", width = 1, dash = "dot"),
                showlegend = FALSE, hoverinfo = "none") %>%
      add_trace(x = c(cv$rate), y = c(cur_pi),
                type = "scatter", mode = "markers",
                marker = list(color = "#AA0061", size = 8),
                showlegend = FALSE,
                hovertemplate = paste0("<b style='color:#AA0061;'>$%{y:,.0f}/mo at ",
                                       cv$rate, "%</b><extra></extra>")) %>%
      layout(
        paper_bgcolor = "white", plot_bgcolor = "white",
        font    = list(family = RI_FONT_FAMILY, size = RI_FONT_SIZE),
        margin  = list(l = 60, r = 20, t = 10, b = 45),
        showlegend = FALSE,
        hovermode  = "x",
        hoverlabel = list(bgcolor = "#fff", bordercolor = "#EBEBEB",
                          font = list(family = RI_FONT_FAMILY, size = 9, color = "#1A1A1A")),
        xaxis = list(
          title    = list(text = "Interest Rate (%)",
                          font = list(size = RI_FONT_SIZE, color = "#5E6A71")),
          tickfont = list(size = RI_FONT_SIZE, family = RI_FONT_FAMILY, color = "#333"),
          gridcolor = RI_GRID_COLOR, zeroline = FALSE, showline = FALSE,
          ticksuffix = "%", range = c(0, 20)
        ),
        yaxis = list(
          tickfont  = list(size = RI_FONT_SIZE, family = RI_FONT_FAMILY, color = "#333"),
          gridcolor = RI_GRID_COLOR, zeroline = FALSE, showline = FALSE,
          tickprefix = "$", tickformat = "~s"
        )
      ) %>%
      ri_config()
  })

  # ── Amortization table ───────────────────────────────────────
  output$amort_table <- renderUI({
    cv <- calc()
    req(cv$loan > 0)

    df   <- cv$df
    rows <- lapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      tags$tr(
        tags$td(paste0("Year ", row$year)),
        tags$td(fmt_dollar(row$principal)),
        tags$td(fmt_dollar(row$interest)),
        tags$td(fmt_dollar(row$balance))
      )
    })

    rows <- c(rows, list(
      tags$tr(class = "amort-total",
        tags$td("Total"),
        tags$td(fmt_dollar(cv$loan)),
        tags$td(fmt_dollar(cv$total_int)),
        tags$td("—")
      )
    ))

    tags$table(class = "amort-tbl",
      tags$thead(tags$tr(
        tags$th("Year"),
        tags$th("Principal Paid"),
        tags$th("Interest Paid"),
        tags$th("Remaining Balance")
      )),
      tags$tbody(rows)
    )
  })
}

shinyApp(ui, server)

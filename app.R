library(shiny)
library(bslib)
library(scrutiny)

MAX_ROWS <- 15

# ── helpers ──────────────────────────────────────────────────────────────────

count_decimals <- function(s) {
  if (is.null(s) || !nzchar(trimws(s))) {
    return(0L)
  }
  s <- trimws(gsub(",", ".", s))
  if (grepl("\\.", s)) nchar(sub(".*\\.", "", s)) else 0L
}

parse_num <- function(s) {
  if (is.null(s) || !nzchar(trimws(s))) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(gsub(",", ".", trimws(s))))
}

safe_grim <- function(x_str, n_val) {
  x <- parse_num(x_str)
  n <- suppressWarnings(as.integer(n_val))
  if (is.na(x) || is.na(n) || n < 2) {
    return(NA)
  }
  tryCatch(
    grim(x = x, n = n, digits_x = count_decimals(x_str)),
    error = function(e) NA
  )
}

safe_grimmer <- function(x_str, sd_str, n_val) {
  x <- parse_num(x_str)
  sd <- parse_num(sd_str)
  n <- suppressWarnings(as.integer(n_val))
  if (is.na(x) || is.na(sd) || is.na(n) || n < 2) {
    return(list(ok = NA, reason = ""))
  }
  r <- tryCatch(
    grimmer(
      x = x,
      sd = sd,
      n = n,
      digits_x = count_decimals(x_str),
      digits_sd = count_decimals(sd_str),
      show_reason = TRUE
    ),
    error = function(e) NULL
  )
  if (is.null(r)) {
    return(list(ok = NA, reason = ""))
  }
  list(ok = as.logical(r[[1]]), reason = r[[2]])
}

friendly_reason <- function(reason) {
  if (reason == "GRIM inconsistent") {
    return("Mean fails GRIM")
  }
  if (grepl("GRIMMER inconsistent", reason)) {
    return("SD fails GRIMMER")
  }
  reason
}

result_ui <- function(ok, reason = NULL) {
  if (is.na(ok)) {
    return(span())
  }
  if (ok) {
    span(
      class = "badge rounded-pill bg-success px-3 py-2",
      HTML("&#10003;&nbsp; Consistent")
    )
  } else {
    tagList(
      span(
        class = "badge rounded-pill bg-danger px-3 py-2",
        HTML("&#10007;&nbsp; Inconsistent")
      ),
      if (!is.null(reason) && nzchar(reason)) {
        span(class = "text-muted small ms-2", friendly_reason(reason))
      }
    )
  }
}

summary_bar <- function(results_vec) {
  tested <- !is.na(results_vec)
  if (sum(tested) == 0) {
    return(NULL)
  }
  n_pass <- sum(results_vec[tested])
  n_fail <- sum(!results_vec[tested])
  n_tot <- sum(tested)
  div(
    class = "d-flex gap-4 align-items-center px-3 py-2 rounded mt-3",
    style = "background:#f1f3f5; font-size:.875rem; border-left: 3px solid #dee2e6;",
    span(
      class = "text-muted",
      paste(n_tot, if (n_tot == 1) "case" else "cases", "tested")
    ),
    span(class = "text-success fw-semibold", paste(n_pass, "consistent")),
    span(class = "text-danger fw-semibold", paste(n_fail, "inconsistent"))
  )
}

# ── row UI builders ───────────────────────────────────────────────────────────

grim_row_ui <- function(i) {
  div(
    class = "row g-2 align-items-center mb-1",
    div(
      class = "col-auto text-muted",
      style = "min-width:28px; font-size:.8rem; text-align:right;",
      i
    ),
    div(
      class = "col-4 col-sm-3 col-md-2",
      textInput(
        paste0("gm_x_", i),
        NULL,
        width = "100%",
        placeholder = "e.g. 5.23"
      )
    ),
    div(
      class = "col-4 col-sm-3 col-md-2",
      numericInput(
        paste0("gm_n_", i),
        NULL,
        value = NA,
        min = 2,
        step = 1,
        width = "100%"
      )
    ),
    div(
      class = "col d-flex align-items-center",
      uiOutput(paste0("gm_badge_", i))
    )
  )
}

grimmer_row_ui <- function(i) {
  div(
    class = "row g-2 align-items-center mb-1",
    div(
      class = "col-auto text-muted",
      style = "min-width:28px; font-size:.8rem; text-align:right;",
      i
    ),
    div(
      class = "col-3 col-md-2",
      textInput(
        paste0("gr_x_", i),
        NULL,
        width = "100%",
        placeholder = "e.g. 5.23"
      )
    ),
    div(
      class = "col-3 col-md-2",
      textInput(
        paste0("gr_sd_", i),
        NULL,
        width = "100%",
        placeholder = "e.g. 1.11"
      )
    ),
    div(
      class = "col-3 col-md-2",
      numericInput(
        paste0("gr_n_", i),
        NULL,
        value = NA,
        min = 2,
        step = 1,
        width = "100%"
      )
    ),
    div(
      class = "col d-flex align-items-center",
      uiOutput(paste0("gr_badge_", i))
    )
  )
}

# ── column header row ─────────────────────────────────────────────────────────

grim_header <- div(
  class = "row g-2 mb-1",
  style = "font-size:.8rem; font-weight:600; color:#868e96; text-transform:uppercase; letter-spacing:.05em;",
  div(style = "min-width:28px; flex:0 0 auto;"),
  div(class = "col-4 col-sm-3 col-md-2 ps-2", "Mean"),
  div(class = "col-4 col-sm-3 col-md-2 ps-2", "Sample size (n)"),
  div(class = "col ps-2", "Result")
)

grimmer_header <- div(
  class = "row g-2 mb-1",
  style = "font-size:.8rem; font-weight:600; color:#868e96; text-transform:uppercase; letter-spacing:.05em;",
  div(style = "min-width:28px; flex:0 0 auto;"),
  div(class = "col-3 col-md-2 ps-2", "Mean"),
  div(class = "col-3 col-md-2 ps-2", "SD"),
  div(class = "col-3 col-md-2 ps-2", "Sample size (n)"),
  div(class = "col ps-2", "Result")
)

# ── custom CSS ────────────────────────────────────────────────────────────────

custom_css <- tags$style(HTML(
  "
  body { background-color: #f8f9fa; }
  .navbar-brand { font-weight: 700; letter-spacing: -.02em; }
  .card { border: none; box-shadow: 0 1px 4px rgba(0,0,0,.08); }
  .card-header { background: white; border-bottom: 1px solid #e9ecef; font-weight: 600; font-size: 1rem; }
  .form-control, .form-select { border-color: #dee2e6; font-size: .9rem; }
  .form-control:focus { border-color: #2c7be5; box-shadow: 0 0 0 .2rem rgba(44,123,229,.15); }
  .btn-outline-primary { color: #2c7be5; border-color: #2c7be5; }
  .btn-outline-primary:hover { background: #2c7be5; }
  .badge { font-size: .8rem !important; font-weight: 500; letter-spacing: .01em; }
  .bg-success { background-color: #12b886 !important; }
  .bg-danger  { background-color: #fa5252 !important; }
  p.lead { font-size: .95rem; }
"
))

# ── UI ───────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title = "GRIM & GRIMMER Tester",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2c7be5",
    "navbar-bg" = "#1e3a5f",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  navbar_options = navbar_options(bg = "#1e3a5f", underline = FALSE),
  header = custom_css,

  # ── GRIM tab ───────────────────────────────────────────────────────────────
  nav_panel(
    "GRIM",
    div(
      class = "container py-4",
      style = "max-width:680px;",

      card(
        card_header("GRIM Test"),
        card_body(
          p(
            class = "text-muted mb-3",
            "GRIM (Granularity-Related Inconsistency of Means) checks whether a ",
            "reported mean is arithmetically possible given the sample size. ",
            "Enter each mean exactly as it appears in the paper ",
            tags$em("(trailing zeros matter: 5.20 ≠ 5.2)"),
            "."
          ),
          grim_header,
          uiOutput("grim_rows"),
          div(
            class = "d-flex gap-2 mt-3",
            actionButton(
              "grim_add",
              "+ Add row",
              class = "btn btn-outline-primary btn-sm"
            ),
            actionButton(
              "grim_rm",
              "− Remove last",
              class = "btn btn-outline-secondary btn-sm"
            )
          ),
          uiOutput("grim_summary")
        )
      )
    )
  ),

  # ── GRIMMER tab ────────────────────────────────────────────────────────────
  nav_panel(
    "GRIMMER",
    div(
      class = "container py-4",
      style = "max-width:820px;",

      card(
        card_header("GRIMMER Test"),
        card_body(
          p(
            class = "text-muted mb-3",
            "GRIMMER extends GRIM to also check whether a reported standard deviation (SD) ",
            "is consistent with the mean and sample size. ",
            "Enter all values exactly as reported ",
            tags$em("(trailing zeros matter)"),
            "."
          ),
          grimmer_header,
          uiOutput("grimmer_rows"),
          div(
            class = "d-flex gap-2 mt-3",
            actionButton(
              "grimmer_add",
              "+ Add row",
              class = "btn btn-outline-primary btn-sm"
            ),
            actionButton(
              "grimmer_rm",
              "− Remove last",
              class = "btn btn-outline-secondary btn-sm"
            )
          ),
          uiOutput("grimmer_summary")
        )
      )
    )
  )
)

# ── server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  grim_n <- reactiveVal(3)
  grimmer_n <- reactiveVal(3)

  output$grim_rows <- renderUI(lapply(seq_len(grim_n()), grim_row_ui))
  output$grimmer_rows <- renderUI(lapply(seq_len(grimmer_n()), grimmer_row_ui))

  observeEvent(input$grim_add, grim_n(min(grim_n() + 1, MAX_ROWS)))
  observeEvent(input$grim_rm, grim_n(max(grim_n() - 1, 1)))
  observeEvent(input$grimmer_add, grimmer_n(min(grimmer_n() + 1, MAX_ROWS)))
  observeEvent(input$grimmer_rm, grimmer_n(max(grimmer_n() - 1, 1)))

  # Pre-register all row outputs
  for (i in seq_len(MAX_ROWS)) {
    local({
      ii <- i

      output[[paste0("gm_badge_", ii)]] <- renderUI({
        x_str <- input[[paste0("gm_x_", ii)]]
        n_val <- input[[paste0("gm_n_", ii)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NULL)
        }
        result_ui(safe_grim(x_str, n_val))
      })

      output[[paste0("gr_badge_", ii)]] <- renderUI({
        x_str <- input[[paste0("gr_x_", ii)]]
        sd_str <- input[[paste0("gr_sd_", ii)]]
        n_val <- input[[paste0("gr_n_", ii)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NULL)
        }
        res <- safe_grimmer(x_str, sd_str, n_val)
        result_ui(res$ok, res$reason)
      })
    })
  }

  # Summary bars
  output$grim_summary <- renderUI({
    n <- grim_n()
    results <- vapply(
      seq_len(n),
      function(i) {
        x_str <- input[[paste0("gm_x_", i)]]
        n_val <- input[[paste0("gm_n_", i)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NA)
        }
        safe_grim(x_str, n_val)
      },
      logical(1)
    )
    summary_bar(results)
  })

  output$grimmer_summary <- renderUI({
    n <- grimmer_n()
    results <- vapply(
      seq_len(n),
      function(i) {
        x_str <- input[[paste0("gr_x_", i)]]
        sd_str <- input[[paste0("gr_sd_", i)]]
        n_val <- input[[paste0("gr_n_", i)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NA)
        }
        safe_grimmer(x_str, sd_str, n_val)$ok
      },
      logical(1)
    )
    summary_bar(results)
  })
}

shinyApp(ui, server)

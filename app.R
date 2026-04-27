library(shiny)
library(bslib)
library(scrutiny)

MAX_ROWS <- 15

# ── helpers ──────────────────────────────────────────────────────────────────

parse_num <- function(s) {
  if (is.null(s) || !nzchar(trimws(s))) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(gsub(",", ".", trimws(s))))
}

safe_grim <- function(x_str, n_str) {
  x <- parse_num(x_str)
  n <- suppressWarnings(as.integer(parse_num(n_str)))
  if (is.na(x) || is.na(n) || n < 2) {
    return(NA)
  }
  tryCatch(
    grim(x = x, n = n, digits_x = decimal_places_scalar(x_str)),
    error = function(e) NA
  )
}

safe_grimmer <- function(x_str, sd_str, n_str) {
  x <- parse_num(x_str)
  sd <- parse_num(sd_str)
  n <- suppressWarnings(as.integer(parse_num(n_str)))
  if (is.na(x) || is.na(sd) || is.na(n) || n < 2) {
    return(list(ok = NA, reason = ""))
  }
  r <- tryCatch(
    grimmer(
      x = x,
      sd = sd,
      n = n,
      digits_x = decimal_places_scalar(x_str),
      digits_sd = decimal_places_scalar(sd_str),
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
    style = "background:#f1f3f5; font-size:.875rem; border-left:3px solid #dee2e6;",
    span(
      class = "text-muted",
      paste(n_tot, if (n_tot == 1) "case" else "cases", "tested")
    ),
    span(class = "text-success fw-semibold", paste(n_pass, "consistent")),
    span(class = "text-danger fw-semibold", paste(n_fail, "inconsistent"))
  )
}

next_free <- function(active) {
  candidate <- setdiff(seq_len(MAX_ROWS), active)
  if (length(candidate) == 0) {
    return(NULL)
  }
  min(candidate)
}

# ── row UI builders ───────────────────────────────────────────────────────────

rm_btn <- function(id) {
  actionButton(
    id,
    label = HTML("&times;"),
    class = "btn btn-sm btn-link text-secondary p-0",
    style = "font-size:1.1rem; line-height:1; opacity:.5;",
    title = "Remove this row"
  )
}

grim_row_ui <- function(id, pos) {
  div(
    class = "row g-2 align-items-center mb-1",
    div(
      class = "col-auto text-muted",
      style = "min-width:28px; font-size:.8rem; text-align:right;",
      pos
    ),
    div(
      class = "col-4 col-sm-3 col-md-2",
      textInput(
        paste0("gm_x_", id),
        NULL,
        width = "100%",
        placeholder = "e.g. 5.23"
      )
    ),
    div(
      class = "col-4 col-sm-3 col-md-2",
      textInput(
        paste0("gm_n_", id),
        NULL,
        width = "100%",
        placeholder = "e.g. 30"
      )
    ),
    div(
      class = "col d-flex align-items-center",
      uiOutput(paste0("gm_badge_", id))
    ),
    div(class = "col-auto", rm_btn(paste0("gm_rm_", id)))
  )
}

grimmer_row_ui <- function(id, pos) {
  div(
    class = "row g-2 align-items-center mb-1",
    div(
      class = "col-auto text-muted",
      style = "min-width:28px; font-size:.8rem; text-align:right;",
      pos
    ),
    div(
      class = "col-3 col-md-2",
      textInput(
        paste0("gr_x_", id),
        NULL,
        width = "100%",
        placeholder = "e.g. 5.23"
      )
    ),
    div(
      class = "col-3 col-md-2",
      textInput(
        paste0("gr_sd_", id),
        NULL,
        width = "100%",
        placeholder = "e.g. 1.11"
      )
    ),
    div(
      class = "col-3 col-md-2",
      textInput(
        paste0("gr_n_", id),
        NULL,
        width = "100%",
        placeholder = "e.g. 30"
      )
    ),
    div(
      class = "col d-flex align-items-center",
      uiOutput(paste0("gr_badge_", id))
    ),
    div(class = "col-auto", rm_btn(paste0("gr_rm_", id)))
  )
}

# ── column headers ────────────────────────────────────────────────────────────

hdr_style <- "min-height:36px; font-size:.8rem; font-weight:600; color:#868e96; text-transform:uppercase; letter-spacing:.05em;"

grim_header <- div(
  class = "row g-2 align-items-end mb-0",
  style = hdr_style,
  div(class = "col-auto", style = "min-width:28px;"),
  div(class = "col-4 col-sm-3 col-md-2 ps-2", "Mean"),
  div(class = "col-4 col-sm-3 col-md-2 ps-2", "Sample size (n)"),
  div(class = "col ps-2", "Result"),
  div(class = "col-auto", style = "width:30px;")
)

grimmer_header <- div(
  class = "row g-2 align-items-end mb-0",
  style = hdr_style,
  div(class = "col-auto", style = "min-width:28px;"),
  div(class = "col-3 col-md-2 ps-2", "Mean"),
  div(class = "col-3 col-md-2 ps-2", "SD"),
  div(class = "col-3 col-md-2 ps-2", "Sample size (n)"),
  div(class = "col ps-2", "Result"),
  div(class = "col-auto", style = "width:30px;")
)

# ── custom CSS ────────────────────────────────────────────────────────────────

custom_css <- tags$style(HTML(
  "
  body { background-color: #f8f9fa; }
  .navbar-brand { font-weight: 700; letter-spacing: -.02em; }
  .card { border: none; box-shadow: 0 1px 4px rgba(0,0,0,.08); }
  .card-header { background: white; border-bottom: 1px solid #e9ecef; font-weight: 600; font-size: 1rem; }
  .form-control { border-color: #dee2e6; font-size: .9rem; }
  .form-control:focus { border-color: #2c7be5; box-shadow: 0 0 0 .2rem rgba(44,123,229,.15); }
  .btn-outline-primary { color: #2c7be5; border-color: #2c7be5; }
  .btn-outline-primary:hover { background: #2c7be5; color: white; }
  .badge { font-size: .8rem !important; font-weight: 500; letter-spacing: .01em; }
  .bg-success { background-color: #12b886 !important; }
  .bg-danger  { background-color: #fa5252 !important; }
  .shiny-input-container { margin-bottom: 0; }
  ::placeholder { color: #adb5bd !important; font-style: italic; }
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
            class = "mt-3",
            actionButton(
              "grim_add",
              "+ Add row",
              class = "btn btn-outline-primary btn-sm"
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
            class = "mt-3",
            actionButton(
              "grimmer_add",
              "+ Add row",
              class = "btn btn-outline-primary btn-sm"
            )
          ),
          uiOutput("grimmer_summary")
        )
      )
    )
  )
)

# ── server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  grim_slots <- reactiveVal(1:3)
  grimmer_slots <- reactiveVal(1:3)

  output$grim_rows <- renderUI({
    slots <- grim_slots()
    if (length(slots) == 0) {
      return(p(
        class = "text-muted fst-italic small mt-2 mb-0",
        "No rows. Click '+ Add row' to add one."
      ))
    }
    mapply(grim_row_ui, id = slots, pos = seq_along(slots), SIMPLIFY = FALSE)
  })

  output$grimmer_rows <- renderUI({
    slots <- grimmer_slots()
    if (length(slots) == 0) {
      return(p(
        class = "text-muted fst-italic small mt-2 mb-0",
        "No rows. Click '+ Add row' to add one."
      ))
    }
    mapply(grimmer_row_ui, id = slots, pos = seq_along(slots), SIMPLIFY = FALSE)
  })

  observeEvent(input$grim_add, {
    slots <- grim_slots()
    ns <- next_free(slots)
    if (!is.null(ns)) {
      updateTextInput(session, paste0("gm_x_", ns), value = "")
      updateTextInput(session, paste0("gm_n_", ns), value = "")
      grim_slots(c(slots, ns))
    }
  })

  observeEvent(input$grimmer_add, {
    slots <- grimmer_slots()
    ns <- next_free(slots)
    if (!is.null(ns)) {
      updateTextInput(session, paste0("gr_x_", ns), value = "")
      updateTextInput(session, paste0("gr_sd_", ns), value = "")
      updateTextInput(session, paste0("gr_n_", ns), value = "")
      grimmer_slots(c(slots, ns))
    }
  })

  # Pre-register outputs and remove observers for every possible slot
  for (i in seq_len(MAX_ROWS)) {
    local({
      ii <- i

      observeEvent(
        input[[paste0("gm_rm_", ii)]],
        {
          grim_slots(setdiff(grim_slots(), ii))
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      observeEvent(
        input[[paste0("gr_rm_", ii)]],
        {
          grimmer_slots(setdiff(grimmer_slots(), ii))
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      output[[paste0("gm_badge_", ii)]] <- renderUI({
        x_str <- input[[paste0("gm_x_", ii)]]
        n_str <- input[[paste0("gm_n_", ii)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NULL)
        }
        result_ui(safe_grim(x_str, n_str))
      })

      output[[paste0("gr_badge_", ii)]] <- renderUI({
        x_str <- input[[paste0("gr_x_", ii)]]
        sd_str <- input[[paste0("gr_sd_", ii)]]
        n_str <- input[[paste0("gr_n_", ii)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NULL)
        }
        res <- safe_grimmer(x_str, sd_str, n_str)
        result_ui(res$ok, res$reason)
      })
    })
  }

  output$grim_summary <- renderUI({
    slots <- grim_slots()
    results <- vapply(
      slots,
      function(i) {
        x_str <- input[[paste0("gm_x_", i)]]
        n_str <- input[[paste0("gm_n_", i)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NA)
        }
        safe_grim(x_str, n_str)
      },
      logical(1)
    )
    summary_bar(results)
  })

  output$grimmer_summary <- renderUI({
    slots <- grimmer_slots()
    results <- vapply(
      slots,
      function(i) {
        x_str <- input[[paste0("gr_x_", i)]]
        sd_str <- input[[paste0("gr_sd_", i)]]
        n_str <- input[[paste0("gr_n_", i)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NA)
        }
        safe_grimmer(x_str, sd_str, n_str)$ok
      },
      logical(1)
    )
    summary_bar(results)
  })
}

shinyApp(ui, server)

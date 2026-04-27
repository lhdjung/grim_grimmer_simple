library(shiny)
library(bslib)
library(scrutiny)

addResourcePath("images", "images")

MAX_ROWS <- 15

# ── helpers ──────────────────────────────────────────────────────────────────

parse_num <- function(s) {
  if (is.null(s) || !nzchar(trimws(s))) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(gsub(",", ".", trimws(s))))
}

safe_grim <- function(x_str, n_str, digits_x, items) {
  x <- parse_num(x_str)
  n <- suppressWarnings(as.integer(parse_num(n_str)))
  dx <- suppressWarnings(as.integer(digits_x))
  it <- suppressWarnings(as.integer(items))
  if (
    is.na(x) || is.na(n) || n < 2 || is.na(dx) || dx < 0 || is.na(it) || it < 1
  ) {
    return(NA)
  }
  tryCatch(grim(x = x, n = n, digits_x = dx, items = it), error = function(e) {
    NA
  })
}

safe_grimmer <- function(x_str, sd_str, n_str, digits_x, digits_sd, items) {
  x <- parse_num(x_str)
  sd <- parse_num(sd_str)
  n <- suppressWarnings(as.integer(parse_num(n_str)))
  dx <- suppressWarnings(as.integer(digits_x))
  ds <- suppressWarnings(as.integer(digits_sd))
  it <- suppressWarnings(as.integer(items))
  if (
    is.na(x) ||
      is.na(sd) ||
      is.na(n) ||
      n < 2 ||
      is.na(dx) ||
      dx < 0 ||
      is.na(ds) ||
      ds < 0 ||
      is.na(it) ||
      it < 1
  ) {
    return(list(ok = NA, reason = ""))
  }
  r <- tryCatch(
    grimmer(
      x = x,
      sd = sd,
      n = n,
      digits_x = dx,
      digits_sd = ds,
      items = it,
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

short_reason <- function(reason) {
  if (reason == "GRIM inconsistent") {
    return("GRIM")
  }
  if (grepl("GRIMMER inconsistent", reason)) {
    m <- regmatches(reason, regexpr("\\d+", reason))
    if (length(m) > 0 && nzchar(m)) {
      return(paste0("Test ", m))
    }
    return("GRIMMER")
  }
  reason
}

# ── validation ────────────────────────────────────────────────────────────────

# Returns an error string, or NULL if all inputs are valid / not yet entered.
validate_grim_row <- function(x_str, n_str, dp_x, items) {
  if (!is.null(x_str) && nzchar(trimws(x_str))) {
    if (is.na(parse_num(x_str))) {
      return("Mean must be a number")
    }
    if (!is.null(dp_x) && !is.na(dp_x) && dp_x != round(dp_x)) {
      return("Decimal places must be a whole number")
    }
    dp_int <- suppressWarnings(as.integer(dp_x))
    if (!is.na(dp_int)) {
      actual <- decimal_places_scalar(x_str)
      if (dp_int < actual) {
        return(sprintf(
          "Mean has %d decimal place%s but d.p. is set to %d",
          actual,
          if (actual == 1L) "" else "s",
          dp_int
        ))
      }
    }
  }
  if (!is.null(n_str) && nzchar(trimws(n_str))) {
    n_num <- parse_num(n_str)
    if (is.na(n_num)) {
      return("N must be a number")
    }
    if (n_num != round(n_num)) {
      return("N must be a whole number")
    }
    if (n_num < 2) return("N must be at least 2")
  }
  if (!is.null(items) && !is.na(items)) {
    if (items != round(items) || items < 1) {
      return("Items must be a positive whole number")
    }
  }
  NULL
}

validate_grimmer_row <- function(x_str, sd_str, n_str, dp_x, dp_sd, items) {
  err <- validate_grim_row(x_str, n_str, dp_x, items)
  if (!is.null(err)) {
    return(err)
  }
  if (!is.null(sd_str) && nzchar(trimws(sd_str))) {
    if (is.na(parse_num(sd_str))) {
      return("SD must be a number")
    }
    if (!is.null(dp_sd) && !is.na(dp_sd) && dp_sd != round(dp_sd)) {
      return("SD decimal places must be a whole number")
    }
    dp_int <- suppressWarnings(as.integer(dp_sd))
    if (!is.na(dp_int)) {
      actual <- decimal_places_scalar(sd_str)
      if (dp_int < actual) {
        return(sprintf(
          "SD has %d decimal place%s but d.p. is set to %d",
          actual,
          if (actual == 1L) "" else "s",
          dp_int
        ))
      }
    }
  }
  NULL
}

# ── result / error UI ─────────────────────────────────────────────────────────

error_ui <- function(msg) {
  span(
    style = "color:#e67e22; font-size:.8rem;",
    HTML("&#9888;&nbsp;"),
    msg
  )
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
    short <- if (!is.null(reason) && nzchar(reason)) {
      short_reason(reason)
    } else {
      NULL
    }
    if (!is.null(short)) {
      div(
        class = "d-flex align-items-center gap-2",
        span(
          class = "badge rounded-pill bg-danger px-3 py-2",
          HTML("&#10007;&nbsp; Inconsistent")
        ),
        span(
          class = "text-danger",
          style = "font-size:.75rem; white-space:nowrap;",
          short
        )
      )
    } else {
      span(
        class = "badge rounded-pill bg-danger px-3 py-2",
        HTML("&#10007;&nbsp; Inconsistent")
      )
    }
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

# ── row UI (pre-created; show/hide via CSS) ───────────────────────────────────

rm_btn <- function(id) {
  actionButton(
    id,
    label = tags$img(
      src = "images/trash-can.svg",
      height = "16px",
      alt = "Remove"
    ),
    class = "btn btn-sm p-1 rm-btn",
    style = "line-height:1;",
    title = "Remove this row"
  )
}

# numericInput for decimal places (shared by GRIM and GRIMMER rows)
dp_input <- function(id) {
  numericInput(id, NULL, value = 2, min = 0, max = 10, step = 1, width = "80px")
}

items_input <- function(id) {
  numericInput(id, NULL, value = 1, min = 1, step = 1, width = "70px")
}

grim_row <- function(id) {
  div(
    id = paste0("grim_slot_", id),
    class = "row g-2 align-items-center mb-1",
    style = if (id <= 3) "" else "display:none;",
    div(
      class = "col-3",
      textInput(
        paste0("gm_x_", id),
        NULL,
        width = "100%",
        placeholder = "e.g. 5.23"
      )
    ),
    div(class = "col-auto", dp_input(paste0("gm_dp_", id))),
    div(
      class = "col-auto",
      textInput(
        paste0("gm_n_", id),
        NULL,
        width = "110px",
        placeholder = "e.g. 30"
      )
    ),
    div(class = "col-auto", items_input(paste0("gm_items_", id))),
    div(
      class = "col d-flex align-items-center",
      uiOutput(paste0("gm_badge_", id))
    ),
    div(class = "col-auto", rm_btn(paste0("gm_rm_", id)))
  )
}

grimmer_row <- function(id) {
  div(
    id = paste0("grimmer_slot_", id),
    class = "row g-2 align-items-center mb-1",
    style = if (id <= 3) "" else "display:none;",
    div(
      class = "col",
      textInput(
        paste0("gr_x_", id),
        NULL,
        width = "100%",
        placeholder = "e.g. 5.23"
      )
    ),
    div(class = "col-auto", dp_input(paste0("gr_dp_x_", id))),
    div(
      class = "col",
      textInput(
        paste0("gr_sd_", id),
        NULL,
        width = "100%",
        placeholder = "e.g. 1.11"
      )
    ),
    div(class = "col-auto", dp_input(paste0("gr_dp_sd_", id))),
    div(
      class = "col-auto",
      textInput(
        paste0("gr_n_", id),
        NULL,
        width = "110px",
        placeholder = "e.g. 30"
      )
    ),
    div(class = "col-auto", items_input(paste0("gr_items_", id))),
    div(
      class = "col d-flex align-items-center",
      uiOutput(paste0("gr_badge_", id))
    ),
    div(class = "col-auto", rm_btn(paste0("gr_rm_", id)))
  )
}

# ── column headers ────────────────────────────────────────────────────────────
# col-auto header cells wrap a fixed-width inner div matching the input width,
# so they align with col-auto data cells regardless of Bootstrap gutter.

hdr_style <- "min-height:36px; font-size:.8rem; font-weight:600; color:#868e96; text-transform:uppercase; letter-spacing:.05em;"
dp_hdr <- div(
  style = "width:80px; padding-left:2px; white-space:nowrap;",
  "Decimals"
)
n_hdr <- div(
  style = "width:110px; padding-left:2px; white-space:nowrap; text-transform:none; letter-spacing:0;",
  "SAMPLE SIZE (N)"
)
items_hdr <- div(
  style = "width:70px; padding-left:2px; white-space:nowrap;",
  "Items"
)

grim_header <- div(
  class = "row g-2 align-items-end mb-0",
  style = hdr_style,
  div(class = "col-3 ps-2", "Mean"),
  div(class = "col-auto", dp_hdr),
  div(class = "col-auto", n_hdr),
  div(class = "col-auto", items_hdr),
  div(class = "col ps-2", "Result")
)

grimmer_header <- div(
  class = "row g-2 align-items-end mb-0",
  style = hdr_style,
  div(class = "col ps-2", "Mean"),
  div(class = "col-auto", dp_hdr),
  div(class = "col ps-2", "SD"),
  div(class = "col-auto", dp_hdr),
  div(class = "col-auto", n_hdr),
  div(class = "col-auto", items_hdr),
  div(class = "col ps-2", "Result")
)

# ── custom CSS ────────────────────────────────────────────────────────────────

custom_css <- tags$style(HTML(
  "
  body { background-color: #f8f9fa; }

  /* ── navbar ─────────────────────────────────────────────────────────── */
  nav.navbar {
    min-height: 56px;
  }
  nav.navbar > .container-fluid {
    align-items: center !important;
    min-height: 56px;
  }
  .navbar-brand {
    padding: 0 1rem 0 0 !important;
    margin-right: .5rem !important;
    border-right: 5px solid rgba(255,255,255,.25);
    align-self: stretch;
    display: flex !important;
    align-items: center !important;
    font-weight: 700;
    letter-spacing: -.02em;
  }
  .navbar-brand img {
    display: block;
    height: 56px;
    width: auto;
  }
  nav.navbar .navbar-nav {
    align-items: center !important;
  }
  nav.navbar .nav-link {
    border-bottom: none !important;
  }

  /* ── cards & inputs ─────────────────────────────────────────────────── */
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
  .rm-btn { background: transparent !important; border: none !important; opacity: 1 !important; }
  .rm-btn img { opacity: 1 !important; filter: none; }
  .rm-btn:hover { background-color: #fa5252 !important; border-radius: 4px; }
  .rm-btn:hover img { filter: brightness(0) invert(1) !important; }
"
))

# ── UI ───────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title = div(
    class = "d-flex align-items-center gap-2",
    tags$img(
      src = "images/inspect-sr.png",
      alt = "scrutiny",
      height = "56"
    ),
    "GRIM & GRIMMER Tester"
  ),
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2c7be5",
    "navbar-bg" = "#1e3a5f",
    "navbar-padding-y" = "0",
    "navbar-brand-padding-y" = "0",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  navbar_options = navbar_options(bg = "#1e3a5f", underline = FALSE),
  header = tagList(custom_css),

  # ── GRIM tab ───────────────────────────────────────────────────────────────
  nav_panel(
    "GRIM",
    div(
      class = "container py-4",
      style = "max-width:720px;",
      card(
        card_header("GRIM Test"),
        card_body(
          # fmt: skip
          p(
            class = "text-muted mb-3",
            "GRIM (granularity-related inconsistency of means) checks whether a ",
            "reported mean is arithmetically possible given the sample size. ",
            br(), br(), "Set ", tags$em("Decimals"),
            " to the number of decimal places to which the mean was reported.",
            "This is needed because trailing zeros matter for granularity:",
            tags$em("5.20 ≠ 5.2"), ". Multi-item / mean-scored measures",
            "require the number of items in", tags$em("Items.")
          ),
          grim_header,
          tagList(lapply(seq_len(MAX_ROWS), grim_row)),
          uiOutput("grim_empty"),
          uiOutput("grim_vis"),
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
      style = "max-width:900px;",
      card(
        card_header("GRIMMER Test"),
        card_body(
          # fmt: skip
          p(
            class = "text-muted mb-3",
            "GRIMMER extends GRIM to also check whether a reported standard ",
            "deviation (SD) is consistent with the mean and sample size. ",
            tags$em("Result"), "shows the reason for any inconsistencies.",
            br(), br(),  "Set ", tags$em("Decimals"),
            " for mean and SD to the number of decimal places they were",
            "reported to. Multi-item / mean-scored measures require the number",
            "of items in", tags$em("Items.")
          ),
          grimmer_header,
          tagList(lapply(seq_len(MAX_ROWS), grimmer_row)),
          uiOutput("grimmer_empty"),
          uiOutput("grimmer_vis"),
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
  ),

  nav_panel(
    "About",
    div(
      class = "container py-4",
      style = "max-width:900px;",
      card(
        card_header("About"),
        card_body(
          p(
            class = "text-muted mb-3",
            "GRIM and GRIMMER represent",
            a(
              "INSPECT-SR check 4.8",
              href = "https://inspect.sr/chapters/check_4_8.html",
              .noWS = "after"
            ),
            ". INSPECT-SR (",
            a(
              "Wilkinson et al., 2025",
              href = "https://www.medrxiv.org/content/10.1101/2025.09.03.25334905v3",
              .noWS = "outside"
            ),
            ") is a framework for assessing the trustworthiness of",
            "randomised controlled trials in systematic reviews. However, it",
            "can be applied to other fields, as well.",
            br(),
            br(),
            "Shiny app made by Lukas Jung, University of Bern, using the",
            a(
              "scrutiny",
              href = "https://lhdjung.github.io/scrutiny/"
            ),
            "package for error detection in science."
          )
        )
      )
    )
  )
)

# ── server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  grim_slots <- reactiveVal(1:3)
  grimmer_slots <- reactiveVal(1:3)

  vis_css <- function(slots, prefix) {
    rules <- vapply(
      seq_len(MAX_ROWS),
      function(i) {
        display <- if (i %in% slots) "flex" else "none"
        sprintf("#%s_slot_%d{display:%s!important}", prefix, i, display)
      },
      character(1)
    )
    tags$style(paste(rules, collapse = ""))
  }

  output$grim_vis <- renderUI(vis_css(grim_slots(), "grim"))
  output$grimmer_vis <- renderUI(vis_css(grimmer_slots(), "grimmer"))

  output$grim_empty <- renderUI({
    if (length(grim_slots()) == 0) {
      p(
        class = "text-muted fst-italic small mt-2 mb-0",
        "No rows. Click '+ Add row' to add one."
      )
    }
  })
  output$grimmer_empty <- renderUI({
    if (length(grimmer_slots()) == 0) {
      p(
        class = "text-muted fst-italic small mt-2 mb-0",
        "No rows. Click '+ Add row' to add one."
      )
    }
  })

  observeEvent(input$grim_add, {
    slots <- grim_slots()
    ns <- next_free(slots)
    if (!is.null(ns)) grim_slots(c(slots, ns))
  })
  observeEvent(input$grimmer_add, {
    slots <- grimmer_slots()
    ns <- next_free(slots)
    if (!is.null(ns)) grimmer_slots(c(slots, ns))
  })

  # Pre-register outputs and remove observers for every possible slot
  for (i in seq_len(MAX_ROWS)) {
    local({
      ii <- i

      observeEvent(
        input[[paste0("gm_rm_", ii)]],
        {
          current <- grim_slots()
          if (ii %in% current) {
            updateTextInput(session, paste0("gm_x_", ii), value = "")
            updateTextInput(session, paste0("gm_n_", ii), value = "")
            updateNumericInput(session, paste0("gm_dp_", ii), value = 2)
            updateNumericInput(session, paste0("gm_items_", ii), value = 1)
            grim_slots(setdiff(current, ii))
          }
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      observeEvent(
        input[[paste0("gr_rm_", ii)]],
        {
          current <- grimmer_slots()
          if (ii %in% current) {
            updateTextInput(session, paste0("gr_x_", ii), value = "")
            updateTextInput(session, paste0("gr_sd_", ii), value = "")
            updateTextInput(session, paste0("gr_n_", ii), value = "")
            updateNumericInput(session, paste0("gr_dp_x_", ii), value = 2)
            updateNumericInput(session, paste0("gr_dp_sd_", ii), value = 2)
            updateNumericInput(session, paste0("gr_items_", ii), value = 1)
            grimmer_slots(setdiff(current, ii))
          }
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      output[[paste0("gm_badge_", ii)]] <- renderUI({
        x_str <- input[[paste0("gm_x_", ii)]]
        n_str <- input[[paste0("gm_n_", ii)]]
        dp <- input[[paste0("gm_dp_", ii)]]
        items <- input[[paste0("gm_items_", ii)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NULL)
        }
        err <- validate_grim_row(x_str, n_str, dp, items)
        if (!is.null(err)) {
          return(error_ui(err))
        }
        result_ui(safe_grim(x_str, n_str, dp, items))
      })

      output[[paste0("gr_badge_", ii)]] <- renderUI({
        x_str <- input[[paste0("gr_x_", ii)]]
        sd_str <- input[[paste0("gr_sd_", ii)]]
        n_str <- input[[paste0("gr_n_", ii)]]
        dp_x <- input[[paste0("gr_dp_x_", ii)]]
        dp_sd <- input[[paste0("gr_dp_sd_", ii)]]
        items <- input[[paste0("gr_items_", ii)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NULL)
        }
        err <- validate_grimmer_row(x_str, sd_str, n_str, dp_x, dp_sd, items)
        if (!is.null(err)) {
          return(error_ui(err))
        }
        res <- safe_grimmer(x_str, sd_str, n_str, dp_x, dp_sd, items)
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
        dp <- input[[paste0("gm_dp_", i)]]
        items <- input[[paste0("gm_items_", i)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NA)
        }
        if (!is.null(validate_grim_row(x_str, n_str, dp, items))) {
          return(NA)
        }
        safe_grim(x_str, n_str, dp, items)
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
        dp_x <- input[[paste0("gr_dp_x_", i)]]
        dp_sd <- input[[paste0("gr_dp_sd_", i)]]
        items <- input[[paste0("gr_items_", i)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NA)
        }
        if (
          !is.null(validate_grimmer_row(
            x_str,
            sd_str,
            n_str,
            dp_x,
            dp_sd,
            items
          ))
        ) {
          return(NA)
        }
        safe_grimmer(x_str, sd_str, n_str, dp_x, dp_sd, items)$ok
      },
      logical(1)
    )
    summary_bar(results)
  })
}

shinyApp(ui, server)

library(shiny)
library(bslib)
library(scrutiny)

# # Deploy like this:
# rsconnect::deployApp(
#   appName = "inspect-sr-means-variances",
#   account = "errors"
# )

addResourcePath("images", "images")

MAX_ROWS <- 15


# Helpers -----------------------------------------------------------------

parse_num <- function(s) {
  if (is.null(s) || !nzchar(trimws(s))) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(gsub(",", ".", trimws(s))))
}

safe_grim <- function(x_str, n_str, items, percent = FALSE) {
  x <- parse_num(x_str)
  n <- suppressWarnings(as.integer(parse_num(n_str)))
  dx <- decimal_places_scalar(gsub(",", ".", trimws(x_str)))
  if (percent) {
    dx <- dx + 2L
  }
  it <- suppressWarnings(as.integer(items))
  if (anyNA(c(x, n, it)) || n < 2 || it < 1) {
    return(NA)
  }
  tryCatch(
    grim(x = x, n = n, digits_x = dx, items = it, percent = percent),
    error = function(e) NA
  )
}

grim_uninformative <- function(x_str, n_str, items, percent = FALSE) {
  n <- suppressWarnings(as.integer(parse_num(n_str)))
  dx <- decimal_places_scalar(gsub(",", ".", trimws(x_str)))
  it <- suppressWarnings(as.integer(items))
  x_clean <- gsub(",", ".", trimws(x_str))
  if (anyNA(c(n, dx, it)) || is.na(parse_num(x_str)) || n < 2 || it < 1) {
    return(FALSE)
  }
  p <- tryCatch(
    grim_probability(
      x = x_clean,
      n = n,
      digits_x = dx,
      items = it,
      percent = percent
    ),
    error = function(e) NA_real_
  )
  isTRUE(p == 0)
}

safe_grimmer <- function(x_str, sd_str, n_str, items) {
  x <- parse_num(x_str)
  sd <- parse_num(sd_str)
  n <- suppressWarnings(as.integer(parse_num(n_str)))
  dx <- decimal_places_scalar(gsub(",", ".", trimws(x_str)))
  ds <- decimal_places_scalar(gsub(",", ".", trimws(sd_str)))
  it <- suppressWarnings(as.integer(items))
  if (anyNA(c(x, sd, n, it)) || n < 2 || it < 1) {
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

short_reason <- function(reason) {
  if (reason == "GRIM inconsistent") {
    return("GRIM")
  }
  if (grepl("GRIMMER inconsistent", reason)) {
    m <- regmatches(reason, regexpr("\\d+", reason))
    if (length(m) > 0 && nzchar(m)) {
      return(paste0("GRIMMER ", m))
    }
    return("GRIMMER")
  }
  reason
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


# Validation --------------------------------------------------------------

validate_combined_row <- function(x_str, sd_str, n_str, items, type) {
  if (!is.null(x_str) && nzchar(trimws(x_str))) {
    if (is.na(parse_num(x_str))) return("Mean must be a number")
  }
  sd_given <- !is.null(sd_str) && nzchar(trimws(sd_str))
  if (sd_given && isTRUE(type == "Percentage")) {
    return("SD and percentage are incompatible")
  }
  if (sd_given && is.na(parse_num(sd_str))) {
    return("SD must be a number")
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


# Result / error UI -------------------------------------------------------

error_ui <- function(msg) {
  span(
    style = "color:#e67e22; font-size:.8rem;",
    HTML("&#9888;&nbsp;"),
    msg
  )
}

uninformative_label <- function() {
  span(
    class = "text-muted",
    style = "font-size:.75rem; white-space:nowrap; cursor:help;",
    title = paste(
      "Every possible mean is achievable for this N and item count,",
      "so the GRIM portion of this test cannot fail.",
      "GRIMMER's SD-based checks (and TIDES, where applicable) remain valid."
    ),
    "Uninformative GRIM"
  )
}

result_ui <- function(ok, reason = NULL, uninformative = FALSE) {
  if (is.na(ok)) {
    return(span())
  }
  if (ok) {
    badge <- span(
      class = "badge rounded-pill bg-success px-3 py-2",
      HTML("&#10003;&nbsp; Consistent")
    )
    if (uninformative) {
      div(
        class = "d-flex align-items-center gap-2",
        badge,
        uninformative_label()
      )
    } else {
      badge
    }
  } else {
    short <- if (!is.null(reason) && nzchar(reason)) {
      short_reason(reason)
    } else {
      NULL
    }
    badge <- span(
      class = "badge rounded-pill bg-danger px-3 py-2",
      HTML("&#10007;&nbsp; Inconsistent")
    )
    extras <- list()
    if (!is.null(short)) {
      extras <- c(
        extras,
        list(span(
          class = "text-danger",
          style = "font-size:.75rem; white-space:nowrap;",
          short
        ))
      )
    }
    if (uninformative) {
      extras <- c(extras, list(uninformative_label()))
    }
    if (length(extras) > 0) {
      do.call(
        div,
        c(
          list(class = "d-flex align-items-center gap-2", badge),
          extras
        )
      )
    } else {
      badge
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


# Row UI (pre-created; show/hide via CSS) ---------------------------------

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

items_input <- function(id) {
  numericInput(id, NULL, value = 1, min = 1, step = 1, width = "100%")
}

combined_row <- function(id) {
  div(
    id = paste0("cb_slot_", id),
    style = if (id <= 3) "" else "display:none;",
    div(
      class = "grid-cell",
      selectInput(
        paste0("cb_type_", id),
        NULL,
        choices = c("Mean", "Percentage"),
        selected = "Mean",
        width = "100%"
      )
    ),
    div(
      class = "grid-cell",
      textInput(
        paste0("cb_x_", id),
        NULL,
        width = "100%",
        placeholder = "e.g. 5.23"
      )
    ),
    div(
      class = "grid-cell",
      textInput(
        paste0("cb_sd_", id),
        NULL,
        width = "100%",
        placeholder = "optional"
      )
    ),
    div(
      class = "grid-cell",
      textInput(
        paste0("cb_n_", id),
        NULL,
        width = "100%",
        placeholder = "e.g. 30"
      )
    ),
    div(class = "grid-cell", items_input(paste0("cb_items_", id))),
    div(
      class = "grid-cell d-flex align-items-center",
      uiOutput(paste0("cb_badge_", id))
    ),
    div(class = "grid-cell", rm_btn(paste0("cb_rm_", id)))
  )
}


# Column headers ----------------------------------------------------------

combined_header <- div(
  div(class = "grid-hdr", "Type"),
  div(class = "grid-hdr", "Mean or percentage"),
  div(class = "grid-hdr", "SD (optional)"),
  div(class = "grid-hdr", "Sample size"),
  div(class = "grid-hdr", "Items averaged over"),
  div(class = "grid-hdr", "Result"),
  div()
)


# Custom CSS --------------------------------------------------------------

custom_css <- tags$style(HTML(
  "
  body { background-color: #f8f9fa; }

  /* ── navbar ─────────────────────────────────────────────────────────── */
  nav.navbar {
    min-height: 56px;
    padding-top: 0 !important;
    padding-bottom: 0 !important;
  }
  nav.navbar > .container-fluid {
    align-items: center !important;
    min-height: 56px;
  }
  .navbar-brand {
    position: relative;
    padding: 0 1.5rem 0 0 !important;
    margin-right: .75rem !important;
    align-self: stretch;
    display: flex !important;
    align-items: center !important;
    font-weight: 700;
    letter-spacing: -.02em;
  }
  .navbar-brand::after {
    content: '';
    position: absolute;
    right: 0;
    top: 15%;
    height: 70%;
    width: 1px;
    background: linear-gradient(to bottom, transparent, rgba(255,255,255,.5) 30%, rgba(255,255,255,.5) 70%, transparent);
  }
  .navbar-brand img {
    display: block;
    height: 56px;
    width: auto;
  }
  nav.navbar .navbar-nav {
    align-items: center !important;
    gap: .25rem;
  }
  nav.navbar .nav-link {
    border-bottom: none !important;
    border-radius: .375rem !important;
    padding: .35rem .8rem !important;
    color: rgba(255,255,255,.75) !important;
    font-weight: 500;
    transition: background-color .18s ease, color .18s ease;
  }
  nav.navbar .nav-link:hover {
    background-color: rgba(255,255,255,.12) !important;
    color: #fff !important;
  }
  nav.navbar .nav-link.active {
    background-color: #fff !important;
    color: #1e3a5f !important;
    font-weight: 600;
  }

  /* ── cards & inputs ─────────────────────────────────────────────────── */
  .card { border: none; box-shadow: 0 1px 4px rgba(0,0,0,.08); }
  .card-header { background: white; border-bottom: 1px solid #e9ecef; font-weight: 600; font-size: 1rem; }
  .form-control { border-color: #dee2e6; font-size: .9rem; }
  .form-control:focus { border-color: #2c7be5; box-shadow: 0 0 0 .2rem rgba(44,123,229,.15); }
  .btn-outline-primary { color: #2c7be5; border-color: #2c7be5; }
  .btn-outline-primary:hover { background: #2c7be5; color: white; }

  /* ── action buttons ──────────────────────────────────────────────────── */
  #combined_add, #download_csv {
    border: none !important;
    color: #fff !important;
    font-size: .875rem !important;
    font-weight: 500 !important;
    padding: .375rem .875rem !important;
    border-radius: .375rem !important;
    transition: background-color .2s ease, box-shadow .2s ease, transform .1s ease !important;
  }
  #combined_add {
    background-color: #2c7be5 !important;
    box-shadow: 0 1px 4px rgba(44,123,229,.35) !important;
  }
  #combined_add:hover, #combined_add:focus {
    background-color: #1a68d1 !important;
    color: #fff !important;
    box-shadow: 0 4px 12px rgba(44,123,229,.45) !important;
    transform: translateY(-1px);
  }
  #combined_add:active {
    background-color: #155ab8 !important;
    transform: translateY(0);
    box-shadow: 0 1px 4px rgba(44,123,229,.35) !important;
  }
  #download_csv {
    background-color: #495057 !important;
    box-shadow: 0 1px 4px rgba(73,80,87,.35) !important;
  }
  #download_csv:hover, #download_csv:focus {
    background-color: #343a40 !important;
    color: #fff !important;
    box-shadow: 0 4px 12px rgba(73,80,87,.45) !important;
    transform: translateY(-1px);
  }
  #download_csv:active {
    background-color: #212529 !important;
    transform: translateY(0);
    box-shadow: 0 1px 4px rgba(73,80,87,.35) !important;
  }
  .badge { font-size: .8rem !important; font-weight: 500; letter-spacing: .01em; }
  .bg-success { background-color: #12b886 !important; }
  .bg-danger  { background-color: #fa5252 !important; }
  .shiny-input-container { margin-bottom: 0; }
  ::placeholder { color: #adb5bd !important; font-style: italic; }
  .rm-btn { background: transparent !important; border: none !important; opacity: 1 !important; }
  .rm-btn img { opacity: 1 !important; filter: none; }
  .rm-btn:hover { background-color: #fa5252 !important; border-radius: 4px; }
  .rm-btn:hover img { filter: brightness(0) invert(1) !important; }

  /* ── input grid ──────────────────────────────────────────────────────── */
  .combined-grid {
    display: grid;
    grid-template-columns: 130px 1fr 1fr 110px 90px 1fr auto;
    column-gap: .5rem;
    row-gap: 0;
  }
  .combined-grid > div {
    display: grid;
    grid-column: 1 / -1;
    grid-template-columns: subgrid;
    align-items: center;
  }
  .combined-grid > div:first-child { align-items: end; }
  .grid-cell { padding: 2px 0; }
  .grid-hdr {
    padding: 4px 0 2px;
    font-size: .8rem;
    font-weight: 600;
    color: #868e96;
    text-transform: uppercase;
    letter-spacing: .05em;
    word-break: normal;
    hyphens: none;
  }
  .grid-hdr-n { text-transform: none; letter-spacing: 0; }
"
))


# Main UI -----------------------------------------------------------------

ui <- page_navbar(
  title = div(
    class = "d-flex align-items-center gap-2",
    tags$img(
      src = "images/inspect-sr.png",
      alt = "scrutiny",
      height = "56"
    ),
    "Consistency Tester"
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

  nav_panel(
    "GRIM & GRIMMER",
    div(
      class = "container py-4",
      style = "max-width:950px;",
      card(
        card_header("GRIM & GRIMMER Test"),
        card_body(
          # fmt: skip
          p(
            class = "text-muted mb-3",
            "GRIM checks whether a reported mean is arithmetically possible given",
            "the sample size. GRIMMER extends this to also check the standard deviation (SD).",
            br(), br(),
            "Enter a mean and N to run GRIM. Adding an SD switches to GRIMMER.",
            "Mean-scored multi-item scales require the number of items in",
            tags$em("Items."), "Use ", tags$em("Type"),
            " to test percentages instead of means. If you do, ", tags$em("SD"),
            " must be empty."
          ),
          div(
            class = "combined-grid",
            combined_header,
            tagList(lapply(seq_len(MAX_ROWS), combined_row))
          ),
          uiOutput("combined_empty"),
          uiOutput("combined_vis"),
          div(
            class = "mt-3 d-flex gap-2",
            actionButton(
              "combined_add",
              "+ Add row",
              class = "btn btn-grim-add"
            ),
            downloadButton(
              "download_csv",
              "Download CSV",
              class = "btn btn-grim-dl"
            )
          ),
          uiOutput("combined_summary")
        )
      ),
      p(
        class = "text-muted mt-3 mb-0",
        style = "font-size:.78rem; text-align:center;",
        "App by Lukas Jung and Ian Hussey, University of Bern."
      )
    )
  ),

  nav_panel(
    "Guidance",
    div(
      class = "container py-4",
      style = "max-width:900px;",
      card(
        card_header("Guidance on app usage"),
        card_body(
          p(
            "This app is primarily directed at INSPECT-SR users who are not",
            "experts in GRIM and GRIMMER. It is designed to emphasize ease of",
            "use above maximum functionality. For more advanced use of these",
            "tests, see",
            a(
              "this app",
              href = "https://errors.shinyapps.io/scrutiny/",
              style = "color:#ca225e;",
            ),
            "or the",
            a(
              "scrutiny R package",
              href = "https://lhdjung.github.io/scrutiny/",
              style = "color:#ca225e;",
              .noWS = "after"
            ),
            ". Both allow you to test many values at once, e.g., via",
            "a CSV file. See also",
            a(
              "this website",
              href = "https://trustworthy.scientific.claims/tools/",
              style = "color:#ca225e;"
            ),
            "for more forensic metascience tools.",
            br(),
            br(),
            "With mean-scored scales composed of multiple items, make sure to set",
            tags$em("Items"),
            "to the number of those items. This is crucial for the test outcome.",
            "Also, don't transform any values – enter them just as you read",
            "them in an article, including any trailing zeros. For rounding,",
            "the app assumes numbers were rounded either up from 5 or down",
            "from 5 – both are accepted.",
            br(),
            br(),
            "If",
            tags$em("SD"),
            "is set, these reasons for inconsistencies are given:",
            br(),
            tags$ul(
              tags$li(
                "\"GRIM\": Fails GRIM, i.e., mean and sample size are inconsistent."
              ),
              tags$li(
                "\"GRIMMER 1/2/3\": Fails GRIMMER, i.e., mean, SD, and sample",
                "size are inconsistent. GRIMMER adds 3 separate tests,",
                "so the app will say which one the problem is."
              ),
            ),
            "GRIMMER is GRIM plus 3 additional tests:",
            tags$ol(
              tags$li(
                "The reconstructed sum of squared observations must be a whole number."
              ),
              tags$li("The reconstructed SD must match the reported one."),
              tags$li(
                "The reconstructed sum of squared observations and the",
                "reconstructed sum of integers of which the reported means",
                "are fractions must both be even or both be odd."
              )
            ),
            "Click \"Download CSV\" to get all the results in a tabular file.",
            br(),
            br(),
            tags$strong("When GRIM is uninformative."),
            "GRIM cannot fail when every possible mean is achievable",
            "for the given sample size, i.e., when",
            tags$em("N"),
            "*",
            tags$em("Items"),
            "≥ 10",
            tags$sup("D"),
            "(where",
            tags$em("D"),
            "is the number of decimal places of the reported mean,",
            "plus 2 for percentages). In these cases the app marks the result",
            "with an",
            tags$em("Uninformative GRIM"),
            "label and adds a corresponding entry to the CSV",
            tags$em("notes"),
            "column.",
            "If an SD is provided, the GRIMMER SD-based checks (and TIDES,",
            "where applied) remain informative even when the GRIM portion is not."
          )
        )
      ),
      p(
        class = "text-muted mt-3 mb-0",
        style = "font-size:.78rem; text-align:center;",
        "App by Lukas Jung and Ian Hussey, University of Bern."
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
              style = "color:#ca225e;",
              .noWS = "after"
            ),
            ". INSPECT-SR (",
            a(
              "Wilkinson et al., 2025",
              href = "https://www.medrxiv.org/content/10.1101/2025.09.03.25334905v3",
              style = "color:#ca225e;",
              .noWS = "outside"
            ),
            ") is a framework for assessing the trustworthiness of",
            "randomised controlled trials in systematic reviews. However, it",
            "can be applied to other fields, as well.",
            br(),
            br(),
            "Shiny app made by Lukas Jung and Ian Hussey, University of Bern, using the",
            a(
              "scrutiny",
              href = "https://lhdjung.github.io/scrutiny/",
              style = "color:#ca225e;"
            ),
            "package for error detection in science."
          )
        )
      )
    )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  slots <- reactiveVal(1:3)

  vis_css <- function(s, prefix) {
    rules <- vapply(
      seq_len(MAX_ROWS),
      function(i) {
        display <- if (i %in% s) "grid" else "none"
        sprintf("#%s_slot_%d{display:%s!important}", prefix, i, display)
      },
      character(1)
    )
    tags$style(paste(rules, collapse = ""))
  }

  output$combined_vis <- renderUI(vis_css(slots(), "cb"))

  output$combined_empty <- renderUI({
    if (length(slots()) == 0) {
      p(
        class = "text-muted fst-italic small mt-2 mb-0",
        "No rows. Click '+ Add row' to add one."
      )
    }
  })

  observeEvent(input$combined_add, {
    s <- slots()
    ns <- next_free(s)
    if (!is.null(ns)) slots(c(s, ns))
  })

  # Pre-register outputs and observers for every possible slot
  for (i in seq_len(MAX_ROWS)) {
    local({
      ii <- i

      observeEvent(
        input[[paste0("cb_rm_", ii)]],
        {
          current <- slots()
          if (ii %in% current) {
            updateTextInput(session, paste0("cb_x_", ii), value = "")
            updateTextInput(session, paste0("cb_sd_", ii), value = "")
            updateTextInput(session, paste0("cb_n_", ii), value = "")
            updateNumericInput(session, paste0("cb_items_", ii), value = 1)
            updateSelectInput(
              session,
              paste0("cb_type_", ii),
              selected = "Mean"
            )
            slots(setdiff(current, ii))
          }
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      output[[paste0("cb_badge_", ii)]] <- renderUI({
        x_str <- input[[paste0("cb_x_", ii)]]
        sd_str <- input[[paste0("cb_sd_", ii)]]
        n_str <- input[[paste0("cb_n_", ii)]]
        items <- input[[paste0("cb_items_", ii)]]
        type <- input[[paste0("cb_type_", ii)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NULL)
        }
        err <- validate_combined_row(x_str, sd_str, n_str, items, type)
        if (!is.null(err)) {
          return(error_ui(err))
        }
        sd_given <- !is.null(sd_str) && nzchar(trimws(sd_str))
        uninf <- grim_uninformative(
          x_str,
          n_str,
          items,
          percent = isTRUE(type == "Percentage")
        )
        if (sd_given) {
          res <- safe_grimmer(x_str, sd_str, n_str, items)
          result_ui(res$ok, res$reason, uninformative = uninf)
        } else {
          ok <- safe_grim(
            x_str,
            n_str,
            items,
            percent = isTRUE(type == "Percentage")
          )
          result_ui(
            ok,
            if (isFALSE(ok)) "GRIM inconsistent" else NULL,
            uninformative = uninf
          )
        }
      })
    })
  }

  output$combined_summary <- renderUI({
    s <- slots()
    results <- vapply(
      s,
      function(i) {
        x_str <- input[[paste0("cb_x_", i)]]
        sd_str <- input[[paste0("cb_sd_", i)]]
        n_str <- input[[paste0("cb_n_", i)]]
        items <- input[[paste0("cb_items_", i)]]
        type <- input[[paste0("cb_type_", i)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NA)
        }
        if (
          !is.null(validate_combined_row(x_str, sd_str, n_str, items, type))
        ) {
          return(NA)
        }
        sd_given <- !is.null(sd_str) && nzchar(trimws(sd_str))
        if (sd_given) {
          safe_grimmer(x_str, sd_str, n_str, items)$ok
        } else {
          safe_grim(x_str, n_str, items, percent = isTRUE(type == "Percentage"))
        }
      },
      logical(1)
    )
    summary_bar(results)
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("grim-grimmer-", Sys.time(), ".csv"),
    content = function(file) {
      s <- slots()
      rows <- lapply(s, function(i) {
        x_str <- input[[paste0("cb_x_", i)]]
        sd_str <- input[[paste0("cb_sd_", i)]]
        n_str <- input[[paste0("cb_n_", i)]]
        items <- input[[paste0("cb_items_", i)]]
        type <- input[[paste0("cb_type_", i)]]
        if (is.null(x_str) || !nzchar(trimws(x_str))) {
          return(NULL)
        }
        sd_given <- !is.null(sd_str) && nzchar(trimws(sd_str))
        test <- if (sd_given) "GRIMMER" else "GRIM"
        err <- validate_combined_row(x_str, sd_str, n_str, items, type)
        if (!is.null(err)) {
          return(data.frame(
            type = if (is.null(type)) "Mean" else type,
            mean = trimws(x_str),
            sd = if (sd_given) trimws(sd_str) else "",
            n = if (!is.null(n_str)) trimws(n_str) else "",
            items = if (!is.null(items) && !is.na(items)) items else NA_real_,
            test = test,
            consistent = NA,
            inconsistency = err,
            notes = "",
            stringsAsFactors = FALSE
          ))
        }
        if (sd_given) {
          res <- safe_grimmer(x_str, sd_str, n_str, items)
          ok <- res$ok
          reason <- if (!is.na(ok) && !ok) friendly_reason(res$reason) else ""
        } else {
          ok <- safe_grim(
            x_str,
            n_str,
            items,
            percent = isTRUE(type == "Percentage")
          )
          reason <- ""
        }
        uninf <- grim_uninformative(
          x_str,
          n_str,
          items,
          percent = isTRUE(type == "Percentage")
        )
        notes <- if (uninf) {
          paste(
            "Uninformative GRIM: every possible mean is achievable for this N",
            "and item count. GRIMMER and TIDES checks remain valid."
          )
        } else {
          ""
        }
        data.frame(
          type = if (is.null(type)) "Mean" else type,
          mean = trimws(x_str),
          sd = if (sd_given) trimws(sd_str) else "",
          n = if (!is.null(n_str)) trimws(n_str) else "",
          items = if (!is.null(items) && !is.na(items)) items else NA_real_,
          test = test,
          consistent = ok,
          inconsistency = reason,
          notes = notes,
          stringsAsFactors = FALSE
        )
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0) {
        df <- data.frame(
          type = character(),
          mean = character(),
          sd = character(),
          n = character(),
          items = numeric(),
          test = character(),
          consistent = logical(),
          inconsistency = character(),
          notes = character(),
          stringsAsFactors = FALSE
        )
      } else {
        df <- do.call(rbind, rows)
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

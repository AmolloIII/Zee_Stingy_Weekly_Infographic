
library(googlesheets4)
library(dplyr)
library(lubridate)
library(htmltools)

Sys.setenv(TZ = "Africa/Nairobi")
REPORT_ROUND <- 4L
EXPECTED_PAYOUT <- 5000
# Keep verified round starts explicit; set the start before selecting a new round.
ROUND_STARTS <- c(`1` = "2026-02-08", `4` = "2026-08-30")
SEND_NOTIFICATIONS <- TRUE
SEND_EMAIL <- TRUE
SEND_TELEGRAM_TEXT <- TRUE
SEND_TELEGRAM_IMAGE <- TRUE
EXPORT_PNG <- TRUE
OUTPUT_DIR <- "weekly_motivation_output"
# EMAIL_TO <- "amollozeethird@gmail.com"  # Current test recipient from your script.
# Required env: SHEET_ID (prefer the URL copied from your browser).
# Authentication: GSHEET_CREDENTIALS (JSON contents) OR GSHEET_CREDENTIALS_PATH.
# Example: Sys.setenv(GSHEET_CREDENTIALS_PATH =
#   "D:/Rstuff/ZeeStingyAlerts/myrstuff-fe49a7146f1b.json")
# Email: MY_GMAIL_ACCOUNT, SMTP_PASSWORD; Telegram: BOT_TOKEN, CHAT_ID.
# Optional: CHROMOTE_CHROME = absolute path to Chrome for PNG rendering.

add_ordinal <- function(day) {
  suffix <- ifelse(day %% 100 %in% 11:13, "th",
    c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")[day %% 10 + 1])
  paste0(day, suffix)
}
format_payout_date <- function(date) {
  paste0(wday(date, label = TRUE, abbr = FALSE), ", ", add_ordinal(day(date)),
         " ", month(date, label = TRUE, abbr = FALSE), " ", year(date))
}

# =============================================================================
# READ PAYOUT ORDER AND RECONCILE RECORDED PAYMENTS
# =============================================================================
member_key <- function(x) tolower(gsub("\\s+", " ", trimws(as.character(x))))
blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
amount_value <- function(x) {
  raw <- trimws(as.character(x))
  out <- suppressWarnings(as.numeric(gsub(",", "", raw, fixed = TRUE)))
  out[blank(x)] <- 0
  if (any(!is.finite(out) | out < 0)) stop("Invalid/negative amount in Payouts.")
  out
}
read_tab <- function(name) {
  read_sheet(Sys.getenv("SHEET_ID"), sheet = name, col_types = "c",
             .name_repair = "minimal")
}
authenticate_sheets <- function() {
  if (!nzchar(Sys.getenv("SHEET_ID"))) stop("Set SHEET_ID.")
  path <- Sys.getenv("GSHEET_CREDENTIALS_PATH")
  if (nzchar(path)) {
    if (!file.exists(path)) stop("GSHEET_CREDENTIALS_PATH does not exist.")
    gs4_auth(path = path)
    return(invisible(TRUE))
  }
  token <- Sys.getenv("GSHEET_CREDENTIALS")
  if (!nzchar(token)) stop("Set GSHEET_CREDENTIALS.")
  f <- tempfile(fileext = ".json")
  on.exit(unlink(f), add = TRUE)
  writeLines(token, f)
  gs4_auth(path = f)
}

build_schedule <- function(order, payouts, today = Sys.Date()) {
  if (!all(c("Member") %in% names(order)) || !"Member" %in% names(payouts))
    stop("Both sheets must have a Member column.")
  order <- order[!blank(order$Member), , drop = FALSE]
  payouts <- payouts[!blank(payouts$Member), , drop = FALSE]
  order$key <- member_key(order$Member)
  payouts$key <- member_key(payouts$Member)
  if (anyDuplicated(order$key) || anyDuplicated(payouts$key))
    stop("Duplicate member names; use one row per member in each sheet.")
  if (!setequal(order$key, payouts$key))
    stop("Member names do not match between Payout Order and Payouts: ",
         paste(union(setdiff(order$key, payouts$key),
                     setdiff(payouts$key, order$key)), collapse = ", "))
  n <- nrow(order)
  if (!n) stop("No members found.")
  round_cols <- grep("^Round [0-9]+$", names(order), value = TRUE)
  round_cols <- round_cols[vapply(order[round_cols], function(x) any(!blank(x)), logical(1))]
  if (!length(round_cols)) stop("No populated rounds in Payout Order.")
  round_ids <- as.integer(sub("Round ", "", round_cols, fixed = TRUE))
  if (anyDuplicated(round_ids) || !identical(sort(round_ids), seq_len(max(round_ids))))
    stop("Round columns must be consecutive from Round 1.")
  selected <- if (is.na(REPORT_ROUND)) max(round_ids) else REPORT_ROUND
  if (!selected %in% round_ids) stop("Selected round has no payout order.")
  start <- as.Date(unname(ROUND_STARTS[as.character(selected)]))
  if (length(start) != 1L || is.na(start)) stop("Set verified ROUND_STARTS for selected round.")
  round_cols <- round_cols[round_ids == selected]
  schedule <- bind_rows(lapply(round_cols, function(col) {
    pos <- suppressWarnings(as.numeric(order[[col]]))
    if (anyNA(pos) || any(pos != floor(pos)) || !identical(sort(as.integer(pos)), seq_len(n)))
      stop(col, " must assign each position 1..", n, " exactly once (no blanks).")
    r <- as.integer(sub("Round ", "", col, fixed = TRUE))
    data.frame(key = order$key, full_name = order$Member, Round = r,
               Position = as.integer(pos),
               PayoutRaw = start + 7 * (pos - 1L))
  })) %>% arrange(PayoutRaw)
  # Date-column headings are day/month/year, as in the screenshot.
  # Total Payouts is a lifetime summary and is NEVER allocated to a round.
  date_cols <- setdiff(names(payouts), c("Member", "Total Payouts", "key"))
  date_cols <- date_cols[vapply(payouts[date_cols], function(x) any(!blank(x)), logical(1)) |
                           grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", date_cols)]
  dates <- as.Date(date_cols, format = "%d/%m/%Y")
  if (anyNA(dates) || anyDuplicated(dates))
    stop("Payouts contains invalid/duplicate date headers; use d/m/yyyy.")
  ledger <- bind_rows(lapply(seq_along(date_cols), function(j) {
    data.frame(key = payouts$key, ActualDate = dates[j],
               Amount = amount_value(payouts[[date_cols[j]]]))
  }))
  if (!nrow(ledger)) ledger <- data.frame(key=character(), ActualDate=as.Date(character()), Amount=numeric())
  ledger <- ledger %>% filter(Amount > 0)
  if (any(ledger$ActualDate > today))
    warning("Future-dated payouts excluded from amounts received as of today.")
  # Only receipts within the selected round window are considered.
  # Date-only columns cannot identify cross-round arrears unambiguously.
  window_end <- start + 7 * n
  next_starts <- as.Date(unname(ROUND_STARTS))
  next_starts <- next_starts[!is.na(next_starts) & next_starts > start]
  if (length(next_starts)) window_end <- min(window_end, min(next_starts))
  lifetime_ledger <- ledger
  ledger <- ledger %>% filter(ActualDate >= start, ActualDate < window_end)
  ledger$Round <- rep(selected, nrow(ledger))
  paid <- ledger %>% filter(ActualDate <= today) %>%
    group_by(key, Round) %>%
    summarise(Received = sum(Amount), LastPayment = max(ActualDate), .groups = "drop")
  schedule <- left_join(schedule, paid, by = c("key", "Round")) %>%
    mutate(Received = coalesce(Received, 0), Expected = EXPECTED_PAYOUT,
           Balance = pmax(Expected - Received, 0),
           Overpayment = pmax(Received - Expected, 0),
           DaysUntil = as.integer(PayoutRaw - today),
           Status = case_when(
             Received >= Expected ~ "Paid",
             Received > 0 & DaysUntil < 0 ~ "Partial / overdue",
             Received > 0 ~ "Partial",
             DaysUntil < 0 ~ "Overdue",
             PayoutRaw <= floor_date(today, "week", week_start = 1) + days(6) ~ "Current",
             TRUE ~ "Upcoming"),
           Timeline = case_when(
             Status == "Paid" ~ paste0("Last payment: ", format(LastPayment, "%d/%m/%Y")),
             DaysUntil < 0 ~ paste0(abs(DaysUntil), " days overdue"),
             DaysUntil == 0 ~ "Due today",
             DaysUntil == 1 ~ "Due tomorrow",
             TRUE ~ paste0("Due in ", DaysUntil, " days")),
           PayoutDate = format_payout_date(PayoutRaw))
  # Compare lifetime total with all dated entries (not just the selected round).
  if ("Total Payouts" %in% names(payouts)) {
    totals <- lifetime_ledger %>% group_by(key) %>% summarise(total = sum(Amount), .groups = "drop")
    check <- left_join(payouts, totals, by = "key")
    if (any(abs(amount_value(check$`Total Payouts`) - coalesce(check$total, 0)) > .01))
      warning("Total Payouts differs from dated entries. Report uses dated entries only.")
  }
  schedule %>% filter(Round == selected) %>% arrange(Position) %>%
    mutate(RowNumber = Position)
}
load_and_process_data <- function() {
  authenticate_sheets()
  available <- sheet_names(Sys.getenv("SHEET_ID"))
  missing <- setdiff(c("Payout Order", "Payouts"), available)
  if (length(missing)) stop("Missing worksheet(s): ", paste(missing, collapse = ", "))
  build_schedule(read_tab("Payout Order"), read_tab("Payouts"))
}

# =============================================================================
# INFOGRAPHIC: FOUR VISIBLE COLUMNS, SHARED BY HTML, EMAIL AND PNG
# =============================================================================
esc <- function(x) as.character(htmltools::htmlEscape(as.character(x)))
status_palette <- function(status) {
  if (status == "Paid") return(c(bg = "#e5f6ee", fg = "#157347"))
  if (grepl("overdue", status, ignore.case = TRUE))
    return(c(bg = "#ffebea", fg = "#b42318"))
  if (status == "Partial") return(c(bg = "#fff2d8", fg = "#946200"))
  if (status == "Current") return(c(bg = "#e7efff", fg = "#2459bd"))
  c(bg = "#eeebff", fg = "#6355ba")
}

create_infographic_body <- function(df) {
  n <- nrow(df)
  if (!n) stop("Cannot render an empty schedule.")
  paid <- sum(df$Status == "Paid")
  attention <- sum(grepl("overdue", df$Status, ignore.case = TRUE))
  due <- sum(df$Status %in% c("Current", "Partial"))
  upcoming <- sum(df$Status == "Upcoming")
  pct <- round(100 * paid / n)
  round <- unique(df$Round)
  generated <- format(Sys.time(), "%d %B %Y, %I:%M %p", tz = "Africa/Nairobi")
  date_span <- paste(format(min(df$PayoutRaw), "%d %b"),
                     format(max(df$PayoutRaw), "%d %b %Y"), sep = " &ndash; ")
  card <- function(value, label, color, bg) paste0(
    "<td width='25%' style='padding:5px;vertical-align:top;'>",
    "<table role='presentation' width='100%' cellpadding='0' cellspacing='0' style='background:", bg,
    ";border:1px solid #e7e8f0;border-radius:12px;'><tr><td style='padding:18px 6px;text-align:center;'>",
    "<div style='font-size:30px;line-height:36px;font-weight:800;color:", color, ";'>", value, "</div>",
    "<div style='font-size:12px;color:#667085;margin-top:5px;'>", label, "</div>",
    "</td></tr></table></td>")
  cards <- paste0("<table role='presentation' width='100%' cellpadding='0' cellspacing='0'><tr>",
    card(paid, "Paid", "#157347", "#f0faf4"),
    card(due, "Current / partial", "#2459bd", "#f3f6ff"),
    card(attention, "Overdue", "#b42318", "#fff5f3"),
    card(upcoming, "Upcoming", "#6355ba", "#f7f5ff"), "</tr></table>")
  # Draw a table-based progress bar for broad email compatibility.
  bar <- if (pct == 0) {
    "<td width='100%' height='9' style='background:#e9e8f5;border-radius:8px;'></td>"
  } else if (pct == 100) {
    "<td width='100%' height='9' style='background:#29a575;border-radius:8px;'></td>"
  } else paste0("<td width='", pct,
      "%' height='9' style='background:#29a575;border-radius:8px;'></td><td width='",
      100 - pct, "%' style='background:#e9e8f5;border-radius:8px;'></td>")
  unpaid <- df[df$Status != "Paid", , drop = FALSE]
  spotlight <- if (!nrow(unpaid)) {
    "Every member has received their full token for this round."
  } else {
    pending <- unpaid[order(unpaid$Position), , drop = FALSE][1, ]
    paste0("<strong>", esc(pending$full_name), "</strong> &nbsp;&middot;&nbsp; ",
           esc(pending$Status), " &nbsp;&middot;&nbsp; ", esc(pending$Timeline))
  }
  rows <- vapply(seq_len(n), function(i) {
    pal <- status_palette(df$Status[i])
    bg <- if (i %% 2L == 0L) "#f8f9fd" else "#ffffff"
    paste0("<tr style='background:", bg, ";'>",
      "<td style='padding:13px 10px;border-bottom:1px solid #edf0f5;text-align:center;'>",
      "<span style='display:inline-block;color:#6558c5;font-weight:bold;'>",
      sprintf("%02d", df$Position[i]), "</span></td>",
      "<td style='padding:13px 10px;border-bottom:1px solid #edf0f5;font-weight:600;color:#25304c;'>",
      esc(df$full_name[i]), "</td>",
      "<td style='padding:13px 8px;border-bottom:1px solid #edf0f5;'>",
      "<span style='display:inline-block;padding:5px 9px;border-radius:20px;font-size:11px;font-weight:700;background:",
      pal['bg'], ";color:", pal['fg'], ";'>", esc(df$Status[i]), "</span></td>",
      "<td style='padding:13px 10px;border-bottom:1px solid #edf0f5;font-size:12px;color:#667085;'>",
      esc(df$Timeline[i]), "</td></tr>")
  }, character(1))
  paste0(
    "<table role='presentation' width='100%' cellpadding='0' cellspacing='0' style='background:#f0f2f8;'>",
    "<tr><td style='padding:22px 10px;'><table role='presentation' width='100%' cellpadding='0' cellspacing='0' ",
    "style='max-width:820px;margin:auto;background:#ffffff;border:1px solid #e4e7f0;border-radius:18px;font-family:Arial,Helvetica,sans-serif;'>",
    "<tr><td style='padding:28px 26px;background:#5e57ca;color:#ffffff;border-radius:18px 18px 0 0;'>",
    "<div style='font-size:11px;letter-spacing:2px;font-weight:bold;color:#e0ddff;'>THE STINGY11 &nbsp;/&nbsp; GROWING TOGETHER</div>",
    "<h1 style='font-size:26px;line-height:1.25;margin:12px 0 8px;color:#ffffff;'>Weekly Motivation<br>Token Schedule</h1>",
    "<div style='font-size:13px;color:#eeecff;'>Round ", round, " &nbsp;&bull;&nbsp; ", n,
    " members &nbsp;&bull;&nbsp; ", date_span, "</div></td></tr>",
    "<tr><td style='padding:18px 19px 6px;'>", cards, "</td></tr>",
    "<tr><td style='padding:10px 26px 22px;'>",
    "<table role='presentation' width='100%'><tr><td style='font-size:12px;font-weight:bold;color:#344054;'>Round completion</td>",
    "<td align='right' style='font-size:12px;color:#667085;'>", paid, " of ", n, " paid &nbsp; <strong>", pct,
    "%</strong></td></tr></table><table role='presentation' width='100%' cellpadding='0' cellspacing='0' style='margin-top:7px;'><tr>",
    bar, "</tr></table></td></tr>",
    "<tr><td style='padding:0 26px 22px;'><table role='presentation' width='100%' style='background:#f2f0ff;border-radius:10px;'>",
    "<tr><td style='padding:14px 16px;font-size:12px;line-height:1.6;color:#51479f;'>",
    "<div style='font-size:10px;letter-spacing:1px;font-weight:bold;margin-bottom:5px;'>",
    if (nrow(unpaid)) "FIRST UNPAID POSITION" else "ROUND COMPLETE", "</div>", spotlight, "</td></tr></table></td></tr>",
    "<tr><td style='padding:0 26px 8px;font-size:15px;color:#25304c;font-weight:bold;'>Round ", round, " at a glance</td></tr>",
    "<tr><td style='padding:4px 16px 20px;'><table width='100%' cellpadding='0' cellspacing='0' ",
    "style='border-collapse:collapse;font-size:13px;table-layout:fixed;'>",
    "<thead><tr style='background:#eef0f7;color:#667085;font-size:10px;letter-spacing:.6px;'>",
    "<th scope='col' width='8%' style='padding:12px 4px;text-align:center;'>#</th>",
    "<th scope='col' width='30%' style='padding:12px 10px;text-align:left;'>MEMBER</th>",
    "<th scope='col' width='25%' style='padding:12px 8px;text-align:left;'>STATUS</th>",
    "<th scope='col' width='37%' style='padding:12px 10px;text-align:left;'>TIMELINE</th>",
    "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></td></tr>",
    "<tr><td style='padding:0 26px 20px;'><div style='padding:14px 16px;background:#eef7ff;border-left:3px solid #4b93dc;font-size:11px;line-height:1.7;color:#345b7d;'>",
    "<strong>A quick reminder</strong><br>Tokens are scheduled weekly on Sunday by 8:00 PM. ",
    "Paid status is based on recorded payouts. Partial payments remain open until the full token is received.",
    "</div></td></tr><tr><td style='padding:16px 26px 22px;border-top:1px solid #edf0f5;text-align:center;color:#98a2b3;font-size:10px;line-height:1.7;'>",
    "<strong style='color:#667085;'>Small contributions. Shared progress.</strong><br>",
    "Updated ", esc(generated), " EAT &nbsp;&middot;&nbsp; The Stingy11",
    "</td></tr></table></td></tr></table>")
}

create_html_document <- function(df) paste0(
  "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width,initial-scale=1'>",
  "<title>The Stingy11 | Weekly Motivation</title>",
  "<style>body{margin:0;background:#f0f2f8}*{box-sizing:border-box}",
  "td,th{overflow-wrap:break-word}@media print{body{background:white}}</style>",
  "</head><body>", create_infographic_body(df), "</body></html>")

export_infographic <- function(df) {
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  html <- file.path(OUTPUT_DIR, "weekly_motivation.html")
  csv <- file.path(OUTPUT_DIR, "weekly_motivation.csv")
  png <- file.path(OUTPUT_DIR, "weekly_motivation.png")
  writeLines(enc2utf8(create_html_document(df)), html, useBytes = TRUE)
  # Export only the four requested display columns. Amounts remain internal.
  display <- df %>% transmute(Position, Member = full_name, Status, Timeline)
  write.csv(display, csv, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  # Never send a stale image if rendering this run fails.
  if (file.exists(png)) unlink(png)
  image_ready <- FALSE
  if (EXPORT_PNG) {
    image_ready <- tryCatch({
      if (!requireNamespace("webshot2", quietly = TRUE))
        stop("Install webshot2 and Chrome/Chromium for PNG export.")
      webshot2::webshot(url = normalizePath(html, winslash = "/", mustWork = TRUE),
        file = png, vwidth = 900, vheight = 1000, selector = "body", zoom = 2)
      file.exists(png)
    }, error = function(e) {
      message("PNG export skipped: ", conditionMessage(e))
      FALSE
    })
  }
  list(html = normalizePath(html, winslash = "/"), csv = csv,
       png = if (image_ready) png else NULL)
}

# =============================================================================
# OPTIONAL DELIVERY (DISABLED UNTIL SEND_NOTIFICATIONS IS TRUE)
# =============================================================================
send_email_notification <- function(df) {
  tryCatch({
    if (!requireNamespace("blastula", quietly = TRUE)) {
      stop("Install blastula.")
    }
    
    sender <- trimws(Sys.getenv("MY_GMAIL_ACCOUNT"))
    
    if (!nzchar(sender) || !nzchar(Sys.getenv("SMTP_PASSWORD"))) {
      stop("Set MY_GMAIL_ACCOUNT and SMTP_PASSWORD.")
    }
    
    # Read all members' email addresses
    contacts <- read_tab("Member Contact Info")
    
    email_col <- grep(
      "email",
      names(contacts),
      ignore.case = TRUE,
      value = TRUE
    )
    
    if (length(email_col) != 1L) {
      stop("Member Contact Info must contain exactly one email column.")
    }
    
    recipients <- trimws(as.character(contacts[[email_col]]))
    recipients <- unique(
      tolower(recipients[!is.na(recipients) & nzchar(recipients)])
    )
    
    if (!length(recipients)) {
      stop("No member email addresses found.")
    }
    
    if (any(!grepl("^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$", recipients))) {
      stop("Invalid email address found in Member Contact Info. Check the email column.")
    }
    
    creds <- blastula::creds_envvar(
      user = sender,
      pass_envvar = "SMTP_PASSWORD",
      provider = "gmail"
    )
    
    body <- blastula::compose_email(
      body = htmltools::HTML(create_infographic_body(df))
    )
    
    # BCC keeps members' addresses private
    blastula::smtp_send(
      email = body,
      from = sender,
      to = "benamolo@gmail.com",
      bcc = recipients,
      subject = paste0(
        "The Stingy11 | Round ", unique(df$Round),
        " | ", Sys.Date()
      ),
      credentials = creds
    )
    
    message("Email sent to ", length(recipients), " members.")
    TRUE
    
  }, error = function(e) {
    message("Email failed: ", conditionMessage(e))
    FALSE
  })
}

telegram_request <- function(method, body, encode) {
  if (!requireNamespace("httr", quietly = TRUE)) stop("Install httr.")
  token <- Sys.getenv("BOT_TOKEN")
  chat <- Sys.getenv("CHAT_ID")
  if (!nzchar(token) || !nzchar(chat)) stop("Set BOT_TOKEN and CHAT_ID.")
  body$chat_id <- chat
  # Suppress transport details, which can contain the bot token in the URL.
  response <- tryCatch(httr::POST(
    paste0("https://api.telegram.org/bot", token, "/", method),
    body = body, encode = encode, httr::timeout(60)),
    error = function(e) stop("Telegram connection failed; check network and configuration.", call. = FALSE))
  if (httr::status_code(response) != 200L) stop("Telegram HTTP ", httr::status_code(response))
  result <- httr::content(response, as = "parsed", encoding = "UTF-8")
  if (!isTRUE(result$ok)) stop("Telegram did not confirm delivery.")
  TRUE
}

send_telegram_notification <- function(df) {
  tryCatch({
    lines <- paste0(df$Position, ". ", df$full_name, " | ", df$Status, "\n", df$Timeline)
    title <- paste0("THE STINGY11 - ROUND ", unique(df$Round), "\nWeekly motivation tokens\n\n")
    # Split at member boundaries to stay within Telegram's message limit.
    chunks <- character()
    current <- title
    for (line in lines) {
      if (nchar(current, type = "chars") + nchar(line, type = "chars") + 2 > 3500) {
        chunks <- c(chunks, current)
        current <- title
      }
      current <- paste0(current, line, "\n\n")
    }
    chunks <- c(chunks, current)
    for (msg in chunks) telegram_request("sendMessage", list(text = msg), "form")
    message("Telegram text sent.")
    TRUE
  }, error = function(e) {
    message("Telegram text failed: ", conditionMessage(e))
    FALSE
  })
}

send_telegram_infographic <- function(path, round) {
  tryCatch({
    if (is.null(path) || !file.exists(path)) stop("No new infographic PNG is available.")
    if (!requireNamespace("httr", quietly = TRUE)) stop("Install httr.")
    telegram_request("sendPhoto", list(
      photo = httr::upload_file(path, type = "image/png"),
      caption = paste0("The Stingy11 | Round ", round, " | Weekly Motivation")), "multipart")
    message("Telegram infographic sent.")
    TRUE
  }, error = function(e) {
    message("Telegram image failed: ", conditionMessage(e))
    FALSE
  })
}

main <- function() {
  tryCatch({
    message("Reading payout order and recorded payouts...")
    df <- load_and_process_data()
    files <- export_infographic(df)
    message("HTML infographic: ", files$html)
    message("CSV report: ", files$csv)
    print(df %>% transmute(Position, Member = full_name, Status, Timeline))
    if (!SEND_NOTIFICATIONS) {
      message("Preview ready. Set SEND_NOTIFICATIONS <- TRUE to enable delivery.")
      if (interactive()) utils::browseURL(files$html)
      return(invisible(list(data = df, files = files)))
    }
    outcomes <- logical()
    if (SEND_EMAIL) outcomes["email"] <- send_email_notification(df)
    if (SEND_TELEGRAM_TEXT) outcomes["telegram_text"] <- send_telegram_notification(df)
    if (SEND_TELEGRAM_IMAGE) outcomes["telegram_image"] <-
      send_telegram_infographic(files$png, unique(df$Round))
    if (length(outcomes) && !all(outcomes)) message("Some notifications failed; see messages above.")
    invisible(list(data = df, files = files, delivery = outcomes))
  }, error = function(e) {
    message("Script stopped: ", conditionMessage(e))
    invisible(FALSE)
  })
}

if (sys.nframe() == 0L) main()

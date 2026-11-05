# Reading and writing data files ####

# File extensions that CycloDesk reads as data
cd_data_ext <- function() {
  c("txt", "csv", "dat", "tsv", "prn", "asc", "xlsx", "xls", "xlsm", "las")
}

cd_is_data <- function(f) {
  tolower(tools::file_ext(f)) %in% cd_data_ext()
}

#' Read a data series file
#'
#' Reads a data file with one or more numeric columns: the first column is depth or time, the following columns are
#' proxy values. Supported formats:
#' \itemize{
#'   \item text files (\code{.txt}, \code{.csv}, \code{.dat}, \code{.tsv}, \code{.prn}, \code{.asc}): the separator
#'   (comma, semicolon, tab or white space), a header line and a decimal comma (with semicolon separators) are
#'   detected automatically; lines starting with \code{#} or \code{\%} are skipped;
#'   \item Excel workbooks (\code{.xlsx}, \code{.xls}, \code{.xlsm}, needs the package readxl): one sheet, with or
#'   without a header row;
#'   \item LAS well-log files (Log ASCII Standard 1.2 and 2.0, wrapped or not): the depth curve and all log curves,
#'   with the NULL value of the file set to \code{NA}.
#' }
#'
#' @param file Path of the file (character string).
#' @param col Column of proxy values to return next to the first column (integer, 2 or higher), or \code{NULL} to
#'   return all numeric columns. \code{Default=NULL}.
#' @param sheet Sheet of an Excel workbook, by name or number; \code{NULL} reads the first sheet. Ignored for other
#'   files. \code{Default=NULL}.
#' @return A data frame with numeric columns. The column names come from the header (for LAS files: mnemonic and
#'   unit), or are \code{x}, \code{y}, \code{y2}, ... when the file has none.
#' @examples
#' f <- tempfile(fileext = ".txt")
#' write_series(data.frame(depth = 1:10, value = rnorm(10)), f)
#' head(read_series(f))
#' @export
read_series <- function(file, col = NULL, sheet = NULL) {
  if (!file.exists(file)) {
    stop("file not found: ", file, call. = FALSE)
  }
  ext <- tolower(tools::file_ext(file))
  if (ext %in% c("xlsx", "xls", "xlsm")) {
    d <- cd_read_excel(file, sheet)
  } else if (ext == "las") {
    d <- cd_read_las(file)
  } else {
    d <- cd_read_text(file)
  }
  keep <- logical(ncol(d))
  for (j in seq_len(ncol(d))) {
    v <- suppressWarnings(as.numeric(d[[j]]))
    keep[j] <- sum(!is.na(v)) > 0
    d[[j]] <- v
  }
  d <- d[, keep, drop = FALSE]
  if (ncol(d) < 1) {
    stop("no numeric columns in ", basename(file), call. = FALSE)
  }
  if (is.null(names(d)) || any(!nzchar(names(d)))) {
    nm <- c("x", "y", if (ncol(d) > 2) { paste0("y", 2:(ncol(d) - 1)) } else { character() })
    names(d) <- nm[seq_len(ncol(d))]
  }
  d <- d[!is.na(d[[1]]), , drop = FALSE]
  if (!is.null(col)) {
    col <- as.integer(col)
    if (col > ncol(d) || col < 2) {
      col <- min(2L, ncol(d))
    }
    d <- d[, c(1L, col), drop = FALSE]
    d <- d[!is.na(d[[2]]), , drop = FALSE]
  }
  rownames(d) <- NULL
  d
}

# Text files: separator, header and decimal comma detected
cd_read_text <- function(file) {
  lines <- readLines(file, warn = FALSE, n = 50)
  lines <- lines[nzchar(trimws(lines)) & !grepl("^\\s*[#%]", lines)]
  if (!length(lines)) {
    stop("the file is empty: ", basename(file), call. = FALSE)
  }
  sep <- cd_guess_sep(lines)
  dec <- "."
  if (sep %in% c(";", "\t", "") && length(lines) > 1 && grepl("[0-9],[0-9]", lines[2]) && !grepl("[0-9]\\.[0-9]", lines[2])) {
    dec <- ","
  }
  first <- cd_split(lines[1], sep)
  header <- any(is.na(suppressWarnings(as.numeric(sub(",", ".", first, fixed = TRUE)))))
  if (sep == "") {
    d <- utils::read.table(file, header = header, comment.char = "#", strip.white = TRUE, check.names = FALSE,
        blank.lines.skip = TRUE, stringsAsFactors = FALSE, fill = TRUE, dec = dec)
  } else {
    d <- utils::read.table(file, header = header, sep = sep, comment.char = "#", strip.white = TRUE,
        check.names = FALSE, blank.lines.skip = TRUE, stringsAsFactors = FALSE, fill = TRUE, dec = dec, quote = "\"")
  }
  if (!header) {
    names(d) <- rep("", ncol(d))
  }
  d
}

# Excel workbooks (readxl)
cd_read_excel <- function(file, sheet = NULL) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("reading Excel files needs the package readxl: install.packages(\"readxl\")", call. = FALSE)
  }
  if (is.null(sheet) || (is.character(sheet) && !nzchar(sheet))) {
    sheet <- 1
  }
  raw <- suppressMessages(as.data.frame(readxl::read_excel(file, sheet = sheet, col_names = FALSE,
      .name_repair = "minimal"), stringsAsFactors = FALSE))
  raw <- raw[rowSums(!is.na(raw)) > 0, colSums(!is.na(raw)) > 0, drop = FALSE]
  if (!nrow(raw)) {
    stop("the sheet is empty: ", basename(file), call. = FALSE)
  }
  first <- as.character(unlist(raw[1, ], use.names = FALSE))
  header <- any(is.na(suppressWarnings(as.numeric(first))) & !is.na(first))
  if (header) {
    nm <- first
    nm[is.na(nm)] <- paste0("col", which(is.na(nm)))
    raw <- raw[-1, , drop = FALSE]
    names(raw) <- nm
  } else {
    names(raw) <- rep("", ncol(raw))
  }
  raw
}

cd_excel_sheets <- function(file) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    return(character())
  }
  out <- try(readxl::excel_sheets(file), silent = TRUE)
  if (inherits(out, "try-error")) {
    return(character())
  }
  out
}

# LAS well-log files (Log ASCII Standard 1.2 / 2.0)
cd_read_las <- function(file) {
  L <- sub("\r$", "", readLines(file, warn = FALSE))
  t <- trimws(L)
  is_sec <- startsWith(t, "~")
  if (!any(is_sec)) {
    stop("no LAS sections (~V, ~C, ~A) in ", basename(file), call. = FALSE)
  }
  sec_at <- which(is_sec)
  sec_name <- toupper(substr(t[sec_at], 2, 2))
  sec_of <- rep("", length(t))
  for (k in seq_along(sec_at)) {
    to <- if (k < length(sec_at)) { sec_at[k + 1] - 1 } else { length(t) }
    if (to > sec_at[k]) {
      sec_of[(sec_at[k] + 1):to] <- sec_name[k]
    }
  }
  body <- nzchar(t) & !startsWith(t, "#") & !is_sec
  pat <- "^([^.]+)\\.(\\S*)\\s*(.*?):(.*)$"
  null <- NA_real_
  wl <- t[body & sec_of == "W"]
  nl <- wl[grepl("^NULL\\s*\\.", toupper(wl))]
  if (length(nl)) {
    null <- suppressWarnings(as.numeric(trimws(sub(pat, "\\3", nl[1], perl = TRUE))))
  }
  cl <- t[body & sec_of == "C"]
  if (!length(cl)) {
    stop("no curves (~C section) in ", basename(file), call. = FALSE)
  }
  mn <- trimws(sub(pat, "\\1", cl, perl = TRUE))
  un <- trimws(sub(pat, "\\2", cl, perl = TRUE))
  nm <- mn
  nm[nzchar(un)] <- paste0(mn[nzchar(un)], " (", un[nzchar(un)], ")")
  al <- t[body & sec_of == "A"]
  if (!length(al)) {
    stop("no data (~A section) in ", basename(file), call. = FALSE)
  }
  v <- suppressWarnings(as.numeric(strsplit(paste(gsub(",", " ", al), collapse = " "), "[[:space:]]+")[[1]]))
  nc <- length(nm)
  n <- length(v) %/% nc
  if (n < 1) {
    stop("could not read the data of ", basename(file), call. = FALSE)
  }
  m <- matrix(v[seq_len(n * nc)], ncol = nc, byrow = TRUE)
  if (!is.na(null)) {
    m[!is.na(m) & abs(m - null) < 1e-6 * max(1, abs(null))] <- NA
  }
  d <- as.data.frame(m)
  names(d) <- nm
  d
}

#' Write a data series file
#'
#' Writes a data frame or matrix as a tab-separated text file. Two-column series are written without a header so
#' that Acycle (MATLAB) can read them; tables with more columns get a header line.
#'
#' @param x Data frame or matrix with numeric columns.
#' @param file Path of the file to write (character string).
#' @param header Write the column names as the first line (logical or \code{NULL}; \code{NULL} writes a header only
#'   for more than two columns). \code{Default=NULL}.
#' @param digits Significant digits written (integer). \code{Default=10}.
#' @return The file path, invisibly.
#' @export
write_series <- function(x, file, header = NULL, digits = 10) {
  x <- as.data.frame(x)
  if (is.null(header)) {
    header <- ncol(x) > 2
  }
  out <- x
  for (j in seq_len(ncol(out))) {
    if (is.numeric(out[[j]])) {
      out[[j]] <- signif(out[[j]], digits)
    }
  }
  utils::write.table(out, file, sep = "\t", row.names = FALSE, col.names = isTRUE(header), quote = FALSE)
  invisible(file)
}

cd_guess_sep <- function(lines) {
  l <- lines[min(2L, length(lines))]
  if (grepl("\t", l)) {
    return("\t")
  }
  if (grepl(";", l)) {
    return(";")
  }
  if (grepl(",", l)) {
    return(",")
  }
  ""
}

cd_split <- function(line, sep) {
  if (sep == "") {
    p <- strsplit(trimws(line), "[[:space:]]+")[[1]]
  } else {
    p <- strsplit(line, sep, fixed = TRUE)[[1]]
  }
  gsub("^\"|\"$", "", trimws(p))
}

# Prepare a two-column series for analysis: numeric, no NA, sorted, duplicates averaged
cd_clean <- function(d) {
  d <- as.data.frame(d)[, 1:2]
  names(d) <- c("x", "y")
  d <- d[stats::complete.cases(d), , drop = FALSE]
  d <- d[order(d$x), , drop = FALSE]
  if (anyDuplicated(d$x)) {
    u <- unique(d$x)
    g <- match(d$x, u)
    s <- rowsum(d$y, g)
    n <- rowsum(rep(1, nrow(d)), g)
    y <- as.numeric(s / n)
    d <- data.frame(x = u, y = y)
  }
  rownames(d) <- NULL
  d
}

# Regular sampling check and median step
cd_dt <- function(x) {
  dx <- diff(sort(x))
  dx <- dx[dx > 0]
  if (!length(dx)) {
    return(NA_real_)
  }
  stats::median(dx)
}

cd_is_even <- function(x, tol = 1e-6) {
  dx <- diff(x)
  if (length(dx) < 2) {
    return(TRUE)
  }
  (max(dx) - min(dx)) <= tol * max(abs(dx), 1e-12) + 1e-9 * stats::median(dx)
}

# Make the series evenly spaced (needed by most spectral tools) and say so in the log
cd_even <- function(d, dt = NULL) {
  d <- cd_clean(d)
  if (cd_is_even(d$x) && is.null(dt)) {
    return(d)
  }
  if (is.null(dt)) {
    dt <- cd_dt(d$x)
  }
  xo <- seq(d$x[1], d$x[nrow(d)], by = dt)
  data.frame(x = xo, y = stats::approx(d$x, d$y, xo)$y)
}

# Folder listing for the file browser ####

cd_list_folder <- function(path, show_all = FALSE) {
  if (!dir.exists(path)) {
    return(list(dirs = character(), files = character()))
  }
  all <- list.files(path, all.files = FALSE, no.. = TRUE, include.dirs = TRUE)
  full <- file.path(path, all)
  isd <- dir.exists(full)
  dirs <- sort(all[isd])
  files <- all[!isd]
  if (!show_all) {
    files <- files[cd_is_data(files) | grepl("\\.(rds|pdf|png|r)$", files, ignore.case = TRUE)]
  }
  files <- files[order(tolower(files))]
  list(dirs = dirs, files = files)
}

# Base name of an output file: input name without extension plus Acycle-style suffix
cd_out_name <- function(input, suffix, ext = ".txt") {
  b <- tools::file_path_sans_ext(basename(input))
  paste0(b, suffix, ext)
}

# Number formatting for suffixes and code
cd_fmt <- function(x) {
  if (is.null(x) || !length(x)) {
    return("")
  }
  out <- character(length(x))
  for (i in seq_along(x)) {
    out[i] <- format(signif(x[i], 6), scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  }
  paste(out, collapse = "_")
}

# Deparse a value for generated R code
cd_dep <- function(x) {
  if (is.character(x) && length(x) == 1) {
    return(sprintf("\"%s\"", gsub("\\\\", "/", x)))
  }
  paste(deparse(x, width.cutoff = 500L), collapse = "")
}

# Image tab: a colour or brightness record along a route on an image ####
#
# Outcrop photographs, satellite images (an exported image or screenshot whose use is permitted), core scans and
# tree-ring images: click a route along the beds (or across the rings), set the scale with a scale bar, and the
# colour is averaged across a swath around the route at every step. The result is a depth series of grey value,
# CIE L*a*b*, RGB and HSV.

cd_image_ext <- function() {
  c("png", "jpg", "jpeg", "tif", "tiff")
}

#' Read an image as an array
#'
#' @param file Path of a PNG, JPEG or TIFF image.
#' @return A numeric array (rows x columns x channels) with values from 0 to 1: one channel for a grey-scale image,
#'   three (red, green, blue) for a colour image; an alpha channel is dropped.
#' @keywords internal
cd_image_read <- function(file) {
  ext <- tolower(tools::file_ext(file))
  pkg <- switch(ext, png = "png", jpg = "jpeg", jpeg = "jpeg", tif = "tiff", tiff = "tiff",
      stop("not a PNG, JPEG or TIFF image: ", basename(file), call. = FALSE))
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("reading .%s images needs the R package '%s': install.packages(\"%s\")", ext, pkg, pkg), call. = FALSE)
  }
  img <- switch(pkg,
      png = png::readPNG(file),
      jpeg = jpeg::readJPEG(file),
      tiff = tiff::readTIFF(file))
  if (is.list(img)) {
    img <- img[[1]]
  }
  if (length(dim(img)) == 2) {
    dim(img) <- c(dim(img), 1L)
  }
  nc <- dim(img)[3]
  if (nc == 2) {
    img <- img[, , 1, drop = FALSE]
  } else if (nc == 4) {
    img <- img[, , 1:3, drop = FALSE]
  }
  if (max(img, na.rm = TRUE) > 1) {
    img <- img / if (max(img, na.rm = TRUE) > 255) { 65535 } else { 255 }
  }
  img
}

#' Colour and brightness record along a route on an image
#'
#' Samples an image along a route (a polyline of clicked points) at regular steps. At every step the pixels on a
#' line perpendicular to the route (the swath) are averaged, which suppresses noise, cracks and small clasts. The
#' distance along the route is converted to depth (or distance) with the scale of the image.
#'
#' @param image Path of a PNG, JPEG or TIFF image, or an array as returned by \code{png::readPNG()} (values 0-1).
#' @param route The route: data frame or matrix with the x (column, from the left) and y (row, from the top)
#'   pixel coordinates of at least two points, in the order in which the record runs.
#' @param width Width of the swath across the route in pixels (numeric, >= 1). \code{Default=15}.
#' @param step Distance between samples along the route in pixels (numeric, > 0). \code{Default=1}.
#' @param scale Pixels per unit of depth or distance (numeric, > 0), e.g. from a scale bar; \code{NA} gives the
#'   distance in pixels. \code{Default=NA}.
#' @param start Depth or distance at the first point of the route (numeric). \code{Default=0}.
#' @param direction \code{1} when depth increases along the route, \code{-1} when it decreases (e.g. a route clicked
#'   upwards on a core photograph with depth increasing downwards). \code{Default=1}.
#' @param stat How the pixels of the swath are combined: \code{"mean"} or \code{"median"} (robust against cracks and
#'   specks). \code{Default="mean"}.
#' @return A data frame with one row per step: \code{depth}, \code{distance_px}, \code{x}, \code{y} (pixel position
#'   on the route), \code{grey} (luma, 0-255), \code{L}, \code{a}, \code{b} (CIE L*a*b*), \code{R}, \code{G},
#'   \code{B} (0-255), \code{hue} (degrees), \code{saturation} and \code{value} (0-1). For a grey-scale image the
#'   colour columns repeat the grey value.
#' @examples
#' img <- array(0, dim = c(50, 200, 3))
#' for (j in 1:200) {
#'   img[, j, ] <- 0.5 + 0.4 * sin(2 * pi * j / 25)
#' }
#' prof <- cd_image_profile(img, data.frame(x = c(1, 200), y = c(25, 25)), width = 9, scale = 100)
#' plot(prof$depth, prof$grey, type = "l")
#' @export
cd_image_profile <- function(image, route, width = 15, step = 1, scale = NA, start = 0, direction = 1,
    stat = c("mean", "median")) {
  stat <- match.arg(stat)
  img <- if (is.character(image)) { cd_image_read(image) } else { image }
  if (length(dim(img)) == 2) {
    dim(img) <- c(dim(img), 1L)
  }
  H <- dim(img)[1]
  W <- dim(img)[2]
  nch <- dim(img)[3]
  r <- as.data.frame(route)
  rx <- as.numeric(r[, 1])
  ry <- as.numeric(r[, 2])
  ok <- is.finite(rx) & is.finite(ry)
  rx <- rx[ok]
  ry <- ry[ok]
  if (length(rx) < 2) {
    stop("the route needs at least two points", call. = FALSE)
  }
  seg <- sqrt(diff(rx)^2 + diff(ry)^2)
  keep <- c(TRUE, seg > 0)
  rx <- rx[keep]
  ry <- ry[keep]
  seg <- seg[seg > 0]
  if (!length(seg)) {
    stop("the points of the route coincide", call. = FALSE)
  }
  cum <- c(0, cumsum(seg))
  step <- max(step, 1e-6)
  s <- seq(0, cum[length(cum)], by = step)
  px <- stats::approx(cum, rx, xout = s)$y
  py <- stats::approx(cum, ry, xout = s)$y
  # direction of the segment each sample lies on, and the normal to it
  k <- pmin(findInterval(s, cum, rightmost.closed = TRUE), length(seg))
  tx <- diff(rx)[k] / seg[k]
  ty <- diff(ry)[k] / seg[k]
  nw <- max(1L, as.integer(round(width)))
  off <- if (nw == 1L) { 0 } else { seq(-(width - 1) / 2, (width - 1) / 2, length.out = nw) }
  n <- length(s)
  # pixel (row, column) of every sample of every swath: n x nw
  cx <- outer(px, rep(1, nw)) + outer(-ty, off)
  cy <- outer(py, rep(1, nw)) + outer(tx, off)
  col <- pmin(pmax(floor(cx) + 1, 1), W)
  row <- pmin(pmax(floor(cy) + 1, 1), H)
  inside <- cx >= 0 & cx < W & cy >= 0 & cy < H
  ch <- matrix(NA_real_, n, nch)
  for (j in seq_len(nch)) {
    v <- img[cbind(as.vector(row), as.vector(col), j)]
    v[!as.vector(inside)] <- NA
    m <- matrix(v, n, nw)
    if (stat == "mean") {
      ch[, j] <- rowMeans(m, na.rm = TRUE)
    } else {
      for (i in seq_len(n)) {
        ch[i, j] <- stats::median(m[i, ], na.rm = TRUE)
      }
    }
  }
  ch[!is.finite(ch)] <- NA
  rgb <- if (nch >= 3) { ch[, 1:3, drop = FALSE] } else { cbind(ch[, 1], ch[, 1], ch[, 1]) }
  okc <- stats::complete.cases(rgb)
  lab <- matrix(NA_real_, n, 3)
  hsv <- matrix(NA_real_, 3, n)
  if (any(okc)) {
    lab[okc, ] <- grDevices::convertColor(rgb[okc, , drop = FALSE], from = "sRGB", to = "Lab")
    hsv[, okc] <- grDevices::rgb2hsv(rgb[okc, 1], rgb[okc, 2], rgb[okc, 3], maxColorValue = 1)
  }
  sc <- if (is.finite(scale) && scale > 0) { scale } else { 1 }
  data.frame(depth = start + direction * s / sc, distance_px = s, x = px, y = py,
      grey = 255 * (0.299 * rgb[, 1] + 0.587 * rgb[, 2] + 0.114 * rgb[, 3]),
      L = lab[, 1], a = lab[, 2], b = lab[, 3], R = 255 * rgb[, 1], G = 255 * rgb[, 2], B = 255 * rgb[, 3],
      hue = 360 * hsv[1, ], saturation = hsv[2, ], value = hsv[3, ])
}

# The image reduced for display (at most max_px along its longest side) as a raster
cd_image_display <- function(img, max_px = 1600) {
  H <- dim(img)[1]
  W <- dim(img)[2]
  k <- max(1L, as.integer(ceiling(max(H, W) / max_px)))
  small <- img[seq(1, H, by = k), seq(1, W, by = k), , drop = FALSE]
  if (dim(small)[3] == 1) {
    small <- small[, , 1]
  }
  list(raster = grDevices::as.raster(small), W = W, H = H)
}

# The swath outline of a route (both edges), for drawing
cd_image_swath <- function(rx, ry, width) {
  n <- length(rx)
  if (n < 2) {
    return(NULL)
  }
  nx <- numeric(n)
  ny <- numeric(n)
  for (i in seq_len(n)) {
    a <- max(1, i - 1)
    b <- min(n, i + 1)
    dx <- rx[b] - rx[a]
    dy <- ry[b] - ry[a]
    l <- sqrt(dx^2 + dy^2)
    if (l > 0) {
      nx[i] <- -dy / l
      ny[i] <- dx / l
    }
  }
  h <- (width - 1) / 2
  list(x1 = rx + nx * h, y1 = ry + ny * h, x2 = rx - nx * h, y2 = ry - ny * h)
}

cd_plot_image_route <- function(disp, route = NULL, width = 15, scale_pts = NULL) {
  oldpar <- cd_par_keep_coords(graphics::par(no.readonly = TRUE))
  on.exit(graphics::par(oldpar))
  graphics::par(mar = c(2.6, 3.6, 0.6, 0.6), mgp = c(1.8, 0.5, 0), las = 1, tcl = -0.25, cex.axis = 0.8)
  graphics::plot(NA, xlim = c(0, disp$W), ylim = c(disp$H, 0), asp = 1, xaxs = "i", yaxs = "i", xlab = "x (pixels)",
      ylab = "y (pixels)")
  graphics::rasterImage(disp$raster, 0, disp$H, disp$W, 0, interpolate = TRUE)
  if (!is.null(scale_pts) && nrow(scale_pts)) {
    graphics::lines(scale_pts$x, scale_pts$y, col = "black", lwd = 5)
    graphics::lines(scale_pts$x, scale_pts$y, col = "#ffd400", lwd = 2.5)
    graphics::points(scale_pts$x, scale_pts$y, pch = 22, bg = "#ffd400", col = "black", cex = 1.3)
  }
  if (!is.null(route) && nrow(route)) {
    sw <- cd_image_swath(route$x, route$y, width)
    if (!is.null(sw)) {
      graphics::lines(sw$x1, sw$y1, col = "white", lty = 2)
      graphics::lines(sw$x2, sw$y2, col = "white", lty = 2)
    }
    graphics::lines(route$x, route$y, col = "black", lwd = 4)
    graphics::lines(route$x, route$y, col = "#ff6b4a", lwd = 2)
    graphics::points(route$x, route$y, pch = 21, bg = "#ff6b4a", col = "white", cex = 1.4, lwd = 1.5)
    graphics::text(route$x[1], route$y[1], "start", pos = 3, col = "white", font = 2, cex = 0.8)
  }
  graphics::box()
  invisible(NULL)
}

cd_plot_image_profile <- function(prof, channel, xlab) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::par(mar = c(3.6, 4.4, 1.4, 1), mgp = c(2.2, 0.6, 0), las = 1, tcl = -0.3, cex.axis = 0.85)
  y <- prof[[channel]]
  graphics::plot(prof$depth, y, type = "l", col = "#1f4e79", xlab = xlab, ylab = channel,
      main = sprintf("Profile along the route: %s (%d samples)", channel, nrow(prof)), cex.main = 0.85, font.main = 1)
  graphics::grid(col = "grey90", lty = 1)
  graphics::lines(prof$depth, y, col = "#1f4e79")
  invisible(NULL)
}

cd_image_channels <- function() {
  c("Grey value (luma, 0-255)" = "grey", "Lightness L*" = "L", "Red-green a*" = "a", "Yellow-blue b*" = "b",
      "Red (0-255)" = "R", "Green (0-255)" = "G", "Blue (0-255)" = "B", "Hue (degrees)" = "hue",
      "Saturation" = "saturation", "Value (brightness)" = "value")
}

cd_image_tab <- function() {
  shiny::tabPanel("Image", value = "image",
      shiny::fluidRow(
          shiny::column(4, class = "cd-toolform",
              shiny::tags$h4("Colour record along a route on an image"),
              shiny::tags$div(class = "cd-info", "Outcrop photographs, core scans, tree-ring images or satellite images:",
                  " click a route along the beds (or across the rings), set the scale, and the colour is averaged across",
                  " a swath at every step along the route. For satellite images use an exported image or screenshot",
                  " that you are allowed to use, and set the scale from its scale bar."),
              shiny::tags$div(class = "cd-uses", "CycloDesk::cd_image_profile (png, jpeg, tiff packages)"),
              cd_section("1. Image",
                  shiny::uiOutput("img_file_ui"),
                  shiny::fileInput("img_upload", "or copy an image into the working folder",
                      accept = c(".png", ".jpg", ".jpeg", ".tif", ".tiff"))),
              cd_section("2. Route and scale",
                  shiny::radioButtons("img_mode", "A click on the image", c("adds a point to the route" = "route",
                      "sets the scale bar (two points)" = "scale"), selected = "route"),
                  shiny::tags$div(class = "cd-btnrow",
                      shiny::actionButton("img_undo", "Undo last point", icon = shiny::icon("rotate-left"), class = "btn-sm"),
                      shiny::actionButton("img_clear", "Clear route", icon = shiny::icon("eraser"), class = "btn-sm"),
                      shiny::actionButton("img_reverse", "Reverse direction", icon = shiny::icon("arrows-left-right"),
                          class = "btn-sm")),
                  shiny::fluidRow(
                      shiny::column(6, shiny::numericInput("img_width", "Swath width (pixels)", 15, min = 1, step = 2)),
                      shiny::column(6, shiny::numericInput("img_step", "Step (pixels)", 1, min = 0.1, step = 0.5))),
                  shiny::selectInput("img_stat", "Across the swath", c("Mean" = "mean",
                      "Median (robust against cracks and specks)" = "median")),
                  shiny::fluidRow(
                      shiny::column(6, shiny::numericInput("img_scalelen", "Length of the scale bar", 1, min = 0)),
                      shiny::column(6, shiny::numericInput("img_ppu", "Pixels per unit", NA, min = 0))),
                  shiny::tags$div(class = "cd-hint", "Click both ends of a scale bar (or of a known distance) with",
                      " \"sets the scale bar\" and give its length; or type the pixels per unit directly. Without a",
                      " scale the record is in pixels."),
                  shiny::fluidRow(
                      shiny::column(6, shiny::numericInput("img_start", "Depth at the start", 0)),
                      shiny::column(6, shiny::selectInput("img_dir", "Along the route, depth", c("increases" = "1",
                          "decreases" = "-1"))))),
              cd_section("3. Record",
                  shiny::selectInput("img_channel", "Show", cd_image_channels()),
                  shiny::tags$div(class = "cd-btnrow",
                      shiny::actionButton("img_extract", "Extract and write", icon = shiny::icon("file-export"),
                          class = "btn-primary btn-sm")),
                  shiny::tags$div(class = "cd-hint", "Writes <image>-profile.txt (all colour columns, with header) and",
                      " <image>-<channel>.txt (depth and the shown channel) to the working folder.")),
              shiny::tags$br()),
          shiny::column(8,
              shiny::plotOutput("img_plot", height = "560px", click = "img_click"),
              shiny::verbatimTextOutput("img_text"),
              shiny::plotOutput("img_profile", height = "230px"))))
}

cd_image_server <- function(input, output, session, ctx) {
  rv <- ctx$rv
  st <- shiny::reactiveValues(file = NULL, disp = NULL, route = data.frame(x = numeric(), y = numeric()),
      scale_pts = data.frame(x = numeric(), y = numeric()), ver = 0)
  cache <- new.env(parent = emptyenv())

  images <- shiny::reactive({
    rv$tick
    fs <- list.files(rv$folder)
    fs[tolower(tools::file_ext(fs)) %in% cd_image_ext()]
  })
  output$img_file_ui <- shiny::renderUI({
    fs <- images()
    if (!length(fs)) {
      return(shiny::tags$div(class = "cd-hint", "No PNG, JPEG or TIFF images in the working folder yet."))
    }
    shiny::selectInput("img_file", "Image in the working folder", choices = fs, selected = shiny::isolate(st$file))
  })
  shiny::observeEvent(input$img_upload, {
    u <- input$img_upload
    if (is.null(u)) {
      return()
    }
    dest <- file.path(rv$folder, u$name)
    if (file.exists(dest)) {
      shiny::showNotification(paste(u$name, "is already in the working folder; that file is used."), type = "warning")
    } else {
      file.copy(u$datapath, dest)
    }
    ctx$refresh()
    shiny::updateSelectInput(session, "img_file", choices = c(images(), u$name), selected = u$name)
  })
  shiny::observeEvent(input$img_file, {
    f <- input$img_file
    if (is.null(f) || !nzchar(f) || identical(f, st$file)) {
      return()
    }
    img <- shiny::withProgress(message = "Reading the image ...", value = 0.5, {
      try(cd_image_read(file.path(rv$folder, f)), silent = TRUE)
    })
    if (inherits(img, "try-error")) {
      shiny::showNotification(conditionMessage(attr(img, "condition")), type = "error", duration = 10)
      return()
    }
    cache$img <- img
    st$file <- f
    st$disp <- cd_image_display(img)
    st$route <- data.frame(x = numeric(), y = numeric())
    st$scale_pts <- data.frame(x = numeric(), y = numeric())
    st$ver <- st$ver + 1
    ctx$say("Image: ", f, sprintf(" (%d x %d pixels)", dim(img)[2], dim(img)[1]))
  })

  shiny::observeEvent(input$img_click, {
    cl <- input$img_click
    if (is.null(st$disp) || is.null(cl$x) || is.null(cl$y)) {
      return()
    }
    x <- min(max(cl$x, 0), st$disp$W)
    y <- min(max(cl$y, 0), st$disp$H)
    if (identical(input$img_mode, "scale")) {
      sp <- st$scale_pts
      if (nrow(sp) >= 2) {
        sp <- sp[0, ]
      }
      st$scale_pts <- rbind(sp, data.frame(x = x, y = y))
    } else {
      st$route <- rbind(st$route, data.frame(x = x, y = y))
    }
  })
  shiny::observeEvent(input$img_undo, {
    if (nrow(st$route)) {
      st$route <- st$route[-nrow(st$route), , drop = FALSE]
    }
  })
  shiny::observeEvent(input$img_clear, {
    st$route <- data.frame(x = numeric(), y = numeric())
  })
  shiny::observeEvent(input$img_reverse, {
    if (nrow(st$route)) {
      st$route <- st$route[rev(seq_len(nrow(st$route))), , drop = FALSE]
    }
  })
  # pixels per unit from the scale bar
  shiny::observeEvent(list(st$scale_pts, input$img_scalelen), {
    sp <- st$scale_pts
    L <- input$img_scalelen
    if (nrow(sp) == 2 && is.finite(L) && L > 0) {
      d <- sqrt(diff(sp$x)^2 + diff(sp$y)^2)
      if (d > 0) {
        shiny::updateNumericInput(session, "img_ppu", value = signif(d / L, 6))
      }
    }
  }, ignoreInit = TRUE)

  ppu <- shiny::reactive({
    v <- input$img_ppu
    if (is.null(v) || !is.finite(v) || v <= 0) { NA_real_ } else { v }
  })
  # the profile, recomputed (debounced) when the route or the settings change
  prof_trigger <- shiny::reactive({
    list(st$route, st$ver, input$img_width, input$img_step, input$img_stat, ppu(), input$img_start, input$img_dir)
  })
  prof <- shiny::debounce(shiny::reactive({
    prof_trigger()
    r <- st$route
    if (is.null(cache$img) || nrow(r) < 2) {
      return(NULL)
    }
    out <- try(cd_image_profile(cache$img, r, width = max(1, input$img_width), step = max(0.1, input$img_step),
        scale = ppu(), start = if (is.finite(input$img_start)) { input$img_start } else { 0 },
        direction = as.numeric(input$img_dir), stat = input$img_stat), silent = TRUE)
    if (inherits(out, "try-error")) {
      return(NULL)
    }
    out
  }), 400)

  output$img_plot <- shiny::renderPlot({
    if (is.null(st$disp)) {
      cd_message_plot("Choose an image (section 1), then click a route along the beds")
      return(invisible(NULL))
    }
    cd_plot_image_route(st$disp, route = st$route, width = max(1, input$img_width), scale_pts = st$scale_pts)
  }, res = 96)
  output$img_profile <- shiny::renderPlot({
    p <- prof()
    if (is.null(p)) {
      cd_message_plot("Click at least two points along the beds to see the record")
      return(invisible(NULL))
    }
    cd_plot_image_profile(p, input$img_channel, xlab = if (is.na(ppu())) { "Distance along the route (pixels)" } else {
      "Depth / distance" })
  }, res = 96)
  output$img_text <- shiny::renderText({
    if (is.null(st$disp)) {
      return("")
    }
    p <- prof()
    sc <- ppu()
    paste(c(sprintf("%s: %d x %d pixels. Route: %d points%s.", st$file, st$disp$W, st$disp$H, nrow(st$route),
        if (nrow(st$scale_pts) == 2) { sprintf("; scale bar %s pixels", cd_fmt(sqrt(diff(st$scale_pts$x)^2 +
            diff(st$scale_pts$y)^2))) } else { "" }),
        if (is.null(p)) { NULL } else {
          sprintf("%d samples, %s to %s %s (%s pixels per unit).", nrow(p), cd_fmt(min(p$depth)), cd_fmt(max(p$depth)),
              if (is.na(sc)) { "pixels" } else { "units" }, if (is.na(sc)) { "no scale" } else { cd_fmt(sc) })
        }), collapse = "\n")
  })

  shiny::observeEvent(input$img_extract, {
    p <- prof()
    if (is.null(p)) {
      shiny::showNotification("Click at least two points along the beds first.", type = "warning")
      return()
    }
    b <- tools::file_path_sans_ext(st$file)
    ch <- input$img_channel
    f1 <- paste0(b, "-profile.txt")
    f2 <- paste0(b, "-", ch, ".txt")
    s2 <- p[, c("depth", ch)]
    s2 <- s2[stats::complete.cases(s2), ]
    s2 <- s2[order(s2$depth), ]
    write_series(p, file.path(rv$folder, f1), header = TRUE)
    write_series(s2, file.path(rv$folder, f2))
    r <- st$route
    code <- c(sprintf("route <- data.frame(x = %s,\n    y = %s)", cd_dep(signif(r$x, 6)), cd_dep(signif(r$y, 6))),
        sprintf(paste0("prof <- CycloDesk::cd_image_profile(\"%s\", route, width = %s, step = %s, scale = %s, start = %s,\n",
            "    direction = %s, stat = \"%s\")"), st$file, cd_fmt(input$img_width), cd_fmt(input$img_step),
            if (is.na(ppu())) { "NA" } else { cd_fmt(ppu()) }, cd_fmt(input$img_start), input$img_dir, input$img_stat),
        sprintf("CycloDesk::write_series(prof, \"%s\", header = TRUE)", f1),
        sprintf("rec <- prof[order(prof$depth), c(\"depth\", \"%s\")]", ch),
        sprintf("CycloDesk::write_series(rec[complete.cases(rec), ], \"%s\")", f2))
    ctx$add_code(code, paste("Image profile:", st$file))
    ctx$say("Image profile written: ", f1, ", ", f2)
    shiny::showNotification(paste("Written", f1, "and", f2), type = "message")
    ctx$refresh(select = f2)
  })
}

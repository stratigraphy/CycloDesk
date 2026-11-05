# Astronomical solutions and insolation ####

#' Astronomical solution with a local cache
#'
#' Returns an astronomical solution through \code{astrochron::getLaskar()}. The download (a few MB) is kept in the
#' user cache folder of CycloDesk, so later calls work offline.
#'
#' @param sol Solution name (character string): \code{"la04"} (eccentricity, obliquity and longitude of perihelion,
#'   Laskar et al. 2004), \code{"la10a"}, \code{"la10b"}, \code{"la10c"}, \code{"la10d"} (eccentricity, Laskar et
#'   al. 2011a) or \code{"la11"} (eccentricity, Laskar et al. 2011b). \code{Default="la04"}.
#' @return A data frame; the first column is time in ka.
#' @export
cd_laskar <- function(sol = "la04") {
  dir <- tools::R_user_dir("CycloDesk", "cache")
  f <- file.path(dir, paste0(sol, ".rds"))
  if (file.exists(f)) {
    return(readRDS(f))
  }
  d <- astrochron::getLaskar(sol = sol, verbose = FALSE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(d, f)
  d
}

#' Daily mean insolation from the La2004 solution
#'
#' Computes the daily mean insolation at the top of the atmosphere (Berger, 1978) for a latitude and a true solar
#' longitude, from the eccentricity, obliquity and longitude of perihelion of La2004.
#'
#' @param tmin,tmax Time interval in ka (numeric, tmin >= 0). \code{Default=0, 1000}.
#' @param dt Time step in ka (numeric, 1 or larger: La2004 is given every ka). \code{Default=1}.
#' @param lat Latitude in degrees (numeric, -90 to 90). \code{Default=65}.
#' @param lon True solar longitude in degrees from the vernal equinox (numeric): 90 is the June solstice, 180 the
#'   September equinox, 270 the December solstice. \code{Default=90}.
#' @param S0 Solar constant in W/m2 (numeric). \code{Default=1365}.
#' @param solution Optional data frame as returned by \code{cd_laskar("la04")}; \code{NULL} reads it from the cache.
#' @return A data frame with columns \code{time_ka} and \code{insolation} (W/m2).
#' @export
cd_insolation <- function(tmin = 0, tmax = 1000, dt = 1, lat = 65, lon = 90, S0 = 1365, solution = NULL) {
  if (is.null(solution)) {
    solution <- cd_laskar("la04")
  }
  s <- solution[solution[, 1] >= tmin & solution[, 1] <= tmax, , drop = FALSE]
  if (dt > 1) {
    s <- s[(seq_len(nrow(s)) - 1) %% round(dt) == 0, , drop = FALSE]
  }
  t <- s[, 1]
  ecc <- s$ecc_LA04
  obl <- s$tilt_LA04
  varpi <- s$prec_LA04 + pi
  lam <- lon * pi / 180
  phi <- lat * pi / 180
  delta <- asin(sin(obl) * sin(lam))
  nu <- lam - varpi
  rho_inv2 <- ((1 + ecc * cos(nu)) / (1 - ecc^2))^2
  ch <- -tan(phi) * tan(delta)
  h0 <- numeric(length(ch))
  for (i in seq_along(ch)) {
    if (ch[i] >= 1) {
      h0[i] <- 0
    } else if (ch[i] <= -1) {
      h0[i] <- pi
    } else {
      h0[i] <- acos(ch[i])
    }
  }
  q <- S0 / pi * rho_inv2 * (h0 * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(h0))
  data.frame(time_ka = t, insolation = q)
}

# Example data sets available in the Basic Series menu
cd_examples <- function() {
  c("IODP 926 grey scale (Zeeden et al. 2013)" = "grey",
      "Sullivan core magnetic susceptibility (Pas et al. 2018)" = "mag",
      "ETP astronomical signal example" = "astrosignal_example",
      "Depth rank example" = "depth_rank_example",
      "Total solar irradiance 0-9400 ka (Steinhilber et al. 2012)" = "TSI",
      "Bisciaro Fm XRF (several columns)" = "Bisciaro_XRF")
}

cd_example_data <- function(name) {
  e <- new.env()
  utils::data(list = name, package = "WaverideR", envir = e)
  as.data.frame(get(name, envir = e))
}

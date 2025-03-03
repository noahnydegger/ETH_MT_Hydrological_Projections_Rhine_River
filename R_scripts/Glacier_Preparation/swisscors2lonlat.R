swisscors2lonlat <- function (chx, chy)
{
    if ("list" %in% class(chx)) {
        a <- chx[[1]]
        b <- chx[[2]]
        chx <- a
        chy <- b
        rm(a, b)
    }
    if ("matrix" %in% class(chx)) {
        if (dim(chx)[2] != 2) {
            stop("Matrix chx does not have two columns.")
        }
        a <- chx[, 1]
        b <- chx[, 2]
        chx <- a
        chy <- b
        rm(a, b)
    }
    x <- chy
    y <- chx
    if (length(x) != length(y)) {
        stop(message = "Unequal length of x- & y-coordinates !")
    }
    #if (min(x, na.rm = TRUE) < 50000 | max(x, na.rm = TRUE) >
    #    320000) {
    #    stop(message = "Northing (x) seems to be outside of Switzerland (50'000 - 320'000) !")
    #}
    #if (min(y, na.rm = TRUE) < 450000 | max(y, na.rm = TRUE) >
    #    900000) {
    #    stop(message = "Easting (y) seems to be outside of Switzerland (450'000 - 850'000) !")
    #}
    y.d <- (y - 6e+05)/1e+06
    x.d <- (x - 2e+05)/1e+06
    lam.d <- 2.6779094 + 4.728982 * y.d + 0.791484 * y.d * x.d +
        0.1306 * y.d * x.d^2 - 0.0436 * y.d^3
    phi.d <- 16.9023892 + 3.238272 * x.d - 0.270978 * y.d^2 -
        0.002528 * x.d^2 - 0.0447 * y.d^2 * x.d - 0.014 * x.d^3
    lam <- lam.d * 100/36
    phi <- phi.d * 100/36
    if (length(lam) > 1) {
        ll <- cbind(lam, phi)
    }
    else {
        ll <- c(lam, phi)
    }
    ll
}

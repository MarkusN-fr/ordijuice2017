#' Display JUICE header data as surface on ordination plot in 3d.
#'
#' \code{orglsurf} plots environmental data contained in the short header data
#' from JUICE as a surface on 3d ordination plots.
#'
#'
#' @seealso
#' \code{\link{draw.3d}}

#--------------------------------------------


orglsurf <-

function (x, y, choices = c(1,2,3), knots = 10, family = 'gaussian', col = 'red', thinplate = TRUE, add = FALSE, display = "sites",

    w = weights(x), ...)

    {

    require (geometry, quietly = T)

    GRID = 30

    w <- eval(w)

    if (!is.null(w) && length(w) == 1)

        w <- NULL

    if (!require(mgcv))

        stop("Requires package `mgcv'")

    X <- scores(x, choices = choices, display = display)

    x1 <- X[, 1]

    x2 <- X[, 2]

    x3 <- X[, 3]

    if (thinplate)

        mod <- gam(y ~ s(x1, x2, x3, k = knots), family = family, weights = w) else

          mod <- gam(y ~ s(x1, k = knots) + s(x2, k = knots) + s(x3, k = knots), family = family, weights = w)

    xn1 <- seq(min(x1), max(x1), len = GRID)

    xn2 <- seq(min(x2), max(x2), len = GRID)

    xn3 <- seq(min(x3), max(x3), len = GRID)

    newd <- expand.grid(x1 = xn1, x2 = xn2, x3 = xn3)

    fit <- predict(mod, type = "response", newdata = as.data.frame(newd))



    isinside <- rep (T, nrow (newd))

    convexhulls <- convhulln (X)

    for (ro in seq (1, nrow (convexhulls)))

      {

      bas.matrix <- X[convexhulls[ro,],]

      D <- det (bas.matrix)

      d <- 1

      bas.matrix.a <- bas.matrix

      bas.matrix.a[,1] <- 1

      a <- -d/D*det (bas.matrix.a)



      bas.matrix.b <- bas.matrix

      bas.matrix.b[,2] <- 1

      b <- -d/D*det (bas.matrix.b)



      bas.matrix.c <- bas.matrix

      bas.matrix.c[,3] <- 1

      c <- -d/D*det (bas.matrix.c)

      kolik <- a*newd[,1]+b*newd[,2]+c*newd[,3] + d

      isinside[kolik < 0] <- F

      }

if (!add) ordirgl (x)

fit.isinside <- fit[isinside]

rescale.fit.isinside <-  abs(fit.isinside-min(fit.isinside)) /  diff (range (fit.isinside)) *100

rgl.sprites (newd[isinside,], col = heat.colors (100)[ceiling(rescale.fit.isinside)],  lit=FALSE, alpha=.05, textype="alpha", texture=system.file("textures/particle.png", package="rgl") )

return (mod)

}

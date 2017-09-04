#' Plot group centroids in ordination plot.
#'
#' \code{ordicenter} determines the coordinates of the group centroids,
#' and plots them in 2 dimensions.
#'
#'
#' @seealso \code{\link{draw.2d}}
#' \code{\link{orglcenter}}

#--------------------------------------------


ordicenter <-

  function (ord, groups, display = "sites", w = weights(ord, display),

            show.groups, ...)

  {

    weights.default <- function(object, ...) NULL

    pts <- scores(ord, display = display, ...)

    w <- eval(w)

    if (length(w) == 1)

      w <- rep(1, nrow(pts))

    if (is.null(w))

      w <- rep(1, nrow(pts))

    if (!missing(show.groups))

    {

      take <- groups %in% show.groups

      pts <- pts[take, , drop = FALSE]

      groups <- groups[take]

      w <- w[take]

    }

    out <- seq(along = groups)

    inds <- names(table(groups))

    for (is in inds)

    {

      gr <- out[groups == is]

      if (length(gr) > 1)

      {

        X <- pts[gr, ]

        W <- w[gr]

        ave <- apply(X, 2, weighted.mean, w = W)

        vegan:::ordiArgAbsorber(ave[1], ave[2], labels = is, FUN = text, cex = 1.2, ...)

      }

      if (length(gr) == 1)

      {

        X <- pts[gr, ]

        W <- w[gr]

        vegan:::ordiArgAbsorber(X[1], X[2], labels = is, FUN = text, cex = 1.2, ...)

      }

    }

    invisible()

  }

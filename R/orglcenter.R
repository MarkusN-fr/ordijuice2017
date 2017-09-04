#' Group centroids for ordination plots in 3d.
#'
#' \code{orglcenter} determines the coordinates of the group centroids,
#' and plots them in 3 dimensions.
#'
#'
#' @seealso \code{\link{ordicenter}}
#' \code{\link{draw.3d}}

#--------------------------------------------


orglcenter <-

function (object, groups, display = "sites", w = weights(object,

    display), choices = 1:3, ...)

{

    weights.default <- function(object, ...) NULL

    pts <- scores(object, display = display, choices = choices, ...)

    out <- seq(along = groups)

    w <- eval(w)

    if (length(w) == 1) w <- rep(1, nrow(pts))

    if (is.null(w)) w <- rep(1, nrow(pts))

    inds <- names(table(groups))

    shift <- 0.02 * diff (range (pts[,1]))

    for (is in inds) {

      gr <- out[groups == is]

      if (length(gr) > 1)

        {

        X <- pts[gr, ]

        W <- w[gr]

        ave <- apply(X, 2, weighted.mean, w = W)

        rgl.texts(ave[1]+shift, ave[2]+shift, ave[3]+shift, text = is, col = 'black', ...)

        }

      if (length (gr) == 1)

        {

        X <- pts[gr, ]

        rgl.texts (X+shift, text = is, col = 'black', ...)

        }

      }

  invisible()

}

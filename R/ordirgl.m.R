#' Basic 3d plot for ordination.
#'
#' \code{ordirgl.m} provides the basic structure for 3d plotting the ordination
#'  results. This function is called by \code{\link{draw.3d}}, and some of its
#'  arguments are passed on to \code{ordirgl.m}.
#'
#'  It is a slightly modified version of \code{ordirgl} from \code{vegan3d}.
#'
#'
#' @seealso \code{\link{draw.3d}}

#--------------------------------------------



ordirgl.m <-

function (object, display = "sites", choices = 1:3, type = "p", ax.col = "red", arr.col = "yellow", text, envfit.m, ordi.type, ...)

{

    if (!require(rgl))

        stop("Requires package 'rgl'")

      if (ordi.type == "pcoa") {

        library(BiodiversityR)

        x <- add.spec.scores(object, input.data$spec.data, method='pcoa.scores')$cproj

     } else {

       x <- scores(object, display = display, choices = choices,

                   ...)

      }

    if (ncol(x) < 3)

        stop("3D display needs three dimensions...")

    if (type == "p")

        points3d(x[, 1], x[, 2], x[, 3], ...)

    else if (type == "t") {

        if (missing(text))

            text <- rownames(x)

        texts3d(x[, 1], x[, 2], x[, 3], text, justify = "center",

            ...)

    }

    lines3d(range(x[, 1]), c(0, 0), c(0, 0), col = ax.col)

    lines3d(c(0, 0), range(x[, 2]), c(0, 0), col = ax.col)

    lines3d(c(0, 0), c(0, 0), range(x[, 3]), col = ax.col)

    texts3d(1.1 * max(x[, 1]), 0, 0, colnames(x)[1], col = ax.col, adj = 0.5)

    texts3d(0, 1.1 * max(x[, 2]), 0, colnames(x)[2], col = ax.col, adj = 0.5)

    texts3d(0, 0, 1.1 * max(x[, 3]), colnames(x)[3], col = ax.col, adj = 0.5)

    if (!missing(envfit.m)) {

      for (column in seq (1, dim (as.matrix(envfit.m))[2]))

      {

       object.fit <- envfit (object, envfit.m[,column, drop = F], choices = 1:4)

       bp <- scores(object.fit, dis = "bp", choices = choices)

       cn <- scores(object.fit, dis = "cn", choices = choices)

       if (!is.null(cn) && !any(is.na(cn))) {

            bp <- bp[!(rownames(bp) %in% rownames(cn)), , drop = FALSE]

            texts3d(cn[, 1], cn[, 2], cn[, 3], rownames(cn),

                col = arr.col[column], adj = 0.5)

            points3d(cn[, 1], cn[, 2], cn[, 3], size = 5, col = arr.col[column])

        }

        if (!is.null(bp) && nrow(bp) > 0) {

            mul <- c(range(x[, 1]), range(x[, 2]), range(x[,

                3]))/c(range(bp[, 1]), range(bp[, 2]), range(bp[,

                3]))

            mul <- mul[is.finite(mul) & mul > 0]

            mul <- min(mul)

            bp <- bp * mul

            for (i in 1:nrow(bp)) {

                lines3d(c(0, bp[i, 1]), c(0, bp[i, 2]), c(0,

                  bp[i, 3]), col = arr.col [column])

                texts3d(1.1 * bp[i, 1], 1.1 * bp[i, 2], 1.1 *

                  bp[i, 3], rownames(bp)[i], col = arr.col[column], adj = 0.5)

            }

        }

      }

    }

    invisible()

}

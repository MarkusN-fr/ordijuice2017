#' Envelopes for ordination plots in 3d.
#'
#' \code{orglhull} plots envelopes for the groups of ordination results in 3d.
#'
#'
#' @seealso \code{\link{plot.envfit.m}}
#' \code{\link{draw.3d}}

#--------------------------------------------


orglhull <-

function (object, groups, display = 'sites', choices = 1:3, show.groups = NULL,  ...)

{

require (geometry, quietly = T)

pts <- scores (object, choices = choices, display = display, ...)

if (!is.null(show.groups))

{take <- groups %in% show.groups

  pts <- pts[take, , drop = FALSE]

  groups <- groups[take]}

out <- seq (along = groups)

inds <- names (table (groups))

for (is in inds)

  {

  gr <- out[groups == is]

  if (length (gr) == 2)

    {

    X <- pts[gr,]

    rgl.lines (X[,1], X[,2], X[,3], lwd = 2, ...)

    }



  if (length (gr) == 3)

    {

    X <- pts[gr,]

    rgl.triangles (X[,1], X[,2], X[,3], ...)

    }

  if (length (gr) > 3)

    {

    X <- pts[gr,]

    # check if the points are not situated in hyperplane

      bas.matrix <- X[1:3,]

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

      kolik <- a*X[,1]+b*X[,2]+c*X[,3] + d

      save (X, file = 'X.r')

      if (all (abs (kolik) <=  1e-10)) X <- X + runif (length (X), min = -1e-5, max = 1e-5)



    tr <-  t (convhulln (X))

    print (tr)

    rgl.triangles (X[tr,1], X[tr,2], X[tr,3], ...)

    }

   }

   invisible ()

}

#' Open R window for ordination plot.
#'
#' \code{open.r.window} opens the appropriate window for the ordination plot,
#' according to the desired visual representation (2d or 3d).
#'
#' @param three.dim Logical. If the plot should be three-dimensional
#' or not.
#' @param resolution Resolution (size) of the window.
#'
#' @return R window that will contain the ordination plot.
#'
#' @seealso \code{\link{draw.2d}}
#' \code{\link{draw.2d}}

#--------------------------------------------


open.r.window <-

function (three.dim, resolution)

{

  if (!three.dim)

  {

    windows (xpos = 300, ypos = 10)

    plot.new()

    text (.5,.5, '..................', cex = 3, col = 'red')

    bringToTop (dev.cur(), stay = F)

  } else

  {

    library (rgl)

    open3d()

    par3d (windowRect = c (300, 100, resolution[1]-100, resolution[2]-100), userMatrix =  matrix (c(1,0,0,0,0,0.9659258, -0.2588190, 0, 0, 0.2588190, 0.9659258, 0, 0, 0, 0, 1), ncol  = 4, byrow = T))

    rgl.points (seq(-5,5, len = 50),0,0, col = 'red', cex = 4)

    rgl.bringtotop (stay = F)

  }

}

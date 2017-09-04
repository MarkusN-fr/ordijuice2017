#' Perfom DCA (Detrended Correspondance Analysis) on JUICE data.
#'
#' \code{draw.dca} performs DCA (Detrended Correspondance Analysis) on the
#' data output from JUICE. It is directly called from the JUICE interface,
#' and one of the centerpieces of ordijuice.
#' JUICE's output is transformed by the function
#' \code{\link{read.check.data}}, that is passed on some of the arguments
#'  listed here, so that \code{draw.dca} can use it.
#' All the input arguments are provided by the JUICE software.
#'
#' @param display.in.diagram What to display in diagram : 'species',
#' 'sites' or 'both'.
#' @param display.species How the species should be represented : as
#' points, with labels etc.
#' @param display.sites How the sites should be represented : as
#' points, with labels, group labels etc.
#' @param axes.shown Number of axes to plot.
#' @param down.rare.spec Transforms the input data so that rare species are
#' downweighed.
#' @param display.EIV Logical. If Ellenberg's Indicator Values should
#' be added to the plot.
#' @param display.header Logical. If header-data from JUICE should be
#' plotted as well.
#' @param display.envelope Logical. If the groups should have envelopes.
#' @param header.name Name given to the header-data.
#' @param display.header.style How the header-data should be represented
#'  on the plot.
#' @param display.spider Logical. If the groups should have a spiderplot
#'  within.
#' @param display.group.center Logical. If the groups should have
#' centroids.
#' @param three.dim Logical. If the output should be plotted in 2 or 3
#' dimensions. According to its value, the ordination will be plotted with
#' \code{\link{draw.2d}} or \code{\link{draw.3d}}.
#' @param resolution Resolution (size) of the plot window.
#' @param bw Logical. Plot in black & white or in color.
#' @param ... additional arguments to be passed to or from methods.
#'
#' @return Invisible.
#'
#' @seealso
#' \code{\link{draw.ca}}
#' \code{\link{draw.nmds}}
#' \code{\link{draw.pca}}
#' \code{\link{draw.pcoa}}
#' \code{\link{draw.2d}}
#' \code{\link{draw.3d}}
#'
#' @export

#--------------------------------------------


draw.dca <-

  function(display.in.diagram = c('sites'),

           display.species = c('none'),

           display.sites = c('points'),

           axes.shown = c(1,2),

           down.rare.spec = 0,

           display.EIV = FALSE,

           display.header = FALSE,

           display.envelope = FALSE,

           header.name = 'env',

           display.header.style = c('arrow'),

           display.spider,

           display.group.center = FALSE,

           three.dim = FALSE,

           resolution = c(1280, 768),

           bw = FALSE,

           ...)

  {

    newRversion <- check.install (display.spider)

    open.r.window(three.dim, resolution)

    if (newRversion) pb <- myTkProgressBar (paste (ifelse (three.dim, '3D', '2D'),'DCA - Analysis progress'), 'Importing data from JUICE', 0, 100, 20)

    else pb <- NULL


    write ('End of ordination', file='result.txt')

    library (vegan)

    input.data <- read.check.data (display.sites = display.sites, display.EIV = display.EIV, display.header = display.header, display.envelope = display.envelope, display.spider = display.spider, display.group.center = display.group.center)



    # 2. update progress bar

    if (newRversion) setTkProgressBar (pb, label = 'Calculation of ordination', value = 40)



    # calculation of ordination

    last.result <-  use.last (input.data, 'dca', setting = list (down.rare.spec = down.rare.spec))

    if (last.result$use.last.result) spec.data.ord <- last.result$last.data.result  else

      spec.data.ord <- decorana(input.data$spec.data, iweigh=down.rare.spec)

    ordi.type <- "dca"

    # 3. update progress  bar

    if (newRversion) setTkProgressBar (pb, label = 'Saving results', value = 60)



    save.ord.result (spec.data.ord, last.result$use.last.result, 'dca', input.data$deleted.plots)



    # 3. update progress  bar

    if (newRversion) setTkProgressBar (pb, label = 'Drawing the figure', value = 80)





    if (three.dim)

      draw.3d(input.data = input.data, spec.data.ord = spec.data.ord, display.in.diagram = display.in.diagram, display.species = display.species, display.sites = display.sites, axes.shown = axes.shown, display.EIV = display.EIV, display.header = display.header, display.envelope = display.envelope, header.name = header.name, display.header.style = display.header.style, display.spider = display.spider, display.group.center = display.group.center, pb = pb, ordi.type = ordi.type) else

        draw.2d(input.data = input.data, spec.data.ord = spec.data.ord, display.in.diagram = display.in.diagram, display.species = display.species, display.sites = display.sites, axes.shown = axes.shown, display.EIV = display.EIV, display.header = display.header, display.envelope = display.envelope, header.name = header.name, display.header.style = display.header.style, display.spider = display.spider, display.group.center = display.group.center, bw = bw, pb = pb, ordi.type = ordi.type)



    if (!last.result$use.last.result)

    {

      last.data <- list (last.matrix.sum = sum(input.data$spec.data), last.matrix.species = colnames (input.data$spec.data), last.matrix.sites = rownames (input.data$spec.data), last.result = spec.data.ord)

      save (last.data, file = 'dca_lfa.r')

      last.data.quick <- list (type.of.analysis = 'dca', size.of.matrix = dim (input.data$spec.data), setting = list (down.rare.spec = down.rare.spec))

      save (last.data.quick, file = 'dca_lfq.r')

    }

  }

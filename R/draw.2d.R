#' Plot ordination in 2 dimensions.
#'
#' \code{draw.2d} plots the calculated ordination results in 2 dimensions, in
#' a new R window. It can be called from one of the following functions:
#' \code{\link{draw.ca}}
#' \code{\link{draw.dca}}
#' \code{\link{draw.nmds}}
#' \code{\link{draw.pca}}
#' \code{\link{draw.pcoa}}.
#' All the input arguments are provided by the JUICE software.
#'
#' @param input.data List. Generated by \code{\link{read.check.data}},
#' and containing the data output from JUICE.
#' @param spec.data.ord Object of different classes, according to the
#' chosen ordination. Contains the ordination results.
#' @param display.in.diagram What to display in diagram : 'species',
#' 'sites' or 'both'.
#' @param display.species How the species should be represented : as
#' points, with labels etc.
#' @param display.sites How the sites should be represented : as
#' points, with labels, group labels etc.
#' @param axes.shown Number of axes to plot.
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
#' @param bw Logical. Plot in black & white or in color.
#' @param pb Progress bar generated by the ordination function.
#' @param ordi.type Specifies the type of ordination that has been
#' performed on the data.
#'
#'
#' @return Invisible.
#'
#' @seealso \code{\link{draw.3d}}

#--------------------------------------------

draw.2d <-

  function (input.data,
            spec.data.ord,
            display.in.diagram,
            display.species,
            display.sites,
            axes.shown,
            display.EIV,
            display.header,
            display.envelope,
            header.name,
            display.header.style,
            display.spider,
            display.group.center,
            bw,
            pb,
            ordi.type)

  {

    display.long.header <- input.data$display.long.header

    envfit.result <- list ()

    ### writes warnings

    if (length (input.data$title.warnings) > 0) write.warnings (input.data$title.warnings)



    ## plots the basic plot


    if (display.in.diagram == "both") {

      draw.both = TRUE

      ordiplot.arg <- c("species", "sites")

    } else {

      draw.both = FALSE

      ordiplot.arg <- display.in.diagram
    }


    #Note : according to the ordination, ordiplot takes different arguments. Thus the differenciation of "ca".

    if (ordi.type == "ca" | ordi.type == "pca" | ordi.type == "pcoa") {

      pl <- ordiplot(spec.data.ord, display = ordiplot.arg, type = 'n', choices = axes.shown)

    } else {

    pl <- ordiplot(spec.data.ord, display = display.in.diagram, type = 'n', choices = axes.shown)

    }

    ## display short header data as surface

    if (display.header & (class(input.data$short.header.file$factor) == 'numeric' | class(input.data$short.header.file$factor) == 'integer'))

    {

      if (display.header.style == 'surface')

      {

        ordisurf (pl,  input.data$short.header.file$factor, add=TRUE, choices = axes.shown, labels=header.name, col = if (bw) 'grey' else 'green')

        legend ('topright', legend=header.name, text.col = if (bw) 'grey' else 'green')

      }

    }



    ### draw spider plot

    if (display.spider == TRUE)

    {

      grp <- input.data$short.header.file$group

      for (i in seq(1:max(range(grp))))

      {

        ordispider(pl, grp, show.groups=i, col='lightgrey')

      }

    }



    ### draw species and sites

    if (display.in.diagram == 'species' | draw.both == T)

    {

      if (display.species == 'points') points (pl, what = 'species', pch = '+', col = if (bw) 'grey' else 'red')

      if (display.species == 'all.labels') text (pl, what = 'species', col = if (bw) 'black' else 'red', cex = 0.8)

      if (display.species == 'some.labels') orditorp (spec.data.ord, dis = 'sp', col = if (bw) 'black' else 'red', pcol = 'gray', pch = '+', cex = 0.8)

    }



    if (display.in.diagram == 'sites' | draw.both == T)

    {

      if (display.sites == 'points') points (pl, what = 'sites', pch=21, bg = if (bw) 'black' else 'yellow', col = if (bw) 'lightgrey' else 'red', cex = .5)

      if (display.sites == 'all.labels') text (pl, what = 'sites')

      if (display.sites == 'some.labels') orditorp (spec.data.ord, dis = 'si', pcol = 'gray', choices = axes.shown)

      if (display.sites == 'groups')

      { grp <- input.data$short.header.file$group

      for (i in seq(1:max(range(grp))))

      {text (pl, what = 'si', select = (grp == i), col = if (bw) 'black' else i, labels=grp)}

      }

    }



    ## plots Ellenberg indicator values

    if (display.EIV == TRUE)

    {

      if ( sum(colSums(input.data$ellenb))!=0 )

      {

        ef <- envfit(spec.data.ord, input.data$ellenb, choices = axes.shown, perm = 499)

        plot.envfit.m(ef, lwd=1.4, cex=1, col = 'grey', length = .1)

        plot.envfit.m(ef, lwd=1.4, cex=1, p = 0.01, col = if (bw) 'black' else 'navy', length = .1)

        envfit.result <- append (envfit.result, list(ef = ef))

      }

    }



    ## plots envelopes

    if (display.envelope == TRUE)

    {

      grp <- input.data$short.header.file$group

      for (i in seq(1:max(range(grp)))) ordihull(pl, grp, show.groups=i, col = if (bw) 'grey' else i)

    }



    ## plots the group number at the center of group

    if (display.group.center == TRUE)

    {

      grp <- input.data$short.header.file$group

      for (i in seq(1:max(range(grp)))) ordicenter(pl, grp, show.groups=i, col= if (bw) 'black' else i)

    }



    ## plots short header data

    if (display.header & (class(input.data$short.header.file$factor) == 'numeric' | class(input.data$short.header.file$factor) == 'integer'))

    {

      if (display.header.style == 'arrow')

      {

        ehs <- with (input.data$short.header.file, envfit (spec.data.ord, factor, choices = axes.shown))

        plot.envfit.m (ehs, add=TRUE, col=if (bw) 'black' else 'seagreen4', labels=header.name, lwd = 1.4, length = .1)

        envfit.result <- append (envfit.result, list(ehs = ehs))

      }

    }



    ## plots long header data

    if (display.long.header)

    {

      ehl <- with (input.data$long.header.file, envfit (spec.data.ord, input.data$long.header.file, choices = axes.shown))

      plot.envfit.m (ehl, add = TRUE, col = if (bw) 'black' else 'blue', lwd = 1.4)

      envfit.result <- append (envfit.result, list(ehl = ehl))

    }



    if (!is.null (pb)) close (pb)



    library (tcltk)

    directory <- paste('2D_figures_', format (Sys.time(), "%y%m%d_%H%M%OS"), sep = '')

    base <- tktoplevel()

    tkwm.title(base, "2D")



    spec.frm <- tkframe(base,borderwidth=2)

    frame.b <- tkframe(spec.frm, relief="groove", borderwidth=2)

    frame.b2 <- tkframe(spec.frm, relief="groove", borderwidth=2)



    tkpack (tkbutton(frame.b, text = "Save as BMP", command = function() {dir.create (directory); savePlot (file = paste(directory,'/snapshot_', format (Sys.time(), "%y%m%d_%H%M"), '.bmp', sep = ''), type = 'bmp')}))

    tkpack (tkbutton(frame.b, text = "Save as JPG", command = function() {dir.create (directory); savePlot (file = paste(directory,'/snapshot_', format (Sys.time(), "%y%m%d_%H%M"), '.jpg', sep = ''), type = 'jpg')}))

    tkpack (tkbutton(frame.b, text = "Save as PDF", command = function() {dir.create (directory); savePlot (file = paste(directory,'/snapshot_', format (Sys.time(), "%y%m%d_%H%M"), '.pdf', sep = ''), type = 'pdf')}))





    tkpack(tkbutton (frame.b, text = "Show saved figure", command = function (){try(dir.create (directory)); sel.file <- choose.files(default = paste (getwd(), '/', directory, '/*.*', sep = ''), multi = FALSE); if (length (unlist(strsplit(sel.file, split = ''))) > 1) shell.exec (sel.file)}))

    tkpack (tkbutton (frame.b2, text = "Show summary", command = function (){try(shell.exec (paste (getwd(), '/ordi-result.txt', sep = '')))}))

    quit <- tkbutton(base, text = "Quit", bg = 'red', command = function() {tkdestroy(base); dev.off()})

    tkpack (frame.b, frame.b2, fill = "x")

    tkpack(spec.frm, quit)

    tkraise (base)

    bringToTop (dev.cur(), stay = F)



    ## keeps window open

    repeat  if (dev.cur () == 1) break



    ## checks once per day for new available ordijuice update

    check.update ()

  }

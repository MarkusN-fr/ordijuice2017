#' Tk Progress Bar for ordination progress.
#'
#' \code{myTkProgressBar} creates the progress bar accompanying the calculation
#'  of the ordination, and the plotting.
#'
#' @param title Title of the progress bar.
#' @param label Additional label shown in the window.
#' @param min Minimal value taken by the bar.
#' @param max Maximal value taken by the bar.
#' @param initial Initial value taken by the bar.
#' @param width Size of the window containing the progress bar.
#'
#' @return Window containing a progress bar.

#--------------------------------------------



myTkProgressBar <-

  function (title = "R progress bar", label = "", min = 0, max = 1, initial = 0, width = 300)

  {

    #  windows (xpos = 0, ypos = 0)

    #  plot.new()

    #  bringToTop (-1)

    #  dev.off(dev.cur())

    library (tcltk)

    useText <- FALSE

    have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"

    if (!have_ttk && as.character(tclRequire("PBar")) == "FALSE")

      useText <- TRUE

    .win <- tktoplevel()

    .val <- initial

    .killed <- FALSE

    tkwm.geometry(.win, sprintf("%dx80", width + 40))

    tkwm.title(.win, title)

    #fn <- tkfont.create(family = "helvetica", size = 12)

    if (useText) {

      #.lab <- tklabel(.win, text = label, font = fn, padx = 20)

      .lab <- tklabel(.win, text = label, padx = 20)

      tkpack(.lab, side = "left")

      fn2 <- tkfont.create(family = "helvetica", size = 16)

      .vlab <- tklabel(.win, text = "0%", font = fn2, padx = 20)

      tkpack(.vlab, side = "right")

      up <- function(value) {

        if (!is.finite(value) || value < min || value > max)

          return()

        .val <<- value

        tkconfigure(.vlab, text = sprintf("%d%%", round(100 *

                                                          (value - min)/(max - min))))

      }

    }

    else {

      #.lab <- tklabel(.win, text = label, font = fn, pady = 10)

      .lab <- tklabel(.win, text = label, pady = 10)

      .tkval <- tclVar(0)

      tkpack(.lab, side = "top")

      #tkpack(tklabel(.win, text = "", font = fn), side = "bottom")

      tkpack(tklabel(.win, text = ""), side = "bottom")

      pBar <- if (have_ttk)

        ttkprogressbar(.win, length = width, variable = .tkval)

      else tkwidget(.win, "ProgressBar", width = width, variable = .tkval)

      tkpack(pBar, side = "bottom")

      up <- function(value) {

        if (!is.finite(value) || value < min || value > max)

          return()

        .val <<- value

        tclvalue(.tkval) <<- 100 * (value - min)/(max - min)

      }

    }

    getVal <- function() .val

    kill <- function() if (!.killed) {

      tkdestroy(.win)

      .killed <<- TRUE

    }

    title <- function(title) tkwm.title(.win, title)

    lab <- function(label) tkconfigure(.lab, text = label)

    tkbind(.win, "<Destroy>", kill)

    up(initial)

    tkraise (.win)

    structure(list(getVal = getVal, up = up, title = title, label = lab,

                   kill = kill, window=.win), class = "tkProgressBar")

  }

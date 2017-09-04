#' Check for the correct installation of necessary packages for ordijuice.
#'
#' \code{check.install} checks if the required packages are installed, and
#' downloads the missing ones. It also checks for a new JUICE-version, as
#' well as the current version of R.
#'
#' @param display.spider Logical, provided by the JUICE software
#' @return A logical, indicating whether the user's R-version is up to date or
#' not. If the R-version is inferior to 3.2.2, ordijuice will ask the user to
#' update to the latest R-version in order to ensure full functionality.
#'
#' @seealso \code{\link{install.libraries}} \code{\link{check.update}}

#--------------------------------------------

check.install <- function (display.spider)

{

  library(tcltk)

  ## check for correct installation of all libraries

  libraries.needed <- c("permute", "lattice", "mgcv", "cluster", "nlme", "MASS", "vegan", "abind", "magic", "geometry",
                        "stringi", "yaml", "digest", "Rcpp", "evaluate", "highr", "markdown", "stringr", "httpuv",
                        "mime", "xtable", "R6", "sourcetools", "htmlwidgets", "htmltools", "knitr", "jsonlite",
                        "shiny", "magrittr", "rgl", "tkrgl", "rpanel", "scatterplot3d", "vegan3d", "ordijuice",
                        "backports", "checkmate", "viridisLite", "rematch", "minqa", "nloptr", "RcppEigen", "zoo",
                        "Formula", "latticeExtra", "gridExtra", "htmlTable", "viridis", "base64enc", "cellranger",
                        "lme4", "SparseM", "MatrixModels", "sandwich", "Hmisc", "e1071", "readxl", "pbkrtest",
                        "quantreg", "RcmdrMisc", "car", "tcltk2", "relimp", "Rcmdr", "BiodiversityR", "coda", "arm",
                        "bitops", "matrixcalc", "mi", "caTools", "rprojroot", "mvtnorm", "TH.data", "sem", "rmarkdown",
                        "nortest", "multcomp", "lmtest", "leaps", "effects", "aplpack")

  not.installed <<- libraries.needed [!libraries.needed %in% .packages(all = T)]

  if (length (not.installed) > 0)

  {

    answ <- winDialog.m(type = 'yesnocancel', message = paste('Some R libraries are not installed correctly! \nDo you want to re-install them now? \n(you need to be connected to internet)'))

    if (answ == 'YES') install.libraries (not.installed)
  }

  ## checks for the version of JUICE and asks for update if it's old

  if (is.null (display.spider)) winDialog.m(type = 'ok', message = 'To get full functionality, you need to install newer version of JUICE!')



  ## check for the version of R and ask for update if it's old

  Rver <- getRversion ()

  if (Rver < '3.2.2')
    {
    winDialog.m(type = 'ok', message = paste('Currently, you are using ', Rver, ' version of R. To ensure full functionality, please update to version 3.2.2 or newer.', sep = ''))
    return(FALSE)
    }
        #Rver >= '3.2.2'
    else {
    return(TRUE)
    }
}

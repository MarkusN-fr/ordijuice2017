
#' Install missing packages.
#'
#' \code{install.libraries} downloads and installs the missing packages.
#'
#' @param libs Vector from \code{\link{check.install}}, containing the names
#' of the missing packages.
#' @return Invisible.
#'
#' @seealso \code{\link{check.install}}

#--------------------------------------------


install.libraries <- function  (libs)

{

  bar <- winProgressBar("Updating R packages", "Downloading", 0, length(not.installed), 0, 500)

  prog <- 0

  for (i in 1:length(not.installed)) {

    setWinProgressBar(bar, label=paste("Downloading ", not.installed[i], "   (", i,"/", length(not.installed), ")", sep=""), value=prog)

    prog <- i

    install.packages (not.installed[i], repos="https://cloud.r-project.org/", dependencies=F)

    if (not.installed[i] == 'ordijuice') {detach (package:ordijuice); install.packages ('ordijuice', contriburl='http://sci.muni.cz/botany/zeleny/R/windows/contrib/R-2.8.1')}

  }

  setWinProgressBar(bar, label="Download successful", value=length(not.installed))

  Sys.sleep(1)

  close(bar)

}

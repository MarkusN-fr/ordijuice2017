#' Dialog window for Windows.
#'
#' \code{winDialog.m} is a modified version of the function
#' \code{utils::winDialog}. The modification ensures usability in a
#' non-interactive session.
#'
#'
#' @seealso \code{\link[utils]{winDialog}}

#--------------------------------------------


winDialog.m <- function (type = c("ok", "okcancel", "yesno", "yesnocancel"),
                          message)
{
  #if (!interactive())
  #  stop("winDialog() cannot be used non-interactively")
  type <- match.arg(type)
  res <- .External2(utils:::C_winDialog, type, message)
  if (res == 10L)
    return(invisible(NULL))
  c("NO", "CANCEL", "YES", "OK")[res + 2L]
}

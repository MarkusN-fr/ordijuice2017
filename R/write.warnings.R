#' Show error/warning messages.
#'
#' \code{write.warnings} collects and shows error/warnings that occured during
#' the execution of an ordijuice script.


#--------------------------------------------


write.warnings <-

  function (title.warnings)

  {

    temp.warnings <- NULL

    for (i in seq (1,length (title.warnings))) temp.warnings <- append (temp.warnings, paste (title.warnings[i], '\n', sep = ''))

    winDialog.m ('ok', paste(temp.warnings, collapse = ''))
  }



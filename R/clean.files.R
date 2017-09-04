#' Delete temporary files created by JUICE and ordijuice.
#'
#' \code{clean.files} deletes the output-files from JUICE and ordijuice in the
#' working directory of R. The function is called directly from JUICE's
#' interface
#'
#' @export

#--------------------------------------------


clean.files <-

  function ()

  {

    files.to.delete <- c('JUICE2.txt.Rout', 'result.txt', 'ordi-result.txt', 'JUICE2.txt', 'sh_expo.txt', 'specdata.txt', 'dca_lfa.r', 'dca_lfq.r', 'pca_lfa.r', 'pca_lfq.r', 'nmds_lfa.r', 'nmds_lfq.r', 'updat.R', 'updtchck.r', 'clean.R')

    deleted.files <- files.to.delete [files.to.delete %in% list.files ()]

    try (file.remove (deleted.files))



    del.dir1 <- list.files (pattern = '2D_figures_')

    try (unlink (del.dir1, recursive = T))

    del.dir2 <- list.files (pattern = '3D_snapshots_')

    try (unlink (del.dir2, recursive = T))

    if (length (deleted.files) > 0 || length (del.dir1) > 0 || length (del.dir2) > 0)

      winDialog.m ('ok', message = paste(c('These items have been deleted: \n\n files:\n', deleted.files, '\n\ndirectories:\n', c (del.dir1, del.dir2)), sep = '', collapse = ' '))

  }

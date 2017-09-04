#' Determines if current data is identical to previously used data.
#'
#' \code{use.last} determines whether the imported data from JUICE has already
#' been used for the previous ordination, in order to save time.


#--------------------------------------------


use.last <-

  function (input.data, type.of.analysis, setting)

  {

    use.last.result <- FALSE

    if(all(c(paste (type.of.analysis, '_lfa.r', sep = ''), paste (type.of.analysis, '_lfq.r', sep = '')) %in% dir ()))

    {

      try (load (paste (type.of.analysis, '_lfq.r', sep = '')))

      if (last.data.quick$type.of.analysis == type.of.analysis & all(last.data.quick$size.of.matrix == dim(input.data$spec.data)) & last.data.quick$setting[[1]] == setting[[1]])

      {

        try (load (paste (type.of.analysis, '_lfa.r', sep = '')))

        if (last.data$last.matrix.sum == sum(input.data$spec.data) & all(last.data$last.matrix.species == colnames (input.data$spec.data)) & all(last.data$last.matrix.sites == rownames (input.data$spec.data)))

          use.last.result <- TRUE

      }

    }



    if(use.last.result)

      list (use.last.result = use.last.result, last.data.result = last.data$last.result) else

        list (use.last.result = use.last.result, last.data.result = NULL)

  }

#' Save the ordination result in a file.
#'
#' \code{save.ord.result} saves the ordination results in a .txt file in
#' the working R directory.


#--------------------------------------------


save.ord.result <-

  function (ord.result, use.last.result, type, deleted.plots)

  {
    save(ord.result, file="DEBUG.R")

    if (type == 'dca')

    {

      dca <- ord.result

      cat ('Results of Dentrended correspondence analysis\n\n', sep = '\t', file = 'ordi-result.txt')

      if (use.last.result) cat ('PREVIOUSLY SAVED RESULTS USED FOR VISUALIZATION (e.g. as the data are the same as in previous ', type, ' analysis, also the results of previous analysis are used for visualization; this is intendet to save some time for calculation ...)\n\n', append = T, file = 'ordi-result.txt')

      cat('           ','DCA1  ', 'DCA2  ','DCA3  ', 'DCA4  ', '\n', sep = '\t', append = T, file = 'ordi-result.txt')

      cat('Eigenvalues', formatC(dca$evals, digits = 4, format = 'f'), '\n', sep = '\t', append = T, file = 'ordi-result.txt')

      cat('Decorana values', formatC (dca$evals.decorana, digits = 4, format = 'f'),'\n', sep = '\t', append = T, file = 'ordi-result.txt')

      cat('Axis length', formatC (apply(dca$rproj, 2, max), digits = 4, format = 'f'), '\n\n', sep = '\t', append = T, file = 'ordi-result.txt')

      cat('There are two versions of eigenvalues - the first (Eigenvalues) are the values calculated by the function "decorana" in vegan, the second (Decorana values) are the values calculated by the original DECORANA program of Mark Hill - these are included only for reference and should not be used. For more details see manual of vegan.\n\n', sep = '\t', append = T, file = 'ordi-result.txt')



      cat('Species scores\n\n', sep = '\t', append = T, file = 'ordi-result.txt')



      cat('        ','DCA1  ', 'DCA2  ','DCA3  ', 'DCA4  ', '\n', sep = '\t', append = T, file = 'ordi-result.txt')

      format.cproj <- formatC (dca$cproj, digits = 4, format = 'f')

      for (r in seq (1, dim (dca$cproj)[1]))

        cat (rownames (dca$cproj)[r], format.cproj[r,], '\n', sep = '\t', append = T, file = 'ordi-result.txt')



      cat('\nSite scores\n\n', sep = '\t', append = T, file = 'ordi-result.txt')

      cat('  ','DCA1  ', 'DCA2  ','DCA3  ', 'DCA4  ', '\n', sep = '\t', append = T, file = 'ordi-result.txt')

      format.rproj <- formatC (dca$rproj, digits = 4, format = 'f')

      for (r in seq (1, dim (dca$rproj)[1]))

        cat (formatC(rownames (dca$rproj)[r], format = 'd'), format.rproj[r,], '\n', sep = '\t', append = T, file = 'ordi-result.txt')



      if (!is.null (deleted.plots))

      {

        cat('\nPlots deleted due to incomplete header data:\n\n', sep = '\t', append = T, file = 'ordi-result.txt')

        cat(deleted.plots, append = T, file = 'ordi-result.txt')

      }

    }

    if (type == 'nmds')

    {

      nmds <- ord.result

      cat ('Results of Nonmetric Multidimensional Scaling using isoMDS (MASS package)\n\n', sep = '\t', file = 'ordi-result.txt')

      if (use.last.result) cat ('PREVIOUSLY SAVED RESULTS USED FOR VISUALIZATION (e.g. as the data are the same as in previous ', type, ' analysis, also the results of previous analysis are used for visualization; this is intended to save some time for calculation ...)\n\n', append = T, file = 'ordi-result.txt')



      cat("Distance:", nmds$distance, "\n", append = T, file = 'ordi-result.txt')

      cat("Dimensions:", nmds$dims, "\n", append = T, file = 'ordi-result.txt')

      cat("Stress:    ", nmds$stress, "\n", append = T, file = 'ordi-result.txt')

      if (nmds$converged) cat("Two convergent solutions found after", nmds$tries,

                              "tries\n", append = T, file = 'ordi-result.txt') else cat("No convergent solutions - best solution after", nmds$tries, "tries\n", append = T, file = 'ordi-result.txt')



      cat('\nSpecies scores\n\n', sep = '\t', append = T, file = 'ordi-result.txt')

      if (nmds$ndim == 2)  cat('        ','NMDS1 ', 'NMDS2 ', '\n', sep = '\t', append = T, file = 'ordi-result.txt') else

        cat('        ','NMDS1 ', 'NMDS2 ', 'NMDS3 ', '\n', sep = '\t', append = T, file = 'ordi-result.txt')

      format.cproj <- formatC (nmds$species, digits = 4, format = 'f')

      for (r in seq (1, dim (nmds$species)[1])) cat (rownames (nmds$species)[r], format.cproj[r,], '\n', sep = '\t', append = T, file = 'ordi-result.txt')



      cat('\nSite scores\n\n', sep = '\t', append = T, file = 'ordi-result.txt')

      if (nmds$ndim == 2)  cat('       ','NMDS1 ', 'NMDS2 ', '\n', sep = '\t', append = T, file = 'ordi-result.txt') else

        cat('    ','NMDS1 ', 'NMDS2 ', 'NMDS3 ', '\n', sep = '\t', append = T, file = 'ordi-result.txt')

      format.rproj <- formatC (nmds$points, digits = 4, format = 'f')

      for (r in seq (1, dim (nmds$points)[1])) cat (rownames (nmds$points)[r], format.rproj[r,], '\n', sep = '\t', append = T, file = 'ordi-result.txt')



      if (!is.null (deleted.plots))

      {

        cat('\nPlots deleted due to incomplete header data:\n\n', sep = '\t', append = T, file = 'ordi-result.txt')

        cat(deleted.plots, append = T, file = 'ordi-result.txt')

      }

    }



    if (type == 'pca')

    {

      pca <- ord.result

      cat ('Results of Principal Components Analysis\n\n', sep = '\t', file = 'ordi-result.txt')

      if (use.last.result) cat ('PREVIOUSLY SAVED RESULTS USED FOR VISUALIZATION (e.g. as the data are the same as in previous ', type, ' analysis, also the results of previous analysis are used for visualization; this is intended to save some time for calculation ...)\n\n', append = T, file = 'ordi-result.txt')

      cat('                  ','PC1   ', 'PC2   ', 'PC3   ', 'PC4   ', '\n', sep = '\t', append = T, file = 'ordi-result.txt')

      cat("Standard deviation:", formatC (pca$sdev[1:4], digits = 4, format = 'f'), "\n", sep = '\t', append = T, file = 'ordi-result.txt')



      cat('\nSpecies scores\n\n', sep = '\t', append = T, file = 'ordi-result.txt')

      cat('        ','PC1 ', 'PC2 ', 'PC3', 'PC4', '\n', sep = '\t', append = T, file = 'ordi-result.txt')

      format.cproj <- formatC (pca$rotation[,1:4], digits = 4, format = 'f')

      for (r in seq (1, dim (format.cproj)[1])) cat (rownames (format.cproj)[r], format.cproj[r,], '\n', sep = '\t', append = T, file = 'ordi-result.txt')



      cat('\nSite scores\n\n', sep = '\t', append = T, file = 'ordi-result.txt')

      cat('      ','PC1   ', 'PC2   ', 'PC3   ', 'PC4   ', '\n', sep = '\t', append = T, file = 'ordi-result.txt')

      format.rproj <- formatC (scores (pca, display = 'sites')[,1:4], digits = 4, format = 'f')

      for (r in seq (1, dim (format.rproj)[1])) cat (rownames (format.rproj)[r], format.rproj[r,], '\n', sep = '\t', append = T, file = 'ordi-result.txt')



      if (!is.null (deleted.plots))

      {

        cat('\nPlots deleted due to incomplete header data:\n\n', sep = '\t', append = T, file = 'ordi-result.txt')

        cat(deleted.plots, append = T, file = 'ordi-result.txt')

      }

    }

    ######PCoA

    if (type == 'pcoa')

    {

      pcoa <- ord.result

      cat ('Results of Principal Coordinates Analysis\n\n', sep = '\t', file = 'ordi-result.txt')

      if (use.last.result) cat ('PREVIOUSLY SAVED RESULTS USED FOR VISUALIZATION (e.g. as the data are the same as in previous ', type, ' analysis, also the results of previous analysis are used for visualization; this is intended to save some time for calculation ...)\n\n', append = T, file = 'ordi-result.txt')

      cat('                  ','PCo1   ', 'PCo2   ', 'PCo3   ', 'PCo4   ', '\n', sep = '\t', append = T, file = 'ordi-result.txt')

      cat("Eigenvalues", formatC (pcoa$eig[1:4], digits = 4, format = 'f'), "\n", sep = '\t', append = T, file = 'ordi-result.txt')



      #cat('\nSpecies scores\n\n', sep = '\t', append = T, file = 'ordi-result.txt')

      #cat('        ','PCo1 ', 'PCo2 ', 'PCo3', 'PCo4', '\n', sep = '\t', append = T, file = 'ordi-result.txt')

      #format.cproj <- formatC (pcoa$rotation[,1:4], digits = 4, format = 'f')

      #for (r in seq (1, dim (format.cproj)[1])) cat (rownames (format.cproj)[r], format.cproj[r,], '\n', sep = '\t', append = T, file = 'ordi-result.txt')



      #cat('\nSite scores\n\n', sep = '\t', append = T, file = 'ordi-result.txt')

      #cat('      ','PCo1   ', 'PCo2   ', 'PCo3   ', 'PCo4   ', '\n', sep = '\t', append = T, file = 'ordi-result.txt')

      #format.rproj <- formatC (scores (pcoa, display = 'sites')[,1:4], digits = 4, format = 'f')

      #for (r in seq (1, dim (format.rproj)[1])) cat (rownames (format.rproj)[r], format.rproj[r,], '\n', sep = '\t', append = T, file = 'ordi-result.txt')



      if (!is.null (deleted.plots))

      {

        cat('\nPlots deleted due to incomplete header data:\n\n', sep = '\t', append = T, file = 'ordi-result.txt')

        cat(deleted.plots, append = T, file = 'ordi-result.txt')

      }

    }

  }

#' Reads data output from JUICE.
#'
#' \code{read.check.data} reads the data output from JUICE, and adapts it for
#' the other ordination-related functions contained in ordijuice. It also
#' checks for errors in the data, and transforms it according to the value of
#' the input arguments.
#'
#' @param display.sites How the sites should be represented : as
#' points, with labels, group labels etc.
#' @param display.EIV Logical. If Ellenberg's Indicator Values should
#' be added to the plot.
#' @param display.header Logical. If header-data from JUICE should be
#' plotted as well.
#' @param display.envelope Logical. If the groups should have envelopes.
#' @param display.spider Logical. If the groups should have a spiderplot
#'  within.
#' @param display.group.center Logical. If the groups should have
#' centroids.
#' @param hellinger.trans Logical. If Hellinger transformation should be
#'  performed on the data. Warning: hellinger transformation only makes sense
#'  with PCA and RDA.
#'
#'
#' @seealso
#' \code{\link{draw.ca}}
#' \code{\link{draw.dca}}
#' \code{\link{draw.nmds}}
#' \code{\link{draw.pca}}
#' \code{\link{draw.pcoa}}
#' \code{\link{draw.2d}}
#' \code{\link{draw.3d}}


#--------------------------------------------


read.check.data <-

function (

                     display.sites,

                     display.EIV = FALSE,

                     display.header = FALSE,

                     display.envelope = FALSE,

                     display.spider = FALSE,

                     display.group.center = FALSE,

                     hellinger.trans = FALSE)

{

### reads species data

  spec.data <- read.table (file='specdata.txt', sep=';', header=T, row.names=NULL)

  main.label <- NULL

  spec.data.truefalse1 <- rep(FALSE, nrow(spec.data))

  spec.data.truefalse2 <- rep(FALSE, nrow(spec.data))

  spec.data.truefalse3 <- rep(FALSE, nrow(spec.data))

  title.warnings <- NULL

  ellenb <- NULL

  short.header.file <- NULL

  long.header.file <- NULL

  deleted.plots <- NULL

  spec.data <- spec.data[order(spec.data[,1]),]

  difference <- c(FALSE, diff(spec.data[,1])==0)

  if (sum(difference)>0) title.warnings <- append(title.warnings, 'Two or more plots have the same number - only one of them is used!')

  spec.data <- spec.data[!difference,]

  row.names(spec.data) <- spec.data[,1]

  spec.data <- spec.data[,2:length(names(spec.data))]



### reads file with header data

  header.file <- try(read.table (file='sh_expo.txt', head=T, sep=';', fill=TRUE, row.names=NULL))

  value.type <- as.numeric(as.matrix(header.file[1,]))

  header.file <- header.file[-1,]



if (any(names (header.file) == 'Locality'))

  {

  title.warnings <- append (title.warnings, 'Locality cannot be selected as environmental variable!')

  write.warnings (title.warnings)

  }





   should.be.cont <- NULL

   if (class (header.file) != 'try-error')

   {

   display.long.header <- ifelse (dim (header.file)[2] > 5, TRUE, FALSE)

   if (display.long.header)

   {

    for (i in seq (6, length (value.type)))

      {

      if (is.factor (header.file[,i]) & value.type[i] == 1) should.be.cont <- append (should.be.cont, names (header.file)[i])

      if (!is.factor(header.file[,i]) & value.type[i] == 2) header.file[,i] <- as.factor (header.file[,i])

      }

      if (!is.null(should.be.cont)) title.warnings <- append (title.warnings, paste('Variable',should.be.cont, 'has been changed into factor.', sep = ' '))

    }

   header.file <- header.file[order(header.file[,1]),]

   header.file <- header.file[!difference,]

   short.header.file <- header.file[,1:3]

   names (short.header.file) <- c('rel.no', 'factor', 'group')

   if (display.long.header) long.header.file <- header.file[,6:dim (header.file)[2], drop = F]

   } else

    {

    header.file <- NULL

    display.long.header <- FALSE

    title.warnings <- append(title.warnings, 'File with header data is missing or corrupted!')

   }



# warnings about data quality and missing values

if (sum(rowSums(spec.data)==0)>0)

  {

  title.warnings <- append(title.warnings, 'Plot(s) with no species have been omitted!')

  spec.data.truefalse1 <- rowSums(spec.data)==0

    }



if (sum(colSums(spec.data)==0)>0)

  {

  spec.data <- spec.data[,colSums(spec.data)!=0]

  }

if (display.header)

  {

   if (sum(is.na(short.header.file$factor))>0)

    {

    spec.data.truefalse2 <- is.na(header.file[,2])

    title.warnings <- append(title.warnings, paste (sum (spec.data.truefalse2), ' plot', if (sum(spec.data.truefalse2) > 1) 's', ' with missing short header data ', if (sum(spec.data.truefalse2) > 1) 'have' else 'has', ' been omitted!', sep = ''))

    }

  if (class(short.header.file$factor)!='numeric' & class(short.header.file$factor)!='integer' & !is.null (short.header.file))

    {

    title.warnings <- append(title.warnings, 'Short header data contain non-numeric values!')

    }

  }

#---------
   #Hellinger Transformation

if (hellinger.trans) {

  library(vegan)

  spec.data <- decostand(spec.data, method="hellinger")

}


#-------
if (display.long.header)

  {

  is.na.long.header <- is.na (long.header.file)

  missing.value.long.header <- long.header.file == "          "

  missing.values.merged <- rowSums(is.na.long.header|missing.value.long.header, na.rm = T) > 0

  if (sum (missing.values.merged)>0)

    {

    title.warnings <- append(title.warnings, paste (sum (missing.values.merged), ' plot', if (sum(missing.values.merged) > 1) 's', ' with missing header data ', if (sum(missing.values.merged) > 1) 'have' else 'has', ' been omitted!', sep = ''))

    spec.data.truefalse3 <- missing.values.merged

    }

  }



if (display.EIV)

  {

  ellenb <- read.table ("ecol.txt", sep=';', header=TRUE, row.names=1)

  ellenb <- ellenb[, c(3, 5, 7, 9, 11, 13)]

  if (sum(colSums(ellenb))==0) title.warnings <- append(title.warnings, 'You have not initialized Ellenberg indicator values!') else

    {

    ellenb <- ellenb[order(as.numeric(row.names(ellenb))),]

    ellenb <- ellenb[!difference,]

    names(ellenb) <- c ('LIGHT', 'TEMP', 'CONT', 'MOIST', 'REACT', 'NUTR')

    }

  }



if (sum(spec.data.truefalse1)>0 | sum(spec.data.truefalse2)>0 | sum (spec.data.truefalse3)>0)

  {

  complete.header.data <- (spec.data.truefalse1+spec.data.truefalse2+spec.data.truefalse3)==0

  deleted.plots <- rownames (spec.data)[!complete.header.data]

  spec.data <- spec.data[complete.header.data,]

  short.header.file <- short.header.file [complete.header.data,]

  if (display.long.header) long.header.file <- long.header.file [complete.header.data,, drop = F]

  if (display.EIV) ellenb <- ellenb [complete.header.data,,drop = F]

  }

input.data <- list(spec.data = spec.data, short.header.file = short.header.file, long.header.file = long.header.file, ellenb = ellenb, title.warnings = title.warnings, display.long.header = display.long.header, deleted.plots = deleted.plots)

input.data

}


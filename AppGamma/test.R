while(length(taxRank) > 0)
{
  # save the amount of entries within the taxRank object as an integer within the
  # tRlength object
  tRlength <- length(taxRank)

  temps3 <- NULL

  tempdf <- plyr::ddply(taxaAbund, taxRank, plyr::numcolwise(sum))
  # create empty two column dataframe for the Names and Long_Names
  name <- data.frame(Name = as.character(),
                     Long_Name = as.character())
  # start a for loop to iterate through the rows within the tempdf dataframe object
  # it iterates starting from 1 with the variable dfrow
  for (dfrow in 1:nrow(tempdf)) {
    # if clause to detect if current row (dfrow) of the first column (1) of the
    # name dataframe object is NA
    if (is.na(name[dfrow, 1]))
    {
      # if clause to detect if the current row (dfrow) of the current column
      # (tRlength) of the tempdf dataframe object is NA
      # the check for the NA needs to be the first check as the string detection
      # that is used in the later statements would return NA in the case of an NA
      # entry and not TRUE/FALSE
      if (is.na(tempdf[dfrow, tRlength]))
      {
        # start of for loop to iterate from 0 to penultimate value of tRlength
        # using the TR variable
        for (TR in 0:(tRlength - 1))
        {
          # start while loop under the condition that the current row (dfrow) of the
          # first column (1) of the name dataframe object is NA
          if (is.na(name[dfrow, 1]))
          {
            # if clause checking the current row (dfrow) of the columns that come
            # before the current column (tRlength - TR) for their content - checks
            # if the content is neither NA nor contains "unknown" nor "incertae"
            if (!is.na(tempdf[dfrow, tRlength - TR]) &
                !stringr::str_detect(tempdf[dfrow, tRlength - TR],
                                     stringr::fixed("unknown",
                                                    ignore_case = T)) &
                !stringr::str_detect(tempdf[dfrow, tRlength - TR],
                                     stringr::fixed("incertae",
                                                    ignore_case = T)) )
            {
              # if the above stated if clauses are all TRUE the current row (dfrow)
              # of the first column (1) of the name dataframe object will be
              # assigned a string stating that the current taxonomic rank is NA
              # and what taxonomic rank is known
              name[dfrow, 1] <- paste0("NA_",
                                       taxRank[tRlength],
                                       "_(",
                                       taxRank[tRlength - TR],
                                       " = ",
                                       tempdf[dfrow, tRlength - TR],
                                       ")")
            }
            # if clause that only gets checked if at least one of the conditions of
            # the previous if clause resulted in a FALSE - this if clause has the
            # the condition is that tRlength - TR == 1 meaning which results in all
            # columns of the tempdf dataframe being checked and none containing an
            # entry that does not have the "unknown" or the "incertae" string within
            # them
            else if
            (
              tRlength - TR == 1
            )
            {
              # if the above if clause is fulfilled and thus no column of the current
              # row (dfrow) of the tempdf dataframe object contains a viable entry
              # then the first column of the current row (dfrow) of the name
              # dataframe object will be assigned a string containing that the
              # current taxonimic rank as well as the highest taxonomic rank are
              # NA
              name[dfrow, 1] <- paste0("NA_",
                                       taxRank[tRlength],
                                       "_(",
                                       taxRank[tRlength - TR],
                                       " = NA)")
            } # end of the if and else if statements checking the columns of the
            # tempdf object for their contents
          }# end of the while loop conditioned on the fact that current row (dfrow)
          # of the first column (1 - the name column) of the name dataframe object
          # is NA
        }# end of for loop iterating with TR from 0 to tRlength
      }# end of if to detect NA in current row of tempdf

      # if clause to detect if the current row (dfrow) of the current column
      # (tRlength) of the tempdf dataframe object contains the "unknown" string
      else if
      (
        stringr::str_detect(tempdf[dfrow, tRlength],
                              stringr::fixed("unknown",
                                             ignore_case = T))
      )
      {
        # start of for loop to iterate from 0 to penultimate value of tRlength
        # using the TR variable
        for (TR in 0:(tRlength - 1))
        {
          # start while loop under the condition that the current row (dfrow) of the
          # first column (1) of the name dataframe object is NA
          if (is.na(name[dfrow, 1]))
          {
            # if clause checking the current row (dfrow) of the columns that come
            # before the current column (tRlength - TR) for their content - checks
            # if the content is neither NA nor contains "unknown" nor "incertae"
            if (!is.na(tempdf[dfrow, tRlength - TR]) &
                !stringr::str_detect(tempdf[dfrow, tRlength - TR],
                                     stringr::fixed("unknown",
                                                    ignore_case = T)) &
                !stringr::str_detect(tempdf[dfrow, tRlength - TR],
                                     stringr::fixed("incertae",
                                                    ignore_case = T)) )
            {
              # if the above stated if clauses are all TRUE the current row (dfrow)
              # of the first column (1) of the name dataframe object will be
              # assigned a string stating that the current taxonomic rank is unknown
              # and what taxonomic rank is known
              name[dfrow, 1] <- paste0("Unknown_",
                                       taxRank[tRlength],
                                       "_(",
                                       taxRank[tRlength - TR],
                                       " = ",
                                       tempdf[dfrow, tRlength - TR],
                                       ")")
            }
            # if clause that only gets checked if at least one of the conditions of
            # the previous if clause resulted in a FALSE - this if clause has the
            # the condition is that tRlength - TR == 1 meaning which results in all
            # columns of the tempdf dataframe being checked and none containing an
            # entry that does not have the "unknown" or the "incertae" string within
            # them
            else if
            (
             tRlength - TR == 1
            )
            {
              # if the above if clause is fulfilled and thus no column of the current
              # row (dfrow) of the tempdf dataframe object contains a viable entry
              # then the first column of the current row (dfrow) of the name
              # dataframe object will be assigned a string containing that the
              # current taxonimic rank as well as the highest taxonimic rank are
              # unknown
              name[dfrow, 1] <- paste0("Unknown_",
                                       taxRank[tRlength],
                                       "_(",
                                       taxRank[tRlength - TR],
                                       " = Unknown)")
            } # end of the if and else if statements checking the columns of the
              # tempdf object for their contents
          }# end of the while loop conditioned on the fact that current row (dfrow)
           # of the first column (1 - the name column) of the name dataframe object
           # is NA
        }# end of for loop iterating with TR from 0 to tRlength
      }# end of if to detect "unknown" string

      # if clause that gets checked only if the above if statement checking for the
      # unknown string returns a false
      # this if clause checks if the current row (dfrow) of the current column
      # (tRlenght) of the tempdf dataframe object contains the "incertae" string
      else if
      (
        stringr::str_detect(tempdf[dfrow, tRlength],
                                   stringr::fixed("incertae",
                                                  ignore_case = T))
      )
      {
        # start of for loop to iterate from 0 to penultimate value of tRlength
        # using the TR variable
        for (TR in 0:(tRlength - 1))
        {
          # start while loop under the condition that the current row (dfrow) of the
          # first column (1) of the name dataframe object is NA
          if (is.na(name[dfrow, 1]))
          {
            # if clause checking the current row (dfrow) of the columns that come
            # before the current column (tRlength - TR) for their content - checks
            # if the content is neither NA nor contains "unknown" nor "incertae"
            if (!is.na(tempdf[dfrow, tRlength - TR]) &
                !stringr::str_detect(tempdf[dfrow, tRlength - TR],
                                     stringr::fixed("unknown",
                                                    ignore_case = T)) &
                !stringr::str_detect(tempdf[dfrow, tRlength - TR],
                                     stringr::fixed("incertae",
                                                    ignore_case = T)) )
            {
              # if the above stated if clauses are all TRUE the current row (dfrow)
              # of the first column (1) of the name dataframe object will be
              # assigned a string stating that the current taxonomic rank is
              # incertae and what taxonomic rank is known (certain)
              name[dfrow, 1] <- paste0("incertae_",
                                       taxRank[tRlength],
                                       "_(",
                                       taxRank[tRlength - TR],
                                       " = ",
                                       tempdf[dfrow, tRlength - TR],
                                       ")")
            }
            # if clause that only gets checked if at least one of the conditions of
            # the previous if clause resulted in a FALSE - this if clause has the
            # the condition is that tRlength - TR == 1 meaning which results in all
            # columns of the tempdf dataframe being checked and none containing an
            # entry that does not have the "unknown" or the "incertae" string within
            # them
            else if
            (
              tRlength - TR == 1
            )
            {
              # it the above if clause is fulfilled and thus no column of the current
              # row (dfrow) of the tempdf dataframe object contains a viable entry
              # then the first column of the current row (dfrow) of the name
              # dataframe object will be assigned a string containing that the
              # current taxonimic rank as well as the highest taxonimic rank are
              # unknown/incertae/uncertain
              name[dfrow, 1] <- paste0("incertae_",
                                       taxRank[tRlength],
                                       "_(",
                                       taxRank[tRlength - TR],
                                       " = Unknown)")
            } # end of the if and else if statements checking the columns of the
              # tempdf object for their contents
          } # end of the while loop conditioned on the fact that current row (dfrow)
            # of the first column (1 - the name column) of the name dataframe object
            # is NA -
        }# end of for loop iterating with TR from 0 to tRlength
      }# end of if to detect "incertae" string

      # else statement that gets executed if all the previous if statements of this
      # magnitude returned FALSE (meaning that current taxonomic rank (tRlength) of
      # the current row (dfrow) of the tempdf dataframe object is available and does
      # neither contains the "unknown" nor the "incertae string")
      else
      {
        # assign the contents of the current row of the current taxonomic rank of the
        # tempdf dataframe object as the contents of the current row of the first
        # column of the name dataframe object
        name[dfrow, 1] <- tempdf[dfrow, tRlength]
      } # end of if and else statements checking the content of current row of the
        # tempdf dataframe object
    }# end of if the current row in name object is NA

    # assignment of the Long_Name column within the name object:

    # iterates with the variable LongNameTaxRank from 1 to the length of the current
    # taxRank (tRlength)
    for (LongNameTaxRank in 1:tRlength) {
      # check if the current row (dfrow) has an entry in the Long_name column
      # (col 2) is NA
      if (is.na(name[dfrow, 2]))
      {
        # if statement checking the columns of current row (dfrow) of the tempdf
        # dataframe object - starting with the first column (utilizing the LongNameTaxRank
        # variable) - checking the column entries for NOT being NA
        if (!is.na(tempdf[dfrow, LongNameTaxRank]))
        {
          # assign the contents of the tempdf column and row as the contents of the
          # name row and column 2
          name[dfrow, 2] <- tempdf[dfrow, LongNameTaxRank]
        }
        # else statement - will only be executed if the above if statement returned
        # FALSE
        else
        {
          # assign "NA_(current taxonomic rank[LongNameTaxRank])" as the content of the current
          # row of the secondcolumn of the name dataframe object
          name[dfrow, 2] <- paste0("NA_(",
                                   taxRank[LongNameTaxRank],
                                   ")")
        } # end of else/if statements checking the contents of the current row
          # (dfrow) of the tempdf dataframe object

      } # end of if checking that the current row of the second column of the name
        # dataframe object is NA
      # else statement - will only be executed if the above if statement returned
      # FALSE
      else
      {
        # if statement checking the columns of current row (dfrow) of the tempdf
        # dataframe object - starting with the first column (utilizing the LongNameTaxRank
        # variable) - checking the column entries for being NA
        if (is.na(tempdf[dfrow, LongNameTaxRank]))
        {
          # append "_NA" to the entry of the current row of the second column of the
          # name dataframe object
          name[dfrow, 2] <- paste0(name[dfrow, 2], "_NA")
        }
        # else statement - will only be executed if the above if statement returned
        # FALSE
        else
        {
          # append the entry of the current row (dfrow) of the current column (LongNameTaxRank)
          # of the tempdf dataframe object to the entry of the current row (dfrow)
          # of the second column of the name dataframe object
          name[dfrow, 2] <- paste0(name[dfrow, 2], "_", tempdf[dfrow, LongNameTaxRank])
        }
      } # and of else/if statements checking if the current row (dfrow) of the
        # second column of the name dataframe object is NA

    } # end of for loop iterating from 1 to the length of the taxRank object


  } # end of for loop iterating through the amount of rows of the tempdf object
    # using the dfrow variable

  # bind the created elements within the name object onto the temporary df
  tempdf <- cbind(tempdf,
                  name)
  # rearranges the temporaray df to display all character columns before the
  # numeric columns
  tempdf <- tempdf[, c(names(dplyr::select_if(tempdf, is.character)),
                       names(dplyr::select_if(tempdf, is.numeric)))]

  # detect and assign duplicates within the Name column
  duplicate_names <- tempdf$Name[duplicated(tempdf$Name)]

  # if there are duplicates the first occasion of it will be appended an asteriks
  while(length(duplicate_names) > 0) {
    for (i in 1:nrow(tempdf)){
      if (tempdf$Name[i] %in% duplicate_names) {
        tempdf$Name[i] <- paste0(tempdf$Name[i], "*")
        duplicate_names <- tempdf$Name[duplicated(tempdf$Name)]
        break
      }

    }
  }

  # assign the element withing the Name column as the respective row name
  row.names(tempdf) <- tempdf$Name

  # create column that contains for each row the sum of all numeric elements
  tempdf$Total <- rowSums(dplyr::select_if(tempdf, is.numeric))

  # create a secondary temporary dataframe for the relative abundances
  temprela <- tempdf[, names(dplyr::select_if(tempdf, is.numeric))]
  for (i in 1:ncol(temprela)) {
    temprela[,i] <- temprela[,i] * 100 / sum(temprela[,i])
  }
  temprela <- cbind(tempdf[names(dplyr::select_if(tempdf, is.character))],
                    temprela)

  # create element contain the the rownames sorted by Abundance for highest
  # to lowest
  tNames <- row.names(tempdf[base::order(tempdf$Total, decreasing = T),])

  # assign the dataframe and the sorted name element to a temporary S3 object
  temps3$Abundance <- tempdf
  temps3$relativeAbundance <- temprela
  temps3$Sorted_Names <- tNames


  # calculate cooccurance for all taxonomic ranks below the highest rank
  if (tRlength > 1)
  {
    # create a table containing just the information if the current taxonomic
    # rank exists within the samples
    tempOcc <- dplyr::select_if(dplyr::select(tempdf,
                                              -Total),is.numeric)
    tempOcc[tempOcc > 0] <- 1
    tempOcc <- cbind(dplyr::select(tempdf, where(is.character)), tempOcc)

    # assign the created dataframe as the occurance subobject within the temporary
    # S3 object (temps3)
    temps3$Occurance <- tempOcc

    message(paste0("Calculating Co-Occurance of ",
                   taxRank[tRlength]))
    # calculates Co-Occurance using the Cooccur package
    temps3$Co_Occurance <- cooccur::cooccur(dplyr::select(tempOcc,
                                                          where(is.numeric)),
                                            spp_names = TRUE)

    rm(tempOcc)
  }



  # create Tidy table with the Samples as names
  temps3$Tidy <- tidyr::pivot_longer(tempdf,
                                     c(colnames(dplyr::select_if(tempdf,
                                                                 where(
                                                                   is.numeric)
                                     )
                                     )),
                                     names_to = "Sample",
                                     values_to = "Abundance")

  # assign the temporary S3 object to the current taxonomic rank
  assign(taxRank[tRlength], temps3)

  # save an xls table with the abundances
  xlsx::write.xlsx(temps3$Abundance,
                   paste0(tabs,
                          "/Abundance.xls"),
                   sheetName = taxRank[tRlength],
                   append = T)
  xlsx::write.xlsx(temps3$relativeAbundance,
                   paste0(tabs,
                          "/Abundance.xls"),
                   sheetName = paste0("relativ",
                                      taxRank[tRlength]),
                   append = T)

  # move to the next higher taxonomic rank
  taxRank <- taxRank[0:(tRlength - 1)]

  # remove temporary files
  rm(tempdf)
  rm(temps3)
  rm(temprela)
}



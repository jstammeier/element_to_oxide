# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # rstudio only

pprint <- function(...) print(paste(...)) #'credit to Jthräno'

#######   Import data             #######

terminal_version_f = function() {
  file_chr = file.choose()
  convert_f(file_chr)
  View(Results_df)
}


### Determine which element is trace and which is major
# import table with oxide to and from element factors
# including list of all elements and respective oxides
e_t_o_df <- read.table(file = "Element_to_oxide.txt",
                        sep = "\t",
                        header = T)

# adds column that calculates oxide to element  
e_t_o_df$oxide_to_element <- with(e_t_o_df, 1 / element_to_oxide)

get_major_minor_f = function(file_chr) {

  ########### classical approach #############
  #Determine major vs minor elements by concentration level,
  # i.e. %-range = major; ppm-range = minor/trace

  major_minor_df <- read.table( file = file_chr
                               ,sep = "\t"
                               ,header = T
                               ,nrows = 1 
  )

  ## determines rows that are major (=%) and minor (=ppm) elements
  # first renders logical vector with major/minor true
  major_bol <- grepl("%", major_minor_df[1, ], fixed = T) #
  minor_bol <- grepl("ppm", major_minor_df[1, ], fixed = T)

  # secondly uses above vector to store respective elements/oxides in dataframe
  major_df <- as.data.frame(major_minor_df[, major_bol])
  minor_df <- as.data.frame(major_minor_df[, minor_bol])

  ## the following eliminates the columns that are not elemental data, 
  # by comparing them to the complete list of elements as included in e_t_o_df
  #|| This is frequently the case with % e.g. LOI, Sum, Total etc. 

  # first converts the major elements to character vector
  major_chr <- as.character(colnames(major_df))

  # second combines all elements and oxides in character vector
  all_existing_elements_chr <- as.character(e_t_o_df$oxide)
  all_existing_elements_chr <- append(all_existing_elements_chr,
                                      as.character(e_t_o_df$element))


  # third checks if elements formerly stored as major elements are
  # indeed true elemental data, stored as logical vector
  # the latter is then used to select only these true elemental data
  major_chr <- major_chr %in% all_existing_elements_chr
  major_df <- as.data.frame(major_df[, major_chr])

  ret = list(major_df, minor_df)
  return(ret)
}

# filter_major_minor_f = function(varlist, major_df, minor_df) {
#   new_major = 
#   new_minor =
#   dt[ ,colnames(dt) %in% list, with=FALSE]
# }

convert_f = function(file_chr, major_df, minor_df) {

  cnames = read.table( file = file_chr
                      ,sep = "\t"
                      ,header = T
                      ,nrows = 1
  )

  cnames <- cnames[-c(1), ]
  cnames <- as.character(names(cnames)) # deletes first row

  data_df = read.table( file = file_chr
                       ,sep = "\t"
                       ,header = F
                       ,skip = 2
                       ,col.names = cnames
  )
 
  ## loeschen
#   tmp = get_major_minor_f(file_chr)
#   major_df = tmp[[1]]
#   minor_df = tmp[[2]]

  ## Create data frame that only contains important columns, metadata and elemental data
  # rename important non-elemental (metadata) columns
  names(data_df)[1] <- "name"

  # select columns to extract from RFA data frame
  selected_columns <- c(
                        "name",
                        "LOI", #potentially needs to go, not always included, how to fix? #FUCK
                        colnames(major_df),
                        colnames(minor_df)
  )

  # can be renamed to Results_df = data_df later
  tmp_df <- data_df[, selected_columns]
  Results_df <- data_df[, c('name', 'LOI')]

  ## Calculate all concentrations of minor elements that are reported as oxides to
  # elemental concentration
  # loop over all minor elements as defined in minor_df (i.e. ppm range)

  for (i in colnames(minor_df)) {
    print(i)
    if (grepl("O", i, fixed = TRUE)) {
      print("its a minor match")
      rownames(e_t_o_df) <- e_t_o_df$oxide
      fac <- e_t_o_df[i, "oxide_to_element"]
      pprint(i,
             "recalculated to",
             e_t_o_df[i, "element"],
             "by multiplication with",
             fac)
      f <- function(x) {
        x * fac
      }

      Results_df[, e_t_o_df[i, "element"]] <- f(tmp_df[,i])
    }
    else {
      Results_df[, i] <- tmp_df[,i]
    }
  }

  ## Calculate all concentrations of major elements that are reported as elements to
  # oxide concentration
  # loop over all major elements as defined in major_df (i.e. % range)
  for (i in colnames(major_df)) {
    print(i)
    if (!grepl("O", i, fixed = TRUE)) {
      print("its a major match")
      rownames(e_t_o_df) <- e_t_o_df$element
      fac <- e_t_o_df[i, "element_to_oxide"]
      pprint(i,
             "recalculated to",
             e_t_o_df[i, "oxide"],
             "by multiplication with",
             fac)
      f <- function(x) {
        x * fac
      }
      Results_df[, e_t_o_df[i, "oxide"]] <- f(tmp_df[, i])
    }
    else {
      Results_df[, i] <- tmp_df[,i]
    }
  }

  return(Results_df)
}


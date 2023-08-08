setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # rstudio only

data_df = read.table(file = "24_2022_example-data_raw.xlsx",
                     sep = "\t",
                     header =
                       T)



for (i in 1:length(excel_sheets(path_data))) {
  #######   Import data             #######

  cnames <- read_excel(
    path_data,
    col_names = TRUE,
    range = cell_rows(8)
    ,
    sheet = i
  ) %>%
    names()
  # remove punctuation and special characters
  cnames <- gsub("[[:punct:]]", "", cnames)

  RFA <- read_excel(path_data,
                    skip = 11,
                    col_names = cnames
                    ,
                    sheet = i)

  ### Determine which element is trace and which is major
  # import table with oxide to and from element factors
  # including list of all elements and respective oxides
  e_t_o_df <- read.table(file = "Element_to_oxide.txt",
                         sep = "\t",
                         header =
                           T)
  e_t_o_df$oxide_to_element <- with(e_t_o_df, 1 / element_to_oxide)


  major_minor_df <- read_excel(
    path_data
    ,
    range = cell_rows(c(8, 10)) # import rows including %&ppm unit plus elements
    ,
    col_names = cnames
    ,
    sheet = i
  )

  major_minor_df <-
    major_minor_df[-c(1, 2), ] # delete row in between

  ## determines rows that are major (=%) and minor (=ppm) elements
  # first renders logical vector with major/minor true
  major_bol <- grepl("%", major_minor_df[1, ], fixed = T)
  minor_bol <- grepl("ppm", major_minor_df[1, ], fixed = T)

  # secondly uses above vector to store respective elements/oxides in dataframe
  major_df <- as.data.frame(major_minor_df[, major_bol])
  minor_df <- as.data.frame(major_minor_df[, minor_bol])

  ## the following eliminates the columns that are not elemental data, by comparing them to the complete list of elements || This is frequently the case with % e.g. LOI

  # first coverts the major elements to character vector
  major_chr <- as.character(colnames(major_df))

  # second combina all elements and oxides in character vector
  all_existing_elements_chr <- as.character(e_t_o_df$oxide)
  all_existing_elements_chr <- append(all_existing_elements_chr,
                                      as.character(e_t_o_df$element))
  # third checks if elements formerly stored as major elements are
  # indeed true elemental data, stored as logical vector
  # the latter is then used to select only these true elemental data
  major_chr <- major_chr %in% all_existing_elements_chr
  major_df <- as.data.frame(major_df[, major_chr])


  ## Create data frame that only contains important columns, metadata and elemental data
  # rename important non-elemental (metadata) columns
  names(RFA)[startsWith(names(RFA), "Sample name")] <- "name"
  names(RFA)[startsWith(names(RFA), "Meas datetime")] <- "datetime"
  names(RFA)[startsWith(names(RFA), "Loss On")] <- "LOI"

  # select columns to extract from RFA data frame
  selected_columns <- c(
    "name",
    "datetime",
    "material",
    "Initial",
    "Final",
    "Norm",
    "LOI",
    colnames(major_df),
    colnames(minor_df)
  )

  RFA <- RFA[, names(RFA) %in% selected_columns]

  ## Calculate all oxides (here for minor elements) to elemental concentration
  # loop over all minor elements as defined in minor_df (i.e. ppm range)

  for (i in colnames(minor_df)) {
    print(i)
    if (grepl("O", i, fixed = TRUE)) {
      print("its a match")
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
      RFA[, i] <- data.frame(lapply(RFA[, i], f))
      names(RFA)[names(RFA) == i] <- e_t_o_df[i, "element"]
      names(minor_df)[names(minor_df) == i] <-
        e_t_o_df[i, "element"]
    }
  }

  ## Calculate all major elements to oxide concentration
  # loop over all major elements as defined in major_df (i.e. % range)

  for (i in colnames(major_df)) {
    print(i)
    if (!grepl("O", i, fixed = TRUE)) {
      print("its a match")
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
      RFA[, i] <- data.frame(lapply(RFA[, i], f))
      names(RFA)[names(RFA) == i] <- e_t_o_df[i, "oxide"]
      names(major_df)[names(major_df) == i] <- e_t_o_df[i, "oxide"]
    }
  }
  # View(RFA)

  #######   Primary Data reduction  QC/QA  #######
  ## minor elements:
  # deletes everything below 10 ppm

  h <- function(x) {
    ifelse(x < 10, NA, x)
  }

  # minor_elements = noquote(paste("'",colnames(RFA[,26:ncol(RFA)]),"'",collapse=", ",sep=""))
  # RFA[, c(colnames(RFA[, minor:ncol(RFA)]))] = lapply(RFA[, c(colnames(RFA[, minor:ncol(RFA)]))], h)
  RFA[, names(RFA) %in% colnames(minor_df)] <-
    lapply(RFA[, names(RFA) %in% colnames(minor_df)], h)

  ## major elements
  # deletes everything below 0.02 wt.%

  k <- function(x) {
    ifelse(x < 0.02, NA, x)
  }

  RFA[, names(RFA) %in% colnames(major_df)] <-
    lapply(RFA[, names(RFA) %in% colnames(major_df)], k)

  ######    Calculate Sum of all main elements####

  RFA_sum = RFA
  RFA_sum$Total <-
    rowSums(cbind(RFA[, names(RFA) %in% colnames(major_df)], RFA$LOI),
            na.rm = TRUE)

  results_conc_xrf <- RFA
  #   subset(RFA, select = -c(
  #   Pb
  #   ,
  #   F
  #   ,
  #   Mo, Co, La
  #   , Ce, W
  #   # , Ge
  #   , Bi, Cs, Nd, Ta
  # )) #Fuck ändern in: wenn vorhanden dann weg damit

  results_conc_xrf$type <- "CRM"
  results_conc_xrf$series <-
    "22_2023" #import filename auseinander nehmen
  results_conc_xrf$preparation <- "borate fusion"
  results_conc_xrf$analysis <- "xrf"

  results_conc_xrf_melted = reshape2::melt(
    results_conc_xrf
    ,
    id.vars = c(
      "name",
      "material",
      "series",
      "analysis",
      "preparation"
      ,
      "datetime",
      "Initial",
      "Final",
      "Norm",
      "type"
    )
    ,
    value.name = "concentration"
    ,
    variable.name = "element"
  )

  results_conc_xrf_melted = dplyr::mutate(
    results_conc_xrf_melted
    ,
    dilution = Final / Initial
    ,
    Final = NULL
    ,
    Initial = NULL
    ,
    Norm = NULL
  )

  results_conc_xrf_melted_list[[length(results_conc_xrf_melted_list) + 1]] = results_conc_xrf_melted

}
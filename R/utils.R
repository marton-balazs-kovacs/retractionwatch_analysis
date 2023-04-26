#' Insert random number of NA
#'
#' @param df a dataframe to insert NAs into
#' @param max the maximum number of NAs inserted in a row
#' @param proportion the percentage of rows to include NA in
#'
#' @return A dataframe where random number of NAs is inserted from the last variable backwards
#'
#' @export
#'
insert_na <- function(df, max, proportion = 10) {
  n_rows <- round(nrow(df) * proportion / 100, 0)
  print(n_rows)
  n_na <- sample(1:max, n_rows, replace = TRUE)
  # Exclude some of the columns that we do not to have NA in
  col_names <- colnames(df)
  row_ids <- sample(1:nrow(df), n_rows, replace = TRUE)
  for (i in 1:n_rows) {
    row_id <- row_ids[i]
    col_names_filtered <- col_names[length(col_names):(length(col_names) - n_na[i])]
    df[row_id, col_names_filtered] <- NA
  }
  return(df)
}

#' Function to NOT include a vector in another vector
`%ni%` <- Negate(`%in%`)

#' Reads in and merge local csv files
#' The function reads in and merge individual locale csv files
#' into one tibble from different subdirectories. The function also
#' saves directory, subdirectory and file names as a variable.
#' 
#' @param pattern The pattern to look for when listing files in the locale directory.
#' @param path The path to the locale directory that contains the subdirectories.
#' @param subfolder_name A character vector of the subfolder names to look into.
#' @param exclude A character string. Filenames containing this string will not be read in.
#' @param sep Used as delim in read_delim.
#' 
#' @return All files that meets the criteria merged into a tibble from the specified subdirectory.
read_plus <- function(pattern, path, subfolder_name, include = NULL, sep) {
  if(!str_ends(path,"/")) { # this allows to run this function without using last /
    path <- paste0(path,"/")
  }
  files <- tibble(list = list.files(path = paste0(path, subfolder_name), pattern = pattern, full.names = T, recursive = T))
  
  if(!is.null(include)){
    files <- filter(files, str_detect(list, include))
  }
  
  files %>%
    pull(list) %>% 
    map_dfr(.,
            ~ read_delim(.x, delim = sep) %>% 
              mutate(filename = str_remove_all(.x, ".*/|.csv")))
}

#' Generate lorem ipsum to sample from in free text responses
rand_lorem_ipsum <- function(n) {
  words <- strsplit(stringi::stri_rand_lipsum(1), " ")
  random_words <- sample(words[[1]], n, replace = TRUE)
  return(paste(random_words, collapse = " "))
}

#' Function to simulate random values with ceiling
rnorm_ceiling <- function(ceiling, ...) {
  rand_vec <- rnorm(...)
  ifelse(rand_vec > ceiling, ceiling, rand_vec)
}
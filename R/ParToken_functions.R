
tokenize_dataset <- function(path, data_type = "csv", text = "text", unit = "word", language = "en", preprocess = TRUE){
  #load data
  if (!require("foreach", character.only = TRUE)) {
    install.packages("foreach")
    library(foreach)
  }
  if (!require("doParallel", character.only = TRUE)) {
    install.packages("doParallel")
    library(doParallel)
  }
  if (!require("future.apply", character.only = TRUE)) {
    install.packages("future.apply")
    library(future.apply)
  }
  if (!require("tidyverse", character.only = TRUE)) {
    install.packages("tidyverse")
    library(tidyverse)
  }

  if (data_type == "csv") {
    df <- read.csv(path)
  } else if (data_type == "xlsx") {
    if (!require("readxl", character.only = TRUE)) {
      install.packages("readxl")
    }
    library(readxl)
    df <- read_excel("your_file.xlsx")
  } else if (data_type == "tsv") {
    df <- read.table("your_file.tsv", header = TRUE, sep = "\t")
  } else {
    stop("Data type is not supported")
  }

  corp <- corpus(df, text_field = text)

  future::plan(future::multisession)

  corp <- as.character(corp)
  corp <- split(corp, rep_len(1:core, length(txt)))

  if (!preprocess){
    tokens_no_pre <- do.call(c, #parallel tokenization
                             future_lapply(corp, tokens, what = unit,
                                           remove_separator = FALSE))
    cat("Tokens without preprocessing")
    return(tokens_no_pre)
  }
  else {
    tokens_pre <- do.call(c, #parallel tokenization
                          future_lapply(corp, tokens, what = unit,
                                        remove_separator = FALSE,
                                        remove_punct = TRUE,
                                        remove_symbols = TRUE,
                                        remove_numbers = TRUE,
                                        remove_url = TRUE))
    tokens_pre <- tokens_pre %>%
      tokens_tolower() %>%
      tokens_remove(stopwords(language))

    cat("Tokens with preprocessing")
    return(tokens_pre)
  }

}





tokenize_dataframe <- function(df, text = "text", unit = "word", language = "en", preprocess = TRUE){
  #load data
  if (!require("foreach", character.only = TRUE)) {
    install.packages("foreach")
    library(foreach)
  }
  if (!require("doParallel", character.only = TRUE)) {
    install.packages("doParallel")
    library(doParallel)
  }
  if (!require("future.apply", character.only = TRUE)) {
    install.packages("future.apply")
    library(future.apply)
  }
  if (!require("tidyverse", character.only = TRUE)) {
    install.packages("tidyverse")
    library(tidyverse)
  }

  corp <- corpus(df, text_field = text)

  future::plan(future::multisession)

  corp <- as.character(corp)
  corp <- split(corp, rep_len(1:core, length(txt)))

  if (!preprocess){
    tokens_no_pre <- do.call(c, #parallel tokenization
                             future_lapply(corp, tokens, what = unit,
                                           remove_separator = FALSE))
    cat("Tokens without preprocessing")
    return(tokens_no_pre)
  }
  else {
    tokens_pre <- do.call(c, #parallel tokenization
                          future_lapply(corp, tokens, what = unit,
                                        remove_separator = FALSE,
                                        remove_punct = TRUE,
                                        remove_symbols = TRUE,
                                        remove_numbers = TRUE,
                                        remove_url = TRUE))
    tokens_pre <- tokens_pre %>%
      tokens_tolower() %>%
      tokens_remove(stopwords(language))

    cat("Tokens with preprocessing")
    return(tokens_pre)
  }

}




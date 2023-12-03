#' Parallel tokenize an df
#'
#' @param df df you want to tokenize
#' @param text which column of the ds has the text in it
#' @param unit tokenize by which unit "word" or "sentence"
#' @param language language of the stop words
#' @param preprocess should the tokens be preprocessed? TRUE or FALSE
#'
#' @return tokens
#' @export
#'
#' @examples pre_tokens <- tokenize_dataframe(path = "PPE_party.csv", text = "fused", preprocess = FALSE)
tokenize_dataframe <- tokenize_dataframe2 <- function(df, text = "text", unit = "word", language = "en", preprocess = TRUE){
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
  if (!require("quanteda", character.only = TRUE)) {
    install.packages("quanteda")
    library(quanteda)
  }

  corp <- corpus(df, text_field = text)
  core = as.numeric(detectCores())

  future::plan(future::multisession)

  corp <- as.character(corp)
  corp <- split(corp, rep_len(1:core, length(corp)))

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




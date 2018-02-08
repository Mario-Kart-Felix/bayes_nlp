

simplify_document <- function(df, string_col, id_col, stop_words = NULL, stop_regex = NULL, stem_terms = NULL, lemmatize = FALSE){
  quo_col <- enquo(string_col)
  id_col <- enquo(id_col)

  # create regex pattern for stemming
  stem_pattern = paste(stem_terms, collapse = "|")

  # create group id for reassembling step
  # remove stop words
  # remove punctuation
  df <- df %>%
    select(!!id_col, !!quo_col) %>%
    unnest_tokens(token, !!quo_col, token = "words") %>%
    group_by(!!id_col) %>%
    mutate(groupid = 1:n()) %>%
    ungroup() %>%
    anti_join(stop_words, by = c("token"="word")) %>%
    mutate(token = gsub("[[:punct:]]", "", token))

  # remove regex pattern matches
  if(!is.null(stop_regex)){
    df <- df %>% regex_anti_join(stop_regex)
  }

  # stemming
  if(!is.null(stem_terms)){

    df <- df %>%
      mutate(token = ifelse(is.na(str_extract(token, stem_pattern)),
                            token,
                            str_extract(token, stem_pattern)))
  }

  # lemmatization, file is written to disk, run through a lemmatizer from the command line, and returned
  # to R as a csv file
  if(lemmatize){
    dir = tempdir()
    infile = paste0(dir, "\\input.txt")
    outfile = paste0(dir, "\\output.txt")
    write_delim(df %>% select(token) %>% distinct(), infile)
    shell(paste0("cd ", gsub("\\", "/", dir, fixed = TRUE), "&&tag-english input.txt > output.txt"))
    output <- read_tsv(outfile) %>% select(token, token_1)

    file.remove(outfile)
    file.remove(infile)
  }

  #reassemble document
  df %>%
    left_join(output) %>%
    select(!!id_col, groupid, token_1) %>%
    group_by(token_1) %>%
    filter(n() >= 5) %>%
    group_by(!!id_col) %>%
    arrange(groupid) %>%
    summarise(token = paste(token_1, collapse = " "))

}

#obtain unique counts of each word, to be performed after simplify_document()
get_counts_unique <- function(df, message_col){
  message_col <- enquo(message_col)

  df %>% select(!!message_col) %>%
    unnest_tokens(token, !!message_col, token = "words") %>%
    group_by(token) %>%
    mutate(n = n()) %>%
    unique() %>%
    ungroup()

}

#calculate log probability tables, this is the "training"
get_prob_tables <- function(group_1, group_0, message_col){
  message_col <- enquo(message_col)

  #word counts for both groups(classes)
  counts_1 <- get_counts_unique(group_1, !!message_col)
  counts_0 <- get_counts_unique(group_0, !!message_col)

  allterms <- data.frame(token = unique(c(counts_1$token, counts_0$token)), stringsAsFactors = FALSE)

  ###Posterior probabilities for each term given the document is spam or ham###
  prob_table_1 <- allterms %>% left_join(counts_1, by = "token") %>%
    mutate(n = replace(n, is.na(n), 0), n = n + 1)

  prob_table_0 <- allterms %>% left_join(counts_0, by = "token") %>%
    mutate(n = replace(n, is.na(n), 0), n = n + 1)

  prob_table_1 <- prob_table_1 %>% mutate(logprob_1 = log(n/sum(n)))
  prob_table_0 <- prob_table_0 %>% mutate(logprob_0 = log(n/sum(n)))

  list(prob_table_1, prob_table_0)

}

#Use probability tables from get_prob_tables() to obtain predictions
test_model <- function(test_df, table_1, table_0, message_col, id_col, prior_0 = 0.5, prior_1 = 0.5){
  quo_col <- enquo(message_col)
  id_quo <- enquo(id_col)

  test_df %>%
    select(!!id_quo, !!quo_col, score) %>%
    unnest_tokens(token, !!quo_col) %>%
    filter(token %in% table_1$token) %>%
    left_join(table_1) %>%
    left_join(table_0, by = "token") %>%
    group_by(!!id_quo) %>%
    summarise(prob_1 = prior_1 + sum(logprob_1), prob_0 = prior_0 + sum(logprob_0), score = mean(score)) %>%
    mutate(pred = as.integer(prob_1 > prob_0))

}




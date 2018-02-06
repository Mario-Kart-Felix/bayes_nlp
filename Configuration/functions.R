
#get occurences of words within messages for unique email id's
simplify_document <- function(df, join_col, stop_words = NULL, stop_regex = NULL, stem_terms = NULL){
  df <- df %>% rowid_to_column()
  quo_col <- enquo(join_col)
  stem_pattern = paste(stem_terms, collapse = "|")

  df <- df %>%
    select(rowid, !!quo_col) %>%
    unnest_tokens(token, message, token = "words") %>%
    group_by(rowid) %>%
    mutate(groupid = 1:n()) %>%
    ungroup() %>%
    anti_join(stop_words, by = c("token"="word")) %>%
    regex_anti_join(stop_regex) %>%
    mutate(token = gsub("[[:punct:]]", "", token))

  if(!is.null(stem_terms)){

    df <- df %>%
      mutate(token = ifelse(is.na(str_extract(token, stem_pattern)),
                            token,
                            str_extract(token, stem_pattern)))
  }



  dir = tempdir()
  infile = paste0(dir, "\\input.txt")
  outfile = paste0(dir, "\\output.txt")
  write_delim(df %>% select(token) %>% distinct(), infile)
  shell(paste0("cd ", gsub("\\", "/", dir, fixed = TRUE), "&&tag-english input.txt > output.txt"))
  output <- read_tsv(outfile) %>% select(token, token_1)

  file.remove(outfile)

  df <- df %>%
    left_join(output) %>%
    select(rowid, groupid, token_1) %>%
    rename(token = token_1) %>%
    group_by(rowid) %>%
    arrange(groupid) %>%
    summarise(token = paste(token, collapse = " "))

  df

}

get_counts_unique <- function(df, join_col, stop_words = NULL, stop_regex = NULL, stem_terms = NULL, lemmatize = FALSE){

  df <- df %>% rownames_to_column()
  quo_col <- enquo(join_col)
  stem_pattern = paste(stem_terms, collapse = "|")


    frame <- df %>%
      select(rowname, !!quo_col) %>%
      unnest_tokens(token, message, token = "words") %>%
      anti_join(stop_words, by = c("token"="word")) %>%
      regex_anti_join(stop_regex) %>%
      mutate(token = gsub("[[:punct:]]", "", token))

    if(!is.null(stem_terms)){

      frame <- frame %>%
               mutate(token = ifelse(is.na(str_extract(token, stem_pattern)),
                                               token,
                                               str_extract(token, stem_pattern)))
    }



    frame <- frame %>%
      group_by(token) %>%
      mutate(n = n()) %>%
      filter(n > 1) %>%
      distinct(token, n) %>%
      ungroup()

    frame
}

get_prob_tables <- function(spam_train, ham_train){
  #word counts for both groups
  word_counts_spam <- get_counts_unique(spam_train, message, ngrams)
  word_counts_ham <- get_counts_unique(ham_train, message, multinomial, ngrams)

  allterms <- data.frame(token = unique(c(word_counts_spam$token, word_counts_ham$token)), stringsAsFactors = FALSE)

  ###Posterior probabilities for each term given the document is spam or ham###
    prob_table_spam <- allterms %>% left_join(word_counts_spam, by = "token") %>%
      mutate(n = replace(n, is.na(n), 0), n = n + 1)

    prob_table_ham <- allterms %>% left_join(word_counts_ham, by = "token") %>%
      mutate(n = replace(n, is.na(n), 0), n = n + 1)

    prob_table_spam <- prob_table_spam %>% mutate(logprob = log(n/sum(n)))
    prob_table_ham <- prob_table_ham %>% mutate(logprob = log(n/sum(n)))

    list(prob_table_spam, prob_table_ham)

}

#test the model given a spam and ham table.
test_model <- function(test_emails, spam_table, ham_table, ngrams = FALSE, multinomial = FALSE, prior_ham = 0.5, prior_spam = 0.5){

  test_emails["pred"] <- 0

  for(i in 1:nrow(test_emails)){

        test_mail <- unnest_tokens(test_emails[i,], token, message) %>%
            select(token) %>%
            anti_join(stop_words, by = c("token"="word")) %>%
            filter(token %in% spam_table$token) %>%
            ungroup()

    ###should be same for both
    log_prob_ham <- log(prior_ham) +
      sum((test_mail %>%
             left_join(ham_table, by = "token"))$logprob)

    log_prob_spam <- log(prior_spam) +
      sum((test_mail %>%
             left_join(spam_table, by = "token"))$logprob)


    if(log_prob_spam > log_prob_ham){
      test_emails[i,]$pred <- 1
    }

  }

  test_emails

}





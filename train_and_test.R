source("requirements.R")
library(tidytext)
library(stringr)
library(fuzzyjoin)

fraud_messages <- load_fraud_messages()
ham_messages <- load_regular_messages()

data("stop_words")
stop_regex = data.frame(token = c("\\b[0-9]{1}\\b", "_{2,}", "[^A-Za-z0-9[:punct:]]+"), stringsAsFactors = FALSE)
stem_terms = c("grantstar", "grant(?!star)")
doc_stripped <- simplify_document(fraud_messages, message, stop_words, stop_regex, stem_terms)


########### Test Toxicity Comments ################

comments_scores <- read_csv("~/Git Repos/bayesclassifier/Data/comments_scores.csv")
comments_scores <- comments_scores %>% rename(message = comment)

doc_stripped <- simplify_document(comments_scores, message, rev_id, stop_words, stop_regex)

comments_toxic <- comments_scores %>%
  filter(score == -1) %>%
  select(rev_id, score) %>%
  left_join(doc_stripped) %>%
  rename(message = token)

comments_good <- comments_scores %>%
  filter(score == 1) %>%
  select(rev_id, score) %>%
  left_join(doc_stripped) %>%
  rename(message = token)

toxic_train = comments_toxic %>% sample_frac(0.8)
good_train = comments_good %>% sample_frac(0.8)

toxic_test = comments_toxic %>% anti_join(toxic_train)
good_test = comments_good %>% anti_join(good_train)

p_toxic <- nrow(comments_toxic)/nrow(comments_good)
p_good <- 1-p_toxic

preds_2 <- test_model(rbind(toxic_test, good_test), probs[[1]], probs[[2]], prior_0 = p_good, prior_1 = p_toxic)
barely <- preds %>% filter(score == -1, pred == 0) %>% select(rev_id)

table(preds$score, preds$pred)

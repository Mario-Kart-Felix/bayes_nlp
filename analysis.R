source("requirements.R")
source("functions.R")


comments <- read_delim("Data/toxicity_annotated_comments.tsv",
                       "\t", escape_double = FALSE, trim_ws = TRUE)

scores <- read_delim("Data/toxicity_annotations.tsv",
                     "\t", escape_double = FALSE, trim_ws = TRUE)

#############################################################



##########Build Toxic and Nontoxic Tables##############

data("stop_words")

subs <- "[^[:alnum:][:space:]'{1}]|NEWLINE_TOKEN"

comments <- comments %>% mutate(comment = gsub(subs, " ", comment))

scores_collapsed <- scores %>% group_by(rev_id) %>%
  mutate(score = ifelse(sum(toxicity_score) < -1, -1, 1)) %>%
  slice(1)

scores_multinomial <- scores %>% group_by(rev_id)  %>%
  mutate(score = sum(toxicity_score)) %>%
  slice(1)

comments_scores <- comments %>% left_join(scores_collapsed) %>%
                                select(-toxicity_score, -toxicity)

comments_scores_multinomial <- scores_multinomial %>% left_join(scores_multinomial) %>%
                               select(-toxicity_score, -toxicity)


write.csv(comments_scores, "Data/comments_scores.csv")


########Get Counts########

counts <- get_counts_unique(df = comments, multinomial = TRUE)

alltokens <- comments %>% select(comment) %>%
                          mutate(comment = gsub("NEWLINE_TOKEN", " ", comment),
                                 comment = gsub("[[:punct:]]", " ", comment)) %>%
                          unnest_tokens(token, comment, token = "words") %>%
                          anti_join(stop_words, by = c("token" = "word")) %>%
                          group_by(token) %>%
                          mutate(n=n()) %>%
                          distinct(token, n)








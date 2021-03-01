

c3_full <- c3 %>% na.omit()
w3_withcov <- b3 %>% filter(ids %in% c3_full$ids)

w2_f <- w2_full %>%
  select(ids:wrkngmom) %>%
  gather(key = "question", value = "w2", -ids)

observed_responses <- inner_join(w2_f, w3_withcov %>%
  gather(key = "question", value = "w3", -ids)) %>%
  na.omit()


t12 %>% group_by(question) %>%
  summarise(count = n()) %>% arrange(count)


#Given ID and a new response, calculate the likelihood
p <- cbind(w2_full %>% select(ids:wrkngmom), l5$posterior) %>%
  filter(ids %in% two_waves) %>%
  gather(key = "question", value = "w2", -c(ids, `1`, `2`, `3`, `4`, `5`)) %>%
  gather(key = "class", value = "prob_class", -c(ids, question, w2)) %>%
  mutate(class = as.numeric(class))

responses <- tidy(l5) %>%
  select(-std.error) %>%
  spread(outcome, estimate)

pred_resp_probs <- left_join(p, responses, by = c("question"="variable", "class"="class")) %>%
  gather(key = "resp", value = "prob_resp", -c(ids, question, w2, class, prob_class)) %>%
  group_by(ids, question, resp) %>%
  mutate(uncond_prob_resp = prob_class * prob_resp) %>%
  summarise(prob_resp = sum(uncond_prob_resp)) %>%
  spread(resp, prob_resp)

#Sum of the probabilities of giving each response times the probability of each class

test <- pred_resp_probs %>% group_by(ids, question) %>% summarise(total = sum(prob_resp))


pred_resp_probs %>%
  left_join(l3, by = c("ids", "question")) %>%
  mutate(likelihood = ifelse(w3 == 1, ))


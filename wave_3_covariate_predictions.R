
#Predict everything with people who are in wave 2
#Predict wave 3 beliefs for people who have covariates (don't need to have all beliefs!)


#3360 people at wave 3
#2447 people have all beliefs
b3_full <- b3 %>% na.omit()
#2520 people have all controls
c3_full <- c3 %>% na.omit()

#2144 people in wave 4
#2044 people 
b4_full <- b4 %>% na.omit()


w3_withcov <- b3 %>% filter(ids %in% c3_full$ids)

#Wave 2 beliefs
#Wave 3 beliefs
#Wave 2 controls
#Wave 3 controls

cp <- controls_3 %>%
  filter(ids %in% full_only) %>%
  mutate(attend = ifelse(is.na(attend), mean(attend, na.rm = TRUE), attend))

cp <- c3 %>% filter(ids %in% w2_full$ids)


cp <- c3 %>% filter(ids %in% two_waves) %>%
  left_join(c1)


pred_prob_estimates_3 <- vector(mode = "list", length = length(attitude_vars))
for (i in 1:length(attitude_vars)) {
  var <- attitude_vars[i]
  multinom_df <- w2_full %>%
    select(ids, var, bntraev, bntramnl, bntracat, bntrajew, 
           bntraoth, bntradk, bntraaf, attend, bnpblack, bnpoth,
           parba, parclose, gender, south, west, midwest, 
           agecats, compgrad, pshrblf)
  names(multinom_df)[2] <- "resp"
  multinom_df <- filter(multinom_df, !is.na(resp))
  multinom_df$resp <- as.factor(multinom_df$resp)
  
  m1 <- multinom(resp ~ bntraev + bntramnl + bntracat + bntrajew + 
                   bntraoth + bntradk + bntraaf + 
                   attend + parba + parclose + gender + 
                   south + agecats + compgrad + pshrblf, 
                 data = multinom_df)
  pred_probs <- cbind(cp %>%
                        select(ids), predict(m1, cp, "probs")) %>%
    mutate(question = var)
  
  pred_prob_estimates_3[[i]] <- pred_probs
}

multinom_probs <- left_join(observed_responses, bind_rows(pred_prob_estimates), 
                            by = c("ids", "question")) %>%
  select(ids, question, w2, `1`, `2`, `3`, `4`, `5`) %>%
  mutate(`2` = ifelse(is.na(`2`), 0, `2`),
         `4` = ifelse(is.na(`4`), 0, `4`)) 

multinom_patterns_3 <- vector(mode = "list", length = 10)
for (i in 1:10) {
  multinom_count_3 <- multinom_probs %>%
    rowwise() %>%
    mutate(pred_resp_3 = sample(1:5, 1, replace = TRUE, prob = c(`1`, `2`, `3`, `4`, `5`))) %>%
    select(ids, question, w2, pred_resp_3) %>%
    mutate(pattern = paste(w2, pred_resp_3, sep = "->")) %>%
    group_by(question, w2, pred_resp_3, pattern) %>%
    summarise(n = n())
  names(multinom_count_3)[5] <- i
  multinom_patterns_3[[i]] <- multinom_count_3
}

for (i in 1:length(multinom_patterns_3)) {
  if (i == 1) {
    multinom_predictions_3 <- multinom_patterns_3[[i]]
  } else {
    multinom_predictions_3 <- full_join(multinom_predictions_3, multinom_patterns_3[[i]], by = c("question", "w2", "pred_resp_3", "pattern"))
  }
}

mn_pred_3 <- multinom_predictions_3 %>%
  gather(key = "key", value = "value", -c(question, w2, pred_resp_3, pattern)) %>%
  ungroup() %>%
  select(question, pattern, value, key) %>%
  mutate(model = "multinom_3")

mn_pred_3 %>% group_by(question, key) %>%
  summarise(n = sum(value, na.rm = TRUE)) %>% spread(key, n)

mn_ssd_3 <- mn_pred_3 %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  left_join(actual_counts, by = c("question", "pattern"="observed_pat")) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(ssd = sqrt((value - count)^2)) %>% 
  group_by(question, key) %>%
  summarise(sum_ssd = sum(ssd)) %>%
  mutate(model = "4. mn_3")




predictable <- c3_full %>% filter(ids %in% c(w2_full$ids))
cp_mat <- predictable %>% 
  mutate(intercept = 1) %>%
  select(intercept, bntraev, bntramnl, bntracat, bntrajew, bntraoth, 
         bntraoth, bntradk, bntralds, bntraaf, attend, gender, midwest, 
         south, west, agecats, compgrad, pshrblf) %>%
  as.matrix()

w3_class_probs <- cp_mat %*% l5$coeff %>%
  as.data.frame() %>%
  mutate(prob1 = exp(0)/(1 + exp(V1) + exp(V2) + exp(V3) + exp(V4)),
         prob2 = exp(V1)/(1 + exp(V1) + exp(V2) + exp(V3) + exp(V4)),
         prob3 = exp(V2)/(1 + exp(V1) + exp(V2) + exp(V3) + exp(V4)),
         prob4 = exp(V3)/(1 + exp(V1) + exp(V2) + exp(V3) + exp(V4)),
         prob5 = exp(V4)/(1 + exp(V1) + exp(V2) + exp(V3) + exp(V4))) %>%
  mutate(ids = predictable$ids) %>%
  select(ids, prob1, prob2, prob3, prob4, prob5)


########## Predicting Responses based on Latent Classes
lca_patterns_3 <- vector(mode = "list", length = 10)
for (i in 1:10) {
  p <- w3_class_probs
  classes <- p %>%
    rowwise() %>%
    mutate(class = sample(1:5, 1, replace = TRUE, 
                          prob = c(prob1, prob2, prob3, prob4, prob5))) %>%
    select(ids, class)
  
  #For each question, sample from the probabilities of giving each response
  lca_question_class <- long_blf %>% ungroup() %>%
    select(ids, question, x, y) %>%
    left_join(classes, by = "ids") %>%
    filter(!is.na(class))
  
  rprobs <- tidy(l5) %>% 
    mutate(question = variable) %>%
    select(question, class, outcome, estimate) %>%
    spread(outcome, estimate)
  
  pred_resp <- left_join(lca_question_class, rprobs, by = c("question", "class")) %>%
    rowwise() %>%
    mutate(pred_resp = sample(1:5, 1, replace = TRUE, prob = c(`1`, `2`, `3`, `4`, `5`))) %>%
    select(ids, question, x, y, pred_resp)
  
  patterns <- pred_resp %>%
    filter(!is.na(y)) %>%
    mutate(pattern = paste(x, pred_resp, sep = "->")) %>%
    group_by(question, x, pred_resp, pattern) %>%
    summarise(n = n())
  
  names(patterns) <- c("question", "x", "pred_resp", "pattern", i)
  
  lca_patterns_3[[i]] <- patterns
}
#For each person. 
#Sample from their posterior probs a class
#Assign 

for (i in 1:length(lca_patterns_3)) {
  if (i == 1) {
    lca_predictions_3 <- lca_patterns_3[[i]]
  } else {
    lca_predictions_3 <- full_join(lca_predictions_3, lca_patterns_3[[i]], by = c("question", "x", "pred_resp", "pattern"))
  }
}

lca_predictions %>%
  gather(key = "key", value = "value", -c(question, X2, pred_resp, pattern)) %>%
  filter(question == "divrceok") %>%
  ggplot(aes(x = pattern, y = value)) + 
  geom_boxplot() + 
  geom_point(data = actual_counts %>% filter(question == "divrceok"), 
             aes(y = n), shape = 21, fill ="firebrick") +
  coord_flip()

lca_pred_3 <- lca_predictions_3 %>%
  gather(key = "key", value = "value", -c(question, x, pred_resp, pattern)) %>%
  ungroup() %>%
  select(question, pattern, value, key) %>%
  mutate(model = "lca_3") 

lca_ssd_3 <- lca_pred_3 %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  left_join(actual_counts) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(ssd = (value - count)^2) %>% 
  group_by(question, key) %>%
  summarise(sum_ssd = sum(ssd)) %>% mutate(model = "lca_3")


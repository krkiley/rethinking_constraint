
#Predict everything with people who are in wave 2
#Predict wave 3 beliefs for people who have covariates (don't need to have all beliefs!)

#2594 people at wave 2...
#2536 have all beliefs
b2_full <- b2 %>% na.omit()
#2571 have all controls
c2_full <- c2 %>% na.omit()
#2534 have both beliefs and controls all at wave 2
w2_full <- inner_join(b2_full, c2_full, by = "ids")

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

pred_prob_estimates_3 <- vector(mode = "list", length = length(attitude_vars))
for (i in 1:length(attitude_vars)) {
  var <- attitude_vars[i]
  multinom_df <- w2_full %>%
    select(ids, var, bntraev, bntramnl, bntracat, bntrajew, 
           bntraoth, bntradk, bntralds, bntraaf, attend,
           gender, midwest, south, west, agecats, compgrad,
           pshrblf)
  names(multinom_df)[2] <- "resp"
  multinom_df$resp <- as.factor(multinom_df$resp)
  
  m1 <- multinom(resp ~ bntraev + bntramnl + bntracat + bntrajew + 
                   bntraoth + bntradk + bntralds + bntraaf + 
                   attend + 
                   gender + midwest + south + west + 
                   agecats + compgrad +
                   pshrblf, data = multinom_df)
  pred_probs <- cbind(multinom_df %>%
                        select(ids, resp), predict(m1, cp, "probs")) %>%
    mutate(question = var)
  
  pred_prob_estimates_3[[i]] <- pred_probs
}

multinom_probs_3 <- left_join(long_blf, bind_rows(pred_prob_estimates_3)) %>%
  ungroup() %>%
  select(ids, question, resp, `1`, `2`, `3`, `4`, `5`) %>%
  mutate(`2` = ifelse(is.na(`2`), 0, `2`),
         `4` = ifelse(is.na(`4`), 0, `4`)) %>%
  filter(ids %in% c(c3_full$ids))

multinom_patterns_3 <- vector(mode = "list", length = 10)
for (i in 1:10) {
  multinom_count_3 <- multinom_probs %>%
    rowwise() %>%
    mutate(pred_resp = sample(1:5, 1, replace = TRUE, prob = c(`1`, `2`, `3`, `4`, `5`))) %>%
    select(ids, question, x, y, pred_resp) %>%
    filter(!is.na(y)) %>%
    mutate(pattern = paste(x, pred_resp, sep = "->")) %>%
    group_by(question, x, pred_resp, pattern) %>%
    summarise(n = n())
  names(multinom_count_3)[5] <- i
  multinom_patterns_3[[i]] <- multinom_count_3
}

for (i in 1:length(multinom_patterns_3)) {
  if (i == 1) {
    multinom_predictions_3 <- multinom_patterns_3[[i]]
  } else {
    multinom_predictions_3 <- full_join(multinom_predictions_3, multinom_patterns_3[[i]], by = c("question", "x", "pred_resp", "pattern"))
  }
}

mn_pred_3 <- multinom_predictions_3 %>%
  gather(key = "key", value = "value", -c(question, x, pred_resp, pattern)) %>%
  ungroup() %>%
  select(question, pattern, value, key) %>%
  mutate(model = "multinom_3")


mn_ssd_3 <- mn_pred_3 %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  left_join(actual_counts) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(ssd = (value - count)^2) %>% 
  group_by(question, key) %>%
  summarise(sum_ssd = sum(ssd)) %>%
  mutate(model = "mn_3")




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


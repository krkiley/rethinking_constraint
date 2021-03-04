
source("~/Dropbox/rethinking_constraint/clean_controls.R")
source("~/Dropbox/rethinking_constraint/clean_beliefs.R")
 
library(poLCA)

#2594 people at wave 2...
#2536 have all beliefs
b2_full <- b2 %>% na.omit()
#2564 have all controls
c2_full <- c2 %>% na.omit()
#2534 have both beliefs and controls all at wave 2
w2_full <- inner_join(b2_full, c2_full, by = "ids") %>%
  left_join(c1) %>% na.omit()

#What number of classes minimizes the BIC?
lca_results <- vector(mode = "list", length = 10)
lca_classes <- matrix(NA, nrow = nrow(w2_full), ncol = 10)
for (i in 2:10) {
  l1 <- poLCA(cbind(aftrlife, angels, demons, astrolgy, reincar, miracles, god,
                    heaven, godworld, moralrel, moralchg, brkmoral, 
                    relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
                    wrkngmom)~
                bntraev + bntramnl + bntracat + bntrajew + 
                bntraoth + bntradk + bntraaf + 
                attend + 
                bnpblack + bnpoth + parba + 
                parclose + gender + 
                south + 
                agecats + compgrad + pshrblf, 
              nclass = i, data = w2_full,
              maxiter = 5000)
  lca_results[[i]] <- data.frame(class = i, aic = l1$aic, bic = l1$bic, x2 = l1$Chisq)
  
  lca_classes[,i] <- l1$predclass
  
}

#Comparison of number of classes
bind_rows(lca_results) %>%
  gather(key = "measure", value = "value", -c(class)) %>%
  ggplot(aes(x = class, y = value, color = measure)) + 
  geom_line() + 
  facet_wrap(~measure, scales = "free")

#Best-fitting model with five classes
l5 <- poLCA(cbind(aftrlife, angels, demons, astrolgy, reincar, miracles, god,
                  heaven, godworld, moralrel, moralchg, brkmoral, 
                  relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
                  wrkngmom)~
              bntraev + bntramnl + bntracat + bntrajew + 
              bntraoth + bntradk + bntraaf + 
              attend + parba + 
              parclose + gender + 
              south + 
              agecats + compgrad + pshrblf, 
            nclass = 5, data = w2_full,
            maxiter = 5000)

#81390.17 is best-fitting model

#Visualization of class distribution
tidy(l5) %>%
  mutate(cat = ifelse(variable %in% c("aftrlife", "angels", "demons" ,"astrolgy", 
                                      "reincar", "godworld", "heaven", "god",
                                      "miracles"),
                      "religion",
                      ifelse(variable %in% c("moralrel", "moralchg", "brkmoral",
                                             "relprvte"),
                             "morality", "family"))) %>%
  ggplot(aes(x = variable, y = estimate, fill = as.factor(outcome))) + 
  geom_bar(stat = "identity", position = "stack", color = "black") + 
  facet_grid(class~cat, scales = "free_x", space = "free") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x = "", y = "", fill = "") + 
  scale_fill_brewer(type = "qual", palette = 1,
                    labels = c("Strongly Agree/Yes",
                               "Agree", "Don't Know/Maybe",
                               "Disagree",
                               "Strongly Disagree/No")) 

save(l5, file = "~/Dropbox/rethinking_constraint/lca5.Rdata")


w2_full %>% 
  mutate(class = l5$predclass) %>%
  select(ids, bntraev:class) 

#Testing Hypothesis 1 and 2: Over time standard error as a function 
# of within-group standard deviation 
l2 <- b2 %>%
  gather(key = "question", value = "w2", -ids)

l3 <- b3 %>%
  gather(key = "question", value = "w3", -ids)

l4 <- b4 %>%
  gather(key = "question", value = "w4", -ids)

#Calculating within-person standard deviation over time
#For people with t=3
within_person <- full_join(l2, l3, by = c("ids", "question")) %>%
  full_join(l4, by = c("ids", "question")) %>%
  gather(key = "key", value = "value", -c(ids, question)) %>%
  mutate(value_34 = ifelse(key == "w2", 0, value)) %>%
  group_by(ids, question) %>%
  summarise(sd = sd(value, na.rm = TRUE), mean = sum(value_34, na.rm = TRUE)/2,
            count = sum(!is.na(value))) %>%
  filter(count > 2)

#Create a long version of the beliefs in the w2_full data set
within_group <- w2_full %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, godworld, moralrel, moralchg, brkmoral, 
         relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
         wrkngmom) %>%
  mutate(predclass = l5$predclass) %>%
  gather(key = "question", value = "resp", -c(ids, predclass)) %>%
  group_by(question, predclass) %>%
  mutate(grp_sd = sd(resp, na.rm = TRUE),
         grp_mean = mean(resp, na.rm = TRUE)) 

#Put those two together
long_blf <- left_join(within_person, within_group) %>%
  filter(!is.na(predclass)) %>%
  filter(!is.na(sd)) 

save(long_blf, file = "~/Dropbox/rethinking_constraint/long_blf.Rdata")

#Make data for plotting
plot_data <- long_blf %>%
  group_by(predclass, question, grp_sd, grp_mean) %>%
  summarise(mean_sd = mean(sd, na.rm = TRUE),
            mean_mean = mean(mean, na.rm = TRUE)) %>%
  mutate(predclass = recode(predclass, "1"="Skeptics",
                            "2"="Unconstrained", 
                            "3"="Moderates",
                            "4"="Believers",
                            "5"="Ambivalents")) 
  
save(plot_data, file = "~/Dropbox/rethinking_constraint/plot_data.Rdata")

plot_data %>%
  ggplot(aes(x = grp_sd, y = mean_sd, fill = as.factor(predclass))) + 
  geom_abline(slope = 1, linetype = 2, color = "gray") + 
  geom_point(shape = 21) + 
  geom_text_repel(aes(label = question), size = 2) + 
  labs(x = "Within-class S.D., time 1",
       y = "Mean within-person S.D., times 1-3",
       fill = "Class") +
  theme_minimal() + 
  scale_fill_brewer(type = "qual")

plot_data %>% ungroup() %>%
  summarise(cor = cor(grp_sd, mean_sd, use = "pairwise.complete.obs"))

plot_data %>%
  ggplot(aes(x = grp_mean, y = mean_mean, fill = as.factor(predclass))) + 
  geom_abline(slope = 1, linetype = 2, color = "gray") + 
  geom_point(shape = 21) + 
  geom_text_repel(aes(label = question), size = 2) + 
  labs(x = "Within-class mean, time 1",
       y = "Average within-person mean, times 2,3",
       fill = "Class") +
  theme_minimal() + 
  scale_fill_brewer(type = "qual")


t <- data.frame(long_blf)
pdata <- pdata.frame(x = t, index = c("ids", "question"))

m1 <- plm(sd ~ grp_sd, # + question, 
          model = "within",
          effect = "twoway",
          data = pdata)

m2 <- plm(mean ~ grp_mean, 
          model = "within", 
          effect = "twoway",
          data = pdata)

texreg(list(m1, m2))

################################################
######### PATTERN PREDICTION COMPONENT #########
################################################

prediction_iters <- 1000

####### Predicting based on w3 marginals
marginal_percents <- observed_responses %>%
  group_by(question, w3) %>%
  summarise(pct = n()) %>%
  mutate(pct = pct/sum(pct)) %>%
  spread(w3, pct) %>%
  mutate(`2`= ifelse(is.na(`2`), 0, `2`),
         `4`= ifelse(is.na(`4`), 0, `4`))

marginal_patterns <- vector(mode = "list", length = prediction_iters)
for (i in 1:prediction_iters) {
  marginal_count <- left_join(observed_responses, marginal_percents) %>%
    rowwise() %>%
    mutate(pred_resp_3 = sample(1:5, 1, replace = TRUE, prob = c(`1`, `2`, `3`, `4`, `5`))) %>%
    select(ids, question, w2, pred_resp_3) %>%
    mutate(pattern = paste(w2, pred_resp_3, sep = "->")) %>%
    group_by(question, w2, pred_resp_3, pattern) %>%
    summarise(n = n())
  
  names(marginal_count)[5] <- i
  marginal_patterns[[i]] <- marginal_count
}

for (i in 1:length(marginal_patterns)) {
  if (i == 1) {
    marginal_predictions <- marginal_patterns[[i]]
  } else {
    marginal_predictions <- full_join(marginal_predictions, marginal_patterns[[i]], by = c("question", "w2", "pred_resp_3", "pattern"))
  }
}

marginal_pred <- marginal_predictions %>%
  gather(key = "key", value = "value", -c(question, w2, pred_resp_3, pattern)) %>%
  ungroup() %>%
  select(question, pattern, key, value) %>%
  mutate(model = "marginal")



########## Predicting Responses based on Latent Classes
observed_responses <- 
  inner_join(w2_f, 
             w3_withcov %>% gather(key = "question", value = "w3", -ids)) %>%
  na.omit() %>%
  mutate(observed_pat = paste(w2, w3, sep = "->")) 

two_waves <- unique(observed_responses$ids)

observed_responses_count <- observed_responses %>%
  group_by(question, observed_pat) %>% summarise(count = n())

lca_patterns <- vector(mode = "list", length = prediction_iters)
for (i in 1:prediction_iters) {
  
  p <- cbind(w2_full, l5$posterior) %>%
    filter(ids %in% two_waves)
  
  classes <- p %>%
    rowwise() %>%
    mutate(class = sample(1:5, 1, replace = TRUE, prob = c(`1`, `2`, `3`, `4`, `5`))) %>%
    select(ids, class)
  
  #For each question, sample from the probabilities of giving each response
  lca_question_class <- observed_responses %>%
    left_join(classes, by = "ids") 
  
  rprobs <- tidy(l5) %>% 
    mutate(question = variable) %>%
    select(question, class, outcome, estimate) %>%
    spread(outcome, estimate)
  
  pred_resp <- left_join(lca_question_class, rprobs, by = c("question", "class")) %>%
    rowwise() %>%
    mutate(pred_resp_3 = sample(1:5, 1, replace = TRUE, prob = c(`1`, `2`, `3`, `4`, `5`))) %>%
    select(ids, question, w2, pred_resp_3)
  
  patterns <- pred_resp %>%
    mutate(pattern = paste(w2, pred_resp_3, sep = "->")) %>%
    group_by(question, w2, pred_resp_3, pattern) %>%
    summarise(n = n())
  
  names(patterns) <- c("question", "w2", "pred_resp_3", "pattern", i)
  
  lca_patterns[[i]] <- patterns
}

for (i in 1:length(lca_patterns)) {
  if (i == 1) {
    lca_predictions <- lca_patterns[[i]]
  } else {
    lca_predictions <- full_join(lca_predictions, lca_patterns[[i]], by = c("question", "w2", "pred_resp_3", "pattern"))
  }
}

lca_pred <- lca_predictions %>%
  gather(key = "key", value = "value", -c(question, w2, pred_resp_3, pattern)) %>%
  ungroup() %>%
  select(question, pattern, key, value) %>%
  mutate(model = "lca")


### Multinomial from just the covariates
attitude_vars <- c("aftrlife", "angels", "demons", "astrolgy", "reincar", "miracles", "god",
                   "heaven", "godworld", "moralrel", "moralchg", "brkmoral", 
                   "relprvte", "unmarsex", "divrceok", "manmar", "wommar", "mandecid", 
                   "wrkngmom")

pred_prob_estimates <- vector(mode = "list", length = length(attitude_vars))
for (i in 1:length(attitude_vars)) {
  var <- attitude_vars[i]
  multinom_df <- w2_full %>%
    select(ids, var, bntraev, bntramnl, bntracat, bntrajew, 
           bntraoth, bntradk, bntraaf, attend, bnpblack, bnpoth,
           parba, parclose, gender, south, west, midwest, 
           agecats, compgrad, pshrblf)
  names(multinom_df)[2] <- "resp"
  multinom_df$resp <- as.factor(multinom_df$resp)
  
  m1 <- multinom(resp ~ bntraev + bntramnl + bntracat + bntrajew + 
                   bntraoth + bntradk + bntraaf + 
                   attend + parba + parclose + gender + 
                   south + agecats + compgrad + pshrblf, 
                 data = multinom_df)
  pred_probs <- cbind(multinom_df %>%
                        select(ids, resp), predict(m1, multinom_df, "probs")) %>%
    mutate(question = var)
  
  pred_prob_estimates[[i]] <- pred_probs
}


multinom_probs <- left_join(observed_responses, bind_rows(pred_prob_estimates), 
          by = c("ids", "question")) %>%
  select(ids, question, w2, `1`, `2`, `3`, `4`, `5`) %>%
  mutate(`2` = ifelse(is.na(`2`), 0, `2`),
         `4` = ifelse(is.na(`4`), 0, `4`)) 

multinom_patterns <- vector(mode = "list", length = prediction_iters)
for (i in 1:prediction_iters) {
  multinom_count <- multinom_probs %>%
    rowwise() %>%
    mutate(pred_resp_3 = sample(1:5, 1, replace = TRUE, prob = c(`1`, `2`, `3`, `4`, `5`))) %>%
    select(ids, question, w2, pred_resp_3) %>%
    mutate(pattern = paste(w2, pred_resp_3, sep = "->")) %>%
    group_by(question, w2, pred_resp_3, pattern) %>%
    summarise(n = n())
  names(multinom_count)[5] <- i
  multinom_patterns[[i]] <- multinom_count
}

for (i in 1:length(multinom_patterns)) {
  if (i == 1) {
    multinom_predictions <- multinom_patterns[[i]]
  } else {
    multinom_predictions <- full_join(multinom_predictions, multinom_patterns[[i]], by = c("question", "w2", "pred_resp_3", "pattern"))
  }
}

mn_pred <- multinom_predictions %>%
  gather(key = "key", value = "value", -c(question, w2, pred_resp_3, pattern)) %>%
  ungroup() %>%
  select(question, pattern, value, key) %>%
  mutate(model = "multinom")


#Multinomial prediction using wave 3 covariates
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

multinom_patterns_3 <- vector(mode = "list", length = prediction_iters)
for (i in 1:prediction_iters) {
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

cp_mat <- cp %>% 
  mutate(intercept = 1) %>%
  select(intercept, bntraev, bntramnl, bntracat, bntrajew,
         bntraoth, bntradk, bntraaf, attend, parba, parclose, 
         gender, south, agecats, compgrad, pshrblf) %>%
  as.matrix()

w3_class_probs <- cp_mat %*% l5$coeff %>%
  as.data.frame() %>%
  mutate(prob1 = exp(0)/(1 + exp(V1) + exp(V2) + exp(V3) + exp(V4)),
         prob2 = exp(V1)/(1 + exp(V1) + exp(V2) + exp(V3) + exp(V4)),
         prob3 = exp(V2)/(1 + exp(V1) + exp(V2) + exp(V3) + exp(V4)),
         prob4 = exp(V3)/(1 + exp(V1) + exp(V2) + exp(V3) + exp(V4)),
         prob5 = exp(V4)/(1 + exp(V1) + exp(V2) + exp(V3) + exp(V4))) %>%
  mutate(ids = cp$ids) %>%
  select(ids, prob1, prob2, prob3, prob4, prob5)


########## Predicting Responses based on Latent Classes
lca_patterns_3 <- vector(mode = "list", length = prediction_iters)
for (i in 1:prediction_iters) {
  p <- w3_class_probs
  classes <- p %>%
    rowwise() %>%
    mutate(class = sample(1:5, 1, replace = TRUE, 
                          prob = c(prob1, prob2, prob3, prob4, prob5))) %>%
    select(ids, class)
  
  #For each question, sample from the probabilities of giving each response
  lca_question_class <- observed_responses %>%
    left_join(classes, by = "ids") 
  
  rprobs <- tidy(l5) %>% 
    mutate(question = variable) %>%
    select(question, class, outcome, estimate) %>%
    spread(outcome, estimate)
  
  pred_resp <- left_join(lca_question_class, rprobs, by = c("question", "class")) %>%
    rowwise() %>%
    mutate(pred_resp_3 = sample(1:5, 1, replace = TRUE, prob = c(`1`, `2`, `3`, `4`, `5`))) %>%
    select(ids, question, w2, pred_resp_3)
  
  patterns <- pred_resp %>%
    mutate(pattern = paste(w2, pred_resp_3, sep = "->")) %>%
    group_by(question, w2, pred_resp_3, pattern) %>%
    summarise(n = n())
  
  names(patterns) <- c("question", "w2", "pred_resp_3", "pattern", i)
  
  lca_patterns_3[[i]] <- patterns
}
#For each person. 
#Sample from their posterior probs a class
#Assign 

for (i in 1:length(lca_patterns_3)) {
  if (i == 1) {
    lca_predictions_3 <- lca_patterns_3[[i]]
  } else {
    lca_predictions_3 <- full_join(lca_predictions_3, lca_patterns_3[[i]], by = c("question", "w2", "pred_resp_3", "pattern"))
  }
}


lca_pred_3 <- lca_predictions_3 %>%
  gather(key = "key", value = "value", -c(question, w2, pred_resp_3, pattern)) %>%
  ungroup() %>%
  select(question, pattern, key, value) %>%
  mutate(model = "lca_3") 


#What is the actual count of responses
actual_counts <- observed_responses_count

#How off is each model?
marginal_ssd <- marginal_pred %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  left_join(actual_counts, by = c("question", "pattern"="observed_pat")) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(ssd = sqrt((value - count)^2)) %>% 
  group_by(question, key) %>%
  summarise(sum_ssd = sum(ssd)) %>%
  mutate(model = "1. Marginal distribution")

lca_ssd <- lca_pred %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  left_join(actual_counts, by = c("question", "pattern"="observed_pat")) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(ssd = sqrt((value - count)^2)) %>% 
  group_by(question, key) %>%
  summarise(sum_ssd = sum(ssd)) %>%
  mutate(model = "3. Belief system (LCA)")

mn_ssd <- mn_pred %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  left_join(actual_counts, by = c("question", "pattern"="observed_pat")) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(ssd = sqrt((value - count)^2)) %>% 
  group_by(question, key) %>%
  summarise(sum_ssd = sum(ssd)) %>%
  mutate(model = "2. Idiosyncratic beliefs (Multinomial)")

mn_ssd_3 <- mn_pred_3 %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  left_join(actual_counts, by = c("question", "pattern"="observed_pat")) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(ssd = sqrt((value - count)^2)) %>% 
  group_by(question, key) %>%
  summarise(sum_ssd = sum(ssd)) %>%
  mutate(model = "4. Idiosyncratic beliefs (Multinomial) with T2 covariates")

lca_ssd_3 <- lca_pred_3 %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  left_join(actual_counts, by = c("question", "pattern"="observed_pat")) %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  mutate(ssd = sqrt((value - count)^2)) %>% 
  group_by(question, key) %>%
  summarise(sum_ssd = sum(ssd)) %>%
  mutate(model = "5. Belief System (LCA) with T2 covariates")

prediction_error <- bind_rows(marginal_ssd, mn_ssd, lca_ssd, mn_ssd_3, 
                               lca_ssd_3)

save(prediction_error, file = "~/Dropbox/rethinking_constraint/prediction_error.Rdata")

save(mn_ssd, file = "~/Dropbox/rethinking_constraint/mn_ssd.Rdata")
save(lca_ssd, file = "~/Dropbox/rethinking_constraint/lca_ssd.Rdata")

bind_rows(mn_ssd, lca_ssd, marginal_ssd, mn_ssd_3, 
          lca_ssd_3) %>%
  mutate(sd = sum_ssd) %>%
  ggplot(aes(x = model, y = sd, fill = model)) + 
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) + 
  #coord_flip() +
  facet_wrap(~question, scales = "free") + 
  theme_bw() +
  labs(y = "Sum of squared deviations from expected count", y = "",
       fill = "") + 
  scale_fill_brewer(type = "qual", palette = "Paired") +
  geom_vline(xintercept = 3.5, linetype = 2, color = "black")



bind_rows(mn_ssd, lca_ssd, marginal_ssd, mn_ssd_3, 
          lca_ssd_3) %>%
  group_by(model, question) %>%
  summarise(mean = mean(sum_ssd), p975 = quantile(sum_ssd, .975),
            p025 = quantile(sum_ssd, .025)) %>%
  ggplot(aes(x = model, y = mean, fill = model)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = p025, ymax = p975)) + 
  facet_wrap(~question, scales = "free") + 
  theme_bw() + 
  


#Marginal ... Prediction





one_2_one_pred <- long_blf %>%
  filter(ids %in% w2_full$ids) %>%
  filter(!is.na(y)) %>%
  mutate(pred_resp = x) %>%
  mutate(pattern = paste(x, pred_resp, sep = "->")) %>%
  group_by(question, pattern) %>%
  summarise(pred_n = n()) %>%
  full_join(actual_counts) %>%
  mutate(pred_n = ifelse(is.na(pred_n), 0, pred_n),
         count = ifelse(is.na(count), 0, count)) %>%
  mutate(ssd = (pred_n - count)^2) %>%
  group_by(question) %>%
  summarise(sum_ssd = sum(ssd))



m1 <-  g



lca_predictions %>%
  gather(key = "key", value = "value", -c(question, x, pred_resp, pattern)) %>%
  ungroup() %>%
  group_by(question, pred_resp, key) %>% summarise(n = sum(value, na.rm=TRUE))
  select(question, pattern, value, key) %>%
  mutate(model = "lca")

lca_predictions  
  


lca_predictions
  
actual_counts <- long_blf %>% ungroup() %>%
  filter(ids %in% w2_full$ids) %>%
  filter(!is.na(y)) %>%
  select(question, x, y) %>%
  mutate(pattern = paste(x, y, sep = "->")) %>%
  group_by(question, pattern) %>%
  summarise(count = n())

marginal_probabilities <- long_blf %>% ungroup() %>%
  filter(ids %in% w2_full$ids) %>%
  filter(!is.na(y)) %>%
  group_by(question, y) %>%
  summarise(count = n()) %>%
  group_by(question) %>%
  mutate(pct = count/sum(count)) %>%
  select(-count) %>%
  spread(y, pct)






bind_rows(mn_pred, lca_pred) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  filter(question %in% c("divrceok", "moralrel")) %>%
  ggplot(aes(x = pattern, y = value, fill = model)) + 
  geom_boxplot() + 
  geom_point(data = actual_counts %>% filter(question %in% c("divrceok", "moralrel")), 
             aes(y = n), shape = 21, fill ="firebrick") +
  coord_flip() + 
  facet_wrap(~question, scales = "free")

vq <- n2 %>% select(ids, howdecid)

w2_full %>%
  mutate(class = l5$predclass) %>%
  mutate(class = recode(class, "1"="Skeptics",
                "2"="Unconstrained", 
                "3"="Moderates",
                "4"="True Believers",
                "5"="Ambivalents")) %>%
  left_join(vq) %>%
  group_by(class, howdecid) %>%
  summarise(n = n()) %>% group_by(howdecid) %>%
  mutate(n = n/sum(n)) %>%
  spread(class, n)





n4$moralitychnge_w4

l2 <- b2 %>%
  gather(key = "question", value = "w2", -ids)

l3 <- b3 %>%
  gather(key = "question", value = "w3", -ids)

l4 <- b4 %>%
  gather(key = "question", value = "w4", -ids)

#People over time
sds <- full_join(l2, l3, by = c("ids", "question")) %>%
  full_join(l4, by = c("ids", "question")) %>%
  gather(key = "key", value = "value", -c(ids, question)) %>%
  group_by(ids, question) %>%
  summarise(sd = sd(value, na.rm = TRUE), mean = mean(value, na.rm = TRUE),
            count = n()) %>%
  filter(count > 2)

#Groups
sd_data <- left_join(long_blf, sds, by = c("ids", "question")) %>%
  mutate(group_mean = mean(x, na.rm = TRUE)) %>%
  group_by(predclass, question, grp_sd, group_mean) %>%
  summarise(mean_sd = mean(sd, na.rm = TRUE),
            mean_mean = mean(mean, na.rm = TRUE)) %>%
  mutate(predclass = recode(predclass, "1"="Ambivalents",
                        "2"="Atheiest/Agnostics", 
                        "3"="Mainline Protestants",
                        "4"="Constrained Christians",
                        "5"="Unconstrained")) 

save(sd_data, file = "~/Dropbox/rethinking_constraint/sd_data.Rdata")

sd_data %>%
  ggplot(aes(x = grp_sd, y = mean_sd, fill = as.factor(predclass))) + 
  geom_abline(slope = 1, linetype = 2, color = "gray") + 
  geom_point(shape = 21) + 
  geom_text_repel(aes(label = question), size = 2) + 
  labs(x = "Within-class S.D., time 1",
       y = "Mean within-person S.D., times 1-3",
       fill = "Class") +
  theme_minimal() + 
  scale_fill_brewer(type = "qual")


sd_data %>%
  ggplot(aes(x = group_mean, y = mean_mean, fill = as.factor(predclass))) + 
  geom_abline(slope = 1, linetype = 2, color = "gray") + 
  geom_point(shape = 21) + 
  geom_text_repel(aes(label = question), size = 2) + 
  labs(x = "Within-class mean, time 1",
       y = "Average within-person mean, times 1-3",
       fill = "Class") +
  theme_minimal() + 
  scale_fill_brewer(type = "qual")


t <- left_join(long_blf, sds, by = c("ids", "question")) %>%
  group_by(question, predclass) %>%
  mutate(group_mean = mean(x, na.rm = TRUE))

pdata <- pdata.frame(x = t, index = c("ids"))

m1 <- plm(sd ~ grp_sd + question, 
          model = "within",
          effect = "individual",
          data = pdata)


m2 <- plm(mean ~ group_mean + question, 
          model = "within", 
          effect = "individual",
          data = pdata)


cor(sd_data$grp_sd, sd_data$mean_sd)

left_join(long_blf, sds, by = c("ids", "question")) %>%
  group_by(predclass, question) %>%
  summarise(grp_sd = sd(x, na.rm = TRUE),
            mean_sd = mean(sd, na.rm = TRUE)) %>%
  group_by(predclass) %>%
  summarise(cor = cor(grp_sd, mean_sd))


vaiseyq <- n2 %>%
  select(ids, howdecid)

vaisey_test <- w2_full %>%
  mutate(class = l5$predclass) %>%
  left_join(vaiseyq) %>%
  filter(howdecid != 5)

m2 <- multinom(howdecid ~ as.factor(class), data = vaisey_test)

tidy(m2)



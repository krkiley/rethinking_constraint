





class_covs_ind <- w2_full %>%
  select(ids, bntraev:parba) %>%
  mutate(female = gender,
         male = 1-gender,
         notsouth = 1-south,
         parnoba = 1-parba) %>%
  mutate(class = l5$predclass) %>%
  mutate(class = recode(class, "1"="Agnostic/Athiests",
                        "2"="Unconstrained", 
                        "3"="Mainline Protestants",
                        "4"="Constrained Christians",
                        "5"="Ambivalents")) %>%
  gather(key = "question", value = "value", 
         -c(ids, class)) %>%
  group_by(question, class) %>%
  summarise(n = sum(value)) %>%
  #filter(question == "bntraev") %>%
  group_by(question) %>%
  mutate(n = n/sum(n)) %>%
  spread(class, n) %>% 
  filter(question %in% c("bntraaf", "bntracat", "bntradk", "bntraev",
                         "bntrajew", "bntramnl", "bntranor",
                         "bntraoth", "male", "female", "south", 
                         "notsouth", "parba", "parnoba"))


class_cov_cont <- w2_full %>% select(ids, attend, agecats, compgrad, pshrblf, parclose) %>%
  mutate(class = l5$predclass) %>%
  mutate(class = recode(class, "1"="Agnostic/Athiests",
                        "2"="Unconstrained", 
                        "3"="Mainline Protestants",
                        "4"="Constrained Christians",
                        "5"="Ambivalents")) %>%
  gather(key = "question", value = "value", 
         -c(ids, class)) %>%
  group_by(question, class) %>%
  summarise(mean = mean(value)) %>%
  mutate(mean = ifelse(question == "agecats", mean + 16,
                       ifelse(question == "compgrad", mean + 9, mean))) %>%
  spread(class, mean) 
  
  
class_props <- data.frame(question = "class props", 
           class = c(1,2,3,4,5), value = colMeans(l5$posterior)) %>%
  mutate(class = recode(class, "1"="Agnostic/Athiests",
                        "2"="Unconstrained", 
                        "3"="Mainline Protestants",
                        "4"="Constrained Christians",
                        "5"="Ambivalents")) %>%
  spread(class, value) 


cov_table <- bind_rows(class_props, class_covs_ind, class_cov_cont)
cov_table[,-1] <-round(cov_table[,-1],2)




knitr::kable(cov_table, format = "latex")



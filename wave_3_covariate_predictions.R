
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


mn_pred_3 %>% group_by(question, key) %>%
  summarise(n = sum(value, na.rm = TRUE)) %>% spread(key, n)





#predictable <- c3_full %>% filter(ids %in% c(w2_full$ids))



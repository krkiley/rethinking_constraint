

library(haven)
library(poLCA)


g6 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel06.dta")


civlibs <- g6 %>%
  select(id_1, degree_1,
         spkath_1, colath_1, libath_1, 
         spkrac_1, colrac_1, librac_1,
         spkcom_1, colcom_1, libcom_1,
         spkmil_1, colmil_1, libmil_1,
         spkhomo_1, colhomo_1, libhomo_1) %>%
  mutate(colath_1 = ifelse(colath_1 == 4, 1, 2),
         colrac_1 = ifelse(colrac_1 == 4, 1, 2),
         colcom_1 = ifelse(colcom_1 == 4, 1, 2),
         colmil_1 = ifelse(colmil_1 == 4, 1, 2),
         colhomo_1 = ifelse(colhomo_1 == 4, 1, 2)) %>%
  mutate(degree = ifelse(degree_1 %in% c(3, 4), 1, 0)) %>%
  na.omit()


lca_results <- vector(mode = "list", length = 10)
lca_classes <- matrix(NA, nrow = nrow(civlibs), ncol = 10)
for (i in 2:10) {
  l1 <- poLCA(cbind(spkath_1, colath_1, libath_1, 
                      spkrac_1, colrac_1, librac_1,
                      spkcom_1, colcom_1, libcom_1,
                      spkmil_1, colmil_1, libmil_1,
                      spkhomo_1, colhomo_1, libhomo_1)~degree,
                data = civlibs, nclass = i,
              maxiter = 5000)
  lca_results[[i]] <- data.frame(class = i, aic = l1$aic, bic = l1$bic, x2 = l1$Chisq)
  
  lca_classes[,i] <- l1$predclass
  
}

bind_rows(lca_results) %>%
  gather(key = "measure", value = "value", -c(class)) %>%
  ggplot(aes(x = class, y = value, color = measure)) + 
  geom_line() + 
  facet_wrap(~measure, scales = "free")


l4 <- poLCA(cbind(spkath_1, colath_1, libath_1, 
                  spkrac_1, colrac_1, librac_1,
                  spkcom_1, colcom_1, libcom_1,
                  spkmil_1, colmil_1, libmil_1,
                  spkhomo_1, colhomo_1, libhomo_1)~1,
            data = civlibs, nclass = 4,
            maxiter = 5000)


tidy(l4) %>%
  ggplot(aes(x = variable, y = estimate, fill = as.factor(outcome))) + 
  geom_bar(stat = "identity", position = "stack", color = "black") + 
  facet_grid(class~., scales = "free_x", space = "free") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x = "", y = "", fill = "") + 
  scale_fill_brewer(type = "qual", palette = 1) 



civlibs_w1 <- civlibs %>%
  mutate(class = l4$predclass) %>%
  gather(key = "question", value = "value", -c(id_1, class, degree, degree_1)) %>%
  group_by(class, question) %>%
  mutate(grp_sd = sd(value, na.rm = TRUE)) %>%
  mutate(question = gsub("_1", "", question))




civlibs_time <- g6 %>%
  select(id_1, spkath_1, colath_1, libath_1, 
         spkrac_1, colrac_1, librac_1,
         spkcom_1, colcom_1, libcom_1,
         spkmil_1, colmil_1, libmil_1,
         spkhomo_1, colhomo_1, libhomo_1,
         spkath_2, colath_2, libath_2, 
         spkrac_2, colrac_2, librac_2,
         spkcom_2, colcom_2, libcom_2,
         spkmil_2, colmil_2, libmil_2,
         spkhomo_2, colhomo_2, libhomo_2,
         spkath_3, colath_3, libath_3, 
         spkrac_3, colrac_3, librac_3,
         spkcom_3, colcom_3, libcom_3,
         spkmil_3, colmil_3, libmil_3,
         spkhomo_3, colhomo_3, libhomo_3) %>%
  mutate(colath_1 = ifelse(colath_1 == 4, 1, 2),
         colrac_1 = ifelse(colrac_1 == 4, 1, 2),
         colcom_1 = ifelse(colcom_1 == 4, 1, 2),
         colmil_1 = ifelse(colmil_1 == 4, 1, 2),
         colhomo_1 = ifelse(colhomo_1 == 4, 1, 2),
         colath_2 = ifelse(colath_2 == 4, 1, 2),
         colrac_2 = ifelse(colrac_2 == 4, 1, 2),
         colcom_2 = ifelse(colcom_2 == 4, 1, 2),
         colmil_2 = ifelse(colmil_2 == 4, 1, 2),
         colhomo_2 = ifelse(colhomo_2 == 4, 1, 2),
         colath_3 = ifelse(colath_3 == 4, 1, 2),
         colrac_3 = ifelse(colrac_3 == 4, 1, 2),
         colcom_3 = ifelse(colcom_3 == 4, 1, 2),
         colmil_3 = ifelse(colmil_3 == 4, 1, 2),
         colhomo_3 = ifelse(colhomo_3 == 4, 1, 2)) %>%
  gather(key = "question", value = "value", -id_1) %>%
  separate(question, into = c("question", "wave")) %>%
  group_by(id_1, question) %>% 
  summarise(person_sd = sd(value, na.rm = TRUE),
            n = n()) %>%
  filter(n > 2)




joined <- left_join(civlibs_w1, civlibs_time, by = c("id_1", "question")) 

joined %>%
  group_by(class, question, grp_sd) %>%
  summarise(avg_sd = mean(person_sd, na.rm = TRUE)) %>%
  ggplot(aes(x = grp_sd, y = avg_sd, fill = as.factor(class))) + 
  geom_abline(slope = 1, color = "gray", linetype = 2) + 
  geom_point(shape = 21) + 
  geom_text_repel(aes(label = question), size = 3)

left_join(civlibs_w1, civlibs_time, by = c("id_1", "question")) %>%
  group_by(class, question, grp_sd) %>%
  summarise(avg_sd = mean(person_sd, na.rm = TRUE)) %>%
  group_by(class) %>%
  summarise(cor = cor(grp_sd, avg_sd))


library(plm)

t <- data.frame(joined)
pdata <- pdata.frame(x = t, index = c("id_1", "question"))

p1 <- plm(person_sd ~ grp_sd, 
          index = c("question", "id_1"), 
          effect = "twoways", 
          model = "within",
          data = pdata)



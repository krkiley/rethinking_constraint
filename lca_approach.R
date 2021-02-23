
source("~/Dropbox/hill_kreisi/functions/model_function.R")


library(haven)
#Load in NSYR
n1 <- read_dta("~/Dropbox/data/nsyr/nsyr1.DTA")
n2 <- read_dta("~/Dropbox/data/nsyr/nsyr2.DTA")
n3 <- read_dta("~/Dropbox/data/nsyr/nsyr3.DTA")
n4 <- read_dta("~/Dropbox/data/nsyr/nsyr4.dta")

nnw <- read_dta("~/Dropbox/data/nsyr/w1-w3 network variables.dta")

controls <- n2 %>% 
  select(ids, bntraev, bntramnl, bntracat, bntrajew,
         bntranor, bntraoth, bntradk, bntralds, bntraaf,
         bntraafm,
         gender, ATTEND1, CEN_DIV, 
         agecats, compgrad, frnds, frrelblf, frnotrel) %>%
  mutate(attend = ifelse(ATTEND1 == 999, 0, ifelse(ATTEND1 == 666, NA_real_, ATTEND1)),
         northeast = ifelse(CEN_DIV %in% c(1,2), 1,0),
         midwest = ifelse(CEN_DIV %in% c(3,4), 1, 0),
         south = ifelse(CEN_DIV %in% c(5,6,7), 1, 0),
         west = ifelse(CEN_DIV %in% c(8,9), 1, 0),
         compgrad = ifelse(compgrad > 19, NA,
                           ifelse(compgrad < 9, 9,
                                  ifelse(compgrad >= 16, compgrad - 3, 
                                         compgrad))),
         compgrad = compgrad - 9,
         frnds = ifelse(frnds == 666, NA_real_, frnds),
         fshrblf = ifelse(bntranor == 1, frnotrel, frrelblf),
         fshrblf = ifelse(fshrblf %in% c(666,888), NA_real_,
                          ifelse(fshrblf %in% c(777,999), 0, fshrblf)),
         pshrblf = fshrblf/frnds,
         agecats = agecats - 16) %>%
  select(ids, bntraev, bntramnl, bntracat, bntrajew,
         bntranor, bntraoth, bntradk, bntralds, bntraaf, bntraafm,
         gender, attend, midwest, south, west, agecats,
         compgrad, frnds, fshrblf, pshrblf) %>%
  mutate(bntraaf = ifelse(bntraafm == 1, 1, bntraaf))
  
b2 <- n2 %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, wrldorig, goduseev, moralrel, moralchg, brkmoral,
         relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
         wrkngmom) %>%
  zap_labels() %>%
  mutate(aftrlife = recode(aftrlife, "1"=1, "2"=3, "3"=5,
                           "777"=3, "666"=NA_real_, "888"=NA_real_),
         angels = recode(angels, "1"=1, "2"=3, "3"=5,
                         "777"=3, "666"=NA_real_, "888"=NA_real_),
         demons = recode(demons, "1"=1, "2"=3, "3"=5,
                         "777"=3, "666"=NA_real_, "888"=NA_real_),
         astrolgy = recode(astrolgy, "1"=1, "2"=3, "3"=5,
                           "777"=3, "666"=NA_real_, "888"=NA_real_),
         reincar = recode(reincar, "1"=1, "2"=3, "3"=5,
                          "777"=3, "666"=NA_real_, "888"=NA_real_),
         miracles = recode(miracles, "1"=1, "2"=3, "3"=5,
                           "777"=3, "666"=NA_real_, "888"=NA_real_),
         god = recode(god, "1"=1, "2"=5, "3"=3, "777"=2, "666"=NA_real_),
         heaven = recode(heaven, "1"=1, "0"=5, "666"=NA_real_, "777"=3, "888"=NA_real_),
         godworld = recode(wrldorig, "1"=1, "2"=5, "3"=3, "4"=3,
                           "666"=NA_real_, "777"=3, "888"=NA_real_),
         moralrel = ifelse(moralrel %in% c(666,888), NA_real_, moralrel),
         moralchg = ifelse(moralchg %in% c(666,888), NA_real_, moralchg),
         brkmoral = ifelse(brkmoral %in% c(666,888), NA_real_, brkmoral),
         relprvte = ifelse(relprvte %in% c(666,888), NA_real_, relprvte),
         manmar = ifelse(manmar %in% c(666,888), NA_real_, manmar),
         wommar = ifelse(wommar %in% c(666,888), NA_real_, wommar),
         mandecid = ifelse(mandecid %in% c(666,888), NA_real_, mandecid),
         wrkngmom = ifelse(wrkngmom %in% c(666,888), NA_real_, wrkngmom),
         unmarsex = ifelse(unmarsex %in% c(666,888), NA_real_, unmarsex),
         divrceok = recode(divrceok, "1"=1, "2"=5, "777"=3, "666"=NA_real_,
                           "888"=NA_real_)) %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, godworld, moralrel, moralchg, brkmoral, 
         relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
         wrkngmom) %>%
  mutate(wave = "2")


b3 <- n3 %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, wrldorig, goduseev, moralrel, moralchg, brkmoral,
         relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
         wrkngmom, bntraev, bntramnl, bntracat, bntrajew,
         bntranr, bntraoth, bntraica, bntraicn, bntraia,
         bntralds, bntrablm, bntrable, gender, ATTEND1) %>%
  zap_labels() %>%
  mutate(aftrlife = recode(aftrlife, "1"=1, "2"=3, "3"=5,
                           "777"=3, "888"=NA_real_, "666"=NA_real_,
                           "999"=NA_real_),
         angels = recode(angels, "1"=1, "2"=3, "3"=5,
                         "777"=3, "888"=NA_real_, "666"=NA_real_,
                         "999"=NA_real_),
         demons = recode(demons, "1"=1, "2"=3, "3"=5,
                         "777"=3, "888"=NA_real_, "666"=NA_real_,
                         "999"=NA_real_),
         astrolgy = recode(astrolgy, "1"=1, "2"=3, "3"=5,
                           "777"=3, "888"=NA_real_, "666"=NA_real_,
                           "999"=NA_real_),
         reincar = recode(reincar, "1"=1, "2"=3, "3"=5,
                          "777"=3, "888"=NA_real_, "666"=NA_real_,
                          "999"=NA_real_),
         miracles = recode(miracles, "1"=1, "2"=3, "3"=5,
                           "777"=3, "888"=NA_real_, "666"=NA_real_, 
                           "999"=NA_real_),
         god = recode(god, "1"=1, "2"=5, "3"=3, "777"=3, "888"=NA_real_,
                      "999"=NA_real_),
         heaven = recode(heaven, "1"=1, "0"=5, "666"=NA_real_, "777"=3, "888"=NA_real_,
                         "999"=NA_real_),
         godworld = recode(wrldorig, "1"=1, "2"=5, "3"=3, "4"=3,
                           "666"=NA_real_, "777"=3, "888"=NA_real_, "999"=NA_real_),
         moralrel = ifelse(moralrel %in% c(666,888, 999), NA_real_, moralrel),
         moralchg = ifelse(moralchg %in% c(666,888, 999), NA_real_, moralchg),
         brkmoral = ifelse(brkmoral %in% c(666,888, 999), NA_real_, brkmoral),
         relprvte = ifelse(relprvte %in% c(666,888,999), NA_real_, relprvte),
         manmar = ifelse(manmar %in% c(666,888,999), NA_real_, manmar),
         wommar = ifelse(wommar %in% c(666,888,999), NA_real_, wommar),
         mandecid = ifelse(mandecid %in% c(666,888,999), NA_real_, mandecid),
         wrkngmom = ifelse(wrkngmom %in% c(666,888,999), NA_real_, wrkngmom),
         unmarsex = ifelse(unmarsex %in% c(666,888,999), NA_real_, unmarsex),
         divrceok = recode(divrceok, "1"=1, "2"=5, "777"=3, "666"=NA_real_,
                           "888"=NA_real_, "999"=NA_real_)) %>%
  mutate(bntranor = bntranr,
         bntradk = ifelse(bntraica == 1 | bntraicn == 1 | bntraia == 1, 1, 0),
         bntraaf = ifelse(bntrablm == 1 | bntrable == 1, 1, 0),
         attend = ifelse(ATTEND1 == 999, 0, ifelse(ATTEND1 == 777, NA_real_, ATTEND1))) %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, godworld, moralrel, moralchg, brkmoral, 
         relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
         wrkngmom, bntraev, bntramnl, bntracat, bntrajew,
         bntranor, bntraoth, bntradk, bntralds, bntraaf,
         gender, attend) %>%
  mutate(bntraev = ifelse(bntralds == 1, 1, bntraev),
         bntraoth = ifelse(bntrajew == 1, 1, bntraoth)) %>%
  mutate(wave = "3")

b4 <- n4 %>%
  select(ids, afterlife_w4, angels_w4, demons_w4, astrolgy_w4, reincar_w4, miracles_w4, god_w4,
         heaven_w4, wrldorigin_w4, moralrel_w4, moralitychnge_w4, brkmorality_w4,
         relprvte_w4, unmarsex_w4, divrceok_w4, manmar_w4, womenmar_w4, mandecide_w4, 
         wrkngmom_w4) %>%
  mutate(aftrlife = afterlife_w4, angels = angels_w4, demons = demons_w4, 
         astrolgy = astrolgy_w4, reincar = reincar_w4, miracles = miracles_w4, 
         god = god_w4, heaven = heaven_w4, wrldorig = wrldorigin_w4, 
         moralrel = moralrel_w4, 
         moralchg = moralitychnge_w4,  brkmoral = brkmorality_w4,
         relprvte = relprvte_w4, unmarsex = unmarsex_w4, divrceok = divrceok_w4, 
         manmar = manmar_w4, wommar = womenmar_w4, mandecid = mandecide_w4, 
         wrkngmom = wrkngmom_w4) %>%
  zap_labels() %>%
  mutate(aftrlife = recode(aftrlife, "1"=1, "2"=2, "3"=3,
                           "777"=2, "888"=NA_real_, "666"=NA_real_,
                           "999"=NA_real_),
         angels = recode(angels, "1"=1, "2"=2, "3"=3,
                         "777"=2, "888"=NA_real_, "666"=NA_real_,
                         "999"=NA_real_),
         demons = recode(demons, "1"=1, "2"=2, "3"=3,
                         "777"=2, "888"=NA_real_, "666"=NA_real_,
                         "999"=NA_real_),
         astrolgy = recode(astrolgy, "1"=1, "2"=2, "3"=3,
                           "777"=2, "888"=NA_real_, "666"=NA_real_,
                           "999"=NA_real_),
         reincar = recode(reincar, "1"=1, "2"=2, "3"=3,
                          "777"=2, "888"=NA_real_, "666"=NA_real_,
                          "999"=NA_real_),
         miracles = recode(miracles, "1"=1, "2"=2, "3"=3,
                           "777"=2, "888"=NA_real_, "666"=NA_real_, 
                           "999"=NA_real_),
         god = recode(god, "1"=1, "2"=2, "0"=3, "777"=2, "888"=NA_real_,
                      "999"=NA_real_),
         heaven = recode(heaven, "1"=1, "0"=3, "2"=2, "666"=NA_real_, "777"=2, "888"=NA_real_,
                         "999"=NA_real_),
         godworld = recode(wrldorig, "1"=1, "2"=3, "3"=2, "4"=2,
                           "666"=NA_real_, "777"=2, "888"=NA_real_, "999"=NA_real_),
         moralrel = ifelse(moralrel %in% c(666,888, 999), NA_real_, moralrel),
         moralchg = ifelse(moralchg %in% c(666,888, 999), NA_real_, moralchg),
         brkmoral = ifelse(brkmoral %in% c(666,888, 999), NA_real_, brkmoral),
         relprvte = ifelse(relprvte %in% c(666,888,999), NA_real_, relprvte),
         manmar = ifelse(manmar %in% c(666,888,999), NA_real_, manmar),
         wommar = ifelse(wommar %in% c(666,888,999), NA_real_, wommar),
         mandecid = ifelse(mandecid %in% c(666,888,999), NA_real_, mandecid),
         wrkngmom = ifelse(wrkngmom %in% c(666,888,999), NA_real_, wrkngmom),
         unmarsex = ifelse(unmarsex %in% c(666,888,999), NA_real_, unmarsex),
         divrceok = recode(divrceok, "1"=1, "0"=3, "777"=2, "666"=NA_real_,
                           "888"=NA_real_, "999"=NA_real_)) %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, godworld, moralrel, moralchg, brkmoral, 
         relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
         wrkngmom) %>%
  mutate(wave = "4")


#b2.1 <- bind_rows(b2, b3, b4) %>% na.omit()
b2.1 <- left_join(b2, controls) %>% na.omit()

0.2592 + 0.07988 + 0.1939 + 0.03648 + 0.1694 + 0.02419 + 0.1513 + 
  0.01997 + 0.06144
 
library(poLCA)

lca_results <- vector(mode = "list", length = 10)
lca_classes <- matrix(NA, nrow = nrow(b2.1), ncol = 10)
for (i in 2:10) {
  l1 <- poLCA(cbind(aftrlife, angels, demons, astrolgy, reincar, miracles, god,
                    heaven, godworld, moralrel, moralchg, brkmoral, 
                    relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
                    wrkngmom)~
                bntraev + bntramnl + bntracat + bntrajew + 
                bntraoth + bntradk + bntralds + bntraaf + 
                attend + 
                gender + midwest + south + west + 
                agecats + compgrad +
                pshrblf, 
              nclass = i, data = b2.1,
              maxiter = 5000)
  lca_results[[i]] <- data.frame(class = i, aic = l1$aic, bic = l1$bic, x2 = l1$Chisq)
  
  lca_classes[,i] <- l1$predclass
  
}






bind_rows(lca_results) %>%
  gather(key = "measure", value = "value", -c(class)) %>%
  ggplot(aes(x = class, y = value, color = measure)) + 
  geom_line() + 
  facet_wrap(~measure, scales = "free")


l6 <- poLCA(cbind(aftrlife, angels, demons, astrolgy, reincar, miracles, god,
                  heaven, godworld, moralrel, moralchg, brkmoral, 
                  relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
                  wrkngmom)~bntraev + bntramnl + bntracat + 
              bntraoth + bntradk + bntraaf + gender + attend, 
            nclass = 6, data = b3.1, 
            maxiter = 2000)


tidy(l6) %>%
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
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x = "", y = "")



l5 <- poLCA(cbind(aftrlife, angels, demons, astrolgy, reincar, miracles, god,
                  heaven, godworld, moralrel, moralchg, brkmoral, 
                  relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
                  wrkngmom)~
              bntraev + bntramnl + bntracat + bntrajew + 
              bntraoth + bntradk + bntralds + bntraaf + 
              attend + 
              gender + midwest + south + west + 
              agecats + compgrad +
              pshrblf, 
            nclass = 5, data = b2.1,
            maxiter = 5000)


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


table(l6$predclass)




t <- left_join(b2.1, full_beliefs, by = c("ids"="ids_2")) %>%
  left_join(predictors, by=c("ids"="ids")) %>%
  mutate(class = as.factor(class)) %>%
  mutate(diff = abs(astrolgy_2 - astrolgy_3))

summary(lm(diff ~ as.factor(class), data = t))

names(b2) <- paste(names(b2), "_2", sep = "")
names(b3) <- paste(names(b3), "_3", sep = "")
names(b4) <- paste(names(b4), "_4", sep = "")
full_beliefs <- full_join(b2, b3, by = c("ids_2"="ids_3")) %>%
  full_join(b4, by = c("ids_2"="ids_4"))


names(b2.1) <- paste(names(b2.1), "_2", sep = "")
t <- b2.1%>%
  mutate(group = l5$predclass) %>%
  full_join(full_beliefs) %>%
  select(ids_2:wrkngmom_3, group) %>%
  filter(!is.na(group)) %>%
  gather(key = "var", value = "value", -c(ids_2, group)) %>%
  group_by(group, var) %>%
  mutate(grp.var = sd(value, na.rm = TRUE)) %>%
  arrange(ids_2) %>%
  group_by(ids_2, var) %>%
  separate(var, into = c("question", "wave")) %>%
  arrange(ids_2, question) %>%
  group_by(ids_2, question) %>%
  mutate(grp.var = ifelse(wave == "2", grp.var, NA),
         grp.var = max(grp.var, na.rm = TRUE)) %>%
  spread(wave, value) %>%
  mutate(abs_diff = abs(as.numeric(`3`)-as.numeric(`2`))) %>%
  filter(!is.na(group)) %>%
  filter(question %in% c("aftrlife", "angels", "demons", "astrolgy", "reincar", "miracles", "god",
                    "heaven", "godworld", "moralrel", "moralchg", "brkmoral", 
                    "relprvte", "unmarsex", "divrceok", "manmar", "wommar", "mandecid", 
                    "wrkngmom")) 

t <- data.frame(t) %>%
  na.omit()
pdata <- pdata.frame(x = t, index = c("ids_2", "question"))

m1 <- plm(sqrt(abs_diff) ~ grp.var, family = poisson, 
          model = "within", 
          data = pdata)

t %>%
  group_by(group, question, grp.var) %>%
  summarise(abs_diff = mean(abs_diff, na.rm = TRUE)) %>%
  ggplot(aes(x = grp.var, y = abs_diff, fill = as.factor(group))) + 
  geom_point(shape = 21) + 
  geom_text_repel(aes(label = question), size = 2) + 
  labs(x = "Within-Class S.D., wave 2",
       y = "Avg. Within-Person Change between Wave 2 and 3",
       fill = "Class") +
  theme_minimal()

t %>%
  group_by(group, question, grp.var) %>%
  summarise(abs_diff = mean(abs_diff, na.rm = TRUE)) %>%
  group_by(group) %>%
  summarise(cor(grp.var, abs_diff))

b2.1$class <- l5$predclass
b2.1 %>%
  group_by(class) %>%
  select(class, bntraev_2, bntramnl_2, bntracat_2, bntrajew_2,
         bntranor_2, bntraoth_2, bntradk_2, bntralds_2, bntraaf_2,
         gender_2, attend_2, midwest_2, south_2, west_2,
         agecats_2, compgrad_2, pshrblf_2) %>%
  gather(key = "key", value = "value", -class) %>%
  group_by(class, key) %>%
  summarise(tot = sum(value)) %>%
  spread(class, tot)
  group_by(key) %>%
  mutate(pct = tot/sum(tot)) %>%
  select(class, key, pct) %>%
  spread(class, pct) 
  summarise(across(c(bntraev_2, bntramnl_2, bntracat_2, bntrajew_2,
                     bntranor_2, bntraoth_2, bntradk_2, bntralds_2, bntraaf_2,
                     gender_2, attend_2, midwest_2, south_2, west_2,
                     agecats_2, compgrad_2, pshrblf_2), sum)) %>% View()



b2.1%>%
  mutate(group = l5$predclass) %>%
  full_join(full_beliefs) %>%
  select(ids_2:wrkngmom_3, group) %>%
  filter(!is.na(group)) %>%
  gather(key = "var", value = "value", -c(ids_2, group)) %>%
  filter(grepl("_2", var)==TRUE) %>%
  mutate(var = gsub("_2", "", var)) %>%
  group_by(group, var) %>%
  summarise(grp.sd = sd(value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(var %in% c("aftrlife", "angels", "demons", "astrolgy", "reincar", "miracles", "god",
                    "heaven", "godworld", "moralrel", "moralchg", "brkmoral", 
                    "relprvte", "unmarsex", "divrceok", "manmar", "wommar", "mandecid", 
                    "wrkngmom")) %>%
  mutate(cat = ifelse(var %in% c("aftrlife", "angels", "demons" ,"astrolgy", 
                                      "reincar", "godworld", "heaven", "god",
                                      "miracles"),
                      "religion",
                      ifelse(var %in% c("moralrel", "moralchg", "brkmoral",
                                             "relprvte"),
                             "morality", "family"))) %>%
  ggplot(aes(x = var, y = grp.sd, fill = as.factor(group))) + 
  geom_point(shape = 21) + 
  facet_grid(cat~., scales = "free_y") + 
  coord_flip() 



joined <- full_join(n2, n3, by = c("ids")) 
j <- joined %>%
  mutate(attend1 = ifelse(ATTEND1.x == 999, 0, ifelse(ATTEND1.x == 666, NA_real_,
                                                      ATTEND1.x)),
         attend2 = ifelse(ATTEND1.y == 999, 0, ifelse(ATTEND1.y == 777, NA_real_,
                                                      ATTEND1.y))) %>%
  mutate(attend_change = abs(attend1 - attend2)) %>%
  mutate(trad_change = ifelse(bntraev.x == 1 & bntraev.y == 0 |
                                bntramnl.x == 1 & bntramnl.y == 0 |
                                bntracat.x == 1 & bntracat.y == 0 |
                                bntrajew.x == 1 & bntrajew.y == 0 |
                                bntralds.x == 1 & bntralds.y == 0 |
                                bntraoth.x == 1 & bntraoth.y == 0 |
                                bntranor == 1 & bntranr == 0,
                                1,0)) %>%
  select(ids, attend_change, trad_change)


post <- b2.1%>%
  mutate(group = l5$predclass) 



prop.table(table(post$group, post$attend),2)



#Model 1: Each person's response is predicted by their response at time 2



#Model 2: Each person's response is predicted by a multinomial draw from all
# responses at time 1...
multinom_dfs <- vector(mode = "list", length = 100)
for (i in 1:100) {
  moralrel <- t %>%
    filter(question == "moralrel")
  
  multinom_probs <- moralrel %>%
    group_by(X2) %>%
    summarise(n = n()) %>%
    mutate(pct = n/sum(n),
           se = sqrt((pct*(1-pct))/n)) %>%
    rowwise() %>%
    mutate(prob = rnorm(1, pct, se)) %>%
    mutate(prob = ifelse(prob < 0, 0, prob)) %>%
    select(X2, prob)
  moralrel$pred_3 <- sample(1:5, nrow(moralrel), replace = TRUE, 
                            prob = multinom_probs$prob)
  multinom_dfs[[i]] <- moralrel %>%
    group_by(question, X2, pred_3) %>%
    summarise(n = n())
}
  
for (i in 1:length(multinom_dfs)) {
  if (i == 1) {
    multinom_results <- multinom_dfs[[i]]
  } else {
    multinom_results <- full_join(multinom_results, multinom_dfs[[i]], by = c("question", "X2", "pred_3"))
  }
}

multinom_results %>%
  mutate(pattern = paste(X2, "->", pred_3, sep = " ")) %>%
  gather(key = "key", value = "value", -c(question, X2, pred_3, pattern)) %>%
  filter(question %in% c("moralrel")) %>%
  ggplot(aes(x = pattern, y = value)) + 
  geom_boxplot() + 
  geom_point(data = short.t %>%
               filter(question == "moralrel"), 
             aes(x = pattern, y = n), shape = 21, fill = "firebrick") + 
  coord_flip() + 
  facet_wrap(~question, scales = "free")

#Model 3: Each person's response is predicted by a multinomial draw from their 
#respective group's probabilities
dfs <- vector(mode = "list", length = 100)
for (i in 1:100) {
  
  sample(1:5, 2544, replace = TRUE, l5$posterior)
  
  df <- left_join(t, probs, by = c("question"="variable", "group"="class")) %>%
    select(-c(grp.var, abs_diff)) %>%
    na.omit()
  
  
  probs <- tidy(l5) %>%
    rowwise() %>%
    mutate(prob = rnorm(1, estimate, std.error)) %>%
    mutate(prob = ifelse(prob < 0, 0, prob)) %>%
    select(-c(estimate, std.error)) %>%
    spread(outcome, prob) 
  df$pred_3 <- apply(df, 1, 
                     function(x) 
                       sample(c(1,2,3,4,5), 1, 
                              prob = c(x[6], x[7], x[8], x[9], x[10])))
  dfs[[i]] <- df %>%
    group_by(question, X2, pred_3) %>%
    summarise(n = n())
  
}

for (i in 1:length(dfs)) {
  if (i == 1) {
    results <- dfs[[i]]
  } else {
    results <- full_join(results, dfs[[i]], by = c("question", "X2", "pred_3"))
  }
}

results <- results %>%
  mutate(pattern = paste(X2, "->", pred_3, sep = " ")) %>%
  gather(key = "key", value = "value", -c(question, X2, pred_3, pattern)) %>%
  filter(question %in% c("moralrel")) %>%
  mutate(model = "lca")

multinom_results <- multinom_results %>%
  mutate(pattern = paste(X2, "->", pred_3, sep = " ")) %>%
  gather(key = "key", value = "value", -c(question, X2, pred_3, pattern)) %>%
  filter(question %in% c("moralrel")) %>%
  mutate(model = "multinom")

bind_rows(results, multinom_results) %>%
  ggplot(aes(x = pattern, y = value)) + 
  geom_boxplot() + 
  geom_point(data = short.t %>%
               filter(question == "moralrel"), 
             aes(x = pattern, y = n), shape = 21, fill = "firebrick") + 
  coord_flip() + 
  facet_wrap(~model)

bind_rows(multinom_results %>% mutate(model = "multinom"), 
          results %>% mutate(model = "lca"))


short.t <- t %>%
  group_by(question, X2, X3) %>%
  summarise(n = n()) %>%
  na.omit() %>%
  mutate(pattern = paste(X2, "->", X3, sep = " ")) %>%
  filter(question %in% c("aftrlife", "god", "moralrel")) 













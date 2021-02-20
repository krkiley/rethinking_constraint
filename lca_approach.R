
source("~/Dropbox/hill_kreisi/functions/model_function.R")


#Load in NSYR
n1 <- read_dta("~/Dropbox/data/nsyr/nsyr1.DTA")
n2 <- read_dta("~/Dropbox/data/nsyr/nsyr2.DTA")
n3 <- read_dta("~/Dropbox/data/nsyr/nsyr3.DTA")
n4 <- read_dta("~/Dropbox/data/nsyr/nsyr4.dta")


b2 <- n2 %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, wrldorig, goduseev, moralrel, moralchg, brkmoral,
         relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
         wrkngmom) %>%
  zap_labels() %>%
  mutate(aftrlife = recode(aftrlife, "1"=1, "2"=2, "3"=3,
                           "777"=2, "666"=NA_real_, "888"=NA_real_),
         angels = recode(angels, "1"=1, "2"=2, "3"=3,
                         "777"=2, "666"=NA_real_, "888"=NA_real_),
         demons = recode(demons, "1"=1, "2"=2, "3"=3,
                         "777"=2, "666"=NA_real_, "888"=NA_real_),
         astrolgy = recode(astrolgy, "1"=1, "2"=2, "3"=3,
                           "777"=2, "666"=NA_real_, "888"=NA_real_),
         reincar = recode(reincar, "1"=1, "2"=2, "3"=3,
                          "777"=2, "666"=NA_real_, "888"=NA_real_),
         miracles = recode(miracles, "1"=1, "2"=2, "3"=3,
                           "777"=2, "666"=NA_real_, "888"=NA_real_),
         god = recode(god, "1"=1, "2"=3, "3"=2, "777"=2, "666"=NA_real_),
         heaven = recode(heaven, "1"=1, "0"=3, "666"=NA_real_, "777"=2, "888"=NA_real_),
         godworld = recode(wrldorig, "1"=1, "2"=3, "3"=2, "4"=2,
                           "666"=NA_real_, "777"=2, "888"=NA_real_),
         moralrel = ifelse(moralrel %in% c(666,888), NA_real_, moralrel),
         moralchg = ifelse(moralchg %in% c(666,888), NA_real_, moralchg),
         brkmoral = ifelse(brkmoral %in% c(666,888), NA_real_, brkmoral),
         relprvte = ifelse(relprvte %in% c(666,888), NA_real_, relprvte),
         manmar = ifelse(manmar %in% c(666,888), NA_real_, manmar),
         wommar = ifelse(wommar %in% c(666,888), NA_real_, wommar),
         mandecid = ifelse(mandecid %in% c(666,888), NA_real_, mandecid),
         wrkngmom = ifelse(wrkngmom %in% c(666,888), NA_real_, wrkngmom),
         unmarsex = ifelse(unmarsex %in% c(666,888), NA_real_, unmarsex),
         divrceok = recode(divrceok, "1"=1, "2"=3, "777"=2, "666"=NA_real_,
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
         wrkngmom) %>%
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
         god = recode(god, "1"=1, "2"=3, "3"=2, "777"=2, "888"=NA_real_,
                      "999"=NA_real_),
         heaven = recode(heaven, "1"=1, "0"=3, "666"=NA_real_, "777"=2, "888"=NA_real_,
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
         divrceok = recode(divrceok, "1"=1, "2"=3, "777"=2, "666"=NA_real_,
                           "888"=NA_real_, "999"=NA_real_)) %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, godworld, moralrel, moralchg, brkmoral, 
         relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
         wrkngmom) %>%
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
b2.1 <- b2 %>% na.omit()

library(poLCA)

lca_results <- vector(mode = "list", length = 10)
for (i in 1:10) {
  l1 <- poLCA(cbind(aftrlife, angels, demons, astrolgy, reincar, miracles, god,
                    heaven, godworld, moralrel, moralchg, brkmoral, 
                    relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
                    wrkngmom)~wave, nclass = i, data = b2.1,
              maxiter = 5000)
  lca_results[[i]] <- data.frame(class = i, aic = l1$aic, bic = l1$bic, x2 = l1$Chisq)
  
}


bind_rows(lca_results) %>%
  gather(key = "measure", value = "value", -c(class)) %>%
  ggplot(aes(x = class, y = value, color = measure)) + 
  geom_line() + 
  facet_wrap(~measure, scales = "free")


l6 <- poLCA(cbind(aftrlife, angels, demons, astrolgy, reincar, miracles, god,
                  heaven, godworld, moralrel, moralchg, brkmoral, 
                  relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
                  wrkngmom)~1, nclass = 6, data = b2.1, 
            maxiter = 2000)


tidy(l6) %>%
  mutate(cat = ifelse(variable %in% c("aftrlife", "angels", "demons" ,"astrolgy", 
                                      "reincar", "godworld", "heaven", "god",
                                      "miracles"),
                      "religion",
                      ifelse(variable %in% c("moralrel", "moralchg", "brkmoral"),
                             "morality", "family"))) %>%
  ggplot(aes(x = variable, y = estimate, fill = as.factor(outcome))) + 
  geom_bar(stat = "identity", position = "stack", color = "black") + 
  facet_grid(class~cat, scales = "free_x")


table(l6$predclass)



b2.1$class <- l6$predclass

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
  mutate(group = l6$predclass) %>%
  full_join(full_beliefs) %>%
  select(ids_2:wrkngmom_3, group) %>%
  filter(!is.na(group)) %>%
  gather(key = "var", value = "value", -c(ids_2, group)) %>%
  group_by(group, var) %>%
  mutate(grp.var = var(value, na.rm = TRUE)) %>%
  arrange(ids_2) %>%
  group_by(ids_2, var) %>%
  separate(var, into = c("question", "wave")) %>%
  arrange(ids_2, question) %>%
  group_by(ids_2, question) %>%
  mutate(grp.var = ifelse(wave == "2", grp.var, NA),
         grp.var = max(grp.var, na.rm = TRUE)) %>%
  spread(wave, value) %>%
  mutate(abs_diff = abs(as.numeric(`3`)-as.numeric(`2`))) %>%
  filter(!is.na(group))

t <- data.frame(t)
pdata <- pdata.frame(x = t, index = c("ids_2", "question"))

m1 <- plm(abs_diff ~ grp.var, model = "within", data = pdata)






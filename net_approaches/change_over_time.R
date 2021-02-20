

#Wave 2 Beliefs
b2 <- n2 %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, wrldorig, moralrel, moralchg, brkmoral,
         relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
         wrkngmom) %>%
  zap_labels() %>%
  mutate(aftrlife = recode(aftrlife, "1"=1, "2"=.5, "3"=0,
                           "777"=.5, "666"=NA_real_, "888"=NA_real_),
         angels = recode(angels, "1"=1, "2"=.5, "3"=0,
                         "777"=.5, "666"=NA_real_, "888"=NA_real_),
         demons = recode(demons, "1"=1, "2"=.5, "3"=0,
                         "777"=.5, "666"=NA_real_, "888"=NA_real_),
         astrolgy = recode(astrolgy, "1"=1, "2"=.5, "3"=0,
                           "777"=.5, "666"=NA_real_, "888"=NA_real_),
         reincar = recode(reincar, "1"=1, "2"=.5, "3"=0,
                          "777"=.5, "666"=NA_real_, "888"=NA_real_),
         miracles = recode(miracles, "1"=1, "2"=.5, "3"=0,
                           "777"=.5, "666"=NA_real_, "888"=NA_real_),
         god = recode(god, "1"=1, "2"=0, "3"=.5, "777"=.5, "666"=NA_real_),
         heaven = recode(heaven, "1"=1, "0"=0, "666"=NA_real_, "777"=.5, "888"=NA_real_),
         godworld = recode(wrldorig, "1"=1, "2"=0, "3"=.5, "4"=.5,
                           "666"=NA_real_, "777"=.5, "888"=NA_real_),
         moralrel = ifelse(moralrel %in% c(666,888), NA_real_, moralrel),
         moralrel = range01(moralrel),
         moralchg = ifelse(moralchg %in% c(666,888), NA_real_, moralchg),
         moralchg = range01(moralchg),
         brkmoral = ifelse(brkmoral %in% c(666,888), NA_real_, brkmoral),
         brkmoral = range01(brkmoral),
         relprvte = ifelse(relprvte %in% c(666,888), NA_real_, relprvte),
         relprvte = range01(relprvte),
         manmar = ifelse(manmar %in% c(666,888), NA_real_, manmar),
         manmar = range01(manmar),
         wommar = ifelse(wommar %in% c(666,888), NA_real_, wommar),
         wommar = range01(wommar),
         mandecid = ifelse(mandecid %in% c(666,888), NA_real_, mandecid),
         mandecid = range01(mandecid),
         wrkngmom = ifelse(wrkngmom %in% c(666,888), NA_real_, wrkngmom),
         wrkngmom = range01(wrkngmom),
         unmarsex = ifelse(unmarsex %in% c(666,888), NA_real_, unmarsex),
         unmarsex = range01(unmarsex),
         divrceok = recode(divrceok, "1"=1, "2"=0, "777"=.5, "666"=NA_real_,
                           "888"=NA_real_)) %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, godworld, moralrel, moralchg, brkmoral, 
         relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
         wrkngmom)
names(b2) <- paste(names(b2), "_2", sep = "")


#Wave 3 Beliefs
b3 <- n3 %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, wrldorig, moralrel, moralchg, brkmoral,
         relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
         wrkngmom) %>%
  zap_labels() %>%
  mutate(aftrlife = recode(aftrlife, "1"=1, "2"=.5, "3"=0,
                           "777"=.5, "888"=NA_real_, "666"=NA_real_,
                           "999"=NA_real_),
         angels = recode(angels, "1"=1, "2"=.5, "3"=0,
                         "777"=.5, "888"=NA_real_, "666"=NA_real_,
                         "999"=NA_real_),
         demons = recode(demons, "1"=1, "2"=.5, "3"=0,
                         "777"=.5, "888"=NA_real_, "666"=NA_real_,
                         "999"=NA_real_),
         astrolgy = recode(astrolgy, "1"=1, "2"=.5, "3"=0,
                           "777"=.5, "888"=NA_real_, "666"=NA_real_,
                           "999"=NA_real_),
         reincar = recode(reincar, "1"=1, "2"=.5, "3"=0,
                          "777"=.5, "888"=NA_real_, "666"=NA_real_,
                          "999"=NA_real_),
         miracles = recode(miracles, "1"=1, "2"=.5, "3"=0,
                           "777"=.5, "888"=NA_real_, "666"=NA_real_, 
                           "999"=NA_real_),
         god = recode(god, "1"=1, "2"=0, "3"=.5, "777"=.5, "888"=NA_real_,
                      "999"=NA_real_),
         heaven = recode(heaven, "1"=1, "0"=0, "666"=NA_real_, "777"=.5, "888"=NA_real_,
                         "999"=NA_real_),
         godworld = recode(wrldorig, "1"=1, "2"=0, "3"=.5, "4"=.5,
                           "666"=NA_real_, "777"=.5, "888"=NA_real_, "999"=NA_real_),
         moralrel = ifelse(moralrel %in% c(666,888, 999), NA_real_, moralrel),
         moralrel = range01(moralrel),
         moralchg = ifelse(moralchg %in% c(666,888, 999), NA_real_, moralchg),
         moralchg = range01(moralchg),
         brkmoral = ifelse(brkmoral %in% c(666,888, 999), NA_real_, brkmoral),
         brkmoral = range01(brkmoral),
         relprvte = ifelse(relprvte %in% c(666,888,999), NA_real_, relprvte),
         relprvte = range01(relprvte),
         manmar = ifelse(manmar %in% c(666,888,999), NA_real_, manmar),
         manmar = range01(manmar),
         wommar = ifelse(wommar %in% c(666,888,999), NA_real_, wommar),
         wommar = range01(wommar),
         mandecid = ifelse(mandecid %in% c(666,888,999), NA_real_, mandecid),
         mandecid = range01(mandecid),
         wrkngmom = ifelse(wrkngmom %in% c(666,888,999), NA_real_, wrkngmom),
         wrkngmom = range01(wrkngmom),
         unmarsex = ifelse(unmarsex %in% c(666,888,999), NA_real_, unmarsex),
         unmarsex = range01(unmarsex),
         divrceok = recode(divrceok, "1"=1, "2"=0, "777"=.5, "666"=NA_real_,
                           "888"=NA_real_, "999"=NA_real_)) %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, godworld, moralrel, moralchg, brkmoral, 
         relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
         wrkngmom)
names(b3) <- paste(names(b3), "_3", sep = "")



#Wave 4 Beliefs
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
  mutate(aftrlife = recode(aftrlife, "1"=1, "2"=.5, "3"=0,
                           "777"=.5, "888"=NA_real_, "666"=NA_real_,
                           "999"=NA_real_),
         angels = recode(angels, "1"=1, "2"=.5, "3"=0,
                         "777"=.5, "888"=NA_real_, "666"=NA_real_,
                         "999"=NA_real_),
         demons = recode(demons, "1"=1, "2"=.5, "3"=0,
                         "777"=.5, "888"=NA_real_, "666"=NA_real_,
                         "999"=NA_real_),
         astrolgy = recode(astrolgy, "1"=1, "2"=.5, "3"=0,
                           "777"=.5, "888"=NA_real_, "666"=NA_real_,
                           "999"=NA_real_),
         reincar = recode(reincar, "1"=1, "2"=.5, "3"=0,
                          "777"=.5, "888"=NA_real_, "666"=NA_real_,
                          "999"=NA_real_),
         miracles = recode(miracles, "1"=1, "2"=.5, "3"=0,
                           "777"=.5, "888"=NA_real_, "666"=NA_real_, 
                           "999"=NA_real_),
         god = recode(god, "1"=1, "2"=0, "3"=.5, "777"=.5, "888"=NA_real_,
                      "999"=NA_real_),
         heaven = recode(heaven, "1"=1, "0"=0, "2"=.5, "666"=NA_real_, "777"=.5, "888"=NA_real_,
                         "999"=NA_real_),
         godworld = recode(wrldorig, "1"=1, "2"=0, "3"=.5, "4"=.5,
                           "666"=NA_real_, "777"=.5, "888"=NA_real_, "999"=NA_real_),
         moralrel = ifelse(moralrel %in% c(666,888, 999), NA_real_, moralrel),
         moralrel = range01(moralrel),
         moralchg = ifelse(moralchg %in% c(666,888, 999), NA_real_, moralchg),
         moralchg = range01(moralchg),
         brkmoral = ifelse(brkmoral %in% c(666,888, 999), NA_real_, brkmoral),
         brkmoral = range01(brkmoral),
         relprvte = ifelse(relprvte %in% c(666,888,999), NA_real_, relprvte),
         relprvte = range01(relprvte),
         manmar = ifelse(manmar %in% c(666,888,999), NA_real_, manmar),
         manmar = range01(manmar),
         wommar = ifelse(wommar %in% c(666,888,999), NA_real_, wommar),
         wommar = range01(wommar),
         mandecid = ifelse(mandecid %in% c(666,888,999), NA_real_, mandecid),
         mandecid = range01(mandecid),
         wrkngmom = ifelse(wrkngmom %in% c(666,888,999), NA_real_, wrkngmom),
         wrkngmom = range01(wrkngmom),
         unmarsex = ifelse(unmarsex %in% c(666,888,999), NA_real_, unmarsex),
         unmarsex = range01(unmarsex),
         divrceok = recode(divrceok, "1"=1, "2"=0, "777"=.5, "666"=NA_real_,
                           "888"=NA_real_, "999"=NA_real_)) %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, godworld, moralrel, moralchg, brkmoral, 
         relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
         wrkngmom)
names(b4) <- paste(names(b4), "_4", sep = "")


full_beliefs <- full_join(b2, b3, by = c("ids_2"="ids_3")) %>%
  full_join(b4, by = c("ids_2"="ids_4"))

diffs23 <- numeric(length = nrow(full_beliefs))
diffs34 <- numeric(length = nrow(full_beliefs))
for (i in 1:nrow(full_beliefs)) {
  diffs23[i] <- as.numeric(ab_diff(full_beliefs[i,2:20], full_beliefs[i,21:39]))
  diffs34[i] <- as.numeric(ab_diff(full_beliefs[i,21:39], full_beliefs[i,40:58]))
}

full_beliefs$diff23 <- diffs23
full_beliefs$diff34 <- diffs34


t <- left_join(b2.1, full_beliefs) %>%
  left_join(predictors, by=c("ids_2"="ids")) %>%
  mutate(group = as.factor(group)) %>%
  mutate(diff = abs(astrolgy_2 - astrolgy_3))


heaven <- lm(diff ~ as.factor(group), data = t)
manmar <- lm(diff ~ as.factor(group), data = t)
moralchg <- lm(diff ~ as.factor(group), data = t)
mandecid <- lm(diff ~ as.factor(group), data = t)
astro <- lm(diff ~ as.factor(group), data = t)

m1 <- multinom(group ~ attend + bntraev + bntramnl + bntracat, data = t)


predictors <- n2 %>%
  select(ids, ATTEND1, bntraev, bntramnl, bntracat) %>%
  mutate(attend = ifelse(ATTEND1 == 999, 0, ifelse(ATTEND1 == 666, NA_real_, ATTEND1)))



tidy(m1, exponentiate = FALSE) %>%
  filter(std.error < 80) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term, y = estimate, 
             ymin = estimate - 1.96*std.error, 
             ymax = estimate + 1.96*std.error,
             fill = y.level)) + 
  geom_linerange() + 
  geom_point(shape = 21) + 
  coord_flip() + 
  facet_wrap(~y.level) 

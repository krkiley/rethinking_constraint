

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
         divrceok = recode(divrceok, "1"=1, "0"=5, "777"=3, "666"=NA_real_,
                           "888"=NA_real_, "999"=NA_real_)) %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, godworld, moralrel, moralchg, brkmoral, 
         relprvte, unmarsex, divrceok, manmar, wommar, mandecid, 
         wrkngmom) 


l2 <- b2 %>%
  gather(key = "question", value = "w2", -ids)

l3 <- b3 %>%
  gather(key = "question", value = "w3", -ids)

l4 <- b4 %>%
  gather(key = "question", value = "w4", -ids)


sds <- full_join(l2, l3, by = c("ids", "question")) %>%
  full_join(l4, by = c("ids", "question")) %>%
  gather(key = "key", value = "value", -c(ids, question)) %>%
  group_by(ids, question) %>%
  summarise(sd = sd(value, na.rm = TRUE),
            count = n()) %>%
  filter(n > 2)

left_join(long_blf, sds, by = c("ids", "question")) %>%
  group_by(predclass, question, grp_sd) %>%
  summarise(mean_sd = mean(sd, na.rm = TRUE)) %>%
  ggplot(aes(x = grp_sd, y = mean_sd, fill = as.factor(predclass))) + 
  geom_point(shape = 21) + 
  geom_text_repel(aes(label = question), size = 2) + 
  labs(x = "Within-Class S.D., wave 2",
       y = "Avg. Within-Person Change between Wave 2 and 3",
       fill = "Class") +
  theme_minimal()

left_join(long_blf, sds, by = c("ids", "question"))

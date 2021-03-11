
library(haven)
#Load in NSYR
n1 <- read_dta("~/Dropbox/data/nsyr/nsyr1.DTA")
n2 <- read_dta("~/Dropbox/data/nsyr/nsyr2.DTA")
n3 <- read_dta("~/Dropbox/data/nsyr/nsyr3.DTA")
n4 <- read_dta("~/Dropbox/data/nsyr/nsyr4.dta")

#wave 1 measures that are invariant over time
c1 <- n1 %>%
  select(ids, pdadeduc, pmomeduc, pincome, bnpwhite,
         bnpblack, bnplatin, BN2ADLT) %>%
  mutate(pdadeduc = ifelse(pdadeduc > 700, NA, pdadeduc),
         pmomeduc = ifelse(pmomeduc > 700, NA, pmomeduc),
         pdadba = ifelse(pdadeduc >= 9, 1, 0),
         pmomba = ifelse(pmomeduc >= 9, 1, 0),
         parba = ifelse(is.na(pdadba), pmomba, 
                        ifelse(is.na(pmomba), pdadba,
                               ifelse(pdadba == 1 | pmomba == 1, 1, 0))),
         income = ifelse(pincome > 11, NA, pincome),
         twopar = BN2ADLT) %>%
  mutate(bnpoth = ifelse(bnpwhite == 0 & bnpblack == 0, 1, 0)) %>%
  select(ids, bnpblack, bnpoth, parba)
  

c2 <- n2 %>% 
  select(ids, bntraev, bntramnl, bntracat, bntrajew,
         bntranor, bntraoth, bntradk, bntralds, bntraaf,
         bntraafm,
         gender, ATTEND1, CEN_DIV, 
         momclose, dadclose, 
         agecats, compgrad, frnds, frrelblf, frnotrel,
         sfrrelbl, sfrnotrl) %>%
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
         frrelblf = ifelse(frnds == 1, sfrrelbl, frrelblf),
         frnotrel = ifelse(frnds == 1, sfrnotrl, frnotrel),
         fshrblf = ifelse(bntranor == 1, frnotrel, frrelblf),
         fshrblf = ifelse(fshrblf %in% c(666,888), NA_real_,
                          ifelse(fshrblf %in% c(777,999), 0, fshrblf)),
         pshrblf = fshrblf/frnds,
         agecats = agecats - 16,
         momclose = ifelse(momclose > 600, NA, 7 - momclose),
         dadclose = ifelse(dadclose > 600, NA, 7 - dadclose),
         parclose = ifelse(is.na(momclose), dadclose,
                           ifelse(is.na(dadclose), momclose,
                                  ifelse(momclose > dadclose, momclose, dadclose)))) %>%
  mutate(bntraev = ifelse(bntralds == 1, 1, bntraev),
         bntraaf = ifelse(bntraafm == 1, 1, bntraaf)) %>%
  select(ids, bntraev, bntramnl, bntracat, bntrajew,
         bntranor, bntraoth, bntradk, bntraaf,
         gender, attend, midwest, south, west, agecats,
         compgrad, pshrblf, parclose) 


c3 <- n3 %>%
  select(ids, bntraev, bntramnl, bntracat, bntrajew,
         bntranr, bntraoth, bntraica, bntraicn, bntraia, bntralds, bntrable,
         bntrablm,
         momclose, dadclose, 
         gender, ATTEND1, cendiv, 
         agecats, compgrad, numfrien, frrelblf, frnotrel,
         sfrrelbl, sfrnotrl) %>%
  mutate(attend = ifelse(ATTEND1 == 999, 0, ifelse(ATTEND1 %in% c(666,777), 
                                                   NA_real_, ATTEND1)),
         northeast = ifelse(cendiv %in% c("NEW ENGLAND","MIDDLE ATLANTIC"), 1,0),
         midwest = ifelse(cendiv %in% c("EAST NORTH CENTRAL","WEST NORTH CENTRAL"), 1, 0),
         south = ifelse(cendiv %in% c("EAST SOUTH CENTRAL","SOUTH ATLANTIC","WEST SOUTH CENTRAL"), 1, 0),
         west = ifelse(cendiv %in% c("MOUNTAIN","PACIFIC"), 1, 0),
         compgrad = ifelse(compgrad > 19, NA,
                           ifelse(compgrad < 9, 9,
                                  ifelse(compgrad >= 16, compgrad - 3, 
                                         compgrad))),
         frnds = numfrien,
         compgrad = compgrad - 9,
         compgrad = ifelse(is.na(compgrad), mean(compgrad, na.rm = TRUE), compgrad),
         frnds = ifelse(frnds %in% c(666), NA_real_, frnds),
         frnds = ifelse(frnds %in% c(999,888), 0, frnds),
         bntranor = bntranr,
         frrelblf = ifelse(sfrrelbl %in% c(0,1), sfrrelbl, frrelblf),
         frnotrel = ifelse(sfrnotrl %in% c(0,1), sfrnotrl, frnotrel),
         fshrblf = ifelse(bntranor == 1, frnotrel, frrelblf),
         fshrblf = ifelse(fshrblf %in% c(666,888), NA_real_,
                          ifelse(fshrblf %in% c(777,999), 0, fshrblf)),
         pshrblf = fshrblf/frnds,
         pshrblf = ifelse(is.na(pshrblf), mean(pshrblf, na.rm = TRUE), pshrblf),
         pshrblf = ifelse(is.infinite(pshrblf),0, pshrblf),
         agecats = agecats - 16,
         bntradk = ifelse(bntraica == 1 | bntraicn == 1 | bntraia == 1, 1, 0),
         bntraaf = bntrable,
         bntraafm = bntrablm, 
         momclose = ifelse(momclose > 600, NA, 7 - momclose),
         dadclose = ifelse(dadclose > 600, NA, 7 - dadclose),
         parclose = ifelse(is.na(momclose), dadclose,
                           ifelse(is.na(dadclose), momclose,
                                  ifelse(momclose > dadclose, momclose, dadclose)))) %>%
  select(ids, bntraev, bntramnl, bntracat, bntrajew,
         bntranor, bntraoth, bntradk, bntralds, bntraaf, bntraafm,
         gender, attend, midwest, south, west, agecats,
         compgrad, frnds, fshrblf, pshrblf, parclose) %>%
  mutate(bntraev = ifelse(bntralds == 1, 1, bntraev),
         bntraaf = ifelse(bntraafm == 1, 1, bntraaf))


c4 <- n4 %>%
  select(ids, bntraev_w4, bntramnl_w4, bntracat_w4, bntrajew_w4,
         bntranr_w4, bntraoth_w4, bntraica_w4, bntraicn_w4, bntraia_w4, bntralds_w4, 
         bntrable_w4, bntrablm_w4,
         momclose_w4, dadclose_w4,
         gender_w4, attend1_w4, cendiv_w4, 
         agecats_w4) %>%
  mutate(bntraev=bntraev_w4, bntramnl=bntramnl_w4, bntracat=bntracat_w4, 
         bntrajew=bntrajew_w4, bntranr=bntranr_w4, bntraoth=bntraoth_w4, 
         bntraica=bntraica_w4, bntraicn=bntraicn_w4, bntraia=bntraia_w4, 
         bntralds=bntralds_w4, bntrable=bntrable_w4, bntrablm=bntrablm_w4,
         momclose=momclose_w4, dadclose=dadclose_w4, gender=gender_w4, 
         ATTEND1=attend1_w4, cendiv=cendiv_w4, 
         agecats=agecats_w4) %>%
  select(ids, bntraev, bntramnl, bntracat, bntrajew,
         bntranr, bntraoth, bntraica, bntraicn, bntraia, bntralds, bntrable,
         bntrablm,
         gender, ATTEND1, cendiv, 
         agecats) %>%
  mutate(attend = ifelse(ATTEND1 == -99, 0, ifelse(ATTEND1 %in% c(666,777), 
                                                   NA_real_, ATTEND1)),
         northeast = ifelse(cendiv %in% c(1,2), 1,0),
         midwest = ifelse(cendiv %in% c(3,4), 1, 0),
         south = ifelse(cendiv %in% c(5,6,7), 1, 0),
         west = ifelse(cendiv %in% c(8,9), 1, 0),
         bntranor = bntranr,
         agecats = agecats - 16,
         bntradk = ifelse(bntraica == 1 | bntraicn == 1 | bntraia == 1, 1, 0),
         bntraaf = bntrable,
         bntraafm = bntrablm) %>%
  select(ids, bntraev, bntramnl, bntracat, bntrajew,
         bntranor, bntraoth, bntradk, bntralds, bntraaf, bntraafm,
         gender, attend, midwest, south, west, agecats) %>%
  mutate(bntraaf = ifelse(bntraafm == 1, 1, bntraaf))




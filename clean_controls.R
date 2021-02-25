



c3 <- n3 %>%
  select(ids, bntraev, bntramnl, bntracat, bntrajew,
         bntranr, bntraoth, bntraica, bntraicn, bntraia, bntralds, bntrable,
         bntrablm,
         gender, ATTEND1, cendiv, 
         agecats, compgrad, numfrien, frrelblf, frnotrel,
         sfrrelbl, sfrnotrl) %>%
  mutate(attend = ifelse(ATTEND1 == 999, 0, ifelse(ATTEND1 %in% c(666,777), 
                                                   NA_real_, ATTEND1)),
         northeast = ifelse(cendiv %in% c(1,2), 1,0),
         midwest = ifelse(cendiv %in% c(3,4), 1, 0),
         south = ifelse(cendiv %in% c(5,6,7), 1, 0),
         west = ifelse(cendiv %in% c(8,9), 1, 0),
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
         bntraafm = bntrablm) %>%
  select(ids, bntraev, bntramnl, bntracat, bntrajew,
         bntranor, bntraoth, bntradk, bntralds, bntraaf, bntraafm,
         gender, attend, midwest, south, west, agecats,
         compgrad, frnds, fshrblf, pshrblf) %>%
  mutate(bntraaf = ifelse(bntraafm == 1, 1, bntraaf))


c4 <- n4 %>%
  select(ids, bntraev_w4, bntramnl_w4, bntracat_w4, bntrajew_w4,
         bntranr_w4, bntraoth_w4, bntraica_w4, bntraicn_w4, bntraia_w4, bntralds_w4, 
         bntrable_w4, bntrablm_w4,
         gender_w4, attend1_w4, cendiv_w4, 
         agecats_w4, compgrad_w4, numfrien_w4, frrelblf_w4, frnotrel_w4,
         sfrrelbl_w4, sfrnotrl_w4) 
  
  select(ids, bntraev, bntramnl, bntracat, bntrajew,
         bntranr, bntraoth, bntraica, bntraicn, bntraia, bntralds, bntrable,
         bntrablm,
         gender, ATTEND1, cendiv, 
         agecats, compgrad, numfrien, frrelblf, frnotrel,
         sfrrelbl, sfrnotrl) %>%
  mutate(attend = ifelse(ATTEND1 == 999, 0, ifelse(ATTEND1 %in% c(666,777), 
                                                   NA_real_, ATTEND1)),
         northeast = ifelse(cendiv %in% c(1,2), 1,0),
         midwest = ifelse(cendiv %in% c(3,4), 1, 0),
         south = ifelse(cendiv %in% c(5,6,7), 1, 0),
         west = ifelse(cendiv %in% c(8,9), 1, 0),
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
         bntraafm = bntrablm) %>%
  select(ids, bntraev, bntramnl, bntracat, bntrajew,
         bntranor, bntraoth, bntradk, bntralds, bntraaf, bntraafm,
         gender, attend, midwest, south, west, agecats,
         compgrad, frnds, fshrblf, pshrblf) %>%
  mutate(bntraaf = ifelse(bntraafm == 1, 1, bntraaf))


l5$coeff

lodds2 <- cbind(1, cmat) %*% mvrnorm(1, as.vector(l5$coeff), l5$coeff.V)[1:17] 
lodds3 <- cbind(1, cmat) %*% mvrnorm(1, as.vector(l5$coeff), l5$coeff.V)[18:34] 
lodds4 <- cbind(1, cmat) %*% mvrnorm(1, as.vector(l5$coeff), l5$coeff.V)[35:51] 
lodds5 <- cbind(1, cmat) %*% mvrnorm(1, as.vector(l5$coeff), l5$coeff.V)[52:68] 

exp(c(1, lodds2, lodds3, lodds4, lodds5))/
  (1 + exp(lodds2) + exp(lodds3) + exp(lodds4) + exp(lodds5))

probs <- cbind(1/
  (1 + exp(lodds2) + exp(lodds3) + exp(lodds4) + exp(lodds5)),
exp(lodds2)/
  (1 + exp(lodds2) + exp(lodds3) + exp(lodds4) + exp(lodds5)),
exp(lodds3)/
  (1 + exp(lodds2) + exp(lodds3) + exp(lodds4) + exp(lodds5)),
exp(lodds4)/
  (1 + exp(lodds2) + exp(lodds3) + exp(lodds4) + exp(lodds5)),
exp(lodds5)/
  (1 + exp(lodds2) + exp(lodds3) + exp(lodds4) + exp(lodds5)))

probs[!is.na(probs),]


sample(1:5, 1:nrow(probs), replace = TRUE, prob = probs)

cmat <- controls_3 %>%
  select(bntraev, bntramnl, bntracat, bntrajew, bntraoth, bntradk,
  bntralds, bntraaf, attend, gender, midwest, south, west, 
  agecats, compgrad, pshrblf) %>%
  as.matrix()
cbind(1, cmat)

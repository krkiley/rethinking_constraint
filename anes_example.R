load("~/Dropbox/hill_kreisi/results/ambivalence_everywhere_results/full_data_results/anes90results.Rdata")


source("~/Dropbox/ambivalence_everywhere/functions/model_function.R")

anes_stable_results %>%
  filter(govins, jobguar, servspend, fschild9, fscrime9, 
         fsaids9, fsschool9, fswelf9, fsfood9, fsenviro9,
         fssocsec9, govblks, affactanes9, eqop, toofar, 
         nochance, morechance, worryless, #10
         treateq, newstyles, moretol, adjmoral, tradties, #15
         eqroles, homomil9, homojob9, abortion9)

anes90results <- list(govins_model, jobguar_model, servspend_model, govblks_model, affactanes9_model, #5
                      eqop_model, toofar_model, nochance_model, morechance_model, worryless_model, #10
                      treateq_model, newstyles_model, moretol_model, adjmoral_model, tradties_model, #15
                      eqroles_model, libcon90_model, partyid90_model, dontcare90_model, complex90_model, #20
                      govbiz90_model, termlimits_model, congapprove_model, stayhome_model, thermclint_model, #25
                      thermgore_model, thermhill_model, thermjksn_model, thermblks_model, thermwhts_model, #30
                      thermcons_model, thermlibs_model, thermunion_model, thermbiz_model, thermpoor_model, #35
                      thermwelf_model, thermhisp_model, thermenviro_model, thermwomlib_model, betterecon_model, #40 
                      betterfrgn_model, betterhlth_model, raisetax_model, clintspend_model, repspend_model, #45
                      demspend_model, replibcon_model, demlibcon_model, fschild9_model, fscrime9_model, #50
                      fsaids9_model, fsschool9_model, fswelf9_model, fsfood9_model, fsenviro9_model, #55
                      fssocsec9_model, incimm9_model, homomil9_model, homojob9_model, abortion9_model, #60
                      schpray9_model, relimpt9_model, viewbible9_model, crooked9_model, trustgov9_model, #65
                      wastetax9_model) #66


#Sample from .... and look at correlation

anes90 <- read_dta("~/Dropbox/data/anes/anes9297/anes_mergedfile_1992to1997.dta")

anes90 <- anes90 %>%
  mutate(id = 1:nrow(anes90))

id <- anes90 %>%
  select(id)
for (i in 1:length(anes90results)) {
    
    var <- anes90results[[i]]$model_info$var
    qtype <- anes90results[[i]]$model_info$qtype
    df <- anes90results[[i]]$grp_predict_summary %>%
      select(id, y3, prob1)
    names(df) <- c("id", var)
    if (i == 1) {
      anes_stable_results <- left_join(id, df)
    } else {
      anes_stable_results <- left_join(anes_stable_results, df, by = c("id"="id"))
    }
    
}

anes_stable_results <- anes_stable_results %>%
  select(govins, jobguar, servspend, fschild9, fscrime9, 
         fsaids9, fsschool9, fswelf9, fsfood9, fsenviro9,
         fssocsec9, govblks, affactanes9, eqop, toofar, 
         nochance, morechance, worryless, #10
         treateq, newstyles, moretol, adjmoral, tradties, #15
         eqroles, homomil9, homojob9, abortion9)

stab.cors <- cor(anes_stable_results[,2:ncol(anes_stable_results)], use = "pairwise.complete.obs")


stab.cors[abs(stab.cors) < .15] <- 0
stab.cors[abs(stab.cors) > .15] <- 1

#sc2 <- stab.cors[rowSums(stab.cors, na.rm = TRUE) > 1, colSums(stab.cors, na.rm = TRUE) > 1]

stab.g <- graph_from_adjacency_matrix(stab.cors, mode = "undirected", 
                                      diag = FALSE)
plot(stab.g, 
     vertex.size = 5,
     vertex.color = "gray",
     vertex.label.cex = .7,
     vertex.label.color = "black")


gss_stable_results[gss_stable_results > 1]





anes90results[[1]] 

#Stable:Agree. ... #Stable:Disagree... #Ambivalent
id <- anes90 %>%
  select(id)
for (i in 1:length(anes90results)) {
  
  var <- anes90results[[i]]$model_info$var
  qtype <- anes90results[[i]]$model_info$qtype
  
  df <- anes90results[[i]]$grp_predict_summary %>%
    select(id, y1, y2, y3, prob1)
  
  if (qtype %in% c("3", "Five")) {
    df <- df %>%
      mutate(response = ifelse(prob1 < .3, 2, 
                               ifelse(prob1 > .3 & 
                                        (y1 %in% c(1,2) | y2 %in% c(1,2) | y3 %in% c(1,2)), 
                                      1, 3)))
  } else if (qtype == "7") {
    df <- df %>%
      mutate(response = ifelse(prob1 < .3, 2, 
                               ifelse(prob1 > .3 & 
                                        (y1 %in% c(1,2,3) | y2 %in% c(1,2,3) | y3 %in% c(1,2,3)), 
                                      1, 3)))
    
  }
  
  df <- df %>%
    select(id, response)
  names(df) <- c("id", var)
  if (i == 1) {
    anes_stable_results <- left_join(id, df)
  } else {
    anes_stable_results <- left_join(anes_stable_results, df, by = c("id"="id"))
  }
  
}


as2 <- anes_stable_results %>%
  select(id, govins, jobguar, servspend, fschild9, fscrime9, 
         fsaids9, fsschool9, fswelf9, fsfood9, fsenviro9,
         fssocsec9, govblks, affactanes9, eqop, toofar, 
         nochance, morechance, worryless, #10
         treateq, newstyles, moretol, adjmoral, tradties, #15
         eqroles, homomil9, homojob9, abortion9) 
  
as2 %>%
  gather(key = "variable", value = "value", -id) %>%
  group_by(variable, value) %>%
  summarise(n = n()) %>%
  mutate(value = recode(value, "1"="Stable:Agree", "2"="Ambivalent", "3"="Stable:Disagree")) %>%
  mutate(grp = ifelse(variable %in% c("govins", "jobguar", "servspend", "fschild9", "fscrime9", 
                                 "fsaids9", "fsschool9", "fswelf9", "fsfood9", "fsenviro9",
                                 "fssocsec9"), "econ",
                      ifelse(variable %in% c("govblks", "affactanes9", "eqop", "toofar", 
                                        "nochance", "morechance", "worryless", #10
                                        "treateq"), "civrts", "moral"))) %>%
  mutate(pct = n/sum(n)) %>% 
  select(grp, variable, value, pct) %>%
  spread(value, pct) %>% View()



as2 %>%
  mutate(sum = rowSums(across(c(govins:abortion9), ~ifelse(.x %in% c(1,3), 1, -0)))) %>% 
  ggplot(aes(x = sum)) + 
  geom_histogram(color = "black", fill = "gray", bins = 28) +
  scale_x_continuous(limits = c(0, 27))


  rowwise() %>%
  summarise(across(govins:abortion9), rowSums())

as3 <- as2 %>%
  gather(key = "variable", value = "value", -id) %>%
  unite(new_var, variable, value, sep = "_") %>% mutate(e = 1) %>%
  spread(new_var, e, fill = 0)

test <-(t(as.matrix(as3[,-1])) %*% as.matrix(as3[,-1]))

test/colSums(as.matrix(as3[,-1]))



mat1 <- as.matrix(as3[,-1]) %*% t(as.matrix(as3[,-1]))
mat1[mat1 > .5] <- 1
mat1[mat1 < .5] <- 0
gr1 <- graph_from_adjacency_matrix(mat1, mode = "undirected", weighted = TRUE, diag = FALSE)

plot(gr1)


mat1 <- t(as.matrix(as3[1:200,-1])) %*% as.matrix(as3[1:200,-1])


tidy(multinom(govins ~ as.factor(jobguar), data = as2))/27


g1 <- (as.matrix(as3[,-1]) %*% t(as.matrix(as3[,-1])))



as4 <- as2 %>%
  gather(key = "variable", value = "value", -id) %>%
  unite(new_var, variable, value, sep = "_") %>% mutate(e = 1) %>%
  spread(new_var, e, fill = 0) %>%
  select(-c(contains("_2"))) %>%
  select(-c(contains("_NA")))


mat1 <- as.matrix(as4[1:100,-1]) %*% t(as.matrix(as4[1:100,-1]))
median(mat1)
mat1[mat1 < 9] <- 0
mat1[mat1 >= 9] <- 1
gr1 <- graph_from_adjacency_matrix(mat1, mode = "undirected", diag = FALSE)
plot(gr1, vertex.size = 2)

mat1 <- as.matrix(as4[,-1]) %*% t(as.matrix(as4[,-1]))

mat2 <- t(as.matrix(as4[,-1])) %*% as.matrix(as4[,-1])





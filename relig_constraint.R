source("~/Dropbox/hill_kreisi/functions/model_function.R")


#Load in NSYR
n1 <- read_dta("~/Dropbox/data/nsyr/nsyr1.DTA")
n2 <- read_dta("~/Dropbox/data/nsyr/nsyr2.DTA")
n3 <- read_dta("~/Dropbox/data/nsyr/nsyr3.DTA")
n4 <- read_dta("~/Dropbox/data/nsyr/nsyr4.dta")


# Plan
# Measure of constraint. How to do that?


weights <- diag(var(b2.1[,2:20]))
ab_diff <- function(vec1, vec2) {
  val <- sum(1 - abs(vec1 - vec2))/length(vec1)
  return(val)
}
range01 <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}

b2.1 <- b2 %>% na.omit()

c.dist <- cosine(t(as.matrix(b2.1[,2:14])))
diag(c.dist) <- NA

hist(c.dist)

which(c.dist==0,arr.ind = T)

b2.1[c(586, 2470),]

test_bel <- b2 %>% 
  filter(ids_2 %in% c(1662, 2122, 2470, 2614, 560, 1773, 1952))


n <- 1000
dist <- matrix(NA, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    if (i < j) {
      dist[i,j] <- dist[j,i] <- as.numeric(ab_diff(b2.1[i,2:20], b2.1[j,2:20]))
    }
  }
}

#dist.long <- dist

mat <- ifelse(dist3 > 12, 12, dist)
mat <- ((max(dist3, na.rm = TRUE)-dist3)/max(dist3,na.rm = TRUE))^3

mat2 <- ifelse(dist < 0.773576, 0, dist)

g <- graph_from_adjacency_matrix(mat2, mode = "undirected", weighted = TRUE,
                                 diag = FALSE)
c1 <- cluster_fast_greedy(g, weights = E(g)$weight)

Isolated = which(degree(g)==0)
G2 = delete.vertices(g, Isolated)


lay1 <- layout_with_mds(g)


plot(g2, vertex.size = 2, vertex.label=NA, 
     #layout = layout_with_fr(g, weight = E(g)$weight), 
     vertex.color=ifelse(membership(c1) > 2, 3, membership(c1)),
     edge.width = E(g2)$weight/10)



shortb2 <- b2.1[1:1000,]

shortb2$grp







sample








plot(g2, vertex.size = 2, vertex.label=NA, vertex.color=membership(c1))


b2.1$group <- c1$membership

shortb2 <- b2.1[1:500,]
shortb2$grp <- c1$membership
b2.1$group[b2.1$group > 1 & b2.1$group < 31] <- 2


shortb2 %>%
  group_by(grp) %>%
  summarise(across(c(aftrlife_2, angels_2, demons_2, astrolgy_2, 
                   reincar_2, miracles_2, god_2,
                   heaven_2, godworld_2, moralrel_2, moralchg_2, brkmoral_2, 
                   relprvte_2, unmarsex_2, divrceok_2, manmar_2, wommar_2, mandecid_2, 
                   wrkngmom_2), mean)) %>% View()

which(dist==0,arr.ind = T)

shortb2 %>%
  group_by(grp) %>% mutate(n = n()) %>% mutate(grp = paste("Group ", grp, " (n = ", n, ")", sep = "")) %>%
  select(-n) %>%
  gather(key = "question", value = "resp", -c(ids_2, grp))  %>%
  ggplot(aes(x = question, y = resp, fill = as.factor(grp))) + 
  geom_boxplot(outlier.shape = NA) + 
  facet_grid(.~grp, scales = "free") + 
  coord_flip() + 
  theme_minimal() +
  theme(legend.position = "none")


b2.1 %>%
  group_by(group) %>% mutate(n = n()) %>% mutate(group = paste("Group ", group, " (n = ", n, ")", sep = "")) %>%
  select(-n) %>%
  gather(key = "question", value = "resp", -c(ids_2, group))  %>%
  ggplot(aes(x = question, y = resp, fill = as.factor(group))) + 
  geom_jitter(shape = 21, alpha = .2) + 
  facet_grid(question~group, scales = "free") + 
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none")



b2.1 %>%
  gather(key = "question", value = "resp", -c(ids_2, grp)) %>%
  ggplot(aes(x = question, y = resp, fill = as.factor(grp))) + 
  geom_quasirandom(shape = 21) + 
  coord_flip() + 
  facet_grid(.~grp, scales = "free") + 


View(b2.1[c(50, 53, 183, 242, 311),])
View(b2.1[c(1, 2, 5, 6, 9, 14),])
View(b2.1[c(3,4,7,8),])



#Belief matrix, wave 2
b2 <- n2 %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, wrldorig, goduseev, moralrel, moralchg, brkmoral,
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
         evolut = ifelse(wrldorig %in% c(2, 3, 4, 777), 1, 
                         ifelse(goduseev %in% c(666,888,999), NA_real_, goduseev)),
         evolut = ifelse(evolut == 777, .5, evolut),
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


# Belief matrix wave 3
b3 <- n3 %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, wrldorig, goduseev, moralrel, moralchg, brkmoral) %>%
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
         evolut =  recode(goduseev, "999"=1, "1"=1, "2"=0, "777"=.5, "888"=NA_real_),
         moralrel = ifelse(moralrel %in% c(666,888, 999), NA_real_, moralrel),
         moralrel = range01(moralrel),
         moralchg = ifelse(moralchg %in% c(666,888, 999), NA_real_, moralchg),
         moralchg = range01(moralchg),
         brkmoral = ifelse(brkmoral %in% c(666,888, 999), NA_real_, brkmoral),
         brkmoral = range01(brkmoral)) %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, godworld, moralrel, moralchg, brkmoral, )
names(b3) <- paste(names(b3), "_3", sep = "")

b23 <- full_join(b2, b3, by = c("ids_2"="ids_3"))

diffs <- numeric(length = nrow(b23))
for (i in 1:nrow(b23)) {
  diffs[i] <- as.numeric(ab_diff(b23[i,2:13], b23[i,14:25]))
}

b23$diff <- diffs

df <- left_join(b23, df1, by = c("ids_2"="ids"))

summary(lm(diff ~ 
             #network variables
             # frnds1 + pct_vol + pct_drugs + pct_trouble + pct_shrblf +
             # parclose + anc + 
             #religious tradition
             bntrtev + bntrtmnl + bntrtafp + bntrtcat + bntrtjew + 
             bntrtmor + bntrtoth + bntracdk + attend + 
             #demographics 
             female + age + race_black + race_other + south + 
             peduc + income + two_adult + gpa, data = df))
#Frequency of Church attendance at wave 1 is associated with less belief change between waves 2 and 3.
#Being a mormon and being an evangelical at wave 1 is associated with less belief change between waves 2 and 3.
#higher GPA and higher parent edcucation are associated wiht less change.

b12 <- b12 %>%
  mutate(delta_afterlife = abs(aftrlife_1 - aftrlife_2),
         delta_angels = abs(angels_1 - angels_2),
         delta_demons = abs(demons_1 - demons_2),
         delta_astrolgy = abs(astrolgy_1 - astrolgy_2),
         delta_reincar = abs(reincar_1 - reincar_2),
         delta_miracles = abs(miracles_1 - miracles_2),
         delta_god = abs(god_1 - god_2),
         delta = delta_afterlife + delta_angels + delta_demons + 
           delta_astrolgy + delta_reincar + delta_miracles + delta_god)


ggplot(b12, aes(x=delta)) + 
  geom_histogram(bins = 14, color = "black", fill = "gray")


### Wave 4

b4 <- n4 %>%
  select(ids, afterlife_w4, angels_w4, demons_w4, astrolgy_w4, reincar_w4, miracles_w4, god_w4,
         heaven_w4, wrldorigin_w4, goduseevoltn_w4, moralrel_w4, moralitychnge_w4, brkmorality_w4) %>%
  mutate(aftrlife = afterlife_w4, angels = angels_w4, demons = demons_w4, 
         astrolgy = astrolgy_w4, reincar = reincar_w4, miracles = miracles_w4, 
         god = god_w4, heaven = heaven_w4, wrldorig = wrldorigin_w4, 
         goduseev = goduseevoltn_w4,  moralrel = moralrel_w4, 
         moralchg = moralitychnge_w4,  brkmoral = brkmorality_w4) %>%
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
         evolut =  recode(goduseev, "999"=1, "1"=1, "2"=0, "777"=.5, "888"=NA_real_),
         moralrel = ifelse(moralrel %in% c(666,888, 999), NA_real_, moralrel),
         moralrel = range01(moralrel),
         moralchg = ifelse(moralchg %in% c(666,888, 999), NA_real_, moralchg),
         moralchg = range01(moralchg),
         brkmoral = ifelse(brkmoral %in% c(666,888, 999), NA_real_, brkmoral),
         brkmoral = range01(brkmoral)) %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, godworld, moralrel, moralchg, brkmoral)
names(b4) <- paste(names(b4), "_4", sep = "")


b34 <- full_join(b3, b4, by = c("ids_3"="ids_4"))

diffs <- numeric(length = nrow(b34))
for (i in 1:nrow(b34)) {
  diffs[i] <- as.numeric(ab_diff(b34[i,2:13], b34[i,14:25]))
}

b34$diff <- diffs


df <- left_join(b34, df1, by = c("ids_3"="ids"))

summary(lm(diff ~ 
             #network variables
             # frnds1 + pct_vol + pct_drugs + pct_trouble + pct_shrblf +
             # parclose + anc + 
             #religious tradition
             bntrtev + bntrtmnl + bntrtafp + bntrtcat + bntrtjew + 
             bntrtmor + bntrtoth + bntracdk + attend + 
             #demographics 
             female + age + race_black + race_other + south + 
             peduc + income + two_adult + gpa, data = df))



b23 %>% select(ids_2, diff)
b34 %>% select(ids_3, diff)

t <- full_join(b23 %>% select(ids_2, diff), b34 %>% select(ids_3, diff),
          by = c("ids_2"="ids_3"))

t %>%
  ggplot(aes(x = diff.x, y = diff.y)) + 
  geom_jitter(shape = 21, fill = "gray", alpha = .5) + 
  geom_smooth()

### Wave 1... Which I Might not Use

library(RCA)


r1 <- RCA(b2.1[1:300,2:14])


dist()




#Do they come from the same religious denomination


for (i in 1:100) {
  
}
shortb2[sample(1:500, replace = TRUE),]

boot.mat <- as.matrix(b2.1[sample(1:nrow(b2.1), 10000, replace = TRUE),2:20])
n <- nrow(boot.mat)
dist <- matrix(NA, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    if (i < j) {
      dist[i,j] <- dist[j,i] <- as.numeric(ab_diff(boot.mat[i,], boot.mat[j,]))
    }
  }
}


belief.mat <- as.matrix(b2.1[,2:20])
n <- nrow(belief.mat)
dist <- matrix(NA, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    if (i < j) {
      dist[i,j] <- dist[j,i] <- as.numeric(ab_diff(belief.mat[i,], belief.mat[j,]))
    }
  }
}


mat2 <- ifelse(dist < quantile(dist, .95, na.rm = TRUE), 0, dist^2)

g <- graph_from_adjacency_matrix(mat2, mode = "undirected", weighted = TRUE,
                                 diag = FALSE)
c1 <- cluster_leading_eigen(g, weights = E(g)$weight)

plot(g, vertex.size = 2, vertex.label=NA, 
     #layout = layout_with_fr(g, weight = E(g)$weight), 
     vertex.color=membership(c1),
     edge.width = E(g)$weight/100)

belief.mat %>%
  as.data.frame() %>%
  mutate(group = c1$membership) %>%
  mutate(group = ifelse(group %!in% c(1,46, 47, 48), 5, group)) %>%
  group_by(group) %>% mutate(n = n()) %>% 
  mutate(group = paste("Group ", group, " (n = ", n, ")", sep = "")) %>%
  select(-n) %>%
  gather(key = "question", value = "resp", -c(group))  %>%
  ggplot(aes(x = question, y = resp, fill = as.factor(group))) + 
  geom_boxplot(outlier.shape = NA) + 
  facet_grid(question~group, scales = "free") + 
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none")

t <- b2.1%>%
  mutate(group = c1$membership) %>%
  mutate(group = ifelse(group %!in% c(1,46, 47, 48), 5, group)) %>%
  full_join(full_beliefs, "ids_2"="ids_2") %>%
  mutate(diff = abs(moralchg_2 - moralchg_3))


t <- b2.1%>%
  mutate(group = c1$membership) %>%
  mutate(group = ifelse(group %!in% c(1, 143, 144, 145), 5, group)) %>%
  full_join(full_beliefs, "ids_2"="ids_2") %>%
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
  mutate(abs_diff = abs(`3`-`2`)) %>%
  filter(!is.na(group)) %>%
  ungroup() %>%
  mutate(grp.var = range01(grp.var))

t <- data.frame(t)
pdata <- pdata.frame(x = t, index = c("ids_2", "question"))

m1 <- plm(abs_diff ~ grp.var, model = "within", data = pdata)


summary(lm(diff ~ as.factor(group), data = t))

table(belief.mat[,18])

b3.1 <- b3 %>% na.omit()
belief.mat3 <- as.matrix(b3.1[,2:20])
n <- nrow(b3.1)
dist3 <- matrix(NA, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    if (i < j) {
      dist3[i,j] <- dist3[j,i] <- as.numeric(ab_diff(belief.mat[i,1], belief.mat[j,1]))
    }
  }
}



# What's the average difference between responses
# Does this decrease the average distance between responses
# What grouping decreases average distance between all responses the most

# Is that what you're already doing?

belief.mat


## The first question is whether people who participate in similar organizations have
# have more similar beliefs
# To test this, I ... pariwise belief similarity on 

upper.tri(dist) %>%
  as.data.frame() %>%
  gather(key = "V1")

g.dist  <- graph.adjacency(dist,weighted=TRUE, 
                           mode = "undirected")
df.dist <- get.data.frame(g.dist)

df.dist %>%
  filter(from == 2, to == 1)

ids <- b2.1 %>%
  mutate(pos = 1:nrow(b2.1)) %>%
  select(pos, ids_2:group) %>%
  select(pos, ids_2)


distances <- left_join(df.dist, ids, by = c("from"="pos")) %>%
  mutate(from = ids_2) %>%  select(-ids_2) %>%
  left_join(ids, by = c("to"="pos")) %>%
  mutate(to = ids_2, similarity = weight^2) %>% select(-c(ids_2, weight))

controls <- n2 %>%
  select(ids, bntraev, bntramnl, bntracat, ATTEND1) %>%
  mutate(attend = ifelse(ATTEND1 == 999, 0, ifelse(ATTEND1==666, NA_real_, ATTEND1))) %>%
  select(-ATTEND1)

test <- distances %>%
  left_join(controls, by = c("to"="ids")) %>%
  left_join(controls, by = c("from"="ids")) %>%
  mutate(both_evan = bntraev.x * bntraev.y,
         both_mnl = bntramnl.x * bntramnl.y,
         both_cat = bntracat.x * bntracat.y,
         avg_attend = (attend.x + attend.y)/12)

summary(lm(similarity ~ both_evan + both_mnl + both_cat + 
             avg_attend + 
             I(both_evan * avg_attend) + 
             I(both_mnl * avg_attend) + 
             I(both_cat * avg_attend), 
           data = test))






source("~/Dropbox/hill_kreisi/functions/model_function.R")


#Load in NSYR
n1 <- read_dta("~/Dropbox/data/nsyr/nsyr1.DTA")
n2 <- read_dta("~/Dropbox/data/nsyr/nsyr2.DTA")
n3 <- read_dta("~/Dropbox/data/nsyr/nsyr3.DTA")
n4 <- read_dta("~/Dropbox/data/nsyr/nsyr4.dta")



ab_diff <- function(vec1, vec2) {
  val <- sum(abs(vec1 - vec2))
  return(val)
}
range01 <- function(x){(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))}

b2.1 <- b2 %>% na.omit()

c.dist <- cosine(t(as.matrix(b2.1[,2:14])))
diag(c.dist) <- NA

hist(c.dist)

which(c.dist==0,arr.ind = T)

b2.1[c(586, 2470),]

n <- 200
dist <- matrix(NA, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    if (i < j) {
      dist[i,j] <- dist[j,i] <- as.numeric(ab_diff(b2.1[i,2:14], b2.1[j,2:14]))
    }
  }
}

dist.long <- dist

mat <- max(dist, na.rm = TRUE)-dist

mat2 <- ifelse(dist < 1.5, 1, 0)

g <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE,
                                 diag = FALSE)
c1 <- cluster_edge_betweenness(g)

g2 <- graph_from_adjacency_matrix(mat2, mode = "undirected", weighted = TRUE,
                                  diag = FALSE)

plot(c1, g2, vertex.size = 2, vertex.label=NA, 
     layout = layout_with_fr(g2))




#Belief matrix, wave 2
b2 <- n2 %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, wrldorig, goduseev, moralrel, moralchg, brkmoral) %>%
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
         brkmoral = range01(brkmoral)) %>%
  select(ids, aftrlife, angels, demons, astrolgy, reincar, miracles, god,
         heaven, godworld, moralrel, moralchg, brkmoral)
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
         heaven, godworld, moralrel, moralchg, brkmoral)
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

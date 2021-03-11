

text <- read_csv("~/Dropbox/rethinking_constraint/question_text.csv")
w2_full %>%
  select(ids:wrkngmom) %>%
  gather(key = "question", value = "response", -ids) %>%
  group_by(question, response) %>%
  summarise(n = n()) %>%
  spread(response, n) %>%
  knitr::kable(format = "latex")


l3 <- l3 %>%
  filter(ids %in% w3_withcov$ids)

l4 <- l4 %>%
  filter(ids %in% long_blf$ids)

w2_full %>%
  select(ids:wrkngmom) %>%
  gather(key = "question", value = "response", -ids) %>%
  left_join(l3) %>% left_join(l4) %>%
  gather(key = "time", value = "value", -c(ids, question)) %>%
  filter((time == "response") | (time == "w3" & !is.na(value)) |
           (time == "w4" & !is.na(value))) %>%
  group_by(question, time, value) %>% 
  summarise(n = n()) %>%
  spread(value, n) %>%
  mutate(time = recode(time, "response"=1, "w3"=2, "w4"=3)) %>%
  rowwise() %>%
  mutate(obs = sum(c(`1`, `2`, `3`, `4`, `5`), na.rm = TRUE)) %>%
  mutate(cat = ifelse(question %in% c("aftrlife", "angels", "demons" ,"astrolgy", 
                                      "reincar", "godworld", "heaven", "god",
                                      "miracles"),
                      "Religion",
                      ifelse(question %in% c("moralrel", "moralchg", "brkmoral",
                                             "relprvte"),
                             "Morality", "Family"))) %>%
  left_join(text) %>%
  select(cat, question, text, time:obs) %>%
  arrange(desc(cat), question) %>%
  knitr::kable(format = "latex")

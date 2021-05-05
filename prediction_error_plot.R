

load("~/Dropbox/rethinking_constraint/prediction_error.Rdata")



prediction_error %>%
  filter(question %in% c("miracles", "heaven", "astrolgy")) %>%
  mutate(sd = sum_ssd) %>%
  ggplot(aes(x = model, y = sd, fill = model)) + 
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) + 
  facet_wrap(~question, scales = "free") + 
  theme_bw() +
  labs(y = "Deviation from expected", x = "",
       fill = "") + 
  scale_fill_brewer(type = "qual", palette = "Paired") +
  geom_vline(xintercept = 3.5, linetype = 2, color = "black") + 
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=7))  +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))


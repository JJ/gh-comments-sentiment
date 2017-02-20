library(ggplot2)
library(dplyr)
data<-read.csv("results-20170220-181013.csv", header=TRUE)
stats1<-data %>% select(lang, count) %>% 
  mutate(tone=as.factor(ifelse(count>0, "positive", ifelse(count<0, "negative", "neutral")))) %>%
  select(lang, tone) %>% 
  group_by(lang, tone) %>% 
  summarize(count=n()) %>%
  ungroup() %>%  arrange(lang, desc(tone)) %>%
  group_by(lang) %>%
  mutate(tone_percent = 100*count/sum(count), label_pos=cumsum(tone_percent) - 0.5 * tone_percent)
temp<-stats1 %>% filter(tone=="positive") %>% arrange(-tone_percent) %>% select(lang)
temp$pos<-seq.int(nrow(temp))
stats1<-merge(stats1, temp, by="lang")
ggplot() + theme_bw() + geom_bar(aes(y=tone_percent, x=reorder(lang, -pos), fill=tone), data=stats1, stat="identity") + 
  geom_text(data=stats1, aes(x = lang, y = label_pos, ymax=label_pos, hjust = 0.5, label = paste0(round(tone_percent),"%")), size=4) + 
  labs(x="Language", y="Percentage of sentiment") +
  scale_fill_manual(values=c('#F45E5A', '#5086FF', '#17B12B'), guide = guide_legend(reverse = TRUE)) + 
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) + coord_flip()

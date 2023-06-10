library(likert)
library(tidyverse)
library(stringr)
library(viridis)

likert_data <- read.csv("survey_likert.csv")

colnames(likert_data)

likert_data %<>% 
  group_by(Question) %>% 
  mutate(Sum = sum(Count)) %>% 
  group_by(Category) %>% 
  mutate(percent = round(100*Count/Sum, 2))


unique(likert_data$Category)

likert_data$Category <- factor(likert_data$Category,
                               levels = c("Strongly agree",
                                          "Somewhat agree",
                                          "Neither agree nor disagree",
                                          "Somewhat disagree",
                                          "Strongly disagree"))

myColors <- c("#0571b0","#92c5de","#f7f7f7","#f4a582","#ca0020", 'black')#actual plot creation

ggplot(data = likert_data, aes(x = Question , y = percent, fill = Category)) +
  geom_bar(stat="identity", width = 0.7) +
  scale_fill_manual(values = myColors)+
  theme_classic() +
  coord_flip() +
  ylab("Percentage (%)") +
  xlab(" ") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  theme(axis.text=element_text(size=14),
       axis.title=element_text(size=14),
       legend.text = element_text(size=12))

library(likert)
library(tidyverse)
library(stringr)
library(viridis)
library(patchwork)

# data with column names as codes
likert_data <- read.csv("data/often.csv")
likert_impact <- read.csv("data/impacts.csv")

colnames(likert_data)

# [1] "Often"               "Always"              "Most.of.the.time"   
# [4] "About.half.the.time" "Sometimes"           "Never"


colnames(likert_impact)

# [1] "Impact"                     "Strongly.agree"            
# [3] "Agree"                      "Neither.agree.nor.disagree"
# [5] "Disagree"                   "Strongly.disagree" 


######### Likert Uncertainty Often ########


likert_data$Often <- recode_factor(likert_data$Often,
                           "The environment (e.g., any physical or ecological aspect of a system)"  = "The environment",
                           "The evidence (e.g., existing knowledge on an intervention or management strategy)" = "Evidence",
                           "The actions or decisions of teammates, and/or colleagues)"  = "Teammates and/or colleagues",
                           "The actions or decisions of leaders of my agency or organization)" = "Agency or organization leaders",
                           "The actions or decisions of elected leaders)" = "Elected leaders",
                           "The actions or decisions of stakeholders or rightsholders.)" = "Stakeholders or rightsholders",
                           "Funding (e.g., availability and timelines)" = "Funding")

likert_impact$Impact <- recode_factor(likert_impact$Impact,
                                   "The environment (e.g., any physical or ecological aspect of a system)"  = "The environment",
                                   "The evidence (e.g., existing knowledge on an intervention or management strategy)" = "Evidence",
                                   "The actions or decisions of teammates, and/or colleagues)"  = "Teammates and/or colleagues",
                                   "The actions or decisions of leaders of my agency or organization)" = "Agency or organization leaders",
                                   "The actions or decisions of elected leaders)" = "Elected leaders",
                                   "The actions or decisions of stakeholders or rightsholders.)" = "Stakeholders or rightsholders",
                                   "Funding (e.g., availability and timelines)" = "Funding")




colnames(likert_data)

likert_often <- likert_data %>% pivot_longer(Always:Never,
                             names_to = "ranking",
                             values_to = "count")

likert_often <- likert_often %>% rename(type = Often)


often_sum <- likert_often %>% 
            group_by(type) %>% 
            mutate(Sum = sum(count)) %>% 
            group_by(ranking) %>% 
            mutate(percent = round(100*count/Sum, 2))


unique(likert_often$ranking)

likert_often$ranking <- factor(likert_often$ranking,
                               levels = c("Never",
                                          "Sometimes",
                                          "About.half.the.time",
                                          "Most.of.the.time",
                                          "Always"))


likert_often

write.csv(likert_often, 'data/likert_often.csv')
write.csv(often_sum, 'data/likert_often_sum.csv')


unique(likert_often$type)



unique(likert_often$ranking)


t <- often_sum %>% filter(type %in% list_pres)


######## ggplot ###
often_sum$ranking <- recode_factor(often_sum$ranking,
                                   'Most.of.the.time' = 'Most of the time',
                                   'About.half.the.time' = 'About half the time')

colours = c("Always" = "#005f73",
            "Most of the time" = "#0a9396",
            "About half the time" = "#ee9b00",
            "Sometimes" = "#ca6702",
            "Never" = "#9b2226")


often_sum$ranking <- factor(often_sum$ranking, levels = c('Always',
                                                'Most of the time',
                                                'About half the time',
                                                'Sometimes',
                                                'Never'))

often_sum$ranking <- recode_factor(often_sum$ranking,
                                   'Most.of.the.time' = 'Most of the time',
                                   'About.half.the.time' = 'About half the time')

unique(often_sum$type)


often_plot <- ggplot(data = often_sum, aes(x = type, y = percent, fill = ranking)) +
            geom_bar(stat="identity", width = 0.7) +
            theme_classic() +
            coord_flip() +
            ylab("Percentage (%)") +
            xlab(" ") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14),
                  legend.text = element_text(size=12),
                  legend.title = element_blank()) +
            ggtitle("How often does _____ cause you uncertainty?") +
            guides(fill = guide_legend(reverse = TRUE)) +
            scale_fill_manual(values = colours)

# ,
# label.position = "bottom"
# legend.position = "bottom"

######## Likert Impact

impacts <- likert_impact %>% pivot_longer(Strongly.agree:Strongly.disagree,
                                             names_to = "ranking",
                                             values_to = "count")

impact_sum <- impacts %>% replace_na(list(count = 0)) %>% 
            group_by(Impact) %>% 
            mutate(Sum = sum(count)) %>% 
            group_by(ranking) %>% 
            mutate(percent = round(100*count/Sum, 2))

unique(impact_sum$ranking)

impact_sum$ranking <- recode_factor(impact_sum$ranking,
                                   'Strongly.disagree' = 'Strongly disagree',
                                   'Neither.agree.nor.disagree' = 'Neither agree nor disagree',
                                 'Strongly.agree' = 'Strongly agree')

impact_sum$ranking <- factor(impact_sum$ranking,
                               levels = c("Strongly agree",
                                          "Agree",
                                          "Neither agree nor disagree",
                                          "Disagree",
                                          "Strongly disagree"))

write.csv(impacts, 'data/likert_impacts.csv')


colours_imp = c("Strongly agree" = "#005f73",
            "Agree" = "#0a9396",
            "Neither agree nor disagree" = "#ee9b00",
            "Disagree" = "#ca6702",
            "Strongly disagree" = "#9b2226")


impact_plot <- ggplot(data = impact_sum, aes(x = Impact, y = percent, fill = ranking)) +
            geom_bar(stat="identity", width = 0.7) +
            scale_fill_manual(values = colours_imp)+
            theme_classic() +
            coord_flip() +
            ylab("Percentage (%)") +
            xlab(" ") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14),
                  legend.text = element_text(size=12),
                  legend.title = element_blank()) +
            ggtitle("How does _____ impact your ability to make decisions?") +
            guides(fill = guide_legend(reverse = T))


likert_plot <- often_plot / impact_plot + plot_annotation(tag_levels = 'A')
likert_plot

ggsave('output/likert_often_impact.jpg', likert_plot)

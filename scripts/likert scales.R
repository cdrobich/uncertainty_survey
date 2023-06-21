library(likert)
library(tidyverse)
library(stringr)
library(viridis)

# data with column names as codes
likert_data <- read.csv("data/often.csv")
likert_impact <- read.csv("data/impacts.csv")


colnames(likert_data)

### Translate

colnames(likert_data)

# [1] "Often"               "Always"              "Most.of.the.time"   
# [4] "About.half.the.time" "Sometimes"           "Never"


colnames(likert_impact)

# [1] "Impact"                     "Strongly.agree"            
# [3] "Agree"                      "Neither.agree.nor.disagree"
# [5] "Disagree"                   "Strongly.disagree" 


######### Likert Uncertainty Often ########

colnames(likert_data)

likert_often <- likert_data %>% pivot_longer(Always:Never,
                             names_to = "ranking",
                             values_to = "count")

likert_often <- likert_often %>% rename(type = Often)


likert_often %<>% 
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

unique(likert_often$type)


list_pres <- c("The environment (e.g., any physical or ecological aspect of a system)",
               "The evidence (e.g., existing knowledge on an intervention or management strategy)",
               "The actions or decisions of elected leaders)",
               "Funding (e.g., availability and timelines)")


unique(likert_often$ranking)


t <- likert_often %>% filter(type %in% list_pres)


ggplot(data = likert_often, aes(x = type, y = percent, fill = ranking)) +
            geom_bar(stat="identity", width = 0.7) +
            scale_fill_manual(values = colours)+
            theme_classic() +
            coord_flip() +
            ylab("Percentage (%)") +
            xlab(" ") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14),
                  legend.text = element_text(size=12),
                  legend.title = element_blank(),
                  legend.position = "bottom") +
            ggtitle("How often does XX cause you uncertainty") +
            guides(fill = guide_legend(reverse = TRUE,
                                       label.position = "bottom"))


ggplot(data = t, aes(x = type, y = percent, fill = ranking)) +
            geom_bar(stat="identity", width = 0.7) +
            scale_fill_manual(values = colours)+
            theme_grey() +
            coord_flip() +
            ylab("Percentage (%)") +
            xlab(" ") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14),
                  legend.text = element_text(size=12),
                  legend.title = element_blank(),
                  legend.position = "bottom") +
            #ggtitle("How often does XX cause you uncertainty") +
            guides(fill = guide_legend(reverse = TRUE,
                                       label.position = "bottom"))


colours = c("Always" = "#2c7bb6",
            "Most.of.the.time" = "#abd9e9",
            "Sometimes" = "#ffffbf",
            "About.half.the.time" = "#fdae61",
            "Never" = "#d7191c")



impacts <- likert_impact %>% pivot_longer(Strongly.agree:Strongly.disagree,
                                             names_to = "ranking",
                                             values_to = "count")


impacts <- impacts

impacts %<>% replace_na(list(count = 0)) %>% 
            group_by(Impact) %>% 
            mutate(Sum = sum(count)) %>% 
            group_by(ranking) %>% 
            mutate(percent = round(100*count/Sum, 2))

unique(impacts$ranking)



impacts$ranking <- factor(impacts$ranking,
                               levels = c("Strongly.disagree",
                                          "Disagree",
                                          "Neither.agree.nor.disagree",
                                          "Agree",
                                          "Strongly.agree"))

write.csv(impacts, 'data/likert_impacts.csv')


f <- impacts %>% filter(Impact %in% list_pres)
f



ggplot(data = f, aes(x = Impact, y = percent, fill = ranking)) +
            geom_bar(stat="identity", width = 0.7) +
            scale_fill_manual(values = colours_imp)+
            theme_gray() +
            coord_flip() +
            ylab("Percentage (%)") +
            xlab(" ") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
            theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=14),
                  legend.text = element_text(size=12),
                  legend.title = element_blank(),
                  legend.direction = "horizontal",
                  legend.position = "bottom") +
            ggtitle("impact") +
            guides(fill = guide_legend(reverse = T,
                                       label.position = "bottom"))


colours_imp = c("Strongly.agree" = "#2c7bb6",
            "Agree" = "#abd9e9",
            "Neither.agree.nor.disagree" = "#ffffbf",
            "Disagree" = "#fdae61",
            "Strongly.disagree" = "#d7191c")


unique(likert_often$ranking)
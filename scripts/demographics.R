library(tidyverse)
library(viridis)
library(stringr)
library(patchwork)


data <- read.csv("data/survey_likerts.csv")
colnames(data)
str(data)

data$organization <- recode_factor(data$organization,
                                   "Non-government agency (e.g., Nature Conservancy Canada, Ducks Unlimited Canada)" = "Non-government Agency",
                                   "Canadian provincial or territorial government agency" = "Provincial/Territorial Government",
                                   "Canadian federal government agency" = "Federal Government")
unique(data$organization)


data %>% count(organization) 

#                        organization   n
# 1             Non-government Agency  44
# 2 Provincial/Territorial Government  31
# 3                Federal Government  30
# 4                  Municipal agency   2
# 5                             Other  10
# 6            Conservation authority  22
# 7          Industry/private company   9
# 9             Indigenous government   1



data$duration <- recode_factor(data$duration,
                               "Over 10 years" = "over 10 years")


data$duration <- recode_factor(data$duration,
                               "over 10 years" = "> 10 years",
                               "Less than 1 year" = "< 1 year")
unique(data$duration)

count <- data %>% count(duration)
count <- count[-6,]

count %>%  
          mutate(Sum = sum(n)) %>% 
          group_by(duration) %>% 
          mutate(percent = round(100*n/Sum, 2))

#   duration         n   Sum percent
# 1 > 10 years      62   148   41.9 
# 2 < 1 year        12   148    8.11
# 3 1 - 2 years     18   148   12.2 
# 4 3 - 5 years     26   148   17.6 
# 5 5 - 10 years    30   148   20.3 

41.9 + 20.3



org_colour = c("Non-government Agency" = '#277f8e',
               'Provincial/Territorial Government' = '#a0da39',
               'Federal Government' = '#4ac16d',
               'Municipal agency' = '#1fa187',
               'Other' = '#fde725',
               'Conservation authority' = '#365c8d',
               'Industry/private company' = '#46327e',
               'Indigenous government' = '#440154')



orgs <- data %>% count(organization)
orgs <- orgs[-8,]
colnames(orgs)



org_n <- orgs %>% mutate(organization = fct_reorder(organization,
                                           n, .desc = FALSE)) %>% 
            ggplot() + 
            geom_segment(aes(x = organization, xend = organization, 
                              y = 0, yend = n, 
                             colour = organization),lwd = 5) +
  geom_point(aes(x = organization, y = n, colour = organization),
             size=7, shape = 19, stroke = 1.5) +
            coord_flip() +
            xlab(" ") +
            ylab("Number of respondents") +
            theme_minimal() +
            theme(axis.text = element_text(size = 13),
                  axis.title = element_text(size = 13),
                  legend.position = "none",
                  legend.key.size = unit(1, 'cm'),
                  legend.text = element_text(size = 12),
                  #legend.direction = "horizontal",
                  legend.title = element_blank()) +
            ylim(0, 50) +
            scale_colour_manual(values = org_colour)



###### years worked

colnames(data)

duration <- data %>% group_by(organization) %>% 
            count(duration)
duration <- duration[-31,]
duration <- duration[-25,]

unique(duration$duration)


duration_n <- duration %>% ggplot(aes(fill = organization, y = n, 
                        x = factor(duration,
                                   level = c('< 1 year',
                                             '1 - 2 years',
                                             '3 - 5 years',
                                             '5 - 10 years',
                                             '> 10 years')))) +
            geom_bar(position = "dodge", stat = "identity") +
            theme_minimal() +
            coord_flip() +
            xlab(" ") +
            ylab("Number of respondents") +
            theme_minimal() +
            theme(axis.text = element_text(size = 13),
                  axis.title = element_text(size = 13),
                  legend.position = c(0.65,0.2),
                  legend.key.size = unit(0.5, 'cm'),
                  legend.text = element_text(size = 12),
                  #legend.direction = "horizontal",
                  legend.title = element_blank()) +
            scale_fill_manual(values = org_colour)


org_dur <- org_n + duration_n  + 
            plot_layout(ncol = 2, widths = c(1,2)) +
            plot_annotation(tag_levels = 'A')

### location

location <- data %>% select(location)

location <- location %>% separate_longer_delim(location, delim = ",")

loc <- location %>% count(location)
loc <- loc[2:15,]
write.csv(loc, "location.csv")



location_n <- loc %>% mutate(location = fct_reorder(location,
                                           n, .desc = FALSE)) %>% 
            ggplot() + 
            geom_segment( aes(x = location, xend = location, 
                              y = 0, yend = n),
                          colour = "#2a9d8f", lwd = 4) +
            geom_point(aes(x = location, y = n), colour = "#2a9d8f",
                       fill = "white", size=6, shape = 19, stroke = 1.5) +
            coord_flip() +
            xlab(" ") +
            ylab("Number of respondents") +
            theme_minimal() +
            theme(axis.text = element_text(size = 18),
                  axis.title = element_text(size = 18),
                  #legend.position = "none",
                  legend.key.size = unit(1, 'cm'),
                  legend.text = element_text(size = 12),
                  #legend.direction = "horizontal",
                  legend.title = element_blank()) +
            ylim(0, 70)




############ LIKERT ###########33

colnames(data)

unique(data$organization)

likert <- data %>% filter(organization %in% org_list)

colours = c("Always" = "#264653",
            "Most of the time" = "#2A9D8F",
            "Never" = "#E9C46A",
            "About half the time" = "#F4A261",
            "Sometimes" = "#e76F51")


likert$collaborative_within <- factor(likert$collaborative_within, levels = c('Never',
                                                'Sometimes',
                                                'About half the time',
                                                'Most of the time',
                                                'Always'))



org_list <- c("Conservation authority",
              "Non-government Agency",
              "Federal Government",
              "Industry/private company",
              "Provincial/Territorial Government")

##### Decision making within my org. is collaborative #####
collab_within_org <- likert %>% group_by(organization) %>% 
            count(collaborative_within)


collab_within_org <- collab_within_org[-5,]

collab_within_org2 <- read.csv("collaborative_within.csv")

### add a 'never' for each org

collab_within_org %>% ggplot(aes(fill = collaborative_within, y = n, 
                                 label = n,
                         x = organization)) + 
            geom_bar(position = "dodge", stat = "identity") +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 15),
                  axis.title = element_text(size = 15),
                  legend.position = "none",
                  legend.key.size = unit(0.5, 'cm'),
                  legend.text = element_text(size = 12),
                  #legend.direction = "horizontal",
                   legend.title = element_blank(),
                  plot.title = element_text(size = 15, face = "bold")) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
            scale_fill_manual(values = colours) +
            guides(fill = guide_legend(reverse = TRUE)) +
            ggtitle("...collaborative within my organization") +
            ylim(0,20)


collab_outside_org <- likert %>% group_by(organization) %>% 
            count(collaborative_outside)

collab_outside_org <- collab_outside_org[-5,]



collab_outside_org$collaborative_outside <- factor(collab_outside_org$collaborative_outside, levels = c('Never',
                                                                              'Sometimes',
                                                                              'About half the time',
                                                                              'Most of the time',
                                                                              'Always'))



collab_outside_org %>% ggplot(aes(fill = collaborative_outside, y = n, 
                                 label = n,
                                 x = organization)) + 
            geom_bar(position = "dodge", stat = "identity") +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 15),
                  axis.title = element_text(size = 15),
                  legend.position = "none",
                  #legend.key.size = unit(0.5, 'cm'),
                  legend.text = element_text(size = 12),
                  legend.title = element_blank(),
                  plot.title = element_text(size = 15, face = "bold")) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
            scale_fill_manual(values = colours) +
            guides(fill = guide_legend(reverse = TRUE)) +
            ggtitle("...collaborative outside my organization") +
            ylim(0, 20)

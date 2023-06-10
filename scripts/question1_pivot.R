library(tidyverse)
library(fmsb)
library(forcats)

q1 <- read.csv("data/question1_reduced.csv")
q1p <- read.csv("data/question1_reduced_proportion.csv")
colnames(q1)
str(q1) 

### Radar Chart ####

q1_long <- q1 %>% 
            select(CODE:RANK1)

q1_long <- as.data.frame(q1_long)

dim(q1_long)

q1_rank1_wide <- pivot_wider(q1_long,
            names_from = CODE,
            values_from = RANK1)

q1_rank1_wide <- rbind(rep(70,8), rep(0,8), q1_rank1_wide)


radarchart(q1_rank1_wide)



#### Horizontal Lollipop #####
q1 %>% mutate(CODE = fct_reorder(CODE, RANK1, .desc = FALSE)) %>%
            ggplot(aes(x = CODE, y = RANK1)) +
            geom_segment(aes(x = CODE, xend = CODE, y = 0, yend = RANK1),
                         color="#775C99",
                         lwd = 2) +
            geom_point( color="#614B81", size = 6, alpha = 1) +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 13)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("1st Ranked")


q1 %>% mutate(CODE = fct_reorder(CODE, RANK2prop, .desc = FALSE)) %>%
            ggplot(aes(x = CODE, y = RANK2prop)) +
            geom_segment(aes(x = CODE, xend = CODE, y = 0, yend = RANK2prop),
                         color="#775C99",
                         lwd = 2) +
            geom_point( color="#614B81", size = 6, alpha = 1) +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 13)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("2nd Ranked")


q1 %>% mutate(CODE = fct_reorder(CODE, RANK3prop, .desc = FALSE)) %>%
            ggplot(aes(x = CODE, y = RANK3prop)) +
            geom_segment(aes(x = CODE, xend = CODE, y = 0, yend = RANK3prop),
                         color="#775C99",
                         lwd = 2) +
            geom_point( color="#614B81", size = 6, alpha = 1) +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 13)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("3rd Ranked")

q1 %>% mutate(CODE = fct_reorder(CODE, RANK4prop, .desc = FALSE)) %>%
            ggplot(aes(x = CODE, y = RANK4prop)) +
            geom_segment(aes(x = CODE, xend = CODE, y = 0, yend = RANK4prop),
                         color="#775C99",
                         lwd = 2) +
            geom_point( color="#614B81", size = 6, alpha = 1) +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 13)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("4th Ranked")

q1 %>% mutate(CODE = fct_reorder(CODE, RANK5prop, .desc = FALSE)) %>%
            ggplot(aes(x = CODE, y = RANK5prop)) +
            geom_segment(aes(x = CODE, xend = CODE, y = 0, yend = RANK5prop),
                         color="#775C99",
                         lwd = 2) +
            geom_point( color="#614B81", size = 6, alpha = 1) +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 13)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("5th Ranked")


######## Stacked Bar Chart ########

# create a dataset

q1_long <- q1 %>% select(CODE,RANK1:RANK5) %>%
            pivot_longer(RANK1:RANK5,
                    names_to = "rank",
                    values_to = "count")

q1p_long$CODE <- as.factor(q1p_long$CODE)


q1_long$rank <- factor(q1_long$rank, levels = c('RANK5',
                                                'RANK4',
                                                'RANK3',
                                                'RANK2',
                                                'RANK1'))



unique(q1p_long$CODE)


# Stacked

colours = c("RANK1" = "#264653",
            "RANK2" = "#2A9D8F",
            "RANK3" = "#E9C46A",
            "RANK4" = "#F4A261",
            "RANK5" = "#e76F51")



q1_bar <- q1_long %>%
            ggplot(aes(fill = rank, y = count, label = count,
                       x = factor(CODE,
                                  level = c('OTHER',
                                            'NATURE',
                                            'CLIMATE CHANGE',
                                            'LEGISLATION',
                                            'PREDICTION',
                                            'SPECIES',
                                            'SUPPORT',
                                            'RESOURCES',
                                            'POLITICAL WILL',
                                            'COLLABORATION',
                                            'FUNDING',
                                            'INFORMATION'
                                            )))) + 
            geom_bar(position = "stack", stat = "identity") +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 16),
                  axis.title = element_text(size = 16),
                  legend.position = c(0.7, 0.3),
                  legend.key.size = unit(1.8, 'cm'),
                  legend.text = element_text(size = 12),
                  legend.direction = "horizontal",
                  legend.title = element_blank()) +
            geom_text(size = 5, alpha = 1, colour = "white",
                      position = position_stack(vjust = 0.55)) +
            guides(fill = guide_legend(reverse = TRUE,
                                       label.position = "bottom")) +
            scale_fill_manual(values = colours)
q1_bar 

ggsave("output/q1_bar.jpeg")


########## Specific Topics ######

breakdown <- read.csv("data/question1_summary.csv")

breakdown <- breakdown %>% select(CODE:RANK5) %>%
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


breakdown$rank <- factor(breakdown$rank, levels = c('RANK5',
                                                'RANK4',
                                                'RANK3',
                                                'RANK2',
                                                'RANK1'))



### Resources 

rs <- c("RESOURCES", "CAPACITY", "PERSONNEL", "EXPERTISE")

resources <- breakdown %>%
            filter(CODE %in% rs)

resources %>% ggplot(aes(fill = rank, y = count, label = count,
                        x = factor(CODE,
                                   level = c('EXPERTISE',
                                             'PERSONNEL',
                                             'RESOURCES',
                                             'CAPACITY'
                                   )))) + 
            geom_bar(position = "stack", stat = "identity") +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 20),
                  axis.title = element_text(size = 20),
                  #legend.position = "none",
                  legend.key.size = unit(1, 'cm'),
                  legend.text = element_text(size = 12),
                  #legend.direction = "horizontal",
                  legend.title = element_blank()) +
            geom_text(size = 10, alpha = 1, colour = "white",
                      position = position_stack(vjust = 0.55)) +
            guides(fill = guide_legend(reverse = TRUE,
                                       label.position = "bottom")) +
            scale_fill_manual(values = colours)

##### funding

fd <- c("FUNDING", "FUND_AMOUNT", "FUND_LENGTH", 
        "FUND_AVAIL")

funding <- breakdown %>%
            filter(CODE %in% fd)


funding %>% ggplot(aes(fill = rank, y = count, label = count,
                      x = factor(CODE,
                                 level = c('FUND_AMOUNT',
                                           'FUND_LENGTH',
                                           'FUND_AVAIL',
                                           'FUNDING'
                                 ))))+ 
            geom_bar(position = "stack", stat = "identity") +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 20),
                  axis.title = element_text(size = 20),
                  #legend.position = "none",
                  legend.key.size = unit(1, 'cm'),
                  legend.text = element_text(size = 12),
                  #legend.direction = "horizontal",
                  legend.title = element_blank()) +
            geom_text(size = 7, alpha = 1, colour = "white",
                      position = position_stack(vjust = 0.55)) +
            guides(fill = guide_legend(reverse = TRUE,
                                       label.position = "bottom")) +
            scale_fill_manual(values = colours)

##### Information

info <- c("CONSENSUS", "DATA", "LONGTERM", "SITE",
          "IMPACT", "METHODS", "CUMULATIVE", "SUCCESS_CRITERIA",
          "SPECIES")

inform <- breakdown %>%
            filter(CODE %in% info)


inform %>% ggplot(aes(fill = rank, y = count, label = count,
                     x = factor(CODE,
                                level = c('SITE',
                                          'CONSENSUS',
                                          'SUCCESS_CRITERIA',
                                          'CUMULATIVE',
                                          'IMPACT',
                                          'LONGTERM',
                                          'METHODS',
                                          'SPECIES',
                                          'DATA'
                                )))) + 
            geom_bar(position = "stack", stat = "identity") +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 20),
                  axis.title = element_text(size = 20),
                  #legend.position = "bottom",
                  legend.key.size = unit(1, 'cm'),
                  legend.text = element_text(size = 12),
                  #legend.direction = "horizontal",
                  legend.title = element_blank()) +
            geom_text(size = 6, alpha = 1, colour = "white",
                      position = position_stack(vjust = 0.55)) +
            guides(fill = guide_legend(reverse = TRUE,
                                       label.position = "bottom")) +
            scale_fill_manual(values = colours)



##### Collaboration


CL <- c("COMM_COLLAB", "RESPONSIBILITIES", "COMPLEXITY", "COSTS_TRADEOFF")

collab <- breakdown %>%
            filter(CODE %in% CL)


collab %>% ggplot(aes(fill = rank, y = count, label = count,
                      x = factor(CODE,
                                 level = c('COMPLEXITY',
                                           'RESPONSIBILITIES',
                                           'COSTS_TRADEOFF',
                                           'COMM_COLLAB'
                                 )))) + 
            geom_bar(position = "stack", stat = "identity") +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 20),
                  axis.title = element_text(size = 20),
                  #legend.position = "bottom",
                  legend.key.size = unit(1, 'cm'),
                  legend.text = element_text(size = 12),
                  #legend.direction = "horizontal",
                  legend.title = element_blank()) +
            geom_text(size = 6, alpha = 1, colour = "white",
                      position = position_stack(vjust = 0.55)) +
            guides(fill = guide_legend(reverse = TRUE,
                                       label.position = "bottom")) +
            scale_fill_manual(values = colours)






## Political Will

PW <- breakdown %>%
            filter(CODE %in% c('POLITICAL_WILL', 'LEGISLATION'))

PW %>% ggplot(aes(fill = rank, y = count, label = count,
                      x = CODE)) + 
            geom_bar(position = "stack", stat = "identity") +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 20),
                  axis.title = element_text(size = 20),
                  #legend.position = "bottom",
                  legend.key.size = unit(1, 'cm'),
                  legend.text = element_text(size = 12),
                  #legend.direction = "horizontal",
                  legend.title = element_blank()) +
            geom_text(size = 6, alpha = 1, colour = "white",
                      position = position_stack(vjust = 0.55)) +
            guides(fill = guide_legend(reverse = TRUE,
                                       label.position = "bottom")) +
            scale_fill_manual(values = colours)




library(tidyverse)
library(fmsb)
library(forcats)

q1 <- read.csv("data/question1_28jun23.csv")

q1_sum <- q1
q1_sum$sum <- rowSums(q1[,3:7])

str(q1)

colnames(q1)
str(q1) 

#### Summed Total #####
q1_sum %>% mutate(CODE = fct_reorder(CODE, sum, .desc = FALSE)) %>%
            ggplot(aes(x = CODE, y = sum)) +
            geom_segment(aes(x = CODE, xend = CODE, y = 0, yend = sum),
                         lwd = 2) +
            geom_point( size = 6, alpha = 1) +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 13)) +
            ylab("Count") +
            xlab(" ") 

q1_sum %>% mutate(THEME = fct_reorder(THEME , sum, .desc = FALSE)) %>%
            ggplot(aes(x = THEME , y = sum)) +
            geom_segment(aes(x = THEME , xend = THEME , y = 0, yend = sum),
                         lwd = 2) +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 13)) +
            ylab("Count") +
            xlab(" ") 

######## Stacked Bar Chart ########

# create a dataset

q1_long <- q1 %>% select(CODE,RANK1:RANK5) %>%
            pivot_longer(RANK1:RANK5,
                    names_to = "rank",
                    values_to = "count")

q1_long$CODE <- as.factor(q1_long$CODE)


q1_long$rank <- factor(q1_long$rank, levels = c('RANK5',
                                                'RANK4',
                                                'RANK3',
                                                'RANK2',
                                                'RANK1'))



unique(q1_long$CODE)


# Stacked

colours = c("RANK1" = "#264653",
            "RANK2" = "#2A9D8F",
            "RANK3" = "#E9C46A",
            "RANK4" = "#F4A261",
            "RANK5" = "#e76F51")


unique(q1_long$CODE)

q1_long$CODE <- recode(q1_long$CODE,
                SITE = "SITE_INFO",
                SYSTEMS = "SOCPOLSCI_SYSTEMS",
                TIMELINE = "TIMELINES",
                INDIGENOUS = "INDIGENOUS_RIGHTS",
                ACCESS = "ACCESS_TO_EVIDENCE",
                LONGTERM = "LONGTERM_DATA",
                LANDACCESS = "LAND_ACCESS",
                CUMULATIVE = "CUMULATIVE_IMPACTS",
                IMPACT = "IMPACT_INFO",
                INTERNALSUPPORT = "INTERNAL_SUPPORT",
                RESOURCESCAPACITY = "RESOURCES_CAPACITY",
                COMMCOLLAB = "COMMUNICATION_COLLABORATION",
                SUCCESS = "SUCCESS_CRITERIA",
                CLIMATECHANGE = "CLIMATE_CHANGE",
                SPECIES = "SPECIES_DATA",
                EXTERNALSUPPORT = "EXTERNAL_SUPPORT")



q1_bar <- q1_long %>%
            ggplot(aes(fill = rank, y = count, #label = count,
                       x = factor(CODE,
                                  level = c('COSTS',
                                            'SITE_INFO',
                                            'JUSTICE',
                                            'CONSENSUS',
                                            'ECOLOGY',
                                            'RESPONSIBILITIES',
                                            'EXPERTISE',
                                            'SOCPOLSCI_SYSTEMS',
                                            'TIMELINES',
                                            'INDIGENOUS_RIGHTS',
                                            'ACCESS_TO_EVIDENCE',
                                            'LONGTERM_DATA',
                                            'TRADEOFFS',
                                            'LAND_ACCESS',
                                            'PERSONNEL',
                                            'CUMULATIVE_IMPACTS',
                                            'IMPACT_INFO',
                                            'NATURE',
                                            'SCIENCE',
                                            'PREDICTION',
                                            'IMPLEMENTATION',
                                            'METHODS',
                                            'INTERNAL_SUPPORT',
                                            'RESOURCES_CAPACITY',
                                            'LEGISLATION',
                                            'OTHER',
                                            'COMMUNICATION_COLLABORATION',
                                            'SUCCESS_CRITERIA',
                                            'CLIMATE_CHANGE',
                                            'SPECIES_DATA',
                                            'EXTERNAL_SUPPORT',
                                            'POLITICS',
                                            'DATA',
                                            'FUNDING'
                                            )))) + 
            geom_bar(position = "stack", stat = "identity") +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 16),
                  axis.title = element_text(size = 16),
                  legend.position = c(0.8, 0.15),
                  legend.key.size = unit(0.7, 'cm'),
                  legend.text = element_text(size = 10),
                  legend.direction = "horizontal",
                  legend.title = element_blank()) +
            # geom_text(size = 4, alpha = 1, colour = "white",
            #           position = position_stack(vjust = 0.55)) +
            guides(fill = guide_legend(reverse = TRUE,
                                       label.position = "bottom")) +
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('COSTS' = "Costs",
                                        'SITE_INFO' = "Site Info",
                                        'JUSTICE' = 'Justice',
                                        'CONSENSUS' = 'Consensus',
                                        'ECOLOGY' = 'Ecology',
                                        'RESPONSIBILITIES' = 'Responsibilities',
                                        'EXPERTISE' = 'Expertise',
                                        'SOCPOLSCI_SYSTEMS' = 'Soc-Poli-Sci Systems',
                                        'TIMELINES' = 'Timelines',
                                        'INDIGENOUS_RIGHTS' = 'Indigenous Rights',
                                        'ACCESS_TO_EVIDENCE' = 'Access to evidence',
                                        'LONGTERM_DATA' = 'Long-term data',
                                        'TRADEOFFS' = 'Tradeoffs',
                                        'LAND_ACCESS' = 'Land access',
                                        'PERSONNEL' = 'Personnel',
                                        'CUMULATIVE_IMPACTS' = 'Cumulative impacts',
                                        'IMPACT_INFO' = 'Impact info',
                                        'NATURE' = 'Nature',
                                        'SCIENCE' = 'Science',
                                        'PREDICTION' = 'Prediction',
                                        'IMPLEMENTATION' = 'Implementation',
                                        'METHODS' = 'Methods',
                                        'INTERNAL_SUPPORT' = 'Internal support',
                                        'RESOURCES_CAPACITY' = 'Resources/Capacity',
                                        'LEGISLATION' = 'Legislation',
                                        'OTHER' = 'Other',
                                        'COMMUNICATION_COLLABORATION' = 'Communcation/Collaboration',
                                        'SUCCESS_CRITERIA' = 'Success criteria',
                                        'CLIMATE_CHANGE' = 'Climate change',
                                        'SPECIES_DATA' = 'Species data',
                                        'EXTERNAL_SUPPORT' = 'External support',
                                        'POLITICS' = 'Politics',
                                        'DATA' = 'Data',
                                        'FUNDING' = 'Funding'))
q1_bar 

ggsave("output/q1_bar.jpeg",
       width = 10,
       height = 14)



# Each Rank ---------------------------------------------------------------
colours = c("RANK1" = "#264653",
            "RANK2" = "#2A9D8F",
            "RANK3" = "#E9C46A",
            "RANK4" = "#F4A261",
            "RANK5" = "#e76F51")

q1$CODE <- recode(q1$CODE,
                       SITE = "SITE_INFO",
                       SYSTEMS = "SOCPOLSCI_SYSTEMS",
                       TIMELINE = "TIMELINES",
                       INDIGENOUS = "INDIGENOUS_RIGHTS",
                       ACCESS = "ACCESS_TO_EVIDENCE",
                       LONGTERM = "LONGTERM_DATA",
                       LANDACCESS = "LAND_ACCESS",
                       CUMULATIVE = "CUMULATIVE_IMPACTS",
                       IMPACT = "IMPACT_INFO",
                       INTERNALSUPPORT = "INTERNAL_SUPPORT",
                       RESOURCESCAPACITY = "RESOURCES_CAPACITY",
                       COMMCOLLAB = "COMMUNICATION_COLLABORATION",
                       SUCCESS = "SUCCESS_CRITERIA",
                       CLIMATECHANGE = "CLIMATE_CHANGE",
                       SPECIES = "SPECIES_DATA",
                       EXTERNALSUPPORT = "EXTERNAL_SUPPORT")

######### Rank 1 ############
rank1 <- q1 %>% select(CODE:RANK1) %>%
            ggplot(aes(y = RANK1,
                       x = factor(CODE,
                                  level = c('COSTS',
                                            'SITE_INFO',
                                            'JUSTICE',
                                            'CONSENSUS',
                                            'ECOLOGY',
                                            'RESPONSIBILITIES',
                                            'EXPERTISE',
                                            'SOCPOLSCI_SYSTEMS',
                                            'TIMELINES',
                                            'INDIGENOUS_RIGHTS',
                                            'ACCESS_TO_EVIDENCE',
                                            'LONGTERM_DATA',
                                            'TRADEOFFS',
                                            'LAND_ACCESS',
                                            'PERSONNEL',
                                            'CUMULATIVE_IMPACTS',
                                            'IMPACT_INFO',
                                            'NATURE',
                                            'SCIENCE',
                                            'PREDICTION',
                                            'IMPLEMENTATION',
                                            'METHODS',
                                            'INTERNAL_SUPPORT',
                                            'RESOURCES_CAPACITY',
                                            'LEGISLATION',
                                            'OTHER',
                                            'COMMUNICATION_COLLABORATION',
                                            'SUCCESS_CRITERIA',
                                            'CLIMATE_CHANGE',
                                            'SPECIES_DATA',
                                            'EXTERNAL_SUPPORT',
                                            'POLITICS',
                                            'DATA',
                                            'FUNDING'
                                  )))) + 
            geom_bar(position = "stack", stat = "identity",
                     fill = "#264653") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 10)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("Rank 1") +
            ylim(0,35) +
            scale_x_discrete(labels = c('COSTS' = "Costs",
                                        'SITE_INFO' = "Site Info",
                                        'JUSTICE' = 'Justice',
                                        'CONSENSUS' = 'Consensus',
                                        'ECOLOGY' = 'Ecology',
                                        'RESPONSIBILITIES' = 'Responsibilities',
                                        'EXPERTISE' = 'Expertise',
                                        'SOCPOLSCI_SYSTEMS' = 'Soc-Poli-Sci Systems',
                                        'TIMELINES' = 'Timelines',
                                        'INDIGENOUS_RIGHTS' = 'Indigenous Rights',
                                        'ACCESS_TO_EVIDENCE' = 'Access to evidence',
                                        'LONGTERM_DATA' = 'Long-term data',
                                        'TRADEOFFS' = 'Tradeoffs',
                                        'LAND_ACCESS' = 'Land access',
                                        'PERSONNEL' = 'Personnel',
                                        'CUMULATIVE_IMPACTS' = 'Cumulative impacts',
                                        'IMPACT_INFO' = 'Impact info',
                                        'NATURE' = 'Nature',
                                        'SCIENCE' = 'Science',
                                        'PREDICTION' = 'Prediction',
                                        'IMPLEMENTATION' = 'Implementation',
                                        'METHODS' = 'Methods',
                                        'INTERNAL_SUPPORT' = 'Internal support',
                                        'RESOURCES_CAPACITY' = 'Resources/Capacity',
                                        'LEGISLATION' = 'Legislation',
                                        'OTHER' = 'Other',
                                        'COMMUNICATION_COLLABORATION' = 'Communcation/Collaboration',
                                        'SUCCESS_CRITERIA' = 'Success criteria',
                                        'CLIMATE_CHANGE' = 'Climate change',
                                        'SPECIES_DATA' = 'Species data',
                                        'EXTERNAL_SUPPORT' = 'External support',
                                        'POLITICS' = 'Politics',
                                        'DATA' = 'Data',
                                        'FUNDING' = 'Funding'))

rank1

######### Rank 2 ###############
rank2 <- q1 %>% select(CODE:RANK2) %>%
            ggplot(aes(y = RANK2,
                       x = factor(CODE,
                                  level = c('COSTS',
                                            'SITE_INFO',
                                            'JUSTICE',
                                            'CONSENSUS',
                                            'ECOLOGY',
                                            'RESPONSIBILITIES',
                                            'EXPERTISE',
                                            'SOCPOLSCI_SYSTEMS',
                                            'TIMELINES',
                                            'INDIGENOUS_RIGHTS',
                                            'ACCESS_TO_EVIDENCE',
                                            'LONGTERM_DATA',
                                            'TRADEOFFS',
                                            'LAND_ACCESS',
                                            'PERSONNEL',
                                            'CUMULATIVE_IMPACTS',
                                            'IMPACT_INFO',
                                            'NATURE',
                                            'SCIENCE',
                                            'PREDICTION',
                                            'IMPLEMENTATION',
                                            'METHODS',
                                            'INTERNAL_SUPPORT',
                                            'RESOURCES_CAPACITY',
                                            'LEGISLATION',
                                            'OTHER',
                                            'COMMUNICATION_COLLABORATION',
                                            'SUCCESS_CRITERIA',
                                            'CLIMATE_CHANGE',
                                            'SPECIES_DATA',
                                            'EXTERNAL_SUPPORT',
                                            'POLITICS',
                                            'DATA',
                                            'FUNDING'
                                  )))) + 
            geom_bar(position = "stack", stat = "identity",
                     fill = "#2A9D8F") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 10)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("Rank 2") +
            ylim(0,35) +
            scale_x_discrete(labels = c('COSTS' = "Costs",
                                        'SITE_INFO' = "Site Info",
                                        'JUSTICE' = 'Justice',
                                        'CONSENSUS' = 'Consensus',
                                        'ECOLOGY' = 'Ecology',
                                        'RESPONSIBILITIES' = 'Responsibilities',
                                        'EXPERTISE' = 'Expertise',
                                        'SOCPOLSCI_SYSTEMS' = 'Soc-Poli-Sci Systems',
                                        'TIMELINES' = 'Timelines',
                                        'INDIGENOUS_RIGHTS' = 'Indigenous Rights',
                                        'ACCESS_TO_EVIDENCE' = 'Access to evidence',
                                        'LONGTERM_DATA' = 'Long-term data',
                                        'TRADEOFFS' = 'Tradeoffs',
                                        'LAND_ACCESS' = 'Land access',
                                        'PERSONNEL' = 'Personnel',
                                        'CUMULATIVE_IMPACTS' = 'Cumulative impacts',
                                        'IMPACT_INFO' = 'Impact info',
                                        'NATURE' = 'Nature',
                                        'SCIENCE' = 'Science',
                                        'PREDICTION' = 'Prediction',
                                        'IMPLEMENTATION' = 'Implementation',
                                        'METHODS' = 'Methods',
                                        'INTERNAL_SUPPORT' = 'Internal support',
                                        'RESOURCES_CAPACITY' = 'Resources/Capacity',
                                        'LEGISLATION' = 'Legislation',
                                        'OTHER' = 'Other',
                                        'COMMUNICATION_COLLABORATION' = 'Communcation/Collaboration',
                                        'SUCCESS_CRITERIA' = 'Success criteria',
                                        'CLIMATE_CHANGE' = 'Climate change',
                                        'SPECIES_DATA' = 'Species data',
                                        'EXTERNAL_SUPPORT' = 'External support',
                                        'POLITICS' = 'Politics',
                                        'DATA' = 'Data',
                                        'FUNDING' = 'Funding'))

rank2

####### Rank 3 ##############
rank3<- q1 %>% select(CODE:RANK3) %>%
            ggplot(aes(y = RANK3,
                       x = factor(CODE,
                                  level = c('COSTS',
                                            'SITE_INFO',
                                            'JUSTICE',
                                            'CONSENSUS',
                                            'ECOLOGY',
                                            'RESPONSIBILITIES',
                                            'EXPERTISE',
                                            'SOCPOLSCI_SYSTEMS',
                                            'TIMELINES',
                                            'INDIGENOUS_RIGHTS',
                                            'ACCESS_TO_EVIDENCE',
                                            'LONGTERM_DATA',
                                            'TRADEOFFS',
                                            'LAND_ACCESS',
                                            'PERSONNEL',
                                            'CUMULATIVE_IMPACTS',
                                            'IMPACT_INFO',
                                            'NATURE',
                                            'SCIENCE',
                                            'PREDICTION',
                                            'IMPLEMENTATION',
                                            'METHODS',
                                            'INTERNAL_SUPPORT',
                                            'RESOURCES_CAPACITY',
                                            'LEGISLATION',
                                            'OTHER',
                                            'COMMUNICATION_COLLABORATION',
                                            'SUCCESS_CRITERIA',
                                            'CLIMATE_CHANGE',
                                            'SPECIES_DATA',
                                            'EXTERNAL_SUPPORT',
                                            'POLITICS',
                                            'DATA',
                                            'FUNDING'
                                  )))) + 
            geom_bar(position = "stack", stat = "identity",
                     fill = "#E9C46A") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 10)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("Rank 3") +
            ylim(0,35) +
            scale_x_discrete(labels = c('COSTS' = "Costs",
                                        'SITE_INFO' = "Site Info",
                                        'JUSTICE' = 'Justice',
                                        'CONSENSUS' = 'Consensus',
                                        'ECOLOGY' = 'Ecology',
                                        'RESPONSIBILITIES' = 'Responsibilities',
                                        'EXPERTISE' = 'Expertise',
                                        'SOCPOLSCI_SYSTEMS' = 'Soc-Poli-Sci Systems',
                                        'TIMELINES' = 'Timelines',
                                        'INDIGENOUS_RIGHTS' = 'Indigenous Rights',
                                        'ACCESS_TO_EVIDENCE' = 'Access to evidence',
                                        'LONGTERM_DATA' = 'Long-term data',
                                        'TRADEOFFS' = 'Tradeoffs',
                                        'LAND_ACCESS' = 'Land access',
                                        'PERSONNEL' = 'Personnel',
                                        'CUMULATIVE_IMPACTS' = 'Cumulative impacts',
                                        'IMPACT_INFO' = 'Impact info',
                                        'NATURE' = 'Nature',
                                        'SCIENCE' = 'Science',
                                        'PREDICTION' = 'Prediction',
                                        'IMPLEMENTATION' = 'Implementation',
                                        'METHODS' = 'Methods',
                                        'INTERNAL_SUPPORT' = 'Internal support',
                                        'RESOURCES_CAPACITY' = 'Resources/Capacity',
                                        'LEGISLATION' = 'Legislation',
                                        'OTHER' = 'Other',
                                        'COMMUNICATION_COLLABORATION' = 'Communcation/Collaboration',
                                        'SUCCESS_CRITERIA' = 'Success criteria',
                                        'CLIMATE_CHANGE' = 'Climate change',
                                        'SPECIES_DATA' = 'Species data',
                                        'EXTERNAL_SUPPORT' = 'External support',
                                        'POLITICS' = 'Politics',
                                        'DATA' = 'Data',
                                        'FUNDING' = 'Funding'))

rank3


####### Rank 4 #############

rank4<- q1 %>% select(CODE:RANK4) %>%
            ggplot(aes(y = RANK4,
                       x = factor(CODE,
                                  level = c('COSTS',
                                            'SITE_INFO',
                                            'JUSTICE',
                                            'CONSENSUS',
                                            'ECOLOGY',
                                            'RESPONSIBILITIES',
                                            'EXPERTISE',
                                            'SOCPOLSCI_SYSTEMS',
                                            'TIMELINES',
                                            'INDIGENOUS_RIGHTS',
                                            'ACCESS_TO_EVIDENCE',
                                            'LONGTERM_DATA',
                                            'TRADEOFFS',
                                            'LAND_ACCESS',
                                            'PERSONNEL',
                                            'CUMULATIVE_IMPACTS',
                                            'IMPACT_INFO',
                                            'NATURE',
                                            'SCIENCE',
                                            'PREDICTION',
                                            'IMPLEMENTATION',
                                            'METHODS',
                                            'INTERNAL_SUPPORT',
                                            'RESOURCES_CAPACITY',
                                            'LEGISLATION',
                                            'OTHER',
                                            'COMMUNICATION_COLLABORATION',
                                            'SUCCESS_CRITERIA',
                                            'CLIMATE_CHANGE',
                                            'SPECIES_DATA',
                                            'EXTERNAL_SUPPORT',
                                            'POLITICS',
                                            'DATA',
                                            'FUNDING'
                                  )))) + 
            geom_bar(position = "stack", stat = "identity",
                     fill = "#F4A261") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 10)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("Rank 4") +
            ylim(0,35) +
            scale_x_discrete(labels = c('COSTS' = "Costs",
                                        'SITE_INFO' = "Site Info",
                                        'JUSTICE' = 'Justice',
                                        'CONSENSUS' = 'Consensus',
                                        'ECOLOGY' = 'Ecology',
                                        'RESPONSIBILITIES' = 'Responsibilities',
                                        'EXPERTISE' = 'Expertise',
                                        'SOCPOLSCI_SYSTEMS' = 'Soc-Poli-Sci Systems',
                                        'TIMELINES' = 'Timelines',
                                        'INDIGENOUS_RIGHTS' = 'Indigenous Rights',
                                        'ACCESS_TO_EVIDENCE' = 'Access to evidence',
                                        'LONGTERM_DATA' = 'Long-term data',
                                        'TRADEOFFS' = 'Tradeoffs',
                                        'LAND_ACCESS' = 'Land access',
                                        'PERSONNEL' = 'Personnel',
                                        'CUMULATIVE_IMPACTS' = 'Cumulative impacts',
                                        'IMPACT_INFO' = 'Impact info',
                                        'NATURE' = 'Nature',
                                        'SCIENCE' = 'Science',
                                        'PREDICTION' = 'Prediction',
                                        'IMPLEMENTATION' = 'Implementation',
                                        'METHODS' = 'Methods',
                                        'INTERNAL_SUPPORT' = 'Internal support',
                                        'RESOURCES_CAPACITY' = 'Resources/Capacity',
                                        'LEGISLATION' = 'Legislation',
                                        'OTHER' = 'Other',
                                        'COMMUNICATION_COLLABORATION' = 'Communcation/Collaboration',
                                        'SUCCESS_CRITERIA' = 'Success criteria',
                                        'CLIMATE_CHANGE' = 'Climate change',
                                        'SPECIES_DATA' = 'Species data',
                                        'EXTERNAL_SUPPORT' = 'External support',
                                        'POLITICS' = 'Politics',
                                        'DATA' = 'Data',
                                        'FUNDING' = 'Funding'))


rank4

######### Rank 5 ###############
rank5<- q1 %>% select(CODE:RANK5) %>%
            ggplot(aes(y = RANK5,
                       x = factor(CODE,
                                  level = c('COSTS',
                                            'SITE_INFO',
                                            'JUSTICE',
                                            'CONSENSUS',
                                            'ECOLOGY',
                                            'RESPONSIBILITIES',
                                            'EXPERTISE',
                                            'SOCPOLSCI_SYSTEMS',
                                            'TIMELINES',
                                            'INDIGENOUS_RIGHTS',
                                            'ACCESS_TO_EVIDENCE',
                                            'LONGTERM_DATA',
                                            'TRADEOFFS',
                                            'LAND_ACCESS',
                                            'PERSONNEL',
                                            'CUMULATIVE_IMPACTS',
                                            'IMPACT_INFO',
                                            'NATURE',
                                            'SCIENCE',
                                            'PREDICTION',
                                            'IMPLEMENTATION',
                                            'METHODS',
                                            'INTERNAL_SUPPORT',
                                            'RESOURCES_CAPACITY',
                                            'LEGISLATION',
                                            'OTHER',
                                            'COMMUNICATION_COLLABORATION',
                                            'SUCCESS_CRITERIA',
                                            'CLIMATE_CHANGE',
                                            'SPECIES_DATA',
                                            'EXTERNAL_SUPPORT',
                                            'POLITICS',
                                            'DATA',
                                            'FUNDING'
                                  )))) + 
            geom_bar(position = "stack", stat = "identity",
                     fill = "#e76F51") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 10)) +
            ylab("Count") +
            xlab(" ") +
            ggtitle("Rank 5") +
            ylim(0,35) +
            scale_x_discrete(labels = c('COSTS' = "Costs",
                                        'SITE_INFO' = "Site Info",
                                        'JUSTICE' = 'Justice',
                                        'CONSENSUS' = 'Consensus',
                                        'ECOLOGY' = 'Ecology',
                                        'RESPONSIBILITIES' = 'Responsibilities',
                                        'EXPERTISE' = 'Expertise',
                                        'SOCPOLSCI_SYSTEMS' = 'Soc-Poli-Sci Systems',
                                        'TIMELINES' = 'Timelines',
                                        'INDIGENOUS_RIGHTS' = 'Indigenous Rights',
                                        'ACCESS_TO_EVIDENCE' = 'Access to evidence',
                                        'LONGTERM_DATA' = 'Long-term data',
                                        'TRADEOFFS' = 'Tradeoffs',
                                        'LAND_ACCESS' = 'Land access',
                                        'PERSONNEL' = 'Personnel',
                                        'CUMULATIVE_IMPACTS' = 'Cumulative impacts',
                                        'IMPACT_INFO' = 'Impact info',
                                        'NATURE' = 'Nature',
                                        'SCIENCE' = 'Science',
                                        'PREDICTION' = 'Prediction',
                                        'IMPLEMENTATION' = 'Implementation',
                                        'METHODS' = 'Methods',
                                        'INTERNAL_SUPPORT' = 'Internal support',
                                        'RESOURCES_CAPACITY' = 'Resources/Capacity',
                                        'LEGISLATION' = 'Legislation',
                                        'OTHER' = 'Other',
                                        'COMMUNICATION_COLLABORATION' = 'Communcation/Collaboration',
                                        'SUCCESS_CRITERIA' = 'Success criteria',
                                        'CLIMATE_CHANGE' = 'Climate change',
                                        'SPECIES_DATA' = 'Species data',
                                        'EXTERNAL_SUPPORT' = 'External support',
                                        'POLITICS' = 'Politics',
                                        'DATA' = 'Data',
                                        'FUNDING' = 'Funding'))
rank5

# Panel -------------------------------------------------------------------
library(patchwork)

ranks <- rank1 + rank2 + rank3 + rank4 + rank5 +
            plot_layout(ncol = 2)


panel_test <- q1_bar + ranks

ggsave("output/panel_test_all.jpg",
       width = 20,
       height = 14)


ggsave("output/panel_final.jpg",
       width = 20,
       height = 14)


ggsave("output/panel_final.tiff",
       width = 20,
       height = 14)


ggsave("output/panel_final.pdf",
       width = 20,
       height = 14)



# Themes ------------------------------------------------------------------
theme_long <- q1 %>% select(THEME,RANK1:RANK5) %>%
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")

theme_long$THEME <- as.factor(theme_long$THEME)


theme_long$rank <- factor(theme_long$rank, levels = c('RANK5',
                                                'RANK4',
                                                'RANK3',
                                                'RANK2',
                                                'RANK1'))



unique(theme_long$THEME)


# Stacked

colours = c("RANK1" = "#264653",
            "RANK2" = "#2A9D8F",
            "RANK3" = "#E9C46A",
            "RANK4" = "#F4A261",
            "RANK5" = "#e76F51")

q1_sum %>% group_by(THEME) %>% 
            summarise(summed = sum(sum)) %>% 
            arrange(desc(summed))

#   THEME                 summed
# 1 Data                     160
# 2 Resources                129
# 3 Governance               127
# 4 Evidence                 106
# 5 Public_support            60
# 6 Social_complexity         59
# 7 Ecological_complexity     51
# 8 Other                     24


theme_bar <- theme_long %>%
            ggplot(aes(fill = rank, y = count, #label = count,
                       x = factor(THEME,
                                  level = c('Other',
                                 'Ecological_complexity',
                                 'Social_complexity',
                                 'Public_support',
                                 'Evidence',
                                 'Governance',
                                 'Resources',
                                 'Data')))) + 
            geom_bar(position = "stack", stat = "identity") +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 20),
                  axis.title = element_text(size = 20),
                  legend.position = "none",
                  legend.title = element_blank()) +
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('Other',
                                        'Ecological_complexity' = 'Ecological Complexity',
                                        'Social_complexity' = 'Social Complexity',
                                        'Public_support' = 'Public Support',
                                        'Evidence',
                                        'Governance',
                                        'Resources',
                                        'Data')) +
            ggtitle('Themes') 


            # guides(fill = guide_legend(reverse = TRUE,
            #                            label.position = "bottom"))

                  # legend.key.size = unit(0.7, 'cm'),
                  # legend.text = element_text(size = 10),
                  # legend.direction = "horizontal",
                  # legend.title = element_blank()) +
            # geom_text(size = 4, alpha = 1, colour = "white",
            #           position = position_stack(vjust = 0.55)) +
         
             



### Resources ranks #####

resources_long <- q1 %>% filter(THEME == "Resources") %>% 
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


resources_long$rank <- factor(resources_long$rank, levels = c('RANK5',
                                                'RANK4',
                                                'RANK3',
                                                'RANK2',
                                                'RANK1'))

resources <- resources_long %>% 
            ggplot(aes(y = count, fill = rank,
                       x = factor(CODE,
                                  level = c('EXPERTISE',
                                            'TIMELINES',
                                            'PERSONNEL',
                                            'RESOURCES_CAPACITY',
                                            'FUNDING')))) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 18),
                  legend.position = "none") +
            ylab("Count") +
            xlab(" ") +
            ylim(0, 80) + 
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('EXPERTISE' = "Expertise",
                                        'TIMELINES' = "Timelines",
                                        'PERSONNEL' = 'Personnel',
                                        'RESOURCES_CAPACITY' = 'Resources/Capacity',
                                        'FUNDING' = 'Funding')) +
            ggtitle('Resources')




####### Data ######


data_long <- q1 %>% filter(THEME == "Data") %>% 
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


data_long$rank <- factor(data_long$rank, levels = c('RANK5',
                                                              'RANK4',
                                                              'RANK3',
                                                              'RANK2',
                                                              'RANK1'))


unique(data_long$CODE)


data <- data_long %>% 
            ggplot(aes(y = count, fill = rank,
                       x = factor(CODE,
                                  level = c('SITE_INFO',
                                            'LONGTERM_DATA',
                                            'IMPACT_INFO',
                                            'METHODS',
                                            'SPECIES_DATA',
                                            'DATA')))) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            ylim(0, 80) + 
            theme(axis.text = element_text(size = 18),
                  legend.position = "none") +
            ylab("Count") +
            xlab(" ") +
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('SITE_INFO' = 'Site',
                                        'LONGTERM_DATA' = 'Longterm',
                                        'IMPACT_INFO' = 'Impact',
                                        'METHODS' = 'Methods',
                                        'SPECIES_DATA' = 'Species',
                                        'DATA' = 'Data')) +
            ggtitle('Data')



unique(q1$THEME)

####### Evidence ######


evidence_long <- q1 %>% filter(THEME == "Evidence") %>% 
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


evidence_long$rank <- factor(evidence_long$rank, levels = c('RANK5',
                                                    'RANK4',
                                                    'RANK3',
                                                    'RANK2',
                                                    'RANK1'))

unique(evidence_long$CODE)

evidence <- evidence_long %>% 
            ggplot(aes(y = count, fill = rank,
                       x = factor(CODE,
                                  level = c('CONSENSUS',
                                            'ACCESS_TO_EVIDENCE',
                                            'CUMULATIVE_IMPACTS',
                                            'SCIENCE',
                                            'PREDICTION',
                                            'IMPLEMENTATION',
                                            'SUCCESS_CRITERIA')))) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            ylim(0, 80) + 
            theme(axis.text = element_text(size = 18),
                  legend.position = "none") +
            ylab("Count") +
            xlab(" ") +
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('CONSENSUS' = 'Consensus',
                                        'ACCESS_TO_EVIDENCE' = ' Access',
                                        'CUMULATIVE_IMPACTS' = 'Cumulative',
                                        'SCIENCE' = 'Science',
                                        'PREDICTION' = 'Prediction',
                                        'IMPLEMENTATION' = 'Implementation',
                                        'SUCCESS_CRITERIA' = 'Success criteria')) +
            ggtitle("Evidence")


# Governance  ---------------------------------------------------

governance_long <- q1 %>% filter(THEME == "Governance") %>% 
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


governance_long$rank <- factor(governance_long$rank, levels = c('RANK5',
                                                            'RANK4',
                                                            'RANK3',
                                                            'RANK2',
                                                            'RANK1'))
governance <- governance_long %>% 
            ggplot(aes(y = count, fill = rank,
                       x = factor(CODE))) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 18),
                  legend.position = "none") +
            ylab("Count") +
            ylim(0, 80) + 
            xlab(" ") +
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('POLITICS' = 'Politics',
                                        'LEGISLATION' = 'Legislation',
                                        'INTERNAL_SUPPORT' = 'Internal Support',
                                        'INDIGENOUS_RIGHTS' = 'Indigenous Rights')) +
            ggtitle("Governance")

# Ecological complexity ---------------------------------------------------

ecological_long <- q1 %>% filter(THEME == "Ecological_complexity") %>% 
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


ecological_long$rank <- factor(ecological_long$rank, levels = c('RANK5',
                                                                'RANK4',
                                                                'RANK3',
                                                                'RANK2',
                                                                'RANK1'))
ecological <- ecological_long %>% 
            ggplot(aes(y = count, fill = rank,
                       x = factor(CODE,
                                  level = c('ECOLOGY',
                                            'NATURE',
                                            'CLIMATE_CHANGE')))) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 11),
                  legend.position = "none") +
            ylab("Count") +
            xlab(" ") +
            ylim(0, 80) + 
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('ECOLOGY' = 'Ecology',
                                        'NATURE' = 'Nature',
                                        'CLIMATE_CHANGE' = 'Climate change')) +
            ggtitle("Ecological Complexity")


# Social complexity  ---------------------------------------------------

social_long <- q1 %>% filter(THEME == "Social_complexity") %>% 
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


social_long$rank <- factor(social_long$rank, levels = c('RANK5',
                                                                'RANK4',
                                                                'RANK3',
                                                                'RANK2',
                                                                'RANK1'))
social <- social_long %>% 
            ggplot(aes(y = count, fill = rank,
                       x = factor(CODE,
                                  level = c('COSTS',
                                            'JUSTICE',
                                            'RESPONSIBILITIES',
                                            'SOCPOLSCI_SYSTEMS',
                                            'TRADEOFFS',
                                            'COMMUNICATION_COLLABORATION')))) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 11),
                  legend.position = "none") +
            ylab("Count") +
            xlab(" ") +
            ylim(0, 80) + 
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('COSTS' = 'Costs',
                                        'JUSTICE' = 'Justice',
                                        'RESPONSIBILITIES' = 'Responsibilities',
                                        'SOCPOLSCI_SYSTEMS' = 'System',
                                        'TRADEOFFS' = 'Tradeoffs',
                                        'COMMUNICATION_COLLABORATION' = 'Communication/Collaboration')) +
            ggtitle("Social Complexity")

# Public support  ---------------------------------------------------

public_long <- q1 %>% filter(THEME == "Public_support") %>% 
            pivot_longer(RANK1:RANK5,
                         names_to = "rank",
                         values_to = "count")


public_long$rank <- factor(public_long$rank, levels = c('RANK5',
                                                        'RANK4',
                                                        'RANK3',
                                                        'RANK2',
                                                        'RANK1'))
public <- public_long %>% 
            ggplot(aes(y = count, fill = rank,
                       x = factor(CODE,
                                  level = c( 'LAND_ACCESS',
                                             'EXTERNAL_SUPPORT')))) + 
            geom_bar(position = "stack", stat = "identity") +
            theme_light() +
            coord_flip() +
            theme_minimal() +
            theme(axis.text = element_text(size = 11),
                  legend.position = "none") +
            ylab("Count") +
            xlab(" ") +
            ylim(0, 80) + 
            scale_fill_manual(values = colours) +
            scale_x_discrete(labels = c('LAND_ACCESS' = 'Land access',
                                         'EXTERNAL_SUPPORT' = 'External support')) +
            ggtitle('Public Support')

# Panel  ------------------------------------------------------------------

theme_indv <- data + resources + governance + 
            evidence + public + social +
            ecological +
            plot_layout(ncol = 2)

layout <- "
AABBBB
AABBBB
"

panel_theme <- theme_bar + theme_indv + 
            plot_layout(design = layout)

ggsave("output/panel_theme_final.pdf",
       width = 20,
       height = 14)


ggsave("output/panel_theme_final.tiff")

ggsave("output/panel_theme_final.jpg",
       width = 20,
       height = 14)

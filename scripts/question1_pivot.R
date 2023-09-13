library(tidyverse)
library(fmsb)
library(forcats)

q1 <- read.csv("data/question1_28jun23.csv")

q1_sum <- q1
q1_sum$sum <- rowSums(q1[,2:6])

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

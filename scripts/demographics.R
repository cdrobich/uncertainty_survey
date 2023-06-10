library(tidyverse)


data <- read.csv("data/survey_likerts.csv")
colnames(data)
str(data)

data$organization <- recode_factor(data$organization,
                                   "Non-government agency (e.g., Nature Conservancy Canada, Ducks Unlimited Canada)" = "Non-government Agency",
                                   "Canadian provincial or territorial government agency" = "Provincial/Territorial Government",
                                   "Canadian federal government agency" = "Federal Government")




orgs <- data %>% count(organization)
orgs <- orgs[-8,]
colnames(orgs)



orgs %>% mutate(organization = fct_reorder(organization,
                                           n, .desc = FALSE)) %>% 
            ggplot() + 
            geom_segment( aes(x = organization, xend = organization, 
                              y = 0, yend = n),
                colour = "#e76f51", lwd = 5) +
  geom_point(aes(x = organization, y = n), colour = "#e76f51",
             fill = "white", size=7, shape = 19, stroke = 1.5) +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 17),
                  axis.title = element_text(size = 17),
                  #legend.position = "none",
                  legend.key.size = unit(1, 'cm'),
                  legend.text = element_text(size = 12),
                  #legend.direction = "horizontal",
                  legend.title = element_blank()) +
            ylim(0, 50)



### location

location <- data %>% select(location)

location <- location %>% separate_longer_delim(location, delim = ",")

loc <- location %>% count(location)
loc <- loc[2:15,]
write.csv(loc, "location.csv")



loc %>% mutate(location = fct_reorder(location,
                                           n, .desc = FALSE)) %>% 
            ggplot() + 
            geom_segment( aes(x = location, xend = location, 
                              y = 0, yend = n),
                          colour = "#2a9d8f", lwd = 4) +
            geom_point(aes(x = location, y = n), colour = "#2a9d8f",
                       fill = "white", size=6, shape = 19, stroke = 1.5) +
            coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() +
            theme(axis.text = element_text(size = 18),
                  axis.title = element_text(size = 18),
                  #legend.position = "none",
                  legend.key.size = unit(1, 'cm'),
                  legend.text = element_text(size = 12),
                  #legend.direction = "horizontal",
                  legend.title = element_blank()) +
            ylim(0, 70)


colnames(data)

collab_within_org <- data %>% group_by(organization) %>% 
            count(collaborative_within)

collab_within_org <- collab_within_org[-27,]

collab_within_org %>% ggplot(aes(fill = collaborative_within, y = n, 
                                 label = n,
                         x = organization)) + 
            geom_bar(position = "dodge", stat = "identity") +
            #coord_flip() +
            xlab(" ") +
            ylab("Count") +
            theme_minimal() 
            # theme(axis.text = element_text(size = 20),
            #       axis.title = element_text(size = 20),
            #       #legend.position = "none",
            #       legend.key.size = unit(1, 'cm'),
            #       legend.text = element_text(size = 12),
            #       #legend.direction = "horizontal",
            #       legend.title = element_blank()) 
            # geom_text(size = 10, alpha = 1, colour = "white",
            #           position = position_stack(vjust = 0.55)) +
            # guides(fill = guide_legend(reverse = TRUE,
            #                            label.position = "bottom")) +
            # scale_fill_manual(values = colours)










collab_within_org <- collab_within_org[5:31,]
















collab_outside_org <- data %>% group_by(organization) %>% 
            count(collaborative_outside)


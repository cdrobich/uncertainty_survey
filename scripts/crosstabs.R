########## crosstabs

library(tidyverse)


# Q4: What is your typical level of decision-making autonomy in your professional role? 

# One (1) would be "no autonomy - not involved in the decision-making process" 
# and five (5) would be "full autonomy - can make decisions without permission or approval".

autonomy <- read.csv("data/crosstab_autonomy.csv")

colnames(autonomy)

autonomy_total <- autonomy %>% select(response:total)
autonomy_total <- autonomy_total[-1,]

autonomy_total$response <- recode_factor(autonomy_total$response,
                                         "one" = "One (no autonomy)",
                                         "five" = "Five (total autonomy)",
                                         "two" = 'Two',
                                         "three" = "Three",
                                         "four" = "Four")
unique(autonomy_total$response)


autonomy_total$response <- factor(autonomy_total$response,
                             levels = c("Five (total autonomy)",
                                        "Four",
                                        "Three",
                                        "Two",
                                        "One (no autonomy)"))


autonomy <- autonomy_total %>% ggplot(aes(x = response, y = total)) +
            geom_col(fill = "#2a9d8f") +
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


install.packages("tidyverse")
install.packages("tidycensus")

library(tidyverse)
library(tidycensus)
census_api_key("333defeaa07a64405f08c53e8a88b2e424222f1f", overwrite = TRUE)

vars_2022 <- load_variables(2022, "acs5")
View(vars_2022)

#AI_URL: https://www.perplexity.ai/search/so-i-am-doing-an-assignment-ch-kOS3Bc3ESsqDoa7wVgm79g

#Median household income of Yolo and Alameda

#Yolo 
yolo_median_income <- get_acs(
  geography = "county",
  variables = c("median_income" =  "B19013_001"),  
  year = 2022,
  state = "CA",
  county = "Yolo"
) %>%
  mutate(
    low_income = estimate * .8,
    high_income = estimate * 1.2
  )
  
yolo_median_income

#Alameda
alameda_median_income <- get_acs(
  geography = "county",
  variables = c("median_income" =  "B19013_001"), 
  year = 2022,
  state = "CA",
  county = "Alameda"
) %>%
  mutate(
    low_income = estimate * .8,
    high_income = estimate * 1.2
  )

alameda_median_income

############################

#Chosen variables and organize main data
chosen_var <- c(
  "total_owners" = "B25045_002",
  "owners_no_vehicle" = "B25045_003",
  "total_renters" = "B25045_011",
  "renters_no_vehicle" = "B25045_012"
)
chosen_var

#Yolo data
yolo_data <- get_acs(
  geography = "county",
  variables = chosen_var,
  year = 2022,
  state= "CA",
  county = ("Yolo")
)
yolo_data

#Alameda data
alameda_data <- get_acs(
  geography = "county",
  variables = chosen_var,
  year = 2022,
  state= "CA",
  county = ("Alameda")
)
alameda_data

#California State data
ca_data <- get_acs(
    geography = "state",
    variables = chosen_var,
    year = 2022,
    state= "CA"
)
ca_data 

#####################################################
#B25045_003/B25045_002 #house no car
#B25045_012/B25045_011 #rent no car
#Do ami from week 2&3

#Yolo
yolo_process <- yolo_data %>%
  select(-moe) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate) %>%
  mutate(
    percent_owners_no_vehicle = (owners_no_vehicle / total_owners) * 100,
    percent_renters_no_vehicle = (renters_no_vehicle / total_renters) * 100, 
    county = "Yolo",
    NAME = str_remove(NAME, " County, California"),
    state = "California"
  )
yolo_process


#Alameda
alameda_process <- alameda_data %>%
  select(-moe) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate) %>%
  mutate(
    percent_owners_no_vehicle = (owners_no_vehicle / total_owners) * 100,
    percent_renters_no_vehicle = (renters_no_vehicle / total_renters) * 100,
  county = "Alameda",
  NAME = str_remove(NAME, " County, California"),
  state = "California"
  )
alameda_process

#California

ca_process <- ca_data %>%
  select(-moe) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate) %>%
  mutate(
    percent_owners_no_vehicle = (owners_no_vehicle / total_owners) * 100,
    percent_renters_no_vehicle = (renters_no_vehicle / total_renters) * 100,
    NAME = "California",
    county = NA_character_,
    state = "California"
  )
ca_process

#####################
#Combine the rows, overall data

combined_data <- bind_rows(
  yolo_process, alameda_process, ca_process) %>%
  select(GEOID, NAME, state, county, everything())


combined_data

knitr::kable(combined_data,
             col.names = c("GEOID", "Location", "State", "County", 
                           "Total Owners", "Owners w/o Vehicle", 
                           "Total Renters", "Renter w/o Vehicle", 
                           "% Owners w/o Vehicle", "% Renters w/o Vehicle"),
             align = c("l", "l", "l", "l", "c", "c", "c", "c", "c", "c"),
             digits = 1)

#############################

viz_data <- combined_data %>%
  pivot_longer(
    cols = c(percent_owners_no_vehicle, percent_renters_no_vehicle),
    names_to = "tenure_type",
    values_to = "percentage") %>%
  mutate(
    tenure_type = case_when(
      tenure_type == "percent_owners_no_vehicle" ~ "Owners",
      tenure_type == "percent_renters_no_vehicle" ~ "Renters"
    ),
    NAME = factor(NAME, levels = c("Yolo", "Alameda", "California"))
  )
viz_data
#VISUAL 1: Combined bar chart
vehicle_plot <- ggplot(viz_data, 
                       aes(x = NAME, y = percentage, fill = tenure_type)) +
  geom_col(position = position_dodge2(preserve = "single"), width = 0.7) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3.5, color = "black") +
  labs(title = "Vehicle Access by Tenure Type (2022)",
       subtitle = "Yolo County vs. Alameda County vs. California State Average",
       y = "Percentage Without Vehicles",
       x = "",
       fill = "Household Type") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        legend.position = "top") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

print(vehicle_plot)

# Visual 2: Horizontal Bars
horizontal_plot <- ggplot(viz_data, 
                          aes(x = percentage, y = reorder(NAME, percentage), fill = tenure_type)) +
  geom_col(position = position_dodge2(reverse = TRUE), width = 0.7) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_dodge2(width = 0.7, reverse = TRUE),
            hjust = -0.1, size = 3.5, color = "black") +
  labs(title = "Vehicle Access Comparison",
       subtitle = "Sorted by Percentage Without Vehicles",
       x = "Percentage Without Vehicles",
       y = "",
       fill = "Household Type") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

print(horizontal_plot)


# Visual 3: Scatter Plot 
scatter_plot <- ggplot(combined_data, 
                       aes(x = percent_owners_no_vehicle, 
                           y = percent_renters_no_vehicle,
                           color = NAME)) +
  geom_point(size = 8, alpha = 0.8) +
  geom_text(aes(label = paste0(round(percent_owners_no_vehicle,1), "%/", 
                               round(percent_renters_no_vehicle,1), "%")),
            color = "white", size = 3.2) +
  labs(title = "Vehicle Access Disparities",
       subtitle = "Owners vs. Renters Without Vehicles",
       x = "Owners Without Vehicles (%)",
       y = "Renters Without Vehicles (%)",
       color = "Jurisdiction") +
  theme_minimal() +
  scale_color_manual(values = c("#2ca02c", "#9467bd", "#d62728")) +
  theme(panel.grid.major = element_line(color = "grey90"),
        legend.position = "top") +
  scale_x_continuous(limits = c(2, 4)) +
  scale_y_continuous(limits = c(10, 17))

print(scatter_plot)













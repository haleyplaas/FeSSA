setwd("Q:\\My Drive\\Code Repositories\\R\\FeSSA")
rm(list = ls())
library(httr);library(jsonlite);library(dplyr);library(ggplot2);library(readxl);library(tidyr);library(patchwork)
#install.packages("")

data.file <- "Q:\\My Drive\\Code Repositories\\R\\FeSSA\\data files\\Fe-SSA_Plaas.xlsx"

FeSSA_with_shipping <- read_excel(data.file, sheet = "with_shipping")
FeSSA_without_shipping <- read_excel(data.file, sheet = "without_shipping")

# Calculate the averages of the columns
averages <- FeSSA_with_shipping %>%
  summarise(across(starts_with("FE"), mean, na.rm = TRUE)) %>% 
  select(-FESUM)

# Reshape the data to long format for ggplot
averages_long <- tidyr::pivot_longer(averages, cols = starts_with("FE"),
                                     names_to = "Category", values_to = "Average")

# Plot the stacked barplot
with_shipping <- ggplot() +
  geom_bar(data = filter(averages_long, Category != "FETOTSRF"),
           aes(x = "Category", y = Average, fill = Category),
           stat = "identity", position = "stack") +
  geom_bar(data = filter(averages_long, Category == "FETOTSRF"),
           aes(x = "FETOTSRF", y = Average, fill = Category),
           stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = NULL, y = "Average Fe conc. in aerosol (kg/kg)", title = "Fe in the Arctic (Nov-Dec 2018) W/ Shipping") +
  theme(axis.text.x = element_blank(), legend.position = "bottom")

# Calculate the averages of the columns
averages <- FeSSA_without_shipping %>%
  summarise(across(starts_with("FE"), mean, na.rm = TRUE)) %>% 
  select(-FESUM)

# Reshape the data to long format for ggplot
averages_long <- tidyr::pivot_longer(averages, cols = starts_with("FE"),
                                     names_to = "Category", values_to = "Average")

# Plot the stacked barplot
without_shipping <- ggplot() +
  geom_bar(data = filter(averages_long, Category != "FETOTSRF"),
           aes(x = "Category", y = Average, fill = Category),
           stat = "identity", position = "stack") +
  geom_bar(data = filter(averages_long, Category == "FETOTSRF"),
           aes(x = "FETOTSRF", y = Average, fill = Category),
           stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = NULL, y = "Average Fe conc. in aerosol (kg/kg)", title = "Fe in the Arctic (Nov-Dec 2018) W/out Shipping") +
  theme(axis.text.x = element_blank(), legend.position = "bottom")

with_shipping + without_shipping

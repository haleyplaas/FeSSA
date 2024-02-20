setwd("Q:\\My Drive\\Code Repositories\\R\\FeSSA")
rm(list = ls())
library(httr);library(jsonlite);library(dplyr);library(ggplot2);library(readxl);library(tidyr);library(patchwork)
#install.packages("")

# Reading in the data ----------------------------------------------------------------
data.file <- "Q:\\My Drive\\Code Repositories\\R\\FeSSA\\data files\\Fe-SSA_Plaas.xlsx"

FeSSA_with_shipping <- read_excel(data.file, sheet = "with_shipping")
FeSSA_without_shipping <- read_excel(data.file, sheet = "without_shipping")


# Visualizing the model (MIMI) outputs -----------------------------------------------

# Calculate the averages of the columns
averages <- FeSSA_with_shipping %>%
  summarise(across(starts_with("FE"), mean, na.rm = TRUE)) %>% 
  select(-FESUM, -FEAGEDSSA, -FEFRESHSSA)

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
  labs(x = NULL, y = "Average Fe conc. in aerosol (ug/m3)", title = "Fe in the Arctic (Nov-Dec 2018) W/ Shipping") +
  theme(axis.text.x = element_blank(), legend.position = "bottom")

# Calculate the averages of the columns
averages <- FeSSA_without_shipping %>%
  summarise(across(starts_with("FE"), mean, na.rm = TRUE)) %>% 
  select(-FESUM, -FEAGEDSSA, -FEFRESHSSA)

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
  labs(x = NULL, y = "Average Fe conc. in aerosol (ug/m3)", title = "Fe in the Arctic (Nov-Dec 2018) W/out Shipping") +
  theme(axis.text.x = element_blank(), legend.position = "bottom")

with_shipping + without_shipping


# Comparing the measurements to the model outputs ---------------------------------------------
# Selecting columns of interest and log transforming the numerical ones
model_obs_comparison <- FeSSA_with_shipping %>% 
                        mutate(FEMOD_FESSA = FEFRESHSSA + FETOTSRF) %>%  
                        mutate_at(vars(-DATE), log10) %>%
                        select(-FESUM) 
  
# Convert 'DATE' column to Date format
model_obs_comparison$DATE <- as.Date(model_obs_comparison$DATE)

# Reshape the data to long format
model_obs_long <- tidyr::pivot_longer(model_obs_comparison, cols = -DATE,
                                      names_to = "Variable", values_to = "Value")
                              

key_mod_obs <- model_obs_long %>% filter(Variable == "FEAGEDSSA" | Variable == "FETOTSRF")

# Plot the time series with log10-transformed data and adjusted y-axis scale
all.aerosol.types <- 
  ggplot(model_obs_long, aes(x = DATE, y = Value, color = Variable)) +
  geom_line(size = 1.5) +
  labs(x = "Date", y = "Log10(ug m-3)", color = "Variable") +
  theme_minimal()
all.aerosol.types

key.aerosol.types <- 
  ggplot(key_mod_obs, aes(x = DATE, y = Value, color = Variable)) +
  geom_line(size = 1.5) +
  labs(x = "Date", y = "Log10(ug m-3)", color = "Variable") +
  theme_minimal()
key.aerosol.types

#for comparing two at a time
#ggplot(model_obs_comparison, aes(x = DATE)) +
  #geom_line(aes(y = FEBBTOTSRF, color = "FEBBTOTSRF"), size=1.5) +
  #geom_line(aes(y = FEAGEDSSA, color = "FEAGEDSSA"), size=1.5) +
  #labs(x = "Date", y = "Log Transformed Value", color = "Variable") +
  #scale_color_manual(values = c("FEBBTOTSRF" = "blue", "FEAGEDSSA" = "red")) +
  #theme_minimal() +
  #labs(title = "Biomass Burning")


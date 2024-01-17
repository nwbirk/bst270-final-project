# Test run for the final project in BST 270

# Import dataset and load packages
sports <- read.csv("~/Downloads/sports-political-donations.csv")
library(tidyverse)


# See here for original article
# https://fivethirtyeight.com/features/inside-the-political-donation-history-of-wealthy-sports-owners/


# Recreate figure 1
# First need to process the donation amount by removing $ and , symbols
sports$donate_amount <- str_replace(sports$Amount, "\\$", "")
sports$donate_amount <- str_replace(sports$donate_amount, "\\,", "")
sports$donate_amount <- str_replace(sports$donate_amount, "\\,", "")
# Check to see if any remaining characters to remove
table(sports$donate_amount)
# Convert from character to numeric
sports$donate_amount <- as.numeric(sports$donate_amount)
# Need to summarize by party, year to get figure values
sports_summary <- sports %>% group_by(Election.Year, Party) %>%
  summarize(total_donated = sum(donate_amount))

# We can see from inspecting this summary table that there are some extra categories
sports_summary

# Can consider creating an even smaller summary dataset that includes the proportion
# Calculated as amount / total
sports_annual_totals <- sports_summary %>% group_by(Election.Year) %>%
  summarize(annual_total = sum(total_donated))
# Join these values as new column and then create proportion column
sports_summary <- inner_join(sports_summary, sports_annual_totals)
sports_summary <- sports_summary %>% mutate(donate_prop = total_donated/annual_total)

# We can now try to plot the results
sports_summary %>% filter(Party %in% c("Republican", "Democrat")) %>% 
  ggplot(aes(Election.Year, donate_prop, fill = Party)) + geom_bar(stat = "identity")


# Recreate Table "MLB owners have donated the most"
# We will reuse the donate_amount variable that we created previously
# Similar summary table needed as before, but split by league instead of year
sports_summary2 <- sports %>% group_by(League, Party) %>%
  summarize(total_donated = sum(donate_amount))

# Can inspect this table and compare totals
sports_summary2 
# We can see that there are some additional categories we don't have, where there are multiple leagues
# Note that adding all the strictly democrat rows, rather than bipartisan but lean democrat
# among the 3 categories that include "MLB" achieves 4466729+289900+427975 = 5184604
# This value matches the table of interest!
# Note that a challenge will be matching NBA as distinct from WNBA

# Need to think about a method for double counting the appropriate rows when generating the table
# Could a custom function be the best approach?





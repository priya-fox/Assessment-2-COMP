library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)

# Read csv files 

users = read.csv("users.csv")
reviews = read.csv("reviews.csv")

# Join users with reviews 

users_reviews <- reviews %>% left_join(users %>%
                                         select(user_id, review_count, average_stars, member_since), by = "user_id")
# Sort users into groups based on membership start date
users_reviews <- users_reviews %>%
  mutate(member_groups = case_when(
    member_since < as.Date("2017-01-01") ~ "Veteran",
    member_since >= as.Date("2017-01-01") & member_since < "2022-01-01" ~ "Intermediate",
    member_since >= as.Date("2022-01-01") ~ "New"
  )
) 

head(users_reviews)

# Handle NA values 
clean_users_reviews <- users_reviews %>% 
  filter(!is.na(member_groups), !is.na(average_stars), !is.na(review_count))

# Count users 
user_counts <- clean_users_reviews %>%
  filter(!is.na(member_groups), !is.na(user_id)) %>%
  group_by(member_groups) %>%
  summarise(user_counts = n_distinct(user_id)) %>%
  arrange(match(member_groups, c("Veteran", "Intermediate", "New")))

print(user_counts)

# Summarise data and handle NA values again
# Number of unique users
# Average Stars 
# Average Reviews 
summary_table <- clean_users_reviews %>%
  group_by(member_groups) %>%
  summarise(
    users_num = n_distinct(user_id),
    Avg_stars = round(mean(average_stars, na.rm = TRUE),2),
    Avg_Reviews = round(mean(review_count, na.rm = TRUE), 2)
  ) %>%
  arrange(match(member_groups, c("Veteran", "Intermediate", "New")))

# Tabulate users, average stars and review counts.
summary_table %>%
  kable("html", caption = "User Review Behaviour by Group") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

colnames(summary_table) # check column names for ggplot

# Create ggplot of average stars in each group
ggplot(summary_table, aes(x=member_groups, 
                          y = Avg_stars, 
                          fill= member_groups))+
  geom_bar(stat = "identity", width = 0.6) +  
  labs(title = "Each Groups Average Stars", 
       x = "User Group", 
       y = "Average Stars") +
  theme_minimal()







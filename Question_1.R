library(tidyverse)
library(knitr)
library(kableExtra)
library(ggplot2)

# Read csv files 

users = read.csv("users.csv")
reviews = read.csv("reviews.csv")

# Join users with reviews 

####Q1

users <- read.csv("users.csv")
reviews <- read.csv("reviews.csv")

by_user_id <- reviews %>% left_join(users %>% select(user_id, member_since), by = "user_id")
head(by_user_id)

# Convert member_since to dates and remove NAs
review_clean <- by_user_id %>% 
  mutate(member_since = as.Date(member_since)) %>%
  filter(!is.na(member_since), !is.na(user_id), !is.na(stars), !is.na(review_id))

review_clean <- review_clean %>% 
  mutate(member_group = case_when(
    member_since < as.Date("2017-01-01") ~ "Veteran",
    member_since >= as.Date("2017-01-01") & member_since < as.Date("2022-01-01") ~ "Intermediate",
    member_since >= as.Date("2022-01-01") ~ "New"
  ))

# Summary by member group 
summary_table <- review_clean %>%
  group_by(member_group) %>%
  summarise(
    user_count = n_distinct(user_id),
    review_count = n_distinct(review_id),  # Count unique reviews
    avg_review_stars = round(mean(stars, na.rm = TRUE), 2),
    avg_reviews_per_user = round(n_distinct(review_id) / n_distinct(user_id), 2)
  ) %>%
  arrange(match(member_group, c("Veteran", "Intermediate", "New")))

summary_table %>%
  kable(caption = "User Review Behaviour by Member Group") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))

ggplot(summary_table, aes(x = member_group, y = avg_review_stars, fill = member_group)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Average Review Stars by User Group",
       x = "User Group",
       y = "Average Stars") +
  theme_minimal() +
  theme(legend.position = "none")







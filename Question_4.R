####Q4

# Join users and reviews by user_id again

user_review <- reviews %>% 
  left_join(users %>% select(user_id, member_since), by = "user_id")

head(user_review)

# Handle NAs
# Create Groups 

clean_reviews <- user_review %>%
  filter(!is.na(member_since), !is.na(stars), !is.na(text)) %>%
  mutate(member_since = as.Date(member_since),
         user_group = case_when(
           member_since < as.Date("2020-01-01") ~ "Before 2020",
           member_since >= as.Date("2020-01-01") ~ "After 2020",
           TRUE ~ NA_character_
         )) %>%
  filter(!is.na(user_group))

# Find review length
clean_reviews <- clean_reviews %>%
  mutate(review_length = nchar(text))

# Summarize review behavior by groups < 2020, => 2020
summary_table <- clean_reviews %>%
  group_by(user_group) %>%
  summarise(
    avg_stars = round(mean(stars, na.rm = TRUE), 2),
    avg_review_length = round(mean(review_length, na.rm = TRUE), 2),
    total_reviews = n()
  )

# Table 
kable(summary_table, caption = "Review Behaviour Between Groups")

ggplot(summary_table, aes(x = user_group, y = avg_review_length, fill = user_group)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(title = "Average Review Length by User Group",
       x = "User Group",
       y = "Average Review Length (Characters)") +
  theme_minimal()

# Check review length to check above code 

clean_reviews %>%
  select(user_id, review_id, text, review_length) %>%
  slice_head(n = 10)

clean_reviews %>%
  summarise(
    min_length = min(review_length),
    max_length = max(review_length),
    avg_length = mean(review_length)
  )

# avg_length = 59.03


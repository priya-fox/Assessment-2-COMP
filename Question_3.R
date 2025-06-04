####Q3

clean_reviews <- reviews %>%
  filter(!is.na(user_id), !is.na(review_id), !is.na(stars))

user_summary <- clean_reviews %>%
  group_by(user_id) %>%
  summarise(
    total_reviews = n_distinct(review_id),
    avg_stars = round(mean(stars, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(total_reviews))

# 3. Get top 10 users by review count
top_users <- user_summary %>%
  slice_max(total_reviews, n = 10)

top_users %>% 
  kable(caption = "Top 10 Users by Review Count")

# Some users have similar total reviews, so more than top 10. 
# the top column showing 5829 and avg_stars is an error. 
# unable to resolve at this time. 

# ggPlot

ggplot(user_summary, aes(x = user_id, y = avg_stars, fill = user_id)) +
  geom_boxplot() +
  labs(title = "Top 10 Users Average Stars",
       x = "Users", y = "Average Star Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

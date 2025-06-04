####Q2 

reviews <- read.csv("reviews.csv")
pga <- read.csv("businessesPGA.csv")
pgb <- read.csv("businessesPGB.csv")

# 1. BusinessPGA
pga_reviews <- reviews %>%
  inner_join(pga %>% select(business_id, state), by = "business_id") %>%
  filter(!is.na(state), state != "", !is.na(stars), !is.na(user_id), !is.na(review_id))

# state != "" Get rid of missing values

head(pga_reviews)

pga_summary <- pga_reviews %>%
  group_by(state) %>%
  summarise(
    avg_stars = round(mean(stars, na.rm = TRUE), 2),
    total_reviews = n_distinct(review_id),
    unique_users = n_distinct(user_id)
  ) %>%
  arrange(desc(unique_users))


# Print the summary table nicely
pga_summary %>%
  kable(caption = "Review Summary by State") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Visualise Unique Users by State - BusinessPGA
ggplot(pga_summary, aes(x = reorder(state, -unique_users), y = unique_users)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Unique Users by State - BusinessPGA",
       x = "State", y = "Number of Unique Users") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Business PGB

# 2. Business PGB
pgb_reviews <- reviews %>%
  inner_join(pgb %>% select(business_id, state), by = "business_id") %>%
  filter(!is.na(state), state != "", !is.na(stars), !is.na(user_id), !is.na(review_id))

pgb_summary <- pgb_reviews %>%
  group_by(state) %>%
  summarise(
    avg_stars = round(mean(stars, na.rm = TRUE), 2),
    total_reviews = n_distinct(review_id),
    unique_users = n_distinct(user_id)
  ) %>%
  arrange(desc(unique_users))

pgb_summary %>%
  kable(caption = "Review Summary by State") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Visualise Unique Users by State - BusinessPGB
ggplot(pgb_summary, aes(x = reorder(state, -unique_users), y = unique_users)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Unique Users by State - BusinessPGB",
       x = "State", y = "Number of Unique Users") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Elaborate on the finding. Is there any difference between the 2 datasets?
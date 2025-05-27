## Q1

#Step 1: Load libraries 

library(ggplot2) # plots
library(kableExtra) # tables 


# Step 2: Read csv file 
users <- read.csv("users.csv")
head(users)

#Step 3: Remove NAs/missing values
users_clean <- users[!is.na(users$member_since) & !is.na(users$review_count) &!is.na(users$average_stars), ]

#Step 4: Categories depending on user joining date 
users_clean$user_group <- ifelse(
  users_clean$member_since < "2017-01-01", "Veteran", # joind before 2017
  ifelse(
    users_clean$member_since > "2017-01-01" & users_clean$member_since < "2022-12-31", "Intermediate", # joined between 2017-2022
  "New" # joined after 2022
)) 
  

# Summarizing data by user group
# - calculate number of users
# - average star rating
# - average number of reviews 

#Step 4 User count 
user_count <- table(users_clean$user_group) # Calculate users in each group
user_count
# Intermediate          New      Veteran 
#  22653                8401         7747 


#r code all 

summary_table <- aggregate(cbind(review_count, average_stars) ~ user_group, data = users_clean, 
                           FUN = function(x) c(Avg_Reviews = mean(x, na.rm = TRUE), Avg_Stars = mean(x, na.rm = TRUE)))
# FUN = function(x) means applied to each groups using cbind. 

summary_table$Avg_Reviews <- round(summary_table$review_count[, "Avg_Reviews"], 2)
summary_table$Avg_Stars <- round(summary_table$average_stars[, "Avg_Stars"], 2)

summary_table <- summary_table[, c("user_group", "Avg_Stars", "Avg_Reviews")]

summary_table %>%
  kbl(caption = "User Review Summary by Group") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

# Avg_reviews and average_stars by user group   
summary_table %>% 
  kbl(caption = "User Reviews by Group") %>%
  kable_styling(bootstrap_options = c("Striped", "hover"), full_width = FALSE)

# Bar Plot for average review stars grouped 
ggplot(summary_table, aes(x = user_group, y = Avg_Stars, fill = user_group)) +
  geom_bar(stat = "identity", width = 0.6) +  # height of bars = average stars
  labs(title = "Average Review Stars by User Group",
       x = "User Group",
       y = "Average Stars") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") # set colors for visualization 
)

# 









## Q1

#Step 1: Load libraries 

library(ggplot2) # plots
library(kableExtra) # tables 


# Step 2: Read csv file 
users <- read.csv("users.csv")
head(users)

#Step 3: Remove NAs/missing values
cleaned_users <- users[!is.na(users$member_since) & !is.na(users$review_count) &!is.na(users$average_stars), ]

#Step 4: Categories depending on user joining date 
cleaned_users$groups <- ifelse(
  cleaned_users$member_since < "2017-01-01", "Veteran", # joined before 2017
  ifelse(
    cleaned_users$member_since > "2017-01-01" & cleaned_users$member_since < "2022-12-31", "Intermediate", # joined between 2017-2022
  "New" # joined after 2022
)) 
  

# Summarizing data by user group
# - calculate number of users
# - average star rating
# - average number of reviews 

#Step 4 User count 
count <- table(cleaned_users$groups) # Calculate users in each group
count
# Intermediate          New      Veteran 
#  22653                8401         7747 

table1 <- aggregate(cbind(review_count, average_stars) ~ groups, 
                    data = cleaned_users, 
                    FUN = function(x) c(Avg_Reviews = mean(x, na.rm = TRUE), 
                                        Avg_Stars = mean(x, na.rm = TRUE)))
# FUN = function(x) means applied to each groups using cbind. 

table1$Avg_Reviews <- round(table1$review_count[, "Avg_Reviews"], 2)
table1$Avg_Stars <- round(table1$average_stars[, "Avg_Stars"], 2)

table1 <- table1[, c("groups", "Avg_Stars", "Avg_Reviews")]
colnames(table1)
# Avg_reviews and average_stars by user group   
table1 %>% 
  kbl(caption = "User Reviews by Group") %>%
  kable_styling(bootstrap_options = c("Striped", "hover"), full_width = FALSE)

# Bar Plot for average review stars grouped 
ggplot(table1, aes(x = groups, y = Avg_Stars, fill = groups)) +
  geom_bar(stat = "identity", width = 0.6) +  # height of bars = average stars
  labs(title = "Each Groups Average Stars",x = "User Group",y = "Average Stars")+
  theme_minimal()











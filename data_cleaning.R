library(dplyr)

d1 <- read.csv("~/Desktop/STAT 3280/Project/Data/student-mat.csv", header=TRUE)
d2 <- read.csv("~/Desktop/STAT 3280/Project/Data/student-por.csv", header=TRUE)

print(colnames(d1))
print(colnames(d2))

# View dimensions of the dataframe 
print(dim(d1))
print(dim(d2))

# Combine d1 and d2
combined_df <- rbind(d1, d2)

# Check for duplicate rows 
duplicate_rows <- combined_df[duplicated(combined_df),]
duplicate_rows # No duplicates 

# Rename columns to be more intuitive 
df <- combined_df 

df <- df %>% rename(address.type = address, 
                    divorced = Pstatus, 
                    mom.edu = Medu, 
                    dad.edu = Fedu, 
                    mom.occ = Mjob, 
                    dad.occ = Fjob, 
                    higher.edu = higher, 
                    home.internet = internet, 
                    weekday.alc = Dalc, 
                    weekend.alc = Walc, 
                    health.status = health,
                    final.grade = G3, 
                    first.period.grade = G1, 
                    second.period.grade = G2
                    )

# df <- df 

# Replacing variable values with original values
df$famsize[df$famsize == "LE3"] <- "less than or equal to 3"
df$famsize[df$famsize == "GT3"] <- "greater than 3"

df$divorced[df$divorced == "T"] <- 0
df$divorced[df$divorced == "A"] <- 1

df$mom.edu[df$mom.edu == 0] <- "no education"
df$mom.edu[df$mom.edu == 1] <- "elementary school"
df$mom.edu[df$mom.edu == 2] <- "middle school"
df$mom.edu[df$mom.edu == 3] <- "high school"
df$mom.edu[df$mom.edu == 4] <- "university or higher"

df$dad.edu[df$dad.edu == 0] <- "no education"
df$dad.edu[df$dad.edu == 1] <- "elementary school"
df$dad.edu[df$dad.edu == 2] <- "middle school"
df$dad.edu[df$dad.edu == 3] <- "high school"
df$dad.edu[df$dad.edu == 4] <- "university or higher"

# Changing 'yes/no' values to binary 1/0 values 
df <- df %>%
  mutate(across(c(schoolsup, famsup, paid, activities, nursery, higher.edu, 
                  home.internet, romantic), ~ ifelse(. == 'yes', 1, 0)))


# Transforming final.grade scale to 0-100 to be more comparable to real scenarios
df <- df %>% mutate(final.grade = final.grade * 5) # Values range from 0 to 20, so just multiply each one by 5
df <- df %>% mutate(first.period.grade = first.period.grade * 5)
df <- df %>% mutate(second.period.grade = second.period.grade *5)

# Making one drinking column 
df$drinking1 <- df$weekday.alc * 5 # Assigning weights for number of days in week and weekend
df$drinking2 <- df$weekend.alc * 2 

df <- df %>% mutate(drinking = rowMeans(select(., c("weekday.alc", "weekend.alc"))))

write.csv(df, "~/Desktop/STAT 3280/Project/cleaned_data.csv", row.names=FALSE)


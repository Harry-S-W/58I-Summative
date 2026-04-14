
# Setting the working directory
getwd()

# libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lme4) # for fancy stats
library(tidyr)

# Ensure the file named "sociophonetics_FACE_data.xlsx" is in the same folder as this r script
df <- readxl::read_excel("sociophonetics_FACE_data.xlsx") # Outputs dataframe called df


# --- CLEANING DATA ---
# This section involves cleaning the data primarily through the tidyverse package. 


# this part is not mandatory but I am renaming the columns to the following standard:
#   - Snake case (i.e. snake_case)
#   - all lower case (besides F for Formant)
#   - No acronyms unless well known (i.e. F for Formant)

df <- df %>% 
  rename(
    speaker = Speaker,
    # col 2 - 4 are good
    text = Text,
    target_transcript = `Target transcript`,
    segments_before = `Segment Before`,
    segments_after = `Segment After`,
    # F for formant can stay capitalized as that is how it is written 
    `F1_time_0.2` = `F1-time_0.2`,
    `F2_time_0.2` = `F2-time_0.2`
  )

# This filters out all the NA values besides the ones in before and after segment
# 
df <- df %>% 
  drop_na(-c(segments_before, segments_after))

df <- df %>% 
  mutate(segments_before = replace_na(segments_before, "None"),
         segments_after = replace_na(segments_after, "None"))

# We can then drop the gender column as all the values are the same
df$gender <- NULL

# class column to Non-Professional and Professional
# This for some reason I could not do so I googled "replace a value in R in a certain column based on value"
# And used data from this source: https://www.geeksforgeeks.org/r-language/replace-values-based-on-condition-in-r/

df$class[df$class == "N"] <- "Non-Professional"
df$class[df$class == "P"] <- "Professional"

# We are also going to make a column called age which will take the average of the years of birth, 
# get the average, and and say who ever is born after that is "younger" and whoever is born before is "older"

age_average <- mean(df$year_of_birth, na.rm = TRUE)
round(age_average, digits = 0) # rounding to whole number

# just like renaming N and P, we can check if an age is less than age_average or greater than
# and put that in a new function using mutate

df <- df %>% 
  mutate(
    age = ifelse(df$year_of_birth < age_average, "Older", "Younger")
  )

df <- df %>% 
  relocate(age, .after = year_of_birth)


# we want to take away all the values in column (target_transcript) which are not
# pure characters (abcdefg...)

df <- df %>%
  mutate(target_transcript = str_to_lower(target_transcript),# makes all words in target_transcript lower case
         target_transcript = str_remove_all(target_transcript, "[[:punct:]><]"), # removes any punctuation
         # ^ it does turn "x-ray" into "xray" and "man-made" into "manmade" but we can adjust our search parameters. 
         target_transcript = str_trim(target_transcript)) # removes white space before and after words


# We now need to filter out any target transcript that is not the FACE (/eɪ/) vowel
# luckily most target transcripts

# We can make a filter which looks for common orthographic structures in FACE lexical set

FACE <- "a.[e]$|ai|ay|eigh|ei|ey"

df <- df[grepl(FACE, df$target_transcript, ignore.case = TRUE), ] # gets rid of all words without the structure defined in the filter above

df <- df[!grepl("are$|air", df$target_transcript, ignore.case = TRUE), ]

# Next steps to clean the data:
# 1. z-score normalization. What we are focusing on is how class (and we can do age as well) changes the proruction
# of the FACE vowel sound. In this case, we don't want the data to be skewed based on the shape of the persons
# vocal tract

# 2. Filtering formant outliers. There are some really high F1/F2 values which need to be filtered out
# the best way to do this is probalby to check how many fall within 2 or so standard deviations

# 3. (optional) Group the segmenets before and after into their respective place of articulation (coronal, glide, velar, etc)

# addressing point 2 - Filtering

# first we can plot F1/F2 to see where our values tend to fall
# we can also draw ellipse around them to show where the majority of the data falls (95%)
# this gives a good idea of what we are dealing with

ggplot(df, aes(x = F2_time_0.2, y = F1_time_0.2, color = class)) +
  geom_point(alpha = 0.4) +
  stat_ellipse(level = 0.95) + # draws a line around 95% of our cluster
  scale_x_reverse() + # Reverse F2 so "Front" is on the left
  scale_y_reverse() + # Reverse F1 so "High" is at the top
  labs(title = "FACE Vowel: F1 vs F2",
       x = "F2 (Hz)", y = "F1 (Hz)") +
  theme_minimal()
  theme(legend.position = "bottom")

# we can now filter out all the data that exists outside of 2.5 SD

df <- df %>%
  group_by(speaker) %>%
  mutate( # making columns where we get the mean of F1/F2 for that specific speaker
    F1_mean = mean(F1_time_0.2),
    F1_sd = sd(F1_time_0.2),
    F2_mean = mean(F2_time_0.2),
    F2_sd = sd(F2_time_0.2)
  ) %>%
  
    filter(
      # filtering out any values that are not within 2.5 SD
      
      F1_time_0.2 > (F1_mean - 2.5 * F1_sd) & F1_time_0.2 < (F1_mean + 2.5 * F1_sd),
      
      F2_time_0.2 > (F2_mean - 2.5 * F2_sd) & F2_time_0.2 < (F2_mean + 2.5 * F2_sd)
    )

# plotting again to see how it has changed

ggplot(df, aes(x = F2_time_0.2, y = F1_time_0.2, color = class)) +
  geom_point(alpha = 0.4) +
  stat_ellipse(level = 0.95) + # draws a line around 95% of our cluster
  scale_x_reverse() + # Reverse F2 so "Front" is on the left
  scale_y_reverse() + # Reverse F1 so "High" is at the top
  labs(title = "Filtered FACE Vowel by class",
       x = "F2 (Hz)", y = "F1 (Hz)") +
  theme_minimal()
  theme(legend.position = "bottom")

# Now that the data has been filtered to remove the extreme outliers, we are going to normalize our data.
# This is because we aren't comparing things like mouth shape, but how different groups say things differently based
# on a controlled categorical variables (their class)

df <- df %>%
  group_by(speaker) %>%
  mutate(
      F1_normalized = (F1_time_0.2 - F1_mean) / F1_sd,
      F2_normalized = (F2_time_0.2 - F2_mean) / F2_sd
  )

summary(df$F1_normalized) # above -2.5 and below +2.5 SD because we filtered them out !!
summary(df$F2_normalized) # above -2.5 and below +2.5 SD because we filtered them out !!

ggplot(df, aes(x = F2_normalized, y = F1_normalized, color = class)) +
  geom_point(alpha = 0.4, size = 1) + 
  stat_ellipse(level = 0.95, linewidth = 1) + 
  scale_x_reverse() + 
  scale_y_reverse() + 
    labs(
    title = "Normalized Vowel by Class",
    x = "F2 (Standard Deviations)",
    y = "F1 (Standard Deviations)",
    color = "Speaker Class"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# FINAL step in Data cleanig is changing the segmenets before and after to places of articulation
# for further analysis. 

# Luckily there are no dental fricatives in the before and after transcript

unique(df$segments_before)
unique(df$segments_after)

# Now we can make a little filter by making a seperate column using mutate
# and then using a match statement to say when a value is so and so, it is this place
# of articulation. 
# We can do this or both columns

df <- df %>%
  mutate(
    place_before = case_when(
      segments_before %in% c("p", "b", "m") ~ "Bilabial",
      segments_before %in% c("f", "v") ~ "Labiodental",
      segments_before == "T" ~ "Dental",
      segments_before %in% c("t", "d", "s", "z", "n", "l", "r") ~ "Alveolar",
      segments_before %in% c("S", "J", "_") ~ "Postalveolar",
      segments_before %in% c("k", "g") ~ "Velar",
      segments_before == "h" ~ "Glottal",
      segments_before == "w" ~ "Labiovelar",
      segments_before %in% c("i", "I") ~ "Palatal_Vowel",
      segments_before == "@" ~ "Schwa",
      TRUE ~ "Other"
    ),
    place_after = case_when(
      segments_after %in% c("p", "b", "m") ~ "Bilabial",
      segments_after %in% c("f", "v") ~ "Labiodental",
      segments_after == "T" ~ "Dental",
      segments_after %in% c("t", "d", "s", "z", "n", "l", "r") ~ "Alveolar",
      segments_after %in% c("S", "J", "_") ~ "Postalveolar",
      segments_after %in% c("k", "g") ~ "Velar",
      segments_after == "h" ~ "Glottal",
      segments_after == "w" ~ "Labiovelar",
      segments_after %in% c("i", "I") ~ "Palatal_Vowel",
      segments_after == "@" ~ "Schwa",
      TRUE ~ "Other"
    )
  )

unique(df$place_before)
unique(df$place_after)
# No "Other" which is good :)

# For reading the new dataset, I am going to move these columns to where the segment_before/after columns are

df <- df %>%
  relocate(place_before, .after = segments_before)
df <- df %>%
  relocate(place_after, .after = segments_after)

# Lastly, we can make a column for voiced/unvoiced

df <- df %>%
  mutate(
    voicing_after = case_when(
      segments_after %in% c("p", "t", "k", "f", "s", "S", "J", "h", "T") ~ "Voiceless",
      segments_after %in% c("b", "d", "g", "v", "z", "_", "m", "n", "l", "r", "w", "i", "I", "@", "-") ~ "Voiced",
      TRUE ~ "Other"
    ),
    
    voicing_before = case_when(
      segments_before %in% c("p", "t", "k", "f", "s", "S", "J", "h", "T") ~ "Voiceless",
      segments_before %in% c("b", "d", "g", "v", "z", "_", "m", "n", "l", "r", "w", "i", "I", "@", "-") ~ "Voiced",
      TRUE ~ "Other"
    )
  )

df <- df %>%
  relocate(voicing_after, .after = place_after)
df <- df %>%
  relocate(voicing_before, .after = place_before)

# we also make a seperate df for average speaker formant values 
df_average_speaker_formants <- df %>%
  group_by(speaker) %>%
  summarize(F1_normalized = mean(F1_normalized), 
            F2_normalized = mean(F2_normalized),
            class = mean(class),
            age = mean(age))

# writing the new dataframe as a csv

write.csv(df,"/Users/harrywoodhouse/CODE/Linguistics as Data Science Code/Summative/clean_data.csv", row.names = FALSE)


# --- STATISTICAL ANALYSIS ---
# Now that the data is clean and we have added some extra bits (place of articulation and voicing)
# we can analyze our data. 
# The first thing we can do is plot our data to see if it is normally distributed 

# Plotting F1
ggplot(df, aes(x = F1_normalized, fill = class)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.2) +
  labs(title = "Distribution of F1 Normalized by Class",
       x = "F1 Normalized (Openness)",
       y = "Density") +
  theme_minimal()

# Plotting F2
ggplot(df, aes(x = F2_normalized, fill = class)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.2) +
  labs(title = "Distribution of F2 Normalized by Class",
       x = "F1 Normalized (Openness)",
       y = "Density") +
  theme_minimal()

# Both plots appear to show the distribution of normalized formant values to be normally
# In this case, we will use a t-test

# Test 1

t.test(F1_normalized ~ class, data = df_average_speaker_formants) 
t.test(F2_normalized ~ class, data = df_average_speaker_formants)

# The above results were not significant (0.7309, and 0.9658 respectively)
# However, to go beyond a standard analysis, below we are running tests to see if other variables affect formant
# values. 

# Test 2

t.test(F1_normalized ~ age, data = df_average_speaker_formants) 
t.test(F2_normalized ~ age, data = df_average_speaker_formants) # Not significant


# The above results were not significant (0.7309, and 0.9658 respectively)
# However, to go beyond a standard analysis, below we are running tests to see if other variables affect formant
# values. 

# Test 3
# Testing if place of voicing BEFORE target vowel affects formants regardless of class

test_3a <- aov(F1_normalized ~ voicing_before, data = df) # p < 2.2e-16
test_3b <- aov(F2_normalized ~ voicing_before, data = df) # p = 2.2e-16

summary(test_3a)
summary(test_3b)

# Super significant

# Test 4
# Testing if place of voicing AFTER target vowel affects formants regardless of class

test_4a <- aov(F1_normalized ~ voicing_after, data = df) # p = 0.048
test_4b <- aov(F2_normalized ~ voicing_after, data = df) # p = 8.36e-11

summary(test_4a)
summary(test_4b)

# Super significant - especially on F2

# To start comparing things like how voicing of different class affects F1/F2 we need
# to start using an ANOVA (Analysis of Variance) test

# Test 5
# Testing if class AND voicing of before transript affects F1/F2

test_5a = aov(F1_normalized ~ class * voicing_before, data = df)
summary(test_5a)
# No significant findings besides the vociing before affecting F1 which we knew already
# No signficance with class * vocing_before

test_5b = aov(F2_normalized ~ class * voicing_before, data = df)
summary(test_5b)
# There is a significance where class affects F2 based on voicing_before 
# this is an interesting finding and i wonder why this is the case

# Test 6
# Testing if class AND voicing of after transript affects F1/F2

test_6a = aov(F1_normalized ~ class * voicing_after, data = df)
summary(test_6a)


test_6b = aov(F2_normalized ~ class * voicing_after, data = df)
summary(test_6b)

# No significance in either test
# Class and the voicing of the post FACE vowel sound does not affect the F1/F2 values.

# Test 7
# Testing if place of articulation before FACE vowel has an affect (regardless of class)

test_7a = aov(F1_normalized ~ place_before, data = df)
summary(test_7a)
# Super significant, p = 1.82e-15

test_7b = aov(F2_normalized ~ place_before, data = df)
summary(test_7b)
# Super significant, p = 2e-16

# Test 8
# Testing if place of articulation after FACE vowel has afect (regardless of class)

test_8a = aov(F1_normalized ~ place_after, data = df)
summary(test_8a)
# Super significant, p = 9.27e-09

test_8b = aov(F2_normalized ~ place_after, data = df)
summary(test_8b)
# Super significant, p = 2e-16

# Test 9
# Testing if place of articulation before FACE vowel AND class has an affect

test_9a = aov(F1_normalized ~ place_before * class, data = df)
summary(test_9a)
# Not significant: 0.109

test_9b = aov(F2_normalized ~ place_before * class, data = df)
summary(test_9b)
# Not significant: 0.810  

# Test 10
# Testing if place of articulation after FACE vowel AND class has an affect 

test_10a = aov(F1_normalized ~ place_after * class, data = df)
summary(test_10a)
# Not significant: 0.56

test_10b = aov(F2_normalized ~ place_after * class, data = df)
summary(test_10b)
# Not significant 0.812

# Test 11
# Linear regression between age and F1/F2 values of FACE
test_11a = lm(F1_normalized ~ year_of_birth, data = df)
summary(test_11a)
# Not significant p = 0.508

test_11b = lm(F2_normalized ~ year_of_birth, data = df)
summary(test_11b)
# Not significant p = 0.804

# Age on its own does not have an affect on formant values it seems 

# Test 12
# Linear regression between age and F1/F2 values of FACE
test_12a = lm(F1_normalized ~ year_of_birth + class, data = df)
summary(test_12a)
# Not significant p = 0.7522

test_12b = lm(F2_normalized ~ year_of_birth + class, data = df)
summary(test_12b)
# Not significant p = 0.9686


# All the tests above are quite standard and frankly quite boring. Below are more
# advanced tests to push the boat out

# ANOVA and T-Test gave some really valuable insight into our data but they treat every
# value independent of one another - but this is in fact not true as speakers give 
# many "tokens" so we can use something called Linear Mixed-Effects Modelling

# paper I learned LME from https://cran.r-project.org/package=lme4/vignettes/lmer.pdf

#Test 13
test_13 <- lmer(F1_normalized ~ class + voicing_after + (1|speaker) + (1|target_transcript), data = df)
summary(test_13)

# Test 14
# Which place of articulation is most significant? 
# Test 6 and 7 showed that place of articulation was super significant in the F1/F2
# values for the FACE vowel. This test determines which ones are more significant

TukeyHSD(test_6a)
TukeyHSD(test_6b)
TukeyHSD(test_7a)
TukeyHSD(test_7b)

# Test 15

centroids <- df %>%
  group_by(class) %>%
  summarise(
    mean_F1 = mean(F1_normalized), # the current mean values in the df are per person
    mean_F2 = mean(F2_normalized)  # these are the mean per class
  )

# calculating the Euclidean Distance between the two rows - which is basically just
# pythagorean theorem
dist_val <- sqrt(
  (centroids$mean_F1[1] - centroids$mean_F1[2])^2 + 
    (centroids$mean_F2[1] - centroids$mean_F2[2])^2
)

print(dist_val)
# distance is absolutely tiny (0.008685532) which shows quantititively that there
# is really no discernible difference in F1/F2 values for the FACE vowel based
# on class. 

# Test 16
# Because class seems to really have no affect at all, we will compare the youngest and oldest 
# people in the data set and see how far apart they are

age_low <- quantile(df$year_of_birth, 0.1, na.rm = TRUE)
age_high <- quantile(df$year_of_birth, 0.9, na.rm = TRUE)

age_stats <- df %>%
  mutate(age_group = case_when(
    year_of_birth <= age_low ~ "Oldest",
    year_of_birth >= age_high ~ "Youngest"
  )) %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group) %>%
  summarise(
    mean_F1 = mean(F1_normalized, na.rm = TRUE),
    mean_F2 = mean(F2_normalized, na.rm = TRUE)
  )

# fyi age_stats[1,] is oldest and age_stats[2,] is youngest
dist_age <- sqrt(
  (age_stats$mean_F1[1] - age_stats$mean_F1[2])^2 + 
    (age_stats$mean_F2[1] - age_stats$mean_F2[2])^2
)

print(dist_age)

# ~ 10x class 
# Age has far more affect than class :)

# END OF FILE :)

median <- range(df$F2_time_0.2[df$class %in% c("N")], na.rm = TRUE)


median(df$F1_time_0.2)
median(df$F2_time_0.2)

mean(df$F1_time_0.2)
mean(df$F2_time_0.2)

range(df$F1_time_0.2)
range(df$F2_time_0.2)

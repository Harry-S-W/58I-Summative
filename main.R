
# Setting the working directory
getwd()

# libraries
library(dplyr)
library(tidyverse)

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

# we want to take away all the values in column (target_transcript) which are not
# pure characters (abcdefg...)

df <- df %>%
  mutate(clean_target_transcript = str_to_lower(target_transcript),# makes all words in target_transcript lower case
         clean_target_transcript = str_remove_all(clean_target_transcript, "[[:punct:]><]"), # removes any punctuation
         # ^ it does turn "x-ray" into "xray" and "man-made" into "manmade" but we can adjust our search parameters. 
         clean_target_transcript = str_trim(clean_target_transcript)) # removes white space before and after words

# to move the new column next to the original target_transcript column, we use the relocate tool
# I searched "How to relocate column in R" and utilized information from https://dplyr.tidyverse.org/reference/relocate.html

df <- df %>%
  relocate(clean_target_transcript, .after = target_transcript)


# We now need to filter out any target transcript that is not the FACE (/eɪ/) vowel
# luckily most target transcripts

# First we are gonna remove any row which is not a valu (N/A) or a random symbol

df <- df %>%
  filter(clean_target_transcript != NA) # not correct will fix


unique(df$clean_target_transcript)

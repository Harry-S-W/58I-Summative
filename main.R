
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
#   - all lower case
#   - No acronyms unless well known (i.e. f for formant)

df1 <- df %>% 
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

print("Ayup")

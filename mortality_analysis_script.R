# Setup ----
library(renv)
library(rstudioapi)
library(tidyverse)

setwd(dirname(getActiveDocumentContext()$path))

renv::restore()

df <- read.delim("mortality_1999-2016.txt")

# Data Cleaning ----

# Remove columns with the "Total" value
df <- subset(df, Notes != "Total")

# Check for NA values
apply(df, 2, function(x) sum(is.na(x)))

# Create new dataframe to examine cause of NA values
year_na <- subset(df, subset = is.na(df$Year))

head(year_na)

# Remove NA values

df <- subset(df, subset = !is.na(df$Year))

# Check empty values

apply(df, 2, function(x) sum(x == ""))

# Check dataframe length to see if empty Notes values = total columns

dim(df)

# Check dataframe structure

str(df)

# Check unique values of columns to be converted into keys

unique(df$ICD.Chapter)
unique(df$ICD.Chapter.Code)
unique(df$Year)
unique(df$Year.Code)
unique(df$Age.Group)
unique(df$Age.Group.Code)
unique(df$Gender)
unique(df$Gender.Code)
unique(df$Race)
unique(df$Race.Code)

# Remove Not Stated values from Age.Group

df <- subset(df, subset = df$Age.Group != "Not Stated")

# Remove non-numeric values from Crude.Rate

df$Crude.Rate <- gsub("[^0-9.]", "", df$Crude.Rate)

# MAYBE MAKE A FUNCTION FOR THIS
# Check non-numeric in Crude.Rate

length(grep("(^$)|([0-9])", df$Crude.Rate, invert = TRUE))

# Check non-numeric in Population

length(grep("(^$)|([0-9])", df$Population, invert = TRUE))

# Check non-numeric in Deaths

length(grep("(^$)|([0-9])", df$Deaths, invert = TRUE))

# Convert Data Types

factors <- c("ICD.Chapter.Code",
            "Age.Group.Code",
            "Gender.Code",
            "Race.Code")

df[,factors] <- lapply(df[,factors],
                        factor)

df$Crude.Rate <- as.numeric(df$Crude.Rate)

df$Deaths <- as.integer(df$Deaths)

df$Population <- as.integer(df$Population)

# Create keys for columns to be removed

gender_key <- unique(df[, c("Gender.Code", "Gender")])
age_key <- unique(df[, c("Age.Group.Code", "Age.Group")])
icd_key <- unique(df[, c("ICD.Chapter.Code", "ICD.Chapter")])
race_key <- unique(df[, c("Race.Code", "Race")])

# Fix long chapter names for ICD 10

icd_key$ICD.Chapter <- c("Parasitic Diseases",
                         "Neoplams",
                         "Blood Diseases",
                         "Endocrine Diseases",
                         "Mental Disorders",
                         "Nervous System Diseases",
                         "Eye Diseases",
                         "Ear Diseases",
                         "Circulatory Diseases",
                         "Respiratory Diseases",
                         "Digestive Diseases",
                         "Skin Diseases",
                         "Muscular Diseases",
                         "Genitourinary Diseases",
                         "Pregnancy/Childbirth",
                         "Perinatal Conditions",
                         "Congenital Malformations",
                         "Not Classified Elsewhere",
                         "Special Purposes",
                         "External Causes"
)

# Add population weights to age key

age_key$Age.Weight <- c(0.013818, # <1
                        0.055317, # 1-4
                        0.145565, # 5-9
                        0.145565, # 10-14
                        0.138646, # 15-19
                        0.138646, # 20-24
                        0.135573, # 25-34
                        0.162613, # 35-44
                        0.134834, # 45-54
                        0.087247, # 55-64
                        0.066037, # 65-74
                        0.044842, # 75-84
                        0.015508  # >=85
)

# Remove unwanted columns
df <- subset(df, select = -c(1, 2, 5, 6, 8, 10))

# Exploratory Analysis ----

total_deaths <- df %>%
  left_join(icd_key, by = c("ICD.Chapter.Code" = "ICD.Chapter.Code")) %>%
  group_by(ICD.Chapter.Code, ICD.Chapter) %>%
  summarise(Deaths = sum(Deaths)) %>%
  arrange(desc(Deaths))

print(total_deaths)

# Create function for calculating age adjusted death rates

age_adjust <- function(X, ...){
  
  X <- group_by(X, ..., Age.Group.Code)
  
  X <- summarise(X, Population = sum(Population), Deaths = sum(Deaths))

  X$Crude.Rate <- (X$Deaths * 100000) / X$Population
    
  X <- merge(
    X,
    age_key[, c("Age.Group.Code", "Age.Weight")],
    by = "Age.Group.Code"
  ) 
  
  X$Age.Adjustment <- X$Age.Weight * X$Crude.Rate
  
  X <- group_by(X, ...)
  
  X <- summarise(X, Crude.Rate.Adjusted = sum(Age.Adjustment))
  
  return(X)
}

# Age Adjusted Crude Rate by year for two causes of death

death_cause <- c("I00-I99", "C00-D48")

filter(df, ICD.Chapter.Code %in% death_cause) %>%
  age_adjust(ICD.Chapter.Code, Year) %>%
  ggplot(aes(x = Year, y = Crude.Rate.Adjusted, color = ICD.Chapter.Code)) +
  geom_line() +
  geom_point() +
  ylab("Age Adjusted Crude Death Rate") +
  labs(title = "Top Causes of Death Over Time") +
  scale_color_discrete(
    labels = icd_key$ICD.Chapter[
      icd_key$ICD.Chapter.Code %in% death_cause
    ],
    name = "Cause of Death"
  )

# Age Adjusted Crude Rate by year and race for two causes of death

filter(df, ICD.Chapter.Code %in% death_cause) %>%
  age_adjust(ICD.Chapter.Code, Year, Race.Code) %>%
  ggplot(
    aes(
      x = Year,
      y = Crude.Rate.Adjusted,
      shape = ICD.Chapter.Code,
      color = Race.Code
    )
  ) +
  geom_line() +
  geom_point() +
  ylab("Age Adjusted Crude Death Rate") +
  labs(title = "Top Causes of Death Over Time") +
  scale_shape_discrete(
    labels = icd_key$ICD.Chapter[
      icd_key$ICD.Chapter.Code %in% death_cause
    ],
    name = "Cause of Death"
  ) +
  scale_color_discrete(labels = race_key$Race, name = "Race")

# Age adjusted crude rate for common causes of death 

death_cause <- total_deaths$ICD.Chapter.Code[
  which(
    total_deaths$Deaths > 999999 & 
      total_deaths$Deaths < 10000000
  )
]

filter(df, ICD.Chapter.Code %in% death_cause) %>%
  age_adjust(ICD.Chapter.Code, Year) %>%
  ggplot(aes(x = Year, y = Crude.Rate.Adjusted, color = ICD.Chapter.Code)) +
  geom_line() +
  geom_point(aes(shape = ICD.Chapter.Code)) + 
  ylab("Age Adjusted Crude Death Rate") +
  labs(title = "Common Causes of Death Over Time") +
  scale_color_discrete(
    labels = icd_key$ICD.Chapter[
      icd_key$ICD.Chapter.Code %in% death_cause
    ],
    name = "Cause of Death"
  ) +
  scale_linetype_discrete(
    labels = icd_key$ICD.Chapter[
      icd_key$ICD.Chapter.Code %in% death_cause
    ],
    name = "Cause of Death"
  ) +
  scale_shape_manual(
    labels = icd_key$ICD.Chapter[
      icd_key$ICD.Chapter.Code %in% death_cause
    ],
    name = "Cause of Death",
    values = c(15, 16, 17, 15, 16, 17, 15, 16)
  )

# Mental disorders by race

filter(df, ICD.Chapter.Code == "F01-F99") %>%
  age_adjust(ICD.Chapter.Code, Year, Race.Code) %>%
  ggplot(aes(x = Year, y = Crude.Rate.Adjusted, color = Race.Code)) +
  geom_line() +
  geom_point() +
  ylab("Age Adjusted Crude Death Rate") +
  labs(title = "Deaths from Mental Illness by Race") +
  scale_color_discrete(labels = race_key$Race, name = "Race")

# Mental disorders by race and gender

filter(df, ICD.Chapter.Code == "F01-F99") %>%
  age_adjust(ICD.Chapter.Code, Year, Race.Code, Gender.Code) %>%
  ggplot(aes(x = Year, y = Crude.Rate.Adjusted, color = Race.Code, shape = Gender.Code)) +
  geom_line() +
  geom_point() +
  ylab("Age Adjusted Crude Death Rate") +
  labs(title = "Deaths from Mental Illness by Race") +
  scale_color_discrete(labels = race_key$Race, name = "Race")

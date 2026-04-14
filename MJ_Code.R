
install.packages("haven")
library(haven)                
install.packages("labelled")
library(labelled)
install.packages("tidyverse")
library(tidyverse)

df_raw <- read_sav("/Users/navs/Downloads/MajorProject/ST606-Project/Data/NPHA_Wave13_August2023.sav", encoding = "latin1")
nrow(df_raw)   
ncol(df_raw) 
head(df_raw)
names(df_raw)
View(df_raw)

names(df_raw)[1:50]
names(df_raw)[51:100]
names(df_raw)[101:150]
names(df_raw)[151:200]
names(df_raw)[343:392]

table(df_raw$hl003newb)
table(df_raw$hl003newb, useNA = "always")
attr(df_raw$hl003newb, "labels")

prop.table(table(df_raw$hl003newb)) * 100

our_variables <- c("hl003newb", "hl020", "hl021_feet", "hl021_inches","age", "gender", "racethnicity","employ", "income4", "q1", "q2", "q13", 
                   "q8", "q26", "q39_2", "q41a", "q41b","q19", "q18", "hl022", "q7_1", "q7_2", "q7_4")

for (v in our_variables) {
  label <- attr(df_raw[[v]], "label")
  print(paste(v, "→", label))
}

summary(as.numeric(df_raw$hl020))#weight
table(df_raw$hl021_feet)#height_feet
table(df_raw$hl021_inches)#height_inches
summary(as.numeric(df_raw$age))#age
table(df_raw$gender)
attr(df_raw$gender, "labels")#gender
table(df_raw$racethnicity)
attr(df_raw$racethnicity, "labels")#race
table(df_raw$income4)
attr(df_raw$income4, "labels")#income
table(df_raw$q1)
attr(df_raw$q1, "labels")#physical health
table(df_raw$q26)
attr(df_raw$q26, "labels")#aspirin



df_raw$diabetes <- ifelse(df_raw$hl003newb == 1, 1,
                          ifelse(df_raw$hl003newb == 2, 0, NA))
df <- df_raw %>% filter(!is.na(diabetes))


df <- df %>%
  mutate(
    weight_lbs   = ifelse(hl020 >= 9000 | hl020 < 50, 
                          NA, as.numeric(hl020)),
    height_ft    = ifelse(hl021_feet >= 90 | hl021_feet < 3, 
                          NA, as.numeric(hl021_feet)),
    height_in    = ifelse(hl021_inches >= 90, 
                          NA, as.numeric(hl021_inches)),
    height_total = height_ft * 12 + height_in,
    bmi          = (weight_lbs / (height_total^2)) * 703,
    bmi          = ifelse(bmi < 10 | bmi > 80, NA, bmi)
  )

cat("Rows:", nrow(df), "\n")
cat("Diabetes Yes:", sum(df$diabetes == 1), "\n")
cat("Diabetes No:", sum(df$diabetes == 0), "\n")
cat("Mean BMI:", round(mean(df$bmi, na.rm=TRUE), 1), "\n")
cat("BMI missing:", sum(is.na(df$bmi)), "\n")


install.packages("naniar")
install.packages("sjPlot")
library(naniar)
library(sjPlot)
library(tidyverse)

library(haven)


df <- df %>% zap_labels()
cat("Labels removed \n")
df <- df %>%
  mutate(
    
    
    
    # Gender: 0=Unknown→NA, 1=Male, 2=Female
    gender_r = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE        ~ NA_character_
    ),
    
    # Race
    race_r = case_when(
      racethnicity == 1 ~ "White NH",
      racethnicity == 2 ~ "Black NH",
      racethnicity == 3 ~ "Other NH",
      racethnicity == 4 ~ "Hispanic",
      racethnicity == 5 ~ "2+ races",
      racethnicity == 6 ~ "Asian/PI",
      TRUE              ~ NA_character_
    ),
    
    # Education
    educ_r = case_when(
      educ5 == 1 ~ "Less than HS",
      educ5 == 2 ~ "HS graduate",
      educ5 == 3 ~ "Some college",
      educ5 == 4 ~ "Bachelors",
      educ5 == 5 ~ "Postgraduate",
      TRUE       ~ NA_character_
    ),
    
    # Marital status
    marital_r = case_when(
      marital == 1 ~ "Married",
      marital == 2 ~ "Widowed",
      marital == 3 ~ "Divorced",
      marital == 4 ~ "Separated",
      marital == 5 ~ "Never married",
      TRUE         ~ NA_character_
    ),
    
    # Employment → collapsed into 4 groups
    employ_r = case_when(
      employ %in% c(1,2)   ~ "Working",
      employ == 5          ~ "Retired",
      employ == 6          ~ "Disabled",
      employ %in% c(3,4,7) ~ "Other/unemployed",
      TRUE                 ~ NA_character_
    ),
    
    # Income
    income_r = case_when(
      income4 == 1 ~ "Less than $30k",
      income4 == 2 ~ "$30k-$60k",
      income4 == 3 ~ "$60k-$100k",
      income4 == 4 ~ "$100k+",
      TRUE         ~ NA_character_
    ),
    

    bmi_cat = case_when(
      bmi < 18.5             ~ "Underweight",
      bmi >= 18.5 & bmi < 25 ~ "Normal weight",
      bmi >= 25  & bmi < 30  ~ "Overweight",
      bmi >= 30              ~ "Obese",
      TRUE                   ~ NA_character_
    ),
    
    
    
    # Physical health: 77/98/99 → NA, collapse to 3 groups
    phys_r = case_when(
      q1 %in% c(77,98,99)  ~ NA_character_,
      q1 %in% c(1,2)       ~ "Excellent/Very good",
      q1 == 3              ~ "Good",
      q1 %in% c(4,5)       ~ "Fair/Poor"
    ),
    
    # Mental health
    ment_r = case_when(
      q2 %in% c(77,98,99)  ~ NA_character_,
      q2 %in% c(1,2)       ~ "Excellent/Very good",
      q2 == 3              ~ "Good",
      q2 %in% c(4,5)       ~ "Fair/Poor"
    ),
    
    # Activity limitation
    act_r = case_when(
      q10 %in% c(77,98,99) ~ NA_character_,
      q10 == 1             ~ "Yes, a lot",
      q10 == 2             ~ "Yes, a little",
      q10 == 3             ~ "No"
    ),
    
    # Health trend
    trend_r = case_when(
      hl022 %in% c(77,98,99) ~ NA_character_,
      hl022 == 1             ~ "Getting better",
      hl022 == 2             ~ "Getting worse",
      hl022 == 3             ~ "Stayed same"
    ),
    
   
    
    # Has PCP
    pcp_r = case_when(
      q8 %in% c(77,98,99) ~ NA_character_,
      q8 == 1             ~ "Has PCP",
      q8 == 2             ~ "No PCP"
    ),
    
  
    
   
    
    # Loneliness
    lonely_r = case_when(
      q12 %in% c(77,98,99) ~ NA_character_,
      q12 == 1             ~ "Hardly ever",
      q12 == 2             ~ "Some of the time",
      q12 == 3             ~ "Often"
    ),
    
    # Isolation
    isolate_r = case_when(
      q13 %in% c(77,98,99) ~ NA_character_,
      q13 == 1             ~ "Hardly ever",
      q13 == 2             ~ "Some of the time",
      q13 == 3             ~ "Often"
    ),
    
    # Social activity → collapsed to 3 groups
    social_r = case_when(
      q15 %in% c(77,98,99) ~ NA_character_,
      q15 %in% c(1,2)      ~ "At least weekly",
      q15 == 3             ~ "At least monthly",
      q15 %in% c(4,5)      ~ "Rarely/Never"
    ),
    
    
    
    # Aspirin
    aspirin_r = case_when(
      q26 %in% c(77,98,99) ~ NA_character_,
      q26 %in% c(1,2)      ~ "Takes aspirin",
      q26 == 3             ~ "No aspirin"
    ),
    
   
    
    # Diet change
    diet_r = case_when(
      q41a %in% c(77,98,99) ~ NA_character_,
      q41a %in% c(1,2)      ~ "Changed diet",
      q41a == 3             ~ "No change"
    ),
    
    # Exercise for weight loss
    exercise_r = case_when(
      q41b %in% c(77,98,99) ~ NA_character_,
      q41b %in% c(1,2)      ~ "Exercised",
      q41b == 3             ~ "Did not exercise"
    ),
    
    #  overweight
    everow_r = case_when(
      q19 %in% c(77,98,99) ~ NA_character_,
      q19 == 1             ~ "Ever overweight",
      q19 == 2             ~ "Never overweight"
    ),
    
    # Current weight status
    weightstatus_r = case_when(
      q18 %in% c(77,98,99) ~ NA_character_,
      q18 == 1             ~ "Underweight",
      q18 == 2             ~ "Right weight",
      q18 == 3             ~ "Slightly overweight",
      q18 == 4             ~ "Overweight"
    )
  )


cat("New columns added:", 
    sum(grepl("_r$|bmi_cat", names(df))), "\n")





cat("Gender:\n")
table(df$gender_r, useNA="always") %>% print()

cat("\nRace:\n")
table(df$race_r, useNA="always") %>% print()

cat("\nPhysical health:\n")
table(df$phys_r, useNA="always") %>% print()

cat("\nAspirin:\n")
table(df$aspirin_r, useNA="always") %>% print()

cat("\nBMI category:\n")
table(df$bmi_cat, useNA="always") %>% print()


df_recoded <- df %>%
  select(
    diabetes, age, bmi_cat,
    gender_r, race_r, educ_r, marital_r, 
    employ_r, income_r,
    phys_r, ment_r, act_r, trend_r,
    pcp_r,
    lonely_r, isolate_r, social_r,
    aspirin_r, diet_r, 
    exercise_r, everow_r, weightstatus_r
  )

vis_miss(df_recoded) +
  labs(title = "Missing Data After Recoding")

miss_var_summary(df_recoded) %>%
  filter(n_miss > 0) %>%
  print()






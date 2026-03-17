
install.packages("haven")
library(haven)                
install.packages("labelled")
library(labelled)
install.packages("tidyverse")

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
table(df_raw$q39_2)
attr(df_raw$q39_2, "labels")#ozempic


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











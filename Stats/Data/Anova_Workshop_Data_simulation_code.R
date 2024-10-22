

### Narrative: comparing mindfullness interventions on wellbeing
## 3 treatment groups - wait_list, meditation, therapist_session
## 3 socioeconomic groups  - based on https://ukgeographics.co.uk/blog/social-grade-a-b-c1-c2-d-e
## 2 time point - pre and post

### Simulating based on WEMWBS (14 - 70 range)

## Setting waitlist condition

### Assigning socioeconomic groups into time points


### Setting pre measurements first 

set.seed(42) # setting seed to make replicable simulations

#### AB socioeconomic group
AB <- round(  digits = 0,  #rounding data to 3 decimal places
               rnorm(n = 16, mean = 50, sd = 5) # rnorm simulates normal distributions
)

#### C1 group
C1 <- round(  digits = 0,  #rounding data to 3 decimal places
              rnorm(n = 16, mean = 47, sd = 7) # rnorm simulates normal distributions
)

#### C2 group
C2 <- round(  digits = 0,  #rounding data to 3 decimal places
              rnorm(n = 10, mean = 42, sd = 4) # rnorm simulates normal distributions
)


#### D group
D <- round( digits = 0,  #rounding data to 3 decimal places
              rnorm(n = 15, mean = 39, sd = 7) # rnorm simulates normal distributions
)


Pre <- data.frame(qpcR:::cbind.na(AB,
                                  C1,
                                  C2, D)) %>% # combining columns 
  pivot_longer( # forces our data to be long instead of wide
    cols = c(AB,
             C1,
             C2, D),  # choosing the variables we want
    names_to = "Socioeconomic_group", # name for the new column with the T titles
    values_to = "WEMWBS" # name for the new column with associated values/numbers.
  ) %>%
  mutate(
    ID_number = row_number(),
    Socioeconomic_group = as.factor(Socioeconomic_group)
  ) %>% 
  na.omit()

### Labeling Time point
Pre$Time_point <- "Pre-trial"


# Setting scores to measure limits
Pre$WEMWBS <- pmin(pmax(Pre$WEMWBS, 14), 70)

### Second measurement (Post-trial)
set.seed(42) # setting seed to make replicable simulations

#### AB socioeconomic group
AB <- round(  digits = 0,  #rounding data to 3 decimal places
              rnorm(n = 16, mean = 48, sd = 4) # rnorm simulates normal distributions
)

#### C1 group
C1 <- round(  digits = 0,  #rounding data to 3 decimal places
              rnorm(n = 16, mean = 45, sd = 6) # rnorm simulates normal distributions
)

#### C2 group
C2 <- round(  digits = 0,  #rounding data to 3 decimal places
              rnorm(n = 10, mean = 40, sd = 4) # rnorm simulates normal distributions
)


#### D group
D <- round(  digits = 0,  #rounding data to 3 decimal places
             rnorm(n = 15, mean = 35, sd = 5) # rnorm simulates normal distributions
)

Post <- data.frame(qpcR:::cbind.na(AB,
                                   C1,
                                   C2, D)) %>% # combining columns 
  pivot_longer( # forces our data to be long instead of wide
    cols = c(AB, C1, C2, D),  # choosing the variables we want
    names_to = "Socioeconomic_group", # name for the new column with the T titles
    values_to = "WEMWBS" # name for the new column with associated values/numbers.
  ) %>%
  mutate(
    Socioeconomic_group = as.factor(Socioeconomic_group),
    ID_number = row_number()
  ) %>% 
  na.omit()
#
### Labeling Time point
Post$Time_point <- "Post-trial"

Post$WEMWBS <- pmin(pmax(Post$WEMWBS, 14), 70)



### Combining Data
Waitlist <- data.frame(rbind(Pre, Post)) 
Waitlist$Treatment <-"Waitlist"

#Creating age and gender columns

Waitlist$Age<- round(  digits = 0,  #rounding data to 3 decimal places
           rnorm(n = 114, mean = 45, sd = 10) # rnorm simulates normal distributions
)

Waitlist$Age <- pmin(pmax(Waitlist$Age, 18), 60)

## Setting Meditation condition

### Assigning socioeconomic groups into time points


### Setting pre measurements first 

set.seed(42) # setting seed to make replicable simulations

#### AB socioeconomic group
AB <- round(  digits = 0,  #rounding data to 0 decimal places
              rnorm(n = 15, mean = 49, sd = 5) # rnorm simulates normal distributions
)

#### C1 group
C1 <- round(  digits = 0,  #rounding data to 0 decimal places
              rnorm(n = 16, mean = 48, sd = 7) # rnorm simulates normal distributions
)

#### C2 group
C2 <- round(  digits = 0,  #rounding data to 3 decimal places
              rnorm(n = 11, mean = 41, sd = 4) # rnorm simulates normal distributions
)


#### D group
D <- round( digits = 0,  #rounding data to 3 decimal places
            rnorm(n = 13, mean = 40, sd = 7) # rnorm simulates normal distributions
)


Pre <- data.frame(qpcR:::cbind.na(AB,
                                  C1,
                                  C2, D)) %>% # combining columns 
  pivot_longer( # forces our data to be long instead of wide
    cols = c(AB, 
             C1,
             C2, D),  # choosing the variables we want
    names_to = "Socioeconomic_group", # name for the new column with the T titles
    values_to = "WEMWBS" # name for the new column with associated values/numbers.
  ) %>%
  mutate(
    ID_number = row_number() + 114,
    Socioeconomic_group = as.factor(Socioeconomic_group)
  ) %>% 
  na.omit()

### Labeling Time point
Pre$Time_point <- "Pre-trial"


# Setting scores to measure limits
Pre$WEMWBS <- pmin(pmax(Pre$WEMWBS, 14), 70)

### Second measurement (Post-trial)
set.seed(42) # setting seed to make replicable simulations

#### AB socioeconomic group
AB <- round(  digits = 0,  #rounding data to 3 decimal places
              rnorm(n = 15, mean = 58, sd = 4) # rnorm simulates normal distributions
)

#### C1 group
C1 <- round(  digits = 0,  #rounding data to 3 decimal places
              rnorm(n = 16, mean = 51, sd = 6) # rnorm simulates normal distributions
)

#### C2 group
C2 <- round(  digits = 0,  #rounding data to 3 decimal places
              rnorm(n = 11, mean = 41, sd = 4.5) # rnorm simulates normal distributions
)


#### D group
D <- round(  digits = 0,  #rounding data to 3 decimal places
             rnorm(n = 13, mean = 37, sd = 6.5) # rnorm simulates normal distributions
)

Post <- data.frame(qpcR:::cbind.na(AB,
                                   C1, 
                                   C2, D)) %>% # combining columns 
  pivot_longer( # forces our data to be long instead of wide
    cols = c(AB, C1, 
             C2, D),  # choosing the variables we want
    names_to = "Socioeconomic_group", # name for the new column with the T titles
    values_to = "WEMWBS" # name for the new column with associated values/numbers.
  ) %>%
  mutate(
    Socioeconomic_group = as.factor(Socioeconomic_group),
    ID_number = row_number() +114
  ) %>% 
  na.omit()
#
### Labeling Time point
Post$Time_point <- "Post-trial"

Post$WEMWBS <- pmin(pmax(Post$WEMWBS, 14), 70)

### Combining Data
Meditation <- data.frame(rbind(Pre, Post)) #%>% # combining columns 
Meditation$Treatment <-"Meditation"


## Making Age
Meditation$Age<- round(  digits = 0,  #rounding data to 3 decimal places
                       rnorm(n = 110, mean = 45, sd = 10) # rnorm simulates normal distributions
)

Meditation$Age <- pmin(pmax(Meditation$Age, 18), 60)


stage_1 <- data.frame(rbind(Waitlist, Meditation))  


## Therapy Group

### Setting pre measurements first 

set.seed(44) # setting seed to make replicable simulations

#### AB socioeconomic group
AB <- round(  digits = 0,  #rounding data to 3 decimal places
              rnorm(n = 17, mean = 50, sd = 5) # rnorm simulates normal distributions
)

#### C1 group
C1 <- round(  digits = 0,  #rounding data to 3 decimal places
              rnorm(n = 20, mean = 47, sd = 7) # rnorm simulates normal distributions
)

#### C2 group
C2 <- round(  digits = 0,  #rounding data to 3 decimal places
              rnorm(n = 16, mean = 42, sd = 4) # rnorm simulates normal distributions
)


#### D group
D <- round( digits = 0,  #rounding data to 3 decimal places
            rnorm(n = 14, mean = 39, sd = 7) # rnorm simulates normal distributions
)


Pre <- data.frame(qpcR:::cbind.na(AB, C1,
                                  C2, D)) %>% # combining columns 
  pivot_longer( # forces our data to be long instead of wide
    cols = c(AB, C1,
             C2, D),  # choosing the variables we want
    names_to = "Socioeconomic_group", # name for the new column with the T titles
    values_to = "WEMWBS" # name for the new column with associated values/numbers.
  ) %>%
  mutate(
    ID_number = row_number() + 224,
    Socioeconomic_group = as.factor(Socioeconomic_group)
  ) %>% 
  na.omit()

### Labeling Time point
Pre$Time_point <- "Pre-trial"


# Setting scores to measure limits
Pre$WEMWBS <- pmin(pmax(Pre$WEMWBS, 14), 70)

### Second measurement (Post-trial)
set.seed(45) # setting seed to make replicable simulations

#### AB socioeconomic group
AB <- round(  digits = 0,  #rounding data to 0 decimal places
              rnorm(n = 17, mean = 56, sd = 4) # rnorm simulates normal distributions
)

#### C1 group
C1 <- round(  digits = 0,  #rounding data to 0 decimal places
              rnorm(n = 20, mean = 53, sd = 4) # rnorm simulates normal distributions
)

#### C2 group
C2 <- round(  digits = 0,  #rounding data to 0 decimal places
              rnorm(n = 16, mean = 48, sd = 5) # rnorm simulates normal distributions
)


#### D group
D <- round(  digits = 0,  #rounding data to 3 decimal places
             rnorm(n = 14, mean = 45, sd = 5) # rnorm simulates normal distributions
)

Post <- data.frame(qpcR:::cbind.na(AB,C1,
                                   C2, D)) %>% # combining columns 
  pivot_longer( # forces our data to be long instead of wide
    cols = c(AB, C1, 
             C2, D),  # choosing the variables we want
    names_to = "Socioeconomic_group", # name for the new column with the T titles
    values_to = "WEMWBS" # name for the new column with associated values/numbers.
  ) %>%
  mutate(
    Socioeconomic_group = as.factor(Socioeconomic_group),
    ID_number = row_number() + 224
  ) %>% 
  na.omit()
#
### Labeling Time point
Post$Time_point <- "Post-trial"

Post$WEMWBS <- pmin(pmax(Post$WEMWBS, 14), 70)

### Combining Data
Therapy <- data.frame(rbind(Pre, Post)) #%>% # combining columns 
Therapy$Treatment <-"Therapy"

## Simulating Age

Therapy$Age<- round(  digits = 0,  #rounding data to 3 decimal places
                         rnorm(n = 134, mean = 45, sd = 10) # rnorm simulates normal distributions
)

Therapy$Age <- pmin(pmax(Therapy$Age, 18), 60)

# Final data


Fin_data <- data.frame(rbind(Waitlist, Meditation, Therapy)) %>%
  mutate(Time_point = as.factor(Time_point),
         Treatment = as.factor(Treatment),
         Treatment = fct_relevel(Treatment,  "Waitlist"),
         Socioeconomic_group = fct_relevel(Socioeconomic_group, "D"),
         Gender = sample(c("Female", "Male", "Non-binary"), 358, replace = TRUE, prob = c(.52, .46, .02)),
         Gender = as.factor(Gender)
         ) %>% 
  filter(#Time_point == "Post-trial",
         Socioeconomic_group != "C2"
         ) 


# changing reference group to Waitlist
#Fin_data$Treatment <- relevel(Fin_data$Treatment, ref = "Waitlist")

# Change reference group of socioeconomic to D

#Fin_data$Socioeconomic_group <- relevel(Fin_data$Socioeconomic_group, ref = "D")

summary(Fin_data)

write.csv(Fin_data, "ANOVA_workshop_data.csv", row.names = FALSE)

## Testing data

summary(lm(WEMWBS ~ Socioeconomic_group * Treatment , Fin_data))


res.aov <- aov(WEMWBS ~ Treatment/Socioeconomic_group  ,
               Fin_data)
summary(res.aov)
res.aov
tukey_hsd(res.aov)  %>% gt()

Fin_data %>%
  group_by( Socioeconomic_group) %>% 
  tukey_hsd(WEMWBS ~ Treatment) %>% 
  gt()


ggplot(Fin_data,
       aes(fill = Treatment, y = WEMWBS,
           x = Socioeconomic_group)) +
  geom_boxplot()

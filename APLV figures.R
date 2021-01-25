library(lme4)
library(lmerTest)
library(MASS)
library(car)
library(tidyr)
library(ggplot2)
library(dplyr)
library(gridExtra)

Data.all_speakers.five_verbs <- read.delim("G:/My Drive/Research/Projects/Ende n-variation/Statistics Files/Data-all_speakers-five_verbs.txt")
data <- Data.all_speakers.five_verbs

## Figure 2

## Count Nasal_elision percentages for each speaker
data.plot <- data %>%
  group_by(Sex, Age_1, Orator) %>%
  count(Nasal_drop)

## Spread nasal realization and deletion out so each speaker has one row
## and counts for realization/deletion each have their own column
data.plot <- spread(data.plot, Nasal_drop, n)

## Add a column for totals
data.plot$total <- data.plot$"0" + data.plot$"n"

## Add column for and compute percents as decimals
data.plot$dec.drop <- data.plot$"0"/data.plot$total
data.plot$dec.realised <- data.plot$"n"/data.plot$total

## Convert decimals to percentages
data.plot$percent.drop <- paste(round(data.plot$dec.drop*100,digits=1))
data.plot$percent.realised <- paste(round(data.plot$dec.realised*100,digits=1))

## Make sure percentage values are numeric
data.plot$percent.drop <- as.numeric(data.plot$percent.drop)
data.plot$percent.realised <- as.numeric(data.plot$percent.realised)

summary(data.plot)

#### PLOT FOR FIGURE 2

## Relabel for a more beautiful output
data.plot$Orator <- ifelse(data.plot$Orator=="Y", "Orator", "Non-orator")
data.plot$Orator <- as.factor(data.plot$Orator)
data.plot$Orator <- relevel(data.plot$Orator, "Orator")
data.plot$Sex <- ifelse(data.plot$Sex=="F", "Women", "Men")
data.plot$Sex <- as.factor(data.plot$Sex)
data.plot$Sex <- relevel(data.plot$Sex, "Women")

plot_both <- function()
{
  
  ggplot(data.plot, aes(x=Age_1, y=percent.realised, shape=Sex)) + 
    geom_point(size=6, alpha=0.85) + scale_shape_manual(values=c(16, 4)) + ylim(0,100) +
    labs(x="Age", y="Percentage of final-/n/ realised", title="/n/-Production by orators vs. non-orators", shape="Gender") + 
    facet_grid(. ~ Orator)
}

ggsave(
  "Figure 2.png",
  plot_both(),
  width = 5.93,
  height = 4.53,
  dpi = 1200
)

# PLOT FOR FIGURE 3 - just linguistic factors

## Count nasal realization for each Fol_seg and Tense groups
data.plot <- data %>%
  group_by(Fol_seg, Tense) %>%
  count(Nasal_drop)

## Spread affricate and stops out so each speaker has one row
## and counts for affricate/stop each have their own column
data.plot <- spread(data.plot, Nasal_drop, n)
data.plot$"0" <- c(1,81,2,2,8,457,23,17,0,225,8,8,3,138,5,2)

## Add a column for totals
data.plot$total <- data.plot$"0" + data.plot$"n"

## Add column for and compute percents as decimals
data.plot$dec.drop <- data.plot$"0"/data.plot$total
data.plot$dec.realised <- data.plot$"n"/data.plot$total

## Convert decimals to percentages
data.plot$percent.drop <- paste(round(data.plot$dec.drop*100,digits=1))
data.plot$percent.realised <- paste(round(data.plot$dec.realised*100,digits=1))

## Make sure percentage values are numeric
data.plot$percent.drop <- as.numeric(data.plot$percent.drop)
data.plot$percent.realised <- as.numeric(data.plot$percent.realised)

summary(data.plot)

#### 3.2 Point plots with ::ggplot ####

## Relabel for a more beautiful output
data.plot$Tense <- ifelse(data.plot$Tense=="fut", "Future", ifelse(data.plot$Tense=="rem", "Remote Past", ifelse(data.plot$Tense=="rec", "Recent Past", "Present")))
data.plot$Tense <- as.factor(data.plot$Tense)

plot_both <- function()
{
  
  ggplot(data.plot, aes(x=Fol_seg, y=percent.realised)) + 
    geom_point(size=6, alpha=0.85) + ylim(0,100) +
    labs(x="Following Segment", y="Percentage of final-/n/ realised", title="/n/-Production by linguistic context") + 
    facet_grid(. ~ Tense)
}

ggsave(
  "Figure 3.png",
  plot_both(),
  width = 5.93,
  height = 4.53,
  dpi = 1200
)
### Figure 4
with(data.five,table(Tense, Age_1))

## Count nasal realization/deletion for each speaker group/genre
data.five.plot <- data.five %>%
  group_by(Tense, Age_1) %>%
  count(Nasal_drop)

## Spread realizations/deletions out so each speaker has one row
## and counts for nasal deletion each have their own column
data.five.plot <- spread(data.five.plot, Nasal_drop, n)
data.five.plot$"0" <- c(2,9,0,1,14,112,0,16,1,22)


## Add a column for totals
data.five.plot$total <- data.five.plot$"0" + data.five.plot$n

## Add column for and compute percents as decimals
data.five.plot$dec.drop <- data.five.plot$"0"/data.five.plot$total
data.five.plot$dec.realised <- data.five.plot$n/data.five.plot$total

## Convert decimals to percentages
data.five.plot$percent.drop <- paste(round(data.five.plot$dec.drop*100,digits=1))
data.five.plot$percent.realised <- paste(round(data.five.plot$dec.realised*100,digits=1))

## Make sure percentage values are numeric
data.five.plot$percent.drop <- as.numeric(data.five.plot$percent.drop)
data.five.plot$percent.realised <- as.numeric(data.five.plot$percent.realised)

summary(data.five.plot)

#### 3.2 Point plots with ::ggplot ####

## Relabel for a more beautiful output
data.five.plot$Tense <- ifelse(data.five.plot$Tense=="fut", "Future", ifelse(data.five.plot$Tense=="rem", "Remote Past", ifelse(data.five.plot$Tense=="rec", "Recent Past", ifelse(data.five.plot$Tense=="inf", "Infinitive", "Present"))))
data.five.plot$Age <- data.five.plot$Age_1
data.five.plot$Tense <- as.factor(data.five.plot$Tense)
data.five.plot$Tense <- relevel(data.five.plot$Tense, "Infinitive")

plot_both <- function()
{
  
  ggplot(data.five.plot, aes(x=Tense, y=percent.realised, shape=Age)) + 
    geom_point(size=6, alpha=0.85) + ylim(0,100) +
    labs(x="Tense", y="Percentage of final-/n/ realised", title="/n/-Production by tense")
}

ggsave(
  "Figure 4.png",
  plot_both(),
  width = 5.93,
  height = 4.53,
  dpi = 1200
)


### Figure 5
with(data.five,table(Genre, Speaker_code))

## Count nasal realization/deletion for each speaker group/genre
data.five.plot <- data.five %>%
  group_by(Genre, Speaker_code) %>%
  count(Nasal_drop)

## Spread realizations/deletions out so each speaker has one row
## and counts for nasal deletion each have their own column
data.five.plot <- spread(data.five.plot, Nasal_drop, n)

## Add a column for totals
data.five.plot$total <- data.five.plot$"0" + data.five.plot$n

## Add column for and compute percents as decimals
data.five.plot$dec.drop <- data.five.plot$"0"/data.five.plot$total
data.five.plot$dec.realised <- data.five.plot$n/data.five.plot$total

## Convert decimals to percentages
data.five.plot$percent.drop <- paste(round(data.five.plot$dec.drop*100,digits=1))
data.five.plot$percent.realised <- paste(round(data.five.plot$dec.realised*100,digits=1))

## Make sure percentage values are numeric
data.five.plot$percent.drop <- as.numeric(data.five.plot$percent.drop)
data.five.plot$percent.realised <- as.numeric(data.five.plot$percent.realised)

summary(data.five.plot)

#### 3.2 Point plots with ::ggplot ####

## Relabel for a more beautiful output
data.five.plot$Genre <- ifelse(data.five.plot$Genre=="Casual", "Formal narrative", ifelse(data.five.plot$Genre=="Kawa","Kawa oration","Sociolinguistic interview"))
data.five.plot$Genre <- as.factor(data.five.plot$Genre)
data.five.plot$Genre <- relevel(data.five.plot$Genre, "Kawa oration")

plot_both <- function()
{
  
  ggplot(data.five.plot, aes(x=Speaker_code, y=percent.realised, shape=Genre)) + 
    geom_point(size=4, alpha=0.85) + ylim(0,100) +
    labs(x="Speaker", y="Percentage of final-/n/ realised", title="/n/-Production by genre")
}

ggsave(
  "Figure 5.png",
  plot_both(),
  width = 5.93,
  height = 4.53,
  dpi = 1200
)

## Figure 6
Data.all.five.five.all <- read.delim("G:/My Drive/Research/Projects/Ende n-variation/Statistics Files/Data-all-five-five-all.txt")

## Count nasal realization/deletion for each speaker group/genre
data.all.five.plot <- Data.all.five.five.all %>%
  group_by(Age_1) %>%
  count(Nasal_drop)

## Spread realizations/deletions out so each speaker has one row
## and counts for nasal deletion each have their own column
data.all.five.plot <- spread(data.all.five.plot, Nasal_drop, n)

## Add a column for totals
data.all.five.plot$total <- data.all.five.plot$"0" + data.all.five.plot$n

## Add column for and compute percents as decimals
data.all.five.plot$dec.drop <- data.all.five.plot$"0"/data.all.five.plot$total
data.all.five.plot$dec.realised <- data.all.five.plot$n/data.all.five.plot$total

## Convert decimals to percentages
data.all.five.plot$percent.drop <- paste(round(data.all.five.plot$dec.drop*100,digits=1))
data.all.five.plot$percent.realised <- paste(round(data.all.five.plot$dec.realised*100,digits=1))

## Make sure percentage values are numeric
data.all.five.plot$percent.drop <- as.numeric(data.all.five.plot$percent.drop)
data.all.five.plot$percent.realised <- as.numeric(data.all.five.plot$percent.realised)

plot_both <- function()
{
  
  ggplot(data.all.five.plot, aes(x=Age_1, y=percent.realised)) + 
    geom_point(size=6, alpha=0.85) + ylim(0,100) + 
    geom_text(aes(label=percent.realised), vjust = -1.5, size=3.5) +
    labs(x="Age", y="Percentage of final-/n/ realised", title="/n/-Production by age")
}

ggsave(
  "Figure 6.png",
  plot_both(),
  width = 5.93,
  height = 4.53,
  dpi = 1200
)
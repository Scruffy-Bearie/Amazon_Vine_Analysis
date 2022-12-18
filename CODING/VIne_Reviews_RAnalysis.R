# USE THE LIBRARY FUNCTION TO LOAD REQUIRED PACKAGES
library(dplyr)
library(tidyverse)

# IMPORT AND READ REQUIRED CSV FILE AS DATAFRAME
vine_reviews <- read.csv('DATA/vine_reviews.csv')
head(vine_reviews)

# OBTAIN THE MEAN, MEDIAN, VARIANCE AND STANDARD DEVIATION OF THE DATAFRAMES "star_rating" COLUMN GROUPED BY VINE MEMBERSHIP
vine_summary <- vine_reviews %>% group_by(vine) %>% summarize(Mean=mean(star_rating), Median=median(star_rating), Variance=var(star_rating), SD=sd(star_rating), .groups = 'keep')
print(vine_summary)

# CONDUCT TWO-SAMPLE T-TEST TO COMPARE PAID RATINGS TO UNPAID RATINGS
t.test(subset(vine_reviews, vine == "Y")$star_rating, subset(vine_reviews, vine == "N")$star_rating)

# USE THE MANN-WHITNEY AND KRUSKAL WALLACE TESTS TO COMPARE POPULATIONS
wilcox.test(star_rating ~ vine, data = vine_reviews)
kruskal.test(star_rating ~ vine, data = vine_reviews)
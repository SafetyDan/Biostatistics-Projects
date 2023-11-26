###############################################################################
### File: Assignment2.R
### Course: PH 504 Biostatistics for Public Health
### Instructor: Theresa Andrasfay, Ph.D.
### Student Name: Danielle Bárcena
### Date:  11/12/2023
###############################################################################

###############################################################################
### Set Working Directory
###############################################################################
# It is important to always set the working directory at the beginning of a file
# so that R knows where to look to find data sets and where to save files we may output

# Create a folder called "Assignment2" in which you save Lastname_Firstname_Assignment2.R and
# the lipid.csv and malaria.csv data sets

###REDACTED LINE 18 - setwd("redacted")###

# If you do not know your filepath, go to Session > Set Working Directory > Choose Directory 
# and then click on the folder you created for Assignment1
# Once you've done that, setwd() with your filepath should show up in the console. 
# Paste that filepath above so when you return to this file in the future, you can just run this .R file

###############################################################################
### Load Required Packages & Data
###############################################################################
### Each time we start a new R session, we need to add the packages to our library that we intend
### to use during the session. This is like opening an app on your phone so it can run in the background.

# Today we will use tidyverse, which is a suite of packages that help simplify our code and make things tidy

library(tidyverse)  

# Since we used tidyverse in lab 1, it should already be installed. If it is not, 
# return to Lab 1 to see how to install a package. 

### Read in the lipid and malaria data sets
lipid <- read_csv("lipid.csv") #assigning the name 'lipid' to the dataset
malaria <- read_csv("malaria.csv") #assigning the name 'malaria' to the dataset

###############################################################################
### Questions 1 - 8 Summary Statistics of the lipid dataset
###############################################################################
# The lipid data set was collected as part of a study of adults living in Northern Ghana. 
# The goal of the study was to examine the relationships among sociodemographic, 
# anthropometric and behavioral factors and lipid levels in a rural African population. 
# Participants were recruited from February to October 2015 in the west and east 
# zones of the Navrongo Health and Demographic Surveillance System (NHDSS) area.

# Agongo, G., Nonterah, E. A., Debpuur, C., Amenga-Etego, L., Ali, S., Oduro, A., 
# Crowther, N., & Ramsay, M. as members of AWI-Gen and the H3Africa Consortium. (2018). 
# The burden of dyslipidaemia and factors associated with lipid levels among adults in rural northern Ghana: 
# An AWI-Gen sub-study. PloS one, 13(11), e0206326. https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0206326 

# This assignment will use the data set “lipid.csv”, which can be found on Canvas as part of the files for this assignment. 
# This is a modified version of the lipid data set that includes the following variables:
#   total_cholesterol: serum cholesterol measured in mmol/l 
#   sc_fat: abdominal subcutaneous fat tissue thickness measured in cm
#   bmi: Body Mass Index, calculated from measured height and weight
#   sex: self-reported sex as either Male or Female
#   age_category: age category of the respondent


### 1) What is the mean BMI? (Round to 2 decimal places)

mean(lipid$bmi)

#The mean BMI is 21.63

### 2) What is the standard deviation of BMI? (Round to 2 decimal places)

sd(lipid$bmi)

#The standard deviation of BMI is 3.61

### 3) What is the z score for a BMI of 30? (Round to 2 decimal places)

# z score is calculated using (value - mean of variable)/(sd of variable)

bmi_mean <- mean(lipid$bmi) #The mean of bmi in the lipid dataset is 21.63024

bmi_sd <- sd(lipid$bmi) #standard deviation of bmi in the lipid dataset is 3.61273

(30 - bmi_mean)/bmi_sd #The Z score for a BMI of 30 is 2.316741

### 4) What is the minimum value of total cholesterol?

min(lipid$total_cholesterol)

#The minimum value of total cholesterol in the lipid dataset is 0.33 mmol/l.

### 5) What is the maximum value of total cholesterol?

max(lipid$total_cholesterol)

#The maximum value of total cholesterol in the lipid dataset is 6.84 mmol/l.

### 6) What is the median of subcutaneous fat?

median(lipid$sc_fat)

#The median value of subcutaneous fat in the lipid dataset is 0.83 cm.

### 7) What is the 25th percentile or 1st quartile of subcutaneous fat? (Round to 2 decimal places)

summary(lipid$sc_fat)

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.2300  0.6200  0.8300  0.9729  1.1800  3.9100 

#The measurement of subcutaneous fat in the first quartile is 0.62 cm.

### 8) What is the 75th percentile or 3rd quartile of subcutaneous fat? (Round to 2 decimal places)

quantile(lipid$sc_fat) #trying it both ways

#0%  25%  50%  75% 100% 
#0.23 0.62 0.83 1.18 3.91

#In the third quartile, subcutaneous fat is at 1.18 cm.


###############################################################################
### Questions 9-10 Summary Statistics Table
###############################################################################

### 9) Create a formatted summary statistics table for the lipid data set
# Create a formatted summary statistics table in a word document 
# Remember to follow the guidelines for a summary statistics table,
# including an informative title, the sample size, informative column headers and variable names,
# and footnotes to provide additional details. 

# Include the appropriate summary statistics for each of the variables contained in the lipid dataset.
# You have already calculated some of the summary statistics above, but will need to do
# several additional calculations to complete the table.

head(lipid) #taking a look at the dataset to assess which statistics to pull

table(lipid$sex)
#Female   Male 
#993    845 

prop.table(table(lipid$sex))*100 #percentage of sample

#female 54% and male 46%

# For subcutaneous fat, display the median and interquartile range.

table(lipid$age_category)

#40-44 45-49 50-54 55-60 
#291   466   413   668

prop.table(table(lipid$age_category))*100

#40-44    45-49    50-54    55-60 
#15.83243 25.35365 22.47008 36.34385

# For the remaining numeric variables, you can display the mean (sd). 

sd(lipid$total_cholesterol) #0.9198838
sd(lipid$sc_fat) #0.500173
sd(lipid$bmi) #3.61273

mean(lipid$total_cholesterol) #3.229282
mean(lipid$sc_fat) #0.9728945
mean(lipid$bmi) #21.63024


# Save this as "Lastname_Firstname_SummaryTable.docx" or ".doc" and upload it to Canvas.



# 10) Write a paragraph describing the summary statistics table. 
# Be sure to include units of measurement where appropriate.
# Paste this into Canvas.


#Characteristics of the adults living in Northern Ghana represented in this study are presented in Table 2. The sample included a generally even distribution of male (46%) and female (54%) participants based on self-reported sex category. Participants in the sample belonged to age categories of 40 to 44 years (15.83%), 45 to 49 years (25.35%), 50 to 54 years (22.47%), or 55 to 60 years (36.34%), the latter being the age category with the greatest proportion of respondents. The adults sampled had an average body mass index (BMI) of 21.63 (SD = 3.61) as calculated from measured height and weight. The mean abdominal subcutaneous fat tissue thickness among participants was 0.97 centimeters (cm) (SD = 0.50) with a mean total cholesterol level of 3.23 millimoles per liter (mmol/L) (SD = 0.92). 

###############################################################################
### Questions 11-12: Line Plot
###############################################################################
# The dataset malaria.csv contains the reported cases of malaria per 100,000 population (rate)
# for each year from 1940 to 2015 in the United States

head(malaria) #taking a look at the dataset

# Create a lineplot displaying the year on the x axis and the 
# reported cases of malaria per 100,000 population on the y axis
# Include labels for both axes and an informative title.
# (See Lab 2 for an example with mortality rates over time)

plot(malaria$year, malaria$rate, type="line", #basic plot to start
  main = "U.S. Malaria rate per 100,000 population: 1940 and 2015", #title
  xlab = "Year", ylab = "Malaria rate per 100,000 population") #x and y labels

# After creating this graph, save it as an image and upload it to Canvas.

#q11-q12-line-plot.png

# Describe the trend in the rate of malaria in the United States.

#In the United States, the rate of malaria dipped until just after 1940s with a brief rise before 1950. This declined drastically around the 1950s to nearly 0 with a small surge a few years in, then a stable plateau after the 1950s through the 2000s.

###############################################################################
### Question 13: Probability and Screening
### (You choose whether you would like to calculate in R or by hand)
###############################################################################

# The National Institute for Occupational Safety and Health has developed a case 
# definition of carpal tunnel syndrome – an affliction of the wrist – that 
# incorporates three criteria: symptoms of nerve involvement, a history of occupational
# risk factors, and the presence of physical exam findings.
# The sensitivity of this definition as a diagnostic test for carpal tunnel syndrome is 0.67, and its specificity is 0.58.

###meeting the defined criteria is going to be referred to as "testing positive/negative" for these work notes

#sensitivity = 0.67
#specificity = 0.58

# In a population where the prevalence of carpal tunnel syndrome is 15%, what is the 
# probability that an individual has carpal tunnel, conditional on being classified as positive for carpal tunnel?
# In other words, among those who test positive for carpal tunnel with this case definition, what is the probability
# that they have carpal tunnel?

prevalence <- .15
sensitivity <- 0.67
specificity <- 0.58
population <- 100

expectedCTS <- population*prevalence #expected carpal tunnel syndrome
expectedCTS #15

expectedPositive <- expectedCTS*sensitivity
expectedPositive #About 10 people (10.05) of the expected rate to have carpal tunnel syndrome expected to test positive

restPop <- population - expectedCTS
restPop #Rest of population = 85 people 

trueNegative <- specificity*restPop
trueNegative #About 49 (49.3) people who do not have carpal tunnel syndrome will test negative

falsePositive <- ((population/100)-specificity)*restPop
falsePositive #About 36 (35.7) people who do not have carpal tunnel syndrome will test positive

totalPositive <- (expectedPositive+falsePositive)
totalPositive # About 46 (45.75) people will test positive regardless of the presence or absence of carpal tunnel syndrome


#proportion actually have carpal tunnel syndrome that test positive
propPositive <- expectedPositive/totalPositive
propPositive #0.2196721 of the population

#proportion do not have carpal tunnel syndrome that test negative

x <-(population-totalPositive)
x #54.25 after removing positive from sample

propNegative <- trueNegative/x
propNegative #0.9087558

#probability <- (prevalence*propPositive)/((prevalence*propPositive)+(restPop*propNegative))
#probability #0.0004263977

probability <- (population*prevalence*propPositive)/((population*prevalence*propPositive)+(population*(restPop/population)*propPositive))
probability #0.15

###############################################################################
### 14) Closing out your R session
###############################################################################
# This concludes assignment 2.

# Be sure to save the R script file as Lastname_Firstname_Assignment2.R (with your name) before closing down R.
# This is very important to have a record of your code so you can get credit for the assignment and come back to it in the future. 

# When you close R studio, you will get a prompt asking if you want to save the workspace image to your working directory.
# In other words, it wants to know if you want to save the objects in your environment for the next time you open up R. 
# This is not necessary because the R script file should have all the code to recreate these objects next time you run it.

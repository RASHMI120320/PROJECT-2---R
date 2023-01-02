#LOADING THE DATA
install.packages("readr")
library("readr")
school_data_1 <- read_csv("/Users/rashmi/Desktop/school_data_1.csv")
head(school_data_1)

install.packages("readstata13")
library("readstata13")
school_data_2 <- read.dta13("/Users/rashmi/Desktop/school_data_2.dta")
tail(school_data_2,n=8)

install.packages("openxlsx")
library("dplyr")
library("openxlsx")
school_data_3 <- read.xlsx("/Users/rashmi/Desktop/school_data_3.xlsx")
glimpse(school_data_3)

#MERGE DATASETS

# merge school_data_1 with school_data_2
school_data_merged<-merge( school_data_1,school_data_2,by="person_id")
# dimensions
dim(school_data_merged)
# Merge school_data_3 with school_data_merged
school_data_merged<-merge(school_data_merged,school_data_3,by=c("person_id","school_id"))
# summary statistics
summary(school_data_merged)

#TIDYING THE DATA
#Number of columns should be equal to the number of variables

ncol(school_data_merged)

install.packages("tidyr")
library("tidyr")

#MAKING DATA TIDY
school_data_tidy <- school_data_merged%>%
    pivot_longer(
      cols = starts_with("test_year"),
      names_to = "year",
      names_prefix = "test_year"
      names_transform = list(year = as.integer),
      values_to = "test_score",
    )

#ncol to get the number of columns of the new dataset

ncol(school_data_tidy)

#IDENTIFYING MISSING VALUES

install.packages("skimr")
library("skimr")

skim(school_data_tidy)

# We observe that parental_schooling and test_scores has some missing values

#Select rows with no missing values
school_data_selected <- filter(school_data_tidy, !is.na(parental_schooling),!is.na(test_score))

#Skim data again
 
skim(school_data_selected)


#MODIFYING THE DATA

#rename summer_camp to summer_school
analysisdata <- rename(school_data_selected, summerschool = summercamp)

#Use head to view the first 6 observations
head(analysisdata)
# Standardize test score
# Group analysisdata by year
analysisdata<-group_by(analysisdata,year)
# Create a new variable with mutate
analysisdata<-mutate(analysisdata, test_score=(test_score-mean(test_score))/sd(test_score))
# show mean of test_score
print(paste("Mean of test score:",mean(analysisdata$test_score)))

#show sd of test_score
print(paste("SD of test score:",sd(analysisdata$test_score))
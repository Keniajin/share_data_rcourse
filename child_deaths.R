#setwd("~/pwani_stats")
#setwd("/MYDATA/Reports/Pwani Stats/pwani2015")
setwd("/MYDATA/Reports/KWTRP_Intro_To_Stats/scripts_data")

# 1.0 Load packages
library(foreign)
library(epicalc)
library(epitools)
library(epiR)

# 2.0 Load the child deaths data
childdeaths <- read.table("child.deaths.csv", header=TRUE, sep=",")

use(childdeaths)
des()
codebook()

# 3.0 generate person-years
childdeaths$date_exit <- as.Date(childdeaths$date_exit, "%m/%d/%y")
childdeaths$date_birth <- as.Date(childdeaths$date_birth, "%m/%d/%y")

person_yrs<-difftime(childdeaths$date_exit,childdeaths$date_birth)
person_yrs<-(as.numeric(person_yrs))/365.25
label.var(person_yrs, "person years observed")
childdeaths<-data.frame(childdeaths,person_yrs)

# 4.0 Calculate rates
childdeaths$status<-as.numeric(childdeaths$status)
total_deaths<-length(childdeaths$status[childdeaths$status==2])
total_deaths

total_pyrs<-sum(childdeaths$person_yrs)
total_pyrs
mort_rate<-(as.numeric(total_deaths)/total_pyrs)
mort_rate

# 5.0 calculate standard error of the rate
s.e.Rate<-sqrt(total_deaths)/total_pyrs*1000 # 
s.e.Rate
s.e.log.Rate<-1/sqrt(total_deaths)
s.e.log.Rate

# 6.0 Compute 95% CI for rate
log.rate=log(mort_rate)
lower.bound=exp(log.rate-1.96*s.e.log.Rate)
lower.bound
upper.bound=exp(log.rate+1.96*s.e.log.Rate)
upper.bound


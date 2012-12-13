#########################################################
# Functions for cleaning data RE: the CLEAN portion of the SDP toolkit
# Author: Jared Knowles
#########################################################

# This first function tests for unique combinations of the id variable 
# (student ID usually), and some group variable that should be 
# stable and unique by student ID (like gender)
# It is a simple wrapper of R's unique function, but is relatively fast

testuniqueness<-function(id,group){
  length(unique(id))==length(unique(id,group))
} 

# Note the output is simply a TRUE/FALSE statement indicating whether there 
# are problems with the uniqueness of these two vectors. 
# To identify the cases where there are problems, we need another function.

# Optional optimization
# Using R's byte compiler
library(compiler)
testuniqueness2<-cmpfun(testuniqueness)

#########################
# Test the performance
##########################

# load benchmark
library(microbenchmark)
load("data/Student_Demographics_Raw.rda")

# Rename data
stuatt<-Student_Demographics_Raw
rm(Student_Demographics_Raw)

# Run the test
testuniqueness(stuatt$sid,stuatt$male)
testuniqueness2(stuatt$sid,stuatt$male)

results<-microbenchmark(testuniqueness(stuatt$sid,stuatt$male),
                        testuniqueness2(stuatt$sid,stuatt$male),times=5)

print(results)

# No speed up from the byte compiler for this function

###############################################################
# DATA.TABLE
#############################

# Load library
library(data.table)
# Set up the dataframe as a data.table (you can still use it like a dataframe)
stuatt2<-data.table(stuatt)
# Set the ID key you want to aggregate or group by
# You can unset this later
setkey(stuatt2,sid)

# Read in a mode function
source('statamode.R')

# Build a new data.table aggregating from the old
system.time(
sturow<-stuatt2[,list(nvals_gender=length(unique(male)),
                      gender_mode=statamode(male,method="stata"),
                      gender_recent=tail(male,1)),by=sid]
)
# Now test in plyr
library(plyr)

system.time(
sturow2<-ddply(stuatt,.(sid),summarize,nvals_gender=length(unique(male)),
               gender_mode=statamode(male,method="stata"),gender_recent=tail(male,1))
)

# Speed is much faster in the former case
# And, are the results the same?

identical(as.data.frame(sturow),sturow2)


# Yes!




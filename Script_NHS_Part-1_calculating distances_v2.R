# v2 March 13, 2019

# Script for calculating the distances between points. Part 1.
# This script is for use in the Numerical Histogram Score (NHS) algorithm. 
   # This is part 1 (distance calculations) of the scripts required to use the NHS algorithm. 
   # Part 2 is the script for calculating the NHS value. Part 2 is a separate script than Part 1.

# By David H. Nguyen, PhD, www.TSG-Lab.org

# NOTE: This script is best viewed in a plain text reader. 



###########
# Begining of ReadMe Info

# The script automates the calculation of distance between two points (x1,y1) and (x2,y2).
# More than that, it does so for sets of points. Example Group1 might have 500 points and Group2 might have 600 points. 
# It also creates names for each distance so that you know exactly which two points a distance was derived from. 
# Example: "Grp1-1 to Grp2-3" means "distance between item1-in-group1 to item3-in-group2"

# Sample Data Frame to Load

#   x.grp1  y.grp1    |   x.grp2  y.grp2
#     2       4       |   20      25
#     6       10      |   31      66
#     16      55      |   53      82


# Two Sample Results That Are Produced By This Script 
# (Result 1 will be side-by-side with Result 2, not stacked in the same columns.)
# This script will create a .csv file in your working directory called "sample_data_output_distances.csv"


# The resulting data will be structured like this:

#   who2who.1           point_1   |      who2who.2             point_2
#   Grp1-1 to Grp2-1    23        |      Grp1-2 to Grp2-1      45
#   Grp1-1 to Grp2-2    54        |      Grp1-2 to Grp2-2      24
#   Grp1-1 to Grp2-3    68        |      Grp1-2 to Grp2-3      84
#   Grp1-1 to Grp2-4    53        |      Grp1-2 to Grp2-4      11




         ###### HOW TO PREPARE YOUR DATA FILE FOR THIS SCRIPT ######

# It takes in a .csv file that contains four columns, each representing one coordinate from the above pair.
# The file should ONLY contain two groups, each with 1 or more points: Group1 vs Group2
# The two columns for Group1 MUST have the following header names: x.grp1, y.grp1
  # The order of the first two columns from left to right MUST be x-coordinate of Group1, then y-coordinate of Group1
# The two columns for Group2 MUST have the following header names: x.grp2, y.grp2 
  # The order of the 3rd column from left to right MUST be the x-coordinate of Group2, then the y-coordinate of Group2. 
# The contents of rows of all columns (1) must be numbers and (2) cannot be empty. 

# The sample data contains six columns, but the file is formatted as described above. 
  # So, only the first four columns (which are the first two groups) will be analyzed by this script. 

# End of ReadMe Info
############


##################################
##################################
##################################
# STEP 1

# Required packages
library(dplyr)
library(tidyr)

# Group1 are the points of interest, or target points, in a list of (x,y) points/coordinates
# Group2 are the neighboring points to which you will calculate the distance from each
# point in Group1. 
# For simplicity, only compare two groups at once.

df = read.csv("Sample Clusters All 3.csv")

# Change column names of neighbors to the format of x.grpN and y.grpN
df = rename(df, x.grp2 = x.blue) 
df = rename(df, y.grp2 = y.blue)
df = rename(df, x.grp1 = x.red)
df = rename(df, y.grp1 = y.red)



# This line filters out any rows that have blanks, so we get an 
# accurate count of items inside
grp1 = df %>% select(x.grp1, y.grp1) %>% filter(x.grp1 != "")



# This defines what points we are measuring distances to.
# "neighbors" should be a data frame that has a column called x.grp2 and 
# one called y.grp2, representing (x,y) coordinates

neighbors = df %>% select(x.grp2, y.grp2) %>% filter(x.grp2 != "")



###################################################
###################################################
###################################################
# STEP 2

# Create function that runs "rep(grp1$x.grp1[1],times=dim(neighbors)[1])".
# The purpose of this function is to take each item in the x.grp1 and 
# create a column that repeats that item up to the length of "neighbors",
# which is grp2.



ref.length = seq(1:length(which(!is.na(df$x.grp1))))

# The following is the x.grp1 only. Do for y.grp1 below. 

bowl_x = c()

bravo_x = function(n){
  i = n 
  xi = rep(grp1$x.grp1[n],times=dim(neighbors)[1])
  bowl_x = c(bowl_x, xi)
}


run.it_x = sapply(ref.length, bravo_x)
run.done_x = data.frame(run.it_x)

# Rename the headers of the run.done with x_1, x_2, etc.
numb.cols_x = ncol(run.done_x)

nombre_x = rep("x_", numb.cols_x)
numero_x = seq(1:numb.cols_x)
spanish_x = paste0(nombre_x,numero_x)
colnames(run.done_x) = c(spanish_x)



###################################################
###################################################

# Repeat the above for the y coordinate column 

bowl_y = c()

bravo_y = function(n){
  i = n 
  yi = rep(grp1$y.grp1[n],times=dim(neighbors)[1])
  bowl_y = c(bowl_y, yi)
}


run.it_y = sapply(ref.length, bravo_y)
run.done_y = data.frame(run.it_y)
numb.cols_y = ncol(run.done_y)

# Rename the headers of the run.done with x_1, x_2, etc.

nombre_y = rep("y_", numb.cols_y)
numero_y = seq(1:numb.cols_y)
spanish_y = paste0(nombre_y,numero_y)
colnames(run.done_y) <- c(spanish_y)


###################################################
###################################################
###################################################
# STEP 3

# Combine the above two columns with the data frame called "neighbors" to create a 
# 4-columned data frame on which the distance formula will be applied


hoorah = function(N){
  i = N
  voila_i = data.frame(run.done_x[,i], run.done_y[,i], neighbors[,1], neighbors[,2])
  return(voila_i)
}

standard = seq(1:length(ref.length))

show.me = lapply(standard, hoorah)


###################################################
###################################################
###################################################
# STEP 4

# APPLY DISTANCE FORMULA TO EACH ROW OF EACH DATA FRAME.



# Create a function that does the distance formula. Example: (x1,y1) to (x2,y2)
distance_form = function(x1, x2, y1, y2){ 
  sqrt((x1 - x2)^2 + (y1 - y2)^2) 
}


# These lines apply the distance formulate to each row of each data frame 
# contained in the list called "show.me"

scribble.ref= seq(1:length(show.me))

scribble.funk = function(N){
  i = N
  create.em = mapply(distance_form, show.me[[i]][1], show.me[[i]][3], 
                     show.me[[i]][2], show.me[[i]][4])
}

create.em = lapply(scribble.ref, scribble.funk)

# Rename each data frame in the list called "create.em" to be "df_1", "df_2", etc. 
fries = length(create.em)
prefix = rep("df_", times = fries)
suffix = seq(1:fries)
crowns = paste0(prefix,suffix)
names(create.em) <- c(crowns)




###################################################
###################################################
###################################################
# STEP 5

# CREATE A COLUMN THAT IDENTIFIES WHICH TWO POINTS 
# EACH DISTANCE MEASURE REPRESENTS 
# Example: "Grp1-1 to Grp2-3" means "distance between item1-in-group1 to item3-in-group2"


how.long = length(ref.length)

highfive = rep("Grp1-", times = how.long)
lowfive = seq(1:how.long)
greeting = paste0(highfive,lowfive)



# this for loop creates a table of commands that need to be parsed-evaluated
container = c()
for (i in greeting){
  phrase = paste("rep('", i, "'", ", times=dim(neighbors)[1])")
  container = c(container, phrase)
}


# this function parses and evaluates its input
yahoo = function(input){
  eval(parse(text=input))
}

# this line applies the "yahoo" function to all items in "container".
# The output is a list called "boom"
boom = sapply(container, yahoo)

# This turns boom into a data frame
boom = as.data.frame(boom)




# Create a list of prefixes that contains a prefix for each item in Grp2


# this is a counter that will iterated across 
roster = c(1:dim(neighbors)[1])



# this is a container to which things will be added
the.suffix = c()

# this for loop creates repeated lines that differ by a number
for (i in roster){
  phrase = paste0("to Grp2-", i)
  the.suffix = c(the.suffix, phrase)
}


# Combine the each prefix ("Grp1-1") with its corresponding 
# same-row suffix ("to Grp2-1")  

counter = c(1:length(ref.length))


cobalt = function (i){ 
  data.frame(boom[,i], the.suffix)
}

dogtags = lapply(counter, cobalt)


# Separate each data frame in dogtags and make it an distinct object

sep.df.box = c()

for (i in 1:length(dogtags)){
  stuff = paste0("sep.df_",i," = as.data.frame(dogtags[[",i,"]])")
  sep.df.box = c(sep.df.box, stuff)
}

for (i in sep.df.box){
  eval(parse(text=i))
}


#####

holder = c()

for (i in 1:length(dogtags)){
  thing = paste0("unite(sep.df_", i, ", 'who2who', c(boom...i. , the.suffix), sep='')")
  holder = c(holder, thing)
}


run.line = function(input){
  eval(parse(text=input))
}

bigbowl = lapply(holder, run.line)

bigbowl = as.data.frame(bigbowl)

# This renames the column names in bigbowl to "who2who_X" (X=integer)
nemo.1 = rep("who2who_", times = length(dogtags))
nemo.2 = seq(1:length(dogtags))
name.new = paste0(nemo.1, nemo.2)

names(bigbowl) = c(name.new)




###################################################
###################################################
###################################################
# STEP 6

# COMBINE THE ROW NAMES WITH THEIR CORRESPONDING DISTANCE MEASURES

# Examples:
# "bigbowl[1]" should be column-bound to "create.em$df_1"
# "bigbowl[2]" should be column-bound to "create.em$df_2"

# The following lines (from "ruler" to "voila") combine each item in "bigbowl" (from STEP 5) 
# with it's corresponding item in "create.em" (from STEP 4)


# This function prints repeating commands to create unique data frames
print.sauron = function(i){
  paste0("data.frame(bigbowl$who2who_", i, ", create.em$df_", i, ")")
}

# This is a counter that a function will iterate over
ruler = seq(1:length(ref.length))

# This creates a list of commands that are ready to be executed
written.sauron = lapply(ruler, print.sauron)

# This function parses and then evaluates each command
gandalf = function(i){
  eval(parse(text=i))
}

# This gives you a list called "voila" that contains your final results!
voila = lapply(written.sauron, gandalf)


# Give each data frame in the list called "voila" a unique name: point_X
prefix = rep("point", times = length(voila))
suffix = seq(1:length(voila))
jewels = paste0(prefix, sep="_", suffix)

# turn the voila list into a data frame called magic
magic = as.data.frame(voila)


# this is a sequence of even numbers
rod = seq(2, dim(magic)[2], by = 2)

# rename the headers for columns that contain distances in the data frame called magic
names(magic)[rod] = c(jewels)


# this is a sequence of odd numbers
ruler.odd = seq(1, dim(magic)[2], by = 2)

# This creates simpler names for the the odd numbered columns: who2who_X
block.1 = rep("who2who_", times = length(ruler.odd))
block.2 = seq(1:length(ruler.odd))
notebook = paste0(block.1, block.2)

# rename the headers for columns that contain relationship in the data frame called magic
names(magic)[ruler.odd] = c(notebook)


# This line saves the data frame called "magic" as a .csv file 
# in your working directory. The name of the file is in the quotation marks.
write.csv(magic, "sample_data_output_distances.csv", row.names = F)





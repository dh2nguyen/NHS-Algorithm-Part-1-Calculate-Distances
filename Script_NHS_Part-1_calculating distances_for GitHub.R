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

#   x.grp1  y.grp1      x.grp2  y.grp2
#     2       4           20      25
#     6       10          31      66
#     16      55          53      82


# Two Sample Results That Are Produced By This Script 
# (Result 1 will be side-by-side with Result 2, not stacked in the same columns.)
# This script will create a .csv file in your working directory called "sample_data_output"


# Result 1

#       who2who           Group1.1
#   Grp1-1 to Grp2-1        23
#   Grp1-1 to Grp2-2        54
#   Grp1-1 to Grp2-3        68
#   Grp1-1 to Grp2-4        53


# Result 2

#       who2who           Group1.2
#   Grp1-2 to Grp2-1        45
#   Grp1-2 to Grp2-2        24
#   Grp1-2 to Grp2-3        84
#   Grp1-2 to Grp2-4        11




         ###### HOW TO PREPARE YOUR DATA FILE FOR THIS SCRIPT ######

# It takes in a .csv file that contains four columns, each representing one coordinate from the above pair.
# The file should ONLY contain two groups, each with 1 or more points: Group1 vs Group2
# The two columns for Group1 MUST have the following header names: x.grp1, y.grp1
  # The order of the first two columns from left to right MUST be x-coordinate of Group1, then y-coordinate of Group1
# The two columns for Group2 MUST have the following header names: x.grp2, y.grp2 
  # The order of the 3rd column from left to right MUST be the x-coordinate of Group2, then the y-coordinate of Group2. 
# The contents of rows of all columns (1) must be numbers and (2) cannot be empty. 

# This script will create a .csv file in your working directory called "sample_data_output"


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


# Load the .csv file
df = read.csv("Sample Clusters.csv")


# This line isolates the x and y coordinate columns of Group1.
# This line also filters out any rows that have blanks, so we get an 
# accurate count of items inside (sometimes your file has empty rows underneath the filled rows)
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


ref.length = seq(1:dim(neighbors)[1])

# The following is the x.grp1 only. The y.grp1 is next. 

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
colnames(run.done_x) <- c(spanish_x)
run.done_x


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

standard = seq(1:dim(neighbors)[1])

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


how.long = length(create.em)

highfive = rep("Grp1-", times = how.long)
lowfive = seq(1:how.long)
greeting = paste0(highfive,lowfive)


# this for loop creates a table of commands that need to be parsed-evaluated
container = c()

for (i in greeting){
  phrase = paste("name1=rep(","'", i, "'", ", times=length(greeting))")
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


# renames columns of "boom" with cleaner integers to simplify headers
colnames(boom) = c(1:dim(boom)[2])



# Create a list of prefixes that contains a prefix for each item in Grp2

# this is a counter that will iterated across 
roster = c(1:length(boom))

# this is a container to which things will be added
the.suffix = c()

# this for loop creates repeated lines that differ by a number
for (i in roster){
  phrase = paste0("to Grp2-", i)
  the.suffix = c(the.suffix, phrase)
}




# Combine the each prefix ("Grp1-1") with its corresponding 
# same-row suffix ("to Grp2-1")  

counter = seq(1:length(boom))

name.them = function(N){
  i = N
  id.tags_i = data.frame(boom[,i], the.suffix)
}

dogtags = lapply(counter, name.them)


# Separate each data frame in dogtags and make it an distinct object

prefix = rep("Grp1-", times = length(dogtags))
suffix = seq(1:10)
titles = paste0(prefix, suffix)

names(dogtags) = titles


# This for loop turns each item in dogtags into a separate data frame 
# named "Group1.1", "Group1.2",etc., so that they can each be manipulated separately

for(i in 1:length(dogtags)){
  nam = paste("Group1", i, sep = ".")
  assign(nam, dogtags[[i]])
}  
  
#####
marks = length(dogtags)

holder = c()

for (i in 1:marks){
  thing = paste0("unite(Group1.", i, ", 'who2who', c(boom...i. , the.suffix), sep='')")
  holder = c(holder, thing)
}

run.line = function(input){
  eval(parse(text=input))
}

bigbowl = lapply(holder, run.line)


# create string of names: bb_1, bb_2, etc.
prefix = rep("bb_", times = dim(bigbowl[[1]])[1])
suffix = seq(1:dim(bigbowl[[1]])[1])
tiara = paste0(prefix, suffix)

# rename items in list called "bigbowl" using names in "tiara"
names(bigbowl) = c(tiara)


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
  paste0("data.frame(bigbowl$bb_", i, ", create.em$df_", i, ")")
}

# This is a counter that a function will iterate over
ruler = seq(1:10)

# This creates a list of commands that are ready to be executed
written.sauron = lapply(ruler, print.sauron)

# This function parses and then evaluates each command
gandalf = function(input){
  eval(parse(text=input))
}

# This gives you a list called "voila" that contains your final results!
voila = lapply(written.sauron, gandalf)


# Give each data frame in the list called "voila" a unique name
prefix = rep("result", times = dim(voila[[1]])[1])
suffix = seq(1:dim(voila[[1]])[1])
jewels = paste0(prefix, sep="_", suffix)


names(voila) = c(jewels)


# turn the voila list into a data frame called magic
magic = as.data.frame(voila)

# rename the column headers in the data frame called magic

rod = seq(2, dim(magic)[2], by = 2)

names(magic)[rod] = c(jewels)


# This line saves the data frame called "magic" as a .csv file 
# in your working directory. The name of the file is in the quotation marks.
write.csv(magic, "sample_data_output.csv", row.names = FALSE)

print("Done! Check your working director for a new file called 'sample_data_output'.")




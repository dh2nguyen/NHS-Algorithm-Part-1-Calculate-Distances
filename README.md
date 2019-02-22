# NHS-Algorithm-Part-1-Calculate-Distances
This is the R script that calculates distances between two sets of (x,y) coordinates/points: Group1 vs Group2.

# NOTE: This script is best viewed in a plain text reader. 


# Script for calculating the distances between points. Part 1.
# This script is for use in the Numerical Histogram Score (NHS) algorithm. 
   # This is part 1 (distance calculations) of the scripts required to use the NHS algorithm. 
   # Part 2 is the script for calculating the NHS value. Part 2 is a separate script than Part 1.

# By David H. Nguyen, PhD, www.TSG-Lab.org




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

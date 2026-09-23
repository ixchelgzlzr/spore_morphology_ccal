############################################################
# Script 1 - Calculate new metrics and Visualize the data
#############################################################

# set up the libraries
library(readxl)

# Read the data
data <- read_excel("data/data_clean.xlsx", skip = 2, col_names = T)


# there are multiple measurements of skirt width so calculate the mean skirt width
# first separate the widths
data <- data %>% separate(Skirt_width, into = c("sw1", "sw2", "sw3"), sep = ",", convert = T) 
# now calculate means
data <- data %>% mutate(skirt_width_mean = rowMeans(pick(sw1, sw2, sw3), na.rm = TRUE), .keep = "unused")

# lets add a roundness score

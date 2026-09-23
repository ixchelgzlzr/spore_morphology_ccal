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

# perhaps what matters is the proportional length of skirt in relation to the whole diameter
data$skirt_width_prop <- data$skirt_width_mean/(data$Longest_diameter + data$Perpendicular_to_longest_diameter)/2

# lets roundness as the longest diameter divided by the perpendicular diameter
# compute
data$roundness <- data$Longest_diameter/data$Perpendicular_to_longest_diameter

# calculate spoe depth
data$spore_depth <- data$`Skirt-point_distance` + data$`Skirt-back_distance`

# check the difference between the anterior and the posterior depth
data$ant_post_depth <- data$`Skirt-point_distance` / data$`Skirt-back_distance`


#-----------------
# plot distributions

# Let's all the continuous variables 
cont_vars <- c("Longest_diameter", "Perpendicular_to_longest_diameter", "Skirt-point_distance", 
               "Skirt-back_distance", "skirt_width_prop", "spore_depth", "ant_post_depth")
con_data  <- data[ , cont_vars]

hist_plots <- vector(mode = "list", length = length(con_data))

# for each continuous variable
for (i in 1:ncol(con_data)){
  
  var           <- colnames(con_data[i])
  this_var      <- unlist(con_data[ , i])
  hist_plots[[i]] <- hist(this_var, main = var, xlab = var)
  
}


# plot all 
# number of continuous variables
n <- ncol(con_data)

# choose number of rows/columns automatically
ncols <- ceiling(sqrt(n))
nrows <- ceiling(n / ncols)

# put all histograms in one figure
par(mfrow = c(nrows, ncols))

for (i in 1:length(hist_plots)){
  
  plot(hist_plots[[i]])
  
}


# reset plotting layout afterward
par(mfrow = c(1, 1))

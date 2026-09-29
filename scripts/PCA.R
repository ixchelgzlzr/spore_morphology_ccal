#########
# PCA  #
########

#install.packages("Hmisc")
#install.packages("corrplot")
#install.packages("GGally")
#install.packages("factoextra")

# libraries
library(Hmisc)
library(corrplot)
library(GGally)
library(factoextra)

# source the data exploration script
source("scripts/data_exploration.R")

# Let's see correlations between continuous data
ggpairs(con_data) + theme_minimal()

# Let's add information about locality to the continuous data
con_data$region <- data$Population

# remove rows with NAs
cont_data_noNA <- na.omit(con_data)

# Run PCA
pca <- prcomp(cont_data_noNA[ , 1:(ncol(cont_data_noNA)-1)], scale. = T)

# view results
summary(pca)

# Visualize the variance explained by each PC
fviz_eig(pca, addlabels = TRUE)

# Biplot
# Extract PCA coordinates
pca_scores <- as.data.frame(pca$x[, 1:2])

# Add population
pca_scores$region <- cont_data_noNA$region

ggplot(pca_scores, aes(x = PC1,y = PC2, colour = region)) +
  stat_ellipse(aes(fill = region), geom = "polygon", alpha = 0.10, colour = NA, level = 0.95) +
  stat_ellipse(linewidth = 0.8, level = 0.95) +
  geom_point(size = 2.5, alpha = 0.8) +
  theme_classic() +
  labs(x = "PC1 (45.9%)", y = "PC2 (25.6%)", colour = "region", fill = "region") +
  theme(legend.position = "right", legend.title = element_blank())




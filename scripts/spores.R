##########################################################
# Let's start visualizing and processing the spore data #
##########################################################
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(factoextra)
library(vegan)
library(cluster)
library(viridis)
library(ggExtra)


# Read the data
data <- read_excel("data/data_clean.xlsx", skip = 2, col_names = T)

# look at the distribution of spore sizes
hist(data$Longest_diameter, xlab = "Spore size (micrometers)", main = "Longest spore diameter", col = "#1F9E89FF")
hist(data$Perpendicular_to_longest_diameter, xlab = "Spore size (micrometers) ", main = "Perpendicular diameter", col = "#1F9E89FF")

# scatter
p <- ggplot(data=data, aes(x = Longest_diameter, y = Perpendicular_to_longest_diameter, colour = Population)) +
  geom_point(size = 2.5) +
  scale_color_viridis(discrete = T, option = "C") +
  theme_bw() + 
  theme(legend.position = "bottom") + xlab("Longest diameter") + ylab("Perpendicular to longest diameter")

p

# Add scatter plots
ggMarginal(p,
           type = "histogram",
           groupColour = TRUE,
           groupFill = TRUE,
           bins = 15,
           alpha = 0.5)


# save image of spore size
pdf("plots/spore_proximal_dimensions.pdf")

ggMarginal(p,
           type = "histogram",
           groupColour = TRUE,
           groupFill = TRUE,
           bins = 15,
           alpha = 0.5)
dev.off()


# Check the distribution of a few variables

# longest diameter
L_diam <- ggplot(data, aes(x = Longest_diameter, y = Population, fill = Population)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_bw() + xlab("Longest diameter") + title(main = "Longest diameter")

L_diam

# roundness: longest diam/perpendicular diam
# compute
data$roundness <- data$Longest_diameter/data$Perpendicular_to_longest_diameter
#plot
roundness.plot <- ggplot(data, aes(x = roundness, y = Population, fill = Population)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_bw() + xlab("Roundness") + title(main = "Roundness")

roundness.plot

# skirt-back
skirt.back.plot <- ggplot(data, aes(x = `Skirt-point_distance`, y = Population, fill = Population)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_bw() + xlab("`Skirt-back_distance`") + title(main = "`Skirt-back_distance`")
skirt.back.plot


# skirt-tip
skirt.tip.plot <- ggplot(data, aes(x = `Skirt-point_distance`, y = Population, fill = Population)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_bw() + xlab("`Skirt-point_distance`") + title(main = "`Skirt-point_distance`")
skirt.tip.plot


# compute addition
data$tip_to_back = data$`Skirt-point_distance` + data$`Skirt-back_distance`

tip_to_back.plot <- ggplot(data, aes(x = `Skirt-point_distance`, y = Population, fill = Population)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_bw() + xlab("tip_to_back") + title(main = "tip_to_back")
tip_to_back.plot





ggplot(data, aes(x = skirt_width_mean, y = Population, colour = Population)) +
  geom_jitter(height = 0.15, width = 0, size = 2.5) +
  theme_bw()


ggplot(data, aes(x = skirt_width_mean, y = Population, fill = Population)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_bw() + xlab("skirt_width_mean") + title(main = "skirt_width_mean")


# Is skirt width correlated with spore size?
plot(data$Longest_diameter, data$skirt_width_mean)

# so let's divide skirt by spore size
data$skirt_prop <- data$skirt_width_mean/data$Longest_diameter
ggplot(data, aes(x = skirt_prop, y = Population, fill = Population)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_bw() + xlab("skirt_prop") + title(main = "skirt_prop")

# Is spore depth correlated with size
plot(data$Longest_diameter, data$tip_to_back)

plot(data$`Skirt-point_distance`, data$`Skirt-back_distance`)

hist(data$`Skirt-point_distance`/data$`Skirt-back_distance`)

# let's do a PCA with all the numeric variables
# define numeric variables
num_vars <- c("Longest_diameter", "roundness", "Skirt-back_distance", "skirt_width_mean")

# keep only numeric variables
PCA_data <- data[ , c("Population", "Samples", num_vars)]

# remove rows with NAs
PCA_data_clean <- na.omit(PCA_data)

# Run PCA
pca <- prcomp(PCA_data_clean[ , -(1:2)], scale. = T)

summary(pca)

# Visualize the variance explained by each PC
fviz_eig(pca, addlabels = TRUE)

# Biplot
# Extract PCA coordinates
pca_scores <- as.data.frame(pca$x[, 1:2])

# Add population
pca_scores$Population <- PCA_data_clean$Population

ggplot(pca_scores, aes(x = PC1,y = PC2, colour = Population)) +
  stat_ellipse(aes(fill = Population), geom = "polygon", alpha = 0.10, colour = NA, level = 0.95) +
  stat_ellipse(linewidth = 0.8, level = 0.95) +
  geom_point(size = 2.5, alpha = 0.8) +
  theme_classic() +
  labs(x = "PC1 (45.9%)", y = "PC2 (25.6%)", colour = "Population", fill = "Population") +
  theme(legend.position = "right", legend.title = element_blank())




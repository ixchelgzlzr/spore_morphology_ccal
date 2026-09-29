############################################
# NMDS- so we can include categorical traits
############################################

#Ciara: don't forget to install packages


library(cluster)
library(vegan)
library(plotly)


# Read the data
source("scripts/data_exploration.R")

# Let's all the continuous variables 
cont_vars <- c("Longest_diameter", "Perpendicular_to_longest_diameter", "Skirt-point_distance", 
               "Skirt-back_distance", "skirt_width_mean", "spore_depth", "ant_post_depth")

# NMDS data
nmds_vars <- c("PDO_Ridge_Depth", "PDP_Areolation", "P-D_Sculpturing_Similarity", "Proximal_face_ornamentation")
nmds_data <- data[ , c("Population", cont_vars, nmds_vars)]


# remove rows with NAs
nmds_data <- na.omit(nmds_data)

# Make sure categorical traits are factors
nmds_data$PDO_Ridge_Depth                <- as.factor(nmds_data$PDO_Ridge_Depth)
nmds_data$PDP_Areolation                 <- as.factor(nmds_data$PDP_Areolation)
nmds_data$`P-D_Sculpturing_Similarity`   <- as.factor(nmds_data$`P-D_Sculpturing_Similarity`)
nmds_data$Proximal_face_ornamentation    <- as.factor(nmds_data$Proximal_face_ornamentation)


# Gower distance
gower_dist <- daisy(nmds_data[, -1], metric = "gower")

# NMDS
set.seed(123)

nmds <- metaMDS(gower_dist, k = 3, trymax = 200, autotransform = FALSE)

# Check stress
# Stress is a metric about how well the ordinarion represents the data
# The closest to zero the better
nmds$stress

# Let's extract the scores 
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))
# And add a opulation column for plotting 
nmds_scores$Population <- nmds_data$Population

# Let's visualize the first two components
ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, colour = Population)) +
  stat_ellipse(aes(fill = Population), geom = "polygon", alpha = 0.10, colour = NA, level = 0.95) +
  stat_ellipse(linewidth = 0.8, level = 0.95) +
  geom_point(size = 2.5, alpha = 0.8) +
  theme_classic() +
  labs(x = "NMDS1", y = "NMDS2", colour = "Population", fill = "Population") +
  theme(legend.title = element_blank())


# Let's now make a 3D plot

# Interactive 3D plot
plot_ly(data = nmds_scores, x = ~NMDS1, y = ~NMDS2, z = ~NMDS3, color = ~Population,
        type = "scatter3d",
        mode = "markers",
        marker = list(size = 5, opacity = 0.8)) %>%
  layout(scene = list(xaxis = list(title = "NMDS1"), yaxis = list(title = "NMDS2"), zaxis = list(title = "NMDS3")),
    legend = list(title = list(text = "Population")))


# Now we do some statistical tests to assign probabilities to this ordination
# We take our distance matrix 
# PERMANOVA
adonis2(gower_dist ~ Population,
        data = nmds_data,
        permutations = 9999)

# Test homogeneity of multivariate dispersion
disp <- betadisper(gower_dist, nmds_data$Population)

anova(disp)
permutest(disp, permutations = 9999)


# Now let's do between populations comparisons to see
# which population differs specifically from which one
pops <- unique(nmds_data$Population)

pairs <- combn(pops, 2, simplify = FALSE)

pairwise_results <- lapply(pairs, function(x) {
  
  # subset the data
  dat_sub <- nmds_data[nmds_data$Population %in% x, ]
  
  # recalculate Gower distance for this subset
  dist_sub <- daisy(dat_sub[, -1], metric = "gower")
  
  # pairwise PERMANOVA
  fit <- adonis2(dist_sub ~ Population,
                 data = dat_sub,
                 permutations = 9999)
  
  data.frame(
    Pop1 = x[1],
    Pop2 = x[2],
    F = fit$F[1],
    R2 = fit$R2[1],
    p = fit$`Pr(>F)`[1]
  )
})

pairwise_results <- do.call(rbind, pairwise_results)

# Correct for multiple comparisons
pairwise_results$p_adj <- p.adjust(pairwise_results$p,
                                   method = "BH")

pairwise_results




# Visuallize each variable by locality

ggplot(nmds_data,
       aes(x = Population,
           y = PDO_Ridge_Depth,
           fill = PDP_Areolation), drop = F) +
  geom_violin() +
  geom_boxplot(width = 0.15) +
  theme_bw()



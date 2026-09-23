






############################################
# NMDS- so we can include categorical traits
############################################

# NMDS data
nmds_vars <- c("PDO_Ridge_Depth", "PDP_Areolation", "P-D_Sculpturing_Similarity", "Proximal_face_ornamentation")
nmds_data <- data[ , c("Population", num_vars, nmds_vars)]


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
nmds$stress

nmds_scores <- as.data.frame(scores(nmds, display = "sites"))

nmds_scores$Population <- nmds_data$Population


ggplot(
  nmds_scores,
  aes(
    x = NMDS1,
    y = NMDS2,
    colour = Population
  )
) +
  stat_ellipse(
    aes(fill = Population),
    geom = "polygon",
    alpha = 0.10,
    colour = NA,
    level = 0.95
  ) +
  stat_ellipse(
    linewidth = 0.8,
    level = 0.95
  ) +
  geom_point(
    size = 2.5,
    alpha = 0.8
  ) +
  theme_classic() +
  labs(
    x = "NMDS1",
    y = "NMDS2",
    colour = "Population",
    fill = "Population"
  ) +
  theme(
    legend.title = element_blank()
  )



library(plotly)

# Extract the 3D NMDS coordinates
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))

# Add population
nmds_scores$Population <- PCA_data_clean$Population

# Interactive 3D plot
plot_ly(
  data = nmds_scores,
  x = ~NMDS1,
  y = ~NMDS2,
  z = ~NMDS3,
  color = ~Population,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 5,
    opacity = 0.8
  )
) %>%
  layout(
    scene = list(
      xaxis = list(title = "NMDS1"),
      yaxis = list(title = "NMDS2"),
      zaxis = list(title = "NMDS3")
    ),
    legend = list(title = list(text = "Population"))
  )


adonis2(gower_dist ~ Population,
        data = PCA_data_clean,
        permutations = 9999)

disp <- betadisper(gower_dist, PCA_data_clean$Population)

anova(disp)
permutest(disp, permutations = 9999)

boxplot(disp)


library(vegan)

pops <- unique(PCA_data_clean$Population)
pairs <- combn(pops, 2, simplify = FALSE)

pairwise_results <- lapply(pairs, function(pair) {
  
  keep <- PCA_data_clean$Population %in% pair
  
  dat_sub <- droplevels(PCA_data_clean[keep, ])
  
  # subset original Gower distance matrix
  dist_sub <- as.dist(as.matrix(gower_dist)[keep, keep])
  
  result <- adonis2(
    dist_sub ~ Population,
    data = dat_sub,
    permutations = 9999
  )
  
  data.frame(
    Pop1 = pair[1],
    Pop2 = pair[2],
    F = result$F[1],
    R2 = result$R2[1],
    p = result$`Pr(>F)`[1]
  )
})

pairwise_results <- do.call(rbind, pairwise_results)

# Correct for multiple comparisons
pairwise_results$p_adj <- p.adjust(
  pairwise_results$p,
  method = "BH"
)

# Sort from strongest to weakest evidence
pairwise_results <- pairwise_results[
  order(pairwise_results$p_adj),
]

pairwise_results


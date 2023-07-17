######################################################################################################################################################
# Carrying Capacities in Space #######################################################################################################################
######################################################################################################################################################
# Weber Manuel #######################################################################################################################################
# July 2023 ##########################################################################################################################################
###### 1. GENERATE GROWTH FORM MAPS ######

# Load required libraries
library(terra)
library(ranger)
library(sf)

# Reading in the data
plots <- read.csv("Plots_2023.csv")
sentinel_raster <- rast("Rasters/Sentinel predictors.tif")
onguma <- read_sf("Onguma shapefile/Onguma_EPSG_4326.shp")[1]

### Grass ----

# Step 1: Prepare the data

# We have the proportion of grass cover in 58 spots as a vector
grass_cover <- plots$Grasses....

# Extract the pixel values of the sentinel bands for the chosen locations
chosen_locations <- data.frame(x = plots$Longitude, y = plots$Latitude)
sentinel_data <- extract(sentinel_raster, chosen_locations, ID = F)

# Combine the grass cover and Sentinel-2 data into one dataframe
df <- data.frame(grass_cover, sentinel_data)

# Step 2: Train the random forest model

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(df), nrow(df) * 0.8)  # 80% for training
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Train the random forest model
rf_model <- ranger(grass_cover ~ ., data = train_data, num.trees = 500)

# Create prediction raster, cropped to the area of interest
prediction <- predict(sentinel_raster, rf_model, na.rm = T)$predictions
prediction <- mask(crop(prediction, onguma), onguma)
plot(prediction)
writeRaster(prediction, "Rasters/grass cover.tif")

# Extract the test data and actual grass cover values
test_input <- test_data[, -1]  # Remove the grass_cover column
actual_grass_cover <- test_data$grass_cover

# Predict grass cover for the test set using the trained model
predicted_grass_cover <- predict(rf_model, data = test_input)$predictions

# Evaluate the performance of the model
accuracy <- mean(predicted_grass_cover == actual_grass_cover)
print(paste("Accuracy:", accuracy))

# Plot the predicted vs actual grass cover for the test set
plot(predicted_grass_cover, actual_grass_cover, xlab = "Predicted", ylab = "Actual")


### Trees ----

# Step 1: Prepare the data

# We have the proportion of grass cover in 58 spots as a vector
tree_cover <- plots$Canopy......woody.

# Extract the pixel values of the sentinel bands for the chosen locations
chosen_locations <- data.frame(x = plots$Longitude, y = plots$Latitude)
sentinel_data <- extract(sentinel_raster, chosen_locations, ID = F)

# Combine the grass cover and Sentinel-2 data into one dataframe
df <- data.frame(tree_cover, sentinel_data)

# Step 2: Train the random forest model

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(df), nrow(df) * 0.8)  # 80% for training
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Train the random forest model
rf_model <- ranger(tree_cover ~ ., data = train_data, num.trees = 500)

prediction <- predict(sentinel_raster, rf_model, na.rm = T)$predictions
prediction <- mask(crop(prediction, onguma), onguma)
plot(prediction)
writeRaster(prediction, "Rasters/tree cover.tif")

# Extract the test data and actual tree cover values
test_input <- test_data[, -1]  # Remove the tree_cover column
actual_tree_cover <- test_data$tree_cover

# Predict tree cover for the test set using the trained model
predicted_tree_cover <- predict(rf_model, data = test_input)$predictions

# Evaluate the performance of the model
accuracy <- mean(predicted_tree_cover == actual_tree_cover)
print(paste("Accuracy:", accuracy))

# Plot the predicted vs actual tree cover for the test set
plot(predicted_tree_cover, actual_tree_cover, xlab = "Predicted", ylab = "Actual")

### Shrubs ----

# Step 1: Prepare the data

# Assuming you have the proportion of grass cover in 58 spots as a vector
shrub_cover <- plots$Shrubs......woody...proxy.for.encroachment.

# Extract the pixel values of the sentinel bands for the chosen locations
chosen_locations <- data.frame(x = plots$Longitude, y = plots$Latitude)
sentinel_data <- extract(sentinel_raster, chosen_locations, ID = F)

# Combine the grass cover and Sentinel-2 data into one dataframe
df <- data.frame(shrub_cover, sentinel_data)

# Step 2: Train the random forest model

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(df), nrow(df) * 0.8)  # 80% for training
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Train the random forest model
rf_model <- ranger(shrub_cover ~ ., data = train_data, num.trees = 500)

prediction <- predict(sentinel_raster, rf_model, na.rm = T)$predictions
prediction <- mask(crop(prediction, onguma), onguma)
plot(prediction)
writeRaster(prediction, "Rasters/shrub cover 6-7.tif")

# Extract the test data and actual shrub cover values
test_input <- test_data[, -1]  # Remove the shrub_cover column
actual_shrub_cover <- test_data$shrub_cover

# Predict grass cover for the test set using the trained model
predicted_shrub_cover <- predict(rf_model, data = test_input)$predictions

# Evaluate the performance of the model
accuracy <- mean(predicted_shrub_cover == actual_shrub_cover)
print(paste("Accuracy:", accuracy))

# Plot the predicted vs actual grass cover for the test set
plot(predicted_shrub_cover, actual_shrub_cover, xlab = "Predicted", ylab = "Actual")

rm(list = ls())

###### 2. CONTEXTUALIZE TO HERBIVORE DENSITIES ######

# Load required libraries
library(terra)
library(sf)
library(tidyverse)

# Read in camera trap data, growth form rasters and voronoi tessellations (waterhole "catchment areas")
voronoi <- read_sf("water_distance.shp")[3]
grass <- rast("Rasters/grass cover 6-7.tif")
shrubs <- rast("Rasters/shrub cover 6-7.tif")
tree <- rast("Rasters/tree cover 6-7.tif")

# The coverage to biomass conversion coefficients were obtained using biomass acquisition, drying in weighing in the field (Grewia flava, Schmidtia pappophoroides)
coefficient_grass <- 4400 # in kg/ha (1 kg/ha = 10 g/m^2)
coefficient_shrub <- 9200 # in kg/ha (1 kg/ha = 10 g/m^2)
coefficient_tree <- 9200 # in kg/ha (1 kg/ha = 10 g/m^2)

# Converting the vegetation layers to proportions and multiplying them by biomass coefficients
grassy <- grass/100 * coefficient_grass
woody <- tree/100 * coefficient_tree + shrubs/100 * coefficient_shrub

# Exporting rasters, we use QGIS to extract the total biomass for every waterhole "catchment area"
writeRaster(grassy, "grassy_biomass.tif")
writeRaster(woody, "woody_biomass.tif")

## Convert herbivores occuring at each waterhole in grazing and browsing units from Oudtshoorn (2019)
# Importing and cleaning camera data of duplicates
data1 <- read.csv("Full annotation 2-7-2023.csv")
data2 <- read.csv("Full annotation 8-7-2023.csv")
data <- rbind(data1,data2)
rm(data1,data2)
data$timestamp <- as.POSIXct(data$timestamp, format = "%d/%m/%Y %H:%M")
data$rounded_timestamp <- round(data$timestamp, "hours")
data <- subset(data, data$image_label_1 != "None")
data <- data[,-c(4,7)]
data$flags <- duplicated(data)
data <- subset(data, data$flags == FALSE)


species <- c("Elephant", "Giraffe", "Zebra", "Warthog", "Eland", "Kudu", "Wildebeest", "Impala", "Gemsbok", "Springbok", "Rhinoceros")
sites <- unique(data$site)

# Create a data frame with 11 rows and columns from the vector
count <- data.frame(matrix(nrow = length(sites), ncol = length(species)))  # Create an empty data frame with 11 rows and columns based on the vector length

# Set column and row names
colnames(count) <- species
rownames(count) <- sites

for(i in species){
  for(j in sites){
    sub <- subset(data, site == j & image_label_1 == i)
    count[j,i] <- sum(sub$image_sighting_count_1)
  }
}

## Game count estimates:
# Zebra 723
# Wildebeest 391
# Eland 321
# Gemsbok 156
# Hartebeest 4
# Impala 1400
# Giraffe 264
# Kudu 648
# Elephant 25
# Rhino 46
# Springbok 148

# To obtain the estimate of animals around each waterhole, we calculate the proportion of records
# Then we multiply the output by the proportion of area covered by camera-equipped waterholes (68%)
# Finally we multiply the result by the estimate of animals on the property for each species
count$Elephant <- count$Elephant/sum(count$Elephant, na.rm = T)*0.68*25
count$Giraffe <- count$Giraffe/sum(count$Giraffe, na.rm = T)*0.68*264
count$Zebra <- count$Zebra/sum(count$Zebra, na.rm = T)*0.68*723
count$Warthog <- count$Warthog/sum(count$Warthog, na.rm = T)*0.68*150 # not part of the count
count$Eland <- count$Eland/sum(count$Eland, na.rm = T)*0.68*321
count$Kudu <- count$Kudu/sum(count$Kudu, na.rm = T)*0.68*648
count$Wildebeest <- count$Wildebeest/sum(count$Wildebeest, na.rm = T)*0.68*391
count$Impala <- count$Impala/sum(count$Impala, na.rm = T)*0.68*1400
count$Gemsbok <- count$Gemsbok/sum(count$Gemsbok, na.rm = T)*0.68*156
count$Springbok <- count$Springbok/sum(count$Springbok, na.rm = T)*0.68*148
count$Rhino <- count$Rhino/sum(count$Rhino, na.rm = T)*0.68*46

# We manually add the estimates to the main dataframe as the waterhole names differ
write.csv(count, "Stocking rates.csv")
count <- read.csv("Waterholes.csv")

# We then convert the estimates into browsing and grazing units (van Oudtshoorn, 2019)
# Browsing units
browsing <- count$Rhino*3.43 +
  count$Giraffe*4.28 +
  count$Warthog*0.12+
  count$Springbok*0.07+
  count$Kudu*0.8+
  count$Elephant*5.56+
  count$Eland*1.56+
  count$Impala*0.25+
  count$Gemsbok*0.09

# Grazing units
grazing <- count$Zebra*0.66+
  count$Wildebeest*0.54+
  count$Gemsbok*0.5+
  count$Impala*0.1+
  count$Eland*0.43+
  count$Elephant*2.57+
  count$Kudu*0.1+
  count$Springbok*0.12+
  count$Warthog*0.1+
  count$Giraffe*0.02

# Conversion into biomass requirements per area unit: 1 animal unit = 11.25 kg/day - 4106.25 kg/year (van Oudtshoorn, 2019)
grazing <- grazing*4106.25/count$Area..ha.
browsing <- browsing*4106.25/count$Area..ha.

# Finally, we calculate what fraction of the annually available biomass is annually consumed given the stocking rate estimates
count$Browsed.fraction <- round(browsing/count$Leaves..kg.ha.*100,2)
count$Grazed.fraction <- round(grazing/count$Grass..kg.ha.*100,2)

write.csv(count, "Stocking rates water.csv")

###### 3. EXTRAPOLATE TO UNEQUIPPED WATERHOLES ######

# A Principal Component Analysis (PCA) helps to summarize most of the variance of multiple indicators of veld utilization into one variable (principal component)
plots <- read.csv("Plots_2023_handsorted.csv")

plants <- read.csv("Plants_2023.csv")
pioneer <- numeric(length(unique(plants$Plot_Number)))
increaser <- numeric(length(unique(plants$Plot_Number)))
for (i in 1:length(unique(plants$Plot_Number))) {
  sub <- subset(plants, Plot_Number == unique(plants$Plot_Number)[i] & Growth_Form == "Grass")
  pioneer[i] <- sum(sub$Succession == "Pioneer") / nrow(sub)
  increaser[i] <- sum(sub$Ecological_Index == "Increaser II") / nrow(sub)
}

data <- data.frame("Erosion" = plots$Erosion.level, # erosion
                   "Crust" = plots$Crust, # crust
                   "Bare" = plots$Bare...., # bare ground
                   "Forbs" = plots$Forbs...., # forbs
                   "Encroachment" = plots$Shrubs......woody...proxy.for.encroachment., # encroachment
                   "Grass" = plots$Grasses...., # grass cover
                   "Succession" = pioneer, # grass succession state (proportion of pioneer species)
                   "Ecological.status" = increaser)  # grass ecological status (proportion of increaser type 2 species)
pca_res <- prcomp(data, scale. = T)

# Plot
library(ggfortify)
autoplot(pca_res, data = data, loadings = T, #size = log(rangemodif$Future.Range[-1])*2,
         loadings.label = T, loadings.label.colour = "black", loadings.colour = "black",
         loadings.label.repel = T)
# Export the values of each point for the new "variable"
pc1 <- pca_res$x[,1]

# Next, we use a random forest model to extrapolate to the property
# Step 1: Prepare the data
# Extract the pixel values of the sentinel bands for the chosen locations
chosen_locations <- data.frame(x = plots$Longitude, y = plots$Latitude)
sentinel_data <- extract(sentinel_raster, chosen_locations, ID = F)

# Combine the grass cover and Sentinel-2 data into one dataframe
df <- data.frame(pc1, sentinel_data)

# Step 2: Train the random forest model

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
#train_indices <- sample(1:nrow(df), nrow(df) * 0.8)  # 80% for training
train_data <- df#[train_indices, ]
#test_data <- df[-train_indices, ]

# Train the random forest model
rf_model <- ranger(pc1 ~ ., data = train_data, num.trees = 500)

# Create prediction raster, cropped to the area of interest
prediction <- predict(sentinel_raster, rf_model, na.rm = T)$predictions
prediction <- mask(crop(prediction, onguma), onguma)
plot(prediction)
writeRaster(prediction, "Rasters/utilization.tif", overwrite = T)

# Finally, we run a GLM (Generalized Linear Model) to estimate the stocking rates at non-equipped waterholes
# We used QGIS to extract the mean utilization score for each waterhole catchement area
count <- read.csv("Stocking rates water.csv")

cleaned_data <- na.omit(count)
utilization_model_grazing <- glm(as.numeric(cleaned_data$Grazed.fraction) ~ cleaned_data$Utilization, family = "gaussian")
utilization_model_browsing <- glm(cleaned_data$Browsed.fraction ~ cleaned_data$Utilization, family = "gaussian")
count$Grazed.fraction.GLM <- count$Utilization*utilization_model_grazing$coefficients[2]+utilization_model_grazing$coefficients[1]
#count$Browsed.fraction.GLM <- count$Utilization*utilization_model_browsing$coefficients[2]+utilization_model_browsing$coefficients[1]


par(mfrow = c(1,2))
plot(count$Browsed.fraction ~ count$Browsed.fraction.GLM, na.rm = T)
abline(a = 0, b = 1)
plot(count$Grazed.fraction ~ count$Grazed.fraction.GLM, na.rm = T)
abline(a = 0, b = 1)

summary(utilization_model_browsing) # insignificant
summary(utilization_model_grazing)

write.csv(count, "Final output.csv")

###### 4. VEGETATION MAP #####
# First the proportions need to be multiplied by proportions of the growth form in the plot

for(i in unique(plots$Plot_Number)){
  sub <- plants[plants$Plot_Number == i,]
  for(j in 1:nrow(sub)){
    if(sub$Growth_Form[j] == "Tree"){
      sub$Coverage[j] <- sub$Coverage[j]*plots[plots$Plot_Number == i,]$Canopy
    } else if(sub$Growth_Form[j] == "Shrub"){
      sub$Coverage[j] <- sub$Coverage[j]*plots[plots$Plot_Number == i,]$Shrubs
    } else if(sub$Growth_Form[j] == "Grass"){
      sub$Coverage[j] <- sub$Coverage[j]*plots[plots$Plot_Number == i,]$Grasses
    } else if(sub$Growth_Form[j] == "Forb"){
      sub$Coverage[j] <- sub$Coverage[j]*plots[plots$Plot_Number == i,]$Forbs
    }
  }
  sub$Coverage <- sub$Coverage/10000 # convert to proportions
  plants[plants$Plot_Number == i,] <- sub
}

### Clustering based on trees and shrubs

# Reading in data
sub <- plants[plants$Growth_Form == "Tree" | plants$Growth_Form == "Shrub",]

# Partitioning around medoids (PAM) clustering algorithm

library(cluster)  # Load the 'cluster' package
library(dplyr)

# Transform data into df with rows as points and columns as species (proportions)

# Pivot the data frame from long to wide format
vec <- unique(sub$Scientific_Name)
vec2 <- numeric(length = length(vec))
result <- matrix(nrow = length(unique(sub$Plot_Number)), ncol = length(vec), dimnames = list(NULL, vec))
k <- 0

for (i in unique(sub$Plot_Number)) {
  k <- k+1
  for (j in 1:length(vec)) {
    selection <- sub %>% filter(Scientific_Name == vec[j] & Plot_Number == i)
    if (nrow(selection) > 0) {
      vec2[j] <- sum(selection$Coverage)
    } else {
      vec2[j] <- 0
    }
  }
  result[k, ] <- vec2
}

pivoted_sub <- data.frame(Plot_Number = unique(sub$Plot_Number), result)


# Create a dissimilarity matrix
dist_matrix <- dist(pivoted_sub[,2:ncol(pivoted_sub)], method = "euclidian")

# Calculate WCSS for different numbers of clusters
wcss <- vector()
for (k in 1:10) {
  pam_results <- pam(dist_matrix, k = k)
  wcss[k] <- pam_results$objective[1]
}

# Plot the elbow curve
plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters",
     ylab = "WCSS", main = "Elbow Method")

# In the plot, we should look for the point where the decrease in WCSS begins to level off, forming an elbow-like shape. This point suggests the optimal number of clusters.

# Silhouette width
sil_width <- vector()
for (k in 2:10) {
  pam_results <- pam(dist_matrix, k = k)
  sil_width[k] <- mean(silhouette(pam_results$clustering, dist_matrix)[,3])
}

# Plot the silhouette widths
plot(2:10, sil_width[2:10], type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters",
     ylab = "Average Silhouette Width", main = "Silhouette Analysis")

# In the silhouette analysis plot, we look for the number of clusters that corresponds to the peak average silhouette width. This indicates the optimal number of clusters.

# Clustering
pam_results <- pam(dist_matrix, k = 4)  # Perform PAM clustering with k = 4

# Access the cluster assignments
cluster_assignments <- cbind(unique(pivoted_sub[,1]), pam_results$clustering)
cluster_assignments <- rbind(cluster_assignments, c(61,5))

plots$Cluster_woody <- numeric(length = nrow(plots))
for(i in unique(plots$Plot.Number)){
  plots[which(plots$Plot.Number == i),]$Cluster_woody <- cluster_assignments[which(cluster_assignments[,1] == i),2]
}


hclust_clustering <- hclust(dist_matrix)
library(stats)
hclust_clustering$labels <- plots$Plot.Number
dend <- as.dendrogram(hclust_clustering)
plot(dend)

### RF model and extrapolation

# Load required libraries
library(terra)
library(ranger)

# Step 1: Prepare the data
plots <- read.csv("Plots_2023_handsorted.csv")
clusters <- plots$Community
unique(clusters)
clusters[clusters == "Karstveld"] <- "Karstveld shrubland"
clusters[clusters == "Salt pan"] <- "Grassland/Dwarf Shrubland"

library(sf)
onguma <- read_sf("Onguma shapefile/Onguma_EPSG_4326.shp")[1]
sentinel_raster <- mask(crop(sentinel_raster, onguma), onguma)

# Extract the pixel values of the sentinel bands for the chosen locations
chosen_locations <- data.frame(x = plots$Longitude, y = plots$Latitude)
sentinel_data <- extract(sentinel_raster, chosen_locations, ID = F)

# Combine the community and Sentinel data into one dataframe
df <- data.frame(clusters, sentinel_data)

# Step 2: Train the random forest model

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
#train_indices <- sample(1:nrow(df), nrow(df) * 0.8)  # 80% for training
train_data <- df#[train_indices, ] # For now we use all data as training, validation will be done later
#test_data <- df[-train_indices, ]

# Train the random forest model
rf_model <- ranger(as.factor(clusters) ~ ., data = train_data, num.trees = 500)

prediction <- predict(sentinel_raster, rf_model, na.rm = T)$predictions
prediction <- mask(crop(prediction, onguma), onguma)
plot(prediction)

smoothed_raster <- focal(prediction, w = matrix(1, nrow = 3, ncol = 3), fun = modal)
plot(smoothed_raster)

writeRaster(prediction, "Rasters/Communities 5-7.tif", overwrite = T)



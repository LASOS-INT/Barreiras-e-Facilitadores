#install.packages("here")
library(here, help, pos = 2, lib.loc = NULL)
source(here('Modules','modules.R'))
source(here('Modules','data_cleaning.R'))
#install_all_packages()
load_library_packages()

df_copy <- df
df_copy[, "strange_dp"] <- strange_dp
df_copy[, "strange_p"] <- strange_p
has_tag_depression <- df$TAG == "1" | df$depression  == "1" 
df_copy[, "has_tag_depression"] <- has_tag_depression
dummy <- dummyVars(" ~ .", data=before_dataset)
before_encoded_dataset <- data.frame(predict(dummy, newdata = before_dataset)) 

# before_encoded_dataset.pca <- prcomp(before_encoded_dataset , center = TRUE,scale. = TRUE)
# summary(before_encoded_dataset.pca) 

# autoplot(before_encoded_dataset.pca, colour="PA_practice_before", data=before_dataset)

set.seed(2)
tsne <- Rtsne(before_encoded_dataset, dims = 2, perplexity=100, check_duplicates = FALSE, verbose=TRUE)

metadata <- data.frame(sample_id = rownames(df_copy),
                       colour = df_copy$PA_practice_before )


main_tsne_df <- data.frame(
    x = tsne$Y[,1],
    y = tsne$Y[,2],
    colour = metadata$colour)

tsne_df_centroids <- main_tsne_df
# Defining the practice centroid points
practice_centroid_x <- mean(tsne_df_centroids[tsne_df_centroids["colour"] == "practice", ]$x)
practice_centroid_y <- mean(tsne_df_centroids[tsne_df_centroids["colour"] == "practice", ]$y)

# Defining the dont_practice centroid points
dont_practice_centroid_x <- mean(tsne_df_centroids[tsne_df_centroids["colour"] == "dont_practice", ]$x)
dont_practice_centroid_y <- mean(tsne_df_centroids[tsne_df_centroids["colour"] == "dont_practice", ]$y)

# Adding centroids to dataset
levels(tsne_df_centroids$colour) <- c(levels(tsne_df_centroids$colour), "centroid_1", "centroid_2")

# Plotting the data points
tsne_df_centroids[nrow(tsne_df_centroids) + 1, ] <- list(practice_centroid_x, practice_centroid_y, "centroid_1")
tsne_df_centroids[nrow(tsne_df_centroids) + 1, ] <- list(dont_practice_centroid_x, dont_practice_centroid_y, "centroid_2")
ggplot(tsne_df_centroids, aes(x, y, colour = colour)) + geom_point()


metadata <- data.frame(sample_id = rownames(df_copy),
                       colour = df_copy$PA_practice_during )
tsne_df <- data.frame(
    x = tsne$Y[,1],
    y = tsne$Y[,2],
    colour = metadata$colour)

ggplot(tsne_df, aes(x, y, colour = colour)) + geom_point()

metadata <- data.frame(sample_id = rownames(df_copy),
                       colour = df_copy$strange_dp)
tsne_df <- data.frame(
    x = tsne$Y[,1],
    y = tsne$Y[,2],
    colour = metadata$colour)

ggplot(tsne_df, aes(x, y, colour = colour)) + geom_point()

metadata <- data.frame(sample_id = rownames(df_copy),
                       colour = df_copy$strange_p)
tsne_df <- data.frame(
    x = tsne$Y[,1],
    y = tsne$Y[,2],
    colour = metadata$colour)

ggplot(tsne_df, aes(x, y, colour = colour)) + geom_point()

metadata <- data.frame(sample_id = rownames(df_copy),
                       colour = df_copy$depression_anxiety)
tsne_df <- data.frame(
    x = tsne$Y[,1],
    y = tsne$Y[,2],
    colour = metadata$colour)

ggplot(tsne_df, aes(x, y, colour = colour)) + geom_point()

distances_sdp <- distance_to_centroids(main_tsne_df, strange_dp, "practice", "dont_practice")
distances_sdp
distances_sp <- distance_to_centroids(main_tsne_df, strange_p, "practice", "dont_practice")
distances_sp

pa_dataset_copy <- pa_dataset
pa_dataset_copy["outlier"] <- strange_dp
outliers_result_sdp <- outliers_checker(distances_sdp, pa_dataset_copy, "PA_practice_during")

plot <- outliers_result_sdp$kappa_x_alpha
ggplot(plot, aes(plot[, "alpha"], plot[, "kappa"] )) + geom_point()

pa_dataset_copy <- pa_dataset
pa_dataset_copy["outlier"] <- strange_p
outliers_result_sp <- outliers_checker(distances_sp, pa_dataset_copy, "PA_practice_during")

plot <- outliers_result_sp$kappa_x_alpha
ggplot(plot, aes(plot[, "alpha"], plot[, "kappa"] )) + geom_point()

pa_train <- outliers_result_sdp$best_model$train
pa_test <- outliers_result_sdp$best_model$test

sedentery_dataset_copy <- sedentary_dataset
sedentery_dataset_copy["outlier"] <- strange_dp
outliers_result <- outliers_checker(distances_sdp, sedentery_dataset_copy, "sedentary_time_range_during")

plot <- outliers_result$kappa_x_alpha

ggplot(plot, aes(plot[, "alpha"], plot[, "kappa"] )) + geom_point()

sedentary_dataset_copy <- sedentary_dataset
sedentary_dataset_copy["outlier"] <- strange_p
outliers_result <- outliers_checker(distances_sp, sedentary_dataset_copy, "sedentary_time_range_during")

plot <- outliers_result$kappa_x_alpha
ggplot(plot, aes(plot[, "alpha"], plot[, "kappa"] )) + geom_point()

sedentary_train <- outliers_result$best_model$train
sedentary_test <- outliers_result$best_model$test

table(pa_behavior_dataset[, "pa_behavior"])
practice_before <- c("change_to_dont_practice", "still_practice")
dont_practice_before <- c("change_to_practice", "still_dont_practice")
pa_behavior_dataset_1 <- pa_behavior_dataset[pa_behavior_dataset$pa_behavior %in% practice_before, ]

new_levels <- droplevels(pa_behavior_dataset_1[, "pa_behavior"], exclude = levels(pa_behavior_dataset_1[, "pa_behavior"]) %in% dont_practice_before)
pa_behavior_dataset_1[, "pa_behavior"] <- new_levels

table(pa_behavior_dataset_1[, "pa_behavior"])



pa_behavior_dataset_copy <- pa_behavior_dataset_1
pa_behavior_dataset_copy["outlier"] <- strange_p[as.numeric(rownames(pa_behavior_dataset_1))]
outliers_result <- outliers_checker(distances_sp, pa_behavior_dataset_copy, "pa_behavior")

plot <- outliers_result$kappa_x_alpha
ggplot(plot, aes(plot[, "alpha"], plot[, "kappa"] )) + geom_point()

pa_behavior_train <- outliers_result$best_model$train
pa_behavior_test <- outliers_result$best_model$test

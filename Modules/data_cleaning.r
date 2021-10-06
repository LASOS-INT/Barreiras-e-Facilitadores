#install.packages("here")
library(here, help, pos = 2, lib.loc = NULL)
source(here('Modules','modules.R'))
#install_all_packages()
load_library_packages()

df <- read_excel(path = here('Data','before_pa.xlsx'))
df <- data.frame(df)
dim(df)
unique(sapply(df, class))
head(df)

sports = c('running_before','hiking_before','cycling_before','muscle_training_before',
'soccer_before','tennis_before','volleyball_before','basketball_before',
'swimming_before','surfing_before','yoga_before','pilates_before',
'fighting_before','other_sports_before')
    
df[, "pa_number_before"] = rowSums(df[, sports])

df <- df[complete.cases(df), ]
dim(df)

df$PA_practice_before[df$PA_practice_before == 0] <- "practice"
df$PA_practice_before[df$PA_practice_before == 1] <- "dont_practice"
df$PA_practice_during[df$PA_practice_during == 0] <- "practice"
df$PA_practice_during[df$PA_practice_during == 1] <- "dont_practice"
df$PA_intensity_before[df$PA_intensity_before == 4] <- 0
df$PA_intensity_during[df$PA_intensity_during == 4] <- 0
df$PA_duration_before[df$PA_duration_before == 5] <- 0


numeric_columns = c(
    'age_range',
    'rooms_range',
    'income_range',
    'scholarity',
    'sedentary_time_range_before',
    'co.resident_range',
    "PA_weekly_frequency_before",
    'PA_intensity_before',
    'PA_intensity_during',
    'PA_duration_before',
    "pa_number_before"
)
columns = names(df)
categorical_columns <- columns[!columns %in% numeric_columns]


df[, categorical_columns] <- lapply(df[, categorical_columns], as.factor)
df[, numeric_columns] <- lapply(df[, numeric_columns], as.integer)


df[1, 2] < df[2, 2] 

head(df[, numeric_columns])


lapply(df, levels)[c("DA", "DP")]
df <- subset(df, select = -c(DA, DP, state, zone))

table(df[, c("PA_practice_before", "PA_duration_before")])
table(df[, c("PA_practice_before", "PA_intensity_before")])
table(df[, c("PA_practice_before", "PA_weekly_frequency_before")])
table(df[, c("PA_practice_before", "pa_number_before")])


differ_dp <- ( df$PA_duration_before != 0 
                | df$PA_intensity_before != 0 
                | df$PA_weekly_frequency_before != 1 
                | df$pa_number_before != 0
            ) 
strange_dp <- df$PA_practice_before == "dont_practice" & differ_dp 

table(strange_dp)


differ_p <- (   df$PA_duration_before == 0 
                | df$PA_intensity_before == 0 
                | df$PA_weekly_frequency_before == 1
                | df$pa_number_before == 0
            )
strange_p <- df$PA_practice_before == "practice" & differ_p
table(strange_p)



output_variables = c(
    "sedentary_time_range_during",
    "PA_intensity_during",
    "PA_practice_during"
)

before_dataset <- df[, !(names(df) %in% output_variables)]
pa_dataset <-  df[, !(names(df) %in% output_variables[-(3)])]
sedentary_dataset <- df[, !(names(df) %in% output_variables[-(1)])]
intesity_dataset <- df[, !(names(df) %in% output_variables[-(2)])]
before_dataset <- before_dataset
pa_dataset <- pa_dataset
sedentary_dataset <- sedentary_dataset
intesity_dataset <- intesity_dataset


df_copy <- df
df_copy[, "strange_dp"] <- strange_dp
df_copy[, "strange_p"] <- strange_p
has_tag_depression <- df$TAG == "1" | df$depression  == "1" 
df_copy[, "has_tag_depression"] <- has_tag_depression
dummy <- dummyVars(" ~ .", data=before_dataset)
before_encoded_dataset <- data.frame(predict(dummy, newdata = before_dataset)) 

before_encoded_dataset.pca <- prcomp(before_encoded_dataset , center = TRUE,scale. = TRUE)
summary(before_encoded_dataset.pca) 

autoplot(before_encoded_dataset.pca, colour="PA_practice_before", data=before_dataset)

tsne <- Rtsne(before_encoded_dataset, dims = 2, perplexity=30, check_duplicates = FALSE, verbose=TRUE)

metadata <- data.frame(sample_id = rownames(df_copy),
                       colour = df_copy$PA_practice_before )
tsne_df <- data.frame(
    x = tsne$Y[,1],
    y = tsne$Y[,2],
    colour = metadata$colour)

ggplot(tsne_df, aes(x, y, colour = colour)) + geom_point()

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

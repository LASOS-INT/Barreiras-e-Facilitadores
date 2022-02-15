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

#row.names(df) <- NULL
dim(df)

df$PA_practice_before[df$PA_practice_before == 0] <- "practice"
df$PA_practice_before[df$PA_practice_before == 1] <- "dont_practice"
df$PA_practice_during[df$PA_practice_during == 0] <- "practice"
df$PA_practice_during[df$PA_practice_during == 1] <- "dont_practice"


df$PA_intensity_before[df$PA_intensity_before == 4] <- 0
df$PA_duration_before[df$PA_duration_before == 5] <- 0



df$sedentary_time_range_during[df$sedentary_time_range_during < 5] <- "less_then_8_hours"
df$sedentary_time_range_during[df$sedentary_time_range_during == 5] <- "8_hour_or_more"

df$sedentary_time_range_before[df$sedentary_time_range_before < 5] <- "less_then_8_hours"
df$sedentary_time_range_before[df$sedentary_time_range_before == 5] <- "8_hour_or_more"

df$PA_barrier_no_interest[df$PA_barrier_no_interest == 0] <- "no"
df$PA_barrier_no_interest[df$PA_barrier_no_interest == 1] <- "yes"



df$PA_barrier_family_responsabilities[df$PA_barrier_family_responsabilities == 0] <- "no"
df$PA_barrier_family_responsabilities[df$PA_barrier_family_responsabilities == 1] <- "yes"


df$PA_perceive_barriers[df$PA_perceive_barriers == 0] <- "no"
df$PA_perceive_barriers[df$PA_perceive_barriers == 1] <- "yes"

df$PA_barrier_places[df$PA_barrier_places == 0] <- "no"
df$PA_barrier_places[df$PA_barrier_places == 1] <- "yes"



df$PA_barrier_hard_work[df$PA_barrier_hard_work == 0] <- "no"
df$PA_barrier_hard_work[df$PA_barrier_hard_work == 1] <- "yes"


pa_behavior1 <- df[df$PA_practice_before == "practice", ]
pa_behavior2 <- df[df$PA_practice_before == "dont_practice", ]

convert_pa_behavior <- function (row) {
    if(row["PA_practice_before"] == "dont_practice" && row["PA_practice_during"] == "dont_practice"){
        "still_dont_practice"
    } else if (row["PA_practice_before"] == "dont_practice" && row["PA_practice_during"] == "practice"){
        "change_to_practice"
    } else if (row["PA_practice_before"] == "practice" && row["PA_practice_during"] == "practice"){
        "still_practice"
    } else {
        "change_to_dont_practice"
    }

}


df["pa_behavior"] <- apply(df, MARGIN=1, convert_pa_behavior)

sed_behavior1 <- df[df$sedentary_time_range_before == "less_then_8_hours", ]
sed_behavior2 <- df[df$sedentary_time_range_before == "8_hour_or_more", ]

convert_sed_behavior <- function (row) {
    if(row["sedentary_time_range_before"] == "less_then_8_hours" && row["sedentary_time_range_during"] == "less_then_8_hours"){
        "still_not_sedentary"
    } else if (row["sedentary_time_range_before"] == "8_hour_or_more" && row["sedentary_time_range_during"] == "8_hour_or_more"){
        "still_sedentary"
    } else if (row["sedentary_time_range_before"] == "8_hour_or_more" && row["sedentary_time_range_during"] == "less_then_8_hours"){
        "change_to_not_sedentary"
    } else {
        "change_to_sedentary"
    }

}


df["sedentary_behavior"] <- apply(df, MARGIN=1, convert_sed_behavior)
table(df["sedentary_behavior"])

numeric_columns = c(
    'age_range',
    'rooms_range',
    'income_range',
    'scholarity',
    'co.resident_range',
    "PA_weekly_frequency_before",
    'PA_intensity_before',
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


differ_p <- (   df$PA_duration_before == 0 
                | df$PA_intensity_before == 0 
                | df$PA_weekly_frequency_before == 1
                | df$pa_number_before == 0
            )
strange_p <- df$PA_practice_before == "practice" & differ_p
table(strange_p)
rownames(df[strange_p, ])


differ_dp <- ( df$PA_duration_before != 0 
                | df$PA_intensity_before != 0 
                | df$PA_weekly_frequency_before != 1 
                | df$pa_number_before != 0
            ) 
strange_dp <- df$PA_practice_before == "dont_practice" & differ_dp 

table(strange_dp)
rownames(df[strange_dp, ])

output_variables = c(
    "sedentary_time_range_during",
    "pa_behavior",
    "PA_practice_during",
    "sedentary_behavior",
    "PA_barrier_no_interest",
    "PA_barrier_family_responsabilities",
    "PA_perceive_barriers",
    "PA_barrier_hard_work",
    'PA_barrier_places'
)

before_dataset <- df[, !(names(df) %in% output_variables)]
pa_dataset <-  df[, !(names(df) %in% output_variables[-(3)])]
sedentary_dataset <- df[, !(names(df) %in% output_variables[-(1)])]
pa_behavior_dataset <- df[, !(names(df) %in% append( output_variables[-(2)], "PA_practice_before"))]
sedentary_behavior_dataset <- df[, !(names(df) %in% append( output_variables[-(4)], "sedentary_time_range_before"))]
no_interst_barrier_dataset <-  df[, !(names(df) %in% output_variables[-(5)])]
family_responsabilities_barrier_dataset <- df[, !(names(df) %in% output_variables[-(6)])]
percieve_barriers_dataset <- df[, !(names(df) %in% output_variables[-(7)])]
hard_work_barrier_dataset <- df[, !(names(df) %in% output_variables[-(8)])]
places_barrier_dataset <- df[, !(names(df) %in% output_variables[-(9)])]

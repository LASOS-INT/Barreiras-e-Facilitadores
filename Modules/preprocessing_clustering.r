library(here, help, pos = 2, lib.loc = NULL) 
source(here('Modules','modules.R'))
#install_all_packages()
load_library_packages()

df_during <- read_excel(path = here('Data','during_dataset.xlsx'))
df_during <- data.frame(df_during)
unique(sapply(df_during, class))
df_during <- df_during[complete.cases(df_during), ]
dim(df_during)
head(df_during)

df_during$PA_intensity[df_during$PA_intensity == 4] <- 0
df_during$PA_duration[df_during$PA_duration == 5] <- 0
df_during$PA_weekly_frequency[df_during$PA_weekly_frequency == 4] <- 0

df_during$PA_practice_during <- ifelse(df_during$PA_practice_during == 0, 1, 0)

df_during$sedentary_time_range_during[df_during$sedentary_time_range_during < 5] <- 0
df_during$sedentary_time_range_during[df_during$sedentary_time_range_during == 5] <- 1

df_during$scholarity[df_during$scholarity <= 3] <- 1
df_during$scholarity[df_during$scholarity > 3 & df_during$scholarity <= 5] <- 2
df_during$scholarity[df_during$scholarity == 6] <- 3

df_during <- df_during[df_during$sex != 3, ]

sports <- c("running","hiking","cycling",
            "muscle_training","soccer","tennis",
            "volleyball","basketball","swimming",
            "surfing","yoga","pilates","fighting","other_sports")

df_during$PA_number <- rowSums(df_during[, sports])

df_during[, 1:ncol(df_during)] <- lapply(df_during[, 1:ncol(df_during)], as.factor)

`%ni%` <- Negate(`%in%`)
df_during <- df_during[names(df_during) %ni% sports]

differ_p <- (   df_during$PA_duration == 0 
                | df_during$PA_intensity == 0 
                | df_during$PA_weekly_frequency == 0
                | df_during$PA_number == 0
            )
strange_p <- df_during$PA_practice_during == 1 & differ_p
df_during <- df_during[!strange_p, ]
table(strange_p)

differ_dp <- ( df_during$PA_duration != 0 
                | df_during$PA_intensity != 0 
                | df_during$PA_weekly_frequency != 0
                | df_during$PA_number != 0
            ) 
strange_dp <- df_during$PA_practice_during == 0 & differ_dp 


table(strange_dp)
df_during <- df_during[!strange_dp, ]

rownames(df_during) <- 1:nrow(df_during)

names(df_during)[8] <- 'PA_barrier_percieve'

df_during[1:18] <- lapply(df_during[1:18], as.factor)
df_during_barriers <- df_during[, 1:8]
dim(df_during_barriers)
head(df_during_barriers)


time_barrier_1 <- ifelse(df_during_barriers$PA_barrier_time_family_responsabilities=='0',0,1)
time_barrier_2 <- ifelse(df_during_barriers$PA_barrier_time_convenience=='0',0,1)


hard_barrier_1 <- ifelse(df_during_barriers$PA_barrier__tiredness=='0',0,1)
hard_barrier_2 <- ifelse(df_during_barriers$PA_barrier_hard_work=='0',0,1)


df_during_barriers$PA_barrier_time <- (time_barrier_1 | time_barrier_2) + 0 
df_during_barriers$PA_barrier_hard <- (hard_barrier_1 | hard_barrier_2) + 0

df_during_barriers$PA_barrier_time_family_responsabilities <- NULL
df_during_barriers$PA_barrier_time_convenience <- NULL

df_during_barriers$PA_barrier__tiredness <- NULL
df_during_barriers$PA_barrier_hard_work <- NULL

df_during_barriers[, 1:ncol(df_during_barriers)] <- lapply(df_during_barriers[, 1:ncol(df_during_barriers)], as.factor)
names(df_during_barriers)

df_during[1:18] <- lapply(df_during[1:18], as.factor)
df_during_facilitators <- df_during[, 9:18]
dim(df_during_facilitators)
head(df_during_facilitators)


home_facilitator_1 <- ifelse(df_during_facilitators$PA_easiness_home_activity=='0',0,1)
home_facilitator_2 <- ifelse(df_during_facilitators$PA_easiness_home_equipament=='0',0,1)
home_facilitator_3 <- ifelse(df_during_facilitators$PA_easiness_home_space=='0',0,1)


group_easiness_1 <- ifelse(df_during_facilitators$PA_easiness_family_activity=='0',0,1)
group_easiness_2 <- ifelse(df_during_facilitators$PA_easiness_collective=='0',0,1)



df_during_facilitators$PA_facilitator_home <- (home_facilitator_1 | home_facilitator_2 | home_facilitator_3) + 0 
df_during_facilitators$PA_facilitator_group<- (group_easiness_1 | group_easiness_2) + 0


df_during_facilitators$PA_easiness_home_activity <- NULL
df_during_facilitators$PA_easiness_home_equipament <- NULL
df_during_facilitators$PA_easiness_home_space <- NULL
df_during_facilitators$PA_easiness_family_activity <- NULL
df_during_facilitators$PA_easiness_collective <- NULL
df_during_facilitators$PA_easiness_unguided <- NULL

df_during_facilitators[, 1:ncol(df_during_facilitators)] <- lapply(df_during_facilitators[, 1:ncol(df_during_facilitators)], as.factor)
names(df_during_facilitators)

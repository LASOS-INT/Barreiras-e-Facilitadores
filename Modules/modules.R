packs = c('ggplot2', 'cluster', 'cowplot', 'randomForest', 'reshape2',
          'caret', 'rpart.plot', 'readxl',
          'e1071', 'AugmenterR', 'smotefamily',
          'ROSE', 'xgboost', 'pROC', 'klaR','patchwork', 'grid',
          'MASS', 'lsr', 'DescTools', 'devtools',
          'dplyr', 'kernlab', 'fastAdaboost', 
          'DataExplorer', 'dummies', 'lattice', 
          'mlbench', 'h2o', 'here', "rattle", "pracma",
           "MLmetrics", "ggfortify", "Rtsne", "obliqueRF", "gbm", "MLeval", "fmsb", "DescTools", "cba")


install_all_packages <- function () {
  lapply(packs, install.packages, character.only = T, logical.return = TRUE)
}



# nearest_clust_rock <- function(df, cluster_i, k, clusters){
#   mode_i = unname(apply(clusteri, 2, modefunc))
#   for(i in 1:k){
    

#       clusteri = df_during_barriers_facilitators[rockCluster(df_matrix, n=3, theta=0.5)$cl == 1, ]
#       unname(apply(clusteri, 2, modefunc))
#   }
# }

nearest_cluster_medoids <- function(medoid, df, meds){
    cluster_medoid <- df[medoid, ]
    diss_medoids <- apply(df[meds, ], 1, function (row) sum(cluster_medoid != row))
    return(names(diss_medoids[-as.integer(medoid)])[which.min(diss_medoids[-as.integer(medoid)])])
}


# db_index_kmodes <- function (num_clusters, df, diss_matrix, iters, s){
#     set.seed(s)
#     kmode <- kmodes(df, num_clusters, iter.max = iters, weighted = FALSE)
#     Db_index <- 0
#     for (i in 1:num_clusters) {
#       Ri <- c()
#       Rij <- c()
#       for (j in 1:num_clusters) {
#           if(j != i){
#             clusteri <- kmode$cluster == i
#             clusterj <- kmode$cluster == j
#             ni <- sum(clusteri)
#             nj <- sum(clusterj)
#             Si <- if(ni > 1) sum(diss_matrix[clusteri, clusteri])/(ni-1) else 0
#             Sj <- if(nj > 1) sum(diss_matrix[clusterj, clusterj])/(nj-1) else 0
#             Mij <- sum(kmode$modes[i, ] != kmode$modes[j, ])
#             Rij <- append(Rij, (Si + Sj)/Mij)
#           }
#       }
#       Ri <- append(Ri, max(Rij))

#     }
#     return(sum(Ri)/num_clusters)
# }


silhouette_values_kmodes <- function(num_clusters, df, diss_matrix, iters, s){

      set.seed(s)
      kmode <- kmodes(df, num_clusters, iter.max = iters, weighted = FALSE)
      a <- c()
      b <- c()
      for (k in 1:num_clusters) {
            cluster <- kmode$cluster == k
            nearest_k <- nearest_cluster(kmode$modes, k)
            n_cluster <-  kmode$cluster == as.integer(nearest_k)

            if (sum(cluster) == 1){
              b_cluster <- 1
              a_cluster <- 1
            } else if (sum(n_cluster) == 1) {
             
              b_cluster <-  diss_matrix[cluster, n_cluster]/(sum(n_cluster))
              a_cluster <-  rowSums(diss_matrix[cluster, cluster])/(sum(cluster)-1)
            } else {
              b_cluster <- rowSums(diss_matrix[cluster, n_cluster])/(sum(n_cluster))
              a_cluster <-  rowSums(diss_matrix[cluster, cluster])/(sum(cluster)-1)

            }
            a <- append(a, a_cluster)
            b <-  append(b, b_cluster)
      }
 
      silhouette_coefficient <- (b-a)/pmax(b, a)
      silhouette_sc <- mean(silhouette_coefficient)

      return(list(silhouette_sc, silhouette_coefficient, kmode))


}

modefunc <- function(x){
  Mode(as.numeric(x))
}

fisher_values_kmodes <- function(num_clusters, df, iters, s){

  set.seed(s)
  kmode <- kmodes(df, num_clusters, iter.max = iters, weighted = FALSE)
  m <- unname(apply(df, 2, modefunc))
  Sb <- 0
  Sw <- 0
  for(i in 1:num_clusters){
      ni <- sum(kmode$cluster == i)
      cluster_df <- df[kmode$cluster == i, ]
      mi <- unname(apply(cluster_df, 2, modefunc))
      if(ni > 1){
        Sb <- ni*(sum(mi != m)**2) + Sb
        Sw <- sum(unname(apply(cluster_df, 1, function (x) sum(unname(x) != mi)))**2) + Sw
      }
  }
  return(list(Sb/Sw, kmode))
}

calisnki_values_kmodes <- function(num_clusters, df, iters, s){

  set.seed(s)
  kmode <- kmodes(df, num_clusters, iter.max = iters, weighted = FALSE)
  m <- unname(apply(df, 2, modefunc))
  BGSS <- 0
  WGSS <- 0
  for(i in 1:num_clusters){
      ni <- sum(kmode$cluster == i)
      cluster_df <- df[kmode$cluster == i, ]
      mi <- unname(apply(cluster_df, 2, modefunc))
      if(ni > 1){
        BGSS <- ni*(sum(mi != m)**2) + BGSS
        WGSS <- sum(apply(cluster_df, 1, function(row) sum(row != kmode$modes[k, ] )**2))
      }
  }
  calisnki <- (BGSS*(nrow(df)-num_clusters))/(WGSS*(num_clusters-1))
  return(list(calisnki, kmode))
}







nearest_cluster <- function(modes, cluster){
    cluster_mode <- modes[cluster, ]
    diss_modes <- apply(modes, 1, function (row) sum(cluster_mode != row))

    return(names(diss_modes[-(cluster)])[which.min(diss_modes[-(cluster)])])
}


nearest_cluster_rock <- function(cluster, df, num_clusters, cluster_mode, rock){
  diff <- nrow(df)
  n_cluster <- 0 
  for(k in 1:num_clusters){
    if(k != cluster){
        n_cluster_mode <- unname(apply(df[rock$cl == k, ], 2, modefunc))
        if(sum(n_cluster_mode != cluster_mode) < diff){
          diff <- sum(n_cluster_mode != cluster_mode)
          n_cluster <- k
        }

    }
  }
  return(n_cluster)
}





calisnki_values_rock <- function(num_clusters, theta, df, distmethod){
  df_matrix <- data.matrix(df) - 1
  rock <- rockCluster(df_matrix, n=num_clusters, theta=theta, fun = "dist", funArgs = list(method=distmethod))
  if (max(as.integer(rock$cl)) > num_clusters){
    return(list(-1, rock))
  }
  m <- unname(apply(df, 2, modefunc))
  BGSS <- 0
  WGSS <- 0
  for(i in 1:num_clusters){
      ni <- sum(rock$cl == i)
      cluster_df <- df[rock$cl == i, ]
      mi <- unname(apply(cluster_df, 2, modefunc))
      if(ni > 1){
        BGSS <- ni*(sum(mi != m)**2) + BGSS
        m_cluster <- unname(apply(cluster_df, 2, modefunc))

        WGSS <- sum(apply(cluster_df, 1, function(row) sum(row != m_cluster)**2))
      }
  }
  calisnki <- (BGSS*(nrow(df)-num_clusters))/(WGSS*(num_clusters-1))
  return(list(calisnki, rock))
}



silhouette_values_rock <- function(num_clusters, theta, df, diss_matrix, distmethod){
      df_matrix <- data.matrix(df) - 1
      rock <- rockCluster(df_matrix, n=num_clusters, theta=theta, fun = "dist", funArgs = list(method=distmethod))
      a <- c()
      b <- c()
      if (max(as.integer(rock$cl)) > num_clusters){
        return(list(-1, -1, rock))
      }
      for(k in 1:num_clusters){
            cluster <- rock$cl == k
            cluster_mode <- unname(apply(df[cluster, ], 2, modefunc))
            nearest_k <-nearest_cluster_rock(k, df, num_clusters, cluster_mode, rock)

            n_cluster <-  rock$cl == as.integer(nearest_k)

            if (sum(cluster) == 1){
              b_cluster <- 1
              a_cluster <- 1
            } else if (sum(n_cluster) == 1) {
             
              b_cluster <-  diss_matrix[cluster, n_cluster]/(sum(n_cluster))
              a_cluster <-  rowSums(diss_matrix[cluster, cluster])/(sum(cluster)-1)
            } else {
              b_cluster <- rowSums(diss_matrix[cluster, n_cluster])/(sum(n_cluster))
              a_cluster <-  rowSums(diss_matrix[cluster, cluster])/(sum(cluster)-1)

            }
            a <- append(a, a_cluster)
            b <-  append(b, b_cluster)
      }
      silhouette_coefficient <- (b-a)/pmax(b, a)
      silhouette_sc <- mean(silhouette_coefficient)
      return(list(silhouette_sc, silhouette_coefficient, rock))
}







load_library_packages <- function() {
  lapply(packs, library, character.only = T, logical.return = TRUE)
}

# fit_fbeta <- function(model_method, model_metric, trControl_func, train_data, test_data, length=5, yname){
#   betas <- seq(0, 2, 0.1)
#   for(i  in length(betas)){
#     fbeta <- function (data, lev=NULL, model = NULL){
#         fb_val <- FBeta_Score(data$obs, data$pred, positive="practice", beta = betas[i])
#         c(FB = fb_val)
#     }
#     fit_model(model_method, model_metric, trControl_func, train_data, test_data, length=5, yname)
#   }
# }

data_augmentation <- function(train_data, yname, maj_class, min_classes, prob_aug=0.9, ycol){
  train_data.aug <- train_data
  for(min in min_classes){
    #print("entrei")
    n = nrow(train_data[train_data[yname]==maj_class,]) - nrow(train_data[train_data[yname]==min,])
    j=0
    while(j<n){
      cand = GenerateMultipleCandidates(data=train_data, Class=min, col=ycol, Prob=prob_aug, amount=1)
      if(!anyNA(cand)){
        train_data.aug <- rbind(train_data.aug, cand)
        j=j+1
      }
    }
  }  
  return(train_data.aug)
}




fit_model_thresholder <- function(model_method, model_metric, trControl_func, train_data, test_data, length=5, yname, best_th){

  form = as.formula(paste(yname,'~.'))

  probs <- seq(.1, 0.9, by = 0.02)

  model <<- train(
    form, 
    data=train_data, 
    method=model_method, 
    metric=model_metric, 
    trControl=trControl_func, 
    tuneLength=length
  )

  real <- as.numeric(factor(test_data[, yname]))-1

  ths <- caret::thresholder(model,
                    threshold = probs,
                    final = TRUE,
                    statistics = "all") 


  chosen_prob <- best_th(ths)
  print(chosen_prob)
  pred <<- predict(model, test_data, type="prob")
  p = ggplot() + 
  geom_line(data = ths, aes(x = prob_threshold,  y = Sensitivity, colour = "Sensitivity"), size=1) +
  geom_line(data = ths, aes(x = prob_threshold, y = Specificity, colour = "Specificity"), size=1) +
  scale_color_manual(name = "Metric", values = c("Sensitivity" = "darkblue", "Specificity" = "red")) + 
  xlab('probability threshold')

  print(p)
  pred <- predict_out_of_probabilities(pred, chosen_prob)
  confM <<- confusionMatrix(pred$class, test_data[, yname], mode="everything")
  return(list(matrix=confM, result=model))
}

predict_out_of_probabilities <- function(pred, chosen_prob){
  posName <- colnames(pred)[1]
  negName <- colnames(pred)[2]
  find_classes <- function (row) {
    if(row[posName] >= chosen_prob){
      posName
    } else {
      negName
    }
  }
  pred["class"] <- apply(pred, MARGIN=1, find_classes)
  pred$class <- as.factor(pred$class)
  return(pred)
}


# fit_model <- function(model_method, model_metric, trControl_func, train_data, test_data, length=5, yname){

#   form = as.formula(paste(yname,'~.'))

#   model <<- train(form, 
#                   data=train_data, 
#                   method=model_method, 
#                   metric=model_metric, 
#                   trControl=trControl_func, 
#                   tuneLength=length)
  
#   predProbs <- predict(model, test_data, type="prob")
#   pred <- predict(model, test_data)
#   probsDataFrame <- data.frame(predProbs, test_data[, yname])
#   newpred <- predict_out_of_probabilities(predProbs, 0.5)

#   confM <<- confusionMatrix(pred, test_data[, yname], mode="everything")
#   return(list(matrix=confM, result=model, probDF=probsDataFrame))
# }

fit_model <- function(model_method, model_metric, trControl_func, tunegrid, train_data, test_data, length=5, yname){

  if(missing(tunegrid)) {

      form = as.formula(paste(yname,'~.'))

      model <<- train(form, 
                      data=train_data, 
                      method=model_method, 
                      metric=model_metric, 
                      trControl=trControl_func, 
                      tuneLength=length)

      pred <<- predict(model, test_data)
      confM <<- confusionMatrix(pred, test_data[, yname], mode="everything")
      proc <- roc(unclass(test_data[, yname]), unclass(pred))
      return(list(matrix=confM, result=model, roc=proc))
    } else {
      form = as.formula(paste(yname,'~.'))

      model <<- train(form, 
                      data=train_data, 
                      method=model_method, 
                      metric=model_metric, 
                      trControl=trControl_func, 
                      tuneGrid = tunegrid)
                      

      pred <<- predict(model, test_data)
      confM <<- confusionMatrix(pred, test_data[, yname], mode="everything")
      proc <- roc(unclass(test_data[, yname]), unclass(pred))
      return(list(matrix=confM, result=model, roc=proc))
    }
}

# create_model <- function(BD, 
#                          ycol=1, 
#                          min_classes, 
#                          maj_class, 
#                          TipoModelo='rpart', 
#                          model_metric='Kappa', 
#                          trn_ctrl_method='boot', 
#                          cp = 0.01, 
#                          kfold = 10, 
#                          prob_aug=0.9) {
#   # BD = as.data.frame(unclass(BD), stringsAsFactors = TRUE)
#   # BD = BD[complete.cases(BD),] #or BD = na.omit(BD)
#   yname <- colnames(BD[ycol])
#   #plot_correlation(BD)
#   #plot_prcomp(BD)
#   set.seed(1)
#   lista_treino <- createDataPartition(BD[,ycol], p=0.7, list=FALSE)
#   treino <<- BD[lista_treino,]
#   teste <<- BD[-lista_treino,]
#   treino.aug <<- treino
#   for(i in length(min_classes)){
#     n = nrow(treino[treino[yname]==maj_class,]) - nrow(treino[treino[yname]==min_classes[i],])
#     j=1
#     while(j<n){
#       cand = GenerateMultipleCandidates(data=treino, Class=min_classes[i], col=ycol, Prob=prob_aug, amount=1)
#       if(!anyNA(cand)){
#         treino.aug <<- rbind(treino.aug, cand)
#         j=j+1
#       }
#     }
#   }
#   form = as.formula(paste(yname,'~.'))
#   modelo <<- train(form , data=treino, method=TipoModelo, metric=model_metric, trControl = trainControl(method = trn_ctrl_method))
#   print(modelo)
#   pred <<- predict(modelo, teste)
#   confM <<- confusionMatrix(pred, teste[,ycol])
#   modelo.aug <<- train(form , data=treino.aug, method=TipoModelo, metric=model_metric, trControl = trainControl(method = trn_ctrl_method))
#   print(modelo.aug)
#   pred.aug <<- predict(modelo.aug, teste)
#   confM.aug <<- confusionMatrix(pred.aug, teste[,ycol])
# }



# tsne_df -> A dataset with x and y information for each  row
# rows -> a boolean vector sinalazing the rows where we want to calculate that distance

distance_to_centroids <- function(tsne_df, rows, pos_class, neg_class){

  filtered_df <- if(!hasArg(rows)) tsne_df else tsne_df[rows, ]
  # Defining the positive class centroid point
  pos_class_centroid_x <- mean(tsne_df[tsne_df["colour"] == pos_class, ]$x)
  pos_class_centroid_y <- mean(tsne_df[tsne_df["colour"] == pos_class, ]$y)

  # Defining the negative class centroid point
  neg_class_centroid_x <- mean(tsne_df[tsne_df["colour"] == neg_class, ]$x)
  neg_class_centroid_y <- mean(tsne_df[tsne_df["colour"] == neg_class, ]$y)

  # Calculating the euclidian distance
  d_pos_class <- ( (pos_class_centroid_x - filtered_df$x)^2  + (pos_class_centroid_y - filtered_df$y)^2  )^0.5
  d_neg_class <-  ( (neg_class_centroid_x - filtered_df$x)^2  + (neg_class_centroid_y - filtered_df$y)^2  )^0.5
  distances_df <- data.frame(
      id = rownames(filtered_df),
      d1 = d_pos_class,
      d2 = d_neg_class
  )

  return(distances_df)
    
}

outliers_checker <- function(distances, dataset, y) {

  # Defining varibles
  best_model <- list(alpha=NULL, Kappa=0, train=NULL, test=NULL)
  possible_alphas <- distances$d1 - distances$d2
  dmin <- floor(min(possible_alphas))
  possible_alphas <- append(possible_alphas, dmin)
  possible_alphas <- sort(possible_alphas)

  # Creating partitions
  set.seed(2)
  train_list <- createDataPartition(dataset[, y], p=0.7, list=FALSE)
  train <- dataset[train_list,]
  test <- dataset[-train_list,]
  rows <- rownames(train[train[, "outlier"] == TRUE, ])
  train_distances <- distances[distances$id  %in% rows, ]
  kappa_x_alpha <- data.frame(kappa=NULL, alpha=NULL, remaining_data=NULL)

  for (alpha in possible_alphas){
      train_copy <- train
      rejected <- !(train_distances$d2 + alpha <  train_distances$d1)
      train_copy[train_copy$outlier, ]$outlier <- train_copy[train_copy$outlier, ]$outlier & rejected

      train_copy <- train_copy[!train_copy$outlier, ]
      train_copy[, ncol(train_copy)] <- NULL
      #print(dim(train_copy))
      set.seed(2)
      model <- fit_model(  
          model_method="rpart",
          model_metric="Kappa",
          trControl_func = trainControl(method = "cv"),
          train_data = train_copy,
          test_data=test,
          yname=y,
          length = 3
      )
      kappa <-  model$matrix$overall["Kappa"]
      #print(kappa)
      kappa_x_alpha <- rbind(kappa_x_alpha, data.frame(kappa=unname(kappa), alpha=alpha))
      if(best_model$Kappa < kappa){
          best_model$alpha <- alpha
          best_model$Kappa <- kappa
          best_model$train <- train_copy
          best_model$test <- test
      }
  }
  return(list(best_model = best_model, kappa_x_alpha=kappa_x_alpha))
}

fbeta <- function (data, lev=NULL, model = NULL){
    fb_val <- FBeta_Score(data$obs, data$pred, beta = 0.5)
    c(FB = fb_val)
}


kmodes_seed <- function (df, k, max_iter, seed){
  set.seed(seed)
  kmode <- kmodes(df, k, iter.max = max_iter, weighted = FALSE)
  return(kmode)
}


f1 <- function(ths){
    
  diff <- abs(ths$Sensitivity - ths$Specificity)
  indexOfMin = match(min(diff), diff)
  return(ths[indexOfMin, "prob_threshold"])
}

f2 <- function(ths){
    desv <- function(x){
      sd(c(unname(x["Sensitivity"]), unname(x["Specificity"])))
    }
    avg <- (ths$Sensitivity + ths$Specificity)/2
    deviation <- apply(ths, desv, MARGIN=1)
    metric <- avg - deviation
    indexOfMin = match(max(metric), metric)
    return(ths[indexOfMin, "prob_threshold"])
}


f1_score <- function (data, lev=NULL, model = NULL){
    fb_val <- FBeta_Score(data$obs, data$pred, beta = 1)
    c(f1_score = fb_val)
}

create_cluster_histograms <- function(colors, names, df, best_k) {

    for(col_name in names){
      myplots <- list()
      
      lower <- min(ordered(df[, col_name]))
      upper <- max(ordered(df[, col_name]))
      if( lower == "0"){
        lower <- as.numeric(lower) - 1
        upper <- as.numeric(upper) - 1
      } else {
        lower <- as.numeric(lower)
        upper <- as.numeric(upper)
      }

      for(cl in 1:best_k){
          plt <- ggplot() + geom_bar( 
            color='black',
            data=df[df$cluster == cl,], 
            aes_string(x=col_name, "(..count..)*100/sum(..count..)"),
            fill=colors[cl],
            position=position_dodge()
          ) + ylab("Relative Frequency") + ylim(0, 100) + scale_x_discrete(limit = paste(c(lower:upper)))
          myplots[[cl]] <- plt 
      }
      wrap_plots(myplots)
      ggsave(path="profiles", file=paste(col_name, ".png", sep=""))
  }

}
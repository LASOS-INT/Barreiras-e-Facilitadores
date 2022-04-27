packs = c('ggplot2', 'cowplot', 'randomForest',
          'caret', 'rpart.plot', 'readxl',
          'e1071', 'AugmenterR', 'smotefamily',
          'ROSE', 'xgboost', 'pROC', 'klaR','patchwork', 'grid',
          'MASS', 'lsr', 'DescTools', 'devtools',
          'dplyr', 'kernlab', 'fastAdaboost', 
          'DataExplorer', 'dummies', 'lattice', 
          'mlbench', 'h2o', 'here', "rattle", "pracma",
           "MLmetrics", "ggfortify", "Rtsne", "obliqueRF", "gbm", "MLeval", "fmsb")


install_all_packages <- function () {
  lapply(packs, install.packages, character.only = T, logical.return = TRUE)
}


nearest_cluster <- function(modes, cluster){
    cluster_mode <- modes[cluster, ]
    diss_modes <- apply(modes, 1, function (row) sum(cluster_mode != row))

    return(names(diss_modes[-(cluster)])[which.min(diss_modes[-(cluster)])])
}



silhouette_values <- function(num_clusters, df, diss_matrix, iters, s){

      set.seed(s)
      kmode <- kmodes(df, num_clusters, iter.max = iters, weighted = FALSE)
      a <- c()
      b <- c()
      for(k in 1:num_clusters){
            cluster <- kmode$cluster == k
            nearest_k <- nearest_cluster(kmode$modes, k)

            n_cluster <-  kmode$cluster == as.integer(nearest_k)
            a_cluster <- rowSums(diss_matrix[cluster, cluster])/(sum(cluster)-1)

            b_cluster <- rowSums(diss_matrix[cluster, n_cluster])/(sum(n_cluster))
            a <- append(a, a_cluster)
            b <-  append(b, b_cluster)
      }
      silhouette_coefficient <- (b-a)/pmax(b, a)
      order <- as.character(sort(as.integer(names(silhouette_coefficient))))
      silhouette_coefficient <- silhouette_coefficient[order]
      silhouette_sc <- mean(silhouette_coefficient)


      return(list(silhouette_sc, silhouette_coefficient, kmode))


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
  set.seed(47)
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
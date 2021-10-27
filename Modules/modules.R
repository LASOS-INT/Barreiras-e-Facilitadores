packs = c('ggplot2', 'cowplot', 'randomForest',
          'caret', 'rpart.plot', 'readxl',
          'e1071', 'AugmenterR', 'smotefamily',
          'ROSE', 'xgboost', 'ROCR', 
          'MASS', 'lsr', 'DescTools', 
          'dplyr', 'kernlab', 'fastAdaboost', 
          'DataExplorer', 'dummies', 'lattice', 
          'mlbench', 'h2o', 'here', "rattle", "MLmetrics", "ggfortify", "Rtsne")


install_all_packages <- function () {
  lapply(packs, install.packages, character.only = T, logical.return = TRUE)
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
    print("entrei")
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


fit_model <- function(model_method, model_metric, trControl_func, train_data, test_data, length=5, yname){

  form = as.formula(paste(yname,'~.'))
  model <<- train(form , data=train_data, method=model_method, metric=model_metric, trControl = trControl_func, tuneLength = length)
  pred <<- predict(model, test_data)
  confM <<- confusionMatrix(pred, test_data[, yname])
  return(list(matrix=confM, result=model))
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
  best_model <- list(alpha=NULL, Kappa=0, dataset=NULL)
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
          best_model$remaining_data <-  c(rownames(train_copy), rownames(test) )
      }
  }
  return(list(best_model = best_model, kappa_x_alpha=kappa_x_alpha))
}

fbeta <- function (data, lev=NULL, model = NULL){
    fb_val <- FBeta_Score(data$obs, data$pred, positive="practice", beta = 0.1)
    c(FB = fb_val)
}
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
  for(i in length(min_classes)){
    n = nrow(train_data[train_data[yname]==maj_class,]) - nrow(train_data[train_data[yname]==min_classes[i],])
    j=0
    while(j<n){
      cand = GenerateMultipleCandidates(data=train_data, Class=min_classes[i], col=ycol, Prob=prob_aug, amount=1)
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

packs = c('ggplot2', 'cowplot', 'randomForest',
          'caret', 'rpart.plot', 'readxl',
          'e1071', 'AugmenterR', 'smotefamily',
          'ROSE', 'xgboost', 'ROCR', 
          'MASS', 'lsr', 'DescTools', 
          'dplyr', 'kernlab', 'fastAdaboost', 
          'DataExplorer', 'dummies', 'lattice', 
          'mlbench', 'h2o', 'here', "rattle")


install_all_packages <- function () {
  lapply(packs, install.packages, character.only = T, logical.return = TRUE)
}


load_library_packages <- function() {
  lapply(packs, library, character.only = T, logical.return = TRUE)
}


create_model <- function(BD, 
                         ycol=1, 
                         min_classes, 
                         maj_class, 
                         TipoModelo='rpart', 
                         model_metric='Kappa', 
                         trn_ctrl_method='boot', 
                         cp = 0.01, 
                         kfold = 10, 
                         prob_aug=0.9) {
  # BD = as.data.frame(unclass(BD), stringsAsFactors = TRUE)
  # BD = BD[complete.cases(BD),] #or BD = na.omit(BD)
  yname <- colnames(BD[ycol])
  #plot_correlation(BD)
  #plot_prcomp(BD)
  set.seed(1)
  lista_treino <- createDataPartition(BD[,ycol], p=0.7, list=FALSE)
  treino <<- BD[lista_treino,]
  teste <<- BD[-lista_treino,]
  treino.aug <<- treino
  for(i in length(min_classes)){
    n = nrow(treino[treino[yname]==maj_class,]) - nrow(treino[treino[yname]==min_classes[i],])
    j=1
    while(j<n){
      cand = GenerateMultipleCandidates(data=treino, Class=min_classes[i], col=ycol, Prob=prob_aug, amount=1)
      if(!anyNA(cand)){
        treino.aug <<- rbind(treino.aug, cand)
        j=j+1
      }
    }
  }
  form = as.formula(paste(yname,'~.'))
  modelo <<- train(form , data=treino, method=TipoModelo, metric=model_metric, trControl = trainControl(method = trn_ctrl_method))
  print(modelo)
  pred <<- predict(modelo, teste)
  confM <<- confusionMatrix(pred, teste[,ycol])
  modelo.aug <<- train(form , data=treino.aug, method=TipoModelo, metric=model_metric, trControl = trainControl(method = trn_ctrl_method))
  print(modelo.aug)
  pred.aug <<- predict(modelo.aug, teste)
  confM.aug <<- confusionMatrix(pred.aug, teste[,ycol])
}
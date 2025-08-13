library(e1071)
library(randomForest)
library(class)
library(caret)

#(Utility)---------------------------------------------------------------------

metrics_topic <- function(topic, numwords=FALSE, groupsize=FALSE, removewords=c(FALSE), type, k=1, P=floor(sqrt(71))) {
  if (type=='DA'){
    metric <- DALOOCV_topic(topic, numwords, groupsize, removewords)
  } else if (type=='RF'){
    metric <- RFLOOCV_topic(topic, numwords, groupsize, removewords, P)
  } else if (type=='KNN'){
    metric <- KNNLOOCV_topic(topic, numwords, groupsize, removewords, k=k)
  } else {
    stop('wrong type')
  }
  
  accuracy <- sum(metric[[1]] == metric[[2]]) / length(metric[[2]])
  
  confusion <- confusionMatrix(metric[[1]], metric[[2]])
  precision <- confusion$byClass['Precision']
  return(list(accuracy, precision))
}

metrics_whole <- function(topic, numwords=FALSE, groupsize=FALSE, removewords=FALSE, type, k=1, P=floor(sqrt(71))) {
  if (type=='DA'){
    metric <- DALOOCV_whole(topic, numwords, groupsize, removewords)
  } else if (type=='RF'){
    metric <- RFLOOCV_whole(topic, numwords, groupsize, removewords, P)
  } else if (type=='KNN'){
    metric <- KNNLOOCV_whole(topic, numwords, groupsize, removewords, k=k)
  } else {
    stop('wrong type')
  }
  accuracy <- sum(metric[[1]] == metric[[2]]) / length(metric[[2]])
  confusion <- confusionMatrix(metric[[1]], metric[[2]])
  precision <- confusion$byClass['Precision']
  return(list(accuracy, precision))
}

whole_data_organiser <- function(topics) {
  
  features <- combine_topics(topics)
  
  humanfeatures <- features[[1]]
  GPTfeatures <- features[[2]]
  
  for (i in 1:(length(humanM$features))){
    if (any(topics == i)){
      #print(c(i, topics))
    } else {
      humanfeatures <- rbind(humanfeatures, humanM$features[[i]])
    }
  }
  
  for (i in 1:(length(GPTM$features))){
    if (any(topics == i)){
      #print(c(i, topic))
    } else {
      GPTfeatures <- rbind(GPTfeatures, GPTM$features[[i]])
    }
  }
  
  # Combine the data frames
  features <- list()
  features[[1]] <- humanfeatures
  features[[2]] <- GPTfeatures
  return(features)
}

combine_topics <- function(topics) {
  humanfeatures <- NULL
  GPTfeatures <- NULL
  
  for (i in topics){
    humanfeatures <- rbind(humanfeatures, humanM$features[[i]])
  }
  
  for (i in topics){
    GPTfeatures <- rbind(GPTfeatures, GPTM$features[[i]])
  }
  
  # Combine the data frames
  features <- list()
  features[[1]] <- humanfeatures
  features[[2]] <- GPTfeatures
  return(features)
}

distinct_data_organiser <- function(testtopic, traintopics) {
  
  test <- list()
  test[[1]] <- humanM$features[[testtopic]]
  test[[2]] <- GPTM$features[[testtopic]]
  
  humantrain <- NULL
  GPTtrain <- NULL
  
  for (traintopic in traintopics){
    humantrain <- rbind(humantrain, humanM$features[[traintopic]])
    GPTtrain <- rbind(GPTtrain, GPTM$features[[testtopic]])
  }
  
  train <- list()
  train[[1]] <- humantrain
  train[[2]] <- GPTtrain
  
  return(list(test, train))
}

group_traindata <- function(traindata, groupsize) {
  human_traindata <- traindata[[1]]
  GPT_traindata <- traindata[[2]]
  
  human_folds <- createFolds(1:nrow(human_traindata), k = groupsize, list = TRUE)
  GPT_folds <- createFolds(1:nrow(GPT_traindata), k = groupsize, list = TRUE)
  
  human_groups <- lapply(human_folds, function(indices) colSums(human_traindata[indices, , drop = FALSE]))
  GPT_groups <- lapply(GPT_folds, function(indices) colSums(GPT_traindata[indices, , drop = FALSE]))
  
  new_traindata <- traindata
  new_traindata[[1]] <- do.call(rbind, human_groups)
  new_traindata[[2]] <- do.call(rbind, GPT_groups)
  
  return(new_traindata)
}

norm <- function(features) {
  normalisedfeatures <- features
  for (i in 1:nrow(normalisedfeatures)) {
    normalisedfeatures[i,] <- normalisedfeatures[i, ]/sum(normalisedfeatures[i, ])
  }
  return(normalisedfeatures)
}

summed_data <- function(topics){
  
  humanfeatures <- NULL
  GPTfeatures <- NULL
  
  for (topic in topics) {
    humanfeatures <- rbind(humanfeatures, apply(humanM$features[[topic]],2,sum))
    GPTfeatures <- rbind(GPTfeatures, apply(GPTM$features[[topic]],2,sum))
  }
  
  summedfeatures <- list()
  summedfeatures[[1]] <- humanfeatures
  summedfeatures[[2]] <- GPTfeatures
  return(summedfeatures)
}

remove_functionwords <- function(features, functionwords) {
  
  for (i in 1:nrow(features[[1]])){
    features[[1]][i,71] <- features[[1]][i,71] + sum(features[[1]][i, functionwords])
  }
  
  for (i in 1:nrow(features[[2]])){
    features[[2]][i,71] <- features[[2]][i,71] + sum(features[[2]][i, functionwords])
  }
  
  
  features[[1]] <- features[[1]][,-functionwords,drop=FALSE]
  features[[2]] <- features[[2]][,-functionwords,drop=FALSE]
  return(features)
}



#(Topic Specific)---------------------------------------------------------------

KNNLOOCV_topic <- function(topics, numwords=FALSE, groupsize=FALSE, removewords=c(FALSE), k) {
  
  # Combine the data frames
  features <- combine_topics(topics)
  
  if (removewords[1]==FALSE){
  } else {
    features <- remove_functionwords(features, removewords)
  }
  
  KNNpredictions <- NULL
  truth <- NULL
  
  for (i in 1:length(features)) {
    for (j in 1:nrow(features[[i]])) {
      
      if (numwords == FALSE) {
        testdata <- norm(matrix(features[[i]][j,],nrow=1))
      } else {
        testdata <- norm(reducewords(matrix(features[[i]][j,],nrow=1), numwords))
      }
      
      traindata <- features
      traindata[[i]] <- traindata[[i]][-j,,drop=FALSE]
      
      if (groupsize == FALSE) {
      } else {
        traindata <- group_traindata(traindata, groupsize)
      }
      
      trainlist <- norm(rbind((traindata[[1]]), (traindata[[2]])))
      authornames <- c(rep(1,nrow(traindata[[1]])), rep(2,nrow(traindata[[2]])))
      
      pred <- myKNN(trainlist, testdata, as.factor(authornames), k=k)
      KNNpredictions <- c(KNNpredictions, pred)
      truth <- c(truth, i)
    }
  }
  
  KNNpredictions <- factor(KNNpredictions, levels = 1:2)
  truth <- factor(truth, levels = 1:2)
  KNNmetric <- list()
  KNNmetric[[1]] <- KNNpredictions
  KNNmetric[[2]] <- truth
  return(KNNmetric)
}

DALOOCV_topic <- function(topics, numwords=FALSE, groupsize=FALSE, removewords=c(FALSE)) {
  
  # Combine the data frames
  features <- combine_topics(topics)
  
  if (removewords[1]==FALSE){
  } else {
    features <- remove_functionwords(features, removewords)
  }
  
  DApredictions <- NULL
  truth <- NULL
  traindata <- features
  
  for (i in 1:length(features)) {
    for (j in 1:nrow(features[[i]])) {

      if (numwords == FALSE) {
        testdata <- matrix(features[[i]][j,],nrow=1)
      } else {
        testdata <- reducewords(matrix(features[[i]][j,],nrow=1), numwords)
      }
      
      traindata <- features
      traindata[[i]] <- traindata[[i]][-j,,drop=FALSE]
      
      if (groupsize == FALSE) {
      } else {
        traindata <- group_traindata(traindata, groupsize)
      }
      
      pred <- discriminantCorpus(traindata, testdata)
      DApredictions <- c(DApredictions, pred)
      truth <- c(truth, i)
    }
  }
  DApredictions <- factor(DApredictions, levels = 1:2)
  truth <- factor(truth, levels = 1:2)
  DAmetric <- list()
  DAmetric[[1]] <- DApredictions
  DAmetric[[2]] <- truth
  return(DAmetric)
}

RFLOOCV_topic <- function(topics, numwords=FALSE, groupsize=FALSE, removewords=c(FALSE), P) {

  # Combine the data frames
  features <- combine_topics(topics)
  
  if (removewords[1]==FALSE){
  } else {
    features <- remove_functionwords(features, removewords)
  }
  
  RFpredictions <- NULL
  truth <- NULL
  traindata <- features
  
  for (i in 1:length(features)) {
    for (j in 1:nrow(features[[i]])) {

      if (numwords == FALSE) {
        testdata <- matrix(features[[i]][j,],nrow=1)
      } else {
        testdata <- reducewords(matrix(features[[i]][j,],nrow=1), numwords)
      }
      
      traindata <- features
      traindata[[i]] <- traindata[[i]][-j,,drop=FALSE]
      
      if (groupsize == FALSE) {
      } else {
        traindata <- group_traindata(traindata, groupsize)
      }
      
      pred <- randomForestCorpus(traindata, testdata, P)
      RFpredictions <- c(RFpredictions, pred)
      truth <- c(truth, i)
    }
  }
  RFpredictions <- factor(RFpredictions, levels = 1:2)
  truth <- factor(truth, levels = 1:2)
  RFmetric <- list()
  RFmetric[[1]] <- RFpredictions
  RFmetric[[2]] <- truth
  return(RFmetric)
}

SVMLOOCV_topic <- function(topics, numwords=FALSE, groupsize=FALSE, removewords=c(FALSE), kernel='linear', cost=0.1, degree=3, gamma=1/71, coef0 = 0) {
  
  # Combine the data frames
  features <- combine_topics(topics)
  
  if (removewords[1]==FALSE){
  } else {
    features <- remove_functionwords(features, removewords)
  }
  
  SVMpredictions <- NULL
  truth <- NULL
  traindata <- features
  
  for (i in 1:length(features)) {
    for (j in 1:nrow(features[[i]])) {
      
      if (numwords == FALSE) {
        testdata <- matrix(features[[i]][j,],nrow=1)
      } else {
        testdata <- reducewords(matrix(features[[i]][j,],nrow=1), numwords)
      }
      
      traindata <- features
      traindata[[i]] <- traindata[[i]][-j,,drop=FALSE]
      
      if (groupsize == FALSE) {
      } else {
        traindata <- group_traindata(traindata, groupsize)
      }
      
      trainlist <- norm(rbind(traindata[[1]], traindata[[2]]))
      authornames <- c(rep(1,nrow(traindata[[1]])), rep(2,nrow(traindata[[2]])))
      
      for (j in 1:70)
      if (all(trainlist[,j]==0)){
        trainlist[1,j] <- 0.0001
      }
      
      if (kernel=='linear') {
        svmfit <- svm(trainlist, as.factor(authornames), kernel="linear", cost=0.1)
      } else if (kernel=='polynomial') {
        svmfit <- svm(trainlist, as.factor(authornames), kernel="polynomial", gamma=gamma, coef0=coef0, degree=degree)
      } else if (kernel=='radial'){
        svmfit <- svm(trainlist, as.factor(authornames), kernel="radial", cost=0.1, gamma=gamma)
      } else if (kernel=='sigmoid'){
        svmfit <- svm(trainlist, as.factor(authornames), kernel="sigmoid", cost=0.1, gamma=gamma, coef0=coef0)
      }
      
      colnames(testdata) <- paste0("V", 1:ncol(testdata))
      testdata <- norm(testdata)
      pred <- predict(svmfit, testdata)
      SVMpredictions <- c(SVMpredictions, pred[[1]])
      truth <- c(truth, i)
    }
  }
  SVMpredictions <- factor(SVMpredictions, levels = 1:2)
  truth <- factor(truth, levels = 1:2)
  SVMmetric <- list()
  SVMmetric[[1]] <- SVMpredictions
  SVMmetric[[2]] <- truth
  return(SVMmetric)
}



#(Whole Set)---------------------------------------------------------------------

DALOOCV_whole <- function(topics, numwords=FALSE, groupsize=FALSE, removewords=c(FALSE)) {
  
  features <- whole_data_organiser(topics)
  # Combine the data frames
  topic_specific <- combine_topics(topics)
  topic_length <- c(nrow(topic_specific[[1]]), nrow(topic_specific[[2]]))
  
  if (removewords[1]==FALSE){
  } else {
    features <- remove_functionwords(features, removewords)
  }
  
  DApredictions <- NULL
  truth <- NULL
  traindata <- features
  
  for (i in 1:length(features)) {
    for (j in 1:(topic_length[i])) {
      
      if (numwords == FALSE) {
        testdata <- matrix(features[[i]][j,],nrow=1)
      } else {
        testdata <- reducewords(matrix(features[[i]][j,],nrow=1), numwords)
      }
      
      traindata <- features
      traindata[[i]] <- traindata[[i]][-j,,drop=FALSE]
      
      if (groupsize == FALSE) {
      } else {
        traindata <- group_traindata(traindata, groupsize)
      }
      
      pred <- discriminantCorpus(traindata, testdata)
      DApredictions <- c(DApredictions, pred)
      truth <- c(truth, i)
    }
  }
  DApredictions <- factor(DApredictions, levels = 1:2)
  truth <- factor(truth, levels = 1:2)
  DAmetric <- list()
  DAmetric[[1]] <- DApredictions
  DAmetric[[2]] <- truth
  return(DAmetric)
}

RFLOOCV_whole <- function(topics, numwords=FALSE, groupsize=FALSE, removewords=c(FALSE), P=floor(sqrt(71))) {
  
  features <- whole_data_organiser(topics)
  # Combine the data frames
  topic_specific <- combine_topics(topics)
  topic_length <- c(nrow(topic_specific[[1]]), nrow(topic_specific[[2]]))
  print(topic_length)
  
  if (removewords[1]==FALSE){
  } else {
    features <- remove_functionwords(features, removewords)
  }
  
  RFpredictions <- NULL
  truth <- NULL
  traindata <- features
  
  for (i in 1:length(features)) {
    for (j in 1:(topic_length[i])) {
      
      if (numwords == FALSE) {
        testdata <- matrix(features[[i]][j,],nrow=1)
      } else {
        testdata <- reducewords(matrix(features[[i]][j,],nrow=1), numwords)
      }
      
      traindata <- features
      traindata[[i]] <- traindata[[i]][-j,,drop=FALSE]
      
      if (groupsize == FALSE) {
      } else {
        traindata <- group_traindata(traindata, groupsize)
      }
      
      pred <- randomForestCorpus(traindata, testdata, P)
      RFpredictions <- c(RFpredictions, pred)
      truth <- c(truth, i)
    }
  }
  RFpredictions <- factor(RFpredictions, levels = 1:2)
  truth <- factor(truth, levels = 1:2)
  RFmetric <- list()
  RFmetric[[1]] <- RFpredictions
  RFmetric[[2]] <- truth
  return(RFmetric)
}

KNNLOOCV_whole <- function(topics, numwords=FALSE, groupsize=FALSE, removewords=c(FALSE), k=1) {
  
  features <- whole_data_organiser(topics)
  # Combine the data frames
  topic_specific <- combine_topics(topics)
  topic_length <- c(nrow(topic_specific[[1]]), nrow(topic_specific[[2]]))
  print(topic_length)
  
  if (removewords[1]==FALSE){
  } else {
    features <- remove_functionwords(features, removewords)
  }
  
  KNNpredictions <- NULL
  truth <- NULL
  traindata <- features
  
  for (i in 1:length(features)) {
    for (j in 1:(topic_length[i])) {
      
      if (numwords == FALSE) {
        testdata <- norm(matrix(features[[i]][j,],nrow=1))
      } else {
        testdata <- norm(reducewords(matrix(features[[i]][j,],nrow=1), numwords))
      }
      
      traindata <- features
      traindata[[i]] <- traindata[[i]][-j,,drop=FALSE]
      
      if (groupsize == FALSE) {
      } else {
        traindata <- group_traindata(traindata, groupsize)
      }
      
      trainlist <- norm(rbind((traindata[[1]]), (traindata[[2]])))
      authornames <- c(rep(1,nrow(traindata[[1]])), rep(2,nrow(traindata[[2]])))
      
      pred <- myKNN(trainlist, testdata, authornames, k=k)
      KNNpredictions <- c(KNNpredictions, pred)
      truth <- c(truth, i)
    }
  }
  
  KNNpredictions <- factor(KNNpredictions, levels = 1:2)
  truth <- factor(truth, levels = 1:2)
  KNNmetric <- list()
  KNNmetric[[1]] <- KNNpredictions
  KNNmetric[[2]] <- truth
  return(KNNmetric)
}

SVMLOOCV_whole <- function(topics, numwords=FALSE, groupsize=FALSE, removewords=c(FALSE), kernel='linear', cost=0.1, degree=3, gamma=1/71, coef0 = 0) {
  
  features <- whole_data_organiser(topics)
  # Combine the data frames
  topic_specific <- combine_topics(topics)
  topic_length <- c(nrow(topic_specific[[1]]), nrow(topic_specific[[2]]))
  print(topic_length)
  
  if (removewords[1]==FALSE){
  } else {
    features <- remove_functionwords(features, removewords)
  }
  
  SVMpredictions <- NULL
  truth <- NULL
  traindata <- features
  
  for (i in 1:length(features)) {
    for (j in 1:(topic_length[i])) {
      
      if (numwords == FALSE) {
        testdata <- matrix(features[[i]][j,],nrow=1)
      } else {
        testdata <- reducewords(matrix(features[[i]][j,],nrow=1), numwords)
      }
      
      traindata <- features
      traindata[[i]] <- traindata[[i]][-j,,drop=FALSE]
      
      if (groupsize == FALSE) {
      } else {
        traindata <- group_traindata(traindata, groupsize)
      }
      
      trainlist <- norm(rbind(traindata[[1]], traindata[[2]]))
      authornames <- c(rep(1,nrow(traindata[[1]])), rep(2,nrow(traindata[[2]])))
      
      if (kernel=='linear') {
        svmfit <- svm(trainlist, as.factor(authornames), kernel="linear", cost=0.1)
      } else if (kernel=='polynomial') {
        svmfit <- svm(trainlist, as.factor(authornames), kernel="polynomial", cost=0.1, degree=degree, gamma=gamma, coef0=coef0)
      } else if (kernel=='radial'){
        svmfit <- svm(trainlist, as.factor(authornames), kernel="radial", cost=0.1, gamma=gamma)
      } else if (kernel=='sigmoid'){
        svmfit <- svm(trainlist, as.factor(authornames), kernel="sigmoid", cost=0.1, gamma=gamma, coef0=coef0)
      }
      
      colnames(testdata) <- paste0("V", 1:ncol(testdata))
      testdata <- norm(testdata)
      pred <- predict(svmfit, testdata)
      SVMpredictions <- c(SVMpredictions, pred[[1]])
      truth <- c(truth, i)
    }
  }
  SVMpredictions <- factor(SVMpredictions, levels = 1:2)
  truth <- factor(truth, levels = 1:2)
  SVMmetric <- list()
  SVMmetric[[1]] <- SVMpredictions
  SVMmetric[[2]] <- truth
  return(SVMmetric)
}



#(Distinct Traindata and Testdata)----------------------------------------------

DALOOCV_distinct <- function(testtopic, traintopics, numwords=FALSE, groupsize=FALSE, removewords=c(FALSE)) {
  
  features <- distinct_data_organiser(testtopic, traintopics)
  
  test <- features[[1]]
  train <- features[[2]]
  
  if (removewords[1]==FALSE){
  } else {
    test <- remove_functionwords(test, removewords)
    train <- remove_functionwords(train, removewords)
  }
  
  DApredictions <- NULL
  truth <- NULL

  for (i in 1:length(train)) {
    for (j in 1:nrow(test[[i]])) {
      
      if (numwords == FALSE) {
        testdata <- matrix(test[[i]][j,],nrow=1)
      } else {
        testdata <- reducewords(matrix(test[[i]][j,],nrow=1), numwords)
      }
      
      traindata <- train
      
      if (groupsize == FALSE) {
      } else {
        traindata <- group_traindata(traindata, groupsize)
      }
      
      pred <- discriminantCorpus(traindata, testdata)
      DApredictions <- c(DApredictions, pred)
      truth <- c(truth, i)
    }
  }
  DApredictions <- factor(DApredictions, levels = 1:2)
  truth <- factor(truth, levels = 1:2)
  DAmetric <- list()
  DAmetric[[1]] <- DApredictions
  DAmetric[[2]] <- truth
  return(DAmetric)
}

RFLOOCV_distinct <- function(testtopic, traintopics, numwords=FALSE, groupsize=FALSE, removewords=c(FALSE), P=floor(sqrt(71))) {
  
  features <- distinct_data_organiser(testtopic, traintopics)
  
  test <- features[[1]]
  train <- features[[2]]
  
  if (removewords[1]==FALSE){
  } else {
    test <- remove_functionwords(test, removewords)
    train <- remove_functionwords(train, removewords)
  }
  
  RFpredictions <- NULL
  truth <- NULL
  
  for (i in 1:length(train)) {
    for (j in 1:nrow(test[[i]])) {
      
      if (numwords == FALSE) {
        testdata <- matrix(test[[i]][j,],nrow=1)
      } else {
        testdata <- reducewords(matrix(test[[i]][j,],nrow=1), numwords)
      }
      
      traindata <- train
      
      if (groupsize == FALSE) {
      } else {
        traindata <- group_traindata(traindata, groupsize)
      }
      
      pred <- randomForestCorpus(traindata, testdata, P)
      RFpredictions <- c(DApredictions, pred)
      truth <- c(truth, i)
    }
  }
  RFpredictions <- factor(DApredictions, levels = 1:2)
  truth <- factor(truth, levels = 1:2)
  RFmetric <- list()
  RFmetric[[1]] <- RFpredictions
  RFmetric[[2]] <- truth
  return(RFmetric)
}

KNNLOOCV_distinct <- function(testtopic, traintopics, numwords=FALSE, groupsize=FALSE, removewords=c(FALSE), k=1) {
  
  features <- distinct_data_organiser(testtopic, traintopics)
  
  test <- features[[1]]
  train <- features[[2]]
  
  if (removewords[1]==FALSE){
  } else {
    test <- remove_functionwords(test, removewords)
    train <- remove_functionwords(train, removewords)
  }
  
  RFpredictions <- NULL
  truth <- NULL
  
  for (i in 1:length(test)) {
    for (j in 1:nrow(test[[i]])) {
      
      if (numwords == FALSE) {
        testdata <- norm(matrix(test[[i]][j,],nrow=1))
      } else {
        testdata <- norm(reducewords(matrix(test[[i]][j,],nrow=1), numwords))
      }
      
      traindata <- train
      
      if (groupsize == FALSE) {
      } else {
        traindata <- group_traindata(traindata, groupsize)
      }
      
      trainlist <- norm(rbind((traindata[[1]]), (traindata[[2]])))
      authornames <- c(rep(1,nrow(traindata[[1]])), rep(2,nrow(traindata[[2]])))
      
      pred <- myKNN(trainlist, testdata, authornames, k=k)
      KNNpredictions <- c(KNNpredictions, pred)
      truth <- c(truth, i)
    }
  }
  RFpredictions <- factor(DApredictions, levels = 1:2)
  truth <- factor(truth, levels = 1:2)
  RFmetric <- list()
  RFmetric[[1]] <- RFpredictions
  RFmetric[[2]] <- truth
  return(RFmetric)
}

SVMLOOCV_distinct <- function(testtopic, traintopics, numwords=FALSE, groupsize=FALSE, removewords=c(FALSE), kernel='linear', cost=0.1, degree=3, gamma=1/71, coef0 = 0) {
  
  features <- distinct_data_organiser(testtopic, traintopics)
  
  test <- features[[1]]
  train <- features[[2]]
  
  if (removewords[1]==FALSE){
  } else {
    test <- remove_functionwords(test, removewords)
    train <- remove_functionwords(train, removewords)
  }
  
  SVMpredictions <- NULL
  truth <- NULL
  
  for (i in 1:length(train)) {
    for (j in 1:nrow(test[[i]])) {
      
      if (numwords == FALSE) {
        testdata <- matrix(test[[i]][j,],nrow=1)
      } else {
        testdata <- reducewords(matrix(test[[i]][j,],nrow=1), numwords)
      }
      
      traindata <- train
      
      if (groupsize == FALSE) {
      } else {
        traindata <- group_traindata(traindata, groupsize)
      }
      
      trainlist <- norm(rbind(traindata[[1]], traindata[[2]]))
      authornames <- c(rep(1,nrow(traindata[[1]])), rep(2,nrow(traindata[[2]])))
      
      if (kernel=='linear') {
        svmfit <- svm(trainlist, as.factor(authornames), kernel="linear", cost=0.1)
      } else if (kernel=='polynomial') {
        svmfit <- svm(trainlist, as.factor(authornames), kernel="polynomial", cost=0.1, degree=degree, gamma=gamma, coef0=coef0)
      } else if (kernel=='radial'){
        svmfit <- svm(trainlist, as.factor(authornames), kernel="radial", cost=0.1, gamma=gamma)
      } else if (kernel=='sigmoid'){
        svmfit <- svm(trainlist, as.factor(authornames), kernel="sigmoid", cost=0.1, gamma=gamma, coef0=coef0)
      }
      
      colnames(testdata) <- paste0("V", 1:ncol(testdata))
      testdata <- norm(testdata)
      pred <- predict(svmfit, testdata)
      SVMpredictions <- c(SVMpredictions, pred[[1]])
      truth <- c(truth, i)
    }
  }
  SVMpredictions <- factor(SVMpredictions, levels = 1:2)
  truth <- factor(truth, levels = 1:2)
  SVMmetric <- list()
  SVMmetric[[1]] <- SVMpredictions
  SVMmetric[[2]] <- truth
  return(SVMmetric)
}



#(MDS)--------------------------------------------------------------------------

MDS_summed <- function(topics) {
  tempfeatures <- summed_data(topics)
  features <- rbind(tempfeatures[[1]], tempfeatures[[2]])
  authornames <- c(rep(1,nrow(tempfeatures[[1]])), rep(2,nrow(tempfeatures[[2]])))
  MDS_numbered(features, authornames)
}

MDS_topic <- function(topics) {
  
  humanfeatures <- NULL
  GPTfeatures <- NULL
  
  for (topic in topics) {
    humanfeatures <- rbind(humanfeatures, humanM$features[[topic]])
    GPTfeatures <- rbind(GPTfeatures, GPTM$features[[topic]])
  }
  
  features <- rbind(humanfeatures, GPTfeatures)
  authornames <- c(rep(1,nrow(humanfeatures)), rep(2,nrow(GPTfeatures)))
  
  MDS_general(features, authornames)
}

MDS_general <- function(features, authornames) {
  
  x <- features
  
  for (i in 1:nrow(x)) {
    x[i,] <- x[i,] / sum(x[i,])
  }
  for (j in 1:ncol(x)) {
    x[,j] <- (x[,j] - mean(x[,j]))/sd(x[,j])
  }
  
  d <- dist(x)
  pts <- cmdscale(d)
  plot(pts)
  
  inds1 <- which(authornames==1)
  points(pts[inds1,],col='red', pch=18, lty=1)
  inds2 <- which(authornames==2)
  points(pts[inds2,],col='blue', pch=18, lty=2)
  
  legend(1, 95, legend=c("Human", "ChatGPT"),
         col=c("red", "blue"), lty=1:2, cex=0.8)
  
  
}

MDS_numbered <- function(features, authornames) {
  
  x <- features
  
  for (i in 1:nrow(x)) {
    x[i,] <- x[i,] / sum(x[i,])
  }
  for (j in 1:ncol(x)) {
    x[,j] <- (x[,j] - mean(x[,j]))/sd(x[,j])
  }
  
  d <- dist(x)
  pts <- cmdscale(d)
  plot(pts)
  
  
  #matrix_data <- as.matrix(d)
  
  #matrix_data[matrix_data == 0] <- NA
  
  #matrix_vector <- na.omit(as.vector(matrix_data))
  
  #smallest_values <- sort(matrix_vector)[1:30]
  
  #for (value in smallest_values){
  #  print(which(matrix_data == value, arr.ind = TRUE))
  #}
  
  
  # Calculate the distance from the first 110 vectors to the vectors 110 elements away
  n_vectors <- nrow(x)
  distances <- vector("list", 110)  # List to store the distances
  
  # Loop over the first 110 vectors
  for (i in 1:110) {
    # Index of the vector that is 110 elements away
    j <- i + 110
    
    # Ensure the second vector is within bounds
    if (j <= n_vectors) {
      # Calculate the distance between the i-th vector and the (i+110)-th vector
      dist_ij <- dist(rbind(x[i,], x[j,]))
      distances[i] <- dist_ij
    }
  }
  
  
  
  inds1 <- which(authornames==1)
  points(pts[inds1,],col='red', pch=18, lty=1)
  inds2 <- which(authornames==2)
  points(pts[inds2,],col='blue', pch=18, lty=2)
  
  title(main='MDS of Each Topic Grouping Summed')
  
  legend(4,7, legend=c("Human", "ChatGPT"),
         col=c("red", "blue"), pch=c(18, 18), lty=1:2, cex=0.8)
  
  
  
  return(distances)
}

class_dist <- function(topics){
  
  tempfeatures <- summed_data(topics)
  features <- rbind(tempfeatures[[1]], tempfeatures[[2]])
  authornames <- c(rep(1,nrow(tempfeatures[[1]])), rep(2,nrow(tempfeatures[[2]])))
  
  x <- features
  
  for (i in 1:nrow(x)) {
    x[i,] <- x[i,] / sum(x[i,])
  }
  for (j in 1:ncol(x)) {
    x[,j] <- (x[,j] - mean(x[,j]))/sd(x[,j])
  }
  
  d <- dist(x)
  pts <- cmdscale(d)
  plot(pts)
  
  # Calculate the distance from the first 110 vectors to the vectors 110 elements away
  n_vectors <- nrow(x)
  distances <- vector("list", 110)  # List to store the distances
  
  # Loop over the first 110 vectors
  for (i in 1:110) {
    # Index of the vector that is 110 elements away
    j <- i + 110
    
    # Ensure the second vector is within bounds
    if (j <= n_vectors) {
      # Calculate the distance between the i-th vector and the (i+110)-th vector
      dist_ij <- dist(rbind(x[i,], x[j,]))
      distances[i] <- dist_ij
    }
  }
  
  return(distances)
}

interclass_dist <- function(features)


#()
#(Plots)------------------------------------------------------------------------

KNNkplot <- function(topics, numwords=FALSE, groupsize=FALSE, removewords=FALSE, n, samples) {
  
  basic_score <- numeric(n)
  
  for (k in 1:n) {
    
    tempaccuracy <- numeric(samples)
    temppreciscion <- numeric(samples)
    
    for (sample in 1:samples) {
      
      KNNmetric <- KNNLOOCV_topic(topics, numwords, groupsize, removewords, k)
      
      tempaccuracy[sample] <- sum(KNNmetric[[1]]==KNNmetric[[2]])/length(KNNmetric[[2]])
      
    }
    basic_score[k] <- mean(tempaccuracy)
  }
  
  plot(1:n, basic_score, type='b', pch=19, col='blue', 
       xlab='k (Number of Neighbors)', ylab='Accuracy', 
       main='KNN Basic Score vs k')
}

RFPplot_topic <- function(topics, numwords=FALSE, groupsize=FALSE, removewords=FALSE, n, samples) {
  
  basic_score <- numeric(n)
  
  for (k in 1:n) {
    
    tempaccuracy <- numeric(samples)
    temppreciscion <- numeric(samples)
    
    for (sample in 1:samples) {
      
      KNNmetric <- RFLOOCV_topic(topic, numwords, groupsize, removewords, P=k)
      
      tempaccuracy[sample] <- sum(KNNmetric[[1]]==KNNmetric[[2]])/length(KNNmetric[[2]])
      print(sample)
    }
    basic_score[k] <- mean(tempaccuracy)
  }
  
  return(basic_score)
}

removewordplot_topic <- function(topic, numwords=FALSE, groupsize=FALSE, type, n, samples) {
  
}




#(Given Functions)--------------------------------------------------------------

reducewords <- function(features, numwords) {
  newfeatures <- features
  numfeatures <- ncol(features)
  
  for (i in 1:nrow(features)) {
    prob <- features[i,]/sum(features[i,])
    temp <- sample(1:numfeatures, numwords,replace=TRUE,prob=prob)
    counts <- numeric(numfeatures)
    for (j in 1:length(temp)) {
      counts[temp[j]] <- counts[temp[j]] + 1
    }
    newfeatures[i,] <- counts
  }
  return(newfeatures)
}

loadCorpus <- function(filedir,featureset="functionwords",maxauthors=Inf) {
  authornames <- list.files(filedir)
  booknames <- list()
  features <- list()
  count <- 0
  
  for (i in 1:length(authornames)) {
    #print(i)
    if (count >= maxauthors) {break}
    files <- list.files(sprintf("%s%s/",filedir,authornames[i]))
    if (length(files)==0) {next}
    
    firstbook <- FALSE
    booknames[[i]] <- character()
    for (j in 1:length(files)) {
      path <- sprintf("%s%s/%s",filedir,authornames[i],files[j])
      
      fields <- strsplit(files[j],split=' --- ')[[1]]  
      
      if (sprintf("%s.txt",featureset) == fields[2]) {
        booknames[[i]] <- c(booknames[[i]], fields[1])
        count <- count+1
        M <- as.matrix(read.csv(path,sep=',',header=FALSE))  
        if (firstbook == FALSE) {
          firstbook <- TRUE
          features[[i]] <- M
        } else {
          features[[i]]  <- rbind(features[[i]],M)
        }
        
      }
    }
  }
  return(list(features=features,booknames=booknames,authornames=authornames))
}

myKNN <- function(traindata, testdata, trainlabels, k=1) {
  if (mode(traindata) == 'numeric' && !is.matrix(traindata)) {
    traindata <- matrix(traindata,nrow=1)
  }
  if (mode(testdata) == 'numeric' && !is.matrix(testdata)) {
    testdata <- matrix(testdata,nrow=1)
  }
  
  mus <- apply(traindata,2,mean) 
  sigmas <- apply(traindata,2,sd)
  sigmas <- as.vector(sigmas)
  sigmas[sigmas == 0] <- 1
  
  #print(sigmas)
  #print(traindata)
  
  for (i in 1:ncol(traindata)) {
    traindata[,i] <- (traindata[,i] - mus[i])/sigmas[i]
  }
  
  for (i in 1:ncol(testdata)) {
    testdata[,i] <- (testdata[,i]-mus[i])/sigmas[i]
  }
  
  #print(traindata)
  #print(testdata)
  #print(length(trainlabels))
  
  preds <- knn(traindata, testdata, trainlabels, k)
  return(preds)
}

discriminantCorpus <- function(traindata, testdata) {
  thetas <- NULL
  preds <- NULL
  
  #first learn the model for each author
  for (i in 1:length(traindata)) {
    words <- apply(traindata[[i]],2,sum)
    inds <- which(words==0) 
    if (length(inds) > 0) {words[inds] <- 1}
    thetas <- rbind(thetas, words/sum(words))
  }
  
  #now classify
  for (i in 1:nrow(testdata)) {
    probs <- NULL
    for (j in 1:nrow(thetas)) {
      probs <- c(probs, dmultinom(testdata[i,],prob=thetas[j,],log=TRUE))
    }
    preds <- c(preds, which.max(probs))
  }
  return(preds)
}

KNNCorpus <- function(traindata, testdata, k=1) {
  train <- NULL
  for (i in 1:length(traindata)) {
    train <- rbind(train, apply(traindata[[i]],2,sum))
  }
  
  for (i in 1:nrow(train)) {
    train[i,] <- train[i,]/sum(train[i,])
  }
  for (i in 1:nrow(testdata)) {
    testdata[i,] <- testdata[i,]/sum(testdata[i,])
  }
  trainlabels <- 1:nrow(train)
  myKNN(train, testdata, trainlabels, k)
}

randomForestCorpus <- function(traindata, testdata, P) {
  x <- NULL
  y <- NULL
  for (i in 1:length(traindata)) {
    x <- rbind(x,traindata[[i]])
    y <- c(y,rep(i,nrow(traindata[[i]])))
  }
  
  for (i in 1:nrow(x)) {
    x[i,] <- x[i,]/sum(x[i,])
  }
  
  for (i in 1:nrow(testdata)) {
    testdata[i,] <- testdata[i,]/sum(testdata[i,])
  }
  
  mus <- apply(x,2,mean)
  sigmas <- apply(x,2,sd)
  sigmas <- as.vector(sigmas)
  sigmas[sigmas == 0] <- 1
    
  
  for (j in 1:ncol(x)) {
    x[,j] <- (x[,j] - mus[j])/sigmas[j]
    testdata[,j] <- (testdata[,j] - mus[j])/sigmas[j]
  }
  
  y <- as.factor(y)
  rf <- randomForest(x,y,mtry=P)
  
  preds <- numeric(nrow(testdata))
  for (i in 1:nrow(testdata)) {
    preds[i] <- predict(rf,testdata[i,])
  }
  return(preds)
}



#(Useless Functions Storage)----------------------------------------------------

KNNLOOCV_storage <- function(topic, numwords=FALSE, k) {
  
  humanfeatures <- humanM$features[[topic]] #select the essays on this particular topic
  GPTfeatures <- GPTM$features[[topic]]
  features <- rbind(humanfeatures, GPTfeatures)
  print(features)
  authornames <- c(rep(1,nrow(humanfeatures)), rep(2,nrow(GPTfeatures)))
  
  train <- NULL
  test <- NULL
  KNNpredictions <- NULL
  truth <- NULL
  
  for (i in 1:nrow(features)) {
    
    train <- norm(features[-i,])
    
    if (numwords==FALSE) {
      test <- norm(features[i,,drop=FALSE])
    }
    else {
      test <- norm(reducewords(features[i,,drop=FALSE], numwords))
    }
    
    pred <- myKNN(train,test,authornames[-i],k=k)
    KNNpredictions <- c(KNNpredictions, pred)
    truth <- c(truth, authornames[i])
  }
  print(c(authornames[-nrow(features)], train))
  
  KNNpredictions <- factor(KNNpredictions, levels = 1:2)
  truth <- factor(truth, levels = 1:2)
  KNNmetric <- list()
  KNNmetric[[1]] <- KNNpredictions
  KNNmetric[[2]] <- truth
  return(KNNmetric)
}

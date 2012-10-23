thesis.algo.neuralnet <- function(dataset, correct_only = FALSE, test = "validation", ret = "accuracy")
{
  data.count <- nrow(dataset)
  data.training.count <- floor(data.count*0.6)
  
  
  # was_failed should be included
  data.inputs <- subset(dataset, select=c(-was_failed,-was_due,-user_word_id,-user_id,-word_id,-was_new,-created_at,-updated_at,-user_role,-time_to_answer,-correct,-user_rated_answer))
  if(correct_only)
  {
    data.outputs <- subset(dataset, select=c(correct))
    print(data.outputs)
    # need to convert to numeric
    for(i in 1:nrow(data.outputs))
    {
      if(data.outputs[i,"correct"] == "true")
      {
        data.outputs[i,"correcta"] <- 1
      }else{
        data.outputs[i,"correcta"] <- 0
      }
    }
    data.outputs <- subset(data.outputs, select=c(correcta))
    print(data.outputs)
  }else{
    data.outputs <- subset(dataset, select=c(user_rated_answer))
  }
  
  data.training.inputs <- data.inputs[1:data.training.count,]
  data.training.outputs <- data.outputs[1:data.training.count,]
  
  data.validation.inputs <- data.inputs[(data.training.count+1):data.count,]
  data.validation.outputs <- data.outputs[(data.training.count+1):data.count,]
  
  # SVM
  library(e1071)
  model <- nnet(data.training.inputs, data.training.outputs, size=5, skip=FALSE, linout=TRUE)
  
  if(test == "validation")
  {
    data.test_set.inputs <- data.validation.inputs
    data.test_set.outputs <- data.validation.outputs
  }else if(test == "training"){
    data.test_set.inputs <- data.training.inputs
    data.test_set.outputs <- data.training.outputs
  }
  pred <- round(predict(model, data.test_set.inputs))
  print(table(pred,t(data.test_set.outputs)))
  
  valid = 0
  invalid = 0
  
  if(correct_only)
  {
    for(i in 1:length(data.test_set.outputs))
    {
      if(data.test_set.outputs[i] == pred[i])
      {
        valid <- valid + 1
      }else{
        invalid <- invalid + 1
      }
    }
  }else{
    # User rated answer
    for(i in 1:length(data.test_set.outputs))
    {
      
      class = data.test_set.outputs[i]
      if(data.test_set.outputs[i] <= 2) {
        if(pred[i] == 2 || pred[i] == 1 || pred[i] == 0)
        {
          valid <- valid + 1
        }else{
          invalid <- invalid + 1
        }
      }else{
        if(pred[i] == 5 || pred[i] == 4 || pred[i] == 3)
        {
          valid <- valid + 1
        }else{
          invalid <- invalid + 1
        }
      }
    }
  }
  accuracy <- (valid/(valid+invalid))*100
  print(accuracy)
  if(ret == "accuracy")
  {
    return(accuracy)
  }else{
    return(model)
  }
  
  #return(model)
}

nnmodel <- thesis.algo.neuralnet(data, correct_only=TRUE)


thesis.algo.svm <- function(dataset, correct_only = FALSE, svm.kernel="radial", test="validation")
{
  data.count <- nrow(dataset)
  data.training.count <- floor(data.count*0.6)
  
  correct_only = FALSE
  
  # was_failed should be included
  data.inputs <- subset(dataset, select=c(-was_failed,-was_due,-user_word_id,-user_id,-word_id,-was_new,-created_at,-updated_at,-user_role,-time_to_answer,-correct,-user_rated_answer))
  if(correct_only)
  {
    data.outputs <- subset(dataset, select=c(correct))
  }else{
    data.outputs <- subset(dataset, select=c(user_rated_answer))
  }
  
  data.training.inputs <- data.inputs[1:data.training.count,]
  data.training.outputs <- data.outputs[1:data.training.count,]
  
  data.validation.inputs <- data.inputs[(data.training.count+1):data.count,]
  data.validation.outputs <- data.outputs[(data.training.count+1):data.count,]
  
  # SVM
  library(e1071)
  model <- svm(data.training.inputs, data.training.outputs, type='C', kernel=svm.kernel)
  
  if(test == "validation")
  {
    data.test_set.inputs <- data.validation.inputs
    data.test_set.outputs <- data.validation.outputs
  }else if(test == "training"){
    data.test_set.inputs <- data.training.inputs
    data.test_set.outputs <- data.training.outputs
  }
  pred <- predict(model, data.test_set.inputs)
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
  return(accuracy)
}
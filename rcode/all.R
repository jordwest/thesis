setwd("~/Dropbox/University/Thesis/thesis/rcode")
source("svm.R")
source("nn.R")

# Load CSV
data_in <- read.csv("not_new.csv")

tests.svm.linear.training <- dim(5)
tests.svm.linear.validation <- dim(5)
tests.svm.radial.training <- dim(5)
tests.svm.radial.validation <- dim(5)
tests.neuralnet.training <- dim(5)
tests.neuralnet.validation <- dim(5)

#res <- thesis.algo.neuralnet(data, test="validation")

for(i in 1:5)
{
  # Randomise order of data
  data <- data_in[sample(nrow(data_in)),]

  tests.svm.linear.training[i] <- thesis.algo.svm(data, correct_only = TRUE, test="training", svm.kernel="linear")
  tests.svm.linear.validation[i] <- thesis.algo.svm(data, correct_only = TRUE, test="validation", svm.kernel="linear")
  tests.svm.radial.training[i] <- thesis.algo.svm(data, correct_only = TRUE, test="training", svm.kernel="radial")
  tests.svm.radial.validation[i] <- thesis.algo.svm(data, correct_only = TRUE, test="validation", svm.kernel="radial")
  tests.neuralnet.training[i] <- thesis.algo.neuralnet(data, correct_only = TRUE, test="training")
  tests.neuralnet.validation[i] <- thesis.algo.neuralnet(data, correct_only = TRUE, test="validation")
}
print("Average linear svm training accuracy: ")
print(mean(tests.svm.linear.training))
print("Average linear svm validation accuracy: ")
print(mean(tests.svm.linear.validation))

print("Average radial svm training accuracy: ")
print(mean(tests.svm.radial.training))
print("Average radial svm validation accuracy: ")
print(mean(tests.svm.radial.validation))

print("Average neural net training accuracy: ")
print(mean(tests.svm.radial.training))
print("Average neural net validation accuracy: ")
print(mean(tests.svm.radial.validation))
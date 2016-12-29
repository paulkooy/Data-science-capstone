memory <- character(12)

memory[1] <- object.size(testModel("sample1.txt",5,5,NULL))
memory[2] <- object.size(testModel("sample2.txt",5,5,NULL))
memory[3] <- object.size(testModel("sample3.txt",5,5,NULL))
memory[4] <- object.size(testModel("sample1.txt",25,5,NULL))
memory[5] <- object.size(testModel("sample2.txt",25,5,NULL))
memory[6] <- object.size(testModel("sample3.txt",25,5,NULL))
memory[7] <- object.size(testModel("sample1.txt",50,5,NULL))
memory[8] <- object.size(testModel("sample2.txt",50,5,NULL))
memory[9] <- object.size(testModel("sample3.txt",50,5,NULL))
memory[10] <- object.size(testModel("sample1.txt",100,5,NULL))
memory[11] <- object.size(testModel("sample2.txt",100,5,NULL))
memory[12] <- object.size(testModel("sample3.txt",100,5,NULL))

memory
#   Script to plot the model testing results
library(ggplot2)

#   Load the model test results
url <- "../data/model/modelTests2.csv"
if (file.exists(url)) {
    modelResults <- read.csv(url)
} else {
    print("No model test results found found.\n")
}

#   Plot the result
ggplot(modelResults, aes(predictScore, execTime, color = textSample, shape = factor(maxNgrams), size = factor(nGramsUsed)), geom = c("point")) + 
    geom_point(aes(color = textSample)) + 
    ggtitle("Prediction score [% correct] versus execution time [seconds]") +
    theme_gray() +
    theme(text = element_text(size = 15))

ggsave("../data/model/modelResults2.png")
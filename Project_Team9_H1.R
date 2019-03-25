#Load the csv Data (files should be present in getwd())
data <- read.csv("C:\\Surya\\Concordia\\SOEN 6611\\Project\\Milestone4\\ApacheAnt\\Dataset_ApacheAnt.csv")
#data <- read.csv("C:\\Surya\\Concordia\\SOEN 6611\\Project\\Milestone4\\JFreeChart\\Dataset_JFreeChart.csv")
#data <- read.csv("C:\\Surya\\Concordia\\SOEN 6611\\Project\\Milestone4\\JMeter\\Dataset_JMeter.csv")



#Assign to input data form from each column
input <- data[,c("ConditionalTestLogic","EmptyTest","GeneralFixture","EagerTest","LazyTest","Potential_Bugs")]

#Summary of Data frame
summary(input)


#Calculation of Spearman coefficient between dependent and independent variables
res1 <-cor(x=input$Potential_Bugs, y=input$ConditionalTestLogic,  method = "pearson")
# for displaying the correlation results
cat("Potential_Bugs vs ConditionalTestLogic = ", res1, "\n")
#plot(res1,xlab='Potential_Bugs' , ylab='ConditionalTestLogic' )
res2 <-cor(x=input$Potential_Bugs, y=input$EmptyTest,  method = "pearson")
cat("Potential_Bugs vs EmptyTest = ", res2, "\n")
#plot(res2,xlab='Potential_Bugs' , ylab='EmptyTest' )
res3 <-cor(x=input$Potential_Bugs, y=input$GeneralFixture,  method = "pearson")
cat("Potential_Bugs vs GeneralFixture = ", res3, "\n")
#plot(res3,xlab='Potential_Bugs' , ylab='GeneralFixture' )
res4 <-cor(x=input$Potential_Bugs, y=input$EagerTest,  method = "pearson")
cat("Potential_Bugs vs EagerTest = ", res4, "\n")
#plot(res4,xlab='Potential_Bugs' , ylab='EagerTest' )
res5 <-cor(x=input$Potential_Bugs, y=input$LazyTest,  method = "pearson")
cat("Potential_Bugs vs LazyTest = ", res5, "\n")
#plot(res5,xlab='Potential_Bugs' , ylab='LazyTest' )


#Building model with less correlated independent variables
model = glm(Potential_Bugs ~ ConditionalTestLogic + EmptyTest+ GeneralFixture + EagerTest+ LazyTest, data = input, family = binomial)
print(summary(model))


model1 <- step(model)
print(summary(model1))


#Load the csv Data
data <- read.csv("C:\\Users\\abhi\\Desktop\\6611_csvFiles\\New test\\Jmeter\\Dataset_JMeter.csv")

#Assign to input data form from each column
input <- data[,c("ConditionalTestLogic","EmptyTest","GeneralFixture","EagerTest","LazyTest","CountDeclClassMethod","SumCyclomatic","MaxInheritanceTree","CountClassDerived","CountClassCoupled","CountDeclMethodAll","PercentLackOfCohesion","AvgCyclomatic","AvgCyclomaticModified","AvgCyclomaticStrict","CountLineCode","Potential_Bugs")]


#Summary of Data frame
summary(input)


#Calculation of Spearman coefficient between dependent and independent variables
res1 <-cor(x=input$EagerTest, y=input$CountDeclClassMethod,  method = "spearman")
# for displaying the correlation results
cat("EagerTest vs CountDeclClassMethod = ", res1, "\n")


res2 <-cor(x=input$EagerTest, y=input$SumCyclomatic,  method = "spearman")
cat("EagerTest vs SumCyclomatic = ", res2, "\n")


res3 <-cor(x=input$EagerTest, y=input$MaxInheritanceTree,  method = "spearman")
cat("EagerTest vs MaxInheritanceTree= ", res3, "\n")

res4 <-cor(x=input$EagerTest, y=input$CountClassDerived,  method = "spearman")
cat("EagerTest vs CountClassDerived= ", res4, "\n")

res5 <-cor(x=input$EagerTest, y=input$CountClassCoupled,  method = "spearman")
cat("EagerTest vs Sum_Cyclomatic= ", res5, "\n")

res6 <-cor(x=input$EagerTest, y=input$CountDeclMethodAll,  method = "spearman")
cat("EagerTest vs CountDeclMethodAll= ", res6, "\n")

res7 <-cor(x=input$EagerTest, y=input$PercentLackOfCohesion,  method = "spearman")
cat("EagerTest vs PercentLackOfCohesion= ", res7, "\n")

res8 <-cor(x=input$EagerTest, y=input$AvgCyclomatic,  method = "spearman")
cat("EagerTest vs AvgCyclomatic = ", res8, "\n")

res9 <-cor(x=input$EagerTest, y=input$AvgCyclomaticModified,  method = "spearman")
cat("EagerTest vs AvgCyclomaticModified = ", res9, "\n")

res10 <-cor(x=input$EagerTest, y=input$AvgCyclomaticStrict,  method = "spearman")
cat("EagerTest vs AvgCyclomaticStrict = ", res10, "\n")

res11 <-cor(x=input$EagerTest, y=input$CountLineCode,  method = "spearman")
cat("EagerTest vs Avg_Cyclomatic_Modified = ", res11, "\n")

#Building model with less correlated independent variables
model = glm( EagerTest ~ CountDeclClassMethod+SumCyclomatic+MaxInheritanceTree+CountClassDerived+CountClassCoupled+CountDeclMethodAll+PercentLackOfCohesion+AvgCyclomatic+AvgCyclomaticModified+AvgCyclomaticStrict+CountLineCode,data = input, family = binomial)
print(summary(model))

model2<-step(model)
summary(model2)
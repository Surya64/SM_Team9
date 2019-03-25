#Load the csv Data
data <- read.csv("C:\\Users\\abhi\\Desktop\\6611_csvFiles\\New test\\Jmeter\\Dataset_JMeter.csv")

#Assign to input data form from each column
input <- data[,c("ConditionalTestLogic","EmptyTest","GeneralFixture","EagerTest","LazyTest","CountDeclClassMethod","SumCyclomatic","MaxInheritanceTree","CountClassDerived","CountClassCoupled","CountDeclMethodAll","PercentLackOfCohesion","AvgCyclomatic","AvgCyclomaticModified","AvgCyclomaticStrict","CountLineCode","Potential_Bugs")]


#Summary of Data frame
summary(input)



#Calculation of Spearman coefficient between dependent and independent variables
res1 <-cor(x=input$LazyTest, y=input$CountDeclClassMethod,  method = "spearman")
# for displaying the correlation results
cat("LazyTest vs CountDeclClassMethod = ", res1, "\n")


res2 <-cor(x=input$LazyTest, y=input$SumCyclomatic,  method = "spearman")
cat("LazyTest vs SumCyclomatic = ", res2, "\n")


res3 <-cor(x=input$LazyTest, y=input$MaxInheritanceTree,  method = "spearman")
cat("LazyTest vs MaxInheritanceTree= ", res3, "\n")

res4 <-cor(x=input$LazyTest, y=input$CountClassDerived,  method = "spearman")
cat("LazyTest vs CountClassDerived= ", res4, "\n")

res5 <-cor(x=input$LazyTest, y=input$CountClassCoupled,  method = "spearman")
cat("LazyTest vs Sum_Cyclomatic= ", res5, "\n")

res6 <-cor(x=input$LazyTest, y=input$CountDeclMethodAll,  method = "spearman")
cat("LazyTest vs CountDeclMethodAll= ", res6, "\n")

res7 <-cor(x=input$LazyTest, y=input$PercentLackOfCohesion,  method = "spearman")
cat("LazyTest vs PercentLackOfCohesion= ", res7, "\n")

res8 <-cor(x=input$LazyTest, y=input$AvgCyclomatic,  method = "spearman")
cat("LazyTest vs AvgCyclomatic = ", res8, "\n")

res9 <-cor(x=input$LazyTest, y=input$AvgCyclomaticModified,  method = "spearman")
cat("LazyTest vs AvgCyclomaticModified = ", res9, "\n")

res10 <-cor(x=input$LazyTest, y=input$AvgCyclomaticStrict,  method = "spearman")
cat("LazyTest vs AvgCyclomaticStrict = ", res10, "\n")

res11 <-cor(x=input$LazyTest, y=input$CountLineCode,  method = "spearman")
cat("LazyTest vs Avg_Cyclomatic_Modified = ", res11, "\n")



model = glm( LazyTest ~ CountDeclClassMethod+SumCyclomatic+MaxInheritanceTree+CountClassDerived+CountClassCoupled+CountDeclMethodAll+PercentLackOfCohesion+AvgCyclomatic+AvgCyclomaticModified+AvgCyclomaticStrict+CountLineCode,data = input, family = binomial)
print(summary(model))


model2<-step(model)
summary(model2)
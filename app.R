# Load packages ----
library(shiny)
library(dplyr)
library(readr)
library(purrr) # Thanks Hadley
library(ggplot2) 
library(rpart)
library(rpart.plot)
library(randomForest)
library(pROC)
library(rsconnect)

# Read data ----
titanic <- read_csv("titanic.csv")
titanic <- titanic %>% select(-ticket,-cabin,-embarked,-boat,-body,-home.dest,-parch,-name) %>%
  mutate(survived = as.factor(survived), sex=as.factor(sex)) %>% 
  filter(!is.na(titanic$survived)) %>%
  # Just so we can run the RF
  filter(complete.cases(.))

# Test/control ----
set.seed(1234)
sample_frac <- sample(1:nrow(titanic),0.75*nrow(titanic))
ti_train <- titanic[sample_frac,]
ti_test <- titanic[-sample_frac,]
#prop.table(table(ti_train$survived))
#prop.table(table(ti_test$survived))

# Simple rpart and rF models ----
r1<-rpart(survived ~ ., data=ti_train)
r2<-randomForest(survived ~ ., data=ti_train)
#rpart.plot(r1)

# Generate predictions ----
ti_test$pred1<-predict(r1, newdata=select(ti_test,-survived), type="prob")[,2]
#p2<-predict(r2, data.matrix(select(ti_test,-survived)))

# Plot ROC using pROC package ----
rc<-roc(ti_test$survived,ti_test$pred1)
#plot(rc)

# Testing ROC calculation logic ----
makeROCdata <- function(response,predicts,cutoff_prob) {
response<-as.integer(response)
results<-list(preds=sapply(predicts,function(x) ifelse(x>cutoff_prob,1,0)))
results$resp<-(response - min(response)) / (max(response)-min(response))

# Feels wrong but in a "ooohhhh you like it" kind of way
results$trps<-mapply(function(r,p) {ifelse(p==1 && r==1,1,0)}, r=results$resp, p=results$preds )
results$faps<-mapply(function(r,p) {ifelse(p==1 && r==0,1,0)}, r=results$resp, p=results$preds )
results$trns<-mapply(function(r,p) {ifelse(p==0 && r==0,1,0)}, r=results$resp, p=results$preds )
results$fans<-mapply(function(r,p) {ifelse(p==0 && r==1,1,0)}, r=results$resp, p=results$preds )

list(cutoff=cutoff_prob,
     truepos=sum(results$trps),
     trueneg=sum(results$trns),
     falsepos=sum(results$faps),
     falseneg=sum(results$fans),
     accuracy=(sum(results$trps)+sum(results$trns))/(sum(results$trps)+sum(results$trns)+sum(results$faps)+sum(results$fans)),
     sensitivity=sum(results$trps)/(sum(results$trps)+sum(results$fans)),
     specificity=sum(results$trns)/(sum(results$trns)+sum(results$faps)),
     fpr=sum(results$faps)/(sum(results$faps)+sum(results$trns)),
     f1score=(2*sum(results$trps))/(2*sum(results$trps)+sum(results$faps)+sum(results$fans))
     )
}

# Quick function test ----
#makeROCdata(ti_test$survived,ti_test$pred1,0.1)$f1score

# Run the ROC logic across cutoff range----
ROCdata<-map(seq(0.00,1.00,by = 0.01), makeROCdata, response= ti_test$survived, predicts=ti_test$pred1) %>%
  # Basically just guessed at this crazy logic - wtf??
  transpose() %>% 
  map(unlist) %>%
  as.data.frame() %>%
  mutate(cutoff=round(cutoff,2))

# Recreate basic ROC plots ----
plot(ROCdata$sensitivity~ROCdata$specificity, type="l", col="blue", xlim=c(1,0))
abline(1,-1)

ggplot(data=ROCdata) + 
  geom_abline(slope = 1, intercept = 1, alpha=0.75, colour="black") +
  #geom_hline(yintercept = select(filter(ROCdata,cutoff==input$cutoff_select),sensitivity), alpha=0.75, colour="grey", linetype="dotted") +
  #geom_vline(xintercept = select(filter(ROCdata,cutoff==input$cutoff_select),specificity), alpha=0.75, colour="grey", linetype="dotted") +
  geom_path(mapping=aes(x=specificity,y=sensitivity), colour="blue") + 
  geom_point(mapping = aes(x=select(filter(ROCdata,cutoff==input$cutoff_select),specificity),y=select(filter(ROCdata,cutoff==input$cutoff_select),sensitivity)),size=1.3) +
  scale_x_reverse() +
  geom_label(mapping = aes(x=select(filter(ROCdata,cutoff==input$cutoff_select),specificity),y=select(filter(ROCdata,cutoff==input$cutoff_select),sensitivity),label=input$cutoff_select), nudge_x=0.1) + 
  labs(x="Specificity",y="Sensitivity")



# Ok - now let's try and build this mofo!! ---- 
server <- function(input, output) {
   
 output$summary <- renderText({
    paste0("Selected cutoff: ", input$cutoff_select)
  })
 
 output$acc <- renderText({
   paste0("Model Accuracy: ", round(100*select(filter(ROCdata,cutoff==input$cutoff_select),accuracy)[[1]],2),"%")
 })
 
 output$f1stat <- renderText({
   paste0("Model F1: ", round(100*select(filter(ROCdata,cutoff==input$cutoff_select),f1score)[[1]],2),"%")
 })
 
 output$confmatrix <- renderTable({
   tn<-select(filter(ROCdata,cutoff==input$cutoff_select),trueneg)[[1]]
   fn<-select(filter(ROCdata,cutoff==input$cutoff_select),falseneg)[[1]]
   fp<-select(filter(ROCdata,cutoff==input$cutoff_select),falsepos)[[1]]
   tp<-select(filter(ROCdata,cutoff==input$cutoff_select),truepos)[[1]]
   cm=tibble(predicted=c("Neg","Neg","Pos","Pos"),actual=c("Neg","Pos","Neg","Pos"), values = c(tn,fp,fn,tp))
   tapply(cm$values,list(cm$predicted,cm$actual),sum)
 }, rownames = T, colnames = T, digits = 0)
 
 output$ROCplot <- renderPlot({
   plot(ROCdata$sensitivity~ROCdata$specificity, type="l", col="blue", xlim=c(1,0))
   abline(1,-1)
   abline(v=select(filter(ROCdata,cutoff==input$cutoff_select),specificity))
   abline(h=select(filter(ROCdata,cutoff==input$cutoff_select),sensitivity))
   text(x=select(filter(ROCdata,cutoff==input$cutoff_select),specificity)+0.05,
        y=select(filter(ROCdata,cutoff==input$cutoff_select),sensitivity)+0.05,
        labels = as.character(input$cutoff_select), bg="white")
 })
 
 output$F1plot <- renderPlot({
   plot(ROCdata$f1score~ROCdata$cutoff, type="l", col="red")
   abline(v=input$cutoff_select)
   par(new=T)
   plot(ROCdata$accuracy~ROCdata$cutoff, type="l", col="blue", xaxt="n",yaxt="n",xlab="",ylab="")
   axis(4)
   mtext("Accuracy",side=4,line=3)
   text(x=input$cutoff_select+0.02,
        y=0.07,
        labels = as.character(input$cutoff_select), bg="white")
   legend("bottomleft",col=c("blue","red"),lty=1,legend=c("F1 Score","Accuracy"))
   
 })
}

# UI section ----
ui<-  fluidPage(
    title="Interactive ROC",
    titlePanel("RB : Interactive ROC"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("cutoff_select", strong("Prediction Cutoff:"), min = 0.00, max = 1.00, value = 0.5)
        ,verbatimTextOutput("summary")
        ,tableOutput("confmatrix")
        ,br()
        ,verbatimTextOutput("acc")
        ,verbatimTextOutput("f1stat")
      ),
      mainPanel(
        plotOutput("ROCplot", width = "80%")
        ,plotOutput("F1plot", width = "80%")
      )
    )
  )

shinyApp(ui = ui, server = server)
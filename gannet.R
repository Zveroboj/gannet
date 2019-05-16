setwd("C:/Users/Owner/Documents/Consulting/ds_modeling_challenge/assets")

suppressPackageStartupMessages(suppressWarnings(library(dplyr)))
suppressPackageStartupMessages(suppressWarnings(library(neuralnet)))
suppressPackageStartupMessages(suppressWarnings(library(MASS)))
suppressPackageStartupMessages(suppressWarnings(library(nnet)))
suppressPackageStartupMessages(suppressWarnings(library(ggplot2)))
suppressPackageStartupMessages(suppressWarnings(library(caret)))
suppressPackageStartupMessages(suppressWarnings(library(randomForest)))
suppressPackageStartupMessages(suppressWarnings(library(rpart.plot)))
suppressPackageStartupMessages(suppressWarnings(library(partykit)))
suppressPackageStartupMessages(suppressWarnings(library(rpart)))
suppressPackageStartupMessages(suppressWarnings(library(forecast)))
suppressPackageStartupMessages(suppressWarnings(library(esquisse)))
suppressPackageStartupMessages(suppressWarnings(library(lmtest)))
suppressPackageStartupMessages(suppressWarnings(library(mfx)))
suppressPackageStartupMessages(suppressWarnings(library(corrplot)))

####Data####

dta <- read.csv("data.csv", na.strings = c(""), header = F) 
head(dta,5)
str(dta)

nms <- read.table("header.txt", sep = ",")
names(dta) <- nms[,1]

dta <- dta[complete.cases(dta),]


####Exploration####

esquisse::esquisser(dta) #create a tableau like experience

library(corrplot)
corrplot::corrplot(cor(dta[,c(1,3:8,10)]),
                   method='shade',shade.col = NA,
                   addCoef.col = 'black',addgrid.col='grey',
                   order = 'original',number.cex = .5,diag = F,
                   number.font = 2,cl.cex = 0.25,
                   addCoefasPercent=F,
                   p.mat = as.matrix(rcorr(as.matrix(dta[,c(1,3:8,10)]),
                                           type="spearman")$P),
                   sig.level = 0.05,
                   insig = c("blank"),
                   #col = RColorBrewer::brewer.pal(6,"Blues"),
                   tl.col = 'navy',tl.srt = 90,tl.cex = .7,
                   cl.pos='n',na.label = 'square',na.label.col = 'white')



####T-Test####

p.v <- c()
Variable <-c()
for (i in c(1,3:6,8,10)) {
  tt  <- t.test(dta[,i] ~ churn, 
                data = dta,
                paired=F,
                alternative="two.sided") # test of means difference in mpg by am
  p.v <- c(p.v,tt$p.value)
  Variable <- c(Variable, names(dta)[i])
  
}
report.t <- data.frame(cbind(Variable,round(as.numeric(as.character(p.v)),4)))
names(report.t)[2]<-"p-value"
head(report.t[order(-rank(report.t$`p-value`)),])
knitr::kable(report.t[order(rank(report.t$`p-value`)),])

report.t$p.v <- as.numeric(as.character(report.t$`p-value`))
summary(dta$client_state)
mosaic.tb<-aggregate(churn ~ client_state, 
                     data = data.frame(dta[dta$client_state!="WY"&
                                             dta$client_state!="WV"&
                                             dta$client_state!='ND'&
                                             dta$client_state!='VT',]), mean)


head(mosaic.tb[order(-rank(mosaic.tb$churn)),])
mosaicplot(client_state~churn,color=c(3,2),
           data = data.frame(dta[dta$client_state!="WY"&
                                   dta$client_state!="WV"&
                                   dta$client_state!='ND'&
                                   dta$client_state!='VT',]))

####ANOVA####

a1 <- aov(churn ~ factor(dta$BC), data = dta,
          contrasts = factor(dta$BC) )

f<-unlist(summary(a1))
f<-as.data.frame(matrix(f,2,5,byrow = F))
f<-cbind(Name=c("Business Category","Residuals"),f)
names(f)<-c("","Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")

tukey<-TukeyHSD(a1)

t.df<-tukey$`factor(dta$BC)`
t.df<-cbind(rownames(t.df),data.frame(t.df))
names(t.df)[1]<-"Business Category"
t.df<-data.frame(t.df,row.names = NULL)
t.df$Business.Category<-gsub("-"," vs. ",t.df$Business.Category)

head(t.df[order(rank(t.df$p.adj)),],15)
t.df2 <- t.df[t.df$p.adj<=0.05,]
knitr::kable(t.df2[order(rank(t.df2$p.adj)),])

####Proportion test####
#prop.test can be used for testing the null that the proportions (probabilities of success) in several groups are the same, or that they equal certain given values. 
names(dta)
dta.p <- dta[,c("client_state","churn")]
head(dta.p)

df2 <- dta.p %>% group_by(client_state) %>% summarise_all(length)
dta.p2 <- as.data.frame(df2)
dta.p2 <- dta.p2[order(rank(dta.p2$churn)),]

ggplot(dta.pc, aes(y=churn, x=reorder(BC, churn)))+geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


dta.pc <- dta.p[dta.p$BC!='Government & Politics'&
                  dta.p$BC!='Business Opportunities'&
                  dta.p$BC!="Community, Garage Sales & Organizations" &
                  dta.p$BC!='Religion & Spirituality'&
                  dta.p$BC!='Farming & Agriculture'&
                  dta.p$BC!='Career & Employment'&
                  dta.p$BC!='Toys & Hobbies',] 

dta.pc1 <- as.data.frame(dta.pc)
dta.pc1$BC<-as.character(dta.pc1$BC)
dta.tbl <- table(dta.pc1$BC, dta.pc1$churn)
dta.tt <- table(dta$client_state,factor(dta$churn))

pr.t <- prop.test(dta.tt, p = NULL,
          alternative = c("two.sided"),
          conf.level = 0.95, correct = TRUE)

est.pr <- as.data.frame(pr.t$estimate)
est.pr$BC <-row.names(dta.tt)
names(est.pr)[1] <- "Proportion of Retained"

est.pr <- dplyr::select(est.pr, BC, `Proportion of Retained`)
est.pr <- data.frame(est.pr, row.names = NULL)
est.pr <- est.pr[order(-rank(est.pr$Proportion.of.Retained)),]
head(est.pr,5); tail(est.pr,5)

ggplot(est.pr, aes(y=Proportion.of.Retained, x=reorder(BC, Proportion.of.Retained)))+
  geom_bar(stat = 'identity', col='grey')+ 
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x='States', y='Proportion of Retained Clients')

p.v.prop<-pr.t$p.value
p.v.prop

####Logit####
dta.lg <- dta[,c(1,3:8,10)]
for (i in 1:dim(dta.lg)[2]){
  dta.lg[,i]<-as.numeric(dta.lg[,i])
  
}

library(PerformanceAnalytics)

chart.Correlation(dta.lg, 
                  method="spearman",
                  histogram=TRUE,
                  pch=16)

library(psych)

corr.test(dta.lg, 
          use = "pairwise",
          method="spearman",
          adjust="none",      # Can adjust p-values; see ?p.adjust for options
          alpha=.05)

#Build a null model
model.null <- glm(churn ~ 1, 
                  data=dta.lg,
                  family = binomial(link="logit")
)

#Build a complete model

model.full <- glm(churn ~ .,
                  data=dta.lg,
                  family = binomial(link="logit")
)



#look for the best fit model:
best.fit<-step(model.null,
               scope = list(upper=model.full),
               direction="both",
               test="Chisq",trace = F,
               data=dta.lg)

final.formula<-best.fit$formula

model.final<-glm(formula = final.formula, family = binomial(link = "logit"), 
                 data = dta.lg)
summary(model.final)
as.data.frame(summary(model.final)$coef)

##Overall p-value for model
anova(model.null,
      model.final, 
      test="Chisq")

##Pseudo-R-squared

McFadden.pseudo.R<-1-(as.numeric(logLik(model.final)/logLik(model.null)))
paste(round(McFadden.pseudo.R*100,2),"%", sep = '')


library(rcompanion)
fdf<-rcompanion::nagelkerke(model.final)
min(fdf$Pseudo.R.squared.for.model.vs.null)
max(fdf$Pseudo.R.squared.for.model.vs.null)

## Marginal effects show the change in probability 
#  when the predictor or independent variable increases by one unit. 

prob<-logitmfx(churn ~ . , data=dta.lg) 
as.data.frame(prob$mfxest)
prob.table<-cbind("Change in probability of Y"=prob$mfxest[,1],
                  "Lower Confidence Limit"=prob$mfxest[,1]-1.96*prob$mfxest[,2],
                  "Upper Confidence Limit"=prob$mfxest[,1]+1.96*prob$mfxest[,2],
                  "P-value" = prob$mfxest[,4])

prob.table<-as.data.frame(prob.table)
prob.table<-data.frame(cbind(row.names(prob$mfxest),prob.table),row.names = NULL)

names(prob.table)<-c("Variables","Change in probability of Y","Lower CI Limit",
                     "Upper CI Limit", "P-value")
sm.prob.table<-prob.table[prob.table$`P-value`<=0.05,]

ggplot(sm.prob.table, aes(x=sm.prob.table$Variables, 
                          y = sm.prob.table$`Change in probability of Y`)) +   
  geom_errorbar(aes(ymin = sm.prob.table$`Lower CI Limit`, 
                    ymax = sm.prob.table$`Upper CI Limit`), 
                width = 0.5, lty=1, lwd=.45, col="red") + #geom_hline(yintercept = 0, color="black")+
  coord_flip()+ 
  geom_point(shape=20, size=3, fill="black") +  
  geom_segment(aes(yend=sm.prob.table$`Change in probability of Y`),
               xend=0, colour="grey50",
               lineend = "round", size=.35, linetype=2)+ 
  geom_text(aes(label=paste(round(sm.prob.table$`Change in probability of Y`,3)*100,"%",
                            sep = '')),
            cex=2, hjust=0,vjust=-1.5,color="black")+
  labs(title= "Marginal Effect(s)", 
       subtitle="Change in Probability of Output if Predictor Changes by 1 
(Red error bar shows a 95% confidence interval around estimate of probability)",
       x="Predictors", y="Probability", 
       caption = "") +   
  theme(plot.title = element_text(family = "sans", face="bold", 
                                  size=13),     
        axis.title = element_text(family = "sans", size=9),
        plot.caption = element_text(family = "sans", size=5)) 




####Classification####
set.seed(5)
TI <- createDataPartition(y=dta$churn, p=0.75, list=FALSE)
tra<- dta[TI,]
val<- dta[-TI,]

head(tra)
c.analysis <- ctree(factor(churn) ~ duration + num_prods + calls + clicks + avg_budget,
                    data = tra)

tp_args <- list(beside = TRUE, just = "center", id = TRUE, text = TRUE)
# args for inner_panel
ip_args <- list(id = F, pval = T)
# args for edge_simple
ep_args <- list(just = "alternate")
plot(c.analysis, gp = gpar(fontsize = 10),
                terminal_panel = node_barplot, 
                tp_args = tp_args,
                inner_panel = node_inner, 
                ip_args = ip_args,
                edge_panel = edge_simple, 
                ep_args = ep_args)

confusionMatrix(factor(val$churn),predict(c.analysis,newdata=val))


####RandomForest####
set.seed(5)
TI <- createDataPartition(y=dta$churn, p=0.75, list=FALSE)
tra<- dta[TI,]
val<- dta[-TI,]

modFit <- randomForest(factor(churn) ~., data = tra, importance=T,proximity=T)


im<-as.data.frame(importance(modFit, scale = T))
im$Activity<-row.names(im)
im<-dplyr::select(im, Activity, `MeanDecreaseGini`)
names(im)<-c("Activity","Mean Decrease Gini")

t<-head(arrange(im, desc(`Mean Decrease Gini`)),10)
ggplot(t, aes(x=`Mean Decrease Gini`,
              y=reorder(Activity,`Mean Decrease Gini`)))+geom_point()+
  labs(x="Mean Decrease Gini", y="Variables", title="Mean Decrease Gini")

cm<-confusionMatrix(predict(modFit,val),
                    factor(val$churn))

acc.rf <- paste(round(cm$overall[1]*100,1),'%', sep = '')
sens.rf <- paste(round(cm$byClass[1]*100,1),'%', sep = '')
spec.rf <- paste(round(cm$byClass[2]*100,1),'%', sep = '')
p.rf<-round(cm$overall[6],3)



####Network####

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

scaled2<-as.data.frame(lapply(dta[,c(1,3:8,10)], normalize))
set.seed(5)
I <- createDataPartition(y=scaled2$churn, p=0.75, list=FALSE)
training<- scaled2[I,]
validating<- scaled2[-I,]

n <- names(training)
f <- as.formula(paste("churn ~", paste(n[!n %in% "churn"], collapse = " + ")))

nn <- neuralnet(f, 
                data = training, 
                hidden = c(2,1),#hidden(nodes per layer, i.e., 5 nodes in first layer and one in second)
                linear.output = F,
                rep = 1, #select number of repeatitions (optional)
                lifesign = 'full'#to select the lowest error
)
plot(nn, rep=1)

pr.nn <- neuralnet::compute(nn,validating[,c(1:5,7:8)], rep = 1)
pr.nn$net.result

pr.nn_ <- pr.nn$net.result*(max(dta$churn)-min(dta$churn))+min(dta$churn)
test.r <- (validating$churn)*(max(dta$churn)-min(dta$churn))+min(dta$churn)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(validating)
MSE.nn
#For classification problems
pr.nnF<-ifelse(pr.nn$net.result>0.5,1,0)
medvF<-ifelse(validating$churn>.5,1,0)
tab0<-table(pr.nnF, medvF)
Maccuracy <- sum(diag(tab0))/sum(tab0)
misclassification <- 1- Maccuracy

val.nn<-data.frame(medvF)
pr.nn.df <- data.frame(pr.nnF)
nn.dd <- data.frame(cbind('Actual' = val.nn, 'Predicted'=pr.nn.df))
names(nn.dd) <- c ('Actual','Predicted')
cm.nn<-confusionMatrix(factor(nn.dd$Actual),
                    factor(nn.dd$Predicted))


#### NNET for classification models with single layer ####

library(nnet)
set.seed(5)
data <- dta
samp <- TI
ir.nn2 <- nnet(factor(churn) ~ ., data = data, subset = samp, size = 6, rang = 0.1,
               decay = 1e-2, maxit = 2000)

labels.nnet <- predict(ir.nn2, data[-samp,], type="class")
labels.nnet <- as.data.frame(labels.nnet)
dt<-data.frame(data$churn[-samp])

ttt<-cbind(dt,labels.nnet)
head(ttt)
tab1 <- table(ttt$data.churn..samp., ttt$labels.nnet)

mean(ttt$labels.nnet== ttt$data.churn..samp.)
plot(table(ttt$data.churn..samp., ttt$labels.nnet),col=c('green','red','blue'))
Maccuracy <- sum(diag(tab1))/sum(tab1)
misclassification <- 1- Maccuracy


####GLM####
library(boot)
head(tra)

set.seed(200)
lm.fit <- glm(churn~.,data=tra[,c(1,3:8,10)])
suppressWarnings(cv.glm(tra[,c(1,3:8,10)],lm.fit,K=10)$delta[1])
prediction <- predict(lm.fit,val[,c(1,3:8,10)])

predFactor<-ifelse(prediction>0.5,1,0)
summary(prediction)
confusionMatrix(predFactor,val[,9])


cv.error <- NULL
k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:2){
  I <- createDataPartition(y=scaled2$churn, p=0.75, list=FALSE)
  training<- scaled2[I,]
  validating<- scaled2[-I,]
  
  nn <- neuralnet(f,data=training,hidden=c(5),linear.output=T)#hidden(nodes,layers)
  
  pr.nn <- compute(nn,validating[,c(1:5,7:8)])
  pr.nN <- pr.nn$net.result*(max(dta$churn)-min(dta$churn))+min(dta$churn)
  
  test.cv.r <- (validating$churn)*
    (max(dta$churn)-min(dta$churn))+min(dta$churn)
  
  cv.error[i] <- sum((test.cv.r - pr.nN)^2)/nrow(validating)
  
#  pbar$step()
}



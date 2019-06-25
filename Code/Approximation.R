###########################################################
source("Base.R")
###########################################################
##### Approximation of Value Functions #####
###########################################################
#### Test calls and Plots ####

samples = as.data.frame(expand.grid(seq(-5,5,by=1),seq(-5,5,by=1)))
colnames(samples)=c("x","y")
samples$f1 = batch_apirequest(samples[1:2], 1, "api-test2D")
samples$f2 = batch_apirequest(samples[1:2], 2, "api-test2D")

fplot(samples,f = "f2")

plot_ly(samples) %>% add_trace(x=~f1,y=~f2,type="scatter",mode="markers")

# Plotting example (3D plot)
library(plotly)

fplot(samples,"f1")
fplot(samples,"f2")
f1f2plot(samples,scaleit=T)

# 3D
samples3D = as.data.frame(expand.grid(seq(-5,5,by=1),seq(-5,5,by=1),seq(-5,5,by=1)))
colnames(samples3D)=c("x","y","z")
samples3D$f1 = batch_apirequest(samples3D[1:3], 1, "api-test3D")
samples3D$f2 = batch_apirequest(samples3D[1:3], 2, "api-test3D")

fplot(samples3D,"f2",type3d = "surface")

###########################################################
#### First approximation tries ####
samples = as.data.frame(expand.grid(seq(-5,5,by=1),seq(-5,5,by=1)))
colnames(samples)=c("x","y")
samples$f1 = batch_apirequest(samples[,1:2], 1, "api-test2D")
samples$f2 = batch_apirequest(samples[,1:2], 2, "api-test2D")

library(mlr)
fplot(samples,"f1")

# Learn f1
f1.task = makeRegrTask(data = samples, target = "f1")
f1.lrn.svm = makeLearner(cl = "regr.ksvm",id="svm")



f1.mdl.svm = train(learner=f1.lrn.svm, task=f1.task)

new_samples = as.data.frame(expand.grid(seq(-4.5,4.5,by=1),seq(-4.5,4.5,by=1)))
colnames(new_samples)=c("x","y")
new_samples$f1 = batch_apirequest(new_samples[,1:2], 1, "api-test2D")
new_samples$f2 = batch_apirequest(new_samples[,1:2], 2, "api-test2D")

samples = rbind(samples,new_samples)

f1.pred.svm = predict(f1.mdl.svm, newdata = new_samples)
performance(pred = f1.pred.svm, measures = mse)

f1.pred.all=predict(f1.mdl.svm, newdata = samples)
samples$f1pred = f1.pred.all$data$response
samples$f1err = (samples$f1pred - samples$f1)
fplot(samples,"f1pred")
fplot(samples,"f1err")

f1f2plot(samples,f_2="f1pred",scaleit=F)

# Learn f1 with resampling
samples = as.data.frame(expand.grid(seq(-5,5,by=1),seq(-5,5,by=1)))
colnames(samples)=c("x","y")
samples$f1 = batch_apirequest(samples[,1:2], 1, "api-test2D")
samples$f2 = batch_apirequest(samples[,1:2], 2, "api-test2D")

f1.task = makeRegrTask(data = samples, target = "f1")
f1.lrn.svm = makeLearner(cl = "regr.ksvm",id="svm")


boots = makeResampleDesc(method = "Bootstrap", iters=200, predict="both")
f1.perf.svm = resample(f1.lrn.svm, f1.task, boots, measures = mse)

f1.preds.svm = as.data.frame(f1.perf.svm$pred)
f1.predinter.svm = f1.preds.svm %>% group_by(id) %>%
  summarize(meanpred = mean(response),
            sdpred=sd(response),
            lowpred=quantile(response,0.1),
            uppred=quantile(response,0.9))
samples = cbind(samples,f1.predinter.svm)
fplot(samples,f = "sdpred")

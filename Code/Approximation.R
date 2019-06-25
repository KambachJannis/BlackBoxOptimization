###########################################################
source("Base.R")
###########################################################
#......... Approximation of Value Functions ..............#
###########################################################
#### Test calls and Plots ####

samples = as.data.frame(expand.grid(seq(-5,5,by=1),seq(-5,5,by=1)))
colnames(samples)=c("x","y")
samples$f1 = batch_apirequest(samples[1:2], 1, "api-test2D")
samples$f2 = batch_apirequest(samples[1:2], 2, "api-test2D")

fplot(samples,f = "f2")

# Plot both function values for pareto front
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
#### First approximation try ####
# Get first observations
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

# Get more observations
new_samples = as.data.frame(expand.grid(seq(-4.5,4.5,by=1),seq(-4.5,4.5,by=1)))
colnames(new_samples)=c("x","y")
new_samples$f1 = batch_apirequest(new_samples[,1:2], 1, "api-test2D")
new_samples$f2 = batch_apirequest(new_samples[,1:2], 2, "api-test2D")
# add to samples
samples = rbind(samples,new_samples)

# make new predictions
f1.pred.svm = predict(f1.mdl.svm, newdata = new_samples)
# evaluate predictions (mean squared error)
performance(pred = f1.pred.svm, measures = mse)

# compare approximations
# predict all observations (training and test)
f1.pred.all=predict(f1.mdl.svm, newdata = samples)
samples$f1pred = f1.pred.all$data$response
samples$f1err = (samples$f1pred - samples$f1)
# Plot predicted values
fplot(samples,"f1pred")
# Plot prediction error
fplot(samples,"f1err")

# Plot real values and prediction
f1f2plot(samples,f_2="f1pred",scaleit=F)

###########################################################
#### Learn f1 with resampling
samples = as.data.frame(expand.grid(seq(-5,5,by=1),seq(-5,5,by=1)))
colnames(samples)=c("x","y")
samples$f1 = batch_apirequest(samples[,1:2], 1, "api-test2D")
samples$f2 = batch_apirequest(samples[,1:2], 2, "api-test2D")

f1.task = makeRegrTask(data = samples[,1:3], target = "f1")
f1.lrn.svm = makeLearner(cl = "regr.ksvm",id="svm")

#### Perform all resampling and model selection here
# TODO
# make nested resampling (with hyperparameter tuning) to find best learner class
# choose best learner, tune hyperparameters on all observations
# Output: One learner class with trained hyperparameters (we call it "best learner")

#### Next: Identify new observations to fetch from API
# Define resampling strategy
boots = makeResampleDesc(method = "Bootstrap", iters=100, predict="both")

# resample best learner (for exampel svm)
f1.perf.svm = resample(f1.lrn.svm, f1.task, boots, measures = mse, models=T)
# For each observation, calculate mean regression ^y_i (meanpred) and model variance sigma²_^y_i (varpred)
f1.preds.svm = as.data.frame(f1.perf.svm$pred)
f1.predinter.svm = f1.preds.svm %>% group_by(id) %>%
  summarize(f1_meanpred = mean(response),
            f1_varpred = var(response))

samples = cbind(samples,f1.predinter.svm)

# Build training set of residuals (error variance / noise)
samples = samples %>% mutate(f1_r = pmax((f1-f1_meanpred)**2 - f1_varpred,0))

# Make training set for noise estimation
D = samples %>% select("x","y","f1_r")  # TODO add z dimension

D.task = makeRegrTask(data=D,target="f1_r")
D.lrn = makeLearner("regr.nnet",id="noise_nnet")

# TODO custom error function
D.mdl = train(task=D.task,learner=D.lrn)

# Tune (maybe)
D.pred = predict(D.mdl, newdata = D)
# evaluate predictions (mean squared error)
performance(pred = D.pred, measures = rmse)


# then make new predictions on a dense grid
max_samples = 1000 # adjust so it is not too computationally expensive
densegrid = as.data.frame(expand.grid(seq(-5,5,length.out = floor(sqrt(max_samples))),seq(-5,5,length.out = floor(sqrt(max_samples)))))
colnames(densegrid)=c("x","y")

# compute model variance sigma²_^y_i (varpred) for every observation in grid
Mdl.pred = lapply(1:length(f1.perf.svm$models), function(m) {
  data.frame(x=densegrid$x,y=densegrid$y,model=m,pred=predict(f1.perf.svm$models[[m]], newdata = densegrid)$data$response)
})
Mdl.predictions = do.call(rbind,Mdl.pred)
Mdl.predictions = Mdl.predictions %>% group_by(x,y) %>%
  summarize(f1_meanpred = mean(pred),
            f1_varpred = var(pred))

# Estimate noise variance using neural net
D.pred = predict(D.mdl, newdata = densegrid)
samples$f1_varnoise = D.pred$data$response
# TODO

# Add both model variance and noise variance together to get total variance
# TODO

# Select observation(s) with highest total variance to be fetched from API
# TODO

# LOOP




fplot(samples,f = "sdpred")


# 

fplot(samples,f = "sdpred")

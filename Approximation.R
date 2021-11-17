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
#### BENCHMARKING ####
# Call all data from dense grid for benchmarking
benchmark = as.data.frame(expand.grid(seq(-5,5,length.out = floor(sqrt(10000))),seq(-5,5,length.out = floor(sqrt(10000)))))
colnames(benchmark)=c("x","y")
benchmark$f1 = batch_apirequest(benchmark %>% select(x,y), 1, "api-test2D")[[1]]
benchmark$f2 = batch_apirequest(benchmark %>% select(x,y), 2, "api-test2D")[[1]]
saveRDS(benchmark,"BenchmarkTest2D")

# Load
benchmark = readRDS("BenchmarkTest2D")
###########################################################
#### Learn f1 with resampling

# Define dense grid for observation selection
max_samples = 10000 # adjust so it is not too computationally expensive
densegrid = as.data.frame(expand.grid(seq(-5,5,length.out = floor(sqrt(max_samples))),seq(-5,5,length.out = floor(sqrt(max_samples)))))
colnames(densegrid)=c("x","y")

# Counts all calls made so far
call_counter=0
# Maximum number of calls that can be made in loop
max_calls = 1000
# Number of new observations called per loop run
new_observations_per_call=100

# Should noise be included in prediction interval estimation?
include_noise = F

# Measure evolution of performance
benchresults = c()

# Get first observations
f1.samples = as.data.frame(expand.grid(seq(-5,5,by=1),seq(-5,5,by=1)))
colnames(f1.samples)=c("x","y")
res = batch_apirequest(f1.samples %>% select(x,y), 1, "api-test2D",call_counter)
f1.samples$f1 = res[[1]]
call_counter= res[[2]]

f2.samples = as.data.frame(expand.grid(seq(-5,5,by=1),seq(-5,5,by=1)))
colnames(f2.samples)=c("x","y")
res = batch_apirequest(f2.samples %>% select(x,y), 2, "api-test2D",call_counter)
f2.samples$f2 = res[[1]]
call_counter= res[[2]]

while(call_counter <= max_calls - new_observations_per_call) {
  # Define tasks and learners
  f1.task = makeRegrTask(data = f1.samples, target = "f1")
  f1.lrn.svm = makeLearner(cl = "regr.ksvm",id="svm")
  
  f2.task = makeRegrTask(data = f2.samples, target = "f2")
  f2.lrn.svm = makeLearner(cl = "regr.ksvm",id="svm")
  
  #### Perform all resampling and model selection here
  # TODO
  # make nested resampling (with hyperparameter tuning) to find best learner class
  # choose best learner, tune hyperparameters on all observations
  # Output: One learner class with trained hyperparameters (we call it "best learner")
  f1.lrn.best = f1.lrn.svm # placeholder
  f2.lrn.best = f2.lrn.svm # placeholder
  
  #### Run Hyperparameter Tuning on whole dataset
  # TODO
  f1.mdl.best = train(learner = f1.lrn.best, task = f1.task)
  
  
  # -------------------------------------------------------- .
  # Benchmark current model
  bench.pred = predict(f1.mdl.best, newdata = benchmark)
  performance(pred = bench.pred, measures = rmse)
  benchresults = c(benchresults,performance(pred = bench.pred, measures = rmse))
  # -------------------------------------------------------- .
  
  
  #### Analyse noise and model variance
  # Define resampling strategy
  boots = makeResampleDesc(method = "Bootstrap", iters=100, predict="both")
  
  # Resample best learner
  f1.perf.best = resample(f1.lrn.best, f1.task, boots, measures = mse, models=T)
  
  # For each observation, calculate mean regression ^y_i (meanpred) and model variance sigma²_^y_i (varpred)
  f1.pred = as.data.frame(f1.perf.best$pred)
  f1.predstat = f1.pred %>% group_by(id) %>%
    summarize(meanpred = mean(response),
              varpred = var(response))
  
  f1.predstat = cbind(f1.samples,f1.predstat)
  f1.predstat = f1.predstat %>% mutate(rmse=sqrt((f1-meanpred)**2))
  
  if(include_noise){
    # Build training set of residuals (error variance / noise)
    f1.predstat = f1.predstat %>% mutate(r_squared = pmax((f1-meanpred)**2 - varpred,0))
    # Make training set for noise estimation
    D = f1.predstat %>%
      mutate(r_log=log(r_squared+0.0000000000001)) %>%
      select("x","y","r_log")  # TODO add z dimension

    D.task = makeRegrTask(data=D,target="r_log")
    D.measure = makeMeasure(id = "C_BS",
                            minimize = TRUE,
                            properties = c("regr", "response"),
                            fun = function(task, model, pred, feats, extra.args){
                              print(pred$data$response)
                              0.5 * sum(log(exp(pred$data$response)-0.0000000000001) + (exp(pred$data$truth)-0.0000000000001)/(exp(pred$data$response)-0.0000000000001))})
    
    # Make learner and tune hyperparameters
    D.lrn = makeLearner("regr.xgboost",id="noise_learner",
                        par.vals = list(
                          objective = "reg:linear",
                          nrounds = 50))
    D.paramset = makeParamSet(
      makeNumericParam("eta", lower = 0.05, upper = 0.5),
      makeIntegerParam("max_depth",lower=3,upper=8),
      makeNumericParam("lambda",lower=0,upper=0.50),
      makeNumericParam("subsample", lower = 0.10, upper = 0.80),
      makeNumericParam("min_child_weight",lower=1,upper=3)
    ) # select parameters to tune
    
    D.contrl = makeTuneControlRandom(maxit = 50)
    D.resDesc=makeResampleDesc("CV", iters = 10)
    
    parallelMap::parallelStartSocket(4)
    D.tunedparams = tuneParams(learner=D.lrn,task=D.task,
                               resampling=D.resDesc,
                               measures = list(D.measure),
                               par.set = D.paramset,control = D.contrl)
    parallelMap::parallelStop()
    
    D.lrn=setHyperPars(D.lrn,par.vals=D.tunedparams$x)
    
    # Train learner
    D.mdl = train(task=D.task,learner=D.lrn)
    
    
    # Evaluate predictions (mean squared error)
    D.pred = predict(D.mdl, newdata = D)
    performance(pred = D.pred, measures = rmse)
    # Test improvement over standard learner
    # D.lrn_def = D.lrn = makeLearner("regr.xgboost",id="noise_learner",
    #                                 par.vals = list(
    #                                   objective = "reg:linear",
    #                                   nrounds = 50))
    # resample(D.lrn_def,task = D.task,resampling = D.resDesc,measures = rmse)
    # resample(D.lrn,task = D.task,resampling = D.resDesc,measures = rmse)
  }
  
  
  #### Select new observations to fetch from API, based on variance
  # Compute model variance sigma²_^y_i (varpred) for every observation in grid
  f1.grid.mdls.pred = lapply(1:length(f1.perf.best$models), function(m) {
    data.frame(x=densegrid$x,y=densegrid$y,model=m,pred=predict(f1.perf.best$models[[m]], newdata = densegrid)$data$response)
  })
  f1.grid = do.call(rbind,f1.grid.mdls.pred)
  f1.grid = f1.grid %>% group_by(x,y) %>%
    summarize(meanpred = mean(pred),
              varpred = var(pred))
  
  # Estimate noise variance
  if(include_noise){
    D.pred = predict(D.mdl, newdata = densegrid)
    f1.grid$varnoise = exp(D.pred$data$response)-0.0000000000001
    } else {f1.grid$varnoise = 0}
    
  # Add both model variance and noise variance together to get total variance
  f1.grid = f1.grid %>% mutate(vartotal=varpred+varnoise)
  
  #fplot(f1.grid,f = "vartotal")
  
  # Select observation(s) with highest total variance to be fetched from API
  f1.grid = f1.grid %>% arrange(desc(vartotal))
  # Leave out observations which have been fetched already
  f1.fetch = (f1.grid %>% select(x,y) %>%
                setdiff(f1.samples %>% select("x","y"))
  )[1:new_observations_per_call,]
  
  # Call API and add new observations to sampleset
  res = batch_apirequest(f1.fetch %>% select(x,y), 1, "api-test2D",call_counter)
  f1.fetch$f1 = res[[1]]
  call_counter = res[[2]]
  
  f1.samples = f1.samples %>% union(f1.fetch)
    
  
}
# LOOP

bench.pred = predict(f1.mdl.best, newdata = benchmark)
viz_benchmark = cbind(benchmark,bench.pred$data) %>% mutate(err=response-truth)
fplot(viz_benchmark, f = "err")
f1f2plot(viz_benchmark, f_1 = "truth", f_2="response",scaleit = F)

viz_benchmark = viz_benchmark %>% mutate(color=50*(err-min(err))/(max(err)-min(err)))
plot(viz_benchmark$x,viz_benchmark$y,col=viridisLite::viridis(50)[floor(viz_benchmark$color)])
points(f1.samples$x,f1.samples$y)

f1.grid = f1.grid %>% mutate(color=50*(vartotal-min(vartotal))/(max(vartotal)-min(vartotal)))
plot(f1.grid$x,f1.grid$y,col=toCol(f1.grid$varnoise))

plot(f1.predstat$x,f1.predstat$y,col=toCol(f1.predstat$rmse))
fplot(f1.predstat,f = "rmse")

toCol = function(x,resolution=50){
  color=resolution*(x-min(x))/(max(x)-min(x))
  viridisLite::viridis(resolution)[floor(color)]
}

# 

fplot(samples,f = "sdpred")

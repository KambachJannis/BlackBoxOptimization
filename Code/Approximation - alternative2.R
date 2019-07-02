###########################################################
source("Base.R")
###########################################################
#......... Approximation of Value Functions (Alternative version) ..............#
###########################################################
#### BENCHMARKING ####
# Call all data from dense grid for benchmarking
benchmark = as.data.frame(expand.grid(seq(-5,5,length.out = floor(sqrt(10000))),seq(-5,5,length.out = floor(sqrt(10000)))))
colnames(benchmark)=c("x","y")
benchmark$f1 = batch_apirequest(benchmark %>% select(x,y), 1, "api-test2D")[[1]]
benchmark$f2 = batch_apirequest(benchmark %>% select(x,y), 2, "api-test2D")[[1]]
saveRDS(benchmark,"BenchmarkTest2D")

{
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
max_calls = 1121
# Number of new observations called per loop run
new_observations_per_call=50

# Should noise be included in prediction interval estimation?
include_noise = F

# Measure evolution of performance
benchresults = c()
benchbelieve = c()
benchbelieve_old = c()

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

round=0
}
while(call_counter <= max_calls - new_observations_per_call & round<5) {
  {
    {
  round = round +1
  print(paste("Round",round))
  
  # Define tasks and learners
  f1.task = makeRegrTask(data = f1.samples, target = "f1")
  f1.lrn.svm = makeLearner(cl = "regr.ksvm",id="svm")
  f1.lrn.xgb = makeLearner(cl = "regr.xgboost",id="xgboost")
  f1.lrn.forest = makeLearner(cl = "regr.randomForest",id="randomForest")
  
  f2.task = makeRegrTask(data = f2.samples, target = "f2")
  f2.lrn.svm = makeLearner(cl = "regr.ksvm",id="svm")
  
  #### Perform all resampling and model selection here
  # TODO
  # make nested resampling (with hyperparameter tuning) to find best learner class
  # choose best learner, tune hyperparameters on all observations
  # Output: One learner class with trained hyperparameters (we call it "best learner")
  
  # Make learner and tune hyperparameters
  forest.paramset = makeParamSet(
    makeIntegerParam("mtry",lower=2,upper=10),
    makeIntegerParam("nodesize",lower=10,upper=50)
  ) # select parameters to tune
  
  forest.contrl = makeTuneControlRandom(maxit = 50)
  forest.resDesc=makeResampleDesc("CV", iters = 10)
  
  forest.tunedparams = tuneParams(learner=f1.lrn.forest,task=f1.task,
                             resampling=forest.resDesc,
                             measures = list(rmse),
                             par.set = forest.paramset,
                             control = forest.contrl)
  
  f1.lrn.forest=setHyperPars(f1.lrn.forest,
                             par.vals=forest.tunedparams$x)
  
  
  f1.lrn.best = f1.lrn.forest # placeholder
  f2.lrn.best = f2.lrn.svm # placeholder
  
  #### Run Hyperparameter Tuning on whole dataset
  # TODO
  f1.mdl.old = f1.mdl.best
  f1.mdl.best = train(learner = f1.lrn.best, task = f1.task)
  
  
  # -------------------------------------------------------- .
  # Benchmark current model
  bench.pred = predict(f1.mdl.best, newdata = benchmark %>% select(x,y,f1))
  performance(pred = bench.pred, measures = rmse)
  benchresults = c(benchresults,performance(pred = bench.pred, measures = rmse))
  benchmark[paste("resp",round,sep="")]=bench.pred$data$response
  benchmark[paste("rse",round,sep="")]=(bench.pred$data %>% transmute(rse=sqrt((truth-response)**2)))$rse
  
  bench.pred = predict(f1.mdl.best, newdata = f1.samples %>% select(x,y,f1))
  performance(pred = bench.pred, measures = rmse)
  benchbelieve = c(benchbelieve,performance(pred = bench.pred, measures = rmse))
  
  bench.pred = predict(f1.mdl.old, newdata = f1.samples %>% select(x,y,f1))
  performance(pred = bench.pred, measures = rmse)
  benchbelieve_old = c(benchbelieve_old,performance(pred = bench.pred, measures = rmse))
  
  # -------------------------------------------------------- .
  }
  
  #### Analyse noise and model variance
  # Define resampling strategy
  loo = makeResampleDesc(method = "LOO", predict="both")
  
  # Resample best learner
  f1.perf.best = resample(f1.lrn.best, f1.task, loo, measures = rmse, models=T)
  
  View(
    f1.perf.best$pred$data %>% filter(set=="test") %>%
    select(id,iter) %>% inner_join(f1.perf.best$measures.test,by="iter")
  )
  
  (f1.perf.best$measures.test$rmse[2]**2+
      316*(f1.perf.best$measures.train$rmse[2]**2))/317
  
  f1.perf.best$pred$data %>% group_by(iter) %>%
    summarize(rmse=sqrt(mean((truth-response)**2)))
  
  f1.predstat = cbind(f1.samples,f1.predstat)
  f1.predstat = f1.predstat %>% mutate(rmse=sqrt((f1-meanpred)**2))
  
  D = f1.predstat %>% select(x,y,rmse)
  D.task = makeRegrTask(data=D,target="rmse")
  
  #fplot(f1.predstat,f="rmse")
  
  # Make learner and tune hyperparameters
  D.lrn = makeLearner("regr.kknn",id="rmse_learner")
  D.paramset = makeParamSet(
    makeIntegerParam("k", lower = 2, upper = 20)
  ) # select parameters to tune
  
  D.contrl = makeTuneControlGrid()
  D.resDesc=makeResampleDesc("CV", iters = 10)
  
  #parallelMap::parallelStartSocket(4)
  D.tunedparams = tuneParams(learner=D.lrn,task=D.task,
                             resampling=D.resDesc,
                             measures = list(rmse),
                             par.set = D.paramset,control = D.contrl)
  #parallelMap::parallelStop()
  
  D.lrn=setHyperPars(D.lrn,par.vals=D.tunedparams$x)
  
  # Train learner
  D.mdl = train(task=D.task,learner=D.lrn)
  
  
  # Evaluate predictions (mean squared error)
  D.pred = predict(D.mdl, newdata = D)
  performance(pred = D.pred, measures = rmse)
  f1.predstat$estim = D.pred$data$response
  
  #f1f2plot(f1.predstat,f_1 = "rmse",f_2 = "estim", scaleit=F)
  
  error_bench = benchmark
  error_bench$pred = predict(f1.mdl.best,newdata = error_bench %>% select(x,y))$data$response
  error_bench$realrse =  sqrt((error_bench$pred - error_bench$f1)**2)
  error_bench$estimrse = predict(D.mdl, newdata = error_bench %>% select(x,y))$data$response
  #f1f2plot(error_bench,f_1 = "realrse",f_2 = "estimrse", scaleit=F)
  
  #### Select new observations to fetch from API, based on error estimates
  
  # Select observation(s) with highest total variance to be fetched from API
  error_bench = error_bench %>% arrange(desc(estimrse))
  # Leave out observations which have been fetched already
  f1.fetch = (error_bench %>% select(x,y,estimrse))
  
  f1.fetch_n = sample_n(f1.fetch,size=new_observations_per_call,
                        weight=(f1.fetch$estimrse**20)) %>%
    select(x,y)
  plot(f1.fetch_n$x,f1.fetch_n$y,xlim=c(-5,5),ylim=c(-5,5))
  
  
  # Call API and add new observations to sampleset
  res = batch_apirequest(f1.fetch_n %>% select(x,y), 1, "api-test2D",call_counter)
  f1.fetch_n$f1 = res[[1]]
  call_counter = res[[2]]
  
  f1.samples = f1.samples %>% union(f1.fetch_n)
  }
}
# LOOP

bench.pred = predict(f1.mdl.best, newdata = benchmark)
viz_benchmark = cbind(benchmark,bench.pred$data) %>% mutate(err=response-truth)
fplot(viz_benchmark, f = "err")
f1f2plot(viz_benchmark, f_1 = "truth", f_2="response",scaleit = F)

viz_benchmark = viz_benchmark %>% mutate(color=50*(err-min(err))/(max(err)-min(err)))
plot(viz_benchmark$x,viz_benchmark$y,col=viridisLite::viridis(50)[floor(viz_benchmark$color)])
plot(f1.samples$x,f1.samples$y)

f1.grid = f1.grid %>% mutate(color=50*(vartotal-min(vartotal))/(max(vartotal)-min(vartotal)))
plot(f1.grid$x,f1.grid$y,col=toCol(f1.grid$varnoise))

plot(f1.predstat$x,f1.predstat$y,col=toCol(f1.predstat$rmse))
fplot(f1.predstat,f = "rmse")

toCol = function(x,resolution=50){
  color=resolution*(x-min(x))/(max(x)-min(x))
  viridisLite::viridis(resolution)[floor(color)]
}

# Comparing the benchmark results
plot(f1.fetch_n$x,f1.fetch_n$y,xlim=c(-5,5),ylim=c(-5,5))
f1f2plot(benchmark,f_1="rse1",f_2="rse5",scaleit=F)
f1f2plot(benchmark,f_1="f1",f_2="resp1",scaleit=F)

fplot(benchmark,f="f1")

plot_ly(benchmark %>% mutate(err=rse2-rse1),
  type = 'scatter',
  mode='markers',
  x=~x,
  y=~y,
  marker=list(
    color=~err,
    colorbar=list(
      title='Colorbar'),
    colorscale='Viridis'))

plot_ly(f1.fetch_n) %>%
  add_trace(x=~x,y=~y,type = 'scatter',
    mode='markers',marker=list(color="black"),showlegend = F)


plot(benchmark$x,benchmark$y,col=toCol(benchresults.map[[1]]$rse-benchresults.map[[2]]$rse))


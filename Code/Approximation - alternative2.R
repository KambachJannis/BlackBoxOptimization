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

# Number of observations called at the beginning
number_of_first_observations=200
# Number of new observations called per loop run
new_observations_per_call=10

# Should noise be included in prediction interval estimation?
include_noise = F

# Measure evolution of performance
benchresults = c()
benchbelieve = c()
benchbelieve_old = c()


# Get first observations
f1.samples = as.data.frame(expand.grid(seq(-5,5,length.out=floor(sqrt(number_of_first_observations))),seq(-5,5,length.out=floor(sqrt(number_of_first_observations)))))
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


#########################################################.
#Preparation of learners and hyperparameters

inner = makeResampleDesc("Subsample", iters = 4)
ctrl = makeTuneControlRandom(maxit = 50)

learners = list(
  makeTuneWrapper("regr.ksvm",
                  resampling = inner,
                  measures=rmse,
                  par.set = makeParamSet(
                    makeNumericParam(id = "C",  upper = 10, lower = -5, trafo = function(x) 2^x, default = log2(1)),
                    makeNumericParam(id = "sigma", upper = 15, lower = -15, trafo = function(x) 2^x),
                    keys = "task"
                  ),
                  control = ctrl,
                  show.info = FALSE),
  
  makeTuneWrapper("regr.xgboost",
                  resampling = inner,
                  measures=rmse,
                  par.set = makeParamSet(
                    makeNumericParam(id = "nrounds", lower = log2(10/10), upper = log2(500/10), trafo = function(x) round(2^x * 10)),
                    makeIntegerParam(id = "max_depth", lower = 3L, upper = 10L),
                    makeNumericParam(id = "eta", lower = 0.001, upper = 0.3),
                    makeNumericParam(id = "gamma", lower = 0, upper = 10),
                    makeNumericParam(id = "colsample_bytree", lower = 0.3, upper = 0.7),
                    makeNumericParam(id = "min_child_weight", lower = 0, upper = 20),
                    makeNumericParam(id = "subsample", lower = 0.25, upper = 1)
                  ),
                  control = ctrl,
                  show.info = FALSE),
  
  makeTuneWrapper("regr.randomForest",
                  resampling = inner,
                  measures=rmse,
                  makeParamSet(
                    makeIntegerParam("nodesize", lower = 1, upper = 10, default = 1),
                    makeIntegerParam(id = "mtry", lower = 1L, upper = 2L)
                  ),
                  control = ctrl,
                  show.info = FALSE),
  
  makeTuneWrapper("regr.kknn",
                  resampling = inner,
                  measures=rmse,
                  par.set = makeParamSet(
                    makeIntegerParam("k", lower = 2, upper = 20),
                    makeDiscreteParam("kernel", values = c("rectangular","triangular", "epanechnikov","biweight","tri-weight","cos", "inv", "gaussian", "rank","optimal"))
                  ),
                  control = ctrl,
                  show.info = FALSE),
  
  makeTuneWrapper("regr.nnet",
                  resampling = inner,
                  measures=rmse,
                  par.set = makeParamSet(
                    makeIntegerParam(id = "size", lower = 1L, upper = 20L),
                    makeNumericParam(id = "decay", lower = -5, upper = 1, trafo = function(x) 10^x)
                  ),
                  control = ctrl,
                  show.info = FALSE)
)


parallelMap::parallelStartSocket(6)
}
while(call_counter <= max_calls - new_observations_per_call & round<80) {
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
  
  
  
  

  
  # Outer resampling loop
  outer = makeResampleDesc("CV", iters = 5)
  nestedResamplingResults = benchmark(learners,f1.task,outer,measures=rmse,show.info=FALSE)
  
  
  nestedResamplingResults = sapply(learners,
    function(learner){
    r = resample(learner, f1.task, resampling = outer, extract = getTuneResult, show.info = FALSE)
    return(rbind(r$learner.id,r$aggr))})
  
  
  
  #### Perform all resampling and model selection here
  # TODO
  # make nested resampling (with hyperparameter tuning) to find best learner class
  # choose best learner, tune hyperparameters on all observations
  # Output: One learner class with trained hyperparameters (we call it "best learner")
  
  # Make learner and tune hyperparameters
  # forest.paramset = makeParamSet(
  #   makeIntegerParam("mtry",lower=2,upper=10),
  #   makeIntegerParam("nodesize",lower=10,upper=50)
  # ) # select parameters to tune
  # 
  # forest.contrl = makeTuneControlRandom(maxit = 50)
  # forest.resDesc=makeResampleDesc("CV", iters = 10)
  # 
  # forest.tunedparams = tuneParams(learner=f1.lrn.forest,task=f1.task,
  #                            resampling=forest.resDesc,
  #                            measures = list(rmse),
  #                            par.set = forest.paramset,
  #                            control = forest.contrl)
  # 
  # f1.lrn.forest=setHyperPars(f1.lrn.forest,
  #                            par.vals=forest.tunedparams$x)
  
  
  f1.lrn.best = f1.lrn.forest # placeholder
  f2.lrn.best = f2.lrn.svm # placeholder
  
  #### Run Hyperparameter Tuning on whole dataset
  # TODO
  if(exists("f1.mdl.best")) f1.mdl.old = f1.mdl.best
  f1.mdl.best = train(learner = f1.lrn.best, task = f1.task)
  
  
  # -------------------------------------------------------- .
  # Benchmark current model
  bench.pred = predict(f1.mdl.best, newdata = benchmark %>% select(x,y,f1))
  
  benchresults = c(benchresults,performance(pred = bench.pred, measures = rmse))
  benchmark[paste("resp",round,sep="")]=bench.pred$data$response
  benchmark[paste("rse",round,sep="")]=(bench.pred$data %>% transmute(rse=sqrt((truth-response)**2)))$rse
  
  print(paste("Current benchmark rmse value is",performance(pred = bench.pred, measures = rmse)))
  
  if(exists("f1.mdl.best")) bench.pred = predict(f1.mdl.best, newdata = f1.samples %>% select(x,y,f1))
  benchbelieve = c(benchbelieve,performance(pred = bench.pred, measures = rmse))
  
  bench.pred = predict(f1.mdl.old, newdata = f1.samples %>% select(x,y,f1))
  benchbelieve_old = c(benchbelieve_old,performance(pred = bench.pred, measures = rmse))
  
  # -------------------------------------------------------- .
  }
  
  #### Analyse noise and model variance
  # Define resampling strategy
  loo = makeResampleDesc(method = "LOO", predict="both")
  
  # Resample best learner
  f1.perf.best = resample(f1.lrn.best, f1.task, loo, measures = rmse)
  
  point_eval = 
    f1.perf.best$pred$data %>% filter(set=="test") %>%
    select(id,iter) %>% inner_join(f1.perf.best$measures.test,by="iter") %>%
      inner_join(f1.perf.best$measures.train,by="iter",suffix = c(".test",".train")) %>%
      mutate(rmse.all = sqrt((rmse.test**2 + (rmse.train**2)*(max(f1.perf.best$pred$data$id)-1))/max(f1.perf.best$pred$data$id)))
  point_eval = point_eval %>% arrange(id)
  point_eval = cbind(f1.samples %>% select(x,y),point_eval)
  
  D = point_eval %>% select(x,y,rmse.all)
  D.task = makeRegrTask(data=D,target="rmse.all")
  
  #fplot(f1.predstat,f="rmse")
  
  # Make learner and tune hyperparameters
  D.lrn = makeLearner("regr.kknn",id="rmse_learner")
  #D.paramset = makeParamSet(
    #makeIntegerParam("k", lower = 2, upper = 20)
  #) # select parameters to tune
  
  #D.contrl = makeTuneControlGrid()
  #D.resDesc=makeResampleDesc("CV", iters = 10)
  
  #parallelMap::parallelStartSocket(4)
  #D.tunedparams = tuneParams(learner=D.lrn,task=D.task,
  #                           resampling=D.resDesc,
   #                          measures = list(rmse),
    #                         par.set = D.paramset,control = D.contrl)
  #parallelMap::parallelStop()
  
  D.lrn=setHyperPars(D.lrn,par.vals=list(k=6))
  
  # Train learner
  D.mdl = train(task=D.task,learner=D.lrn)
  
  
  # Evaluate predictions (mean squared error)
  D.pred = predict(D.mdl, newdata = D)
  performance(pred = D.pred, measures = rmse)
  point_eval$estim = D.pred$data$response
  
  #f1f2plot(point_eval,f_1 = "rmse.all",f_2 = "estim", scaleit=F)
  
  error_bench = benchmark
  error_bench$estimrse = predict(D.mdl, newdata = error_bench %>% select(x,y))$data$response
  #f1f2plot(error_bench,f_1 = "realrse",f_2 = "estimrse", scaleit=F)
  
  #### Select new observations to fetch from API, based on error estimates
  
  # Select observation(s) with highest total variance to be fetched from API
  error_bench = error_bench %>% arrange(desc(estimrse))
  # Leave out observations which have been fetched already
  f1.fetch = error_bench %>% select(x,y) %>%
    setdiff(f1.samples %>% select(x,y)) %>% inner_join(error_bench,by=c("x","y"))
  
  f1.fetch_n = sample_n(f1.fetch,
                        size=new_observations_per_call,
                        weight=((f1.fetch$estimrse-min(f1.fetch$estimrse))**10)) %>% select(x,y)
  
  plot(error_bench$x,error_bench$y,xlim=c(-5,5),ylim=c(-5,5),pch=19,col=toCol(error_bench$estimrse),xlab="x",ylab="y",main="Information value landscape and selected points")
  points(f1.samples$x,f1.samples$y,col="black",pch=3)
  points(f1.fetch_n$x,f1.fetch_n$y,col="red",pch=3)

  
  # Call API and add new observations to sampleset
  res = batch_apirequest(f1.fetch_n %>% select(x,y), 1, "api-test2D",call_counter)
  f1.fetch_n$f1 = res[[1]]
  call_counter = res[[2]]
  
  f1.samples = f1.samples %>% union(f1.fetch_n)
  }
}
parallelMap::parallelStop()
# LOOP

# Evaluation of Performance
plot(benchresults,xlab="Round",ylab="Rmse",main="Performance over rounds")

plot(benchbelieve,xlab="Round",ylab="Rmse",main="Believed performance over rounds")
plot(benchbelieve_old,xlab="Round",ylab="Rmse",main="Believed performance over rounds")

View(data.frame(old=benchbelieve_old,new=benchbelieve) %>% mutate(improve=old-new))

# Show where samples have been fetched from
plot(f1.samples$x,f1.samples$y,xlim=c(-5,5),ylim=c(-5,5),xlab="x",ylab="y",col="red",main="Selected points")

# Comparing the benchmark results
f1f2plot(benchmark,f_1="rse1",f_2="rse5",scaleit=F)
f1f2plot(benchmark,f_1="f1",f_2="resp5",scaleit=F)



##############################################.
# Comparison with grid approach

plot(benchresults,xlab="Round",ylab="Rmse",main="Performance over rounds")
for(i in seq(1,51,by=1)){
tosample = 225 + (i-1)*10

comp.samples = as.data.frame(lhs::maximinLHS(n = tosample,k=2)) %>% rename(x=V1,y=V2) %>% mutate(x=x*10-5,y=y*10-5)
  
#comp.samples = as.data.frame(expand.grid(seq(-5,5,length.out=floor(sqrt(tosample))),seq(-5,5,length.out=floor(sqrt(tosample)))))

colnames(comp.samples)=c("x","y")
res = batch_apirequest(comp.samples %>% select(x,y), 1, "api-test2D")
comp.samples$f1 = res[[1]]

comp.task = makeRegrTask(data = comp.samples, target = "f1")
comp.mdl = train(learner = f1.lrn.best, task = comp.task)

comp.bench.pred = predict(comp.mdl, newdata = benchmark %>% select(x,y,f1))
comp.perf = performance(pred = comp.bench.pred, measures = rmse)
points(i,comp.perf,col="red",pch=7)
}


#Other


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





fplot(point_eval,f="rmse.all")

plot_ly(error_bench) %>%
  add_trace(type = 'scatter',
        mode='markers',
        x=~x,
        y=~y,
        marker=list(
          color=~estimrse,
          colorbar=list(
            title='Colorbar'),
          colorscale='Viridis'))

%>%
  add_trace(f1.fetch_n,
            type = 'scatter',
            mode='markers',
            x=~x,
            y=~y,
            marker=list(
              color="black",
              visible=F))

plot_ly(f1.fetch_n) %>%
  add_trace(x=~x,y=~y,type = 'scatter',
    mode='markers',marker=list(color="black"),showlegend = F)


plot(benchmark$x,benchmark$y,col=toCol(benchresults.map[[1]]$rse-benchresults.map[[2]]$rse))

plot(point_eval$x,point_eval$y,col=toCol(point_eval$rmse.all))


plot(error_bench$x,error_bench$y,col=toCol(error_bench$estimrse))
points(f1.fetch_n$x,f1.fetch_n$y,xlim=c(-5,5),ylim=c(-5,5))

points(f1.samples$x,f1.samples$y)
#################################################################################################################################################.
source("Base.R")
#################################################################################################################################################.
#......... Approximation of 3D Value Functions ..............#

{
# Set the function here
func_i = 2
func = paste("f",func_i,sep="")

api="api"
  
# ---------------------------------------------------------------------------------------------------------------------------------------------- .
# Learn function with resampling
# ---------------------------------------------------------------------------------------------------------------------------------------------- .

# Define dense grid for observation selection
max_samples = 10000 # adjust so it is not too computationally expensive
densegrid = as.data.frame(lhs::randomLHS(n = max_samples,k=3)) %>% rename(x=V1,y=V2,z=V3) %>% mutate(x=x*10-5,y=y*10-5,z=z*10-5)
colnames(densegrid)=c("x","y","z")

# Counts all calls made so far
call_counter=0
# Maximum number of calls that can be made in loop
max_calls = 1000

# Number of observations called at the beginning
number_of_first_observations=200
# Number of new observations called per loop run
new_observations_per_call=10

# Measure evolution of performance
benchbelieve = data.frame(Round=c(0),Learner=c("none"),Believed_Perf=c(0))

# Get first observations
f.samples = as.data.frame(lhs::randomLHS(n = number_of_first_observations,k=3)) %>% rename(x=V1,y=V2) %>% mutate(x=x*10-5,y=y*10-5)
colnames(f.samples)=c("x","y","z")
res = batch_apirequest(f.samples %>% select(x,y,z), func_i, api,call_counter)
f.samples$f = res[[1]]
call_counter= res[[2]]
saveRDS(f.samples,paste(func,"-samples-",call_counter,".Rds",sep=""))


# ---------------------------------------------------------------------------------------------------------------------------------------------- .
# Preparation of learners and hyperparameters
# ---------------------------------------------------------------------------------------------------------------------------------------------- .

inner = makeResampleDesc("Subsample", iters = 4)
outer = makeResampleDesc("CV", iters = 5)
ctrl = makeTuneControlRandom(maxit = 30)

learners = list(
"regr.ksvm.tuned" = 
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
"regr.xgboost.tuned"=  
  makeTuneWrapper("regr.xgboost",
                  resampling = inner,
                  measures=rmse,
                  par.set = makeParamSet(
                    makeNumericParam(id = "nrounds", lower = log2(10/10), upper = log2(100/10), trafo = function(x) round(2^x * 10)),
                    makeIntegerParam(id = "max_depth", lower = 3L, upper = 10L),
                    makeNumericParam(id = "eta", lower = 0.001, upper = 0.3),
                    makeNumericParam(id = "gamma", lower = 0, upper = 10),
                    makeNumericParam(id = "colsample_bytree", lower = 0.3, upper = 0.7),
                    makeNumericParam(id = "min_child_weight", lower = 0, upper = 20),
                    makeNumericParam(id = "subsample", lower = 0.25, upper = 1)
                  ),
                  control = ctrl,
                  show.info = FALSE),
"regr.randomForest.tuned"=  
  makeTuneWrapper("regr.randomForest",
                  resampling = inner,
                  measures=rmse,
                  makeParamSet(
                    makeIntegerParam("nodesize", lower = 1, upper = 10, default = 1),
                    makeIntegerParam(id = "mtry", lower = 1L, upper = 2L)
                  ),
                  control = ctrl,
                  show.info = FALSE),
"regr.kknn.tuned"=  
  makeTuneWrapper("regr.kknn",
                  resampling = inner,
                  measures=rmse,
                  par.set = makeParamSet(
                    makeIntegerParam("k", lower = 2, upper = 20),
                    makeDiscreteParam("kernel", values = c("rectangular","triangular", "epanechnikov","biweight","tri-weight","cos", "inv", "gaussian", "rank","optimal"))
                  ),
                  control = ctrl,
                  show.info = FALSE),
"regr.nnet.tuned"=
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

losertable = data.frame(learner.id=c("regr.ksvm.tuned","regr.xgboost.tuned","regr.randomForest.tuned","regr.kknn.tuned","regr.nnet.tuned"),lifepoints=rep(5,5),stringsAsFactors = F)

round=1

}

# ---------------------------------------------------------------------------------------------------------------------------------------------- .
# Approximation Loop
# ---------------------------------------------------------------------------------------------------------------------------------------------- .

while(call_counter <= max_calls - new_observations_per_call) {
  {
    {
  print("-----------------------------------------------------------------------------------")
  print(paste("Round",round))
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------- .
  # Find and train best learner
  # ---------------------------------------------------------------------------------------------------------------------------------------------- .
  
  # Define tasks
  f.task = makeRegrTask(data = f.samples, target = "f")
  
  if(length(learners)>1){
    # Evaluate all learners with nested resampling
    parallelMap::parallelStartSocket(6)
    nestedResamplingResults = as.data.frame(benchmark(learners,f.task,outer,measures=rmse,show.info=FALSE,keep.pred=F,models=F),stringsAsFactors = F) %>%
      group_by(learner.id) %>% summarize(rmse = mean(rmse)) %>% arrange(rmse) %>% mutate_if(is.factor, as.character)
    parallelMap::parallelStop()
    
    # Calculate lifepoints for learners
    losertable = losertable %>% inner_join(nestedResamplingResults %>% select(learner.id,rmse),by="learner.id") %>% arrange(rmse) %>%
      mutate(lifepoints = pmin(5,lifepoints + c(10,rep(-1,length(learners)-1)))) %>% select(learner.id,lifepoints)
    # Kick out unsuccessful learners
    if(nrow(losertable %>% filter(lifepoints<1))>0){
    print("Kicking the following learners:")
    print(paste((losertable %>% filter(lifepoints<1))$learner.id))
    learners = learners[-(which(names(learners) %in% (losertable %>% filter(lifepoints<1))$learner.id))]
    }
  }
  
  # Choose best learner
  f.lrn.best = learners[[as.character(nestedResamplingResults$learner.id[1])]]
  print(paste("Best learner for this round is",f.lrn.best$id))
  
  # Adjust control strategy to have more iterations
  f.lrn.best$control = makeTuneControlRandom(maxit=100)
  
  # Train the model (automatically run hyperparameter tuning for best learner on whole dataset)
  f.mdl.best = quiet(train(learner = f.lrn.best, task = f.task))

  
  # ---------------------------------------------------------------------------------------------------------------------------------------------- .
  # Benchmark current model
  # ---------------------------------------------------------------------------------------------------------------------------------------------- .
  
  print("Performing benchmarking")
  
  
  # Calculate current performance estimate on given samples
  bench.pred = predict(f.mdl.best, newdata = f.samples %>% select(x,y,z,f))
  best.performance = performance(pred = bench.pred, measures = rmse)
  benchbelieve = benchbelieve %>% union(data.frame(Round=round,Learner=f.mdl.best$learner$id,Believed_Perf=best.performance))
  saveRDS(benchbelieve,paste(func,"benchbelieve.Rds"))
  print(paste("Believed rmse value is", best.performance))
  
  # Save the current global best model
  if(!exists("mdl.globalbest")){
    print(paste("Initialize global best model with believed performance of rmse=",best.performance,sep=""))
    mdl.globalbest = f.mdl.best
  } else {
    globalbest.pred =  predict(mdl.globalbest, newdata = f.samples %>% select(x,y,z,f))
    globalbest.performance = performance(pred = globalbest.pred, measures = rmse)
    
    
    if(globalbest.performance>best.performance){
      print(paste("New global best model with believed performance of rmse=",best.performance," (old model rmse=",globalbest.performance,")",sep=""))
      mdl.formerbest = mdl.globalbest
      mdl.globalbest = f.mdl.best
    } else {
      print(paste("Global best model remains with believed performance of rmse=",globalbest.performance," (current model rmse=",best.performance,")",sep=""))
    }
  }
  
  # Plot evolution of believed performance
  benchbelieve = benchbelieve %>% mutate(Learner=sub("regr.","",sub(".tuned","",Learner)))
  ggplot(benchbelieve[-1,],aes(x=Round,y=Believed_Perf,color=Learner)) + geom_point() +
    labs(x="Round",y="Rmse",title="Believed performance over rounds")
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------- .
  # Analyse informative value of each point
  # ---------------------------------------------------------------------------------------------------------------------------------------------- .
    
  # Define resampling strategy
  
  loo = makeResampleDesc(method = "LOO", predict="both")
  loo.learner = makeLearner(cl=getTuneResult(f.mdl.best)$learner$id,par.vals=getTuneResult(f.mdl.best)$x)
  
  # Resample best learner
  print("Performing LOO resampling...")
  
  f.perf.best = quiet(resample(loo.learner, f.task, loo, measures = rmse, show.info=F))
  
  point_eval = 
    f.perf.best$pred$data %>% filter(set=="test") %>%
    select(id,iter) %>% inner_join(f.perf.best$measures.test,by="iter") %>%
      inner_join(f.perf.best$measures.train,by="iter",suffix = c(".test",".train")) %>%
      mutate(rmse.all = sqrt((rmse.test**2 + (rmse.train**2)*(max(f.perf.best$pred$data$id)-1))/max(f.perf.best$pred$data$id)))
  point_eval = point_eval %>% arrange(id)
  point_eval = cbind(f.samples %>% select(x,y,z),point_eval)
  
  D = point_eval %>% select(x,y,z,rmse.all)
  D.task = makeRegrTask(data=D,target="rmse.all")
  
  # Make learner with hyperparameters to be tuned
  D.lrn = makeTuneWrapper("regr.kknn",
                          resampling = makeResampleDesc("CV", iters = 5),
                          measures=rmse,
                          par.set = makeParamSet(
                            makeIntegerParam("k", lower = 2, upper = 20),
                            makeDiscreteParam("kernel", values = c("rectangular","triangular","optimal"))
                          ),
                          control = makeTuneControlGrid(), show.info=F)
  # Train learner
  D.mdl = train(task=D.task,learner=D.lrn)
  
  # Predict importance on training data
  D.pred = predict(D.mdl, newdata = D)
  point_eval$estim = D.pred$data$response
  
  # Predict importance on new data
  error_bench = densegrid
  error_bench$estimrse = predict(D.mdl, newdata = error_bench %>% select(x,y,z))$data$response
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------- .
  # Fetch new observations from API, based on error estimates
  # ---------------------------------------------------------------------------------------------------------------------------------------------- .
  
  # Select points with possibly high importance to be fetched from API
  error_bench = error_bench %>% arrange(desc(estimrse))
  # Leave out points which have been fetched already
  f.fetch = error_bench %>% select(x,y,z) %>%
    setdiff(f.samples %>% select(x,y,z)) %>% inner_join(error_bench,by=c("x","y","z"))
  
  # Draw without replacement from potential points
  f.fetch_n = sample_n(f.fetch,
                        size=new_observations_per_call,
                        weight=((f.fetch$estimrse-min(f.fetch$estimrse))**10)) %>% select(x,y,z)
  f.fetch_n = f.fetch_n %>% inner_join(error_bench %>% select(x,y,z,estimrse),by=c("x","y","z")) 
  
  # Plot importance map
  importanceplot(error_bench %>% rename(f=estimrse),querydata=f.fetch_n)
  
  # Call API and add new observations to sampleset
  res = batch_apirequest(f.fetch_n %>% select(x,y,z), func_i, api, call_counter)
  f.fetch_n$f = res[[1]]
  call_counter = res[[2]]
  saveRDS(f.samples,paste(func,"-samples-",call_counter,".Rds",sep=""))
  saveRDS(error_bench,paste(func,"-infvalue-",call_counter,".Rds",sep=""))
  saveRDS(f.fetch_n,paste(func,"-fetch_n-",call_counter,".Rds",sep=""))
  
  f.samples = f.samples %>% union(f.fetch_n)
  round = round + 1
  }
  

}

# ---------------------------------------------------------------------------------------------------------------------------------------------- .
# Evaluation of Performance
# ---------------------------------------------------------------------------------------------------------------------------------------------- .

benchbelieve = benchbelieve %>% mutate(Learner=sub("regr.","",sub(".tuned","",Learner)))
ggplot(benchbelieve[-1,],aes(x=Round,y=Believed_Perf,color=Learner)) + geom_point() +
  labs(x="Round",y="Rmse",title="Believed performance over rounds")

# Show where samples have been fetched from
sliceplot(f.samples)

# ---------------------------------------------------------------------------------------------------------------------------------------------- .
# Train final model
# ---------------------------------------------------------------------------------------------------------------------------------------------- .

#Keep in mind to reset learner list
parallelMap::parallelStartSocket(6)
nestedResamplingResults = as.data.frame(benchmark(learners,f.task,outer,measures=rmse,show.info=FALSE,keep.pred=F,models=F),stringsAsFactors = F) %>%
  group_by(learner.id) %>% summarize(rmse = mean(rmse)) %>% arrange(rmse) %>% mutate_if(is.factor, as.character)
parallelMap::parallelStop()

f.lrn.best = learners[[as.character(nestedResamplingResults$learner.id[1])]]
f.lrn.best$control = makeTuneControlRandom(maxit=500)

mdl.final = quiet(train(learner = f.lrn.best, task = f.task))

# ---------------------------------------------------------------------------------------------------------------------------------------------- .
# Visualize results
# ---------------------------------------------------------------------------------------------------------------------------------------------- .

############ Samples
# 3D plot                                                                                                                                             
fplot(f.samples,f="f")

# Slice plot
sliceplot(f.samples)

############ Final Model
# 3D plot  
fplot(finalmap,f="f")

# Slice plot
regulardensegrid = as.data.frame(expand.grid(seq(-5,5,length.out=25),seq(-5,5,length.out=25),seq(-4.9999,4.99999,length.out=25)))
colnames(regulardensegrid) = c("x","y","z")
finalpreds = predict(mdl.final,newdata=regulardensegrid)
finalmap = cbind(regulardensegrid,finalpreds$data$response)%>%rename(f='finalpreds$data$response')
sliceplot(finalmap)
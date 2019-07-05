#################################################################################################################################################.
source("Base.R")
#################################################################################################################################################.
#......... Approximation of 3D Value Functions ..............#

{
  
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

# Function
func_i = 2
func = paste("f",func_i,sep="")

api="api"

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

#parallelMap::parallelStartSocket(4)
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
  
  
  # Only with our sample data
  bench.pred = predict(f.mdl.best, newdata = f.samples %>% select(x,y,z,f))
  best.performance = performance(pred = bench.pred, measures = rmse)
  benchbelieve = benchbelieve %>% union(data.frame(Round=round,Learner=f.mdl.best$learner$id,Believed_Perf=best.performance))
  saveRDS(benchbelieve,paste(func,"benchbelieve.Rds"))
  print(paste("Believed rmse value is", best.performance))
  
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
  
  plot(2:nrow(benchbelieve),benchbelieve$Believed_Perf[-1],xlab="Round",ylab="Rmse",main="Performance over rounds",col="black")
    }
  
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
  
  #fplot(f.predstat,f="rmse")
  
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
  
  # Evaluate predictions (mean squared error)
  D.pred = predict(D.mdl, newdata = D)
  point_eval$estim = D.pred$data$response
  
  #f1f2plot(point_eval,f_1 = "rmse.all",f_2 = "estim", scaleit=F)
  
  error_bench = densegrid
  error_bench$estimrse = predict(D.mdl, newdata = error_bench %>% select(x,y,z))$data$response
  #f1f2plot(error_bench,f_1 = "realrse",f_2 = "estimrse", scaleit=F)
  
  # ---------------------------------------------------------------------------------------------------------------------------------------------- .
  # Fetch new observations from API, based on error estimates
  # ---------------------------------------------------------------------------------------------------------------------------------------------- .
  
  # Select observation(s) with highest total variance to be fetched from API
  error_bench = error_bench %>% arrange(desc(estimrse))
  # Leave out observations which have been fetched already
  f.fetch = error_bench %>% select(x,y,z) %>%
    setdiff(f.samples %>% select(x,y,z)) %>% inner_join(error_bench,by=c("x","y","z"))
  
  f.fetch_n = sample_n(f.fetch,
                        size=new_observations_per_call,
                        weight=((f.fetch$estimrse-min(f.fetch$estimrse))**10)) %>% select(x,y,z)
  
  # Plot map
   plot(error_bench$x,error_bench$y,xlim=c(-5,5),ylim=c(-5,5),pch=19,col=alpha(toCol(error_bench$estimrse),1),xlab="x",ylab="y",main="Information value landscape and selected points")
   points(f.samples$x,f.samples$y,col="black",pch=3)
   points(f.fetch_n$x,f.fetch_n$y,col="red",pch=3)
  
  error_bench = error_bench %>% mutate(colour = toCol(estimrse))
  
  #plots = lapply(-5:4, function(i) plot_ly(error_bench %>% filter(z>i & z<(i+1)),x=~x,y=~y,type="scatter", name=paste(i,"<z<",i+1,sep=""),
  #                                         mode="markers",marker=list(color = ~colour, size=10,
  #                                                                   opacity=0.7), showlegend=F))
  #subplot(plots,nrows = 2)
  
  
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

parallelMap::parallelStop()

# ---------------------------------------------------------------------------------------------------------------------------------------------- .
# Evaluation of Performance
# ---------------------------------------------------------------------------------------------------------------------------------------------- .

plot(benchresults,xlab="Round",ylab="Rmse",main="Performance over rounds")

plot(benchbelieve,xlab="Round",ylab="Rmse",main="Believed performance over rounds")

# Show where samples have been fetched from
plot(f.samples$x,f.samples$y,xlim=c(-5,5),ylim=c(-5,5),xlab="x",ylab="y",col="red",main="Selected points")

# Comparing the benchmark results
f1f2plot(benchmark,f_1="rse1",f_2="rse5",scaleit=F)
f1f2plot(benchmark,f_1="f",f_2="resp5",scaleit=F)

f1f2plot(cbind(benchmark,predict(f.mdl.best, newdata = benchmark %>% select(x,y,z,f))$data),f_1="f",f_2="response",scaleit=F)
f1f2plot(cbind(benchmark,predict(mdl.globalbest, newdata = benchmark %>% select(x,y,z,f))$data),f_1="f",f_2="response",scaleit=F)
fplot(benchmark,f="f")

# ---------------------------------------------------------------------------------------------------------------------------------------------- .
# Comparison with grid approach
# ---------------------------------------------------------------------------------------------------------------------------------------------- .

plot(benchresults,xlab="Round",ylab="Rmse",main="Performance over rounds",ylim=c(0,max(benchresults)))
for(i in seq(1,51,by=1)){
tosample = 200 + (i-1)*10

#comp.samples = as.data.frame(lhs::randomLHS(n = tosample,k=3)) %>% rename(x=V1,y=V2,z=V3) %>% mutate(x=x*10-5,y=y*10-5,z=z*10-5)
  
comp.samples = as.data.frame(expand.grid(seq(-5,5,length.out=floor(tosample^(1/3))),seq(-5,5,length.out=floor(tosample^(1/3))),seq(-5,5,length.out=floor(tosample^(1/3)))))

colnames(comp.samples)=c("x","y","z")
res = batch_apirequest(comp.samples %>% select(x,y,z), func_i, "api-test3D")
comp.samples$f = res[[1]]

comp.task = makeRegrTask(data = comp.samples, target = "f")
comp.mdl = train(learner = f.lrn.best, task = comp.task)

comp.bench.pred = predict(comp.mdl, newdata = benchmark %>% select(x,y,z,f))
comp.perf = performance(pred = comp.bench.pred, measures = rmse)
points(i,comp.perf,col="red",pch=7)
}

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


#Other


bench.pred = predict(f.mdl.best, newdata = benchmark)
viz_benchmark = cbind(benchmark,bench.pred$data) %>% mutate(err=response-truth)
fplot(viz_benchmark, f = "err")
f1f2plot(viz_benchmark, f_1 = "truth", f_2="response",scaleit = F)

viz_benchmark = viz_benchmark %>% mutate(color=50*(err-min(err))/(max(err)-min(err)))
plot(viz_benchmark$x,viz_benchmark$y,col=viridisLite::viridis(50)[floor(viz_benchmark$color)])
plot(f.samples$x,f.samples$y)

f.grid = f.grid %>% mutate(color=50*(vartotal-min(vartotal))/(max(vartotal)-min(vartotal)))
plot(f.grid$x,f.grid$y,col=toCol(f.grid$varnoise))

plot(f.predstat$x,f.predstat$y,col=toCol(f.predstat$rmse))
fplot(f.predstat,f = "rmse")





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
  add_trace(f.fetch_n,
            type = 'scatter',
            mode='markers',
            x=~x,
            y=~y,
            marker=list(
              color="black",
              visible=F))

plot_ly(f.fetch_n) %>%
  add_trace(x=~x,y=~y,type = 'scatter',
    mode='markers',marker=list(color="black"),showlegend = F)


plot(benchmark$x,benchmark$y,col=toCol(benchresults.map[[1]]$rse-benchresults.map[[2]]$rse))

plot(point_eval$x,point_eval$y,col=toCol(point_eval$rmse.all))


plot(error_bench$x,error_bench$y,col=toCol(error_bench$estimrse))
points(f.fetch_n$x,f.fetch_n$y,xlim=c(-5,5),ylim=c(-5,5))

points(f.samples$x,f.samples$y)
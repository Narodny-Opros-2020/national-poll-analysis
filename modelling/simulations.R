#load required libraries. you might have to install some/all of them first
library(MASS)
library(bindata)
library(eqs2lavaan)
library(parallel)
library(INLA)
library(inlabru)
library(prodlim)
#number of subgroups of the population
N=8
#size of the population
M=700000
#largest number of categories per group
C = 4
#sample size of the collected data, the larger the better at the end of the day
S = 1000
extra_noise = F


#set.seed(200809)

#specify your covariance matrix for underlying Gaussian copulas
Sigma = matrix(rnorm(n = N*N,mean = 5,sd = 1), nrow=N, ncol=N)
Sigma = t(Sigma)%*%Sigma
#simulate number of classes in all N groups
ngroups = sample.int(n = C-1,size = N,replace = T) + 1
#simulate bernoulli dependent variables
bernoullies = rmvbin(n = M*max(ngroups),margprob = unlist(lapply(X = ngroups, FUN = function(x)1/x)),sigma = Sigma)

#simulate distributions of different correlated subgroups by categories in the population
categories = data.frame(do.call(cbind,mclapply(X = 1:N, FUN = function(i){unlist(lapply(X = 1:(M),FUN = function(j){sum(bernoullies[((j-1)*ngroups[i]+1):((j)*ngroups[i]),i])}))})))

stat.fair = do.call(cbind, mclapply(categories, summary))
ccat = cor(categories)
plotCov(ccat)


#take a biased sample with S elements from categories/you might want to change the rule for bias creation
i = sample.int(n = N,size = 1)
probs = ifelse(categories[,i]>=stat.fair[sample.int(n = 4,size = 1)+1,i],0.75,0.25)
sample.categories = data.frame(do.call(cbind,mclapply(X = 1:N,FUN = function(i){
  #decide if biased by this category is present
  categories[sample.int(n = M,size = S,prob = probs),i]
}
)))
stat.sample = do.call(cbind, mclapply(sample.categories, summary))
#see the individual biases and the mean bias of the means
print(stat.fair - stat.sample)
print(mean(abs((stat.fair - stat.sample)[4,])))

#define the data-generative model for the bernoulli trials conditional on the indicators of belonging to particular groups
true.betas = matrix(data = runif(n = N*max(ngroups),-10,10),nrow = max(ngroups),ncol = N)
#maybe ine can try multivariate gaussian with the same covariance as in the dependence between the subgroups

#generate the parameter (vote for "wind of change") of interest on the population level
y.population = as.integer(unlist(mclapply(X=1:dim(categories)[1],FUN = function(j)sum(unlist(lapply(X = 1:N,FUN = function(i)true.betas[categories[j,i],i])))))<0)
summary(y.population)
#generate the parameter (vote for "wind of change") of interest on the sample level
y.sample = as.integer(unlist(mclapply(X=1:dim(sample.categories)[1],FUN = function(j)sum(unlist(lapply(X = 1:N,FUN = function(i)true.betas[sample.categories[j,i],i])))))<0)
summary(y.sample)

#at this point you shall see biased results in most of the simulations

#build the Bayesian model on our biased data from the sample 
train.data = cbind(y.sample,sample.categories)
#we might want to add some bym latent process accross locations and/or adjust priors etc. at a later point
un_cats = unique(categories)


tmp = cbind(rep(NA,dim(un_cats)[1]),un_cats)
names(tmp) = names(train.data)
train.data=rbind(tmp,train.data)
rm(tmp)
gc()

control.family1 = list(control.link=list(model="logit"))
pcprior = list(prec = list(prior="gaussian", param = c(0,0.1)))

train.data$id = rep(1,dim(train.data)[1])

if(extra_noise)
{ train.data$idd = 1:dim(train.data)[1]
fml = as.formula(paste0("y.sample~-1+f(id, model=\"iid\", hyper = pcprior)+f(idd, model=\"iid\", hyper = pcprior)+",paste("f(",names(sample.categories),", model=\"iid\",hyper =pcprior)",collapse = "+")))
}else 
{  
  fml = as.formula(paste0("y.sample~-1+f(id, model=\"iid\", hyper = pcprior)+",paste("f(",names(sample.categories),", model=\"iid\",hyper =pcprior)",collapse = "+")))
}
model.inla = inla(fml,family = "binomial",Ntrials = 1,data = train.data,control.compute=list(config = TRUE),control.family=control.family1,
                  control.predictor=list(compute=T,link = 1))
summ.inla = summary(model.inla)
print(summ.inla)
print(model.inla$summary.random)

#prediction
res = c(1,3,4,5,6)*0
#sum of groups count
Ns = 0
ncats = unlist(mclapply(X=1:(dim(categories)[1]), FUN = function(i) paste0(categories[i,], collapse = "+")))
g=function(x)
{
  return((x = 1/(1+exp(-x))))
}

#specify prediction given cell function
predict.cat = function(cell)
{
  patrtn = paste0(un_cats[cell,],collapse = "+")
  ns = sum(ncats == patrtn)
  res = g(summ.inla$linear.predictor[cell,c(1,3,4,5,6)])
  return(list(preds = res,Ncell = ns))
}

predicts = mclapply(X = 1:(dim(un_cats)[1]),FUN = predict.cat)

res = c(1,3,4,5,6)*0
Ns = 0

for(el in predicts)
{
  res = res + el$preds*el$Ncell
  Ns = Ns + el$Ncell
}
res = res/Ns

print(res)
summary(y.sample)[4]
#population rate
summary(y.population)[4]
plot(x = c(1,1,1,1,1,1,1),y=c(res[1],res[2],res[3],res[4],res[5],summary(y.population)[4],summary(y.sample)[4]),col = c(3,2,4,2,5,1,6),xlab = "",ylab="parameter")
legend(0.6, max(res), legend=c("Truth", "Biased","Corr.Mean","95% CI","Corr.Mode","0.5quant"),
       col=c(1,6,3,2,5,4), lty=1, cex=0.8,
       title="Parameters", text.font=1, bg='lightgrey')




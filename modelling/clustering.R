library(MASS)
library(bindata)
library(eqs2lavaan)
library(parallel)
library(INLA)
library(inlabru)
library(prodlim)
library(dplyr)
library(tidyr)
library(spdep)
library(poLCA)


set.seed(200809)
setwd("national-poll-analysis")

#maximal number of latent states in the mixtures allowed
select.size = 7

#see the main inference script 
#for detail
upsample_street = T
extra_noise = F
spat_obl =  T
prefix = "spat_temp_"


#if one reads in the model or computes from scratch
compute = T

#type of data and indecies of candidates and  edit to calculate for different cases
type = 'both'
targets = c("Other", 'Лукашенко', 'Тихановская', 'Дмитриев', 'Черечень', 'Канопацкая', 'Против всех','Да')


#reads prepared data and select X and y fields
dataFN = 'data/prepared_viber_votes_release.csv'
dataFN_STR = 'data/prepared_street_votes_release.csv'
catsFN = 'data/reg_lt_age_gen_edu.csv'

#reads prepared data and select X and y fields
votes_viber = read.csv(dataFN, encoding='UTF-8', na.strings=c("","NA"), stringsAsFactors=T)
votes_str = read.csv(dataFN_STR, encoding='UTF-8', na.strings=c("","NA"), stringsAsFactors=T)
#reads unique categories with count
un_cats = read.csv(catsFN, encoding="UTF-8", stringsAsFactors=T)
if(spat_obl)
{
  
  library(raster)
  library(ggplot2)
  lit  = getData("GADM",country="Lithuania",level=0)
  bel    = getData("GADM",country="Belarus",level=1)
  ukr    = getData("GADM",country="Ukraine",level=0)
  lat = getData("GADM",country="Latvia",level=0)
  pol = getData("GADM",country="Poland",level=0)
  rus = getData("GADM",country="Russia",level=0)
  
  
  ggplot(bel,aes(x=long,y=lat,group=group))+
    geom_polygon(aes(fill=id),color="grey30")+
    geom_polygon(data=lit,fill="grey60",color="grey80")+
    geom_polygon(data=lat,fill="grey60",color="grey80")+
    geom_polygon(data=ukr,fill="grey60",color="grey80")+
    geom_polygon(data=pol,fill="grey60",color="grey80")+
    geom_polygon(data=rus,fill="grey60",color="grey80")+
    coord_map(xlim=c(-1,1)+bbox(bel)["x",],ylim=c(-1,1)+bbox(bel)["y",])+
    scale_fill_discrete(guide="none")+
    theme_bw()+theme(panel.grid=element_blank())
  
  library(spdep)
  
  nbadj = poly2nb(pl = bel)
  nb2INLA("adj.bel", nbadj)
  gf.reg = inla.read.graph("adj.bel")
  summary(gf.reg)
  
  
  
}
if(upsample_street) {
  ups_n = as.integer(dim(votes_viber)[1]/2)
  votes_str = votes_str[sample.int(n = dim(votes_str)[1],size = ups_n,replace = TRUE),]
  votes_viber_train_id = sample.int(n=dim(votes_viber)[1], size = ups_n)
  votes_viber_test =  votes_viber[-votes_viber_train_id,]
  votes_viber = votes_viber[votes_viber_train_id,]
}

names(un_cats)[3] = "gender" 

for (index in 1:length(targets)){
  
  if(index == 1)
  {
    
    votes_str$target = "Other"
    votes_viber$target = "Other"
    votes_str$target[votes_str$candidate==targets[1]]=targets[1]
    votes_str$target[votes_str$candidate==targets[2]]=targets[2]
    votes_viber$target[votes_viber$candidate==targets[1]]=targets[1]
    votes_viber$target[votes_viber$candidate==targets[2]]=targets[2]
    votes_str$target = as.factor(votes_str$target)
    votes_viber$target = as.factor(votes_viber$target)
    
  }else
  {
  
    if(index == 8)
    {
      target_id = "early_voting"
    }else{
      target_id = "candidate"
    }
    
    target = targets[index]
    
    votes_str$target = as.integer(votes_str[[target_id]] == target)
    votes_viber$target = as.integer(votes_viber[[target_id]] == target)
    print(target)
    
  }


  c_names = c("age","education","gender","location_type","region")
  
  #prepare data for inference
  viber_uniques = unique(votes_str$use_viber)
  income_uniques = unique(votes_viber$income)
  education_uniques = unique(votes_viber$education)
  augmented_cats = un_cats[,c("count", c_names)]#crossing(, viber_uniques, income_uniques, education_uniques)
  
  tmp = cbind(augmented_cats[,-1],  rep(NA,dim(augmented_cats)[1]))
  names(tmp) = c("age","education","gender", "location_type","region","target")
  
  if(type=="both")
  {
    votes=rbind(tmp,votes_str[,names(tmp)],votes_viber[,names(tmp)])
  }else if(type=="viber")
  {
    votes=rbind(tmp,votes_viber[,names(tmp)])
  }else
  {
    votes=rbind(tmp,votes_str[,names(tmp)])
  }
  
  if(spat_obl)
  {
    votes$rid = 0
    votes$rid[votes$region=="Брестская"] = 1
    votes$rid[votes$region=="Витебская"] = 6
    votes$rid[votes$region=="Гродненская"] = 3
    votes$rid[votes$region=="Гомельская"] = 2
    votes$rid[votes$region=="Могилевская"] = 4
    votes$rid[votes$region=="Минск"|votes$region=="Минская"] = 5
  }
  
  rm(tmp)
  gc()

  if(extra_noise)
    c_names = c("gender","location_type","region")
  
  g=function(x)
  {
    return((x = 1/(1+exp(-x))))
  }
  control.family1 = list(control.link=list(model="logit"))
  pcprior = list(prec = list(prior = "pc.prec", param=c(1,0.1)))
  
  
  votes$id = rep(1,dim(votes)[1])
  

  if(extra_noise)
  { 
    votes$ageid = as.integer(as.factor(votes$age))
    votes$eduid = 0
    votes$eduid[votes$education=="Базовое / Среднее общее (школа)"] = 2
    votes$eduid[votes$education== "Профессионально-техническое"] = 3
    votes$eduid[votes$education=="Среднее специальное"] = 4
    votes$eduid[votes$education=="Высшее"] = 5
    votes$eduid[votes$education=="Другое"] = 1
  }
    
  if(index == 1)
  {
    #select the best mixture model on the set from 1 to select.size
    ress = NULL     
    minBIC=Inf
    best.mixture=NULL
    best.j = 0
    library(poLCA)
    votes$target = as.factor(votes$target)
    for(j in 1:select.size)
    {
      ress[[j]] = poLCA::poLCA(formula = cbind(age,education,gender,location_type,region,target)~1, data = votes[which(!is.na(votes$target)),c(1,2,3,4,5,6)],nclass = j,graphs = F)
      if(ress[[j]]$bic<minBIC)
      {
        minBIC = ress[[j]]$bic
        best.mixture = ress[[j]] 
        best.j = j
      }
    }
    print(best.mixture)
    save(best.mixture,file = "data/model.mixtute.withcand.best.Rdata")
  }else
  {
  
    if(compute){
      votes$target = as.factor(votes$target)
      best.mixture.dummy = poLCA::poLCA(formula = cbind(age,education,gender,location_type,region)~1, data = votes[,c(1,2,3,4,5)],nclass = best.j,graphs = F)
      print(best.mixture.dummy)
 
      compute = F
      load("data/model.mixtute.withcand.best.Rdata")
      
      #set parameters of mixture from the optimal one (wrt BIC) for all multinomials but the vote, which we can't use on test data
      best.mixture.dummy$probs$age = best.mixture$probs$age
      best.mixture.dummy$probs$education = best.mixture$probs$education
      best.mixture.dummy$probs$gender = best.mixture$probs$gender
      best.mixture.dummy$probs$location_type = best.mixture$probs$location_type
      best.mixture.dummy$probs$region = best.mixture$probs$region
  
      best.mixture.dummy$probs.se$age = best.mixture$probs.se$age
      best.mixture.dummy$probs.se$education = best.mixture$probs.se$education
      best.mixture.dummy$probs.se$gender = best.mixture$probs.se$gender
      best.mixture.dummy$probs.se$location_type = best.mixture$probs.se$location_type
      best.mixture.dummy$probs.se$region = best.mixture$probs.se$region
  
      best.mixture.dummy$P.se = best.mixture$P.se
      best.mixture.dummy$P = best.mixture$P
      
      compute = F
     
      best.mixture= best.mixture.dummy
      save(best.mixture,file = "data/model.mixtute.best.heurist.Rdata")
      rm(best.mixture.dummy)
      gc()
      
    }else
    {
      load("data/model.mixtute.best.heurist.Rdata")
    }
  
    
    #load the INLA model
    print('Loading a model ....')
    load(paste0("data/", prefix, "model_", target, '_','_',upsample_street,'_', type,'_', spat_obl, "_noups.gzip"))
  
    summ.inla = summary(model.inla)
    print(summ.inla)
    print(model.inla$summary.random)
    
    #postsratify to the subpopulations
    nas = 0
    res = as.data.frame(array(0, dim = c(dim(best.mixture$probs[[1]])[1],11)))
    Ns = rep(0,dim(best.mixture$probs[[1]])[1])
    ucat = unique(augmented_cats)
    ncats = c(unlist(mclapply(X=1:(dim(augmented_cats)[1]),FUN = function(i) paste0(as.character(augmented_cats[i,-1]),collapse = "+"))))
    utest = unique(votes_viber_test[,c(target_id,names(augmented_cats)[-1])])
    tcats = c(unlist(mclapply(X=1:(dim(utest)[1]),FUN = function(i) paste0(as.character(utest[i,-1]),collapse = "+"))))
    preds = NULL
    augmented_cats_tmp = augmented_cats
    for(str in c("age", "education", "gender", "location_type", "region"))
    {
      augmented_cats_tmp[[str]] = as.numeric(as.factor(augmented_cats_tmp[[str]]))
    }
    classes = poLCA::poLCA.posterior(best.mixture, y = ((augmented_cats_tmp[,-1])))
    for(cell in 1:dim(augmented_cats)[1])
    {
      #which cluster we are going to infer
      for(cluster_id in 1:dim(best.mixture$probs[[1]])[1])
      {
        if(which.max(classes[cell,])==cluster_id)
        {
        rcur = g(summ.inla$linear.predictor[cell,c(1,3,4,5,6,7,8,9,10,11,12)]) 
        res[cluster_id,] = res[cluster_id,] + rcur*as.integer(augmented_cats[cell,'count'])
        if (anyNA(res)) {
          nas = c(nas, cell)
        }
        Ns[cluster_id] = Ns[cluster_id] + as.integer(augmented_cats[cell,'count'])
        }
        
      }
    
    }
    for(cluster_id in 1:dim(best.mixture$probs[[1]])[1])
    {
      res[cluster_id,]=res[cluster_id,]/as.integer(Ns[cluster_id])
    }
    names(res) = names(rcur)
    print(res)
    write.csv(res,file = paste0("data/", "subclusters_",targets[index],".csv"))
  }
}



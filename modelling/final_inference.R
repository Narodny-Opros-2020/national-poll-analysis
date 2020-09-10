#load the required libraries
#note that you might have to install some of them
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

#fix the seed for reproducibility
set.seed(200809)

#set the working folder
setwd("national-poll-analysis/")

#whether we shall upsample steet data to the size of 50% viber data
#and downsample the viber data to the 50% of its size
upsample_street = F

#whether to add ar1 structure to ordinal predictictors
extra_noise = T
#whether to use BYM2 spatial field in the model
spat_obl =  T
prefix = "spat_temp_"
#data structure to store the results
overall_res = NULL
#if one reads in the model or computes from scratch
compute = T
#"viber", "street" or "both"
type = 'street'

#spesify the set of marginal responses we are going to address
targets = c('Лукашенко', 'Тихановская', 'Дмитриев', 'Черечень', 'Канопацкая', 'Против всех','Да')


#reads prepared data and select X and y fields
dataFN = 'data/prepared_viber_votes_release.csv'
dataFN_STR = 'data/prepared_street_votes_release.csv'
catsFN = 'data/reg_lt_age_gen_edu.csv'

#read the data in
votes_viber = read.csv(dataFN, encoding='UTF-8', na.strings=c("","NA"), stringsAsFactors=T)
votes_str = read.csv(dataFN_STR, encoding='UTF-8', na.strings=c("","NA"), stringsAsFactors=T)
#read unique cells with count in
un_cats = read.csv(catsFN, encoding="UTF-8", stringsAsFactors=T)


if(spat_obl)
{
  #prepare the data for BYM2
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

votes_viber_test =  votes_viber
if(upsample_street) {
  ups_n = as.integer(dim(votes_viber)[1]/2)
  votes_str = votes_str[sample.int(n = dim(votes_str)[1],size = ups_n,replace = TRUE),]
  votes_viber_train_id = sample.int(n=dim(votes_viber)[1], size = ups_n)
  votes_viber_test =  votes_viber[-votes_viber_train_id,]
  votes_viber = votes_viber[votes_viber_train_id,]
}
names(un_cats)[3] = "gender" 

for (index in 1:length(targets)){

  if(index == 7)
  {
    target_id = "early_voting"
  }else{
    target_id = "candidate"
  }
  
  target = targets[index]
  print(target)
  print(type)
  
  

  #creates target variable 
  votes_str$target = as.integer(votes_str[[target_id]] == target)
  votes_viber$target = as.integer(votes_viber[[target_id]] == target)
  
  
  #X column names
  c_names = c("age","education","gender","location_type","region")
  #prepare data for inference
  viber_uniques = unique(votes_str$use_viber)
  income_uniques = unique(votes_viber$income)
  education_uniques = unique(votes_viber$education)
  augmented_cats = un_cats[,c("count", c_names)]
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
  
  #X column names
  if(extra_noise)
    c_names = c("gender","location_type","region")
  
  #define the link function
  g=function(x)
  {
    return((x = 1/(1+exp(-x))))
  }
  control.family1 = list(control.link=list(model="logit"))
  #define the prior for precision
  pcprior = list(prec = list(prior = "pc.prec", param=c(1,0.1)))
  #define the id for iid effects
  votes$id = rep(1,dim(votes)[1])
  
  #define the model
  if(extra_noise)
  { 
    votes$ageid = as.integer(as.factor(votes$age))
    votes$eduid = 0
    votes$eduid[votes$education=="Базовое / Среднее общее (школа)"] = 2
    votes$eduid[votes$education== "Профессионально-техническое"] = 3
    votes$eduid[votes$education=="Среднее специальное"] = 4
    votes$eduid[votes$education=="Высшее"] = 5
    votes$eduid[votes$education=="Другое"] = 1
    

    if(spat_obl)
    {
      fml = as.formula(paste0("target~-1+ f(rid,
    model=\"bym2\",graph=gf.reg)+f(id, model=\"iid\", hyper = pcprior)+f(ageid, model=\"ar1\", hyper = pcprior)+f(eduid, model=\"ar1\", hyper = pcprior)+",paste("f(",c_names,", model=\"iid\",hyper =pcprior)",collapse = "+")))
    }else
      fml = as.formula(paste0("target~-1+f(id, model=\"iid\", hyper = pcprior)+f(ageid, model=\"ar1\", hyper = pcprior)+f(eduid, model=\"ar1\", hyper = pcprior)+",paste("f(",c_names,", model=\"iid\",hyper =pcprior)",collapse = "+")))
  }else 
  {  
    if(spat_obl)
    {
      fml = as.formula(paste0("target~-1+ f(rid,
    model=\"bym2\",graph=gf.reg)+f(id, model=\"iid\", hyper = pcprior)+",paste("f(",c_names,", model=\"iid\",hyper =pcprior)",collapse = "+")))
    }else
      fml = as.formula(paste0("target~-1+f(id, model=\"iid\", hyper = pcprior)+",paste("f(",c_names,", model=\"iid\",hyper =pcprior)",collapse = "+")))
  }
  if(compute)
  {  
    #estimate parameters of the model
    model.inla = inla(fml,family = "binomial",Ntrials = 1,quantiles=c(0.025, 0.5, 0.975,0.0005,0.9995,0.005,0.995,0.05,0.95),data = votes,control.compute=list(config = TRUE,dic=TRUE, waic=TRUE),control.family=control.family1,
                    control.predictor=list(compute=T,link = 1,quantiles=c(0.025, 0.5, 0.975,0.0005,0.9995,0.005,0.995,0.05,0.95)))
  }else{
   
     #load the model
     load(paste0(prefix,"model_", target, '_','_',upsample_street,'_', type,'_', spat_obl, "_noups.gzip"))
  }
  #look at the fitted model
  summ.inla = summary(model.inla)
  print(summ.inla)
  print(model.inla$summary.random)
  
  #perform poststratification
  nas = 0
  #prediction
  res = c(1,3,4,5,6,7,8,9,10,11,12)*0
  #sum of groups count
  Ns = 0
  ucat = unique(augmented_cats)
  ncats = c(unlist(mclapply(X=1:(dim(augmented_cats)[1]),FUN = function(i) paste0(as.character(augmented_cats[i,-1]),collapse = "+"))))
  utest = unique(votes_viber_test[,c(target_id,names(augmented_cats)[-1])])
  tcats = c(unlist(mclapply(X=1:(dim(utest)[1]),FUN = function(i) paste0(as.character(utest[i,-1]),collapse = "+"))))
  preds = NULL
  for(cell in 1:dim(augmented_cats)[1])
  {
    
    rcur = g(summ.inla$linear.predictor[cell,c(1,3,4,5,6,7,8,9,10,11,12)]) 
    res = res + rcur*as.integer(augmented_cats[cell,'count'])
    
    id_test = which(tcats==ncats[cell])
    for(ids in id_test)
    {
      if(!is.na(utest[cell,1]))
        preds = rbind(preds,c(as.integer(utest[cell,1]==target),rcur$mean,rcur$`0.5quant`,rcur$mode,rcur$`0.025quant`,rcur$`0.975quant`))
    }
    
    if (anyNA(res)) {
      nas = c(nas, cell)
    }
    Ns = Ns + as.integer(augmented_cats[cell,'count'])
  }
  res=res/as.integer(Ns)
  mbrier = mean(min((preds[,1]- preds[,-1])^2)/(1-(preds[,5]-preds[,6])^2))
  print(mbrier)
  print(res)
  summary(votes$target)[4]
  overall_res = cbind(overall_res,unlist(c(res,model.inla$mlik[2],model.inla$waic[1],model.inla$dic[1],mbrier)))
  
  if(compute)
  {
    #save the results
    save(model.inla, file = paste0("data/", prefix,"model_", target, '_','_',upsample_street,'_', type,'_', spat_obl, "_noups.gzip"))
    save.image(file = paste0("data/", prefix,"image_", target, '_','_',upsample_street,'_', type,'_', spat_obl, "_noups.gzip"), compress=T)
  }


  #save the results
  write.csv(c(res,model.inla$mlik[2],model.inla$waic[1],model.inla$dic[1],mbrier), paste0("data/", prefix,'spat_noups_', target,'_',upsample_street,'_', '_', type,'_', spat_obl,'_noups.csv'), row.names=F, fileEncoding='UTF-8')

}

#save all results
write.csv(overall_res, paste0("data/", prefix,'all_results','_',upsample_street,'_', '_', type,'_', spat_obl,'_noups.csv'), row.names=F, fileEncoding='UTF-8')


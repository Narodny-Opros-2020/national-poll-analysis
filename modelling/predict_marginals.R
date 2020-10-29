#!/usr/bin/env Rscript

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

targets = c('Лукашенко', 'Тихановская', 'Дмитриев', 'Черечень', 'Канопацкая', 'Против всех','Да')

#set the working folder
setwd("national-poll-analysis/")

catsFN = 'reg_lt_age_gen_edu.csv'

c_names = c("age","education","gender","location_type","region")
un_cats = read.csv(catsFN, encoding="UTF-8", stringsAsFactors=T)
names(un_cats)[3] = "gender"
augmented_cats = un_cats[,c("count", c_names)]

g <- function(x)
{
  return((x = 1/(1+exp(-x))))
}

predict_res <- function(cats, cat){
  summ.inla = summary(model.inla)
  # print(paste('cats size:', dim(cats)[1]))
  res_df = NULL
  for (i in 1:dim(cats)[1]){
    nas = 0
    #prediction
    res = c(1,3,4,5,6,7,8,9,10,11,12)*0
    #sum of groups count
    Ns = 0
    
    selected = augmented_cats[augmented_cats[cat] == as.character(cats[i, cat]),]
    
    r_names = rownames(selected)
    # print(paste('r_names:', length(r_names)))
    for(cell in r_names)
    {
      cell_i = as.integer(cell)
      rcur = g(summ.inla$linear.predictor[cell_i,c(1,3,4,5,6,7,8,9,10,11,12)]) 
      res = res + rcur*as.integer(augmented_cats[cell_i,'count'])
      
      if (anyNA(res)) {
        nas = c(nas, cell_i)
      }
      Ns = Ns + as.integer(augmented_cats[cell_i,'count'])
    }
    Ns_ = as.integer(Ns)
    res=res/Ns_
    #print(paste('cur res:', res))
    
    rownames(res) = NULL
    res_df = rbind(res_df, res)
    print(nas)
  }
  res_df['category'] = cat
  res_df['cat_val'] = cats[cat]
  
  return(res_df)
}


pred = NULL
for (target in targets){
  m_name = paste0('data/', prefix,"model_", target, '_','_',upsample_street,'_', type,'_', spat_obl, "_noups.gzip")
  # print(paste('Loading model', m_name))
  load(m_name)
  
  for (clmn in names(augmented_cats[,-1])) {
    grouped = augmented_cats %>% group_by_at(clmn) %>% summarize(count = sum(count))
    pred_c = predict_res(data.frame(grouped), clmn)
    # print(paste('Prediction', clmn, ':'))
    # print(pred_c)
    pred_c['target'] = target
    
    pred = rbind(pred, pred_c)
  }
}

write.csv(pred, paste0('data/', type, '_marginal_cats_pred_spat_noups.csv'), row.names=F, fileEncoding='UTF-8')

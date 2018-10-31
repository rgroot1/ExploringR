# training

library(dplyr)

library(lubridate)

library(RCurl)

library(jsonlite)

library(digest)

library(RJDBC)

library(aetnar)

library(h2o)

library(h2osteam)





conn<-h2osteam.login(url = "https://xhadsteam1p.aetna.com:9000",

                     username = "a113765", #aid

                     password = "pwd",

                     verify_ssl=FALSE)



cluster_config<-h2osteam.start_h2o_cluster(conn=conn,cluster_name = "h2o1",node_memory = "16G",

                                           num_nodes =12, h2o_version = "3.14.0.6")



h2o.connect(config = cluster_config$connect_params)

h2o.ls()

dir_nm <- paste("~/grand_er/dev/data/model", Sys.Date(), sep="/")

dir.create(dir_nm, showWarnings = FALSE)

setwd(dir_nm)



# Parameter Setting'

variable_remove =c('pst_3m_avd_er','pst_3m_allowed_amt_avoid','pst_3m_pvt_er','pst_3m_allowed_amt_prevent','pst_3m_injury_er','pst_3m_allowed_amt_injury',

                   'pst_3m_er','pst_3m_allowed_amt_all_er','pst_has_pvt','pst_has_injury', 'rk', 'has_avoidable_6mo', 'zip_cd',

                   'rk_aer', 'rk_per', 'zip4')







#des<-read.csv("~/R/er_model_v3/05-20/tabledes.csv")$col_name

des <-read.hive('describe dev_er_enc.er_model_uer_dat1_train_v7')$col_name

#s<- read.hive('select * from dev_er_enc.er_model_v6_uer_dat1_train')

#des <- colnames(s)



table <- "er_model_uer_dat1_train_v7"

hdfsDatabase <- paste0("hdfs://prodmid:8020/dev/derived/consumer_analytics/er/",table)

model_set<- h2o.importFile(path=hdfsDatabase, destination_frame="model_set", col.names=des)



# change data types

model_set$pst_has_avd <- as.factor(model_set$pst_has_avd)

model_set$local_mkt_cd <- as.factor(model_set$local_mkt_cd)

model_set$mbr_rtp_type_cd <- as.factor(model_set$mbr_rtp_type_cd)

model_set$infant_ind <- as.factor(model_set$infant_ind)

model_set$gender_cd <- as.factor(model_set$gender_cd)

model_set$business_ln_cd <- as.factor(model_set$business_ln_cd)

model_set$fund_ctg_cd<- as.factor(model_set$fund_ctg_cd)

model_set$has_pcp<- as.factor(model_set$has_pcp)

model_set$nn_uc_miles<- as.numeric(model_set$nn_uc_miles)

model_set$nn_er_miles<- as.numeric(model_set$nn_er_miles)

model_set$diff_dist<- as.numeric(model_set$diff_dist)

model_set$sd_avd_days<- as.numeric(model_set$sd_avd_days)

model_set$base_avd_days<- as.factor(model_set$base_avd_days)

model_set$avg_avd_days<- as.factor(model_set$avg_avd_days)

model_set$sd_pvt_days<- as.numeric(model_set$sd_pvt_days)

model_set$base_pvt_days<- as.factor(model_set$base_pvt_days)

model_set$avg_pvt_days<- as.factor(model_set$avg_pvt_days)

model_set$avg_days<- as.factor(model_set$avg_days)

model_set$avd_er_pct<- as.numeric(model_set$avd_er_pct)

model_set$prspctv_rsk_nbr <- as.numeric(model_set$prspctv_rsk_nbr)

model_set$mnths_since_er<- as.factor(model_set$mnths_since_er)

model_set$mnths_since_avd_er<- as.factor(model_set$mnths_since_avd_er)

model_set$days_since_er<- as.factor(model_set$days_since_er)

model_set$days_since_avd_er<- as.factor(model_set$days_since_avd_er)

model_set$days_since_injury_er<- as.factor(model_set$days_since_injury_er)

model_set$pcp_hhi_1yr <- as.numeric(model_set$pcp_hhi_1yr)

model_set$avg_days<- as.factor(model_set$avg_days)

model_set$sd_days<- as.numeric(model_set$sd_days)

model_set$family_type <- as.factor(model_set$family_type)

model_set$null_uc <- as.factor(model_set$null_uc)

model_set$null_er <- as.factor(model_set$null_er)

model_set$has_claim_12mo <- as.factor(model_set$has_claim_12mo)

model_set$plan_sponsor <- as.factor(model_set$plan_sponsor)

model_set$zip2 <- as.factor(model_set$zip2)

model_set$urbsubr <- as.factor(model_set$urbsubr)



# impute missing values

#h2o.getId(model_set)

h2o.impute(model_set, "prspctv_rsk_nbr", "median")

h2o.impute(model_set, "ylm_median_income", "median")

h2o.impute(model_set, "nn_uc_miles", "median")

h2o.impute(model_set, "nn_er_miles", "median")

h2o.impute(model_set, "diff_dist", "median")

h2o.impute(model_set, "avg_dist", "median")

h2o.impute(model_set, "pcp_hhi_1yr", "median")





# separate x and y

y <- 'pst_has_avd'

X <- names(model_set)

X <- X[(!X %in% variable_remove) &(!grepl('_id',X)) &(!grepl('_record_type',X)) &(!(X==y)) ]





model_set_t <- model_set[model_set$model_record_type == 'TRAIN', ]

model_set_v <- model_set[model_set$model_record_type == 'VALIDATION', ]

# separate modeling set

split <- h2o.splitFrame(model_set_t, c(0.6, 0.2), seed = 1234)

train <- h2o.assign(split[[1]],"train") 

valid <- h2o.assign(split[[2]],"valid")

test <- h2o.assign(split[[3]],"test")



# grid search on logistic regression

#glm_hyper_params <- list( lambda = c(1, 0.5, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0), alpha = c(0, .25, .5, .75, .1))

id <- as.character(round(runif(1,1,100000)))

#glm_grid_id = paste("glm_avd_dat1", Sys.Date(), id, sep="_")



#glm_grid <- h2o.grid(x = X, y = y, family = 'binomial', 

#                     training_frame = train, validation_frame = valid,

#                     algorithm = "glm", grid_id = glm_grid_id, 

#                     hyper_params = glm_hyper_params,

#                     search_criteria = list(strategy = "Cartesian"),

#                     nfolds = 3, 

#                     fold_assignment = "Modulo",

#                     keep_cross_validation_predictions = TRUE)



## Sort the grid models by AUC

#glm_sortedGrid <- h2o.getGrid(glm_grid_id, sort_by = "auc", decreasing = TRUE)

#glm_model <- h2o.getModel(glm_sortedGrid@model_ids[[1]])

#glm_varimp<-h2o.varimp(glm_model)



#save the machine generated java codes about model

#h2o.download_pojo(glm_model,dir_nm)

#glm.perf<-h2o.performance(glm_model,newdata =test)

#glm.perf.hd <-h2o.performance(glm_model,newdata = model_set_v)



#glm.precision<-h2o.precision(h2o.performance(glm_model,newdata=test))

#glm.recall<-h2o.recall(h2o.performance(glm_model,newdata=test))

#glm.f1<-h2o.F1(h2o.performance(glm_model,newdata =test))



### Save h2o into hdfs

#model_path <- sprintf("hdfs://prodmid:8020/dev/derived/consumer_analytics/er_h20_model")

#glm_model_path <- h2o.saveModel(glm_model,path = model_path)

#write(glm_model_path, file = "repeat_uer_model_path_glm.txt", append = FALSE, sep = " ")



# random search with gbm 

learn_rate_opt <- c(0.01, 0.02, 0.03)

ntrees_opts = seq(50, 200, 50)

max_depth_opts = seq(2, 20, 2)

sample_rate_opts = c(0.7, 0.8, 0.9, 1.0)

col_sample_rate_opts = seq(0.2, 0.9, 0.1)

col_sample_rate_per_tree_opts = seq(0.3,1,0.05)

nbins_cats_opts = seq(50,1000,50) 

nfolds <- 3  



hyper_params = list(learn_rate = learn_rate_opt,

                    ntrees = ntrees_opts,

                    max_depth = max_depth_opts,

                    sample_rate = sample_rate_opts,

                    col_sample_rate = col_sample_rate_opts,

                    col_sample_rate_per_tree = col_sample_rate_per_tree_opts,

                    nbins_cats = nbins_cats_opts)



search_criteria = list(strategy = "RandomDiscrete", #random pick up params from the hyper_params in list() prevent to memory the training data too much

                       max_runtime_secs = 600, 

                       max_models = 10, 

                       stopping_metric = "AUC", #"AUC", 

                       stopping_tolerance = 0.0001, 

                       stopping_rounds = 5,

                       seed = 1234)



gbm_grid_id = paste("gbm_avd_dat1", Sys.Date(), id, sep="_")





er_gbm_grid <- h2o.grid("gbm", 

                        grid_id = gbm_grid_id,

                        x = X, 

                        y = y, 

                        training_frame = train,

                        validation_frame = valid,

                        

                        # per model stopping criteria 

                        stopping_rounds = 2,

                        stopping_tolerance = 1e-3,

                        stopping_metric = "AUC",

                        

                        # how often to score (affects early stopping, training speed)

                        score_tree_interval = 5, 

                        

                        # seed to control sampling 

                        seed = 12345,

                        # grid serach options

                        hyper_params = hyper_params,

                        search_criteria = search_criteria,

                        nfolds = 3, 

                        fold_assignment = "Modulo",

                        keep_cross_validation_predictions = TRUE)



# show grid results

sorted_grid <- h2o.getGrid(grid_id=gbm_grid_id)

#print(sorted_grid)



# select best model

gbm_model <- h2o.getModel(sorted_grid@model_ids[[1]])

#summary(gbm_model)



# use variable importance to get insight into important relationships

gbm_varimp<-h2o.varimp(gbm_model)





#save the machine generated java codes about model

h2o.download_pojo(gbm_model,dir_nm)

gbm.perf<-h2o.performance(gbm_model,newdata =test)

#gbm.perf.hd <-h2o.performance(gbm_model,newdata = model_set_hd)



gbm.precision<-h2o.precision(h2o.performance(gbm_model,newdata=test))

gbm.recall<-h2o.recall(h2o.performance(gbm_model,newdata=test))

gbm.f1<-h2o.F1(h2o.performance(gbm_model,newdata =test))



### Save h2o into hdfs

#model_path <- sprintf("hdfs://prodmid:8020/dev/derived/consumer_analytics/er_h20_model")

gbm_model_path <- h2o.saveModel(gbm_model,path = model_path)

write(gbm_model_path, file = "repeat_uer_model_path_gbm.txt", append = FALSE, sep = " ")





#traing random forest model

rf_grid_id = paste("rf_avd_dat1", Sys.Date(), id, sep="_")

er_rf <- h2o.randomForest(

  ### Use Full Training Set (no split) for Cross-Validation ###

  x = X, 

  y = y, 

  training_frame = train,

  validation_frame = valid,

  ### Added Parameters ###

  ntrees = 150                      # Up to 500 decision trees in the forest 

  ,max_depth = 20                    # trees can grow to depth of 30

  #,seed = 1234                       # SLOW: remove for deployment

  ,stopping_rounds = 5               # stop after validation error does not decrease for 5 iterations/new trees

  ,score_each_iteration = TRUE       # score validation error on every iteration/new tree

  ,model_id = rf_grid_id     # for easy lookup in

  ### Cross-Validation ###

  ,nfolds = 3

  ,fold_assignment = "Modulo"

  ,keep_cross_validation_predictions = TRUE

)



#er_rf

rf_varimp<-h2o.varimp(er_rf)

h2o.download_pojo(er_rf,dir_nm)

rf.perf<-h2o.performance(er_rf,newdata =test)

#rf.perf.hd<-h2o.performance(er_rf,newdata =model_set_hd)



rf.precision<-h2o.precision(h2o.performance(er_rf,newdata=test))

rf.recall<-h2o.recall(h2o.performance(er_rf,newdata=test))

rf.f1<-h2o.F1(h2o.performance(er_rf,newdata =test))



### Save h2o into hdfs

rf_model_path <- h2o.saveModel(er_rf,path = model_path)

write(rf_model_path, file = "repeat_uer_model_path_rf.txt", append = FALSE, sep = " ")





# Train a stacked ensemble using the GBM and RF

ensemble_grid_id = paste("ensemble_avd_dat1", Sys.Date(), id, sep="_")

ensemble <- h2o.stackedEnsemble(x = X,

                                y = y,

                                training_frame = train,

                                validation_frame = valid,

                                model_id = ensemble_grid_id, 

                                base_models = list(gbm_model@model_id, er_rf@model_id))   #glm_model@model_id,





# Eval ensemble performance on a test set

perf <- h2o.performance(ensemble, newdata = test)

#perf.hd <- h2o.performance(ensemble, newdata = valid_sp[[1]])

#2o.download_pojo(ensemble,dir_nm)





precision <- h2o.precision((h2o.performance(ensemble, newdata = test)))

cut_off <- precision %>% filter(precision>=0.45) %>% summarise(thres=min(threshold))



write(cut_off$thres, file = "incidental_uer_model_cut_off.txt", append = FALSE, sep = " ")



### Save h2o into hdfs

ensemble_model_path <- h2o.saveModel(ensemble,path = model_path)

write(ensemble_model_path, file = "repeat_uer_model_path_ensemble.txt", append = FALSE, sep = " ")



### Get the latest model being trained

saved_model <- h2o.loadModel(ensemble_model_path)



# Generate predictions on entire training set (if neccessary)

train_pred <- h2o.predict(saved_model, newdata = model_set) 

train_id <- h2o.cbind(model_set$indiv_id, model_set$yr, model_set$mo, model_set$pst_has_avd, model_set$pst_3m_avd_er)

train_out <-h2o.cbind(train_pred, train_id) %>% as.data.frame()

train_out$predict_date <- Sys.Date()

t1 <- train_out %>% filter(p1>=cut_off$thres) %>% mutate(p_group='above')

t2 <- train_out %>% filter(p1<cut_off$thres) %>% mutate(p_group='below')

train_out<-rbind(t1,t2)



# identify of target members

train_out<-train_out %>% mutate(qtile_group=ifelse(train_out$p1>=quantile(train_out$p1, prob=0.98), 'top_2pct',

                                                   ifelse(train_out$p1>=quantile(train_out$p1, prob=0.85), 'top15pct',

                                                          ifelse(train_out$p1>=quantile(train_out$p1, prob=0.4), 'top60pct','other')))) 



# according to two above criteria to asssign risk group to target population

train_out<-train_out %>% mutate(risk_group=ifelse((p_group=='above'& qtile_group=='top_2pct'),'high_risk',

                                                  ifelse((p_group=='above' & qtile_group=='top15pct'),'medium_risk', 

                                                         ifelse(p_group == 'above'| qtile_group=='top60pct', 'low_risk', 'not considered'))))

# action values/incentives group: only give incentives to high/medium risk population

train_out<-train_out %>% mutate(target_group=ifelse(risk_group=='not considered','no','yes')) %>%

  select(indiv_id, predict, p1, risk_group, target_group, pst_3m_avd_er, pst_has_avd, yr, mo, predict_date)







ensemble_avd_dat1_train_out <- train_out

ensemble_avd_dat1_train_out$pst_has_avd <- as.numeric(ensemble_avd_dat1_train_out$pst_has_avd)

ensemble_avd_dat1_train_out <- ensemble_avd_dat1_train_out %>% mutate(adpredict=ifelse(target_group=='yes',1,0))

confusion_matrix.t<-table(ensemble_avd_dat1_train_out$pst_has_avd, ensemble_avd_dat1_train_out$adpredict) 

write.csv(ensemble_avd_dat1_train_out,"ensemble_avd_dat1_train_out.csv")

write.hive(ensemble_avd_dat1_train_out, "ensemble_avd_dat1_train_out", "dev_er_enc")







ensemble_avd_dat1_perfmence_tab<- ensemble_avd_dat1_train_out  %>% group_by(mo,risk_group) %>% summarise(pm1=min(p1),

                                                                                                         mem_cnt=n(),

                                                                                                         pst_3m_uer_pm=sum(pst_3m_avd_er)/mem_cnt) %>%

  ungroup() %>%

  group_by(risk_group) %>%

  summarise(min_p1=mean(pm1),

            mem_cnt=mean(mem_cnt),

            pst_3m_uer_pm=mean(pst_3m_uer_pm))



write.csv(ensemble_avd_dat1_perfmence_tab,"ensemble_avd_dat1_perfmence_tab.csv")

#write.hive(ensemble_avd_dat1_perfmence_tab, "ensemble_avd_dat1_perfmence_tab", "dev_er_enc")





# scoring

des <-read.hive('describe dev_er_enc.er_model_uer_dat1_score_v7')$col_name



# Parameter Setting'

variable_remove =c('pst_3m_avd_er','pst_3m_allowed_amt_avoid','pst_3m_pvt_er','pst_3m_allowed_amt_prevent','pst_3m_injury_er','pst_3m_allowed_amt_injury',

                   'pst_3m_er','pst_3m_allowed_amt_all_er','pst_has_pvt','pst_has_injury', 'rk', 'has_avoidable_6mo', 'zip_cd',

                   'rk_aer', 'rk_per', 'zip4')



###load score set

score_table <- "er_model_uer_dat1_score_v7" 

hdfsDatabase <- paste0("hdfs://prodmid:8020/dev/derived/consumer_analytics/er/",score_table)

model_set_hd<- h2o.importFile(path=hdfsDatabase, destination_frame="model_set_hd", col.names=des)





# change data types

model_set_hd$pst_has_avd <- as.factor(model_set_hd$pst_has_avd)

model_set_hd$local_mkt_cd <- as.factor(model_set_hd$local_mkt_cd)

model_set_hd$mbr_rtp_type_cd <- as.factor(model_set_hd$mbr_rtp_type_cd)

model_set_hd$infant_ind <- as.factor(model_set_hd$infant_ind)

model_set_hd$gender_cd <- as.factor(model_set_hd$gender_cd)

model_set_hd$business_ln_cd <- as.factor(model_set_hd$business_ln_cd)

model_set_hd$fund_ctg_cd<- as.factor(model_set_hd$fund_ctg_cd)

model_set_hd$has_pcp<- as.factor(model_set_hd$has_pcp)

model_set_hd$nn_uc_miles<- as.numeric(model_set_hd$nn_uc_miles)

model_set_hd$nn_er_miles<- as.numeric(model_set_hd$nn_er_miles)

model_set_hd$diff_dist<- as.numeric(model_set_hd$diff_dist)

model_set_hd$sd_avd_days<- as.numeric(model_set_hd$sd_avd_days)

model_set_hd$base_avd_days<- as.factor(model_set_hd$base_avd_days)

model_set_hd$avg_avd_days<- as.factor(model_set_hd$avg_avd_days)

model_set_hd$sd_pvt_days<- as.numeric(model_set_hd$sd_pvt_days)

model_set_hd$base_pvt_days<- as.factor(model_set_hd$base_pvt_days)

model_set_hd$avg_pvt_days<- as.factor(model_set_hd$avg_pvt_days)

model_set_hd$avg_days<- as.factor(model_set_hd$avg_days)

model_set_hd$avd_er_pct<- as.numeric(model_set_hd$avd_er_pct)

model_set_hd$prspctv_rsk_nbr <- as.numeric(model_set_hd$prspctv_rsk_nbr)

model_set_hd$mnths_since_er<- as.factor(model_set_hd$mnths_since_er)

model_set_hd$mnths_since_avd_er<- as.factor(model_set_hd$mnths_since_avd_er)

model_set_hd$days_since_er<- as.factor(model_set_hd$days_since_er)

model_set_hd$days_since_avd_er<- as.factor(model_set_hd$days_since_avd_er)

model_set_hd$days_since_injury_er<- as.factor(model_set_hd$days_since_injury_er)

model_set_hd$pcp_hhi_1yr <- as.numeric(model_set_hd$pcp_hhi_1yr)

model_set_hd$avg_days<- as.factor(model_set_hd$avg_days)

model_set_hd$sd_days<- as.numeric(model_set_hd$sd_days)

model_set_hd$family_type <- as.factor(model_set_hd$family_type)

model_set_hd$null_uc <- as.factor(model_set_hd$null_uc)

model_set_hd$null_er <- as.factor(model_set_hd$null_er)

model_set_hd$has_claim_12mo <- as.factor(model_set_hd$has_claim_12mo)

model_set_hd$plan_sponsor <- as.factor(model_set_hd$plan_sponsor)

model_set_hd$zip2 <- as.factor(model_set_hd$zip2)

model_set_hd$urbsubr <- as.factor(model_set_hd$urbsubr)





# impute missing values

#h2o.getId(model_set_hd)

h2o.impute(model_set_hd, "prspctv_rsk_nbr", "median")

h2o.impute(model_set_hd, "ylm_median_income", "median")

h2o.impute(model_set_hd, "nn_uc_miles", "median")

h2o.impute(model_set_hd, "nn_er_miles", "median")

h2o.impute(model_set_hd, "diff_dist", "median")

h2o.impute(model_set_hd, "avg_dist", "median")

h2o.impute(model_set_hd, "pcp_hhi_1yr", "median")





# separate x and y

y <- 'pst_has_avd'

X <- names(model_set)

X <- X[(!X %in% variable_remove) &(!grepl('_id',X)) &(!grepl('_record_type',X)) &(!(X==y)) ]



### Get the latest model being trained

saved_model <- h2o.loadModel(ensemble_model_path)



# Generate predictions on entire scoring set (if neccessary)

score_pred <- h2o.predict(saved_model, newdata = model_set_hd) 

score_id <- h2o.cbind(model_set_hd$indiv_id, model_set_hd$mo, model_set_hd$pst_has_avd, model_set_hd$pst_3m_avd_er)

score_out <-h2o.cbind(score_pred, score_id) %>% as.data.frame()

score_out$predict_date <- Sys.Date()

t1 <- score_out %>% filter(p1>=cut_off$thres) %>% mutate(p_group='above')

t2 <- score_out %>% filter(p1<cut_off$thres) %>% mutate(p_group='below')

score_out<-rbind(t1,t2)





# identify of target members

score_out<-score_out %>% mutate(qtile_group=ifelse(score_out$p1>=quantile(score_out$p1, prob=0.98), 'top_2pct',

                                                   ifelse(score_out$p1>=quantile(score_out$p1, prob=0.85), 'top15pct',

                                                          ifelse(score_out$p1>=quantile(score_out$p1, prob=0.4), 'top60pct','other')))) 



# according to two above criteria to asssign risk group to target population

score_out<-score_out %>% mutate(risk_group=ifelse((p_group=='above'& qtile_group=='top_2pct'),'high_risk',

                                                  ifelse((p_group=='above' & qtile_group=='top15pct'),'medium_risk', 

                                                         ifelse(p_group == 'above'| qtile_group=='top60pct', 'low_risk', 'not considered'))))

#                                                                                                  

# action values/incentives group: only give incentives to high/medium risk population

score_out<-score_out %>% mutate(target_group=ifelse(risk_group=='not considered','no','yes')) %>%

  select(indiv_id, predict, p1, risk_group, target_group, pst_3m_avd_er, pst_has_avd, mo, predict_date)





ensemble_avd_dat1_score_out <- score_out

ensemble_avd_dat1_score_out <- ensemble_avd_dat1_score_out %>% mutate(adpredict=ifelse(target_group=='yes',1,0))





write.csv(ensemble_avd_dat1_score_out ,"ensemble_repeat_uer_score_out.csv")

write.hive(ensemble_avd_dat1_score_out, "ensemble_repeat_uer_score_out", "dev_er_enc")


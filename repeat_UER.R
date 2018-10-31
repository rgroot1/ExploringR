
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


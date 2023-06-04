#인당등록대수로 변환한 애들로 해보기
#인당 보유대수,인당 주행거리 말고 rgldvall로 회귀
#인당 변환 안하고, 그냥 로그로 변환
#로그 안취한 값들로 해보기
#1.attached_parareg----
#rgldvall

#treat~ lpop+oil_realprice+lCityDensity+lGRDP+lGRDPsq+popgrbc
w_reg_pc <- weightit(treat ~ lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc,data = attached_parareg, method = "ps", estimand = "ATE")
summary(w_reg_pc)
table_weightreg_nolog<- bal.tab(w_reg_pc, stats = c("m"), thresholds = c(m = 0.1))
table_weightreg_nolog
#create weighted dataset
attached_parareg_weighted.pc <- attached_parareg
attached_parareg_weighted.pc$w_reg_pc <- w_reg_pc$weights

mytable(treat ~ lpop+oil_realprice+lCityDensity+lGRDP+lGRDPsq+popgrbc,data=attached_parareg)

#####2.3.1.1. ATE방법
model_reg<-glm(treat ~lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, family="binomial",data=attached_parareg)
attached_parareg_weighted.pc$ps<-model_reg$fitted.values
#show row if w_reg_pc>=50
attached_parareg_weighted.pc<-subset(attached_parareg_weighted.pc, !(w_reg_pc >= 50))
before_ate<-ggplot(data=attached_parareg_weighted.pc, aes(x=ps))+geom_density(aes(fill=factor(treat)),alpha=0.3)+ggtitle("가중치 부여 전") +My_Theme2
#control 그룹(treat=0)은 propensity score 값이 작은 쪽에 쏠려 있고, treat 그룹(treat=1)은 ps값이 큰 쪽에 쏠려 있다.
#가중치 부여후 propensity score의 density plot
after_ate<-ggplot(data=attached_parareg_weighted.pc, aes(x=ps,weight=w_reg_pc))+geom_density(aes(fill=factor(treat)),alpha=0.3)+ggtitle("가중치 부여 후") +My_Theme2
#grid.arrange(before_ate,after_ate, ncol=2)


#천인당 등록대수 로 변환
attached_parareg_weighted.pc$rgldvall.thousandpc<-attached_parareg_weighted.pc$rgldvall.pc*1000
attached_parareg_weighted.pc$rgldvprivate.thousandpc<-attached_parareg_weighted.pc$rgldvprivate.pc*1000


options("scipen"=100, "digits"=2)
design.ps_pcreg <- svydesign(ids=~0, data=attached_parareg_weighted.pc, weights=~w_reg_pc)

#attached_parareg_weighted.pc의 corr table
new_df <- as.data.frame(cbind(names(attached_parareg_weighted.pc), 1:ncol(attached_parareg_weighted.pc)))
names(new_df) <- c("Column_Name", "Column_Number")

paracorrplot<-attached_parareg_weighted.pc[,c(14,17,19,57,29,63,42,6,15,21,23,72,73)]
#paracorrplot1<-paracorrplot[c(460:1836),]
paracorrplot_raw<-attached_parareg_weighted.pc[,c(14,17,19,5,29,2,37,6,15,21,23,7,8)]
  

#paracorrplot의 Na, NaN, Inf value 확인
paracorrplot_naconfirm<-paracorrplot_raw[!is.finite(rowSums(paracorrplot_raw)),]
nrow(paracorrplot_naconfirm)

corr <- round(cor(paracorrplot), 2)
p.mat <- cor_pmat(as.matrix(paracorrplot))
corrplot_transpformed<-ggcorrplot(cor(paracorrplot), hc.order = FALSE, type = "lower",
                     lab = TRUE,p.mat=p.mat,insig=c("pch"),pch.cex=5)
pvplot_transformed<-ggcorrplot(cor(p.mat), hc.order = FALSE, type = "lower",
                   lab = TRUE)
cor_transformed<-grid.arrange(corrplot_transpformed,pvplot_transformed, ncol=1,nrow=2)

ggsave("cor_transformed.png",cor_transformed,width=30,height=50,units=c("cm"))



corr_raw <- round(cor(paracorrplot_raw), 2)
p.mat_raw <- cor_pmat(as.matrix(paracorrplot_raw))
corrplot_raw<-ggcorrplot(cor(paracorrplot_raw), hc.order = FALSE, type = "lower",
                                  lab = TRUE,p.mat=p.mat_raw,insig=c("pch"),pch.cex=5)
pvplot_raw<-ggcorrplot(cor(p.mat_raw), hc.order = FALSE, type = "lower",
                               lab = TRUE)
cor_raw<-grid.arrange(corrplot_raw,pvplot_raw, ncol=1,nrow=2)

ggsave("cor_raw.png",cor_raw,width=30,height=50,units=c("cm"))



##rgldvall.thousandpc##----

rgldvall.thousandpc.bin <- svyglm(rgldvall.thousandpc~treat+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_pcreg)
summary(rgldvall.thousandpc.bin)
rgldvall.thousandpc.int.CityScale <- svyglm(rgldvall.thousandpc~treat*urban+treat*rural+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_pcreg)
summary(rgldvall.thousandpc.int.CityScale)#treat*urban지역에서 자가용인당등록대수(-)발생/treat*rural지역에서 자가용인당등록대수 (+)발생
rgldvall.thousandpc.int.availability <- svyglm(rgldvall.thousandpc~treat*good+treat*bad+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_pcreg)
summary(rgldvall.thousandpc.int.availability)#모두 유의하지 않음

summary(rgldvprivate.thousandpc.int.CityScale)#treat*urban지역에서 자가용인당등록대수(-)발생/treat*rural지역에서 자가용인당등록대수 (+)발생



##rgldvprivate.thousandpc##----
rgldvprivate.thousandpc.bin <- svyglm(rgldvprivate.thousandpc~treat+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_pcreg)
summary(rgldvprivate.thousandpc.bin)
rgldvprivate.thousandpc.int.CityScale <- svyglm(rgldvprivate.thousandpc~treat*urban+treat*rural+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_pcreg)
summary(rgldvprivate.thousandpc.int.CityScale)#treat*urban에서 자가용인당등록대수 (-)발생
rgldvprivate.thousandpc.int.availability <- svyglm(rgldvprivate.thousandpc~treat*good+treat*bad+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_pcreg)
summary(rgldvprivate.thousandpc.int.availability)#treat*good지역에서 자가용인당등록대수(-)발생


##rgldvall.pc##----
rgldvall.pc.bin <- svyglm(rgldvall.pc~treat+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_pcreg)
summary(rgldvall.pc.bin)
rgldvall.pc.int.CityScale <- svyglm(rgldvall.pc~treat*urban+treat*rural+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_pcreg)
summary(rgldvall.pc.int.CityScale)#treat*urban지역에서 자가용인당등록대수(-)발생
rgldvall.pc.int.availability <- svyglm(rgldvall.pc~treat*good+treat*bad+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_pcreg)
summary(rgldvall.pc.int.availability)#모두 유의하지 않음

##rgldvprivate.pc##----
rgldvprivate.pc.bin <- svyglm(rgldvprivate.pc~treat+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_pcreg)
summary(rgldvprivate.pc.bin)
rgldvall.pc.int.CityScale <- svyglm(rgldvprivate.pc~treat*urban+treat*rural+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_pcreg)
summary(rgldvall.pc.int.CityScale)#treat*urban에서 자가용인당등록대수 (-)발생
rgldvprivate.pc.int.availability <- svyglm(rgldvprivate.pc~treat*good+treat*bad+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_pcreg)
summary(rgldvprivate.pc.int.availability)#treat*good지역에서 자가용인당등록대수(-)발생

#2.attached_paravkm----

####대당주행거리----
w_vkm_veh <- weightit(treat ~ lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc,data = attached_paravkm, method = "ps", estimand = "ATE")
summary(w_vkm_veh)
table_weightveh_nolog<- bal.tab(w_vkm_veh, stats = c("m"), thresholds = c(m = 0.1))
table_weightveh_nolog

#create weighted dataset
attached_paravkm_weighted.veh <- attached_paravkm
attached_paravkm_weighted.veh$w_vkm_veh <- w_vkm_veh$weights

mytable(treat ~ lpop+oil_realprice+lCityDensity+lGRDP+lGRDPsq+popgrbc,data=attached_paravkm)

#####2.3.1.1. ATE방법
model_vkmveh<-glm(treat ~lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, family="binomial",data=attached_paravkm)
attached_paravkm_weighted.veh$ps<-model_vkmveh$fitted.values
#show row if w_vkm_veh>=50
attached_paravkm_weighted.veh<-subset(attached_paravkm_weighted.veh, !(w_vkm_veh >= 50))
before_ate<-ggplot(data=attached_paravkm_weighted.veh, aes(x=ps))+geom_density(aes(fill=factor(treat)),alpha=0.3)+ggtitle("가중치 부여 전") +My_Theme2
#control 그룹(treat=0)은 propensity score 값이 작은 쪽에 쏠려 있고, treat 그룹(treat=1)은 ps값이 큰 쪽에 쏠려 있다.
#가중치 부여후 propensity score의 density plot
after_ate<-ggplot(data=attached_paravkm_weighted.veh, aes(x=ps,weight=w_vkm_veh))+geom_density(aes(fill=factor(treat)),alpha=0.3)+ggtitle("가중치 부여 후") +My_Theme2
grid.arrange(before_ate,after_ate, ncol=2)
#ggsave("bfate.png", grid.arrange(before_ate,after_ate, ncol=2))

options("scipen"=100, "digits"=2)
design.ps_vehvkm <- svydesign(ids=~0, data=attached_paravkm_weighted.veh, weights=~w_vkm_veh)

##vkm_ldv_all.veh##----
vkm_ldv_all.veh.bin <- svyglm(vkm_ldv_all.veh~treat+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_vehvkm)
summary(vkm_ldv_all.veh.bin)
vkm_ldv_all.veh.int.CityScale <- svyglm(vkm_ldv_all.veh~treat*urban+treat*rural+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_vehvkm)
summary(vkm_ldv_all.veh.int.CityScale)#treat*urban에서 전체차량 대당주행거리 +
vkm_ldv_all.veh.int.availability <- svyglm(vkm_ldv_all.veh~treat*good+treat*bad+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_vehvkm)
summary(vkm_ldv_all.veh.int.availability)#treat*bad에서 전체차량 대당주행거리+

##vkm_ldv_private.veh##----
vkm_ldv_private.veh.bin <- svyglm(vkm_ldv_private.veh~treat+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_vehvkm)
summary(vkm_ldv_private.veh.bin)
vkm_ldv_all.veh.int.CityScale <- svyglm(vkm_ldv_private.veh~treat*urban+treat*rural+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_vehvkm)
summary(vkm_ldv_all.veh.int.CityScale)#treat*urban에서 자가용 대당주행거리 +
vkm_ldv_private.veh.int.availability <- svyglm(vkm_ldv_private.veh~treat*good+treat*bad+lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc, design = design.ps_vehvkm)
summary(vkm_ldv_private.veh.int.availability)#treat*bad에서 자가용 대당주행거리+


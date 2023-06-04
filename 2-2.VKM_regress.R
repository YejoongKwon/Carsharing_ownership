packages <- c("cobalt","WeightIt","lme4","tidyverse","psych","stargazer","dplyr","ggplot2", "readxl", "caret", "tidyr", "stringr", "zoo", "purrr", "reshape2", "openxlsx", "ggthemes", "ggrepel","ggcorrplot","gridExtra","MatchIt","moonBook","survey","tableone")
invisible(lapply(packages, library, character.only = TRUE))

attached_paravkm<-read_excel("주행거리_ver3.xlsx")


#(citypoppct)Citypop을도시거주인구 비중으로 변환
attached_paravkm$citypoppct<-100*attached_paravkm$CityPop/attached_paravkm$Pop
attached_paravkm$loilprice<-log(attached_paravkm$oil_realprice)

#종속변수 unit를 천 km에서 '10만(hundred thousand)km' 로 변환
attached_paravkm$vkm_ldv_private.ht<-attached_paravkm$vkm_ldv_private/100
attached_paravkm$vkm_ldv_all.ht<-attached_paravkm$vkm_ldv_all/100
#(popgrbcperthous)popgrbc를 천인당 304050인구수로변환
attached_paravkm$popgrbcperthous<-(attached_paravkm$popgrbc)*(attached_paravkm$Pop)*1000
#(popgrbcpct)popgrbcpc를 % 변수로 변환
attached_paravkm$popgrbcpct<-(attached_paravkm$popgrbc)*100



form7<-treat~ lpop+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct#(현재까지 가장 그럴듯한 결과)
form8<- treat~lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc+citypopratio+Unempperthous
form9<-treat~ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct#(form7에서 lpop 없앰)
form10<-treat~Pop+oil_realprice+lCityDensity+GRDP.pc+popgrbc+citypopratio+marriagerate
form11<-treat~ oil_realprice+lCityDensity+GRDP.pc+popgrbc+citypopratio+marriagerate
form13<-treat~Pop+ oil_realprice+CityDensity+GRDP.pc+popgrbcpct+citypoppct+Unempperthous#(form9에서 Unempperthousand 삽입)
#oil_realprice를 log 취해줌

#attached_paravkm <- attached_paravkm[!(attached_paravkm$year_ %in% c("2009", "2010","2011")),]

#original: form7
w_vkm_pc <- weightit(form9,data = attached_paravkm, method = "ps", estimand = "ATE")
summary(w_vkm_pc)
table_weighvkm_nolog<- bal.tab(w_vkm_pc, stats = c("m"), thresholds = c(m = 0.2))
table_weighvkm_nolog

vkm_Balance<-table_weighvkm_nolog$Balance
vkm_Balance <- tibble::rownames_to_column(vkm_Balance, "index")

#Weightbalance<-as.data.frame(rbind(reg_Balance,vkm_Balance))
#write.xlsx(Weightbalance,"Weightbalance.xlsx")

#ATE 방법으로 진행시킴

#create weighted dataset
attached_paravkm1 <- attached_paravkm
attached_paravkm1$w_vkm_pc <- w_vkm_pc$weights

#options("scipen"=100, "digits"=2)
design.ps_pcvkm <- svydesign(ids=~0, data=attached_paravkm1, weights=~w_vkm_pc)

'
model_vkm<-glm(form7, family="binomial",data=attached_paravkm1)
attached_paravkm1$ps<-model_vkm$fitted.values
#show row if w_reg_pc>=50
attached_paravkm2<-subset(attached_paravkm1, !(w_vkm_pc >= 50))

before_ate<-ggplot(data=attached_paravkm2, aes(x=ps))+geom_density(aes(fill=factor(treat)),alpha=0.3)+ggtitle("가중치 부여 전")+My_Theme2+theme(plot.title = element_text(size = 25))+ylim(0,4)
after_ate<-ggplot(data=attached_paravkm2, aes(x=ps,weight=w_vkm_pc))+geom_density(aes(fill=factor(treat)),alpha=0.3)+ggtitle("가중치 부여 후")+My_Theme2+theme(plot.title = element_text(size = 25))+ylim(0,4)


grid.arrange(before_ate,after_ate, ncol=2)
ggsave("bfate_vkm.png", grid.arrange(before_ate,after_ate, ncol=2))
'
###2-1.전체차량 주행거리
vkm_ldv_all.pc.bint <- svyglm(vkm_ldv_all.ht~treat+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct+factor(t)
                          , design = design.ps_pcvkm)
vkm_ldv_all.pc.bin <- svyglm(vkm_ldv_all.ht~treat+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct,design = design.ps_pcvkm)
summary(vkm_ldv_all.pc.bin)
format(summary(vkm_ldv_private.pc.bin)$coefficients[,1],digits=4)
format(summary(vkm_ldv_private.pc.bin)$coefficients[,2],digits=3)
summary(vkm_ldv_all.pc.bin)
rsq(vkm_ldv_private.pc.bin)
#전체차량 대당주행거리는 treat로 인해 유의하게 줄어들었음.
vkm_ldv_all.pc.int.CityScalet<- svyglm(vkm_ldv_all.ht~treat*urban+treat*rural+ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct+factor(t)
                                    , design = design.ps_pcvkm)
vkm_ldv_all.pc.int.CityScale <- svyglm(vkm_ldv_all.ht~treat*urban+treat*rural+ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct,design = design.ps_pcvkm)
summary(vkm_ldv_all.pc.int.CityScale)
format(summary(vkm_ldv_private.pc.int.CityScale)$coefficients[,1],digits=6)
format(summary(vkm_ldv_private.pc.int.CityScale)$coefficients[,2],digits=5)


rsq(vkm_ldv_private.pc.int.CityScale)


rsq(vkm_ldv_all.pc.int.CityScale)
#전체차량 대당주행거리가 treat로 인해 유의하게 줄어들었음.(앞선결과와 일치함)
#그러나 시군구의 도시화 정도에 따른 카셰어링 개시의 전체차량 대당주행거리에 대한 영향은 유의하지 않았음.

#전체차량 대당주행거리가 treat로 인해 유의하게 줄어들었음.(앞선결과와 일치)
#차고지 접근가능성에 따른 카셰어링 개시의 전체차량 대당주행거리에 대한 영향은 유의하지 않았음.


###2-2.자가용 주행거리

vkm_ldv_private.pc.bin <- svyglm(vkm_ldv_private.ht~treat+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct
                             , design = design.ps_pcvkm)
summary(vkm_ldv_private.pc.bin)
rsq(vkm_ldv_private.pc.bin)

#전체차량 대당주행거리는 treat로 인해 유의하게 줄어들었음.

vkm_ldv_private.pc.int.CityScale <- svyglm(vkm_ldv_private.ht~treat*urban+treat*rural+ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct
                                       , design = design.ps_pcvkm)
summary(vkm_ldv_private.pc.int.CityScale)
rsq(vkm_ldv_private.pc.int.CityScale)
#전체차량 대당주행거리가 treat로 인해 유의하게 줄어들었음.(앞선결과와 일치함)
#그러나 시군구의 도시화 정도에 따른 카셰어링 개시의 전체차량 대당주행거리에 대한 영향은 유의하지 않았음.
#vkm_ldv_all.pc.int.availabilityt <- svyglm(vkm_ldv_private.ht~treat*good+treat*bad+ Pop+ oil_realprice+CityDensity+GRDP.pc+popgrbcpct+citypoppct+Unempperthous+factor(t)
#                                           , design = design.ps_pcvkm)


#전체차량 대당주행거리가 treat로 인해 유의하게 줄어들었음.(앞선결과와 일치)
#차고지 접근가능성에 따른 카셰어링 개시의 전체차량 대당주행거리에 대한 영향은 유의하지 않았음.
vkmmodel.1<-list(vkm_ldv_all.pc.bin,vkm_ldv_private.pc.bin)
summary(vkm_ldv_all.pc.bin)
vkmmodel.2<-list(vkm_ldv_all.pc.int.CityScale,vkm_ldv_private.pc.int.CityScale)

stargazer(vkmmodel.1,title="IPTW 가중치를 적용한 일반회귀모형", type="html", out = "주행거리일반회귀.html",
          dep.var.labels = c("전체","자가용"),
          #covariate.labels=c("카셰어링진입", "휘발유가격","log(인구밀도)","인당GRDP","304050대 인구비중",'도시지역인구비중','천인당실업자수'),
          no.space=T)
stargazer(vkmmodel.2,title="IPTW 가중치를 적용한 DiD회귀모형(카셰어링진입*도시화정도)", type="html", out = "주행거리DiD(도시화).html",
          dep.var.labels = c("전체","자가용"),
          #covariate.labels=c("카셰어링진입","도시","시골","휘발유가격","log(인구밀도)","인당GRDP","304050대 인구비중",'도시지역인구비중','천인당실업자수'),
          no.space=T)


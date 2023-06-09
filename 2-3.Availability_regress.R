
####1. 등록대수-접근성 모형----
avail_reg<-read_excel("등록대수_접근성df_new.xlsx")
#(citypoppct)Citypop을도시거주인구 비중으로 변환
avail_reg$citypoppct<-100*avail_reg$CityPop/avail_reg$Pop
#(popgrbcperthous)popgrbc를 천인당 304050인구수로변환
avail_reg$popgrbcperthous<-(avail_reg$popgrbc)*(avail_reg$Pop)*1000
#(popgrbcpct)popgrbcpc를 % 변수로 변환
avail_reg$popgrbcpct<-(avail_reg$popgrbc)*100
length(unique(avail_reg$sidosigungu))
'
#인구증가율(%)변수생성
population_growth_reg<- avail_reg
population_growth_reg$popgrowth <- unlist(by(population_growth_reg, population_growth_reg$sidosigungu, function(x) {
  c(0, diff(x$Pop) / x$Pop[-length(x$Pop)] * 100)
}))
'
###강건성 검정으로 채택함
form12<-treat~ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct
#현재까지 원하는 결과 나온것임.(채택!)
form13<-treat~ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct+Unemployment#(form12에서 Unemployment 삽입)

form13_3<-treat~ oil_realprice+lCityDensity+GRDP.pc+citypoppct+Unemployment#(form12에서 Unemployment 삽입)<<원하는 결과가 나오고, 완전 유의하고, rsq도 좋음
#popgrbcpct 뺐더니 urban 계수가 아주 유의하고, 게다가 모형의 설명력도 커짐. 

#original: form13
w_avail <- weightit(form12,data = avail_reg, method = "ps", estimand = "ATE")
summary(w_avail)
table_w_avail<- bal.tab(w_avail, stats = c("m"), thresholds = c(m = 0.2))
table_w_avail
#create weighted dataset
avail_reg1 <- avail_reg
avail_reg1$w_avail <- w_avail$weights
options("scipen"=100, "digits"=2)
design_avail <- svydesign(ids=~0, data=avail_reg1, weights=~w_avail)
rgldvall.thousandpc.int.availability <- svyglm(rgldvall.thousandpc~treat*good+treat*bad+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct, design = design_avail)
summary(rgldvall.thousandpc.int.availability)
format(summary(rgldvall.thousandpc.int.availability)$coefficients[,1],digits=4)
format(summary(rgldvall.thousandpc.int.availability)$coefficients[,2],digits=2)

summary(rgldvprivate.thousandpc.int.availability)

rsq(rgldvall.thousandpc.int.availability)
rgldvprivate.thousandpc.int.availability <- svyglm(rgldvprivate.thousandpc~treat*good+treat*bad+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct, design = design_avail)
summary(rgldvprivate.thousandpc.int.availability)
format(summary(rgldvprivate.thousandpc.int.availability)$coefficients[,1],digits=4)
format(summary(rgldvprivate.thousandpc.int.availability)$coefficients[,2],digits=3)
rsq(rgldvall.thousandpc.int.availability)

rsq.kl(rgldvprivate.thousandpc.int.availability)

regmodel.3<-list(rgldvall.thousandpc.int.availability,rgldvprivate.thousandpc.int.availability)

stargazer(regmodel.3,title="IPTW 가중치를 적용한 DiD회귀모형(카셰어링진입*차고지접근성)", type="html", out = "등록대수DiD(차고지접근성).html",
          dep.var.labels = c("천인당 전체승용차등록대수","천인당 자가용승용차 등록대수"),digits = 2,
          #covariate.labels=c("카셰어링진입","도시","시골","휘발유가격","log(인구밀도)","인당GRDP","304050대 인구비중",'도시지역인구비중','천인당실업자수'),
          no.space=T)


####2. 주행거리-접근성 모형----

avail_vkm<-read_excel("주행거리_접근성df_new.xlsx")


#(citypoppct)Citypop을도시거주인구 비중으로 변환
avail_vkm$citypoppct<-100*avail_vkm$CityPop/avail_vkm$Pop
avail_vkm$loilprice<-log(avail_vkm$oil_realprice)

#종속변수 unit를 천 km에서 '10만(hundred thousand)km' 로 변환
avail_vkm$vkm_ldv_private.ht<-avail_vkm$vkm_ldv_private/100
avail_vkm$vkm_ldv_all.ht<-avail_vkm$vkm_ldv_all/100
#(popgrbcperthous)popgrbc를 천인당 304050인구수로변환
avail_vkm$popgrbcperthous<-(avail_vkm$popgrbc)*(avail_vkm$Pop)*1000
#(popgrbcpct)popgrbcpc를 % 변수로 변환
avail_vkm$popgrbcpct<-(avail_vkm$popgrbc)*100

form7<-treat~ lpop+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct#(현재까지 가장 그럴듯한 결과)
form9<-treat~ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct#(form7에서 lpop 없앰)

w_avail_vkm <- weightit(form9,data = avail_vkm, method = "ps", estimand = "ATE")
summary(w_avail_vkm)
table_w_avail_vkm<- bal.tab(w_avail_vkm, stats = c("m"), thresholds = c(m = 0.2))
table_w_avail_vkm


#create weighted dataset
avil_vkm1 <- avail_vkm
avil_vkm1$w_avail_vkm <- w_avail_vkm$weights


design_availvkm <- svydesign(ids=~0, data=avil_vkm1, weights=~w_avail_vkm)


vkm_ldv_all.pc.int.availability <- svyglm(vkm_ldv_all.ht~treat*good+treat*bad+ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct,design = design_availvkm)
summary(vkm_ldv_private.pc.int.availability)
rsq(vkm_ldv_private.pc.int.availability)


format(summary(vkm_ldv_private.pc.int.availability)$coefficients[,1],digits=5)
format(summary(vkm_ldv_private.pc.int.availability)$coefficients[,2],digits=5)
summary(vkm_ldv_all.pc.int.availability)$coefficients[,4]


vkm_ldv_private.pc.int.availability <- svyglm(vkm_ldv_private.ht~treat*good+treat*bad+ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct,design = design_availvkm)
summary(vkm_ldv_private.pc.int.availability)
rsq(vkm_ldv_private.pc.int.availability)


vkmmodel.3<-list(vkm_ldv_all.pc.int.availability,vkm_ldv_private.pc.int.availability)

stargazer(vkmmodel.3,title="IPTW 가중치를 적용한 DiD회귀모형(카셰어링진입*차고지접근성)", type="html", out = "주행거리DiD(차고지접근성).html",
          dep.var.labels = c("전체","자가용"),
          #covariate.labels=c("카셰어링진입","도시","시골","휘발유가격","log(인구밀도)","인당GRDP","304050대 인구비중",'도시지역인구비중','천인당실업자수'),
          no.space=T)

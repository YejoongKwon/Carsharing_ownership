packages <- c("broom","rsq","cobalt","WeightIt","lme4","tidyverse","psych","stargazer","dplyr","ggplot2", "readxl", "caret", "tidyr", "stringr", "zoo", "purrr", "reshape2", "openxlsx", "ggthemes", "ggrepel","ggcorrplot","gridExtra","MatchIt","moonBook","survey","tableone")
invisible(lapply(packages, library, character.only = TRUE))

attached_parareg<-read_excel("등록대수_ver3.xlsx")
options("scipen"=100, "digits"=2)


#(citypoppct)Citypop을도시거주인구 비중으로 변환
attached_parareg$citypoppct<-100*attached_parareg$CityPop/attached_parareg$Pop
#(popgrbcperthous)popgrbc를 천인당 304050인구수로변환
attached_parareg$popgrbcperthous<-(attached_parareg$popgrbc)*(attached_parareg$Pop)*1000
#(popgrbcpct)popgrbcpc를 % 변수로 변환
attached_parareg$popgrbcpct<-(attached_parareg$popgrbc)*100
length(unique(attached_parareg$sidosigungu))
#단위가 100만원이었던 GRDP라는 변수를 단위가 원인 grdp로 바꿈
#attached_parareg$grdp<-(attached_parareg$GRDP)*1000000
#단위가 원인 grdp를 Pop로 나눠서 grdp.pc로 만듦
#attached_parareg$grdp.pc<-(attached_parareg$grdp)/attached_parareg$Pop




#treat~ lpop+oil_realprice+lCityDensity+lGRDP+lGRDPsq+popgrbc+lcitypop+unemployment

###강건성 검정으로 채택함
form12<-treat~ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct

#form12와 같은결과를 나오게함
form13_2<-treat~ oil_realprice+lCityDensity+popgrbcpct+citypoppct+Unemployment
#GRDP.pc 뺐을 때 계수의 방향이 유의하지만, 모형의 설명력 줄어듦

form13_3<-treat~ oil_realprice+lCityDensity+GRDP.pc+citypoppct+Unemployment#(form12에서 Unemployment 삽입)
#popgrbcpct 뺐더니 urban 계수가 아주 유의하고, 게다가 모형의 설명력도 커짐. 
form13_4<-treat~ oil_realprice+lCityDensity+GRDP.pc+citypoppct*popgrbcpct+Unemployment#(form12에서 Unemployment 삽입)

#####현재까지 원하는 결과 나온것임.----
form13<-treat~ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct+Unemployment#(form12에서 Unemployment 삽입)



#1. 천인당등록대수----
#original : form13
w_reg_pc <- weightit(form13,data = attached_parareg, method = "ps", estimand = "ATE")
summary(w_reg_pc)
table_weightreg_nolog<- bal.tab(w_reg_pc, stats = c("m"), thresholds = c(m = 0.2))
table_weightreg_nolog
reg_Balance<-table_weightreg_nolog$Balance
reg_samplesize<-table_weightreg_nolog$Observations
reg_Balance <- tibble::rownames_to_column(reg_Balance, "index")


#create weighted dataset
attached_parareg1 <- attached_parareg
attached_parareg1$w_reg_pc <- w_reg_pc$weights
design.ps_pcreg <- svydesign(ids=~0, data=attached_parareg1, weights=~w_reg_pc)
#Pop+oil_realprice+CityDensity+GRDP.pc+popgrbc+citypopratio+Unemployment
###1-1. 전체차량천인당등록대수----
'
model_reg<-glm(form13, family="binomial",data=attached_parareg1)
attached_parareg1$ps<-model_reg$fitted.values
#show row if w_reg_pc>=50
attached_parareg2<-subset(attached_parareg1, !(w_reg_pc >= 50))

before_ate<-ggplot(data=attached_parareg2, aes(x=ps))+geom_density(aes(fill=factor(treat)),alpha=0.3)+ggtitle("가중치 부여 전")+My_Theme2+theme(plot.title = element_text(size = 25))+ylim(0,4)
after_ate<-ggplot(data=attached_parareg2, aes(x=ps,weight=w_reg_pc))+geom_density(aes(fill=factor(treat)),alpha=0.3)+ggtitle("가중치 부여 후")+My_Theme2+theme(plot.title = element_text(size = 25))+ylim(0,4)
grid.arrange(before_ate,after_ate, ncol=2)
ggsave("bfate_reg.png", grid.arrange(before_ate,after_ate, ncol=2))
'

rgldvall.thousandpc.bin <- svyglm(rgldvall.thousandpc~treat+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct+Unemployment, design = design.ps_pcreg)
summary(rgldvprivate.thousandpc.bin)
rsq(rgldvprivate.thousandpc.bin)

format(summary(rgldvprivate.thousandpc.bin)$coefficients[,1],digits=4)
format(summary(rgldvprivate.thousandpc.bin)$coefficients[,2],digits=2)

summary(rgldvprivate.thousandpc.bin)
rgldvall.thousandpc.bin_coef_vals <- as.numeric(format(summary(rgldvall.thousandpc.bin)$coefficients[,1],digits=4))
names(rgldvall.thousandpc.bin_coef_vals)<-c("Intercept","treat","oil_realprice","lCityDensity","GRDP.pc","popgrbcpct","citypoppct","Unemployment")
rgldvall.thousandpc.bin_se_vals <- as.numeric(format(summary(rgldvall.thousandpc.bin)$coefficients[,2],digits=4))
names(rgldvall.thousandpc.bin_se_vals)<-c("Intercept","treat","oil_realprice","lCityDensity","GRDP.pc","popgrbcpct","citypoppct","Unemployment")


#주유소휘발유가격이 차량등록대수에 가장 유의한 영향을 미침
#영향력의 크기는 4050대 인구비중이 가장 큼
rgldvall.thousandpc.int.CityScale <- svyglm(rgldvall.thousandpc~treat*urban+treat*rural+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct+Unemployment, design = design.ps_pcreg)
rsq(rgldvprivate.thousandpc.int.CityScale)
summary(rgldvall.thousandpc.int.CityScale)
format(summary(rgldvprivate.thousandpc.int.CityScale)$coefficients[,1],digits=4)
format(summary(rgldvprivate.thousandpc.int.CityScale)$coefficients[,2],digits=3)

summary(rgldvprivate.thousandpc.int.CityScale)
rsq(rgldvall.thousandpc.int.CityScale)
summary(rgldvall.thousandpc.int.CityScale)$coefficients[,1]
format(summary(rgldvprivate.thousandpc.int.CityScale)$coefficients[,1],digits=3)
format(summary(rgldvprivate.thousandpc.int.CityScale)$coefficients[,2],digits=3)
summary(rgldvprivate.thousandpc.int.CityScale)
rgldvall.thousandpc.int.CityScale_coef_vals <- as.numeric(format(summary(rgldvall.thousandpc.int.CityScale)$coefficients[,1],digits=4))
names(rgldvall.thousandpc.int.CityScale_coef_vals)<-c("(Intercept)","treat","urban","rural","oil_realprice","lCityDensity","GRDP.pc","popgrbcpct","citypoppct","Unemployment","treat:urban","treat:rural")

rgldvall.thousandpc.int.CityScale_se_vals <- as.numeric(format(summary(rgldvall.thousandpc.int.CityScale)$coefficients[,2],digits=4))
names(rgldvall.thousandpc.int.CityScale_se_vals)<-c("(Intercept)","treat","urban","rural","oil_realprice","lCityDensity","GRDP.pc","popgrbcpct","citypoppct","Unemployment","treat:urban","treat:rural")


###1-2. 자가용차량천인당등록대수----
rgldvprivate.thousandpc.bin <- svyglm(rgldvprivate.thousandpc~treat+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct+Unemployment, design = design.ps_pcreg)
summary(rgldvprivate.thousandpc.bin)
rsq(rgldvprivate.thousandpc.bin)

rgldvprivate.thousandpc.bin_coef_vals <- as.numeric(format(summary(rgldvprivate.thousandpc.bin)$coefficients[,1],digits=4))
names(rgldvprivate.thousandpc.bin_coef_vals)<-names(summary(rgldvprivate.thousandpc.bin)$coefficients[,1])

rgldvprivate.thousandpc.bin_se_vals <- as.numeric(format(summary(rgldvprivate.thousandpc.bin)$coefficients[,2],digits=4))
names(rgldvprivate.thousandpc.bin_se_vals)<-names(summary(rgldvprivate.thousandpc.bin)$coefficients[,2])
#대부분의 변수가 유의하게 나타남
rgldvprivate.thousandpc.int.CityScale <- svyglm(rgldvprivate.thousandpc~treat*urban+treat*rural+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct+Unemployment, design = design.ps_pcreg)
summary(rgldvprivate.thousandpc.int.CityScale)
#treat*urban도시화 지역이 높은곳에서 인구천인당자가용등록대수가 유의하게 감소함
rsq(rgldvprivate.thousandpc.int.CityScale)
rgldvprivate.thousandpc.int.CityScale_coef_vals <- as.numeric(format(summary(rgldvprivate.thousandpc.int.CityScale)$coefficients[,1],digits=4))

names(rgldvprivate.thousandpc.int.CityScale_coef_vals)<-names(summary(rgldvprivate.thousandpc.int.CityScale)$coefficients[,1])

rgldvprivate.thousandpc.int.CityScale_se_vals <- as.numeric(format(summary(rgldvprivate.thousandpc.int.CityScale)$coefficients[,2],digits=4))
names(rgldvprivate.thousandpc.int.CityScale_se_vals)<-names(summary(rgldvprivate.thousandpc.int.CityScale)$coefficients[,2])



#treat*bad 차고지 접근성이 나쁜 지역의 인구천인당자가용등록대수가 유의하게 늘어남.
regmodel.1<-list(rgldvall.thousandpc.bin,rgldvprivate.thousandpc.bin)
regmodel.2<-list(rgldvall.thousandpc.int.CityScale,rgldvprivate.thousandpc.int.CityScale)
#.bin 모형 계수리스트
coef_list.bin <- list(rgldvall.thousandpc.bin_coef_vals,
                      rgldvprivate.thousandpc.bin_coef_vals)
se_list.bin <- list(rgldvall.thousandpc.bin_se_vals, rgldvprivate.thousandpc.bin_se_vals)

#.Cityscale 모형 계수리스트
coef_list.Cityscale<-list(rgldvall.thousandpc.int.CityScale_coef_vals,
                          rgldvprivate.thousandpc.int.CityScale_coef_vals)
se_list.Cityscale<-list(rgldvall.thousandpc.int.CityScale_se_vals,
                        rgldvprivate.thousandpc.int.CityScale_se_vals)
stargazer(regmodel.1,title="IPTW 가중치를 적용한 일반회귀모형(전체차량)", type="html", out = "등록대수일반회귀(전체차량).html",
          dep.var.labels = c("천인당 전체승용차등록대수"),
          #covariate.labels=c("카셰어링진입", "휘발유가격","log(인구밀도)","인당GRDP","304050대 인구비중",'도시지역인구비중','실업률'),
          coefficients=rgldvall.thousandpc.bin_coef_vals, se=rgldvall.thousandpc.bin_se_vals)

----
stargazer(regmodel.1,title="IPTW 가중치를 적용한 일반회귀모형", type="html", out = "등록대수일반회귀.html",
          dep.var.labels = c("천인당 전체승용차등록대수","천인당 자가용승용차 등록대수"),
          #covariate.labels=c("카셰어링진입", "휘발유가격","log(인구밀도)","인당GRDP","304050대 인구비중",'도시지역인구비중','실업률'),
          coefficients=c("",""), se=c("",""))
stargazer(regmodel.2,title="IPTW 가중치를 적용한 DiD회귀모형(카셰어링진입*도시화정도)", type="html", out = "등록대수DiD(도시화).html",
          dep.var.labels = c("천인당 전체승용차등록대수","천인당 자가용승용차 등록대수"),digits = 4, digits.extra=4,
          #covariate.labels=c("카셰어링진입","도시","시골","휘발유가격","log(인구밀도)","인당GRDP","304050대 인구비중",'도시지역인구비중','천인당실업자수'),
          no.space=T,coefficients=coef_list.Cityscale,se=se_list.Cityscale)


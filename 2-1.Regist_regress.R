packages <- c("broom","rsq","cobalt","WeightIt","lme4","tidyverse","psych","stargazer","dplyr","ggplot2", "readxl", "caret", "tidyr", "stringr", "zoo", "purrr", "reshape2", "openxlsx", "ggthemes", "ggrepel","ggcorrplot","gridExtra","MatchIt","moonBook","survey","tableone")
invisible(lapply(packages, library, character.only = TRUE))

attached_parareg<-read_excel("��ϴ��_ver3.xlsx")
options("scipen"=100, "digits"=2)


#(citypoppct)Citypop�����ð����α� �������� ��ȯ
attached_parareg$citypoppct<-100*attached_parareg$CityPop/attached_parareg$Pop
#(popgrbcperthous)popgrbc�� õ�δ� 304050�α����κ�ȯ
attached_parareg$popgrbcperthous<-(attached_parareg$popgrbc)*(attached_parareg$Pop)*1000
#(popgrbcpct)popgrbcpc�� % ������ ��ȯ
attached_parareg$popgrbcpct<-(attached_parareg$popgrbc)*100
length(unique(attached_parareg$sidosigungu))
#������ 100�����̾��� GRDP��� ������ ������ ���� grdp�� �ٲ�
#attached_parareg$grdp<-(attached_parareg$GRDP)*1000000
#������ ���� grdp�� Pop�� ������ grdp.pc�� ����
#attached_parareg$grdp.pc<-(attached_parareg$grdp)/attached_parareg$Pop




#treat~ lpop+oil_realprice+lCityDensity+lGRDP+lGRDPsq+popgrbc+lcitypop+unemployment

###���Ǽ� �������� ä����
form12<-treat~ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct

#form12�� ��������� ��������
form13_2<-treat~ oil_realprice+lCityDensity+popgrbcpct+citypoppct+Unemployment
#GRDP.pc ���� �� ����� ������ ����������, ������ ������ �پ��

form13_3<-treat~ oil_realprice+lCityDensity+GRDP.pc+citypoppct+Unemployment#(form12���� Unemployment ����)
#popgrbcpct ������ urban ����� ���� �����ϰ�, �Դٰ� ������ �����µ� Ŀ��. 
form13_4<-treat~ oil_realprice+lCityDensity+GRDP.pc+citypoppct*popgrbcpct+Unemployment#(form12���� Unemployment ����)

#####������� ���ϴ� ��� ���°���.----
form13<-treat~ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct+Unemployment#(form12���� Unemployment ����)



#1. õ�δ��ϴ��----
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
###1-1. ��ü����õ�δ��ϴ��----
'
model_reg<-glm(form13, family="binomial",data=attached_parareg1)
attached_parareg1$ps<-model_reg$fitted.values
#show row if w_reg_pc>=50
attached_parareg2<-subset(attached_parareg1, !(w_reg_pc >= 50))

before_ate<-ggplot(data=attached_parareg2, aes(x=ps))+geom_density(aes(fill=factor(treat)),alpha=0.3)+ggtitle("����ġ �ο� ��")+My_Theme2+theme(plot.title = element_text(size = 25))+ylim(0,4)
after_ate<-ggplot(data=attached_parareg2, aes(x=ps,weight=w_reg_pc))+geom_density(aes(fill=factor(treat)),alpha=0.3)+ggtitle("����ġ �ο� ��")+My_Theme2+theme(plot.title = element_text(size = 25))+ylim(0,4)
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


#�������ֹ��������� ������ϴ���� ���� ������ ������ ��ħ
#������� ũ��� 4050�� �α������� ���� ŭ
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


###1-2. �ڰ�������õ�δ��ϴ��----
rgldvprivate.thousandpc.bin <- svyglm(rgldvprivate.thousandpc~treat+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct+Unemployment, design = design.ps_pcreg)
summary(rgldvprivate.thousandpc.bin)
rsq(rgldvprivate.thousandpc.bin)

rgldvprivate.thousandpc.bin_coef_vals <- as.numeric(format(summary(rgldvprivate.thousandpc.bin)$coefficients[,1],digits=4))
names(rgldvprivate.thousandpc.bin_coef_vals)<-names(summary(rgldvprivate.thousandpc.bin)$coefficients[,1])

rgldvprivate.thousandpc.bin_se_vals <- as.numeric(format(summary(rgldvprivate.thousandpc.bin)$coefficients[,2],digits=4))
names(rgldvprivate.thousandpc.bin_se_vals)<-names(summary(rgldvprivate.thousandpc.bin)$coefficients[,2])
#��κ��� ������ �����ϰ� ��Ÿ��
rgldvprivate.thousandpc.int.CityScale <- svyglm(rgldvprivate.thousandpc~treat*urban+treat*rural+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct+Unemployment, design = design.ps_pcreg)
summary(rgldvprivate.thousandpc.int.CityScale)
#treat*urban����ȭ ������ ���������� �α�õ�δ��ڰ����ϴ���� �����ϰ� ������
rsq(rgldvprivate.thousandpc.int.CityScale)
rgldvprivate.thousandpc.int.CityScale_coef_vals <- as.numeric(format(summary(rgldvprivate.thousandpc.int.CityScale)$coefficients[,1],digits=4))

names(rgldvprivate.thousandpc.int.CityScale_coef_vals)<-names(summary(rgldvprivate.thousandpc.int.CityScale)$coefficients[,1])

rgldvprivate.thousandpc.int.CityScale_se_vals <- as.numeric(format(summary(rgldvprivate.thousandpc.int.CityScale)$coefficients[,2],digits=4))
names(rgldvprivate.thousandpc.int.CityScale_se_vals)<-names(summary(rgldvprivate.thousandpc.int.CityScale)$coefficients[,2])



#treat*bad ������ ���ټ��� ���� ������ �α�õ�δ��ڰ����ϴ���� �����ϰ� �þ.
regmodel.1<-list(rgldvall.thousandpc.bin,rgldvprivate.thousandpc.bin)
regmodel.2<-list(rgldvall.thousandpc.int.CityScale,rgldvprivate.thousandpc.int.CityScale)
#.bin ���� �������Ʈ
coef_list.bin <- list(rgldvall.thousandpc.bin_coef_vals,
                      rgldvprivate.thousandpc.bin_coef_vals)
se_list.bin <- list(rgldvall.thousandpc.bin_se_vals, rgldvprivate.thousandpc.bin_se_vals)

#.Cityscale ���� �������Ʈ
coef_list.Cityscale<-list(rgldvall.thousandpc.int.CityScale_coef_vals,
                          rgldvprivate.thousandpc.int.CityScale_coef_vals)
se_list.Cityscale<-list(rgldvall.thousandpc.int.CityScale_se_vals,
                        rgldvprivate.thousandpc.int.CityScale_se_vals)
stargazer(regmodel.1,title="IPTW ����ġ�� ������ �Ϲ�ȸ�͸���(��ü����)", type="html", out = "��ϴ���Ϲ�ȸ��(��ü����).html",
          dep.var.labels = c("õ�δ� ��ü�¿�����ϴ��"),
          #covariate.labels=c("ī�ξ����", "�ֹ�������","log(�α��е�)","�δ�GRDP","304050�� �α�����",'���������α�����','�Ǿ���'),
          coefficients=rgldvall.thousandpc.bin_coef_vals, se=rgldvall.thousandpc.bin_se_vals)

----
stargazer(regmodel.1,title="IPTW ����ġ�� ������ �Ϲ�ȸ�͸���", type="html", out = "��ϴ���Ϲ�ȸ��.html",
          dep.var.labels = c("õ�δ� ��ü�¿�����ϴ��","õ�δ� �ڰ���¿��� ��ϴ��"),
          #covariate.labels=c("ī�ξ����", "�ֹ�������","log(�α��е�)","�δ�GRDP","304050�� �α�����",'���������α�����','�Ǿ���'),
          coefficients=c("",""), se=c("",""))
stargazer(regmodel.2,title="IPTW ����ġ�� ������ DiDȸ�͸���(ī�ξ����*����ȭ����)", type="html", out = "��ϴ��DiD(����ȭ).html",
          dep.var.labels = c("õ�δ� ��ü�¿�����ϴ��","õ�δ� �ڰ���¿��� ��ϴ��"),digits = 4, digits.extra=4,
          #covariate.labels=c("ī�ξ����","����","�ð�","�ֹ�������","log(�α��е�)","�δ�GRDP","304050�� �α�����",'���������α�����','õ�δ�Ǿ��ڼ�'),
          no.space=T,coefficients=coef_list.Cityscale,se=se_list.Cityscale)

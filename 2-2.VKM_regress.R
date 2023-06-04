packages <- c("cobalt","WeightIt","lme4","tidyverse","psych","stargazer","dplyr","ggplot2", "readxl", "caret", "tidyr", "stringr", "zoo", "purrr", "reshape2", "openxlsx", "ggthemes", "ggrepel","ggcorrplot","gridExtra","MatchIt","moonBook","survey","tableone")
invisible(lapply(packages, library, character.only = TRUE))

attached_paravkm<-read_excel("����Ÿ�_ver3.xlsx")


#(citypoppct)Citypop�����ð����α� �������� ��ȯ
attached_paravkm$citypoppct<-100*attached_paravkm$CityPop/attached_paravkm$Pop
attached_paravkm$loilprice<-log(attached_paravkm$oil_realprice)

#���Ӻ��� unit�� õ km���� '10��(hundred thousand)km' �� ��ȯ
attached_paravkm$vkm_ldv_private.ht<-attached_paravkm$vkm_ldv_private/100
attached_paravkm$vkm_ldv_all.ht<-attached_paravkm$vkm_ldv_all/100
#(popgrbcperthous)popgrbc�� õ�δ� 304050�α����κ�ȯ
attached_paravkm$popgrbcperthous<-(attached_paravkm$popgrbc)*(attached_paravkm$Pop)*1000
#(popgrbcpct)popgrbcpc�� % ������ ��ȯ
attached_paravkm$popgrbcpct<-(attached_paravkm$popgrbc)*100



form7<-treat~ lpop+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct#(������� ���� �׷����� ���)
form8<- treat~lpop+oil_realprice+lCityDensity+lGRDP.pc+popgrbc+citypopratio+Unempperthous
form9<-treat~ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct#(form7���� lpop ����)
form10<-treat~Pop+oil_realprice+lCityDensity+GRDP.pc+popgrbc+citypopratio+marriagerate
form11<-treat~ oil_realprice+lCityDensity+GRDP.pc+popgrbc+citypopratio+marriagerate
form13<-treat~Pop+ oil_realprice+CityDensity+GRDP.pc+popgrbcpct+citypoppct+Unempperthous#(form9���� Unempperthousand ����)
#oil_realprice�� log ������

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

#ATE ������� �����Ŵ

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

before_ate<-ggplot(data=attached_paravkm2, aes(x=ps))+geom_density(aes(fill=factor(treat)),alpha=0.3)+ggtitle("����ġ �ο� ��")+My_Theme2+theme(plot.title = element_text(size = 25))+ylim(0,4)
after_ate<-ggplot(data=attached_paravkm2, aes(x=ps,weight=w_vkm_pc))+geom_density(aes(fill=factor(treat)),alpha=0.3)+ggtitle("����ġ �ο� ��")+My_Theme2+theme(plot.title = element_text(size = 25))+ylim(0,4)


grid.arrange(before_ate,after_ate, ncol=2)
ggsave("bfate_vkm.png", grid.arrange(before_ate,after_ate, ncol=2))
'
###2-1.��ü���� ����Ÿ�
vkm_ldv_all.pc.bint <- svyglm(vkm_ldv_all.ht~treat+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct+factor(t)
                          , design = design.ps_pcvkm)
vkm_ldv_all.pc.bin <- svyglm(vkm_ldv_all.ht~treat+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct,design = design.ps_pcvkm)
summary(vkm_ldv_all.pc.bin)
format(summary(vkm_ldv_private.pc.bin)$coefficients[,1],digits=4)
format(summary(vkm_ldv_private.pc.bin)$coefficients[,2],digits=3)
summary(vkm_ldv_all.pc.bin)
rsq(vkm_ldv_private.pc.bin)
#��ü���� �������Ÿ��� treat�� ���� �����ϰ� �پ�����.
vkm_ldv_all.pc.int.CityScalet<- svyglm(vkm_ldv_all.ht~treat*urban+treat*rural+ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct+factor(t)
                                    , design = design.ps_pcvkm)
vkm_ldv_all.pc.int.CityScale <- svyglm(vkm_ldv_all.ht~treat*urban+treat*rural+ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct,design = design.ps_pcvkm)
summary(vkm_ldv_all.pc.int.CityScale)
format(summary(vkm_ldv_private.pc.int.CityScale)$coefficients[,1],digits=6)
format(summary(vkm_ldv_private.pc.int.CityScale)$coefficients[,2],digits=5)


rsq(vkm_ldv_private.pc.int.CityScale)


rsq(vkm_ldv_all.pc.int.CityScale)
#��ü���� �������Ÿ��� treat�� ���� �����ϰ� �پ�����.(�ռ������ ��ġ��)
#�׷��� �ñ����� ����ȭ ������ ���� ī�ξ ������ ��ü���� �������Ÿ��� ���� ������ �������� �ʾ���.

#��ü���� �������Ÿ��� treat�� ���� �����ϰ� �پ�����.(�ռ������ ��ġ)
#������ ���ٰ��ɼ��� ���� ī�ξ ������ ��ü���� �������Ÿ��� ���� ������ �������� �ʾ���.


###2-2.�ڰ��� ����Ÿ�

vkm_ldv_private.pc.bin <- svyglm(vkm_ldv_private.ht~treat+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct
                             , design = design.ps_pcvkm)
summary(vkm_ldv_private.pc.bin)
rsq(vkm_ldv_private.pc.bin)

#��ü���� �������Ÿ��� treat�� ���� �����ϰ� �پ�����.

vkm_ldv_private.pc.int.CityScale <- svyglm(vkm_ldv_private.ht~treat*urban+treat*rural+ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct
                                       , design = design.ps_pcvkm)
summary(vkm_ldv_private.pc.int.CityScale)
rsq(vkm_ldv_private.pc.int.CityScale)
#��ü���� �������Ÿ��� treat�� ���� �����ϰ� �پ�����.(�ռ������ ��ġ��)
#�׷��� �ñ����� ����ȭ ������ ���� ī�ξ ������ ��ü���� �������Ÿ��� ���� ������ �������� �ʾ���.
#vkm_ldv_all.pc.int.availabilityt <- svyglm(vkm_ldv_private.ht~treat*good+treat*bad+ Pop+ oil_realprice+CityDensity+GRDP.pc+popgrbcpct+citypoppct+Unempperthous+factor(t)
#                                           , design = design.ps_pcvkm)


#��ü���� �������Ÿ��� treat�� ���� �����ϰ� �پ�����.(�ռ������ ��ġ)
#������ ���ٰ��ɼ��� ���� ī�ξ ������ ��ü���� �������Ÿ��� ���� ������ �������� �ʾ���.
vkmmodel.1<-list(vkm_ldv_all.pc.bin,vkm_ldv_private.pc.bin)
summary(vkm_ldv_all.pc.bin)
vkmmodel.2<-list(vkm_ldv_all.pc.int.CityScale,vkm_ldv_private.pc.int.CityScale)

stargazer(vkmmodel.1,title="IPTW ����ġ�� ������ �Ϲ�ȸ�͸���", type="html", out = "����Ÿ��Ϲ�ȸ��.html",
          dep.var.labels = c("��ü","�ڰ���"),
          #covariate.labels=c("ī�ξ����", "�ֹ�������","log(�α��е�)","�δ�GRDP","304050�� �α�����",'���������α�����','õ�δ�Ǿ��ڼ�'),
          no.space=T)
stargazer(vkmmodel.2,title="IPTW ����ġ�� ������ DiDȸ�͸���(ī�ξ����*����ȭ����)", type="html", out = "����Ÿ�DiD(����ȭ).html",
          dep.var.labels = c("��ü","�ڰ���"),
          #covariate.labels=c("ī�ξ����","����","�ð�","�ֹ�������","log(�α��е�)","�δ�GRDP","304050�� �α�����",'���������α�����','õ�δ�Ǿ��ڼ�'),
          no.space=T)

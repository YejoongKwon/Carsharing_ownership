# Package names
packages <- c("cobalt","WeightIt","lme4","tidyverse","psych","stargazer","dplyr","ggplot2", "readxl", "caret", "tidyr", "stringr", "zoo", "purrr", "reshape2", "openxlsx", "ggthemes", "ggrepel","ggcorrplot","gridExtra","MatchIt","moonBook","survey","tableone")
# Install packages not yet installed
'installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}'
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
My_Theme1 = theme_economist()+theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 13,angle=90,hjust=1),
  axis.title.y = element_text(size = 17))#+ theme(legend.position = "none")

My_Theme2 = theme_economist()+theme(
  axis.title.x = element_text(size = 11),
  axis.text.x = element_text(size = 10,angle=90, hjust=1),
  axis.title.y = element_text(size =11),)+ theme(legend.position = "none")

#1.���������� TRIM----
#confound variables
Oil_Price<-read_excel("allsigungu_update.xlsx",sheet='Oilprice') #��/����
City_Pop<-read_excel("allsigungu_update.xlsx",sheet='CityPop') #��
CityDensity<-read_excel("allsigungu_update.xlsx",sheet='CityDensity') #��/��
GRDP_nu<-read_excel("allsigungu_update.xlsx",sheet='GRDP') #2015�� �鸸 ��
Pop<-read_excel("allsigungu_update.xlsx",sheet='Pop') #��
popgrbc<-read_excel("allsigungu_update.xlsx",sheet='popgrbc')#����
#Y variable
rg_ldv_all<-read_excel("allsigungu_update.xlsx",sheet='rg_ldv_all') #��
rg_ldv_private<-read_excel("allsigungu_update.xlsx",sheet='rg_ldv_private') #��
vkm_ldv_all<-read_excel("allsigungu_update.xlsx",sheet='vkm_ldv_all') #1000 km
vkm_ldv_private<-read_excel("allsigungu_update.xlsx",sheet='vkm_ldv_private') #1000 km
Area<-read_excel("allsigungu_update.xlsx",sheet='Area') #��
Unemp<-read_excel("allsigungu_update.xlsx",sheet='Unemp') #%
#unit transform
'
GRDP_nu[is.na(GRDP_nu)] <-0
GRDP_nu[,3:14] <- lapply(GRDP_nu[,3:14], as.numeric)
GRDP<-GRDP_nu
GRDP[,3:14]<-GRDP_nu[,3:14] * 1000000

vkm_ldv_all[is.na(vkm_ldv_all)] <-0
vkm_ldv_all[,3:12] <- lapply(vkm_ldv_all[,3:12], as.numeric)��
vkm_ldv_all<-vkm_ldv_all
vkm_ldv_all[,3:12]<-vkm_ldv_all[,3:12] * 1000

vkm_ldv_private[is.na(vkm_ldv_private)] <-0
vkm_ldv_private[,3:12] <- lapply(vkm_ldv_private[,3:12], as.numeric)
vkm_ldv_private<-vkm_ldv_private
vkm_ldv_private[,3:12]<-vkm_ldv_private[,3:12] *1000
'
#���� GRDP�� ���̰�, vkm�� km��.
#rowname ���� : sido, sigungu

City_Pop<-City_Pop %>%tidyr::unite(rowname, sido, sigungu) %>%tibble::column_to_rownames()
CityDensity<-CityDensity %>%tidyr::unite(rowname, sido, sigungu) %>%tibble::column_to_rownames()
GRDP<-GRDP_nu %>%tidyr::unite(rowname, sido, sigungu) %>%tibble::column_to_rownames()
Pop<-Pop %>%tidyr::unite(rowname, sido, sigungu) %>%tibble::column_to_rownames()
rg_ldv_all<-rg_ldv_all %>%tidyr::unite(rowname, sido, sigungu) %>%tibble::column_to_rownames()
rg_ldv_private<-rg_ldv_private %>%tidyr::unite(rowname, sido, sigungu) %>%tibble::column_to_rownames()
#vkm_ldv_all<-vkm_ldv_all %>%tidyr::unite(rowname, sido, sigungu) %>%tibble::column_to_rownames()
#vkm_ldv_private<-vkm_ldv_private %>%tidyr::unite(rowname, sido, sigungu) %>%tibble::column_to_rownames()
popgrbc<-popgrbc %>%tidyr::unite(rowname, sido, sigungu) %>%tibble::column_to_rownames()

#make 2009-2020 DATASET
modelyear<- seq(from=2009, to=2020)
modelyear_2012<-seq(from=2012,to=2020)

City_Pop.my <- names(City_Pop)[(names(City_Pop) %in% modelyear)]
CityPop1<- City_Pop[, City_Pop.my]
CityDensity.my <- names(CityDensity)[(names(CityDensity) %in% modelyear)]
CityDensity1<- CityDensity[, CityDensity.my]
GRDP.my <- names(GRDP)[(names(GRDP) %in% modelyear)]
GRDP1<- GRDP[, GRDP.my]
Pop.my <- names(Pop)[(names(Pop) %in% modelyear)]
Pop1<- Pop[, Pop.my]
rg_ldv_all.my <- names(rg_ldv_all)[(names(rg_ldv_all) %in% modelyear)]
rg_ldv_all1<- rg_ldv_all[, rg_ldv_all.my]
rg_ldv_private.my <- names(rg_ldv_private)[(names(rg_ldv_private) %in% modelyear)]
rg_ldv_private1<- rg_ldv_private[, rg_ldv_private.my]
popgrbc.my<-names(popgrbc)[(names(popgrbc) %in% modelyear)]
popgrbc1<- popgrbc[, popgrbc.my]
rgldvall1<-rg_ldv_all1
rgldvprivate1<-rg_ldv_private1

#Grab a list of filenames
dfs_name<-c("CityPop1","CityDensity1","GRDP1","Pop1","popgrbc1","rgldvall1","rgldvprivate1")
combined<-do.call("rbind", mget(dfs_name))


combined <- tibble::rownames_to_column(combined, "variable")
combined<-combined %>%
  separate(variable, c("var", "sigungu"), "1.")
combined_t<-combined %>%
  tidyr::unite(rowname,var,sigungu) %>%
  tibble::column_to_rownames()
combined_tt<-as.data.frame(t(combined_t))
combined_ta <- tibble::rownames_to_column(combined_tt, "year")
ttnamevec<-colnames(combined_ta)

ttname<-sapply(strsplit(ttnamevec, "_"), function(z) paste(z[1], paste(z[-1], collapse = ""), sep = "_"))
colnames(combined_ta)<-ttname
#no�� year��.
nameofcombinedA1 <- names(select(combined_ta, -year_))
combinedb<- melt(combined_ta, id.vars =c("year_"), measure.vars = nameofcombinedA1) %>%
  separate(variable, c("var", "sigungu"), "_")
combinedc <- dcast(combinedb, year_+sigungu ~ var) #�� melt�� b�Լ� �̿�

#��û�ϵ�û���� ����
combinedd<-combinedc[!grepl("û����", combinedc$sigungu),]

#Xst �����ϱ�----
xst<-read_excel("allsigungu_update.xlsx",sheet='xst')
#���ó⵵ ��� ���� NA�� �ñ��� ������������ �����غ���
#Xst<-xst[!with(xst,is.na(greencar)& is.na(socar)),]

xst["greencar"][is.na(xst["greencar"])] <- 0
xst$socar<-as.numeric(xst$socar)
xst["socar"][is.na(xst["socar"])] <- 0

#����� ���ó⵵ �� �������� ���ó⵵�� �������� ��������
Xst<- xst %>% mutate(forerunner= if_else(greencar<socar, xst$greencar, xst$socar))
#forerunner�� 0�� row�� �̾Ƴ��� ifelse ó�� 
flip<-Xst %>% filter(forerunner==0) 
flip = subset(flip, select = -c(forerunner) )
flip2<-flip %>%
  mutate( forerunner= pmax(greencar, socar, na.rm = TRUE))
flip3<-Xst %>% filter(forerunner!=0)
#flip2�� flip3�� rbind �� arrange(sido,sigungu)
X_ <- rbind(flip3,flip2)
X_ordered <- X_[ order(X_$sido,X_$sigungu), ]

bothentry<-X_ordered[!apply(X_ordered[, c('greencar','socar')]==0, 1, any),]

#(scatterplot)�׸�ī ��ī ��� ������ 156 �� �ñ����� ���� ������----
#�׸�ī�� �׸�, ��ī�� �Ķ�
bothentry$greencarspecific<-as.Date(bothentry$greencarspecific)
bothentry$socarspecific<-as.Date(bothentry$socarspecific)

'
#a1<-ggplot(data=bothentry, aes(x=greencarspecific, y=socarspecific))+geom_text_repel(aes(x=greencarspecific, y=socarspecific, label=sigungu),max.overlaps = Inf)+
  geom_vline(xintercept =as.Date("2017-01-01"), linetype= "longdash", color="red", linewidth=1)+geom_hline(yintercept =as.Date("2017-01-01"), linetype= "longdash", color="red", linewidth=1)+
  labs(x = expression(paste(GREENCAR entry)), color = "seagreen")+
  labs(y = expression(paste(SOCAR entry)), color = "blue")+
  theme(axis.text.x=element_text(color="seagreen"),axis.text.y=element_text(color="blue"))+
  theme(panel.background = element_rect(fill = "white"))+
  scale_x_date(date_breaks = "1 year") +
  scale_y_date(date_breaks = "1 year") +  # Add y-axis date breaks
  theme(panel.grid.major.x = element_line(color = "seagreen", linetype = "dotted"),
        panel.grid.major.y = element_line(color = "blue", linetype = "dotted"))+
  xlim(as.Date("2011-01-01"),as.Date("2021-12-25")) + ylim(as.Date("2011-01-01"),as.Date("2021-12-25"))# Customize grid lines
'
# Extract the data used for the geom_text_repel layer
#text_df <- ggplot_build(a1)$data[[1]]

# Create a new column indicating the quadrant for each point
#text_df$quadrant <- ifelse(text_df$y >= as.Date("2017-01-01") & text_df$x >= as.Date("2017-01-01"), "quadrant1",
#                           ifelse(text_df$y < as.Date("2017-01-01") & text_df$x >= as.Date("2017-01-01"), "quadrant2",
#                                  ifelse(text_df$y < as.Date("2017-01-01") & text_df$x < as.Date("2017-01-01"), "quadrant3", "quadrant4")))

# Display the list of points and their respective quadrants
#quadrants_df<-data.frame(point = text_df$label, quadrant = text_df$quadrant)
#writexl::write_xlsx(quadrants_df, path = "��и�.xlsx")
#ggsave("haebom.png", a1, width = 10, height = 10, dpi = 150, units = "in", device='png')

#2017 �������� vline, hline �߱�

#1,3��и��� ��ī-�׸�ī�� ����� �ñ⿡ ������.
#3��и鿡 �ִ� �ֵ��� �α�� ��� �̸� ����
#1��и� �ֵ��� �α�� ��� ���� ����

#2��и��� �ֵ��� �׸�ī�� ��ī���� ���� ����
#4��и��� �ֵ��� ��ī�� �׸�ī���� ���� ����
#��ī�� ���� ������ �ñ����� �� ����.

#ngreencarzone,nsocarzone �հ�������� �ñ��� ����----
#ngreencarzone,nsocarzone �հ谡 5�� �̸��� �ñ��� �����ϰ� �ٽ� regress----

#bothentry ���̱�
bothentry$sidosigungu <- paste(bothentry$sido, bothentry$sigungu)
bothentry$sidosigungu<-str_replace(bothentry$sidosigungu,' ','')
attached<-left_join(combinedd,bothentry,by = c("sigungu" = "sidosigungu"))

#attached$rgldvall <- as.numeric(gsub(",","",attached$rgldvall))
#trim the comma column----

#forerunner�� NA�� �÷� �����ϱ�
noforerunner<-subset(attached, (is.na(attached[,c('forerunner')])))
length(unique(noforerunner$sigungu))#�� 72�� �ñ����� ī�ξ ���� �������� ����
#forerunner�� NA�� treat 0�� �ǵ��� forerunner=NA ���ٰ� ���� �̻��� 100000�� �־����
attached$forerunner <- ifelse(is.na(attached$forerunner), 100000, attached$forerunner)


#forerunner(������ treat)���� year_�÷��� ũ�� 1, year_�÷��� �۰ų� ������ 0
attached$treat<-ifelse(attached$year_>attached$forerunner,1,0)

attached$treat<-ifelse(attached$forerunner==100000,0,attached$treat)


attached[,3:9] <- lapply(attached[,3:9], as.numeric)
attached$pcturban<-attached$CityPop/attached$Pop #��ü�α��� ���ð����α��� ����
attached$pop_u2 <-  attached$pcturban - min(attached$pcturban)

z<-aggregate(attached$pop_u2, list(attached$sigungu), mean)

#���� (����, ���ֵ��� �ñ����ƴϰ� �õ���)128�� �õ��ñ����� �������� �α����ߵ� ����հ���
'z_noseoul<-z[!grepl("����Ư����", z$Group.1),]
z_justseoul<-z[grepl("����Ư���ü���Ư����",z$Group.1),]
zs<-rbind(z_noseoul,z_justseoul)
zs_ordered <- zs[ order(zs$x), ]
'
#����Ư����ġ��, ����Ư���� �̸� ���󺹱�
#zs_ordered$sigungu[zs_ordered$sigungu == '����Ư����ġ������Ư����ġ��'] <- '����Ư����ġ��'
#zs_ordered$sigungu[zs_ordered$sigungu == '����Ư���ü���Ư����'] <- '����Ư����'


# add a row number variable `order`

zs_ordered <- z[ order(z$x), ]

zs_ordered <- cbind(zs_ordered,order=1:nrow(zs_ordered))
colnames(zs_ordered)<-c("sigungu","meanpcturban",'order')

#ggplot(zs_ordered, aes(x=order, y=meanpcturban, label=sigungu))+  geom_point()+My_Theme2+ geom_text(size=4,hjust = 0, nudge_x = 0.05)+theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())
show1<-ggplot(zs_ordered, aes(x=order, y=meanpcturban, label=sigungu))+  geom_point()+ geom_text_repel(aes(x=order, y=meanpcturban, label=sigungu),max.overlaps = Inf)+theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())+geom_vline(xintercept =75, linetype= "longdash", color="red", linewidth=1)+geom_vline(xintercept =129, linetype= "longdash", color="red", linewidth=1)
show2<-show1+theme_bw()
nrow(zs_ordered)
#'+xlim(60, 128)+ylim(0.6,0.8)'

#1~68�� �����ͼ����� �ٽ� ggplot

#noturbanized<-zs_ordered[c(1:68),]
#show2<-ggplot(noturbanized, aes(x=order, y=meanpcturban, label=sigungu))+  geom_point()+My_Theme2+ geom_text_repel(aes(x=order, y=meanpcturban, label=sigungu),max.overlaps = Inf)+theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())+geom_vline(xintercept =12.5, linetype= "longdash", color="red", linewidth=1)
#grid.arrange(show2,show1, ncol=2)


#zs_ordered$RowNames <- row.names(zs_ordered)

#urban-avg-rural indicators in 3 categories----
#1~12������ rural, 13~68�� avg, 69~128�� urban���� �з�.

rural_idx<-zs_ordered[c(1:74),1]
avg_idx<-zs_ordered[c(75:127),1]
urban_idx<-zs_ordered[c(128:228),1]

attached$rural <- rep(0,length(attached$sigungu))#rural �̶�� �����߰�
attached[(attached$sigungu %in% rural_idx),]$rural <- 1
attached$avg <- rep(0,length(attached$sigungu))#avg ��� �����߰�
attached[(attached$sigungu %in% avg_idx),]$avg <- 1
attached$urban <- rep(0,length(attached$sigungu))#urban ��� �����߰�
attached[(attached$sigungu %in% urban_idx),]$urban <- 1

attached$cityscale <- rep(0,length(attached$sigungu))#cityscale ��� �����߰�
attached[(attached$sigungu %in% urban_idx),]$cityscale <-"urban"
attached[(attached$sigungu %in% avg_idx),]$cityscale <-"avg"
attached[(attached$sigungu %in% rural_idx),]$cityscale <-"rural"
attached[, c("cityscale")][attached[, c("cityscale")]== 0] <-'urban'

#ggsave("cityscale_sigungu.png", show2, width = 40, height = 20, dpi = 150, units = "in", device='png')
#(Area)���� ������ ���� �� ����
Area1<-Area[,3:15]
meltedArea1 <- melt(Area1, id.vars =c("sidosigungu"), measure.vars = c(2:13))
colnames(meltedArea1)<-c("sidosigungu","year_","area")
attached <- left_join(attached, meltedArea1, by=c("sigungu" = "sidosigungu","year_"="year_"))


#availability(�δ� ��������)�������� �ñ����� 3���� ����----
#bothentry �����ͼ� ���� �ϸ� ��
bothentry$totalzone <- bothentry$ngreenzone+bothentry$nsocarzone
#156��
#attached���� ���� �������� �����ؼ� bothentry$lastarea ����
lastarea<-attached[c(2509:2736),c('sigungu','area')]
lastarea1<-left_join(bothentry, lastarea, by=c("sidosigungu" = "sigungu"))

lastarea1$totalzoneperarea<-lastarea1$totalzone/lastarea1$area


#������ ��������(totalzonepc)�� �������� ��������(1����)
lastarea1$rank <- NA
order.scores_lastarea1<-order(lastarea1$totalzoneperarea,decreasing=TRUE)

lastarea1$rank[order.scores_lastarea1] <- 1:nrow(lastarea1)
dd_2<-lastarea1
nrow(dd_2)
#(���⼭����)rank�� sidosigungu �� graph----
av1<-ggplot(dd_2, aes(x=rank, y=totalzoneperarea, label=sidosigungu))+  geom_point() +geom_text_repel(aes(x=rank, y=totalzoneperarea, label=sidosigungu),max.overlaps = Inf)+theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())+ylim(0,7)+geom_vline(xintercept =52, linetype= "longdash", color="red", linewidth=1)+geom_vline(xintercept =104, linetype= "longdash", color="red", linewidth=1)+theme_bw()
#av2<-ggplot(dd_2, aes(x=rank, y=totalzone, label=sidosigungu))+  geom_point()+My_Theme2 +xlim(114,230) + ylim(0,130)+geom_text_repel(aes(x=rank, y=totalzone, label=sidosigungu),max.overlaps = Inf)+theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())+geom_vline(xintercept =153, linetype= "longdash", color="red", linewidth=1)

#grid.arrange(av1,av2, ncol=2)
#ggsave("availability_final.png", av1, width = 40, height = 20, dpi = 150, units = "in", device='png')

#�ϴ� 1~52, 53~104, 105~156 ������ ������
#���� ������ �� ��Ż 2�� �̸��� ������ bad ���ٰ� �ٽ� �ֱ�
lastarea1_ordered <- lastarea1[ order(lastarea1$rank), ]


lastarea1_ordered<-cbind(lastarea1_ordered,order=1:nrow(lastarea1_ordered))

lastarea1_ordered<-lastarea1_ordered[,c('sidosigungu','totalzoneperarea','order')]
lastarea2_ordered<-lastarea1_ordered[-c(154:156),]


good_idx<-lastarea2_ordered[c(1:51),1]
soso_idx<-lastarea2_ordered[c(52:104),1]
bad_idx<-lastarea2_ordered[c(105:153),1]


attached$good <- rep(0,length(attached$sigungu))#good �̶�� �����߰�
attached[(attached$sigungu %in% good_idx),]$good <- 1

attached$soso <- rep(0,length(attached$sigungu))#soso ��� �����߰�
attached[(attached$sigungu %in% soso_idx),]$soso <- 1

attached$bad <- rep(0,length(attached$sigungu))#bad ��� �����߰�
attached[(attached$sigungu %in% bad_idx),]$bad <- 1


attached$availiability <- rep(0,length(attached$sigungu))#availability ��� �����߰�
attached[(attached$sigungu %in% good_idx),]$availiability <-"good"
attached[(attached$sigungu %in% soso_idx),]$availiability <-"soso"
attached[(attached$sigungu %in% bad_idx),]$availiability <-"bad"

length(unique(attached$sigungu))


#Oil_Price ����
attached<- left_join(attached, Oil_Price, by=c("sigungu" = "sigungu","year_"="year_"))

#(popgrab)2030�α����� ������ ���� �� ����
popgrab<-read_excel("allsigungu_update.xlsx",sheet='popgrab')
popgrab1<-popgrab[,3:15]
meltedpopgrab <- melt(popgrab1, id.vars =c("sidosigungu"), measure.vars = c(2:13))
colnames(meltedpopgrab)<-c("sidosigungu","year_","popgrab")

length(unique(attached$sigungu))



Unemp1<-Unemp[,3:15]
meltedUnemp <- melt(Unemp1, id.vars =c("sidosigungu"), measure.vars = c(2:13))
colnames(meltedUnemp)<-c("sidosigungu","year_","Unemp")
attached<- left_join(attached, meltedUnemp, by=c("sigungu" = "sidosigungu","year_"="year_"))

#(vkm_ldv)�¿��� ����Ÿ� ������ ���� �� ����
#unit: õkm
vkm_ldv_private<-vkm_ldv_private[,-c(12)]
vkm_ldv_all<-vkm_ldv_all[,-c(12)]

vkm_ldv_private$sigungu <- paste(vkm_ldv_private$sido,vkm_ldv_private$sigungu)
vkm_ldv_private$sigungu <- gsub('\\s+', '',vkm_ldv_private$sigungu)
vkm_ldv_private<-vkm_ldv_private[,-c(1)]

vkm_ldv_all$sigungu <- paste(vkm_ldv_all$sido,vkm_ldv_all$sigungu)
vkm_ldv_all$sigungu <- gsub('\\s+', '',vkm_ldv_all$sigungu)
vkm_ldv_all<-vkm_ldv_all[,-c(1)]


length(unique(attached$sigungu))


melted_vkm_ldv_private <- melt(vkm_ldv_private, id.vars =c("sigungu"), measure.vars = c(2:10))
colnames(melted_vkm_ldv_private)<-c("sidosigungu","year_","vkm_ldv_private")
melted_vkm_ldv_all <- melt(vkm_ldv_all, id.vars =c("sigungu"), measure.vars = c(2:10))
colnames(melted_vkm_ldv_all)<-c("sidosigungu","year_","vkm_ldv_all")
length(unique(attached$sigungu))


'
#(marriagerate)ȥ���� ������ ���� �� ����
marriagerate<-read_excel("allsigungu_update.xlsx",sheet=marriagerate)
marriagerate1<-marriagerate[,3:15]
meltedmarriagerate <- melt(marriagerate1, id.vars =c("sidosigungu"), measure.vars = c(2:13))
colnames(meltedmarriagerate)<-c("sidosigungu","year_","marriagerate")
attached <- left_join(attached, meltedmarriagerate, by=c("sigungu" = "sidosigungu","year_"="year_"))
attached$marriagerate<-as.numeric(attached$marriagerate)
'

attached <- left_join(attached, meltedpopgrab, by=c("sigungu" = "sidosigungu","year_"="year_"))
length(unique(attached$sigungu))


attached <- left_join(attached, melted_vkm_ldv_private, by=c("sigungu" = "sidosigungu","year_"="year_"))
attached <- left_join(attached, melted_vkm_ldv_all, by=c("sigungu" = "sidosigungu","year_"="year_"))
attached<-attached[,-c(10:11)]
colnames(attached)[colnames(attached) == "sigungu"] <- "sidosigungu"




#colnames(attached)[which(names(attached) == "sigungu")] <- "sidosigungu"
'
#Find columns that match the string patterns using grepl() function
cols_sido <- grepl("sido\\.", names(attached))
for (i in which(cols_sido)) {
  names(attached)[i] <- "sido"
}
cols_sigungu<-grepl("sigungu\\.", names(attached))
for (i in which(cols_sigungu)) {
  names(attached)[i] <- "sigungu"
}
# Identify and remove duplicated columns
dup_cols <- which(duplicated(names(attached)))
attached <- attached[,-dup_cols]
'


#�Ǿ����� õ���� �Ǿ�������
attached$Unempperthous<-(attached$Unemp/100)*(attached$Pop)/1000


#��ȯ��������----
#0)�⺻����
attached$id <- as.numeric(factor(attached$sidosigungu,levels=unique(attached$sidosigungu)))#id�� sigungu�� ��ġ�κ�ȯ
attached$name<-attached$sidosigungu

length(unique(attached$sidosigungu))



#attached$t <- as.numeric(attached$year_)-2009
attached$Year <- factor(attached$year_,ordered=TRUE)#�빮��year�� factor������.
length(unique(attached$sidosigungu))

#ggplot(data=attached_parareg_weighted, aes(x=CityDensity))+geom_density(aes(fill=factor(treat)),alpha=0.3)+ggtitle("����ġ �ο� ��") +My_Theme1


#GRDP,rgldvall,rgldvprivate�� ��ȯ
#1)per capita ��ȯ
attached$vkm_ldv_all<-as.numeric(attached$vkm_ldv_all)
attached$vkm_ldv_private<-as.numeric(attached$vkm_ldv_private)
attached<- attached %>% mutate_at(c("vkm_ldv_private","vkm_ldv_all"), ~replace_na(.,0))

attached$GRDP.pc <- attached$GRDP/attached$Pop
attached$rgldvall.pc <- attached$rgldvall/attached$Pop
attached$rgldvprivate.pc <- attached$rgldvprivate/attached$Pop
attached$vkm_ldv_all.pc<-attached$vkm_ldv_all/attached$Pop
attached$vkm_ldv_private.pc<-attached$vkm_ldv_private/attached$Pop
attached$rgldvall.thousandpc<-attached$rgldvall/(attached$Pop)*1000
attached$rgldvprivate.thousandpc<-attached$rgldvprivate/(attached$Pop)*1000

#1-1)log (per capita) ��ȯ
attached$lGRDP.pc <- log(attached$GRDP.pc)
attached$lrgldvall.pc <- log(attached$rgldvall.pc)
attached$lrgldvprivate.pc <- log(attached$rgldvprivate.pc)
attached$lvkm_ldv_all.pc<-log(attached$vkm_ldv_all.pc)
attached$lvkm_ldv_private.pc<-log(attached$vkm_ldv_private.pc)


attached$lCityDensity<-log(attached$CityDensity)
attached$lGRDP<-log(attached$GRDP)
attached$lGRDPsq<-attached$lGRDP**2

#1-1-1)log(per capita)  square ��ȯ
attached$lGRDP.pcsq <- attached$lGRDP.pc**2
attached$lrgldvall.pcsq <- attached$lrgldvall.pc**2
attached$lrgldvprivate.pcsq <- attached$lrgldvprivate.pc**2
attached$lvkm_ldv_all.pcsq <- attached$lvkm_ldv_all.pc**2
attached$lvkm_ldv_private.pcsq <- attached$lvkm_ldv_private.pc**2


#1-1-2)per capita log cubic ��ȯ
attached$lGRDP.pccubic <- attached$lGRDP.pc**3
attached$lrgldvall.pccubic <- attached$lrgldvall.pc**3
attached$lrgldvprivate.pccubic <- attached$lrgldvprivate.pc**3
attached$lvkm_ldv_all.pccubic <- attached$lvkm_ldv_all.pc**3
attached$lvkm_ldv_private.pccubic <- attached$lvkm_ldv_private.pc**3

#2)log ��ȯ
attached$lpop<-attached$Pop/log(attached$Pop)
attached$lpopsq<-attached$lpop**2
attached$lpopcubic<-attached$lpop**3

attached$lcitypop<-attached$CityPop/log(attached$CityPop)
attached$lcitypopsq<-attached$lcitypop**2
attached$lcitypopcubic<-attached$lcitypop**3
#õ������
attached$thousandpop<-attached$Pop*1000
attached$Unemployment<-attached$Unemp/100
attached$lthousandpop<-log(attached$thousandpop)

#1)attached_parareg : ��ϴ�� �������� �����ͼ�(2010-2020)
attached <- attached[attached$sigungu.y.y != '����Ư����',]

attached_parareg<-attached


attached_parareg <- attached_parareg[!(attached_parareg$year_ %in% c("2009")),]
attached_parareg$t <- as.numeric(attached_parareg$year_)-2010
length(unique(attached_parareg$sidosigungu))

#����ó� �ñ��� ������ ����
'
#���⼭ �ñ����� ����Ư���� ���� �ֵ鵵 ���ܽ�Ű��
seoulSubset <- attached_parareg[grep("����Ư����", attached_parareg$name), ]
noseoulSubset <-  attached_parareg[!grepl("����Ư����",attached_parareg$name),]
seoulseoul<-attached_parareg[grep("����Ư���ü���Ư����", attached_parareg$name), ]
attached_parareg<-rbind(noseoulSubset,seoulseoul)
'

#2)attached_paravkm : ����Ÿ� �������� �����ͼ�(2012-2020)
#paravkmrows <- apply(attached, 1, function(x){any(is.na(x))})
#attached_paravkm <- attached[!paravkmrows,]



attached_paravkm<-attached
length(unique(attached_paravkm$sidosigungu))
#2012�� t=1�� �����
attached_paravkm <- attached_paravkm[!(attached_paravkm$year_ %in% c("2009", "2010","2011")),]

attached_paravkm$t<-as.numeric(attached_paravkm$year_)-2012
length(unique(attached_parareg$sidosigungu))

attached_parareg_subset <-subset(attached_parareg, availiability != "0")
attached_paravkm_subset <-subset(attached_paravkm, availiability != "0")


attached_parareg2 <- attached_parareg[,-c(28)]

attached_paravkm2 <- attached_paravkm[,-c(28)]




nrow(attached_parareg2)
#cityscale�� �м� ���� dataset
writexl::write_xlsx(attached_parareg2, path = "��ϴ��_ver3.xlsx")
writexl::write_xlsx(attached_paravkm2, path = "����Ÿ�_ver3.xlsx")
length(unique(attached_paravkm$sidosigungu))
#availability=0�� �����͸� �ƿ� ������ subset
writexl::write_xlsx(attached_parareg_subset, path = "��ϴ��_���ټ�df_new.xlsx")
writexl::write_xlsx(attached_paravkm_subset, path = "����Ÿ�_���ټ�df_new.xlsx")

length(unique(attached_parareg_subset$sidosigungu))
# Package names
packages <- c("dplyr","ggplot2","readxl","caret","ggrepel","ggthemes","tidyverse","lubridate")
# Install packages not yet installed
'installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}'
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
#Xst �����ϱ�----
xst<-read_excel("allsigungu_update.xlsx",sheet='xst')
xst$sidosigungu <- paste(xst$sido, xst$sigungu)
xst$sudogwon <- ifelse(xst$sido %in% c("����Ư����", "��õ������", "��⵵"), 1, 0)
xst[] <- lapply(xst, function(x) ifelse(is.na(x), 0, x))
xst$totalzone<-xst$ngreenzone+xst$nsocarzone

xst <- xst[!(xst$sigungu %in% c("����Ư����", "����Ư����ġ��")), ]
My_Theme = theme_stata()+theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 13,angle=90,hjust=1),
  axis.title.y = element_text(size = 17))+ theme(legend.position = "none")

My_Theme2 = theme_calc()+theme(
  axis.title.x = element_text(size = 13),
  axis.text.x = element_text(size = 13,angle=90, hjust=1),
  axis.title.y = element_text(size =13))+ theme(legend.position = "none")

#1)���� �ñ��� ����ȭ----
Zeonguk_ordered <- xst[ order(-xst$totalzone), ]
Zeonguk_rank38<-Zeonguk_ordered[c(1:38),]
#����׷���
zeonguk<-ggplot(Zeonguk_rank38, aes(x =  reorder(sidosigungu, -totalzone), y =totalzone ,fill=sido)) +geom_bar(stat="identity",position =position_dodge())+xlab(label = "�ñ���")+ylab(label = "����������")+ggtitle(label = "ī�ξ������(����)") +  My_Theme2
#2)������ �ñ��� ����ȭ----
sudogwon <- xst[xst$sudogwon == 1, ]
sudokwon_ordered <- sudogwon[ order(-sudogwon$totalzone), ]
sudokwon_rank23<-sudokwon_ordered[c(1:23),]
sudokwon_bar<-ggplot(sudokwon_rank23, aes(x =  reorder(sidosigungu, -totalzone), y =totalzone ,fill=sido)) +geom_bar(stat="identity",position =position_dodge())+xlab(label = "�ñ���")+ylab(label = "����������")+ggtitle(label = "ī�ξ������(������)") +  My_Theme2
print(sudokwon_bar)
#3)������� �ñ��� ����ȭ----

bisudogwon <- xst[xst$sudogwon == 0, ]
bisudogwon_ordered <- bisudogwon[ order(-bisudogwon$totalzone), ]
bisudogwon_rank23<-bisudogwon_ordered[c(1:23),]
bisudogwon_rank23[(bisudogwon_rank23$sido %in% c("���ֱ�����")),,]
bisudogwon_bar<-ggplot(bisudogwon_rank23, aes(x =  reorder(sidosigungu, -totalzone), y =totalzone ,fill=sido)) +geom_bar(stat="identity",position =position_dodge())+xlab(label = "�ñ���")+ylab(label = "����������")+ggtitle(label = "ī�ξ������(�������)") +  My_Theme2+geom_hline(yintercept =50, linetype= "longdash", color="red", linewidth=1)
print(bisudogwon_bar)

bisudogwon_under85<-bisudogwon_ordered[c(85:117),]
bisudogwon_under85n_bar<-ggplot(bisudogwon_under85, aes(x =  reorder(sidosigungu, -totalzone), y =totalzone ,fill=sido)) +geom_bar(stat="identity",position =position_dodge())+xlab(label = "�ñ���")+ylab(label = "����������")+ggtitle(label = "ī�ξ������(5���̸�)") +  My_Theme2


ggsave("��������(����).png", zeonguk)
ggsave("��������(������).png", sudokwon_bar)
ggsave("��������(�������).png", bisudogwon_bar)


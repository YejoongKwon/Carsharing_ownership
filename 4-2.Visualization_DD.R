#treat이 모두 있는 시군구만 추린 데이터셋 만들기
baba<-attached_parareg %>%
  group_by(name) %>%
  summarize(unique_values = toString(unique(treat)))

# Counting rows with value 0 in 'column2'
specific_column <- "unique_values"
row_count <- sum(baba[[specific_column]] == 0)
cat("Number of rows with value 0 in", specific_column, ":", row_count, "\n")
#treat 1 이 존재하는 시군구는 총 155개. 나머지 72개는 2020까지 아예 진입 없었던 시군구

###rgldvall의 cityscale별 추이----


graphobjone<-subset(attached_parareg,treat==1)
graphobjone<-graphobjone %>% group_by(cityscale,year_) %>% summarise(MEAN_rgldvall_thousandpc = mean(rgldvall.thousandpc))
graphobjone$treat<-1

graphobjzero<-subset(attached_parareg,treat==0)
graphobjzero<-graphobjzero %>% group_by(cityscale,year_) %>% summarise(MEAN_rgldvall_thousandpc = mean(rgldvall.thousandpc))
graphobjzero$treat<-0

real<-rbind(graphobjzero,graphobjone)
real$year_<-as.Date(real$year_,format='%Y')
pd <- position_dodge(0.1)
real$cityscale<-as.factor(real$cityscale)

#sumzeroone<-attached_parareg %>% group_by(cityscale,year_) %>% summarise(MEAN_rgldvall_thousandpc = mean(rgldvall.thousandpc))

real$year_<-as.Date(real$year_,format='%Y')
pd <- position_dodge(0.1)
real$cityscale<-as.factor(real$cityscale)
real$treat<-as.factor(real$treat)
g0<-ggplot(data = real, aes(x = year_,  y = MEAN_rgldvall_thousandpc))
g1<-g0+geom_line(aes(color=treat,
                       group=interaction(cityscale,treat)), size = 1)+
  theme_economist_white()+ #+ scale_colour_economist()+
  facet_wrap(~cityscale, labeller = label_both,scales = "free_x")+
  geom_vline(xintercept=as.Date("2011-06-30"),color='red',linetype='longdash')+
  xlab('Year')+ylab("승용차 등록대수(대)")+ggtitle("천인당 승용차 등록대수 추이(전체)")+scale_x_date(date_breaks = "2 year", date_labels = "%Y",limits = as.Date(c("2010-01-01", "2020-12-31")))+
  labs(fill = "treat",color="서비스 진입")+
  theme(text=element_text(size=12),legend.text = element_text(size = 16))  # Adjust the font size as desired


ggsave("천인당(전체)등록대수추이.png",g1)

#rgldvprivate
graphobjone<-subset(attached_parareg,treat==1)
graphobjone<-graphobjone %>% group_by(cityscale,year_) %>% summarise(MEAN_rgldvprivate_thousandpc = mean(rgldvall.thousandpc))
graphobjone$treat<-1

graphobjzero<-subset(attached_parareg,treat==0)
graphobjzero<-graphobjzero %>% group_by(cityscale,year_) %>% summarise(MEAN_rgldvprivate_thousandpc = mean(rgldvall.thousandpc))
graphobjzero$treat<-0

real<-rbind(graphobjzero,graphobjone)
real$year_<-as.Date(real$year_,format='%Y')
pd <- position_dodge(0.1)
real$cityscale<-as.factor(real$cityscale)

#sumzeroone<-attached_parareg %>% group_by(cityscale,year_) %>% summarise(MEAN_rgldvprivate_thousandpc = mean(rgldvall.thousandpc))

real$year_<-as.Date(real$year_,format='%Y')
pd <- position_dodge(0.1)
real$cityscale<-as.factor(real$cityscale)
real$treat<-as.factor(real$treat)
g0<-ggplot(data = real, aes(x = year_,  y = MEAN_rgldvprivate_thousandpc))
g1<-g0+geom_line(aes(color=treat,
                     group=interaction(cityscale,treat)), size = 1)+
  theme_economist_white()+ #+ scale_colour_economist()+
  facet_wrap(~cityscale, labeller = label_both,scales = "free_x")+
  geom_vline(xintercept=as.Date("2011-06-30"),color='red',linetype='longdash')+
  xlab('Year')+ylab("승용차 등록대수(대)")+ggtitle("천인당 승용차 등록대수 추이(자가용)")+scale_x_date(date_breaks = "2 year", date_labels = "%Y",limits = as.Date(c("2010-01-01", "2020-12-31")))+
  labs(fill = "treat",color="서비스 진입")+
  theme(text=element_text(size=12),legend.text = element_text(size = 16))  # Adjust the font size as desired

print(g1)
ggsave("천인당(승용차)등록대수추이.png",g1)


####주행거리추이

result <- calculate_means(attached_paravkm, vkm_ldv_private.ht)

g0<-ggplot(data = result, aes(x = year_,  y = MEAN))
g1<-g0+geom_line(aes(color=treat,
                     group=interaction(cityscale,treat)), size = 1)+
  theme_economist_white()+ #+ scale_colour_economist()+
  facet_wrap(~cityscale, labeller = label_both,scales = "free_x")+
  geom_vline(xintercept=as.Date("2011-06-30"),color='red',linetype='longdash')+
  xlab('Year')+ylab("승용차 주행거리(십만 km)")+ggtitle("승용차 주행거리 추이(자가용)")+scale_x_date(date_breaks = "2 year", date_labels = "%Y",limits = as.Date(c("2010-01-01", "2020-12-31")))+
  labs(fill = "treat",color="서비스 진입")+
  theme(text=element_text(size=12),legend.text = element_text(size = 16))  # Adjust the font size as desired
print(g1)


ggsave("자가용 주행거리 추이.png",g1)
rm(result)

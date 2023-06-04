form13<-treat~ oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct+Unemployment#(form12에서 Unemployment 삽입)
reg_paradescribe<-attached_parareg1[,c('rgldvall.thousandpc','rgldvprivate.thousandpc',"oil_realprice","lCityDensity","GRDP.pc","popgrbcpct","citypoppct","Unemployment")]
form7<-treat~ lpop+oil_realprice+lCityDensity+GRDP.pc+popgrbcpct+citypoppct#(현재까지 가장 그럴듯한 결과)
vkm_paradescribe<-attached_paravkm[,c('vkm_ldv_all.ht','vkm_ldv_private.ht',"lpop","oil_realprice","lCityDensity","GRDP.pc","popgrbcpct","citypoppct")]
estadistica1<-do.call("rbind",describe(reg_paradescribe))

colnameofestadistica1<-colnames(reg_paradescribe)
colnames(estadistica1)<-colnameofestadistica1
estadistica_reg <- as.data.frame(round(estadistica1, digits = 2))
reg_stats <- tibble::rownames_to_column(estadistica_reg, "index")


estadistica2<-do.call("rbind",describe(vkm_paradescribe))

colnameofestadistica2<-colnames(vkm_paradescribe)
colnames(estadistica2)<-colnameofestadistica2
estadistica_vkm<-as.data.frame(round(estadistica2,digits=2))

vkm_stats <- tibble::rownames_to_column(estadistica_vkm, "index")
#write.xlsx(reg_stats,"reg_stats.xlsx")
#write.xlsx(vkm_stats,"vkm_stats.xlsx")

mytable(form13,data = attached_parareg1,digits = 5)
mytable(form7,data = attached_paravkm,digits = 4)


'ggplot(data = attached_parareg, aes(x = cityscale, y = GRDP)) + 
  geom_boxplot(outlier.shape = NA)  +
  labs(x = "도시화정도", y = "GRDP(백만원)") +
  ggtitle("도시화 정도와 GRDP")+ylim(0,30000000)+theme_economist()+theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))
# Pop+oil_realprice+CityDensity+GRDP.pc+popgrbc+citypopratio+Unempperthous
'

listY <- c('rgldvall.thousandpc','rgldvprivate.thousandpc','oil_realprice', 'GRDP.pc', 'CityDensity', 'popgrbcpct','citypoppct', 'Unempperthous')
listYname<-c('천인당전체차량등록대수','천인당자가용등록대수','휘발유가격','인당GRDP','인구밀도','30~50대인구비중','도시지역인구비중','천인당실업자수')
plots <- list()
cityscale_levels <- c("rural", "avg", "urban")
attached_parareg$cityscale <- factor(attached_parareg$cityscale, levels = cityscale_levels)

# loop through each y variable and create a plot
for (i in 1:length(listY)) {
  yvar <- listY[i]
  q <- quantile(attached_parareg[[yvar]], c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lim <- c(q[1] - 1.5 * iqr, q[2] + 1.5 * iqr)
  plot <- ggplot(data = attached_parareg, aes(x = cityscale, y = !!sym(yvar))) +
    geom_boxplot(outlier.shape = NA) +
    labs(x = "도시화정도", y = listYname[match(listY[i], listY)]) +
    #labs(x = "도시화정도", y = yvar) +
    #ggtitle(paste0("도시화 정도와 ", yvar))+
    ylim(lim[1], lim[2])+
    #ylim(0, 30000000) +
    theme_bw()+
    scale_color_stata()  #theme(
  #  axis.title.x = element_text(size = 20),
  #   axis.text.x = element_text(size = 20),
  #   axis.title.y = element_text(size = 20)
  #)
  plots[[i]] <- plot
}
Boxplot_bycityscale<-gridExtra::grid.arrange(
  grobs = lapply(plots, ggplotGrob),
  ncol = 2,
  top = "Boxplots of y variables by city scale",
  bottom = "Source: attached_parareg dataset"
) + facet_grid(. ~ yvar)

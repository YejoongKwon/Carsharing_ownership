
listY <- c('rgldvall.thousandpc','rgldvprivate.thousandpc','oil_realprice', 'GRDP.pc', 'CityDensity', 'popgrbcpct','citypoppct', 'Unempperthous')
listYname<-c('천인당전체차량등록대수','천인당자가용등록대수','휘발유가격','인당GRDP','인구밀도','30~50대인구비중','도시지역인구비중','천인당실업자수')
plots <- list()


cityscale_levels <- c("rural", "avg", "urban")
attached_parareg$cityscale <- factor(attached_parareg$cityscale, levels = cityscale_levels)

# loop through each y variable and create a plot
for (i in 1:length(listY)) {
  yvar <- listY[i]
  q <- quantile(avail_parareg[[yvar]], c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lim <- c(q[1] - 1.5 * iqr, q[2] + 1.5 * iqr)
  plot <- ggplot(data = avail_parareg, aes(x = cityscale, y = !!sym(yvar))) +
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
  top = "Boxplots of y variables by cityscale",
  bottom = "Source: avail_parareg dataset"
) + facet_grid(. ~ yvar)

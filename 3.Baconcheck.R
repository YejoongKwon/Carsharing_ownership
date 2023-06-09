calculate_means <- function(data, variable) {
  graphobj_one <- subset(data, treat == 1)
  graphobj_one <- graphobj_one %>%
    group_by(cityscale, year_) %>%
    summarise(MEAN = mean({{ variable }}))
  graphobj_one$treat <- 1
  
  graphobj_zero <- subset(data, treat == 0)
  graphobj_zero <- graphobj_zero %>%
    group_by(cityscale, year_) %>%
    summarise(MEAN = mean({{ variable }}))
  graphobj_zero$treat <- 0
  
  real <- rbind(graphobj_zero, graphobj_one)
  real$year_ <- as.Date(real$year_, format = '%Y')
  real$cityscale <- as.factor(real$cityscale)
  real$treat<-as.factor(real$treat)
  return(real)
}

calculate_accessiblemeans <- function(data, variable) {
  graphobj_one <- subset(data, treat == 1)
  graphobj_one <- graphobj_one %>%
    group_by(availiability, year_) %>%
    summarise(MEAN = mean({{ variable }}))
  graphobj_one$treat <- 1
  
  graphobj_zero <- subset(data, treat == 0)
  graphobj_zero <- graphobj_zero %>%
    group_by(availiability, year_) %>%
    summarise(MEAN = mean({{ variable }}))
  graphobj_zero$treat <- 0
  
  real <- rbind(graphobj_zero, graphobj_one)
  real$year_ <- as.Date(real$year_, format = '%Y')
  real$availiability <- as.factor(real$availiability)
  real$treat<-as.factor(real$treat)
  return(real)
}
result <- calculate_means(attached_paravkm, vkm_ldv_all.ht)

g0<-ggplot(data = result, aes(x = year_,  y = MEAN))
g1<-g0+geom_line(aes(color=treat,
                     group=interaction(cityscale,treat)), size = 1)+
  theme_economist_white()+ #+ scale_colour_economist()+
  facet_wrap(~cityscale, labeller = label_both,scales = "free_x")+
  geom_vline(xintercept=as.Date("2011-06-30"),color='red',linetype='longdash')+
  xlab('Year')+ylab("승용차 주행거리(십만 km)")+ggtitle("승용차 주행거리 추이(전체))")+scale_x_date(date_breaks = "2 year", date_labels = "%Y",limits = as.Date(c("2010-01-01", "2020-12-31")))+
  labs(fill = "treat",color="서비스 진입")+
  theme(text=element_text(size=12),legend.text = element_text(size = 16))  # Adjust the font size as desired
print(g1)


#ggsave("주행거리 추이(접근성,자가용).png",g1)
# Sample data: Population by region by year (long-form)
population_long <- data.frame(
  Year = c(2018, 2018, 2018, 2019, 2019, 2019, 2020, 2020, 2020, 2021, 2021, 2021),
  Region = c("Region1", "Region2", "Region3", "Region1", "Region2", "Region3", "Region1", "Region2", "Region3", "Region1", "Region2", "Region3"),
  Population = c(1000, 800, 500, 1200, 900, 550, 1500, 950, 600, 1800, 1100, 700)
)

# Calculate population growth rate by year
population_growth_long <- population_long

population_growth_long$GrowthRate <- unlist(by(population_long, population_long$Region, function(x) {
  c(0, diff(x$Pop) / x$Pop[-length(x$Pop)] * 100)
}))
warnings()
library(bacondecomp)
data(math_reform)
df_bacon <- bacon(incearn_ln ~ reform_math,
                  data = bacondecomp::math_reform,
                  id_var = "state",
                  time_var = "class")
ggplot(df_bacon) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Weight", y = "Estimate", shape = "Type")
attached_parareg1$treat<-as.logical(as.integer(attached_parareg1$treat))
df_bacon <- bacon(rgldvall.thousandpc ~ treat,
                  data = attached_parareg,
                  id_var = "id",
                  time_var = "year_")

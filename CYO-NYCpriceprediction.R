# REQUIRED PACKAGES

if(!require(tidyverse)) 
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) 
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(scales)) 
  install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) 
  install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) 
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(broom)) 
  install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(stringr)) 
  install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(gam)) 
  install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(rpart)) 
  install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) 
  install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(readr)) 
  install.packages("readr", repos = "http://cran.us.r-project.org")

#DATA PREPARATION

nyc_rolling_sales <- read_csv("https://raw.githubusercontent.com/sergiowalias/Edx-Choose-your-own.-NYC/main/nyc-rolling-sales.csv")
View(nyc_rolling_sales)
#DATA FRAME
nyc_clean<-as.data.frame(nyc_rolling_sales)

#clean NA data & convert to numeric
nyc_clean$`SALE PRICE`<-as.numeric(nyc_clean$`SALE PRICE`)
nyc_clean<-nyc_clean%>%filter(!is.na(`SALE PRICE`))
nyc_clean$`GROSS SQUARE FEET`<-as.numeric(nyc_clean$`GROSS SQUARE FEET`)
nyc_clean<-nyc_clean%>%filter(!is.na(`GROSS SQUARE FEET`))%>%filter(!`GROSS SQUARE FEET`==0)
nyc_clean$`LAND SQUARE FEET`<-as.numeric(nyc_clean$`LAND SQUARE FEET`)
nyc_clean<-nyc_clean%>%filter(!is.na(`LAND SQUARE FEET`))%>%filter(!`LAND SQUARE FEET`==0)

#eliminate not usfeul vectors and transform the useful ones
## Missing data
nyc_clean<-nyc_clean%>%select(-`EASE-MENT`,-`APARTMENT NUMBER`)
## Building class. Repeated data
nyc_clean<-nyc_clean%>%select(-`BUILDING CLASS CATEGORY`,-`BUILDING CLASS AT PRESENT`)
nyc_clean<-nyc_clean%>%mutate(building_class=str_extract(`BUILDING CLASS AT TIME OF SALE`,"^[A-Z]"))
nyc_clean<-nyc_clean%>%select(-`BUILDING CLASS AT TIME OF SALE`)

## Building class with proportions
sum(is.na(nyc_clean$`TOTAL UNITS`))
sum(nyc_clean$`TOTAL UNITS`==0)
nyc_clean_0<-nyc_clean%>%filter(nyc_clean$`TOTAL UNITS`==0)%>%group_by(building_class)%>%summarize(n=n())
nyc_clean_0
rm(nyc_clean_0)
nyc_clean<-nyc_clean%>%mutate(com_perce=ifelse(`TOTAL UNITS`==0,0,`COMMERCIAL UNITS`/`TOTAL UNITS`))
nyc_clean<-nyc_clean%>%mutate(res_perce=ifelse(`TOTAL UNITS`==0,0,`RESIDENTIAL UNITS`/`TOTAL UNITS`))
nyc_clean<-nyc_clean%>%select(-`RESIDENTIAL UNITS`,-`COMMERCIAL UNITS`,-`TOTAL UNITS`)
nyc_clean<-nyc_clean%>%filter(res_perce<=1)
nyc_clean<-nyc_clean%>%filter(com_perce<=1)

## Tax class. Repeated data
nyc_clean<-nyc_clean%>%select(-`TAX CLASS AT PRESENT`)
nyc_clean<-nyc_clean%>%mutate(tax_class=`TAX CLASS AT TIME OF SALE`)
nyc_clean<-nyc_clean%>%select(-`TAX CLASS AT TIME OF SALE`)

## Unuseful data
nyc_clean<-nyc_clean%>%select(-ADDRESS,-`ZIP CODE`,-LOT,-BLOCK,-X1,-NEIGHBORHOOD)

## Dates
which(is.na(nyc_clean$`SALE DATE`))
which(nyc_clean$`SALE DATE`<2016-09-01)
# Transform the data to avoid spaces
nyc_clean<-nyc_clean%>%mutate(sale_date=`SALE DATE`)%>%select(-`SALE DATE`)

## Year built: eliminate not usfeul data.
plot(nyc_clean$`YEAR BUILT`,nyc_clean$`SALE PRICE`)
nyc_clean<-nyc_clean%>%filter(`YEAR BUILT`>0)
plot(nyc_clean$`YEAR BUILT`,nyc_clean$`SALE PRICE`)
nyc_clean<-nyc_clean%>%filter(`YEAR BUILT`>1900)
nyc_clean<-nyc_clean%>%mutate(year_built=`YEAR BUILT`)%>%select(-`YEAR BUILT`)
plot(nyc_clean$year_built,nyc_clean$`SALE PRICE`)

#Prices
nyc_clean_price<-nyc_clean%>%group_by(`SALE PRICE`)%>%summarize(n=n())%>%arrange(-n)
nyc_clean_price
rm(nyc_clean_price)
# elimination of non representative prices
nyc_clean<-nyc_clean%>%filter(`SALE PRICE`>10)

# Transformation of variables. 
nyc_clean<-nyc_clean%>%mutate(price_lsf=`SALE PRICE`/`LAND SQUARE FEET`)
nyc_clean<-nyc_clean%>%mutate(FAR=`GROSS SQUARE FEET`/`LAND SQUARE FEET`)%>%select(-`LAND SQUARE FEET`,-`GROSS SQUARE FEET`,-`SALE PRICE`)

#DATA SPLITTING
# 'test' set will be 10% of working data
set.seed(2)
test_index <- createDataPartition(y = nyc_clean$price_lsf, times = 1, p = 0.1, list = FALSE)
nyc_train <- nyc_clean[-test_index,]
nyc_temp <- nyc_clean[test_index,]

# Make sure classes(borough, tax, building) in 'test' set are also in 'train' set
nyc_test <- nyc_temp %>% 
  semi_join(nyc_train, by = "building_class") %>%
  semi_join(nyc_train, by = "tax_class")%>%
  semi_join(nyc_train, by = "BOROUGH")

# Add rows removed from 'test' set back into 'train' set
removed <- anti_join(nyc_temp, nyc_test)
nyc_train <- rbind(nyc_train, removed)
rm(test_index, nyc_temp, removed)

#DATA EXPLORATION: YEAR BUILT
nyc_train_year<-nyc_train%>%group_by(year_built)%>%mutate(avg=mean(`price_lsf`))
plot(nyc_train_year$year_built,nyc_train_year$`avg`)
rm(nyc_train_year)
cor(nyc_train$year_built, nyc_train$price_lsf)
nyc_train%>%ggplot(aes(x=year_built,y=price_lsf))+geom_point()+geom_boxplot()+scale_y_log10()

#DATA EXPLORATION: FAR
plot(nyc_train$FAR,nyc_train$price_lsf)
cor(nyc_train$FAR,nyc_train$price_lsf)
cor(nyc_train$FAR,nyc_train$price_lsf,method = "spearman")

#DATA EXPLORING: SALE DATE.
nyc_train_year<-nyc_train%>%group_by(sale_date)%>%mutate(avg=mean(`price_lsf`))
plot(nyc_train_year$sale_date,nyc_train_year$`avg`)
rm(nyc_train_year)
nyc_train_year<-nyc_train%>%mutate(sale_date = as.numeric(sale_date))
cor(nyc_train_year$sale_date, nyc_train_year$price_lsf)
rm(nyc_train_year)

#DATA EXPLORING: BOROUGH.
ggplot(data=nyc_train)+geom_smooth(mapping=aes(x=year_built,y=price_lsf,group=as.factor(BOROUGH),
                                               color=as.factor(BOROUGH)))+theme(legend.position="bottom")
ggplot(data=nyc_train)+geom_smooth(mapping=aes(x=sale_date,y=price_lsf,group=as.factor(BOROUGH),
                                               color=as.factor(BOROUGH)))+theme(legend.position="bottom")
nyc_train%>%group_by(BOROUGH)%>%
  ggplot(aes(x=as.factor(BOROUGH),y=price_lsf, group=as.factor(BOROUGH),color=as.factor(BOROUGH)))+
  geom_point()+geom_boxplot()+scale_y_log10()+theme(legend.position="bottom")

# Correlations by boroughs
nyc_train_bor<-nyc_train%>%group_by(BOROUGH)%>%summarize(r = cor(year_built, price_lsf,method="spearman"))
nyc_train_bor
rm(nyc_train_bor)
nyc_train_bor<-nyc_train%>%group_by(BOROUGH)%>%summarize(r = cor(as.numeric(sale_date), price_lsf,method="spearman"))
nyc_train_bor
rm(nyc_train_bor)

#STUDY BY BOROUGHS
nyc_train_25<-nyc_train%>%filter(BOROUGH!=1)
nyc_train_1<-nyc_train%>%filter(BOROUGH=="1")
#Graphics by BOROUGHS
ggplot(data=nyc_train_25)+geom_smooth(mapping=aes(x=year_built,y=price_lsf,group=as.factor(BOROUGH),
                                                  color=as.factor(BOROUGH)))+theme(legend.position="bottom")
ggplot(data=nyc_train_25)+geom_smooth(mapping=aes(x=sale_date,y=price_lsf,group=as.factor(BOROUGH),
                                                  color=as.factor(BOROUGH)))+theme(legend.position="bottom")
ggplot(data=nyc_train_1)+geom_smooth(mapping=aes(x=year_built,y=price_lsf,group=as.factor(BOROUGH),
                                                  color=as.factor(BOROUGH)))+theme(legend.position="bottom")
ggplot(data=nyc_train_1)+geom_smooth(mapping=aes(x=sale_date,y=price_lsf,group=as.factor(BOROUGH),
                                                  color=as.factor(BOROUGH)))+theme(legend.position="bottom")
rm(nyc_train_1)
rm(nyc_train_25)

#DATA EXPLORING: TAX CLASS.
ggplot(data=nyc_train)+geom_smooth(mapping=aes(x=year_built,y=price_lsf,group=as.factor(tax_class),
                                               color=as.factor(tax_class)))+theme(legend.position="bottom")
nyc_train_tax<-nyc_train%>%group_by(tax_class)%>%summarize(r = cor(as.numeric(year_built), price_lsf,method="spearman"))
nyc_train_tax
rm(nyc_train_tax)
nyc_train%>%group_by(tax_class)%>%
  ggplot(aes(x=tax_class,y=price_lsf, group=as.factor(tax_class),color=as.factor(tax_class)))+
  geom_point()+geom_boxplot()+scale_y_log10()+theme(legend.position="bottom")

#DATA EXPLORING: BUILDING CLASS. CATEGORICAL
nyc_train%>%group_by(building_class)%>%
  ggplot(aes(x=building_class,y=price_lsf, group=as.factor(building_class),color=as.factor(building_class)))+
  geom_point()+geom_boxplot()+scale_y_log10()+theme(legend.position="bottom")
nyc_train_bc<-nyc_train%>%group_by(building_class)%>%summarize(number= n())
nyc_train_bc
nyc_train_bc<-as.data.frame(nyc_train_bc)
rm(nyc_train_bc)
nyc_train_bc<-nyc_train%>%group_by(building_class)%>%summarize(r = cor(as.numeric(year_built), price_lsf,method="spearman"))
nyc_train_bc%>%arrange(r)
nyc_train_bc%>%arrange(-r)
rm(nyc_train_bc)

#DATA EXPLORING: BUILDING CLASS. CONTINUOUS.
plot(nyc_train$res_perce,nyc_train$price_lsf)
cor(nyc_train$res_perce,nyc_train$price_lsf,method = "spearman")
plot(nyc_train$com_perce,nyc_train$price_lsf)
cor(nyc_train$com_perce,nyc_train$price_lsf,method = "spearman")

# DIRECT LINEAL MODEL (1)
## Only FAR lineal model
fit_11<-lm(price_lsf~FAR,data=nyc_train)
summary(fit_11)
y_test_11<-predict(fit_11,nyc_test)
RMSE_test_n11<-sqrt(mean((y_test_11 - nyc_test$price_lsf)^2))
RMSE_test_n11
RMSE_linear<-tibble(Method="Linear FAR",RMSE=RMSE_test_n11)

## All-included model
fit_12<-lm(price_lsf~FAR+as.factor(BOROUGH)+as.factor(building_class)+
            as.factor(tax_class)+year_built+com_perce+res_perce,data=nyc_train)
summary(fit_12)
y_test_12<-predict(fit_12,nyc_test)
RMSE_test_n12<-sqrt(mean((y_test_12 - nyc_test$price_lsf)^2))
RMSE_test_n12
RMSE_linear<-bind_rows(RMSE_linear,tibble(Method="Linear all-included",RMSE=RMSE_test_n12))

## Commercial and residential not included.
fit_13<-lm(price_lsf~FAR+as.factor(BOROUGH)+as.factor(building_class)+
             as.factor(tax_class)+year_built,data=nyc_train)
summary(fit_13)
y_test_13<-predict(fit_13,nyc_test)
RMSE_test_n13<-sqrt(mean((y_test_13 - nyc_test$price_lsf)^2))
RMSE_test_n13
RMSE_linear<-bind_rows(RMSE_linear,tibble(Method="Linear without com & res",RMSE=RMSE_test_n13))

## FAR, BOROUGH and classes.
fit_14<-lm(price_lsf~FAR+as.factor(BOROUGH)+as.factor(building_class)+
             as.factor(tax_class),data=nyc_train)
summary(fit_14)
y_test_14<-predict(fit_14,nyc_test)
RMSE_test_n14<-sqrt(mean((y_test_14 - nyc_test$price_lsf)^2))
RMSE_test_n14
RMSE_linear<-bind_rows(RMSE_linear,tibble(Method="Linear FAR, BOROUGH, tax, building",RMSE=RMSE_test_n14))

## FAR and Borough
fit_15<-lm(price_lsf~FAR+as.factor(BOROUGH),data=nyc_train)
summary(fit_15)
y_test_15<-predict(fit_15,nyc_test)
RMSE_test_n15<-sqrt(mean((y_test_15 - nyc_test$price_lsf)^2))
RMSE_test_n15
RMSE_linear<-bind_rows(RMSE_linear,tibble(Method="Linear FAR and BOROUGH",RMSE=RMSE_test_n15))

## FAR, Borough and building class.
fit_16<-lm(price_lsf~FAR+as.factor(BOROUGH)+as.factor(building_class),data=nyc_train)
summary(fit_16)
y_test_16<-predict(fit_16,nyc_test)
RMSE_test_n16<-sqrt(mean((y_test_16 - nyc_test$price_lsf)^2))
RMSE_test_n16
RMSE_linear<-bind_rows(RMSE_linear,tibble(Method="Linear FAR BOROUGH and building",RMSE=RMSE_test_n16))

  
## Summary (linears)
RMSE_linear
RMSE_test_1<-RMSE_test_n16

## Summary linear. Graphics.
nyc_test%>%mutate(model_1=y_test_11,model_2=y_test_15,model_3=y_test_16)%>%
  ggplot(aes(x=FAR, y=price_lsf, group=as.factor(BOROUGH), 
             color=as.factor(BOROUGH)))+geom_point(size=0.5)+
  scale_y_log10()+scale_x_log10()+theme(legend.position="bottom")+
  geom_line(aes(y=y_test_11),size=1)
#geom_line(aes(y=y_test_15),size=1)
#geom_line(aes(y=y_test_16),size=1)

nyc_test%>%mutate(model_1=y_test_11,model_2=y_test_15,model_3=y_test_16)%>%
  ggplot(aes(x=FAR, y=price_lsf, group=as.factor(BOROUGH), 
             color=as.factor(BOROUGH)))+geom_point(size=0.5)+
  scale_y_log10()+scale_x_log10()+theme(legend.position="bottom")+
 #geom_line(aes(y=y_test_11),size=1)
 geom_line(aes(y=y_test_15),size=1)
#geom_line(aes(y=y_test_16),size=1)

nyc_test%>%mutate(model_1=y_test_11,model_2=y_test_15,model_3=y_test_16)%>%
  ggplot(aes(x=FAR, y=price_lsf, group=as.factor(BOROUGH), 
             color=as.factor(BOROUGH)))+geom_point(size=0.5)+
  scale_y_log10()+scale_x_log10()+theme(legend.position="bottom")+
  #geom_line(aes(y=y_test_11),size=1)
  #geom_line(aes(y=y_test_15),size=1)
  geom_line(aes(y=y_test_16),size=1)


rm(RMSE_test_n12, RMSE_test_n13,RMSE_test_n14,RMSE_test_n15)
rm(fit_11,fit_12,fit_13,fit_14,fit_15)


#LOESS MODEL (2)
# FAR
grid <- expand.grid(span = seq(0.15, 0.85, len = 15), degree = 1)
fit_2<-train(price_lsf ~ FAR, method = "gamLoess", 
             data = nyc_train, tuneGrid = grid)
y_test_2<-predict(fit_2,nyc_test)
ggplot(fit_2)
fit_2$bestTune
RMSE_test_2<-sqrt(mean((y_test_2 - nyc_test$price_lsf)^2))
RMSE_test_2

# Graphic
nyc_test%>%mutate(model_loess=y_test_2)%>%
  ggplot(aes(x=FAR, y=price_lsf, group=as.factor(BOROUGH), 
             color=as.factor(BOROUGH)))+geom_point(size=0.5)+
  scale_y_log10()+scale_x_log10()+theme(legend.position="bottom")+
  geom_line(aes(y=y_test_2),size=1)

#KNN MODEL- CARET TRAIN (3)
fit_3<-train(price_lsf ~ FAR+as.factor(BOROUGH)+as.factor(building_class), method = "knn", 
             data = nyc_train, tuneGrid = data.frame(k = seq(5, 25, 0.5)))
plot(fit_3)
y_test_3<-predict(fit_3,nyc_test,type="raw")
RMSE_test_3<-sqrt(mean((y_test_3 - nyc_test$price_lsf)^2))
RMSE_test_3

#Graphic
nyc_test%>%mutate(model_knn=y_test_3)%>%
  ggplot(aes(x=FAR, y=price_lsf, group=as.factor(BOROUGH), 
             color=as.factor(BOROUGH)))+geom_point(size=0.5)+
  scale_y_log10()+scale_x_log10()+theme(legend.position="bottom")+
  geom_line(aes(y=y_test_3),size=1)

# RPART (4)
fit_4 <- train(price_lsf ~ FAR+as.factor(BOROUGH)+as.factor(building_class), method = "rpart", 
                     tuneGrid = data.frame(cp = seq(0, 0.01, len = 25)), data = nyc_train)
plot(fit_4)
rpart.plot(fit_4$finalModel)
y_test_4<-predict(fit_4,nyc_test,type="raw")
RMSE_test_n4<-sqrt(mean((y_test_4 - nyc_test$price_lsf)^2))
RMSE_test_n4

#Graphic
nyc_test%>%mutate(model_tree=y_test_4)%>%
ggplot(aes(x=FAR, y=price_lsf, group=as.factor(BOROUGH), 
           color=as.factor(BOROUGH)))+geom_point(size=0.5)+
  scale_y_log10()+scale_x_log10()+theme(legend.position="bottom")+
  geom_line(aes(y=y_test_4),size=1)


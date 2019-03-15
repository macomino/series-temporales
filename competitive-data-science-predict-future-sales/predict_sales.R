library(lubridate)
library(tseries)
library(dplyr)

setwd("C:/Users/barde/Nextcloud/master/Documentacion/Semana 12 - Series temporales/competitive-data-science-predict-future-sales")

data<-read_csv("sales_train_v2.csv")


head(data)





data$fechareal <- strptime(data$date, "%d.%m.%Y")

head(data)



data <- data %>% 
  mutate(new.date=new.date, week(new.date), month=month(new.date))


items <- sort(table(data$item_id), decreasing=T)[1:10]

n.items.per.shop.per.month <- data %>%
  group_by(month, shop_id, item_id, item_price) %>% 
             summarise(n = n()) %>% 
  filter(shop_id %in% seq(1,10)) %>% 
  filter(item_id %in% names(items)) %>% 
  summarise(cost = crossprod(item_price, n))



             


dataset <- data %>% 
  select(month(fechareal), shop_id, item_id, item_price) %>% 
  group_by(fechareal, shop_id, item_id, item_price)


head(dataset)

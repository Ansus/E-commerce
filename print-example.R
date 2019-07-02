

library(ggplot2)
library(funModeling)
library(tidyverse)


dat = read.csv("data.csv", header = TRUE)

basic_eda <- function(data)
{
  glimpse(data)
  df_status(data)
  freq(data) 
  profiling_num(data)
  plot_num(data)
  describe(data)
}

basic_eda(dat)

glimpse(dot)
df_status(dot)
freq(dot)
profiling_num(dot)
plot_num(dot)
describe(dot)


dat2= dat %>%  select(UnitPrice, Quantity)
plot(dat2, xlim= c(1, 20000), ylim=c(0,10))

dat2 %>% filter(UnitPrice>0) %>% plot(xlim= c(0, 20000), ylim=c(0,100))


dat2= dat %>% mutate(StockCode= toupper(StockCode))

dat %>% count(dat$StockCode) %>% arrange(-n)
dat2 %>% count(dat2$StockCode) %>% arrange(-n)

dot= dat %>%  filter(toupper(StockCode)=='DOT') %>% 
  filter(!is.na(CustomerID))

dot2= dat %>%  filter(CustomerID==14096)


ggplot(data=dat4, aes(dat4$UnitPrice, dat4$Quantity)) +
  geom_point()

dat3= dat %>% filter(UnitPrice< -1000 | UnitPrice > 20000
                     | Quantity> 10000 | Quantity< -50000)

dat4= dat %>% filter(UnitPrice> -1000 & UnitPrice < 20000
                     & Quantity< 10000 & Quantity> -50000)


dat4 %>% 
  filter(Quantity<3000 & Quantity>0 & UnitPrice>0 & UnitPrice<5000) %>% 
  ggplot(.,aes(.$UnitPrice, .$Quantity)) +
  geom_point()


glimpse(dat2)
df_status(dat2)
freq(dat2)
profiling_num(dat2)
plot_num(dat2)
describe(dat2)

fivenum(dat2$UnitPrice)

rm(dat5)
dat5= dat2 %>% transmute(UnitPrice= rank(UnitPrice, ties='first'), Quantity= rank(Quantity, ties='first'))
dat5 %>% ggplot(.,aes(.$UnitPrice, .$Quantity)) + geom_point()


boxplot(dat$Quantity)
boxplot()



                                    
  
df_status(dat6)

dat7= dat6 %>% group_by(InvoiceDate) %>% 
  summarise(Value=sum(Quantity*UnitPrice))

ggplot(dat7, aes(InvoiceDate, Value)) +
  geom_bar(stat = "identity")



dat_less0= dat2 %>% filter(UnitPrice<0)
dat_11k= dat2 %>% filter(grepl("A|B|C",InvoiceNo))
dat_2k= dat2 %>% filter(Quantity*UnitPrice<0)

tab_PriceQuant=dat2 %>% group_by(UnitPrice) %>% 
  summarise(Quantity=sum(Quantity))


tab_PriceQuant %>% 
  ggplot(aes(UnitPrice, Quantity)) +
  geom_point()

dat2 %>% filter(UnitPrice<=25) %>% 
ggplot( aes(UnitPrice)) +
  geom_histogram(binwidth = 1)





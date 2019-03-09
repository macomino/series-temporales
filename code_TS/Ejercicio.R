?visitors
plot(visitors,main="Overseas visitors to Australia",ylab="Thousands of people",xlab="Year")
autoplot(visitors)

BoxCox.lambda(visitors)

transformVisitors <- BoxCox(visitors, lambda = 0.33) # Ponemos 0.33 xq es la raiz cubica y es mas facil de explicar
autoplot(transformVisitors)


transformVisitors %>% diff(1) %>% autoplot()

library(tseries)
adf.test(transformVisitors)


transformVisitors %>% diff(1) %>% diff(6) %>% autoplot()


y<- rnorm(100,1,1)
z<-2-0.8*y+rnorm(100,0,1)
plot(z)

autoplot(ts(y))

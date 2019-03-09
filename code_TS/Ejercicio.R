?visitors
plot(visitors,main="Overseas visitors to Australia",ylab="Thousands of people",xlab="Year")
autoplot(visitors)

BoxCox.lambda(visitors)

transformVisitors <- BoxCox(visitors, lambda = 0.2775249)
autoplot(transformVisitors)

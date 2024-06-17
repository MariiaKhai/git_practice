library(plotly)

#Normal Distribution
z<-seq(-3.5,3.5,0.1)  # 71 points from -3.5 to 3.5 in 0.1 steps
q<-seq(0.001,0.999,0.001)  # 1999 points from 0.1% to 99.9% on 0.1% steps
dStandardNormal <- data.frame(Z=z, 
                              Density=dnorm(z, mean=0, sd=1),
                              Distribution=pnorm(z, mean=0, sd=1))  
dnorm(2, mean=0, sd=1)
pnorm(1, mean=0, sd=1)

qStandardNormal <- data.frame(Q=q, 
                              Quantile=qnorm(q, mean=0, sd=1))  

qnorm(0.8413447, mean=0, sd=1)

dStandardNormal
qStandardNormal

#https://plotly.com/r/line-charts/
fig<-plot_ly(dStandardNormal, x=~z, y=~Density, 
             name='Density', 
             mode = 'lines', 
             type = 'scatter',
             line = list(color = 'rgb(22, 96, 167)', width = 4))
fig<-fig %>% add_lines(y=~Distribution, 
                       name='Distribution',
                       mode = 'lines',
                       line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'))
fig<-fig %>% layout(title='Normal distribution',
  paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
  xaxis = list(title = "Months",
              gridcolor = 'rgb(255,255,255)',
              showgrid = TRUE,
              showline = FALSE,
              showticklabels = TRUE,
              tickcolor = 'rgb(127,127,127)',
              ticks = 'outside',
              zeroline = FALSE),
  
  yaxis = list(title = "y",
             gridcolor = 'rgb(255,255,255)',
             showgrid = TRUE,
             showline = FALSE,
             showticklabels = TRUE,
             tickcolor = 'rgb(127,127,127)',
             ticks = 'outside',
             zeroline = FALSE))
fig
plot(qStandardNormal$Quantile)

#Poisson Distribution
lower<-qpois(0.001, lambda=2.5)
upper<-qpois(0.999, lambda=2,5)
n<-seq(lower,upper,1)
q<-seq(0.001,0.999,0.001)
dPoisson25 <- data.frame(N=n, 
                         Density=dpois(n, lambda=2.5),
                         Distribution=ppois(n, lambda=2.5))  
qPoisson25 <- data.frame(Q=q, Quantile=qpois(q, lambda=2.5))  
dPoisson25

fig<-plot_ly(dPoisson25, x=~n, y=~Density, name='Density', mode = 'lines', type = 'scatter')
fig<-fig %>% add_trace(y=~Distribution, name='Distribution',mode = 'lines')
fig

plot(qPoisson25$Quantile)

#Binomial Distribution
lower<-qbinom(0.001, size=100, prob=0.5)
upper<-qbinom(0.999, size=100, prob=0.5)
n<-seq(lower,upper,1)
q<-seq(0.001,0.999,0.001)
dBinom100 <- data.frame(N=n, 
                        Density=dbinom(n, size=100, prob=0.5),
                        Distribution=pbinom(n, size=100, prob=0.5))  
qBinom100 <- data.frame(Q=q, Quantile=qbinom(q, size=100, prob=0.5))  
dBinom100

fig<-plot_ly(dBinom100, x=~n, y=~Density, name='Density', mode = 'lines', type = 'scatter')
fig<-fig %>% add_trace(y=~Distribution, name='Distribution',mode = 'lines')
fig

plot(qBinom100$Quantile)

#Exponential Distribution
lower <- floor(qexp(0.001, rate=0.2))
upper <- ceiling(qexp(0.999, rate=0.2))
t <- seq(lower,upper,0.1)
q <- seq(0.001,0.999,0.001)
dexp02 <- data.frame(T=t, 
                     Density=dexp(t, rate=0.2),
                     Distribution=pexp(t, rate=0.2))  
qexp02 <- data.frame(Q=q, Quantile=qexp(q, rate=0.2))  
dexp02

fig<-plot_ly(dexp02, x=~t, y=~Density, name='Density', mode = 'lines', type = 'scatter')
fig<-fig %>% add_trace(y=~Distribution, name='Distribution',mode = 'lines')
fig

plot(qexp02$Quantile)

#Chi2 Distribution
lower <- floor(qchisq(0.001, df=10))
upper <- ceiling(qchisq(0.999, df=10))
x <- seq(lower,upper,0.1)
q <- seq(0.001,0.999,0.001)
dchisq10 <- data.frame(X=x, 
                       Density=dchisq(x, df=10),
                       Distribution=pchisq(x, df=10))  
qchisq10 <- data.frame(Q=q, Quantile=qchisq(q, df=10))  
head(dchisq10)
fig<-plot_ly(dchisq10, x=~x, y=~Density, name='Density', mode = 'lines', type = 'scatter')
fig<-fig %>% add_trace(y=~Distribution, name='Distribution',mode = 'lines')
fig

plot(qchisq10$Quantile)

#SEM for different distributions
set.seed(123) # for reproducibility
sample_data <- rnorm(n = 20, mean = 50, sd = 10)
sem<-sd(sample_data)/sqrt(length(sample_data))
sem
per_sem<-sem/mean(sample_data)*100
per_sem

set.seed(123) # for reproducibility
sample_data <- sample(0:1, 300, replace=T, prob=c(.80,.20))
sample_data
sem<-sd(sample_data)/sqrt(length(sample_data))
sem
per_sem<-sem/mean(sample_data)*100
per_sem



# Generate a random sample of 50 observations from a normal distribution with mean 12 and standard deviation 3
set.seed(123) # for reproducibility
sample_data <- rnorm(n = 10, mean = 297, sd = 2)

# Specify the null hypothesis and alternative hypothesis
# Ho: population mean >= 300
# Ha: population mean < 300
null_mean <- 298

# Calculate the sample mean, sample standard deviation, and standard error of the mean
sample_mean <- mean(sample_data)
sample_sd <- sd(sample_data)
sem <- sample_sd / sqrt(length(sample_data))
#sem <- 2 / sqrt(10)

# Calculate the test statistic and p-value
test_statistic <- (sample_mean - null_mean) / sem
p_value <- pnorm(test_statistic)

# Set the significance level
alpha <- 0.05

# Determine the critical value based on the mean and std
critical_value <- qnorm(alpha, mean = 300, sd = 2, lower.tail = TRUE)

# Make the decision and conclusion
if (sample_mean > critical_value) {
  print(paste('Reject the null hypothesis. The sample provides evidence to support the alternative hypothesis that the population mean is less than', null_mean))
} else {
  print(paste('Fail to reject the null hypothesis. The sample does not provide enough evidence to support the alternative hypothesis that the population mean is less than', null_mean))
}

print(paste("Test statistic:", test_statistic))
print(paste("P-value:", p_value))
print(paste("Critical value:", critical_value))

t.test(sample_data, mu=300)

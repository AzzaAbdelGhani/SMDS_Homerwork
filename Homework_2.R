# Lab Exercise 1

biased <- function(y){
  sb <- (1/length(y)) * sum((y-mean(y))**2)
  return(sb)
}

set.seed(123)

R <- 1000
n <- 10

sample_matrix <- matrix(data = 0, nrow = 1000, ncol = n+2)

for (i in 1:nrow(sample_matrix)) {
  sub_sample <- rnorm(n,0,1)
  sample_matrix[i,] <- c(sub_sample,biased(sub_sample),var(sub_sample))
}

sigma <- 1
par (mfrow=c(1,2), oma=c(0,0,0,0))
hist(sample_matrix[,11], breaks= 40, probability = TRUE, 
     xlab=expression(s_b^2), main= bquote(s_b^2), cex.main=1.5)
curve(((n-1)/sigma^2) * dchisq(x * ((n-1)/sigma^2), df = n - 1),
      add = TRUE, col="red", lwd=2, main="N(0,1)")

hist(sample_matrix[,12], breaks= 40, probability = TRUE, 
     xlab=expression(s^2), main= bquote(s^2), cex.main=1.5)
curve(((n-1)/sigma^2) * dchisq(x * ((n-1)/sigma^2), df = n - 1),
      add = TRUE, col="red", lwd=2, main="N(0,1)")

cat("Biased estimation for variance is: ",mean(sample_matrix[,11]), "unbiased estimation for variance is:",mean(sample_matrix[,12]))

# It can be seen that biased estimation underestimate the population variance

# ref
# https://blogs.uoregon.edu/rclub/2015/01/20/biased-and-unbiased-estimates/
# https://economictheoryblog.com/2012/06/28/latexlatexs2/

1-0.9983817
# Lab Exercise 2 (Didn't understand the point of exercise?)
M <- 6
K <- 4
n <- 50

great_player <- sample( 1:K, n, replace=TRUE, prob =c( 2/16, 4/16, 5/16, 5/16))
observed <- table(great_player)

chisq.test( observed, p = c( 7/16, 5/16, 3/16, 1/16))

# Since p-value is extremely different, we can say that this player does not follow the given probabilities
# according to observations he is a great player

obs <- matrix(data = 0, nrow = M, ncol = K)
for(i in 1:M){
  obs[i,] <- table(sample( 1:K, n, replace=TRUE, prob =c( 7/16, 5/16, 3/16, 1/16))) # almost same guys
}

# apply instead of for to have more efficient code

chisq.test(obs, p = c( 7/16, 5/16, 3/16, 1/16))
# According to test results observations follow the our assumed distribution

# Let's add the great player to the list
obs_wgreat <- rbind(obs, c(table(sample( 1:K, n, replace=TRUE, prob =c( 2/16, 4/16, 5/16, 5/16)))))
chisq.test(obs_wgreat, p = c( 7/16, 5/16, 3/16, 1/16))

# when we add the great player to the group, test results 
# show that it does not follow the assumed dist.


# Lab Exercise 3

Owners <- c( "Katy Perry", "Justin Bieber", "Taylor Swift", "Cristiano Ronaldo",
             "Kim Kardashian", "Ariana Grande", "Selena Gomez", "Demi Lovato")
Instagram <- c( 69, 98,107, 123, 110, 118, 135, 67)
Twitter <- c( 109, 106, 86, 72, 59, 57, 56, 56)
plot( Instagram, Twitter, pch=21, bg=2, xlim=c(60, 150), ylim=c(40, 120) )
text( Instagram[-6], Twitter[-6]+5, Owners[-6], cex=0.8 )
text( Instagram[6], Twitter[6]-5, Owners[6], cex=0.8 )

?cor.test
# H0 = corr = 0
# corr != 0
cor(Instagram, Twitter, method = c("pearson"))  #c("pearson", "kendall", "spearman"))
cor.test(Instagram, Twitter, method=c("pearson"))

# p value is 0.2957 so we can not reject the null hypot

# Lab Exercise 4

# https://www.youtube.com/watch?v=dVwpe6cdScU , https://www.youtube.com/watch?v=GyEYKasQTFg

# http://www.utstat.toronto.edu/~brunner/oldclass/appliedf12/lectures/2101f12Likelihood1.pdf 
# 

# ref http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r

# Lab Exercise 5

# Could not have time to check it

# DAAG Chapter 3 exercise 11
# 
# The following data represent the total number of aberrant crypt foci (abnormal growths in
# the colon) observed in seven rats that had been administered a single dose of the carcinogen
# azoxymethane and sacrificed after six weeks (thanks to Ranjana Bird, Faculty of Human Ecology,
# University of Manitoba for the use of these data):
#   87 53 72 90 78 85 83
# Enter these data and compute their sample mean and variance. Is the Poisson model appropriate
# for these data? To investigate how the sample variance and sample mean differ under the Poisson
# assumption, repeat the following simulation experiment several times:
#   x <- rpois(7, 78.3)
# mean(x); var(x)

data <- c(87,53,72,90,78,85,83)

cat("Sample mean is:", mean(data), "and sample variance is:", var(data))

# Applying monte carlo method

R <- 1000
n <- 7

sample_matrix <- matrix(data = 0, nrow = 1000, ncol = n+2)

for (i in 1:nrow(sample_matrix)) {
  sub_sample <- rpois(n,78.3)
  sample_matrix[i,] <- c(sub_sample,mean(sub_sample),var(sub_sample))
}

cat("Sample mean under poisson assumption is:", mean(sample_matrix[,8]), 
    "and sample variance under poisson assumption is:", mean(sample_matrix[,9]))

# Mean seems okay but variance is not so we should have some doubt about it.

# DAAG Chapter 3 exercise 13

Markov <- function (N=100, initial.value=1, P){
  X <- numeric(N)
  X[1] <- initial.value + 1  # States 0:5; subscripts 1:6
  n <- nrow(P)
  for (i in 2:N){
    X[i] <- sample(1:n, size=1, prob=P[X[i-1], ])
  }
  X-1
}
Pb <- matrix(nrow = 3, ncol = 3, byrow = TRUE, data = c(.6,.2,.2,.2,.4,.4,.4,.3,.3),
             dimnames = list(c("Sun", "Cloud", "Rain"), c("Sun", "Cloud", "Rain")))
chain <- factor(Markov(1000000, 0, Pb), labels = c("Sun", "Cloud", "Rain"))
table(chain)/length(chain)




# DAAG Chapter 4 exercise 6

# Repeat this several times. There should be no consistent pattern in the acf plot for different
# random samples y1. There will be a fairly consistent pattern in the acf plot for y, a result of
# the correlation that is introduced by adding to each value the next value in the sequence.

par (mfrow=c(1,2), oma=c(0,0,0,0))
y1 <- rnorm(51)
y1 # this is iid
y <- y1[-1] + y1[-51]
y # this is not iid
acf(y1) # acf is `autocorrelation function' (see Ch. 9)
acf(y)
?acf
# comment here

# DAAG Chapter 4 exercise 7

# Create a function that does the calculations in the first two lines of the previous exercise.
# Put the calculation in a loop that repeats 25 times. Calculate the mean and variance for each
# vector y that is returned. Store the 25 means in the vector av, and store the 25 variances in
# the vector v. Calculate the variance of av.


corfun <- function(n=51){
  y1 <- rnorm(n)
  y <- y1[-1]+y1[-n]
  y
}

av <- numeric(25)
v <- numeric(25)
for(i in 1:25){
  z <- corfun()
  av[i] <- mean(z)
  v[i] <- var(z)
  }
var(av)

# CS: Chapter 3, exercises 3.3 (hint: use system.time() function), 3.5.


#Rewrite the following, replacing the loop with efficient code:

n <- 100000; z <- rnorm(n)
zneg <- 0;j <- 1
for (i in 1:n) {
  if (z[i]<0) {
    zneg[j] <- z[i]
    j <- j + 1
  }
}



# CS, 3.5

set.seed(0); n <- 1000
A <- matrix(runif(n*n),n,n); x.true <- runif(n)
y <- A%*%x.true

dim(A)
?solve
?proc.time




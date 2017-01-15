
#--------------------------- Statistical Tests and Clustering

# μ mu, pronounced “mew” = mean of a population.
# σ “sigma” = standard deviation of a population. 
# ρ rho, pronounced “roe” = linear correlation coefficient of a population.
# σ2 refers to the variance of a population.


#--------------------------- A.1 ---------------------------

S1 <- rnorm(100, mean = 0.0, sd = 1.0)
S2 <- rnorm(100, mean = 1.5, sd = 1.0)
S3 <- rnorm(10,  mean = 1.5, sd = 1.0)


#--------------------------- A.2 ---------------------------

dens_S1 <- density(S1)
plot(dens_S1)
dens_S2 <- density(S2)
plot(dens_S2)
dens_S3 <- density(S3)
plot(dens_S3)


#--------------------------- A.3 ---------------------------

plot(dens_S1, xlim = c(min(S1, S2), max(S1, S2)), ylim = c(0, 0.5))
 lines(dens_S2)
rug(S1, col=1, lwd=2.5)
rug(S2, col=1, lwd=2.5)

plot(dens_S1, xlim = c(min(S1, S2), max(S1, S2)), ylim = c(0, 0.6))
lines(dens_S3)
rug(S1, col=1, lwd=2.5)
rug(S3, col=1, lwd=2.5)


#--------------------------- A.4 ---------------------------
# Interpret the above density plots. What do they indicate?

# As overlap of 2 distribution of S1 and S2 is large, they are approximately 
# close to each other. However, in case of S1 and S3, the overlap is shown that
# they are close to each other though it does not true. Because S3 has 1 in 10 
# seed in camparison to S1.


#--------------------------- A.5 ---------------------------
# Perform a t-test between S1 and S2, as well as between S1 and S3. 
# How significant is the difference between the samples?

# Among the most frequently used t-tests are:
#  1. A one-sample location test of whether the mean of a population has a value 
#     specified in a null hypothesis.
#  2. A two-sample location test of the null hypothesis such that the means of two 
#     populations are equal. All such tests are usually called Student's t-tests, 
#     though strictly speaking that name should only be used if the variances of the 
#     two populations are also assumed to be equal; the form of the test used when 
#     this assumption is dropped is sometimes called Welch's t-test. These tests are 
#     often referred to as "unpaired" or "independent samples" t-tests, as they are 
#     typically applied when the statistical units underlying the two samples being 
#     compared are non-overlapping.
#  3. A test of the null hypothesis that the difference between two responses measured 
#     on the same statistical unit has a mean value of zero. For example, suppose we 
#     measure the size of a cancer patient's tumor before and after a treatment. If 
#     the treatment is effective, we expect the tumor size for many of the patients 
#     to be smaller following the treatment. This is often referred to as the "paired" 
#     or "repeated measures" t-test.
#  4. A test of whether the slope of a regression line differs significantly from 0.

# Null Hypothesis
qt(p = 0.05 / 2, df = 28, lower.tail = FALSE)

# The basic concept of hypothesis testing is to form an assertion and test it 
# with data. When performing hypothesis tests, the common assumption is that 
# there is no difference between two samples. This assumption is used as the 
# default position for building the test or conducting a scientific experiment.
# Statisticians refer to this as the null hypothesis (H0). The alternative 
# hypothesis (HA) is that there is a difference between two samples. For example, if the task is to identify the effect of drug A compared to
# drug B on patients, the null hypothesis and alternative hypothesis would be 
# this:
# ● H0: Drug A and drug B have the same effect on patients.
# ● HA: Drug A has a greater effect than drug B on patients.
# It is important to state the null hypothesis and alternative hypothesis, 
# because misstating them is likely to undermine the subsequent steps of the 
# hypothesis testing process. A hypothesis test leads to either rejecting the 
# null hypothesis in favor of the alternative or not rejecting the null hypothesis.

# ● H0: μ1 = μ2
# ● HA: μ1 ≠ μ2

# The basic testing approach is to compare the observed sample means, X1 and X2, 
# corresponding to each population. If the values of X1 and X2 are approximately
# equal to each other, the distributions of X1 and X2 overlap substantially and
# the null hypothesis is supported. A large observed difference between the 
# sample means indicates that the null hypothesis should be rejected. Formally, 
# the difference in means can be tested using Student’s t-test or the Welch’s t-test.

t.test(S1, S2, var.equal = TRUE)       # Performs Student's t-test by var.equal = TRUE
t.test(S1, S3, var.equal = TRUE)

# obtain t value for a two-sided test at a 0.05 significance level
qt(p=0.05/2, df=28, lower.tail= FALSE)

# In both cases, the null hypothesis is rejected.

#--------------------------- A.6 ---------------------------

head(iris)
names(iris)
x = iris[,3:4]
y = iris$Species
k2_1 <- kmeans(x,2)
k2_2 <- kmeans(x,2)
k2_3 <- kmeans(x,2)

k3_1 <- kmeans(x,3)
k3_2 <- kmeans(x,3)
k3_3 <- kmeans(x,3)

k4_1 <- kmeans(x,4)
k4_2 <- kmeans(x,4)
k4_3 <- kmeans(x,4)


#--------------------------- A.7 ---------------------------
# Visualize the results of each clustering.
# Do the clusters remain the same? 
# Are the results as you would expect them to be?

#_____________________________________________________ K = 2 
layout(matrix(c(1, 2, 3), ncol = 1, nrow = 3, byrow = T))

attach(k2_1)

# Contingency Tables: In R, table refers to a class of objects used to store 
# the observed counts across the factors for a given dataset.
table(y, cluster)

plot(x[c("Petal.Length", "Petal.Width")], main = "K2 round one", col=cluster)
points(centers[,c("Petal.Length", "Petal.Width")], col=3:4, pch=23, cex=3)

attach(k2_2)
table(y, cluster)
plot(x[c("Petal.Length", "Petal.Width")], main = "K2 round two", col=cluster)
points(centers[,c("Petal.Length", "Petal.Width")], col=3:4, pch=23, cex=3)

attach(k2_3)
table(y, cluster)
plot(x[c("Petal.Length", "Petal.Width")], main = "K2 round three", col=cluster)
points(centers[,c("Petal.Length", "Petal.Width")], col=3:4, pch=23, cex=3)

#_____________________________________________________ K = 3
attach(k3_1)
table(y, cluster)
plot(x[c("Petal.Length", "Petal.Width")], main = "K3 round one", col=cluster)
points(centers[,c("Petal.Length", "Petal.Width")], col=3:4, pch=23, cex=3)

attach(k3_2)
table(y, cluster)
plot(x[c("Petal.Length", "Petal.Width")], main = "K3 round two", col=cluster)
points(centers[,c("Petal.Length", "Petal.Width")], col=3:4, pch=23, cex=3)

attach(k3_3)
table(y, cluster)
plot(x[c("Petal.Length", "Petal.Width")], main = "K3 round three", col=cluster)
points(centers[,c("Petal.Length", "Petal.Width")], col=3:4, pch=23, cex=3)

#_____________________________________________________ K = 4
attach(k4_1)
table(y, cluster)
plot(x[c("Petal.Length", "Petal.Width")], main = "K4 round one", col=cluster)
points(centers[,c("Petal.Length", "Petal.Width")], col=3:4, pch=23, cex=3)

attach(k4_2)
table(y, cluster)
plot(x[c("Petal.Length", "Petal.Width")], main = "K4 round two", col=cluster)
points(centers[,c("Petal.Length", "Petal.Width")], col=3:4, pch=23, cex=3)

attach(k4_3)
table(y, cluster)
plot(x[c("Petal.Length", "Petal.Width")], main = "K4 round three", col=cluster)
points(centers[,c("Petal.Length", "Petal.Width")], col=3:4, pch=23, cex=3)


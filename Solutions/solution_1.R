
#--------------------------- A.1 ---------------------------
var <- mtcars
var

# R provides several functions, such as class() and typeof(), to examine 
# the characteristics of a given variable. The class() function represents 
# the abstract class of an object. The typeof() function determines the way 
# an object is stored in memory.
typeof(var)
class(var)

# Although i appears to be an integer, i is internally stored using double 
# precision.
i <- 23
typeof(i)              # double
class(i)               # numeric



#--------------------------- A.2 ---------------------------
col_means <- colMeans(var)
col_means

col_median <- apply(var, 2, MARGIN=2, FUN = median)
col_median

col_max <- apply(var, 2, MARGIN=2, FUN = max)
col_max

# or

# Based on the observed counts in the table, the summary() function performs a 
# chi-squared test on the independence of the two factors. Because the reported 
# p-value is greater than 0.05, the assumed independence of the two factors is 
# not rejected.
summary(var)


#--------------------------- A.3 ---------------------------

mpg <- var[, 1]

# mpg plot
plot(mpg, type = 'p', col = 'orange', lwd = 3, 
     main = 'Miles per Gallon', xlab = '', ylab = '')

# density plot which emphasizes the distribution
plot(density(mpg), lty = 3, main = 'Density of Miles per Gallon')
rug(mpg, col=2, lwd=3.5)

# mpg plot
plot(log10(log10(mpg)), type = 'p', col = 'orange', lwd = 3, 
     main = 'Miles per Gallon', xlab = '', ylab = '')

# density plot which emphasizes the distribution
lines(density(mpg), lty = 3)
rug(mpg, col=2, lwd=3.5)


#--------------------------- A.4 ---------------------------
L_100 <- (235.215 / var$mpg) 

# Combinition of dataset and 1/100km column
var_extend <- cbind(var, L_100)

# Rename the name of new column
colnames(var_extend)[12] <- paste("L/100km")
head(var_extend)



#--------------------------- A.5 ---------------------------

# Plot mpg
plot(var_extend$mpg, var_extend$`L/100km`, type = 'p', col = '#2cc114',  
     ylim=range( c(mpg, L_100)), lwd = 3, 
     main = "Relationship Plot", 
     xlab = 'mpg', ylab = 'liter per 100km')



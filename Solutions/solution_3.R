#--------------------------- Association Rule Mining
#--------------------------- A.1 ---------------------------
install.packages(c("arules", "arulesViz"))
require(arules)
require(arulesViz)

#--------------------------- A.2 ---------------------------
Data <- read.transactions("http://user.informatik.uni-goettingen.de/~sherbold/AssociationRules.csv", 
                          rm.duplicates = FALSE, format="basket", sep=" ")
Data@data

image(sample(Data, 100))

summary(Data)

apply(Data@data[, 10:20], 2,
      function(r) paste(Data@itemInfo[r, "labels"], collapse = ", "))


#--------------------------- A.3 ---------------------------
itemsets <- apriori(Data, parameter = list(minlen = 1, maxlen = 1, support = 0.002, target = "frequent itemsets")) 
# confidence = 1.0 as default
summary(itemsets)
# As the maximum support of the 1-itemset is 0.494800, the value of the support should 
# be picked lower than it.

rules <- apriori(Data, parameter = list(support = 0.002, confidence = 0.6)) 
# confidence = 0.8 and support = 0.1 as default
summary(rules)

# One way to approach the problem of setting support is to think about the minimum
# number of transactions you would need before you would consider a pattern interesting.
# Lift is 1 if X and Y are statistically independent of each other. In contrast, a lift 
# of X -> Y greater than 1 indicates that there is some usefulness to the rule. 
# A larger value of lift suggests a greater strength of the association between X and Y.

m <- interestMeasure(rules, transactions = Data)
# In theory, leverage is 0 when X and Y are statistically independent of each other. 
# If X and Y have some kind of relationship, the leverage would be greater than zero. 
# A larger leverage value indicates a stronger relationship between X and Y.


hist(m$support, breaks = 100)
hist(m$confidence, breaks = 100)
hist(m$lift, breaks = 100)
hist(m$leverage, breaks = 100)

inspect(sort(rules, by = "lift")[1:10])

#--------------------------- A.4 ---------------------------
plot(rules, interactive = TRUE)









#--------------------------- A.5 ---------------------------
# While doing this, think about reasonable values for support and confidence. 
# See what happens when you choose different values.



#--------------------------- Logistic Regression
cuse <- read.table("http://data.princeton.edu/wws509/datasets/cuse.dat", header=TRUE)

# The Contraceptive Use Data:
# showing the distribution of 1607 currently married and fecund women interviewed in the 
# Fiji Fertility Survey, according to age, education, desire for more children and current 
# use of contraception.


attach(cuse)
head(cuse)
cuse.logr <- cbind(using, notUsing)

# The model shows how contraceptive use depends on age, education and wantsMore:
lr_cuse <- glm(formula = cuse.logr ~ age + education + wantsMore , 
               family = binomial(logit))
summary(lr_cuse)
# AIC (Akaike Information Criterion)
lr_cuse_AIC <- 1-(29.917/165.772)

new_cuse <- cuse[which(age != "25-29"),]
new_cuse
attach(new_cuse)
new_cuse.logr <- cbind(using, notUsing)
lr_new_cuse <- glm(formula = new_cuse.logr ~ age + education + wantsMore , 
               family = binomial(logit))
lr_new_cuse
summary(lr_new_cuse)

# It has a smaller AIC, thus fitting better.
# Pseudo-R^2
lr_cuse_AIC <- 1-(29.917/165.772)
lr_cuse_AIC
lr_new_cuse_AIC <- 1-(22.362/154.251)
lr_new_cuse_AIC

# Following the special symbol ~ that separates the response from the predictors, we have a 
# standard Wilkinson-Rogers model formula. In this case we are specifying main effects of 
# age, education and wantsMore. Because all three predictors are categorical variables, they
# are treated automatically as factors, as you can see by inspecting the results:




# the coeficients. Which features are important? Why?


# To get a hierarchical analysis of variance table corresponding to introducing each of the 
# terms in the model one at a time, in the same order as in the model formula, 
# try the anova function:
anova(lr_cuse)

## ----load-libraries, echo=FALSE, eval=TRUE,results='hide', message=FALSE, warning=FALSE----
library(ggplot2)
library(dplyr)
library(tidyr)

library(caret)
library(pROC)

## ----read-in-data-and-check-num-values-per-variable, echo=FALSE, eval=TRUE----
crime <- read.csv("https://raw.githubusercontent.com/heathergeiger/Data621_hw3/master/crime-training-data.csv",header=TRUE,stringsAsFactors=FALSE)
crime$chas <- factor(crime$chas)
crime$target <- factor(crime$target)

print("Total number of records in data:")

nrow(crime)

print("Number of non-NA values per variable in data:")

non_NA_per_column <- crime[,setdiff(colnames(crime),c("chas","target"))] %>%
gather() %>%
na.omit(value) %>%
count(key)

non_NA_per_column <- data.frame(non_NA_per_column)

data.frame(Variable = non_NA_per_column[,1],n = non_NA_per_column[,2])

print("Frequency of variable chas 0 vs. 1:")
table(crime$chas)

print("Frequency of variable target 0 vs. 1:")
table(crime$target)

## ----num-unique-per-column, echo=FALSE, eval=TRUE,message=FALSE, warning=FALSE----
num_unique_values_per_variable <- crime %>%
gather() %>%
group_by(key) %>%
summarize(uniques = n_distinct(value))

num_unique_values_per_variable <- data.frame(num_unique_values_per_variable,stringsAsFactors=FALSE)

num_unique_values_per_variable <- data.frame(Variable = num_unique_values_per_variable[,1],Num.uniques = num_unique_values_per_variable[,2],stringsAsFactors=FALSE)

num_unique_values_per_variable[order(num_unique_values_per_variable[,2]),]

## ----num-unique-combinations, echo=FALSE, eval=TRUE----------------------
print("Number of unique combinations of rad/tax/indus/ptratio/zn:")
length(unique(paste0(crime$rad,"/",crime$tax,"/",crime$indus,"/",crime$ptratio,"/",crime$zn)))

print("Number of unique combinations of rad/tax/indus/ptratio/zn/nox:")
length(unique(paste0(crime$rad,"/",crime$tax,"/",crime$indus,"/",crime$ptratio,"/",crime$zn,"/",crime$nox)))

## ----unique-with-vs-without-nox, echo=FALSE, eval=TRUE-------------------
other_vars_vs_nox <- data.frame(Other.vars = paste0(crime$rad,"/",crime$tax,"/",crime$indus,"/",crime$ptratio,"/",crime$zn),
	Nox = crime$nox,
	stringsAsFactors=FALSE)

other_vars_vs_nox <- other_vars_vs_nox[which(duplicated(paste0(other_vars_vs_nox[,1],"/",other_vars_vs_nox[,2])) == FALSE),]

freq_other_vars_vs_nox <- data.frame(table(other_vars_vs_nox[,1]))
colnames(freq_other_vars_vs_nox) <- c("Rad.tax.indus.ptratio.zn","Num.separate.values.nox")

freq_other_vars_vs_nox <- freq_other_vars_vs_nox[freq_other_vars_vs_nox[,2] > 1,]

freq_other_vars_vs_nox[order(freq_other_vars_vs_nox[,2],decreasing=TRUE),]

## ----municip-vs-num-neighborhoods, echo=FALSE, eval=TRUE-----------------
neighborhoods_per_group <- data.frame(table(paste0(crime$rad,"/",crime$tax,"/",crime$indus,"/",crime$ptratio)))
colnames(neighborhoods_per_group) <- c("Value","Num.uniques")
print("Number of neighborhoods per municipality:")
table(neighborhoods_per_group$Num.uniques)

## ----municip-match-up-to-target, echo=FALSE, eval=TRUE-------------------
multi_groups_not_uniqued <- paste0(crime$rad,"/",crime$tax,"/",crime$indus,"/",crime$ptratio)

neighborhoods_per_group <- neighborhoods_per_group[order(neighborhoods_per_group[,2],decreasing=TRUE),]

num_target_vs_neighborhoods_in_group <- c()

for(group in as.vector(neighborhoods_per_group[,1]))
{
num_target_vs_neighborhoods_in_group <- rbind(num_target_vs_neighborhoods_in_group,
	data.frame(Num.neighborhoods = nrow(crime[multi_groups_not_uniqued %in% group,]),
	Num.below = length(which(crime[multi_groups_not_uniqued %in% group,"target"] == 0)),
	Num.above = length(which(crime[multi_groups_not_uniqued %in% group,"target"] == 1)),
	stringsAsFactors=FALSE))
}

num_target_vs_neighborhoods_in_group <- data.frame(num_target_vs_neighborhoods_in_group,
        Proportion.more.frequent.neighborhood.type = ifelse(num_target_vs_neighborhoods_in_group$Num.above > num_target_vs_neighborhoods_in_group$Num.below,
	round(num_target_vs_neighborhoods_in_group$Num.above/num_target_vs_neighborhoods_in_group$Num.neighborhoods,digits=4),
	round(num_target_vs_neighborhoods_in_group$Num.below/num_target_vs_neighborhoods_in_group$Num.neighborhoods,digits=4)),
	stringsAsFactors=FALSE)

num_target_vs_neighborhoods_in_group <- data.frame(num_target_vs_neighborhoods_in_group,
	Majority.neighborhoods.status = ifelse(num_target_vs_neighborhoods_in_group$Num.above > num_target_vs_neighborhoods_in_group$Num.below,
	"High.crime","Low.crime"),
	stringsAsFactors=FALSE)

num_target_vs_neighborhoods_in_group <- data.frame(num_target_vs_neighborhoods_in_group,
	All.neighborhoods.high.crime = ifelse(num_target_vs_neighborhoods_in_group$Num.above == num_target_vs_neighborhoods_in_group$Num.neighborhoods,
	"All.high.crime","At.least.one.low.crime"),
	stringsAsFactors=FALSE)

print("Table for proportions more frequent neighborhood type:")
table(num_target_vs_neighborhoods_in_group$Proportion.more.frequent.neighborhood.type)

stripchart(Num.neighborhoods ~ Majority.neighborhoods.status,
	data=num_target_vs_neighborhoods_in_group,
	method="jitter",pch=21,
	xlab="Number of neighborhoods in municipality",
	ylab="Status of the majority of neighborhoods in municipality")

stripchart(Num.neighborhoods ~ All.neighborhoods.high.crime,
	data=num_target_vs_neighborhoods_in_group,
	method="jitter",pch=21,
	xlab="Number of neighborhoods in municipality",
	ylab="Binary for whether 100% of neighborhoods are high crime")

abline(v=15,lty=2)

legend("topright",legend="x=15",lty=2)

## ----municipalities-in-evaluation, echo=FALSE, eval=TRUE-----------------
evaluation <- read.csv("crime-evaluation-data.csv",header=TRUE,stringsAsFactors=FALSE)

multi_groups_not_uniqued_eval <- paste0(evaluation$rad,"/",evaluation$tax,"/",evaluation$indus,"/",evaluation$ptratio)

print("Number of records in evaluation:")
nrow(evaluation)
print("Number of records matching 121-neighborhood municipality from training:")
length(which(multi_groups_not_uniqued_eval == as.vector(neighborhoods_per_group[neighborhoods_per_group[,2] == 121,1])))
print("Number of records matching 28-neighborhood municipality from training:")
length(which(multi_groups_not_uniqued_eval == as.vector(neighborhoods_per_group[neighborhoods_per_group[,2] == 28,1])))
print("Number of records matching 19-neighborhood municipality from training:")
length(which(multi_groups_not_uniqued_eval == as.vector(neighborhoods_per_group[neighborhoods_per_group[,2] == 19,1])))
print("Number of records matching 16-neighborhood municipality from training:")
length(which(multi_groups_not_uniqued_eval == as.vector(neighborhoods_per_group[neighborhoods_per_group[,2] == 16,1])))

## ----range-black, echo=FALSE, eval=TRUE----------------------------------
range(crime$black)

## ----hist-neighborhood-level, echo=FALSE, eval=TRUE----------------------
par(mfrow=c(1,3))

hist(crime$rm,
xlab="Values",
ylab="Num neighborhoods",
main="rm")

hist(crime$dis,
xlab="Values",
ylab="Num neighborhoods",
main="dis")

hist(crime$lstat,
xlab="Values",
ylab="Num neighborhoods",
main="lstat")

par(mfrow=c(2,2))

hist(crime$medv,
xlab="Values",
ylab="Num neighborhoods",
main="medv")

hist(crime$medv[crime$medv < 50],
xlab="Values",
ylab="Num neighborhoods",
main="medv < 50")

hist(crime$age,
xlab="Values",
ylab="Num neighborhoods",
main="age")

hist(crime$age[crime$age < 100],
xlab="Values",
ylab="Num neighborhoods",
main="age < 100")

## ----initial-transformations, echo=FALSE, eval=TRUE----------------------
crime <- data.frame(crime,ID = paste0(crime$rad,"/",crime$tax,"/",crime$indus,"/",crime$ptratio,"/",crime$zn),stringsAsFactors=FALSE)

neighborhoods_per_group <- data.frame(table(paste0(crime$rad,"/",crime$tax,"/",crime$indus,"/",crime$ptratio,"/",crime$zn)))
colnames(neighborhoods_per_group) <- c("ID","Num.neighborhoods")
neighborhoods_per_group$ID <- as.vector(neighborhoods_per_group$ID)

crime <- merge(crime,neighborhoods_per_group)

crime <- data.frame(crime,big.city = ifelse(crime$Num.neighborhoods >= 15,1,0))

crime$black <- ifelse(crime$black > 136.9,0,1)

crime$big.city <- factor(crime$big.city)
crime$black <- factor(crime$black)

crime <- crime[,c("chas","rm","age","dis","black","lstat","medv","big.city","target")]

## ----binary-vs-binary, echo=FALSE, eval=TRUE-----------------------------
chas <- ifelse(crime$chas == 1,"Borders.Charles","Doesnt.border.Charles")
black <- ifelse(crime$black == 1,"Black.residents.26percent.plus","Black.residents.under.26percent")
big_city <- ifelse(crime$big.city == 1,"Big.city","Small.city")
print("Variables chas vs. black:")
table(chas,black)
print("Variables chas vs. big.city:")
table(chas,big_city)
print("Variables black vs. big.city:")
table(black,big_city)

## ----binary-vs-numeric-predictors, echo=FALSE, eval=TRUE-----------------
for(binary in c("chas","big.city"))
{
par(mfrow=c(2,3))
for(numeric in c("rm","age","dis","lstat","medv")){
        crime_for_stripchart <- crime[,c(numeric,binary)]
        colnames(crime_for_stripchart) <- c("numeric","binary")
        boxplot(numeric ~ binary,data=crime_for_stripchart,xlab=binary,ylab=numeric)
        }
plot.new()
}

## ----pairs-scatterplots-big-vs-small-cities, echo=FALSE, eval=TRUE-------
pairs(crime[crime$big.city == 1,c("rm","age","dis","lstat","medv")],main="Big cities (15+ neighborhoods)")
pairs(crime[crime$big.city == 0,c("rm","age","dis","lstat","medv")],main="Small cities (<15 neighborhoods)")

## ----hist-after-log2-transform, echo=FALSE, eval=TRUE--------------------
par(mfrow=c(1,2))

hist(log2(crime$lstat + 1),
xlab="Values",
ylab="Number of neighborhoods",
main="Log2(lstat + 1)")

hist(log2(crime$medv),
xlab="Values",
ylab="Number of neighborhoods",
main="Log2(medv)")

## ----correlation-after-log2-transform, echo=FALSE, eval=TRUE-------------
par(mfrow=c(1,2))

plot(log2(crime$lstat + 1)[crime$big.city == 1],
log2(crime$medv)[crime$big.city == 1],
xlab="log2(lstat + 1)",
ylab="log2(medv)",
main="Big cities only")

abline(lm(log2(crime$medv)[crime$big.city == 1] ~ log2(crime$lstat + 1)[crime$big.city == 1]),lty=2)

plot(log2(crime$lstat + 1)[crime$big.city == 0],
log2(crime$medv)[crime$big.city == 0],
xlab="log2(lstat + 1)",
ylab="log2(medv)",
main="Small cities only")

abline(lm(log2(crime$medv)[crime$big.city == 0] ~ log2(crime$lstat + 1)[crime$big.city == 0]),lty=2)

par(mfrow=c(1,1))

## ----chas-vs-target-small-cities, echo=FALSE, eval=TRUE------------------
chas_small_cities <- crime$chas[crime$big.city == 0]
target_small_cities <- crime$target[crime$big.city == 0]

chas_small_cities <- ifelse(chas_small_cities == 1,"Borders.Charles","Doesnt.border.Charles")
target_small_cities <- ifelse(target_small_cities == 1,"Above.median.crime","Below.median.crime")

chas_vs_target_small_cities <- table(chas_small_cities,target_small_cities)

print("As raw numbers:")
chas_vs_target_small_cities

print("As proportions:")

chas_vs_target_small_cities[1,] <- chas_vs_target_small_cities[1,]/sum(chas_vs_target_small_cities[1,])
chas_vs_target_small_cities[2,] <- chas_vs_target_small_cities[2,]/sum(chas_vs_target_small_cities[2,])
chas_vs_target_small_cities  <- round(chas_vs_target_small_cities,digits=4)

chas_vs_target_small_cities

## ----boxplot-target-vs-numeric, echo=FALSE, eval=TRUE--------------------
crime_small_cities <- crime[crime$big.city == 0,]

par(mfrow=c(2,3))

for(var in c("rm","age","dis","lstat","medv"))
{
boxplot_data <- crime_small_cities[,c(var,"target")]
colnames(boxplot_data) <- c("var","target")
boxplot(var ~ target,data=boxplot_data,xlab="target",ylab=var)
}

plot.new()

par(mfrow=c(1,2))

boxplot(log2(lstat + 1) ~ target,data=crime_small_cities,xlab="target",ylab="log2(lstat + 1)")
boxplot(log2(medv) ~ target,data=crime_small_cities,xlab="target",ylab="log2(medv)")

## ----contingency-target-vs-rm-and-medv, echo=FALSE, eval=TRUE------------
rm_small_cities <- crime_small_cities$rm

rm_small_cities[crime_small_cities$rm < 5.5] <- "small (<5.5)"
rm_small_cities[crime_small_cities$rm >= 5.5 & crime_small_cities$rm < 6.5] <- "med (>= 5.5, < 6.5)"
rm_small_cities[crime_small_cities$rm >= 6.5 & crime_small_cities$rm < 7.5] <- "large (>=6.5, < 7.5)"
rm_small_cities[crime_small_cities$rm >= 7.5] <- "XL (>= 7.5)"

medv_small_cities <- crime_small_cities$medv

medv_small_cities[crime_small_cities$medv < 15] <- "low (< 15)"
medv_small_cities[crime_small_cities$medv >= 15 & crime_small_cities$medv < 20] <- "low-med (>= 15, < 20)"
medv_small_cities[crime_small_cities$medv >= 20 & crime_small_cities$medv < 25] <- "med (>= 20, < 25)"
medv_small_cities[crime_small_cities$medv >= 25 & crime_small_cities$medv < 30] <- "med-high (>= 25, < 30)"
medv_small_cities[crime_small_cities$medv >= 30] <- "high (>= 30)"

rm_vs_target_table <- table(rm_small_cities,crime_small_cities$target)
medv_vs_target_table <- table(medv_small_cities,crime_small_cities$target)

rm_vs_target_table_proportions <- rm_vs_target_table
for(i in 1:nrow(rm_vs_target_table_proportions)){rm_vs_target_table_proportions[i,] <- rm_vs_target_table_proportions[i,]/sum(rm_vs_target_table_proportions[i,])}
rm_vs_target_table_proportions <- round(rm_vs_target_table_proportions,digits=4)

medv_vs_target_table_proportions <- medv_vs_target_table
for(i in 1:nrow(medv_vs_target_table_proportions)){medv_vs_target_table_proportions[i,] <- medv_vs_target_table_proportions[i,]/sum(medv_vs_target_table_proportions[i,])}
medv_vs_target_table_proportions <- round(medv_vs_target_table_proportions,digits=4)

rm_vs_target_table
rm_vs_target_table_proportions

medv_vs_target_table
medv_vs_target_table_proportions

## ----density-target-vs-rm-and-medv, echo=FALSE, eval=TRUE----------------
target_vs_rm_and_medv_data <- data.frame(Variable = rep(c("High crime, rm","Low crime, rm","High crime, medv","Low crime, medv"),
	times=c(length(which(crime_small_cities$target == 1)),length(which(crime_small_cities$target == 0)),length(which(crime_small_cities$target == 1)),length(which(crime_small_cities$target == 0)))),
	Value = c(crime_small_cities[crime_small_cities$target == 1,"rm"],
	crime_small_cities[crime_small_cities$target == 0,"rm"],
	crime_small_cities[crime_small_cities$target == 1,"medv"],
	crime_small_cities[crime_small_cities$target == 0,"medv"]),
	stringsAsFactors=FALSE)

ggplot(target_vs_rm_and_medv_data[grep('rm',target_vs_rm_and_medv_data$Variable),],
aes(x=Value)) +
geom_density() +
facet_wrap(~Variable)

ggplot(target_vs_rm_and_medv_data[grep('rm',target_vs_rm_and_medv_data$Variable,invert=TRUE),],
aes(x=Value)) +
geom_density() +
facet_wrap(~Variable)

## ----rm-vs-medv-as-binaries, echo=FALSE, eval=TRUE-----------------------
rm_binary <- ifelse(crime_small_cities$rm < 5.5,"Small.average.home","Larger.average.home")
medv_binary <- ifelse(crime_small_cities$medv >= 20,"Higher.value.home","Lower.value.home")

table(rm_binary,medv_binary)

## ----transformation-before-modeling, echo=FALSE, eval=TRUE---------------
crime <- crime[,setdiff(colnames(crime),c("black","chas"))]
crime <- data.frame(crime,
	small.rm = ifelse(crime$rm < 5.5,1,0),
	low.medv = ifelse(crime$medv < 20,1,0),
	stringsAsFactors=FALSE)
crime <- crime[,setdiff(colnames(crime),c("rm","medv"))]

for(var in setdiff(colnames(crime),"target"))
{
crime[,var] <- as.numeric(as.vector(crime[,var]))
}

## ----model-all-simple-incl-bigcity, echo=FALSE, eval=TRUE----------------
model_all_simple_incl_bigcity <- glm(target ~ .,data=crime,family="binomial")

summary(model_all_simple_incl_bigcity)

## ----model-all-but_medv-incl-bigcity-transform-lstat, echo=FALSE, eval=TRUE----
model_all_but_medv_incl_bigcity_dont_transform_lstat <- glm(target ~ age + dis + small.rm + big.city + lstat,data=crime,family="binomial")

summary(model_all_but_medv_incl_bigcity_dont_transform_lstat)

model_all_but_medv_incl_bigcity_do_transform_lstat <- glm(target ~ age + dis + small.rm + big.city + log2(lstat + 1),data=crime,family="binomial")

summary(model_all_but_medv_incl_bigcity_do_transform_lstat)

## ----model-incl-bigcity-age-dis-smallrm, echo=FALSE, eval=TRUE-----------
model_incl_bigcity_age_dis_smallrm <- glm(target ~ age + dis + small.rm + big.city,data=crime,family="binomial")

summary(model_incl_bigcity_age_dis_smallrm)

## ----model-incl-bigcity-age-binary-dis-smallrm, echo=FALSE, eval=TRUE----
crime_binary_age <- crime
crime_binary_age$age <- ifelse(crime_binary_age$age >= 80,1,0)

model_incl_bigcity_age_binary_dis_smallrm <- glm(target ~ age + dis + small.rm + big.city,data=crime_binary_age,family="binomial")

summary(model_incl_bigcity_age_binary_dis_smallrm)

## ----model-incl-bigcity-age-binary-dis, echo=FALSE, eval=TRUE------------
model_incl_bigcity_age_binary_dis <- glm(target ~ age + dis + big.city,data=crime_binary_age,family="binomial")

summary(model_incl_bigcity_age_binary_dis)

## ----predict-sample-data-incl-bigcity-age-binary-dis, echo=FALSE, eval=TRUE----
sample_data_for_model_incl_bigcity_age_binary_dis <- data.frame(big.city = rep(0,times=16),
	age = rep(c(0,1),each=8),
	dis = c(1:6,9,13))

predictions_sample_data_for_model_incl_bigcity_age_binary_dis <- predict(object = model_incl_bigcity_age_binary_dis,
	newdata=sample_data_for_model_incl_bigcity_age_binary_dis,
	type="response")

data.frame(sample_data_for_model_incl_bigcity_age_binary_dis,p = round(predictions_sample_data_for_model_incl_bigcity_age_binary_dis,digits=4))

## ----model-all-minus-bigcity, echo=FALSE, eval=TRUE----------------------
model_all_minus_bigcity <- glm(target ~ .,data=crime_binary_age[,setdiff(colnames(crime),"big.city")],family="binomial")

summary(model_all_minus_bigcity)

## ----model-minus-bigcity-age-dis-medv, echo=FALSE, eval=TRUE-------------
model_minus_bigcity_age_dis_medv <- glm(target ~ age + dis + low.medv,data=crime_binary_age,family="binomial")

summary(model_minus_bigcity_age_dis_medv)

## ----model-minus-bigcity-age-dis, echo=FALSE, eval=TRUE------------------
model_minus_bigcity_age_dis <- glm(target ~ age + dis,data=crime_binary_age,family="binomial")

summary(model_minus_bigcity_age_dis)

## ----confus-matrix-caret, echo=FALSE, eval=TRUE--------------------------
crime$age <- ifelse(crime$age >= 80,1,0)

predictions_model_incl_bigcity_age_binary_dis <- predict(object = model_incl_bigcity_age_binary_dis,data=crime,type="response")
predictions_model_minus_bigcity_age_dis_medv <- predict(object = model_minus_bigcity_age_dis_medv,data=crime,type="response")
predictions_model_minus_bigcity_age_dis <- predict(object = model_minus_bigcity_age_dis,data=crime,type="response")

print("Model #1 (incl. big.city, age, dis):")
caret::confusionMatrix(data = ifelse(predictions_model_incl_bigcity_age_binary_dis > 0.5,1,0),
	reference = crime$target,
	positive="1")

print("Model #2 (incl. age, dis, and low.medv, not big.city):")
caret::confusionMatrix(data = ifelse(predictions_model_minus_bigcity_age_dis_medv > 0.5,1,0),
	reference = crime$target,
	positive="1")

print("Model #3 (incl. age and dis, not big.city):")
caret::confusionMatrix(data = ifelse(predictions_model_minus_bigcity_age_dis > 0.5,1,0),
	reference = crime$target,
	positive="1")

## ----cor-model2-vs-model3, echo=FALSE, eval=TRUE-------------------------
plot(predictions_model_minus_bigcity_age_dis_medv,
predictions_model_minus_bigcity_age_dis,
xlab="Model #2 probability",
ylab="Model #3 probability")

abline(0,1,lty=2)
abline(v=0.5,lty=2)
abline(h=0.5,lty=2)

## ----precision-F1, echo=FALSE, eval=TRUE---------------------------------
true_and_false_positives_and_negatives <- function(dat,actual_column_name,predicted_column_name,positive_value){
    TN_number = length(which(dat[,actual_column_name] != positive_value & dat[,predicted_column_name] != positive_value))
    FP_number = length(which(dat[,actual_column_name] != positive_value & dat[,predicted_column_name] == positive_value))
    TP_number = length(which(dat[,actual_column_name] == positive_value & dat[,predicted_column_name] == positive_value))
    FN_number = length(which(dat[,actual_column_name] == positive_value & dat[,predicted_column_name] != positive_value))
return(data.frame(TN = TN_number,FP = FP_number,TP = TP_number,FN = FN_number)) }

precision_homebrew <- function(dat,actual_column_name,predicted_column_name,positive_value){
    true_and_false_for_this_dat <- true_and_false_positives_and_negatives(dat,actual_column_name,predicted_column_name,positive_value)
return(true_and_false_for_this_dat$TP/(true_and_false_for_this_dat$TP + true_and_false_for_this_dat$FP))
}

sensitivity_homebrew <- function(dat,actual_column_name,predicted_column_name,positive_value){
    true_and_false_for_this_dat <- true_and_false_positives_and_negatives(dat,actual_column_name,predicted_column_name,positive_value)
return(true_and_false_for_this_dat$TP/(true_and_false_for_this_dat$TP + true_and_false_for_this_dat$FN))
}

F1_score_homebrew <- function(dat,actual_column_name,predicted_column_name,positive_value){
    precision_this_dat <- precision_homebrew(dat,actual_column_name,predicted_column_name,positive_value)
    sensitivity_this_dat <- sensitivity_homebrew(dat,actual_column_name,predicted_column_name,positive_value)
return((2 * precision_this_dat * sensitivity_this_dat)/(precision_this_dat + sensitivity_this_dat))
}

model1_predictions_numeric <- predictions_model_incl_bigcity_age_binary_dis
model3_predictions_numeric <- predictions_model_minus_bigcity_age_dis

model1_predictions_factor <- ifelse(model1_predictions_numeric > 0.5,1,0)
model3_predictions_factor <- ifelse(model3_predictions_numeric > 0.5,1,0)

print("Precision model1:")
precision_homebrew(data.frame(Actual = crime$target,Predicted = model1_predictions_factor),"Actual","Predicted","1")

print("Precision model3:")
precision_homebrew(data.frame(Actual = crime$target,Predicted = model3_predictions_factor),"Actual","Predicted","1")

print("F1 model1:")
F1_score_homebrew(data.frame(Actual = crime$target,Predicted = model1_predictions_factor),"Actual","Predicted","1")

print("F1 model3:")
F1_score_homebrew(data.frame(Actual = crime$target,Predicted = model3_predictions_factor),"Actual","Predicted","1")

## ----cor-model1-vs-model3, echo=FALSE, eval=TRUE-------------------------
plot(model1_predictions_numeric,
model3_predictions_numeric,
xlab="Model #1 probability",
ylab="Model #3 probability",
col=ifelse(crime$big.city == 1,"red","black"))

legend("topleft",legend=c("Big city","Small city"),col=c("red","black"),lwd=3)

## ----roc-curves, echo=FALSE, eval=TRUE-----------------------------------
par(mfrow=c(1,2))

roc(response = crime$target,
predictor = model1_predictions_numeric,
plot=TRUE,
main="Model1")

roc(response = crime$target,
predictor = model3_predictions_numeric,
plot=TRUE,
main="Model3")

## ----replot-roc, echo=FALSE, eval=TRUE-----------------------------------
par(mfrow=c(1,1))
roc(response = crime$target,
predictor = model3_predictions_numeric,
plot=TRUE,
main="Model3",
print.thres=seq(from=0.2,to=0.8,by=0.1))

## ----test-thresh, echo=FALSE, eval=TRUE----------------------------------
print("Threshold = 0.6:")
caret::confusionMatrix(data = ifelse(predictions_model_minus_bigcity_age_dis > 0.6,1,0),
        reference = crime$target,
        positive="1")

print("Threshold = 0.7:")
caret::confusionMatrix(data = ifelse(predictions_model_minus_bigcity_age_dis > 0.7,1,0),
	reference = crime$target,
	positive="1")

print("Threshold = 0.8:")
caret::confusionMatrix(data = ifelse(predictions_model_minus_bigcity_age_dis > 0.8,1,0),
	reference = crime$target,
	positive="1")

## ----model-run-on-eval, echo=FALSE, eval=TRUE----------------------------
evaluation$age <- ifelse(evaluation$age >= 80,1,0)

final_predictions_evaluation <- predict(object = model_minus_bigcity_age_dis,
newdata = evaluation,
type="response")

final_predictions_evaluation <- data.frame(p = final_predictions_evaluation,
predicted.target = ifelse(final_predictions_evaluation > 0.5,1,0))

write.table(final_predictions_evaluation,
file="geiger_heather_final_predictions_evaluation.csv",
row.names=FALSE,col.names=TRUE,quote=FALSE,sep=",")


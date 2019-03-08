# Clearing objects from the workspace
rm(list=ls(all=TRUE))


library(dplyr)
library(ggplot2)
library(plm)
library(lmtest)
library(sandwich)
library(caret)
library(foreign)
library(gplots)

setwd("D:/STUDY/SEM2/ECONMETRICS/Project")


datag <- read.dta('guns.dta')
summary(datag)
hist(datag$rob) # Right-skewed
hist(log(datag$rob)) # Normal

hist(datag$vio) # Right-skewed
hist(log(datag$vio)) # Normal

hist(datag$mur) # Right-skewed
hist(log(datag$mur)) # Approx. normal

hist(datag$incarc_rate) # Right-skewed
hist(log(datag$incarc_rate)) # Normal

hist(datag$pb1064) # Right-skewed
hist(log(datag$pb1064)) # Approx. normal

hist(datag$pw1064) # Left-skewed
hist(datag$pw1064^3) # Approx. normal

hist(datag$pm1029) # Approx. normal

hist(datag$pop) # Right-skewed
hist(log(datag$pop)) # Approx. normal

hist(datag$avginc) # Right-skewed
hist(log(datag$avginc)) # Normal

hist(datag$density) # Right-skewed
hist(log(datag$density)) # Normal

# Log transforming all the right-skewed variables
datag$lnrob = log(datag$rob)
datag$lnincarc_rate = log(datag$incarc_rate)
datag$lnpb1064 = log(datag$pb1064)
datag$lnpop = log(datag$pop)
datag$lnavginc = log(datag$avginc)
datag$lndensity = log(datag$density)

datag$law_from_start<-0
datag$law_first_half<-0
datag$law_second_half<-0
datag$no_law<-0
datag$law_in_between <- 0

datag[which(datag$stateid %in% c(18,33,50,53)),20]<-1
datag[which(datag$year %in% c(1978:1988) & datag$shall==1),21]<-1
datag[which(datag$year %in% c(1988:1998) & datag$shall==1),22]<-1
datag[which(datag$stateid %in% c(1,6,8,9,10,11,15,17,19,20,24,25,26,27,29,31,34,35,36,39,44,55)),23]<-1
guns[which(guns$year %in% c(1978:1998) & guns$shall==0),]<-1

hist(datag$mur)
datag$mur <- log(datag$mur)

coplot(shall ~ year|stateid, type="l", data=datag) # Lines
coplot(shall ~ year|stateid, type="b", data=datag)

d1<-datag[which(datag$law_in_between==1),]
d2<-datag[which(datag$no_law==1),]


d1<-plm(mur ~ incarc_rate + pb1064 + pw1064 + pm1029 + avginc + I(avginc*avginc) + density + shall  ,index=c("stateid","year"), model="within", data=datag)
summary(d1)

d2<-plm(mur ~ incarc_rate + pb1064 + pw1064 + pm1029 + avginc + I(avginc*avginc) + density + shall  ,index=c("stateid","year"), model="pooling", data=datag)
summary(d2)

d3<-plm(mur ~ incarc_rate +log(pb1064) + I(pb1064*pb1064) + I(pb1064*pb1064*pb1064) + pw1064 + pm1029 + avginc + I(avginc*avginc) + density + shall  ,index=c("stateid","year"), model="pooling", data=datag)
summary(d3)

plot(datag$pb1064,datag$residuals)
plot(datag$pw1064,datag$residuals)

plot(datag$pb1064,datag$residuals)
plot(datag$pw1064,datag$residuals)

context3<- plm.data(datag,index=c("stateid","year"))
names(context3)

plotmeans(mur ~ stateid, main="Heterogeineity across States", data=context3)
plotmeans(vio ~ stateid, main="Heterogeineity across States", data=context3)
plotmeans(rob ~ stateid, main="Heterogeineity across States", data=context3)

plotmeans(mur ~ year, main="Heterogeineity across States", data=context3)
plotmeans(vio ~ year, main="Heterogeineity across States", data=context3)
plotmeans(rob ~ year, main="Heterogeineity across States", data=context3)

context3$avgcrime<-(context3$vio+context3$mur+context3$rob)/3
hist(context3$avgcrime)
hist(log(context3$avgcrime))

random_model <- plm(mur ~ incarc_rate + pb1064 + pw1064 + pm1029 + avginc + density  ,model="random",data=context3)
within_model <- plm(mur ~ incarc_rate + pb1064 + pw1064 + pm1029 + avginc + density  ,model="within",data=context3)
FE<-plm(mur ~ incarc_rate + pb1064 + pw1064 + pm1029 + avginc + density  ,model="within",data=context3)
pooling_model <- plm(mur ~ incarc_rate + pb1064 + pw1064 + pm1029 + avginc + density  ,model="pooling",data=context3)
pb1064sq<-context3$pb1064*context3$pb1064
pooling_model <- plm(mur ~ incarc_rate + I(incarc_rate*law_from_start) + I(incarc_rate*law_in_between) + pb1064sq + pw1064 + pm1029 + avginc + density  ,model="pooling",data=context3)

summary(FE)
summary(pooling_model)
summary(within_model)
summary(random_
        plot(xi, yi, 
             xlab="GDP", 
             ylab="edu exp", 
             type = "p")
        abline(model8)
        plot(model8$residuals)
        
        
        #write.csv(datag,"D:/Applied Econometrics/Data_sets/datag.csv")
        
        library(corrplot)
        M <- cor(as.matrix(context3[,3:16]))
        corrplot(M, method = "number")
        
        coplot(shall ~ year|stateid, type="l", data=context3) # Lines
        coplot(shall ~ year|stateid, type="b", data=context3)
        library(ggplot2)
        library(car)
        scatterplot(shall~year|stateid, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=context3)
        
        
        library(gplots)
        
        plotmeans(mur ~ stateid, main="Heterogeineity across States", data=context3)
        # plotmeansdraw a 95%
        #confidence interval
        #around the means
        
        #detach("package:gplots")
        
        plotmeans(mur ~ year, main="Heterogeineity across Years", data=context3)
        
        fixef(within_model)
        mean(fixef(within_model))
        
        summary(pooling_model)
        summary((within_model))
        
        
        library(car)
        scatterplot(shall~year|stateid, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=head(context3,46))
        
        coplot(shall ~ year|stateid, type="l", data=context3) # Lines
        coplot(shall ~ year|stateid, type="b", data=context3)
        
        
        #####  Hausman Test #####
        
        # It basically tests whether the unique errors (ui) are correlated with the regressors,
        # the null hypothesis is they are not.
        
        
        phtest(within_model, random_model)
        
        # Hausman Test
        # 
        # data:  shall ~ vio + mur + rob + incarc_rate + pb1064 + context3$pw1064 +  ...
        # chisq = 171.07, df = 10, p-value < 2.2e-16
        # alternative hypothesis: one model is inconsistent
        
        fixed.time<-plm(shall~vio+factor(year)+mur+rob+incarc_rate+pb1064+pw1064+pm1029+pop+avginc+density+law_from_start+law_in_between+no_law, data=datag, index=c("stateid", "year"), model="within")
        summary(fixed.time)
        
        BIC(within_model)
        BIC(within_model)
        
        pFtest(fixed.time, within_model)
        
        # F test for individual effects
        # 
        # data:  shall ~ vio + +factor(year) + mur + rob + incarc_rate + pb1064 +  ...
        # F = 4.1453, df1 = 22, df2 = 1090, p-value = 6.461e-10
        # alternative hypothesis: significant effects
        
        ##### Checking Serial Correlation ####
        
        pbgtest(within_model)
        
        # Breusch-Godfrey/Wooldridge test for serial correlation in panel models
        # 
        # data:  shall ~ vio + mur + rob + incarc_rate+pb1064+context3$pw1064+pm1029 + pop + avginc + density + law_from_start + law_in_between +     no_law
        # chisq = 700.05, df = 23, p-value < 2.2e-16
        # alternative hypothesis: serial correlation in idiosyncratic errors
        
        
        ### Check for Heteroskedasticity ###
        library(lmtest)
        bptest(fixed.time, data = context3, studentize=F)
        
        # Breusch-Pagan test
        # 
        # data:  fixed.time
        # BP = 379.12, df = 34, p-value < 2.2e-16
        
        # There is a Heteroskedasticty in the model
        # we can remove that using HAC estimators
        
        coeftest(fixed.time, vcovHC)model)



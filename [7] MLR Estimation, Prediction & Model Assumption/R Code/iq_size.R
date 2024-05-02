iqsize <- read.table("~/path-to-folder/iqsize.txt", header=T)
head(iqsize)
attach(iqsize)

model <- lm(PIQ ~ Brain + Height, data = iqsize)
predict(model, interval="confidence", se.fit=T,
        newdata=data.frame(Brain=90, Height=70))
# $fit
#        fit      lwr     upr
# 1 105.6391 98.23722 113.041
# 
# $se.fit
# [1] 3.646064

predict(model, interval="confidence", se.fit=T,
        newdata=data.frame(Brain=79, Height=62))
# $fit
#        fit      lwr     upr
# 1 104.8114 91.41796 118.2049
# 
# $se.fit
# [1] 6.597407

predict(model, interval="prediction",
        newdata=data.frame(Brain=90, Height=70))
# $fit
#        fit      lwr     upr
# 1 105.6391 65.34688 145.9314

detach(iqsize)

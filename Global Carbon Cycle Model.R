
rm(list=ls())
cat("\014")


#-------------------------------------------------------------  1.1
rw <- c(rep(1900:2000))
df <- setNames(data.frame(matrix(ncol = 5, nrow = 101)), c("year","na", "nb", "nu", "nd")
)

df[1] <- rw

#Model Parameters

na= 615
kab=1/10 
kba=1/12 
kau=1/10 
kua=1/14 
kud=1/14 
kdu=1/595

g=rep(0,101)
g[1]<- 5
dt<- c(.25, .5, 1, 2, 4, 6, 8)
#Steaddt State Model

df[1,2] <- na
df[1,3] <- (kab*na)/(kba)
df[1,4] <- (kau*na)/(kua)
df[1,5] <- (kud*df[1,4])/(kdu)

#Introducing the emission of 5Gt C in year 1900 @ 0 for rest all years (this stands for all simulations),
# considerng time step dt=1

for(i in 1:100){
  
  df[1+i,1] <- df[i, 1] + dt[3]
  df[1+i,2] <- df[i,2]+((-kab-kau)*df[i,2]+kba*df[i,3]+kua*df[i,4]+g[i])*dt[3]
  df[i+1,3] <- df[i,3]+(df[i,2]*kab+(-kba)*df[i,3])*dt[3]
  df[i+1,4] <- df[i,4]+ (kau*df[i,2]-(kud+kua)*df[i,4]+kdu*df[i,5])*dt[3]
  df[1+i,5] <- df[i,5]+ (kud*df[i,4]+(-kdu)*df[i,5])*dt[3]
}

data1 <- df

#Change in CO2 concentration

for (i in 1:100){
  
  data1[i+1, 6] <- (data1[i+1,2]-data1[i, 2]) 
}

#Difference between disturbed and reference concentration

for (i in 1:100){
  
  data1[i+1, 7] <- (data1[i+1, 2]-data1[1,2]) 
}

#Plotting 
#a. Change in atmospheric CO2 concentration plotting

ggplot(data1,aes(x=year))+
  geom_line(aes(y=V6),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("Change in CO2 Concentration")+
  xlab("Year")+
  ylab("na(i)-na(i-1)  Gt C")+
  theme(text = element_text(size=13))

#b. Difference between disturbed and reference concentration

ggplot(data1,aes(x=year))+
  geom_line(aes(y=V7),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("Difference between disturbed and reference concentration")+
  xlab("Year")+
  ylab("na-na0   Gt C")+
  theme(text = element_text(size=13))

#--------------------------------------------------------------------------------- 1.2
#Atmospheric cincentration calculation in dependencdt of time step

data <- setNames(data.frame(matrix(ncol = 9, nrow = 480)), c("year","na", "nb", "nu", "nd", "na-na0",
                                                             "nb-nb0", "nu-nu0", "nd-nd0")
)
data[1,2] <- na
data[1,3] <- (kab*na)/(kba)
data[1,4] <- (kau*na)/(kua)
data[1,5] <- (kud*df[1,4])/(kdu)
data[1,1] <- 1900
gm=rep(0,481)
gm[1]<- 5


#Time step 0.25
for(i in 1:480){
  
  data[1+i,1] <- data[i, 1] + dt[1]
  data[1+i,2] <- data[i,2]+((-kab-kau)*data[i,2]+kba*data[i,3]+kua*data[i,4]+gm[i])*dt[1]
  data[i+1,3] <- data[i,3]+(data[i,2]*kab+(-kba)*data[i,3])*dt[1]
  data[i+1,4] <- data[i,4]+ (kau*data[i,2]-(kud+kua)*data[i,4]+kdu*data[i,5])*dt[1]
  data[1+i,5] <- data[i,5]+ (kud*data[i,4]+(-kdu)*data[i,5])*dt[1]
  
  data[1+i,6] <- data[1+i,2]-data[1,2]
  data[1+i,7] <- data[1+i,3]-data[1,3]
  data[1+i,8] <- data[1+i,4]-data[1,4]
  data[1+i,9] <- data[1+i,5]-data[1,5]
  }  
data_0.25 <- data

#Plottng Difference between disturbed and reference concentration
#Atmosphere
ggplot(data_0.25,aes(x=year))+
  geom_line(aes(y=data_0.25$`na-na0`),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("Difference between disturbed and reference concentration(dt=0.25)")+
  xlab("Year")+
  ylab("na-na0   Gt C")+
  theme(text = element_text(size=13))

#Deep Ocean
ggplot(data_0.25,aes(x=year))+
  geom_line(aes(y=data_0.25$`nd-nd0`),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("Difference between disturbed and reference concentration(dt=0.25)")+
  xlab("Year")+
  ylab("nd-nd0   Gt C")+
  theme(text = element_text(size=13))


#Time step 0.5 
for(i in 1:480){
  
  data[1+i,1] <- data[i, 1] + dt[2]
  data[1+i,2] <- data[i,2]+((-kab-kau)*data[i,2]+kba*data[i,3]+kua*data[i,4]+gm[i])*dt[2]
  data[i+1,3] <- data[i,3]+(data[i,2]*kab+(-kba)*data[i,3])*dt[2]
  data[i+1,4] <- data[i,4]+ (kau*data[i,2]-(kud+kua)*data[i,4]+kdu*data[i,5])*dt[2]
  data[1+i,5] <- data[i,5]+ (kud*data[i,4]+(-kdu)*data[i,5])*dt[2]
  
  data[1+i,6] <- data[1+i,2]-data[1,2]
  data[1+i,7] <- data[1+i,3]-data[1,3]
  data[1+i,8] <- data[1+i,4]-data[1,4]
  data[1+i,9] <- data[1+i,5]-data[1,5]
  
}  
  
data_0.5 <- data

#Plottng Difference between disturbed and reference concentration
#Atmosphere
ggplot(data_0.5,aes(x=year))+
  geom_line(aes(y=data_0.5$`na-na0`),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("Difference between disturbed and reference concentration(dt=0.5)")+
  xlab("Year")+
  ylab("na-na0   Gt C")+
  theme(text = element_text(size=13))

#Deep Ocean
ggplot(data_0.5,aes(x=year))+
  geom_line(aes(y=data_0.5$`nd-nd0`),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("Difference between disturbed and reference concentration(dt=0.5)")+
  xlab("Year")+
  ylab("nd-nd0   Gt C")+
  theme(text = element_text(size=13))

#Time step 1
for(i in 1:480){
  
  data[1+i,1] <- data[i, 1] + dt[3]
  data[1+i,2] <- data[i,2]+((-kab-kau)*data[i,2]+kba*data[i,3]+kua*data[i,4]+gm[i])*dt[3]
  data[i+1,3] <- data[i,3]+(data[i,2]*kab+(-kba)*data[i,3])*dt[3]
  data[i+1,4] <- data[i,4]+ (kau*data[i,2]-(kud+kua)*data[i,4]+kdu*data[i,5])*dt[3]
  data[1+i,5] <- data[i,5]+ (kud*data[i,4]+(-kdu)*data[i,5])*dt[3]
  
  data[1+i,6] <- data[1+i,2]-data[1,2]
  data[1+i,7] <- data[1+i,3]-data[1,3]
  data[1+i,8] <- data[1+i,4]-data[1,4]
  data[1+i,9] <- data[1+i,5]-data[1,5]
}

data_1 <- data


#Time step 2  
for(i in 1:480){
  
  data[1+i,1] <- data[i, 1] + dt[4]
  data[1+i,2] <- data[i,2]+((-kab-kau)*data[i,2]+kba*data[i,3]+kua*data[i,4]+gm[i])*dt[4]
  data[i+1,3] <- data[i,3]+(data[i,2]*kab+(-kba)*data[i,3])*dt[4]
  data[i+1,4] <- data[i,4]+ (kau*data[i,2]-(kud+kua)*data[i,4]+kdu*data[i,5])*dt[4]
  data[1+i,5] <- data[i,5]+ (kud*data[i,4]+(-kdu)*data[i,5])*dt[4]
  
  data[1+i,6] <- data[1+i,2]-data[1,2]
  data[1+i,7] <- data[1+i,3]-data[1,3]
  data[1+i,8] <- data[1+i,4]-data[1,4]
  data[1+i,9] <- data[1+i,5]-data[1,5]
  
}

data_2 <- data


#Plottng Difference between disturbed and reference concentration
#Atmosphere
ggplot(data_2,aes(x=year))+
  geom_line(aes(y=data_2$`na-na0`),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("Difference between disturbed and reference concentration(dt=2)")+
  xlab("Year")+
  ylab("na-na0   Gt C")+
  theme(text = element_text(size=13))

#Deep Ocean
ggplot(data_2,aes(x=year))+
  geom_line(aes(y=data_2$`nd-nd0`),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("Difference between disturbed and reference concentration(dt=2)")+
  xlab("Year")+
  ylab("nd-nd0   Gt C")+
  theme(text = element_text(size=13))


#Time step 4
for(i in 1:480){
  
  data[1+i,1] <- data[i, 1] + dt[5]
  data[1+i,2] <- data[i,2]+((-kab-kau)*data[i,2]+kba*data[i,3]+kua*data[i,4]+gm[i])*dt[5]
  data[i+1,3] <- data[i,3]+(data[i,2]*kab+(-kba)*data[i,3])*dt[5]
  data[i+1,4] <- data[i,4]+ (kau*data[i,2]-(kud+kua)*data[i,4]+kdu*data[i,5])*dt[5]
  data[1+i,5] <- data[i,5]+ (kud*data[i,4]+(-kdu)*data[i,5])*dt[5]
  
  data[1+i,6] <- data[1+i,2]-data[1,2]
  data[1+i,7] <- data[1+i,3]-data[1,3]
  data[1+i,8] <- data[1+i,4]-data[1,4]
  data[1+i,9] <- data[1+i,5]-data[1,5]
}

data_4 <- data

#Plottng Difference between disturbed and reference concentration
#Atmosphere
ggplot(data_4,aes(x=year))+
  geom_line(aes(y=data_4$`na-na0`),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("Difference between disturbed and reference concentration(dt=4)")+
  xlab("Year")+
  ylab("na-na0   Gt C")+
  theme(text = element_text(size=13))

#Deep Ocean
ggplot(data_4,aes(x=year))+
  geom_line(aes(y=data_4$`nd-nd0`),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("Difference between disturbed and reference concentration(dt=4)")+
  xlab("Year")+
  ylab("nd-nd0   Gt C")+
  theme(text = element_text(size=13))


#Time step 6
for(i in 1:480){
  
  data[1+i,1] <- data[i, 1] + dt[6]
  data[1+i,2] <- data[i,2]+((-kab-kau)*data[i,2]+kba*data[i,3]+kua*data[i,4]+gm[i])*dt[6]
  data[i+1,3] <- data[i,3]+(data[i,2]*kab+(-kba)*data[i,3])*dt[6]
  data[i+1,4] <- data[i,4]+ (kau*data[i,2]-(kud+kua)*data[i,4]+kdu*data[i,5])*dt[6]
  data[1+i,5] <- data[i,5]+ (kud*data[i,4]+(-kdu)*data[i,5])*dt[6]
  
  data[1+i,6] <- data[1+i,2]-data[1,2]
  data[1+i,7] <- data[1+i,3]-data[1,3]
  data[1+i,8] <- data[1+i,4]-data[1,4]
  data[1+i,9] <- data[1+i,5]-data[1,5]
}

data_6 <- data  

#Plottng Difference between disturbed and reference concentration
#Atmosphere
ggplot(data_6,aes(x=year))+
  geom_line(aes(y=data_6$`na-na0`),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("Difference between disturbed and reference concentration(dt=6)")+
  xlab("Year")+
  ylab("na-na0   Gt C")+
  theme(text = element_text(size=13))

#Deep Ocean
ggplot(data_6,aes(x=year))+
  geom_line(aes(y=data_6$`nd-nd0`),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("Difference between disturbed and reference concentration(dt=6)")+
  xlab("Year")+
  ylab("nd-nd0   Gt C")+
  theme(text = element_text(size=13))

#Time step 8  
for(i in 1:480){
  
  data[1+i,1] <- data[i, 1] + dt[7]
  data[1+i,2] <- data[i,2]+((-kab-kau)*data[i,2]+kba*data[i,3]+kua*data[i,4]+gm[i])*dt[7]
  data[i+1,3] <- data[i,3]+(data[i,2]*kab+(-kba)*data[i,3])*dt[7]
  data[i+1,4] <- data[i,4]+ (kau*data[i,2]-(kud+kua)*data[i,4]+kdu*data[i,5])*dt[7]
  data[1+i,5] <- data[i,5]+ (kud*data[i,4]+(-kdu)*data[i,5])*dt[7]
  
  data[1+i,6] <- data[1+i,2]-data[1,2]
  data[1+i,7] <- data[1+i,3]-data[1,3]
  data[1+i,8] <- data[1+i,4]-data[1,4]
  data[1+i,9] <- data[1+i,5]-data[1,5]
}
  
data_8 <- data 

#Plottng Difference between disturbed and reference concentration
#Atmosphere
ggplot(data_8,aes(x=year))+
  geom_line(aes(y=data_8$`na-na0`),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("Difference between disturbed and reference concentration(dt=8)")+
  xlab("Year")+
  ylab("na-na0   Gt C")+
  theme(text = element_text(size=13))

#Deep Ocean
ggplot(data_8,aes(x=year))+
  geom_line(aes(y=data_8$`nd-nd0`),color="steelblue",size=1.2,alpha=0.9,linetype=1)+
  ggtitle("Difference between disturbed and reference concentration(dt=8)")+
  xlab("Year")+
  ylab("nd-nd0   Gt C")+
  theme(text = element_text(size=13))


#Error Norm 

#L2 norm is considered, Root Mean Squared Error (RMSE) 
#na value from the time step 0.25 is considered as  reference solution as it is the smallest dt
#E(t) = R(t) - A(t)
# R= Reference ; A= Approximate



#Norm calculation (L2)

library(Metrics)

l1 <- rmse(data_0.25$na, data_0.25$na)
l2 <- rmse(data_0.25$na, data_0.5$na)
l3 <- rmse(data_0.25$na, data_1$na)
l4 <- rmse(data_0.25$na, data_2$na)
l5 <- rmse(data_0.25$na, data_4$na)
l6 <- rmse(data_0.25$na, data_6$na)
l7 <- rmse(data_0.25$na, data_8$na)

norms <- c(l1, l2, l3, l4, l5, l6, l7)
q <- data.frame(dt, norms)

#Dependency of error norms on time step 

cor(dt, norms)
cor.test(dt, norms)

#Correlation = 0.72

#Plotting result
plot(q$dt, q$norms,  xlim=c(0,8),ylim=c(0,2), xlab= "Time Step", ylab= "Error Norm", col= "blue")
title("Error Norm Plotting")

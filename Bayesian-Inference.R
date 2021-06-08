### 0. Load library ###
setwd("/Users/bomin/user/mymac/대학원/21-1 베이지안특론/기말프로젝트")
library(rjags)
library(readxl)
library(runjags)
library(MASS)

### 1. Data Preprocessing ###
data <- read_excel('/Users/bomin/user/mymac/대학원/21-1 베이지안특론/기말프로젝트/data.xlsx')
hist(data$Death)
table(data$Death)
colnames(data)
# 데이터 형변환
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], as.factor)
data$Death <- as.numeric(data$Death)
str(data)
summary(data)

### 2. GLM ###
glm.summary<-glm(Death ~ . , data, family=poisson(link="log") )
summary(glm.summary)
yhat<-predict(glm.summary, data[2:23], type='response')
par(mfrow=c(1,2))
hist(y, freq=FALSE, xlab="raw data") 
hist(yhat, freq=FALSE, xlab="log-linear fit")

### 3. ZIP ###
y <- data$Death 
# 원조 제로 확률에 대한 연결함수
X <- cbind(rep(1,length(y)), data$Gender, data$Age, data$`V.r/Ship`, data$Edu.level, data$V.type, data$Ac.time, data$Location, data$Enviroment, data$R.division, data$`Ac. Type`, data$Cause)
# 포아송 분포의 로그 연결함수
Z <- cbind(rep(1,length(y)), data$Gender, data$Age, data$`V.r/Ship`, data$Edu.level, data$V.type, data$Ac.time, data$Location, data$Enviroment, data$R.division, data$`Ac. Type`, data$Cause)
# 포아송 분포의 로그 연결함수
p <- ncol(X); q <- ncol(Z)
modelString<-"model{
  # Likelihood
  for(i in 1:length(y)){
    y[i] ~ dpois(mu.pois[i])
    mu.pois[i] <- (1-S[i])*lambda[i]+1e-10*S[i] 
    log(lambda[i]) <- inprod ( X[i,], beta[] )
    S[i] ~ dbern( omega[i] )
    logit(omega[i]) <- inprod ( Z[i,], gamma[] )}
  
  # Prior
  for (i in 1:p){beta[i] ~ dnorm( mu.beta[i], Tau.beta[i] )}
  for(i in 1:q){gamma[i] ~ dnorm( mu.gamma[i], Tau.gamma[i] )}}"
writeLines(modelString, "model_ZIP_mixture.txt")

# prior parameters
mu.beta=rep(0,p)
Tau.beta=rep(0.01,p)
mu.gamma=rep(0,q)
Tau.gamma=rep(0.01,q)
glm.out=glm(y~X-1, family="poisson")
beta.pois=as.vector(glm.out$coefficients)
dataList=list(p=p, q=q, y=y, X=X, Z=Z, mu.beta=mu.beta, Tau.beta=Tau.beta,mu.gamma=mu.gamma, Tau.gamma=Tau.gamma) 
initsList=list(beta=beta.pois, gamma=mu.gamma)

jagsModel.zip=jags.model(file="model_ZIP_mixture.txt", data=dataList, inits=initsList, n.chains=3, n.adapt=1000) 
update(jagsModel.zip, n.iter=3000)
codaSamples=coda.samples(jagsModel.zip, variable.names=c("beta","gamma"), thin=10, n.chains=3, n.iter=10000)
coda::gelman.diag(codaSamples)
summary(codaSamples)

dic.zip=dic.samples(jagsModel.zip,30000); dic.zip

mcmcSamples.zip=as.matrix(codaSamples) 
para.names = variable.names(codaSamples[[1]])
# 경로그림
par(mfrow=c(3,2),mar=c(5,2,4,1))
for(i in c(seq(2,12),seq(14,24))){
  coda::traceplot(codaSamples[,i], main=para.names[i])}
# 자기상관함수
par(mfrow=c(3,2),mar=c(5,2,4,1))
for(i in c(seq(2,12),seq(14,24))){
  acf(codaSamples[,i][[1]],plot = TRUE, main=para.names[i])}
# 사후밀도함수
par(mfrow=c(3,2),mar=c(5,2,4,1))
for(i in c(seq(2,12),seq(14,24))){
  plot( density(mcmcSamples.zip[,i]), xlab="", main=para.names[i])}
  
### 4. ZINB ###
y <- data$Death 
# 원조 제로 확률에 대한 연결함수
X <- cbind(rep(1,length(y)), data$Gender, data$Age, data$`V.r/Ship`, data$Edu.level, data$V.type, data$Ac.time, data$Location, data$Enviroment, data$R.division, data$`Ac. Type`, data$Cause)
# 포아송 분포의 로그 연결함수
Z <- cbind(rep(1,length(y)), data$Gender, data$Age, data$`V.r/Ship`, data$Edu.level, data$V.type, data$Ac.time, data$Location, data$Enviroment, data$R.division, data$`Ac. Type`, data$Cause)
# 포아송 분포의 로그 연결함수
p <- ncol(X); q <- ncol(Z)
modelString_ZINB<-"model{
  # Likelihood
  for(i in 1:length(y)){
    y[i] ~ dnegbin(pi[i],r[i])
    pi[i] <- r[i]/(r[i]+mu[i])
    mu[i]=(1-S[i])*lambda[i] + 1e-10*S[i]
    log(lambda[i]) <- inprod ( X[i,], beta[] )
    S[i] ~ dbern( omega[i] )
    logit(omega[i]) <- inprod ( Z[i,], gamma[] )}
  
  # Prior
  for(i in 1:length(y)){r[i] ~ dunif(0,150)}
  for (i in 1:p){beta[i] ~ dnorm( mu.beta[i], Tau.beta[i] )}
  for(i in 1:q){gamma[i] ~ dnorm( mu.gamma[i], Tau.gamma[i] )}}"
writeLines(modelString_ZINB, "model_ZINB_mixture.txt")

# prior parameters
mu.beta=rep(0,p)
Tau.beta=rep(0.01,p)
mu.gamma=rep(0,q)
Tau.gamma=rep(0.01,q)
glm.nb=glm.nb(y~X-1)
beta.nb=as.vector(glm.nb$coefficients)
dataList=list(p=p, q=q, y=y, X=X, Z=Z, mu.beta=mu.beta, Tau.beta=Tau.beta, mu.gamma=mu.gamma, Tau.gamma=Tau.gamma) 
initsList=list(beta=beta.nb, gamma=mu.gamma)

jagsModel.zinb=jags.model(file="model_ZINB_mixture.txt", data=dataList, inits=initsList, n.chains=3, n.adapt=1000) 
update(jagsModel.zinb, n.iter=3000)
codaSamples.zinb=coda.samples(jagsModel.zinb, variable.names=c("beta","gamma"), thin=10, n.chains=3, n.iter=10000)
coda::gelman.diag(codaSamples.zinb)
summary(codaSamples.zinb)

dic.zinb=dic.samples(jagsModel.zinb,30000); dic.zinb

mcmcSamples.zinb=as.matrix(codaSamples.zinb)
para.names = variable.names(codaSamples.zinb[[1]])
# 경로그림
par(mfrow=c(3,2),mar=c(5,2,4,1))
for(i in c(seq(2,12),seq(14,24))){
  coda::traceplot(codaSamples.zinb[,i], main=para.names[i])}
# 자기상관함수
par(mfrow=c(3,2),mar=c(5,2,4,1))
for(i in c(seq(2,12),seq(14,24))){
  acf(codaSamples.zinb[,i][[1]], plot = TRUE, main=para.names[i])}
# 사후밀도함수
par(mfrow=c(3,2),mar=c(5,2,4,1))
for(i in c(seq(2,12),seq(14,24))){
  plot( density(mcmcSamples.zinb[,i]),  main=para.names[i])}

X=diabetes
mean(X)
xbarra=colMeans(X)
var(X)
S=cov(X)
cor(X)
hist(X[,1])
boxplot(X[,1])
plot(X[,5],X[,6])
pairs(X[,5:8])
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(X[,1],X[,2],X[,3])
install.packages("rgl")
library(rgl)
plot3d(X[,1],X[,2],X[,3])

n=length(X[,1])
x=matrix(0,n,2)
x[,1]=diabetes[,2]
x[,2]=diabetes[,3]


xbarra=colMeans(x)
S=var(x)
d2=matrix(0,n,1)
for (i in 1:n){
  d2[i]=t(x[i,]-xbarra)%*%solve(S)%*%(x[i,]-xbarra)
}

hist(diabetes[,2])
hist(diabetes[,3])
plot(diabetes[,2],diabetes[,3])

1-pchisq(14.44,2)
1-pchisq(8.29,2)
mahalanobis(x,center=xbarra,cov=S)


shapiro.test(X[,5])
shapiro.test(X[,6])
x56=matrix(0,n,2)
x56=X[,5]
x56=X[,6]


install.packages("mvnormalTest")
library(mvnormalTest)
mardia(x56)


plot(altura_peso$altura,altura_peso$peso)

#IC nao simultaneos 95%:
#altura:
LI_altura=mean(altura_peso$altura)-1.96*sqrt(var(altura_peso$altura)/29)
LS_altura=mean(altura_peso$altura)+1.96*sqrt(var(altura_peso$altura)/29)
#peso:
LI_peso=mean(altura_peso$peso)-1.96*sqrt(var(altura_peso$peso)/29)
LS_peso=mean(altura_peso$peso)+1.96*sqrt(var(altura_peso$peso)/29)

LI_altura
LS_altura
LI_peso
LS_peso

#IC simultaneos 95%
quantil_F=qf(0.95,2,27)
quantil_F

#altura:
LI_altura=mean(altura_peso$altura)-sqrt(2*28/27*quantil_F*var(altura_peso$altura)/29)
LS_altura=mean(altura_peso$altura)+sqrt(2*28/27*quantil_F*var(altura_peso$altura)/29)
#peso:
LI_peso=mean(altura_peso$peso)-sqrt(2*28/27*quantil_F*var(altura_peso$peso)/29)
LS_peso=mean(altura_peso$peso)+sqrt(2*28/27*quantil_F*var(altura_peso$peso)/29)

LI_altura
LS_altura
LI_peso
LS_peso

mi0=c(1.70,64)
xbarra=colMeans(altura_peso)
S=cov(altura_peso)
estat_teste=29*27/(2*28)*t(xbarra-mi0)%*%solve(S)%*%(xbarra-mi0)
estat_teste
1-pf(estat_teste,2,27)

install.packages("DescTools")
library(DescTools)
HotellingsT2Test(altura_peso,mu=c(1.68,68))

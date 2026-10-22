head(iris)
iris
S=cov(iris[,1:4])
S
eigen(S)
n=length(iris[,1])
X=matrix(0,n,4)
X[,1]=iris$Sepal.Length
X[,2]=iris$Sepal.Width
X[,3]=iris$Petal.Length
X[,4]=iris$Petal.Width

Y1=eigen(S)$vectors[,1]%*%t(X)
Y1=t(Y1)
Y1
Y2=eigen(S)$vectors[,2]%*%t(X)
Y2=t(Y2)
Y2
Y3=eigen(S)$vectors[,3]%*%t(X)
Y3=t(Y3)
Y3
Y4=eigen(S)$vectors[,4]%*%t(X)
Y4=t(Y4)
Y4

eigen(S)
cor(Y1,Y2)
cor(Y2,Y4)

var(Y1)
var(Y2)


var(iris$Sepal.Length)
var(iris$Sepal.Width)
var(iris$Petal.Length)
var(iris$Petal.Width)

4.22824171/(4.22824171+0.24267075 +0.07820950+0.02383509)

R=cor(X)
eigen(R)


2.91849782/(2.91849782+0.91403047+0.14675688+0.02071484)


mtcars

R=cor(mtcars[,2:7])

eigen(R)
mtcars1=mtcars[,2:7]
prcomp_mtcars=prcomp(mtcars1,scale.=TRUE)
2.0609798^2

4.24763781 +1.14782496+ 0.33046271 +0.13164571 +0.07779941 +0.06462941
4.24763781/6
(4.24763781 +1.14782496)/6


PC1=summary(prcomp_mtcars)$x[,1]
PC2=summary(prcomp_mtcars)$x[,2]
plot(PC1,PC2)
cor(PC1,PC2)

lm_mtcars=lm(mpg~cyl+disp+hp+drat+wt+qsec,data=mtcars)
summary(lm_mtcars)

lm_mtcars_PCA=lm(mpg~PC1+PC2,data=mtcars)
summary(lm_mtcars_PCA)

cor(iris[,1:4])

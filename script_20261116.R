


questions_scale=questions
for (i in 1:20)
questions_scale[,i]=scale(questions[,i])

resultado=factanal(questions_scale,factors=4,scores="Bartlett")
resultado$scores



R=cor(century[,2:7])
eigen(R)
4.4926254/6

plot(eigen(R)$values)

century_scale=century
for (i in 2:7)
  century_scale[,i]=scale(century[,i])

resultado=factanal(century_scale[,2:7],factors=1,scores="Bartlett")
resultado

resultado$scores

century$scores=resultado$scores


mtcars
X=matrix(0,32,5)
X[,1]=mtcars$disp
X[,2]=mtcars$hp
X[,3]=mtcars$drat
X[,4]=mtcars$wt
X[,5]=mtcars$qsec
X
R=cor(X)
eigen(R)
3.33874601/5
(3.33874601+1.14334450)/5


Z=X
for (i in 1:5)
  Z[,i]=scale(X[,i])


resultado=factanal(Z,factors=2,scores="Bartlett")
resultado
resultado$scores

modelo=lm(mtcars$mpg~resultado$scores[,1]+
            resultado$scores[,2])
summary(modelo)

plot(resultado$scores[,1],mtcars$mpg)
plot(resultado$scores[,2],mtcars$mpg)

modelo1=lm(mtcars$mpg~X[,4]+X[,5])
summary(modelo1)

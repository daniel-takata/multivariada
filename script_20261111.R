cor(questions)
var(questions)
0.27/0.34
0.23/0.34
R=cor(questions)
eigen(R)
3.6127691 /20
(3.6127691+3.3696062)/20
(3.6127691+3.3696062+3.0825251)/20
(3.6127691+3.3696062+3.0825251+2.4656282)/20

auto=eigen(R)
L=matrix(0,20,4)
for (i in 1:4)
  L[,i]=auto$vectors[,i]*sqrt(auto$values[i])
L

L%*%t(L)

R-L%*%t(L)


L
varimax(L)


factanal(factors=4,covmat=R)



install.packages("psych")
library(psych)
fa(questions)
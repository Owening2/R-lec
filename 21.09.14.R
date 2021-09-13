# 1. for문으로 다음과 같이 월 이름을 출력
# The month of January
# ...
# The month of December

for(i in month.name){
  cat("The month of",i,'\n')
}

# 2. 짝수이면 TRUE, 홀수이면 FALSE를 출력하는 함수 작성.
# 다음 벡터로 테스트하시오.
# c(-5:5)

x <- c(-5:5)
y <- ifelse(x%%2 == 0 , TRUE, FALSE)
c(y)

# 3. 짝수 개수를 세는 함수 작성.
# 다음 벡터로 테스트하시오.
# c(-5:5)

x <- c(-5:5)
ev_c <- 0
ev_f<-function(x){
  ev_c <- ifelse(x%%2 == 0 , 1, 0)
  sum(ev_c)
}
ev_f(x)


# 4. 주어진 숫자가 원주율보다 크면 TRUE, 아니면 FALSE를 출력하는 함수 작성.
# 3과 1:5 벡터에 대해 테스트하시오

t1 <- c(3)
t2 <- c(1:5)

pie_comp <- function(x){
  ifelse(x>pi, TRUE,FALSE)
}
pie_comp(t1)
pie_comp(t2)

# 5. 2~99까지 수에 대해
# - 3의 배수에 해당하는 수의 합계를 구하시오.
# - 3의 배수에 해당하는 수의 개수를 구하시오.

t3<-c(2:99)
t3<-ifelse(t3%%3==0,t3,0)
sum(t3)
length(t3[which(t3>0)])

# 6. 임의의 수 n을 전달받아, n!을 출력하는 함수를 완성하시오. (n>=2, 5!=5*4*3*2*1)

cus_fac<-function(n){
  a<-c(1:n)
  b<-1
  for(i in a){
    b<- b*i
  } 
  return(b)}
cus_fac(5)


# 7. 반복문을 이용하여 구구단을 출력하시오
for(i in 2:9)
for(j in 1:9)
cat(i,"*",j," = ",i*j,"\n")

# 8. 반복문을 활용하여 출력하시오
#      *
#     ***
#    *****
#   *******


for (i in 1:5){
  for (j in 1:(5-i)){
    if (5-i >0) 
      cat(" ")}
  for (j in 1:(i*2-1)){
    cat("*")
  }
  cat("\n")}
  }


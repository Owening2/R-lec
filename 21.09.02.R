iris
"""
150건 데이터
1~100번까지 데이터 추출 -> Petal.Length Petal.Width 열 저장
-> 회귀모델 : 독립변수(Petal.Length), 종속변수(Petal.Width)
-> 회귀모델의 기울기, 절편 구해지면

101번~150번 까지 데이터 추출 -> Petal.Length Petal.width 열 저장
-> 이미 구한 회귀모델의 기울기, 절편을 이용하여 Petal.Length를 입력 했을때 Petal.Width 예측하여 출력

cost도 함께 출력(예측값 - 실제값) 제곱의 평균
"""
iris_df<-iris[1:100,3:4]
iris_df
iris_model <- lm(Petal.Length ~ Petal.Width, data = iris_df)
summary(iris_model)

x<-iris_df$Petal.Length
y<-iris_df$Petal.Width
slope<-cor(x,y)*(sd(y)/sd(x))
intercept<-mean(y)-s*mean(x)

iris_df2<-iris[101:150,3:4]
yhat <- slope*iris_df2$Petal.Length+intercept
print(yhat)
cost <- mean((yhat-iris_df2$Petal.Width)^2)
print(cost)

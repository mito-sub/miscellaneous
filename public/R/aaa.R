## 脱落変数バイアス(Omitted Variable Bias, OVB)のシミュレーション
## モデル: y = a + bx_1 + cx_2 + dx_3 + fz + e
## z ~ exp(3*x_1 - 4*x_2)/(1 + exp(3*x_1 - 4*x_2)),
## x_1, x_2, x_3 ~ N(0, 3), e~N(0, 1)

library(corrr)

logistic_transform = function(x_1, x_2){
  exp1 = exp(2*x_1 -3*x_2)
  return(exp1/(1+exp1))
}
logistic_transform(2,3)
logistic_transform(-3,2)

a = 2
b = 2.5
c = -1.3
d = 3 # 介入効果

n = 300 # サンプル数
set.seed(123)
x1 = rnorm(n, 0, 3)
x2 = rnorm(n, 0, 3)
x3 = rnorm(n, 0, 3)
x4 = rnorm(n, 0, 3)
e = rnorm(n,0,0.2)
z = logistic_transform(x1, x4)
y = a + b*x1 + c*x3 + d*z + e
plot(z,y)
plot(x1,y)
plot(x2, y)
plot(x3, y)
plot(x4, y) # 当然y, x4は無相関
plot(x1, z)
plot(x2, z)
plot(x3, z)
plot(x4, z) # x4, zは負の相関

df = data.frame(y=y, x1 = x1, x2 = x2, x3 = x3, x4 = x4, z = z)
head(df)
correlate(df)
# 正しいモデル 
res.true = lm(y~x1+x3+z, data = df)
summary(res.true) # 当然、介入効果(zの係数)はほとんど正しく推定されている

# full model(x1 ~ x4, zをふくめたモデル)
res.full = lm(y~., data = df)
summary(res.full) 
# 目的変数と割当に関係ない変数x2,目的変数に関係ない変数を含めた場合だが、
# そこまで因果効果が歪んでない
# -> 色々含めるのは悪くない

#--- では、必要な変数を含めないとどうなるのか見ていく ---#
# 割当Zとは相関の弱いx3を抜いたモデル
res = lm(y~x1+x4+z, data=df)
summary(res) # 割当の効果(zの係数)が大きめに推定されているがそこまで歪んでいるわけではない


## 割当Zと目的変数Yの両方と相関が強いx1を抜く
res = lm(y~x3 + x4+ z, data = df)
summary(res) # すっごい過大に割当の効果が推定された

## ## 目的変数Yとは相関が弱いx4を抜く(=正しいモデル)
res = lm(y~x1 + x3 + z, data = df)
summary(res) ## 当然正しく推定されている



## 割当と目的変数の両方に相関する変数を抜くとバイアスが大きくなる
## 逆に、目的変数のみに相関する変数を抜いてもバイアスはそこまで大きくない
## 割当のみに相関する変数を抜くと、

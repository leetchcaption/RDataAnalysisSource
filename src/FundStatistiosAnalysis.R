mystats <- function(x, na.omit=FALSE){
  if(na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return (c(n=n,mean=m,stdev=s,skew=skew,kurtosi=kurt))
}

vars <- c("mpg","hp","wt")
#summary(mtcars[vars])
#head(mtcars[vars])
sapply(mtcars[vars],mystats)

# 核密度图
par(mtrow <- c(2,1))

d <- density(mtcars$mpg)
plot(d)
plot(d,main = "Kernel Density of Miles Per Gallon")
#设置图形填充
polygon(d,col = "green",border="red")
# ruq(mtcars$mpg,col="black")

# 可比较的核密度图
par(lwd=3) # 双倍线条宽度
library(sm)

# 绘制可以比较的密度图
sm.density.compare(mtcars$mpg,mtcars$cyl,xlab="Mills Par Gallon")
# 添加图形标题
title("MPG Distribution by Car Cylinder")
cyl.f <- factor(mtcars$cyl,levels = c(4,6,8),labels = c("4 cylinder","6 cylinder","8 cylinder"))
colfill <- c(2:(1+length(levels(cyl.f))))
#向图鼠标添加一个图例
legend(locator(1),levels(cyl.f),fill = colfill)

# 线箱图
boxplot.stats(mtcars$mpg)
boxplot(mpg ~ cyl,
        data = mtcars,
        main="Car Mileage Data",
        xlab="Number of cylinders",
        ylab="Miles Per Gallon")
# notch为true,可以得到凹槽
boxplot(mpg ~ cyl,
        data = mtcars,
        notch=TRUE,
        col="blue",
        varwitch=TRUE,
        main="Car Mileage Data",
        xlab="Number of cylinders",
        ylab="Miles Per Gallon")
# 两个交叉因子的线箱图
mtcars$cyl.f <- factor(mtcars$cyl,
                       levels = c(4,6,8),
                       labels = c("4_","6_","8_"))
mtcars$am.f <- factor(mtcars$am,
                      levels = c(0,1),
                      labels = c("auto","standand"))
boxplot(mpg ~ am.f * cyl.f,
        data = mtcars,
        varwidth=TRUE,
        col = c("gold","green"),
        main = "MPG Distribution by Auto Type",
        xlab = "Auto Type")
  
#小提琴图
library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl == 4]
x2 <- mtcars$mpg[mtcars$cyl == 6]
x3 <- mtcars$mpg[mtcars$cyl == 8]
vioplot(x1,x2,x3,
        names = c("4 cyl","6 cyl","8 cyl"),
        col = "green")
title("Violin Plots of Miles Per Gallon")
#点图
dotchart(mtcars$mpg,labels = row.names(mtcars),cex = .7,
         main = "Gas Mileage for Car models",
         xlab = "Miles per Gallon")

#描述型统计分析
vars <- c("mpg","hp","wt")
head(mtcars[vars])
#summary()统计函数
summary(mtcars[vars])
#通过Hmisc中的describe()函数计算描述性统计量
library(Hmisc)
describe(mtcars[vars])
#pastecs包中的stat.desc()描述计算性统计量
library(pastecs)
stat.desc(mtcars[vars])

#频数表和列联表
library(vcd)
head(Arthritis)
# 一维列联表:频数统计表
mytable <- with(Arthritis,table(Improved))
# 用prop.table()将频数转为比例值
density <- prop.table(mytable)*100


library(gmodels)
CrossTable(Arthritis$Treatment,Arthritis$Improved)
# 三维列联表
mytable <- xtabs(~ Treatment + Sex + Improved,data = Arthritis)

#
ftable(addmargins(prop.table(mytable,c(2,3)),3))

library(vcd)
mytable <- xtabs(~ Treatment + Improved, data = Arthritis)
# 卡方独立性检验
chisq.test(mytable)
# Fisher 精确检验
fisher.test(mytable)
assocstats(mytable)


#函数table2float,表格式扁平化
table2float <- function(mytable){
  df <- data.frame(mytable)
  rows <- dim(df)[1]
  cols <- dim(df)[2]
  x <- NULL
  for (i in 1:rows){
    for (j in 1:df$Freq[i]){
      row <- df[i, c(1:(cols - 1))]
      x <- rbind(x, row)
    }
  }
  row.names(x) <- c(1:dim(x)[1])
  return (x)
}





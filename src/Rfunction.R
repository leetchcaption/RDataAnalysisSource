options(digits = 2)
Student <- c("John Davis","Angela Williams","Bullwinkle Moose",
             "David Jones","Janice Markhammer","Cheryl Cushing",
             "Reuven Ytzrhak","Greg Knox","Joel England","Marry Robin")
Math <- c(502,600,412,358,495,512,410,625,573,522)
Science <- c(95,99,80,82,75,85,80,95,89,86)
English <- c(25,22,18,15,20,28,15,30,27,18)
# 建立成绩表数据框
roster <- data.frame(Student,Math,Science,English,stringsAsFactors = FALSE)
# 对成绩进行标准化，赋值给z
z <- scale(roster[,2:4])
# 计算z的每行平均数为学生的综合得分
score <- apply(z,1,mean)
# 在数据库roster后追加score
roster <- cbind(roster,score)
y <- quantile(score,c(.8,.6,.4,.2))
# 定义分数对应的等级
roster$grade[score >= y[1]] <- 'A'
roster$grade[score < y[1] & score >= y[2]] <- 'B'
roster$grade[score < y[2] & score >= y[3]] <- 'C'
roster$grade[score < y[3] & score >= y[4]] <- 'D'
roster$grade[score < y[4]] <- 'F'
#把姓名从空格处拆开
name <- strsplit((roster$Student),' ')
# 拆分lastname和fristname
lastname <- sapply(name,"[",2)
fristname <- sapply(name,'[',1)
roster <- cbind(fristname,lastname,roster[,-1])
# 先按照姓在按照名排序
roster <- roster[order(lastname,fristname),]
# 按照成绩排名
roster <- roster[order(roster$grade),]



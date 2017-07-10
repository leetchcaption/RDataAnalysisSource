library("igraph")
# 读取数据集
g <- read.table("D:\\workspace\\FileForder\\app_sub.txt",header=FALSE,sep=',',colClasses=c("character","character"))
# 去除NA值
g1 <- na.omit(g)
#简单的社交网络
library("igraph") #加载igraph包
x <- par(bg="black") #设置背景色为黑色
g2 = graph.data.frame(d = g1,directed = F) #数据格式转换
V(g2) #查看定点
E(g2) #查看边
plot(g2,layout=layout.fruchterman.reingold,vertex.label=NA) #显示网络图

#对定点和边的格式做调整
#设置vertex.size来调整顶点大小， 设置vertex.color来改变显示颜色。
plot(g2,layout=layout.fruchterman.reingold,vertex.size=20, vertex.color="green",edge.arrow.size=0.5,vertex.label=NA) 
#设置vertex大小和颜色后显示网络图
#plot(g2,layout=layout.fruchterman.reingold,vertex.size=2,vertex.color="red",edge.arrow.size=0.05,vertex.label=N)

#利用walktrap.community进行社区划分，对不同的社区赋值不同的颜色

com = walktrap.community(g2, steps = 100)
V(g2)$sg=com$membership
V(g2)$color = rainbow(max(V(g2)$sg),alpha=0.8)[V(g2)$sg]
plot(g2,layout=layout.fruchterman.reingold, vertex.size=20,vertex.color=V(g2)$color, edge.width=0.4,edge.arrow.size=0.08,edge.color = rgb(1,1,1,0.4),vertex.frame.color=NA,margin= rep(0, 4),vertex.label=NA)

#美化边
E(g2)$color=V(g2)[name=ends(g2,E(g2))[,2]]$color #为edge的颜色赋值
V(g2)[grep("1", V(g2)$name)]$color=rgb(1,1,1,0.8) #为vertex的颜色赋值
plot(g2,layout=layout.fruchterman.reingold, vertex.size=V(g2)$size, vertex.color= V(g2)$color, edge.width=3,edge.color = E(g2)$color,vertex.frame.color=NA,margin= rep(0, 4),vertex.label=NA)




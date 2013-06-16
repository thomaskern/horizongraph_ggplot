library(testthat)
library(grid)
library(ggplot2)
library(plyr)

steps = function(y,intervals) max(abs(y))/intervals

f = function(df){
  r = seq(0,max(abs(df$y)),steps(df$y,2))
  df$g = cut(abs(df$y),r,include.lowest=TRUE)

  df2 = df
  df2$splitter = 2
  df[df$y > 0,"y"] = 0
  df2[df2$y < 0,"y"] = 0
  rbind(df,df2)
}

fac = function(x,pos,col) ifelse(pos > 0,as.character(x[pos,col]),as.character(x[pos+1,col]))

g = function(x,xs){
  pos = x[xs,]$x

  tmp = function(mod) list(x[xs,"group"],get(mod)(pos,ifelse(mod=="-",0.489,0.5)),0,x[xs,"splitter"],fac(x,xs-1,"g"))
  insert.zero = function(mod){
    n = nrow(x) + 1
    if(x[get(mod)(xs,1),"y"] != 0 && 
       get(mod)(xs,1) < nrow(x) && 
       get(mod)(xs,1) > 0)
      x[n,] = tmp(mod)
    x
  }
  if(xs != 1+(nrow(x)/2)){
    x = insert.zero("-") #before current position
    x = insert.zero("+") #after current position
  }
  x
}


colors = c("#B21212","#FF0000","#0971B2","#1485CC")
num.steps = 2

#colors = c("#4C1512","#CC3730","#CC716D","#1B204C","#4855CC","#868CCC")
#num.steps = 3

plot.bands = function(df.all,num.steps){
  #df = df[df$group == "A",]
  step = steps(df.all$y,num.steps)
  p = ggplot() + theme(panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),
                       panel.margin=unit(0,"cm"))

  add.area = function(p,df,c1,c2){
    #p = p + geom_area(aes(x,abs(y),fill=g),df[df$splitter==1,],fill=c1) + geom_area(aes(x,y),df[df$splitter==2,],fill=c2)
    p = p + geom_ribbon(aes(x,ymin=ymin,ymax=ymax),df[df$splitter==1,],fill=c1) + geom_ribbon(aes(x,ymin=ymin,ymax=ymax),df[df$splitter==2,],fill=c2)
    p
  }

  #print(range(df.all$y))
  #print(range(df$y))
  #print(step)

  grounds = c()
  counter = 0
  for(level in levels(df.all$group)){
    output = ifelse(level=="A",T,F)
    df = df.all[df.all$group==level,]
    ground = counter * 1.05 * step
    grounds = c(grounds,ground)

    for(i in (num.steps-1):1){
      df5 = df
      df5$y = abs(df5$y)
      current =  step * i
      df5$y = df5$y - current

      df5[df5$y > step | df5$y < 0,"y"] = 0
      df5[df5$y > 0,"y"] = step
      df5 = Reduce(g,which(df5$y == 0),df5)
      df5$ymin = ground
      df5$ymax = abs(df5$y) + ground
      if(output) print(df5)
      p = add.area(p,df5,colors[length(colors)/2+1-i],colors[length(colors)+1-i])
    }

    for(i in 0:(num.steps-1)){
      df5 = df
      df5$y = abs(df5$y)
      current =  step * i
      df5$y = df5$y - current
      df5[df5$y > step,"y"] = 0
      df5[df5$y < 0,"y"] = 0
      df5 = Reduce(g,which(df5$y == 0),df5)
      df5$ymin = ground
      df5$ymax = abs(df5$y) + ground
      if(output) print(df5)
      p = add.area(p,df5,colors[length(colors)/2-i],colors[length(colors)-i])
    }
    counter = counter + 1
    p = p + geom_hline(yintercept=ground)
  }

  p = p + 
      scale_y_continuous(breaks=grounds+step*1.05/2,labels=levels(df$group)) +
      scale_x_continuous(breaks=seq(df$x)-1)

  print(p)
  p
}

smoothme = function(df,span=0.2,interval=1){
  l = loess("y ~ x",data.frame(x=1:nrow(df),y=df$value),span=0.1)
  tmp = data.frame(x=seq(1,nrow(df),interval),y=predict(l,newdata=data.frame(x=seq(1,nrow(df),interval))), splitter=1)
  tmp
}

eu = function(){
  df = melt(EuStockMarkets)
  df = df[df$Var2=="DAX" & df$Var1 < 200,]
  df
}

calc.diff = function(tmp){
  df = data.frame(x=1:nrow(tmp), y=c(diff(tmp$y),0))
  df$diff_perc = df$y/tmp$y*100
  df
}

df = calc.diff(smoothme(eu()))
df2 = df[abs(df$diff_perc) < 1.3,]
tmp2 = data.frame(x=1:nrow(df2),y=df2$diff_perc,splitter=1)

df_eu = f(tmp2)
#plot(df_eu$x,df_eu$y,type="l")
#plot(tmp2$x,tmp2$y,type="l")
#plot.bands(df3,num.steps)
#b = plot.bands(df_eu,num.steps)
#b = plot.bands(df3,num.steps)

df = data.frame(group=rep(c("A","B","C"),each=10), x=0:9,y=c(1.2,0.8627684,0.02,-1,-0.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858,0.2,0.8627684,0.92,-1,-0.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858,0.3,0.1627684,0.3072174,-0.3,-1.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858),splitter=1) 
df5 = ddply(df,.(group),f)
#print(df5)
plot.bands(df5,num.steps)

#df3 = f(data.frame(group="A",x=0:9,y=c(0.2,0.8627684,0.92,-1,-0.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858),splitter=1) )
#df3 = f(data.frame(group=rep(c("A","B"),each=10), x=0:9,y=c(0.5,0.1627684,0.3072174,-1,-0.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858,0.3,0.1627684,0.3072174,-0.3,-1.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858),splitter=1))
#print(df3)
#plot.bands(df3,num.steps)

#print(p + geom_line(data=df3,aes(x,abs(y),group=splitter)))
#print(p)
#print(df4)

#test_that("asdfasdf",{
#expect_equal(nrow(df4),20+3+3)
#expect_equal(0.0,df4[df4$splitter == 1 & df4$x==3.5,"y"])
#})

library(testthat)
library(grid)
library(ggplot2)
library(plyr)

steps = function(y,i){
  max(y)/i
}


cut.into.parts = function(vals,i){
  a = abs(vals)
  r = seq(0,max(a),steps(vals,i))
  cut(a,r,include.lowest=TRUE)
}

f = function(df,num.steps){
  df$g = cut.into.parts(df$y,num.steps)

  df2 = df
  df2$splitter = 2
  df[df$y > 0,"y"] = 0
  df2[df2$y < 0,"y"] = 0
  rbind(df,df2)
}

fac = function(x,pos,col) ifelse(pos > 0,as.character(x[pos,col]),as.character(x[pos+1,col]))

padding = function(x,xs,nrows){
  pos = x[xs,]$x

  interval.x.axis = function(x,xs,mod){
    ret = abs(x[get(mod)(xs,1),"x"]-x[xs,"x"])/2
    ret
  }

  create.entry = function(mod) list(x[xs,"group"],
                           get(mod)(pos,ifelse(mod=="-",interval.x.axis(x,xs,mod)*0.98,interval.x.axis(x,xs,mod))),
                           0,
                           x[xs,"splitter"],
                           fac(x,xs-1,"g"))

  insert.zero = function(mod){
    if(x[get(mod)(xs,1),"y"] != 0 && 
       get(mod)(xs,1) < nrows && 
       get(mod)(xs,1) > 0 &&
       x[get(mod)(xs,1),"splitter"] == x[xs,"splitter"]){
      x[nrow(x) + 1,] = create.entry(mod)
    }
    x
  }

  x = insert.zero("-") #before current position
  insert.zero("+") #after current position
}

colors = c("#B21212","#FF0000","#0971B2","#1485CC")
num.steps = 2

#colors = c("#590000","#CC3730","#CC716D","#003BF7","#B8BCFB","#CCCEE3")
#num.steps = 3

#colors = c("#590000","#003BF7")
#num.steps = 1

colors2 =LETTERS[1:length(colors)]

plot.bands = function(df.all,num.steps){
  step = steps(df.all$y,num.steps)*1.00000000001
  p = ggplot() + theme(panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),
                       axis.ticks.y=element_blank(),
                       panel.margin=unit(0,"cm"),panel.background  = element_rect(fill = "white", colour = NA))

  add.area = function(p,df,c1,c2){
    df$fill = 0
    df[df$splitter==1,"fill"] = c1
    df[df$splitter==2,"fill"] = c2
    p = p + geom_ribbon(aes(x,ymin=ymin,ymax=ymax,fill=fill),df[df$splitter==1,]) + geom_ribbon(aes(x,ymin=ymin,ymax=ymax,fill=fill),df[df$splitter==2,])
    p
  }

  grounds = c()
  counter = 0
  for(level in levels(df.all$group)){
    output = ifelse(level=="A",T,F)
    df = df.all[df.all$group==level,]
    ground = counter * step
    grounds = c(grounds,ground)

    for(i in 1:num.steps){
      df5 = df
      df5$y = abs(df5$y)
      current_bottom_bottom =  step * (i-1)
      current_bottom_top =  step * (i)

      current_top_bottom = step * (i)
      current_top_top = step * (i+1)

      df5[df5$y < current_bottom_bottom,"y"] = 0
      df5[df5$y >= current_bottom_bottom & df5$y < current_bottom_top ,"y"] = df5[df5$y >= current_bottom_bottom & df5$y < current_bottom_top ,"y"] - current_bottom_bottom
      df5[df5$y > current_top_bottom,"y"] = step
      row.names(df5) = NULL
      df5 = Reduce(function(x,xs){padding(x,xs,nrow(df5))},which(df5$y == 0),df5)

      df5$ymin = ground
      df5$ymax = abs(df5$y) + ground
      p = add.area(p,df5,colors2[length(colors2)/2+1-i],colors2[length(colors2)+1-i])
    }
    counter = counter + 1
    p = p + geom_hline(yintercept=ground)
    #break
  }

  labels = function(vals,side){
    if(num.steps == 1){
      ifelse(side == 1, paste0("(0,",max(abs(vals)),"]"),paste0("[-",max(abs(vals)),",0]"))
    }else
      levels(cut(side*abs(df.all$y),num.steps))
  }

  p = p + scale_fill_manual(values=colors,breaks=colors2,
                            labels=c(labels(df.all$y,-1),labels(df.all$y,1)))
  p = p + 
  scale_y_continuous(breaks=grounds+step*1.05/2,labels=levels(df$group)) +
  scale_x_continuous(breaks=seq(df.all[df.all$group==levels(df.all$group)[1] & df.all$splitter == 1,"x"])-1)
  p = p + theme(legend.position="None")
  p + geom_hline(yintercept=grounds[length(grounds)]+step)
}

create.df = function(newy,newx){
  data.frame(x=newx,y=newy, splitter=1)
}

smoothme = function(df,span=0.4,interval=1,...){
  l = loess("y ~ x",data.frame(x=1:nrow(df),y=df$y),span=span)
  newx = seq(1,nrow(df),interval)
  create.df(predict(l,newdata=data.frame(x=newx)),newx)
}

spline.smoother = function(df,n=3*nrow(df),...){
  l = spline(df$x,df$y,n=n)
  create.df(l$y,l$x)
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

#df = calc.diff(smoothme(eu()))
#df2 = df[abs(df$diff_perc) < 1.3,]
#tmp2 = data.frame(x=1:nrow(df2),y=df2$diff_perc,splitter=1)
#df_eu = f(tmp2)
#plot(df_eu$x,df_eu$y,type="l")
#plot(tmp2$x,tmp2$y,type="l")
#plot.bands(df3,num.steps)
#b = plot.bands(df_eu,num.steps)
#b = plot.bands(df3,num.steps)

df = data.frame(group=rep(c("A","B","C"),each=10), x=0:9,y=c(1.2,1.8627684,0.1,-1,-0.8324571,-0.0061331,-0.8056517,0.1085939,0.6393061,-0.9098858,0.2,0.8627684,0.92,-1,-0.8324571,-1.0061331,-0.5056517,0.1085939,-0.6393061,-0.9098858,0.3,0.1627684,0.3072174,-0.3,-1.5324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858),splitter=1) 
#df = ddply(df,.(group),spline.smoother,span=.3,interval=.01,n=5)
df5 = ddply(df,.(group),f,num.steps=num.steps)
#print(df5)
p = plot.bands(df5,num.steps)
print(p)

df3 = f(data.frame(group="A",x=0:9,y=c(0.2,0.8627684,0.92,-1,-0.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858),splitter=1),num.steps=num.steps)
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

library(testthat)
library(grid)
library(ggplot2)
library(plyr)
library(reshape2)

steps = function(y,i){
  max(y)/i
}


cut.into.parts = function(vals,i){
  a = abs(vals)
  r = seq(0,max(a),steps(vals,i))
  cut(a,r,include.lowest=TRUE)
}

f = function(df,num.steps){
  df$splitter = 1
  df2 = df
  df2$splitter = 2
  df[df$y > 0,"y"] = 0
  df2[df2$y < 0,"y"] = 0
  #print(head(df2))
  print(names(df))
  print(names(df2))
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
                           x[xs,"splitter"]                           )

  insert.zero = function(mod){
    if(x[get(mod)(xs,1),"y"] != 0 && 
       get(mod)(xs,1) < nrows && 
       get(mod)(xs,1) > 0 &&
       x[get(mod)(xs,1),"splitter"] == x[xs,"splitter"]){
      x[nrow(x) + 1,c("group","x","y","splitter")] = create.entry(mod)
    }
    x
  }

  x = insert.zero("-") #before current position
  insert.zero("+") #after current position
}

band.colors = list(c("#590000","#003BF7"),c("#B21212","#FF0000","#0971B2","#1485CC"),c("#590000","#CC3730","#CC716D","#003BF7","#B8BCFB","#CCCEE3"))
num.steps = 2
#num.steps = 3
#num.steps = 1

colors = band.colors[[num.steps]]
colors2 =LETTERS[1:(num.steps*2)]

plot.bands = function(df.all,num.steps){
  step = steps(df.all$y,num.steps)*1.00000000001
  p = ggplot() + theme(panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),
                       axis.ticks.y=element_blank(),
                       panel.background  = element_rect(fill = "white", colour = NA))

  add.area = function(p,df,c1,c2){
    df$fill = ifelse(df$splitter==1,c1,c2)
    p + geom_ribbon(aes(x,ymin=ymin,ymax=ymax,fill=fill),df)
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

      #print(step)
      #if(length(df5[df5$y < current_bottom_bottom,"y"]) == 0)
        #print(df5)

      df5[df5$y < current_bottom_bottom,"y"] = 0
      df5[df5$y >= current_bottom_bottom & df5$y < current_bottom_top ,"y"] = df5[df5$y >= current_bottom_bottom & df5$y < current_bottom_top ,"y"] - current_bottom_bottom
      df5[df5$y > current_top_bottom,"y"] = step
      row.names(df5) = 1:nrow(df5)
      df5 = Reduce(function(x,xs){padding(x,xs,nrow(df5))},which(df5$y == 0),df5)

      df5$ymin = ground
      df5$ymax = abs(df5$y) + ground
      #print(df5)
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

  #p = p + 
    #scale_y_continuous(breaks=grounds+step*1.05/2,labels=levels(df$group)) 
    #scale_x_continuous(breaks=seq(df.all[df.all$group==levels(df.all$group)[1] & df.all$splitter == 1,"x"])-1)

  p = p + theme(legend.position="None")
  p + geom_hline(yintercept=grounds[length(grounds)]+step)
}

create.df = function(df,newy,newx){
   data.frame(x=newx,y=newy,group=df$group[1],splitter=df$splitter[1])
}

smoothme = function(df,span=0.4,interval=1,...){
  l = loess("y ~ x",data.frame(x=df$x,y=df$y),span=span)
  newx = seq(range(df$x)[1],range(df$x)[2],interval)
  create.df(df,predict(l,newdata=data.frame(x=newx)),newx)
}

spline.smoother = function(df,n=3*nrow(df),...){
  l = spline(df$x,df$y,n=n)
  create.df(df,l$y,l$x)
}

eu = function(){
  df = melt(EuStockMarkets)
  #df = df[(df$Var2=="DAX" | df$Var2 == "SMI") & df$Var1 < 200,]
  df = df[df$Var1 < 200,]
  row.names(df) = 1:nrow(df)
  names(df) = c("x","group","y")
  df$x = as.numeric(df$x)
  df$y = as.numeric(df$y)
  df$group = factor(df$group)
  df$splitter = 1
  df = ddply(df,.(group),calc.diff)
  df
}

calc.diff = function(tmp){
  tmp = transform(tmp,diff=c(diff(y),0))
  transform(tmp,diff_perc=diff/y*100)
}

e = eu()
#e = ddply(e,.(group),smoothme,span=0.15,interval=.1,n=20)
df2 = e[abs(e$diff_perc) < 1.0,]
tmp2 = df2
tmp2$y = tmp2$diff_perc

df_eu = ddply(tmp2, .(group),f,num.steps=num.steps)
#b = plot.bands(df_eu,num.steps)
#print(b)

#b = plot.bands(df3,num.steps)

#df = data.frame(group=rep(c("A","B","C"),each=10), x=0:9,y=c(1.2,1.8627684,0.1,-1,-0.8324571,-0.0061331,-0.8056517,0.1085939,0.6393061,-0.9098858,0.2,0.8627684,0.92,-1,-0.8324571,-1.0061331,-0.5056517,0.1085939,-0.6393061,-0.9098858,0.3,0.1627684,0.3072174,-0.3,-1.5324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858),splitter=1) 
##df = ddply(df,.(group),smoothme,span=.2,interval=.1,n=8)
#df5 = ddply(df,.(group),f,num.steps=num.steps)
#p = plot.bands(df5,num.steps)
#print(p)
#return

df3 = data.frame(group="A",x=0:9,y=c(0.2,0.8627684,0.92,-1,-0.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858),splitter=1)
df3 = ddply(df3,.(group),f,num.steps=num.steps)
#print(df3)
p = plot.bands(df3,num.steps)
print(p)

df4 = data.frame(group=rep(c("A","B"),each=10), x=0:9,y=c(0.5,0.4627684,0.2072174,-1,-0.8324571,-1.0061331,-0.5056517,0.3085939,0.4383061,-0.9098858,0.3,0.1627684,0.3072174,-0.3,-1.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858),splitter=1)
#print(df3)
#plot.bands(df3,num.steps)

#print(p + geom_line(data=df3,aes(x,abs(y),group=splitter)))
#print(p)
#print(df4)

#test_that("asdfasdf",{
#expect_equal(nrow(df4),20+3+3)
#expect_equal(0.0,df4[df4$splitter == 1 & df4$x==3.5,"y"])
#})

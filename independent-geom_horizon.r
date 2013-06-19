library(testthat)
library(grid)
library(ggplot2)
library(plyr)
library(reshape2)
library(RColorBrewer)

steps = function(y,i){
  max(y)/i
}

cut.into.parts = function(vals,i){
  a = abs(vals)
  r = seq(0,max(a),steps(vals,i))
  cut(a,r,include.lowest=TRUE)
}

prep = function(df,num.bands){
  if(!is.factor(df$group))
    df$group = factor(df$group)
  df$splitter = 1
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
                                    x[xs,"splitter"])

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

set.color <- function(num.bands, user.colors, band.colors){
  if(!is.null(user.colors)){
    if(length(user.colors) == 2*num.bands)
      user.colors
    else
      stop(paste("You have provided the wrong number of colors. You need to provide ",2*num.bands,"colors, not ",length(colors)))
  }else{
    if(length(band.colors) < num.bands){
      c(rev(brewer.pal(num.bands,"Reds")),rev(brewer.pal(num.bands,"Blues")))
    }else
      band.colors[[num.bands]]
  }
}

plot.bands = function(df.all,num.bands,colors){
  band.colors = list(c("#590000","#003BF7"),
                     #c("#B21212","#FF0000","#0971B2","#1485CC"),
                     c("#B11019","#F5AA9C","#1F61B2","#A2C8DB"),
                     c("#B11019","#DE2F35","#F5AA9C","#1F61B2","#4E8AC6","#A2C8DB"))
  colors2 =LETTERS[1:(num.bands*2)]
  colors = set.color(num.bands,colors,band.colors)
  step = steps(df.all$y,num.bands)*1.00000000001

  p = ggplot() + theme(panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),
                       axis.ticks.y=element_blank(),
                       panel.background  = element_rect(fill = "white", colour = NA),
                       legend.position="None")

  add.area = function(p,df,c1,c2){
    df$fill = ifelse(df$splitter==1,c1,c2)
    p + geom_ribbon(aes(x,ymin=ymin,ymax=ymax,fill=fill),df)
  }

  labels = function(vals,side){
    if(num.bands == 1){
      ifelse(side == 1, paste0("(0,",max(abs(vals)),"]"),paste0("[-",max(abs(vals)),",0]"))
    }else
      levels(cut(side*abs(df.all$y),num.bands))
  }

  grounds = c()
  counter = 0
  y_labels = rev(levels(df.all$group))
  for(level in y_labels){
    output = ifelse(level=="A",T,F)
    df = df.all[df.all$group==level,]
    ground = counter * step
    grounds = c(grounds,ground)

    for(i in 1:num.bands){
      df2 = df
      df2$y = abs(df2$y)

      current_bottom =  step * (i-1)
      current_middle =  step * (i)
      current_top = step * (i+1)

      df2[df2$y < current_bottom,"y"] = 0
      df2[df2$y >= current_bottom & df2$y < current_middle,"y"] = df2[df2$y >= current_bottom & df2$y < current_middle,"y"] - current_bottom
      df2[df2$y > current_middle,"y"] = step
      row.names(df2) = 1:nrow(df2)
      df2 = Reduce(function(x,xs){padding(x,xs,nrow(df2))},which(df2$y == 0),df2)

      df2$ymin = ground
      df2$ymax = abs(df2$y) + ground
      p = add.area(p,df2,colors2[length(colors2)/2+1-i],colors2[length(colors2)+1-i])
    }
    counter = counter + 1
  }

  p = p + scale_fill_manual("Ranges",values=colors,breaks=colors2,
                            labels=c(labels(df.all$y,-1),labels(df.all$y,1)))
  p = p + scale_y_continuous(breaks=grounds+step*1.05/2,labels=y_labels) + scale_x_continuous(breaks=NULL)
  p = p + geom_hline(yintercept=c(grounds,grounds[length(grounds)]+step))
  p
}

create.df = function(df,newy,newx){
  data.frame(x=newx,y=newy,group=df$group[1])
}

smooth.loess = function(df,span=0.4,interval=1,...){
  l = loess("y ~ x",data.frame(x=df$x,y=df$y),span=span)
  newx = seq(range(df$x)[1],range(df$x)[2],interval)
  create.df(df,predict(l,newdata=data.frame(x=newx)),newx)
}

smooth.spline = function(df,n,...){
  l = spline(df$x,df$y,n=n)
  create.df(df,l$y,l$x)
}

eu = function(){
  df = melt(EuStockMarkets)
  row.names(df) = 1:nrow(df)
  names(df) = c("x","group","y")
  df$x = as.numeric(df$x)
  df$y = as.numeric(df$y)
  df$group = factor(df$group)
  df
}

calc.diff = function(tmp){
  tmp = transform(tmp,diff=c(diff(y),0))
  transform(tmp,diff_perc=diff/y*100)
}

smooth.data <- function(df,smoothing,loess.span,loess.interval,spline.n){
  if(!is.null(smoothing) && exists(smoothing)){
    df = ddply(df,.(group),get(paste0("smooth.",smoothing)),span=loess.span,interval=loess.interval,n=spline.n)
  }
  df
}

check_mapping = function(mapping){
  missing_aes <- setdiff(c("x","y","group"), new.names.mapping(mapping))
  if (length(missing_aes) == 0) return()

  stop("horizon requires the following missing aesthetics: ", paste(missing_aes, collapse=", "), call. = FALSE)
}

calculate.diff = function(df,to.calculate){
  if(to.calculate){
    df = ddply(df,.(group),calc.diff)
    df$y = df$diff_perc
  }
  df
}

plot_horizon = function(data,mapping=aes(x=x,y=y),num.bands=2,smoothing=NULL,band.colors=NULL,
                        calculate.diff=FALSE,
                        loess.span=0.5,loess.interval=1,spline.n=3*nrow(data)){
  check_mapping(mapping)
  plot.bands(ddply(smooth.data(calculate.diff(rename(data,mapping),
                                              calculate.diff),
                               smoothing,
                               loess.span,
                               loess.interval,
                               spline.n),
                   .(group),
                   prep,
                   num.bands=num.bands),
             num.bands,
             band.colors)
}

#p = plot_horizon(with(eu(),eu()[x <= 200,]),aes(x,y,group=group),num.bands=2,smoothing="loess",loess.span=0.2,loess.interval=0.1,calculate.diff=TRUE)
#print(p)

df = data.frame(group="A",x=0:9,y=c(0.2,0.8627684,0.92,-1,-0.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858))
p = plot_horizon(df,aes(x,y,group=group),2,smoothing="spline", spline.n=40) 
print(p)

#df = data.frame(group=factor(rep(c("A","B"),each=10),levels=c("B","A")), x=0:9,y=c(0.8,0.4627684,0.2072174,-1,-0.8324571,-1.0061331,-0.5056517,0.3085939,0.4383061,-0.9098858,0.3,0.1627684,0.3072174,-0.3,-1.8324571,-1.0061331,-0.5056517,0.1085939,0.6393061,-0.9098858))
#p = plot_horizon(df,aes(x,y,group=group),2,smoothing="spline", spline.n=40)
#print(p)

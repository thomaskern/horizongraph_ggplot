geom_horizon <- function(mapping = NULL, data = NULL, num.steps=2, smoothing=NULL,
                         smoothme.span=0.4,smoothme.interval=0.5,spline.n=10,band.colors=NULL,...){
  GeomHorizon$new(mapping = NULL, data = NULL, stat = "identity", position = "dodge",
                  num.steps=num.steps,smoothing=smoothing,
                  smoothme.span=smoothme.span,smoothme.interval=smoothme.interval,spline.n=spline.n,
                  band.colors=band.colors,...)
}

GeomHorizon <- proto(Geom,{
                     band.colors <- list(c("#590000","#003BF7"),
                                         c("#B21212","#FF0000","#0971B2","#1485CC"),
                                         c("#590000","#CC3730","#CC716D","#003BF7","#B8BCFB","#CCCEE3"))

                     objname <- "horizon"

                     default_stat <- function(.) StatIdentity
                     default_aes <- function(.) aes(colour=NA, fill="#000000", size=0.5, linetype=1, alpha = NA)
                     required_aes <- c("group","x", "ymin","ymax")
                     default_pos <- function(.) PositionDodge

                     set.color <- function(., num.steps, band.colors){
                       if(!is.null(band.colors)){
                         if(length(band.colors) == 2*num.steps)
                           .$band.color = band.colors
                         else
                           stop(paste("You have provided the wrong number of colors. You need to provide ",2*num.steps,"colors, not ",length(colors)))
                       }else{
                         if(length(.$band.colors)-1 < num.steps){
                           .$band.color = c(rev(brewer.pal(num.steps,"Reds")),rev(brewer.pal(num.steps,"Blues")))
                         }else
                           .$band.color = .$band.colors[[num.steps]]
                       }
                     }

                     new <- function(., mapping=NULL, data=NULL, stat=NULL, position=NULL, num.steps, smoothing,
                                     smoothme.span=NULL, smoothme.interval=NULL, spline.n=NULL,band.colors=NULL,...){
                       .$num.steps=num.steps
                       .$smoothing = smoothing
                       .$spline.n = spline.n
                       .$smoothme.span = smoothme.span
                       .$smoothme.interval = smoothme.interval
                       .$set.color(num.steps,band.colors)

                       .super$new(.,mapping=mapping, data=data, stat=stat, position=position, ...)
                     }

                     reparameterise <- function(., df, params) {
                       df$splitter = 1
                       if(!is.null(.$smoothing)){
                         if(.$smoothing == "spline"){
                           df = ddply(df,.(group),smooth.spline,n=.$spline.n)
                         }else if(.$smoothing == "smoothme")
                           df = ddply(df,.(group),smoothme,span=.$smoothme.span,interval=.$smoothme.interval)
                       }

                       df = ddply(df,.(group),f,num.steps=.$num.steps)

                       df.all = df
                       df.all$group = factor(df.all$group)

                       step = steps(df.all$y,.$num.steps)*1.00000000001

                       ret = data.frame()
                       .$grounds = c()
                       counter = 0
                       total = 1
                       for(level in levels(df.all$group)){
                         df = df.all[df.all$group==level,]
                         ground = counter * step
                         .$grounds = c(grounds,ground)

                         for(i in 1:.$num.steps){
                           df2 = df
                           df2$y = abs(df2$y)

                           current_bottom_bottom =  step * (i-1)
                           current_bottom_top =  step * (i)
                           current_top_bottom = step * (i)
                           current_top_top = step * (i+1)

                           df2[df2$y < current_bottom_bottom,"y"] = 0
                           df2[df2$y >= current_bottom_bottom & df2$y < current_bottom_top ,"y"] = df2[df2$y >= current_bottom_bottom & df2$y < current_bottom_top ,"y"] - current_bottom_bottom
                           df2[df2$y > current_top_bottom,"y"] = step
                           row.names(df2) = 1:nrow(df2)
                           df2 = Reduce(function(x,xs){padding(x,xs,nrow(df2))},which(df2$y == 0),df2)

                           df2$ymin = ground
                           df2$ymax = abs(df2$y) + ground
                           c1 = .$band.color[length(.$band.color)/2+1-i]
                           c2 = .$band.color[length(.$band.color)+1-i]
                           df2$fill = ifelse(df2$splitter==1,c1,c2)
                           df2$counter = ifelse(df2$splitter==1,total,total+1)
                           ret = rbind(ret,df2)
                           total = total + 2
                         }
                         counter = counter + 1
                       }

                       .$grounds = c(.$grounds, .$grounds[length(grounds)]+step)
                       ret$PANEL = 1
                       ret
                     }

                     draw <- function(.,data,scales, coordinates, na.rm=FALSE,...){
                       add.line = function(y)
                         GeomHline$draw(data.frame(yend=y,y=y,size=0.5,linetype=1,colour="#cccccc",alpha=1),scales,coordinates)
                       add.ribbon = function(g)
                         GeomRibbon$draw(data[data$counter == g,],scales,coordinates,...)

                       p = Reduce(function(x,xs){c(x,list(add.ribbon(xs)))},unique(data$counter),list(add.line(.$grounds)))

                       ggname(.$my_name(), do.call(grobTree,p))
                     }
                  })


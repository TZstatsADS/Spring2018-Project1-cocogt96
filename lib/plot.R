#multiplot use to draw multiple plot in a page
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#DrawTrend draw trend of precentage of words thourgh out year
DrawTrend<-function(ff.final2,party){
  mysum<-function(df){
    return(sum(df$count))
  }
  ff.final2=ff.final2[ff.final2$ff.p==party,]
  ff.year.sum<-ddply(ff.final2,.(ff.year),mysum)
  ff.year.term.sum<-ddply(ff.final2,.(term,ff.year),mysum)
  ff.sum.vec<-c()
  for(i in ff.year.term.sum$ff.year){
    s<-ff.year.sum$V1[ff.year.sum$ff.year==i]
    ff.sum.vec<-c(ff.sum.vec,s)
  }
  ff.f<-cbind(ff.year.term.sum,ff.sum.vec)
  ff.pre<-ff.f$V1/ff.f$ff.sum.vec
  ff.ff<-cbind(ff.f,ff.pre)
  ff.ff[order(ff.ff$ff.year),]
  ff.ff$year=unfactor(ff.ff$ff.year)
  if(party=="D")
    Name=Dname
  else if(party=="DPR")
    Name=DPRname
  else if(party=="F")
    Name=Fname
  else if(party=="R")
    Name=Rname
  else
    Name=Wname
  tit<-paste0("frequency of words changes in ",party)
  ggplot(data=ff.ff[ff.ff$term%in%Name,])+
    geom_point(aes(x=year,y=ff.pre))+
    geom_smooth(aes(x=year,y=ff.pre))+
    labs(title=tit,y="precent")
}
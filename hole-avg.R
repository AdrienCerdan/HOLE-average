#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(RColorBrewer)

data=read.csv("hole-prof.dat", sep="")

#############################
### Arguments management ####
#############################

if (length(args)==0) {
  stop("At least one argument must be supplied --> i.e. Rscript hole-avg.R bin (start) (end)", call.=FALSE)
} else if (length(args)==1) {
  # default start and end
  args[2] = 2
  args[3] = dim(data)[2]
} else if (length(args)==2) {
  # default end
  args[3] = dim(data)[2]
}

bin=as.integer(args[1])
start=as.integer(args[2])
end=as.integer(args[3])

# the pore profil output start at 2
if (start == 1 | start == 0 ){
  start = 2
}

#############
### core ####
#############

dataprod <- data[,start:end]

# slice the data within the bin
myBreaks <- gl(ceiling((ncol(dataprod)-1)/bin), bin)[1:(ncol(dataprod)-1)]

# average the data per break
avg_data <- cbind(data[1], 
      sapply(levels(myBreaks), function(y) {
        rowMeans(dataprod[-1][myBreaks == y])
      }))

# number of bin
nbin=(length(levels(myBreaks)))

# generate the legend
ini=0
leg= character(0)
for (i in 1:nbin){
  last=(ini+bin/100)
  if (i == nbin){
    last=(ncol(dataprod)/100)
  }
  leg=c(leg,paste(ini,"-",last,"ns"))
  ini=last
}

##############
### plot  ####
##############

mypalette<-brewer.pal(nbin,"Spectral")
mypalette=c(mypalette,"black","gray")
png('pore_avg.png',width=800,height=700)
matplot(avg_data[,1], y=cbind(avg_data[,2:(ncol(avg_data))],dataprod[,ncol(dataprod)],data[,2]), type = "l",pch=1,col = mypalette, ylim=c(0,10), xlab="Z pore", ylab="pore radius (A)", main="Pore profil GlyR : Gly", axes=FALSE, lwd=3, lty=1)
axis(side=2, at=seq(0, 10, by=0.5))
axis(side=1, at=seq(-60, 60, by=20))
legend("topright", legend =c(leg,"last","ini"), col=mypalette, pch=19)

abline(h=seq(1, 3, by=0.5), col="black", lty="dotted")
dev.off()

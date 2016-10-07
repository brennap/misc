#!/usr/bin/Rscript

# bash code for generating SysStat csvs
##!/bin/bash
#for PL in $(ls); do
#   if [ -d $PL ]; then
#      for SRV in $(ls $PL); do
#         if [ -d $PL/$SRV ]; then
#            for NN in $(seq -w 01 31); do 
#               sadf -td -- -d $PL/$SRV/var/log/sa/sa$NN | sed '1s/^# //' > $PL/sa${NN}_disk_$SRV.csv2 
#               #sadf -td -- -n ALL $PL/$SRV/var/log/sa/sa$NN | sed '1s/^# //' > $PL/sa${NN}_net_$SRV.csv2 
#            done
#         fi
#      done
#   fi
#done

install.packages("ggplot2")
library(ggplot2)

#topdir <- readline(prompt="Top level directory: ")
topdir <- "/home/brennapj/TASK0015295"
# map pools and servers
pools <- list()
for ( pl in list.dirs(path=topdir, recursive=F, full.names=F)) {
   pools[pl] <- list(list.dirs(path=paste(topdir, pl, sep='/'), recursive=F, full.names=F))
}

for ( pl in names(pools) ){
   for ( srv in pools[[pl]] ){
      cur.df <- data.frame()
      # Weird list because 18-31 is Aug., 1-16 is Sep., and 17 is missing; formatC pads 1 -> 01
      for (DD in formatC((c(18:31, 1:16)), width=2, format="d", flag="0")) {
         # Glob our csvs into a dataframe
         cur.df <- rbind(cur.df, read.csv2(file=paste(topdir, '/', pl, '/sa', DD, '_disk_', srv, '.csv2', sep=''), comment.char="#"))
         #for ( csv in list.files(path=topdir, recursive=T, pattern=paste(pl, '/sa', DD, '_disk.', srv, '.csv2', sep='')) {
         #}
      }
      # Data cleaning/formatting
      for ( col in c("tps", "rd_sec.s", "wr_sec.s", "avgrq.sz", "avgqu.sz", "await", "svctm", "X.util")) {
         cur.df[[col]] <- as.numeric(as.character(cur.df[[col]]))
      }
      cur.df$timestamp <- strptime(cur.df$timestamp, "%Y-%m-%d %H:%M:%S")
      cur.df$DISK <- factor(sub("-[0-9]*", "", cur.df$DEV))
      # end data clean
      summary(subset(cur.df, DISK == "dev252"))
      # Identify dists that have experienced over 100% utilization
      summary(subset(cur.df, DISK == "dev252" & X.util > 100))
      qplot(data=subset(cur.df, DISK == "dev252" & tps > 500), x=as.POSIXlt(timestamp), y=tps, xlab="Time", ylab="Transactions per Second", colour=DEV) + geom_line()
   }
}

#list.dirs(path=paste(topdir, pools[1], sep='/'), recursive=F, full.names=F)
#tmp.df <- read.csv2(file="~/TASK0015295/ovm023/sa14_disk_oim62028010.csv2")


ggplot(data=tmp.df, aes(x=as.POSIXlt(timestamp), y=tps, colour=DEV)) + geom_line()
qplot(data=tmp.df, x=as.POSIXlt(timestamp), y=tps, xlab="Time", ylab="Transactions per Second", colour=DEV, geom="line")
qplot(data=tmp.df, x=as.POSIXlt(timestamp), y=tps, xlab="Time", ylab="Transactions per Second", colour=DEV, geom="line", facets=" ~ DISK")

# Graph only the multipath disks
qplot(data=subset(tmp.df, DISK == "dev252"), x=as.POSIXlt(timestamp), y=tps, xlab="Time", ylab="Transactions per Second", colour=DEV, geom="line")
qplot(data=subset(tmp.df, DISK == "dev252" & tps > 500), x=as.POSIXlt(timestamp), y=tps, xlab="Time", ylab="Transactions per Second", colour=DEV) + geom_line()
qplot(data=subset(cur.df, DISK == "dev252" & X.util >= 100), x=DEV, y=tps, xlab="Device", ylab="Transactions per Second", geom="boxplot") + coord_flip()
qplot(data=subset(cur.df, DISK == "dev252" & X.util >= 100), x=as.POSIXlt(timestamp), y=tps, xlab="Time", ylab="Transactions per Second", geom="smooth")

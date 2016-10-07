#!/usr/bin/Rscript

#library("lattice")
library("ggplot2")

csv.default <- paste(Sys.getenv("PWD"),"TASK0015295_vmdd.csv",sep="/")
csv <- readline(prompt=paste0("Location of vmdd.csv [",csv.default,"]: "))
if (csv == "") {csv <- csv.default}
while (!file.exists(csv)) {csv <- readline(prompt=paste0(csv,": file not found\nRe-enter location of vmdd.csv: "))}
rm(csv.default)
dd_info <- read.csv(file=csv)
rm(csv)
dd_info$date.time <- strptime(dd_info$date.time, "%Y-%m-%d %H:%M:%S")

# Group entries by pool
# There is probably a better way to do this...
ovm021 <- c("accods001","sufodb001","sufods001")
ovm023 <- c("accodb001","oraodb001","oraods001")
ovm008 <- c("oraodb002","oraods002","sufodb002")
dd_info$pool[c(dd_info$hostname %in% ovm021)] <- "ovm021"
dd_info$pool[c(dd_info$hostname %in% ovm023)] <- "ovm023"
dd_info$pool[c(dd_info$hostname %in% ovm008)] <- "ovm008"
dd_info$pool <- factor(dd_info$pool)

#lattice
#bwplot(hostname ~ X10G.thrpt | pool, data=dd_info, groups=pool, auto.key=T, horizontal=T, layout=c(1,3), xlab="MB/s", main="Througput Distribution on a 10G Write")

#ggplot2
pdf(file="vmdd.pdf", title="VM dd Test Results")

qplot(y=X10G.thrpt, ylab="MB/s", x=hostname, data=dd_info, geom="boxplot", main="Throughput Distribution on a 10G Write", colour=pool) + coord_flip()
qplot(y=X5G.thrpt, ylab="MB/s", x=hostname, data=dd_info, geom="boxplot", main="Throughput Distribution on a 5G Write", colour=pool) + coord_flip()
qplot(y=X1G.thrpt, ylab="MB/s", x=hostname, data=dd_info, geom="boxplot", main="Throughput Distribution on a 1G Write", colour=pool) + coord_flip()

qplot(y=X10G.time, ylab="Time (s)", x=hostname, data=dd_info, geom="boxplot", main="Duration Distribution on a 10G Write", colour=pool) + coord_flip()
qplot(y=X5G.time, ylab="Time (s)", x=hostname, data=dd_info, geom="boxplot", main="Duration Distribution on a 5G Write", colour=pool) + coord_flip()
qplot(y=X1G.time, ylab="Time (s)", x=hostname, data=dd_info, geom="boxplot", main="Duration Distribution on a 1G Write", colour=pool) + coord_flip()

dev.off()

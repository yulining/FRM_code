# −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
# Name of QuantLet : TENET_group_network
# −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
# Published in : TENET: Tail-Event driven NETwork risk
# −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
# Description : Plot a graph for group network
# −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
# Keywords : network, connectedness, spillover effects,
# contagion effects, adjacency matrix, tail event
# −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
# See also : 
# −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
# Author : Lining Yu 
# −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
# Submitted :
# −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−
# Datafile : 100_firms_returns_and_macro_2015-04-15.csv
# connectedness_matrix_time_point_80.csv
# −−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−−

# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

##install.packages("abind")
library(abind)
##install.packages("qgraph")
library(qgraph)
##install.packages("stats")
library(stats)
setwd("C:/Users/Lining Yu/Dropbox/Doctor/TENET/TENET Codes/2015_08_28 tenet quantnet codes")
setwd("C:/Users/yulining.hub/Dropbox/Doctor/TENET/TENET Codes/2015_08_28 tenet quantnet codes")
## read firms' data
f        = read.csv("100_firms_returns_and_macro_2015-04-15.csv")
## read the connectedness matrix based on estimated paritial derivatives on 12_06_2009 as an example
c        = read.csv("connectedness_matrix_time_point_80.csv")
con      = as.matrix(c)[,-1]

##extract the date
dt       = as.Date(f[,1],format = "%d/%m/%Y")
dt       = dt[49:314]

##read the returns of 100 firms
ff       = f[,2:(ncol(f)-7)]## The seven macro variables are ruled out.
names.fi = colnames(f)[2:101]
col      = c(rep("red",25),rep("blue",25),rep("green4",25),rep("mediumorchid4",25))#purple3
groups   = list(1:25,26:50,51:75,76:100)
shape    = c(rep("circle",25),rep("triangle",25),rep("diamond",25),rep("square",25)) # "" and "diamond"
plot_g   = qgraph(t(con), groups = groups,layout = "groups", layoutScale = c(1.2,1.2), label.font = 2, label.cex = 2, shape = "circle", labels = names.fi, esize = 5, maximum = max(con), color = "white", node.width = 0.8, label.cex = 1.8, label.color = col, edge.color = col, curve = 1, border.width = 1.2, border.color = col, asize = 2.5)
text(x   = -0.9, y = 1.0, labels = substitute( paste(d), list(d = as.character(dt[80])) ), xpd = NA, cex = 1.5)
legend(0.82, 1.1, box.lwd = 2,box.col = "white",bg = "white",c("Depositories","Insurers","Broker-Dealers","Others"),text.col = c("red","blue","green4","purple3"),cex = 1.3)

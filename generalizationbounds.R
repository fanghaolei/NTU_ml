# plot different generalization bounds 
# set ggplot canvas
library(ggplot2)
p <- ggplot(data = data.frame(x = 0))

# plot VC bound
ovc <- function(x){
    sqrt(8/x*(log(80)+50*log(2*x)))
}
p + stat_function(fun = ovc, geom = "line") + xlim(1000, 10000)

# plot Rademacher Penalty Bound
rpb <- function(x){
    sqrt(2/x*(log(2) + 51*log(x))) + sqrt(2/x*log(20)) + 1/x
}
p + stat_function(fun = rpb, geom = "line") + xlim(1000, 10000)

# plot Parrondo and Van den Broek 
pvdb <- function(x){
    1/x +sqrt(1+x*(log(120) + 50*log(2*x)))/x # root of the equation
}
p + stat_function(fun = pvdb, geom = "line") + xlim(1000, 10000)

# plot Devroye
devr <- function(x){
    1/(x-2) + sqrt(4*(1-log(80)-100*log(x)) + 2*x)/(2*x-4)
}
p + stat_function(fun = devr, geom = "line") + xlim(1, 10000)

# plot variant VC bound
vvc <- function(x){
    sqrt(16/x*(log(2/sqrt(3)) + 50*log(x)))
}
p + stat_function(fun = vvc, geom = "line") + xlim(1000, 10000)

# in one plot
p + stat_function(fun = devr, geom = "line", aes(color = "Devroye")) +
    stat_function(fun = pvdb, geom = "line", aes(color = "P & B")) +
    stat_function(fun = vvc, geom = "line", aes(color = "Variant VC"))+
    stat_function(fun = rpb, geom = "line", aes(color = "Rademacher"))+ 
    stat_function(fun = ovc, geom = "line", aes(color = "oiginal VC"))+
    scale_color_manual("Generalization Bounds", 
                       values =c("red","blue", "black", "green", "orange"))+
    xlim(1000, 10000)
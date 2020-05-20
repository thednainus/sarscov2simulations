library(ggplot2)
library(sarscov2simulations)
library(reshape2)
library(lhs)

setwd("Coalescent_simulations/")
source("seijrRmodel.R")


set.seed(25011979)
# sample parameters (default is set to 100)
# parameters that varies
parameters <- sampler(n = 30)
#parameters that are fixed
parameters["tau"] <- 74
parameters["p_h"] <- 0.2
parameters["exogGrowthRate"] <- 25
parameters["gamma0"] <- 73.0
parameters["gamma1"] <- 121.667
parameters["gammaExog"] <-44.0



comptraj <- function( theta){
  x0 <- c( E = theta[['E']] , Il = 1.0E-8, Ih = 1.0E-8, exog = 1.75, #exog = 6.5846e-3,
           R = 0.0, S = theta[["S"]], infections = 0.0)
  (s = dm( theta , x0 = x0, t0 = 2019.92, t1 = 2020.25 , res = 200 ))

}
#parm_test <- parameters[5,]
#s1 = comptraj( parm_test )



traj <- apply(parameters, 1, comptraj)
size <- nrow(parameters)
rownames(parameters) <- (1:size)

param_list <- split(parameters, rownames(parameters))


mapply(plot_parameters,
       traj,
       param_list,
       as.character(sort(as.numeric(names(param_list)))),
       as.character(sort(as.numeric(names(param_list))))
)


plot_parameters <- function(s, parameters, title, filename){

  s.df <- as.data.frame(s[[5]])
  s.df.m <- melt(s.df, id.vars=c("time"))

  print(parameters)

  ggplot(subset(s.df.m, round(time, 3) >= round(parameters[["st"]], 3)),
         aes(x = time, y = value)) + geom_line(aes(color = variable), size=0.7) +
    theme_bw() + ggtitle(title)

  ggsave(paste(filename, "pdf", sep="."), useDingbats=FALSE, width=19.3, height=11.1)
}


#s.df1 <- as.data.frame(s1[[5]])
#s.df.m1 <- melt(s.df1, id.vars=c("time"))
#ggplot(subset(s.df.m1, round(time, 3) >= round(parm_test$st, 3)),
#       aes(x = time, y = value)) + geom_line(aes(color = variable), size=0.7) +
#  theme_bw() + ggtitle("param5")




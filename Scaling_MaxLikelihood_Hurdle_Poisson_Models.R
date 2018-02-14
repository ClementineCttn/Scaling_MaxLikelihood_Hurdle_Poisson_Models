setwd("~/Documents/OF_scaling")
#import and order data
OF = read.csv("OF.csv", header=T) 
OF = OF[order(-OF$POPULATION),]

####### Generate the python commands to run Leitao et al. (2016) maximum likelihood estimations
t = dim(OF)[1]
listN = c(10, seq(30, t, 25)) #subset cities by batch of 25
l = 0
cm = data.frame()
for (n in listN){
  o = OF[1:n,c("IDAIRE","POPULATION", "JOBS", "STOCKFDI", "FLUXFDI")]
  rownames(o) = NULL
  assign(paste0("OF", n), o)
  for (v in c("JOBS", "STOCKFDI", "FLUXFDI")){
    ov = o[,c("IDAIRE","POPULATION",v)]
    write.table(x=ov, file=paste0("leitao/general/OF", n, v, ".txt"), sep="\t",row.names = F, quote = F)
    for (m in c("ConstrainedDAnalysis", "ConstrainedDFixedBetaAnalysis")){
      l = l + 1
      cm[l, 1]=  paste0("python analysis_run2.py 512 general ", m, " ", v, " POPULATION OF", n, v, ".txt")
        }
  }
  write.csv(x=cm, file=paste0("leitao/commands.csv"), sep="\t",row.names = F, quote = F)
}

### Hurdle Models
# from http://data.library.virginia.edu/getting-started-with-hurdle-models/
# Ref on zero-inflated models: 
# Cameron AC, Trivedi PK (2013). Regression Analysis of Count Data. Cambridge University Press, Cambridge.
# Kleiber C, Zeileis A (2008). Applied Econometrics with R. Springer-Verlag, New York. ISBN 978-0-387-77316-2.
# Zeileis A, Kleiber C, Jackman S (2008). “Regression Models for Count Data in R”. Journal of Statistical Software, 27(8). URL https://www.jstatsoft.org/article/view/v027i08.

require("AER")
require("pscl")
 
 l = 0
 tab = data.frame()
 for (n in listN){
   o = OF[1:n,c("IDAIRE","POPULATION", "JOBS", "STOCKFDI", "FLUXFDI")]
   rownames(o) = NULL
   assign(paste0("OF", n), o)
   for (v in c("JOBS", "STOCKFDI", "FLUXFDI")){
     df = o[,c("POPULATION",v)]
      colnames(df) = c("Pop", "Y")
     # simple fit with poisson
      l = l+1
     mod1 <- glm(Y ~ log(Pop), data = df, family = "poisson")

     tab[l,"model"] = "Poisson"
      tab[l,"y"] = v
     tab[l,"x"] = "POPULATION"
     tab[l,"alpha"] =  summary(mod1)$coefficients[1,1]
     tab[l,"beta"] =  summary(mod1)$coefficients[2,1]
     tab[l,"beta_error"] =  summary(mod1)$coefficients[2,2]
     tab[l,"pval"] = summary(mod1)$coefficients[2,4]
     tab[l,"BIC"] = BIC(mod1)
     tab[l,"file"] = paste0("OF", n)
     
     if(min(df$Y) == 0){
     l = l+1
     mod.hurdle <- hurdle(Y ~ log(Pop), data = df, dist = "poisson", zero.dist = "binomial")
     
     tab[l,"model"] = "zeroInflatedHurdle"
     tab[l,"y"] = v
     tab[l,"x"] = "POPULATION"
     tab[l,"alpha"] =  summary(mod.hurdle)$coefficients[[1]][1,1]
     tab[l,"beta"] =  summary(mod.hurdle)$coefficients[[1]][2,1]
     tab[l,"beta_error"] =  summary(mod.hurdle)$coefficients[[1]][2,2]
     tab[l,"pval"] = summary(mod.hurdle)$coefficients[[1]][2,4]
     tab[l,"alpha_zero"] =  summary(mod.hurdle)$coefficients[[2]][1,1]
     tab[l,"beta_zero"] =  summary(mod.hurdle)$coefficients[[2]][2,1]
     tab[l,"beta_error_zero"] =  summary(mod.hurdle)$coefficients[[2]][2,2]
     tab[l,"pval_zero"] = summary(mod.hurdle)$coefficients[[2]][2,4]
     tab[l,"BIC"] = (-2* logLik(mod.hurdle)) + (4*log(n))
     tab[l,"file"] = paste0("OF", n)     
     }
   }
  }
 write.csv(x=tab, file=paste0("results_hurdle_poisson.csv"))
 
 
 
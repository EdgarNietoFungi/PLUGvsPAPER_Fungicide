#Hi alll l
packages <- c("tidyverse","agricolae", "mosaic", "broom", "car","lawstat" )
lapply( packages, require, character.only = TRUE)


plugdiscs <- read.csv("data/Paper_discs_vs_PLUG.csv", stringsAsFactors = TRUE, na.strings = TRUE)
Y <- read.csv("data/Paper_discs_vs_PLUG.csv", stringsAsFactors = TRUE, na.strings = TRUE)

stopifnot(nrow(plugdiscs)== 7*11*2*2*2*4)
tapply(plugdiscs$growth, plugdiscs$inoculum, mean, na.rm= TRUE)
#plugdiscs <- plugdiscs  %>% unite ("index", c(Fungicide, experiment, experimental_repetition,inoculum, treatment, growth), remove = FALSE)
plugdiscs$inoculum <- as.character(plugdiscs$inoculum)

fungs <- levels(Y$F)
ino <- unique(Y$ino)
ept <- levels(Y$experiment)
fungs <- levels(Y$F)
ino <- unique(Y$ino)
ept <- levels(Y$experiment)
trt <- levels(Y$tr)
er <- levels(Y$experimental_r)

i <- j <- k <- l <- m <- 1 

head(Y)

des <- matrix(NA,nrow=308,ncol=1)


jj <- 1

for(i in 1:length(fungs))
{
  for(j in 1:length(ino))
  {
    for(k in 1:length(ept))
    {
      for(l in 1:length(trt))
      {
        
        index1 <- Y$F==fungs[i] & Y$i==ino[j] & Y$experiment==ept[k] & Y$tr==trt[l] & Y$experimental==er[1] 
        index2 <- Y$F==fungs[i] & Y$i==ino[j] & Y$experiment==ept[k] & Y$tr==trt[l] & Y$experimental==er[2] 
        
        #des[jj,1] <- ifelse((range(Y[index1,]$g,na.rm=T)[2]-range(Y[index2,]$g,na.rm=T)[1]),1,0)
        des[jj,1] <- range(Y[index1,]$g,na.rm=T)[2]-range(Y[index2,]$g,na.rm=T)[1]
        
        print(c(i,j,k,l,jj))
        jj <- jj+1
      }
    }
  }
}


308-length(which(des>0))

x <- plugdiscs["Fungicide"]
head(x)
y <- plugdiscs[,1]
head(y)
z <- plugdiscs[1,1]
head(z)
m <- plugdiscs[3,]
head(m)

plugdiscs %>% ggplot(aes(y = growth, x= Fungicide, fill = experiment)) + geom_boxplot() + facet_wrap(~treatment)
plugdiscs %>% group_by(Fungicide, treatment, experiment) %>% do(tidy(shapiro.test(.$growth))) #missing levene test

"resamp2" <-
function(X, rep=1) {
 require(vegan)
  Samp <- function(size, Indivs) {  length(table(sample(Indivs, size,
          replace=F)))  }
   N<-X[X>0]
   TotSp <- length(N)
   TotInds<-sum(N)#### Accumulation curve up to the maximum number of
   #individuals
   Sp <- 1:TotSp
   Inds <- rep(1:TotSp, N)
   Size <- rep(floor(TotInds*0.25):TotInds, rep) ###Note its(TotInds*0.25):TotInds
   sp.count <- sapply(Size, Samp, Inds)
   plot(Size,sp.count,xlab="Number of individuals",ylab="Number of species",log="x")
   Sp.Ac1<-lm(sp.count~log(Size))
   Sp.Ac2<-lm(sp.count~log2(Size))
   fa<-fisher.alpha(N)
   res<-list(Logslope=Sp.Ac1$coefficients[2],Log2slope=Sp.Ac2$coefficients[2],Alpha=fa)
   return(res)
}



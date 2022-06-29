library(microbenchmark)

find_naive <- function(x,samples,n) {
  d <- apply(samples,1,function(z){sum((x-z)^2)})
  lowest_n <- order(d)[1:n]
  samples[lowest_n,]
}

find_with_cluster <- function(x,cls,n) {
  d <- apply(cls$centers,1,function(z){sum((x-z)^2)})
  lowest_n <- order(d)[1:2]
  r <- NULL
  dd <- NULL
  for (i in lowest_n) {
    d <- apply(cls$points[[i]],1,function(z){sum((x-z)^2)})
    o2 <- order(d)[1:n]
    dd <- c(dd,d[o2])
    r <- rbind(r,cls$points[[i]][o2,])
  }
  o <- order(dd)[1:n]
  r[o,]
}

res <- data.frame()
for (ns in seq(5,7,length.out=10)) {
  nsample <- floor(10^ns)
  ncluster <- floor(nsample/100)
  cat("Sample size=",nsample," Number of cluster=",ncluster,"\n")
  
  # generate samples
  samples <- matrix(runif(nsample*2),ncol=2)
  
  # clustering
  cls <- kmeans(samples,ncluster)
  
  # generate the list of points in a cluster
  cls_list <- list()
  for (i in seq_len(ncluster)) {
    cls_list[[i]] <- samples[cls$cluster==i,]
  }
  
  cluster_info <- list(
    ncluster = ncluster,
    points = cls_list,
    centers = cls$centers
  )
  
  b1 <- microbenchmark({
    x <- runif(2)
    p1 <- find_naive(x,samples,2)
  },times=1)
  b2 <- microbenchmark({
    x <- runif(2)
    p2 <- find_with_cluster(x,cluster_info,2)
  },times=1)
  res <- rbind(res,
               data.frame(samples=nsample,
                          naive=mean(b1$time)/1e6,
                          cluster=mean(b2$time)/1e6))
}

#plot(samples)
#points(x[1],x[2],col=2,pch=16)
#points(p1[,1],p1[,2],col=3,pch=16)
#points(p2[,1],p2[,2],col=4,pch=16)


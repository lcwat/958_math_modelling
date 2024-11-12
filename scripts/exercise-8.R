## Exercise 8: Cluster analysis
## use cluster analysis to define common groups along several dimensions of data


# load libraries ----------------------------------------------------------

library(tidyverse)
library(NbClust) # cluster statistics to determine best number of clusters
library(factoextra) # view results of cluster tests from nbclust
library(ggdendro) # plot dendrograms with ggplot grammar
library(ggpubr)

# load data ---------------------------------------------------------------

occup_data <- read_csv("data/occupation-characteristics.csv")

colnames(occup_data)

# occupation is the occupation, iq is iq for that occupation, data/people/things are rated from 
# 0 to 8 in terms of high to low complexity

summary(occup_data)

# need it in a matrix
occup_data <- data.frame(occup_data, row.names = 1) # turn occupation into rownames

# standardize the data
stand_occup <- scale(occup_data)

# use this!!
stand_occup


# 1. do a ward cluster analysis -------------------------------------------

# use hclust with method = "ward"
ward_occup_hclust <- hclust(dist(stand_occup), method = "ward.D")

# try some different plots to decide where to cut tree into how many clusters
plot(ward_occup_hclust$height) # view heights by index 

# view cluster statistics
nb_ward_occup_clust <- NbClust(stand_occup, distance = "euclidean", method = "ward.D")

# ccc specifically believes the best k is 15
nb_ward_occup_clust <- NbClust(stand_occup, distance = "euclidean", method = "ward.D", index = "ccc")

# ******************************************************************* 
#   * Among all indices:                                                
#   * 1 proposed 2 as the best number of clusters 
# * 6 proposed 3 as the best number of clusters 
# * 5 proposed 4 as the best number of clusters 
# * 2 proposed 6 as the best number of clusters 
# * 2 proposed 8 as the best number of clusters 
# * 1 proposed 13 as the best number of clusters 
# * 6 proposed 15 as the best number of clusters 
# 
# ***** Conclusion *****                            
#   
#   * According to the majority rule, the best number of clusters is  3 
# 
# 
# *******************************************************************  

# plot the ward d results as dendogram
plot(ward_occup_hclust, labels = rownames(occup_data))

# plot nb clust results
fviz_nbclust(nb_ward_occup_clust)

# save
ggsave(
  "plots/ex-8-ward-nb-clust-majority-results.png", device = "png",
  height = 5, width = 8, units = "in"
)

# visualize specific methods like elbow, silhouette and gap stat
# Elbow method
fviz_nbclust(stand_occup, hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

# looks like 3 or maybe 4

# Silhouette method
fviz_nbclust(stand_occup, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# looks like 4

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(stand_occup, hcut, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

# looks like 1 or 2

# i'm going to go with three clusters given the results of the nb clust majority and visual inspection
profiling <- cutree(ward_occup_hclust, 3)
clust_occup_wardd <- data.frame(occup_data, profiling)

# now can calculate and plot some results
count(clust_occup_wardd, profiling)

#   profiling  n
# 1         1 55
# 2         2 40
# 3         3 52


# plotting ----------------------------------------------------------------

#######
# the following code is from a blog post by Atrebas from 2019 to add functionality
# on top of ggdendro
# link: https://atrebas.github.io/post/2019-06-08-lightweight-dendrograms/

# ggdendro dendro_data extension that will automatically cutree for you
dendro_data_k <- function(hc, k) { 
  hcdata <- ggdendro::dendro_data(hc, type = "rectangle") 
  seg <- hcdata$segments 
  labclust <- cutree(hc, k)[hc$order] 
  segclust <- rep(0L, nrow(seg)) 
  heights <- sort(hc$height, decreasing = TRUE) 
  height <- mean(c(heights[k], heights[k - 1L]), na.rm = TRUE) 
  
  for (i in 1:k) { 
    xi <- hcdata$labels$x[labclust == i] 
    idx1 <- seg$x >= min(xi) & seg$x <= max(xi) 
    idx2 <- seg$xend >= min(xi) & seg$xend <= max(xi) 
    idx3 <- seg$yend < height 
    idx <- idx1 & idx2 & idx3 
    segclust[idx] <- i 
  } 
  
  idx <- which(segclust == 0L) 
  segclust[idx] <- segclust[idx + 1L] 
  hcdata$segments$clust <- segclust 
  hcdata$segments$line <- as.integer(segclust < 1L) 
  hcdata$labels$clust <- labclust 
  
  hcdata 
  
}

set_labels_params <- function(
    nbLabels,
    direction = c("tb", "bt", "lr", "rl"),
    fan = FALSE
  ) {
  if (fan) {
    angle <- 360 / nbLabels * 1:nbLabels + 90
    idx <- angle >= 90 & angle <= 270
    angle[idx] <- angle[idx] + 180
    hjust <- rep(0, nbLabels)
    hjust[idx] <- 1
  } else {
    angle <- rep(0, nbLabels)
    hjust <- 0
    if (direction %in% c("tb", "bt")) { angle <- angle + 45 }
    if (direction %in% c("tb", "rl")) { hjust <- 1 }
  }
  
  list(angle = angle, hjust = hjust, vjust = 0.5)
}

plot_ggdendro <- function(
    hcdata, 
    direction = c("lr", "rl", "tb", "bt"),
    fan = FALSE,
    scale.color = NULL,
    branch.size = 1,
    label.size  = 3, nudge.label = 0.01, expand.y    = 0.1
  ) {
  
  direction <- match.arg(direction) # if fan = FALSE
  ybreaks <- pretty(segment(hcdata)$y, n = 5)
  ymax <- max(segment(hcdata)$y)
  
  ## branches
  p <- ggplot() +
    geom_segment(
      data = segment(hcdata), 
      aes(
        x = x, y = y, xend = xend, yend = yend, 
        linetype = factor(line), colour = factor(clust)
      ), 
      lineend = "round", show.legend  =  FALSE, size = branch.size
    )
  
  ## orientation
  if (fan) {
    p <- p +
      coord_polar(direction = -1) +
      scale_x_continuous(breaks = NULL, limits = c(0, nrow(label(hcdata)))) +
      scale_y_reverse(breaks = ybreaks)
  } else {
    p <- p + scale_x_continuous(breaks = NULL)
    if (direction %in% c("rl", "lr")) {
      p <- p + coord_flip()
    }
    if (direction %in% c("bt", "lr")) {
      p <- p + scale_y_reverse(breaks = ybreaks)
    } else {
      p <- p + scale_y_continuous(breaks = ybreaks)
      nudge.label <- -(nudge.label)
    }
  }
  
  # labels
  labelParams <- set_labels_params(nrow(hcdata$labels), direction, fan)
  hcdata$labels$angle <- labelParams$angle
  
  p <- p +
    geom_text(
      data = label(hcdata),
      aes(
        x = x, y = y, label = label, colour = factor(clust), angle = angle
      ), 
      vjust = labelParams$vjust, hjust = labelParams$hjust,
      nudge_y = ymax * nudge.label, size = label.size,
      show.legend = FALSE
    )
  
  # colors and limits
  if (!is.null(scale.color)) {
    p <- p + scale_color_manual(values = scale.color)
  }
  
  ylim <- -round(ymax * expand.y, 1)
  p <- p + expand_limits(y = ylim)
  
  p
}

mtc <- scale(mtcars)
D   <- dist(stand_occup)
hc  <- hclust(D)

hcdata <- dendro_data_k(hc, 15)

# basic plot
p <- plot_ggdendro(
  hcdata,
  direction   = "lr",
  expand.y    = 0.2
)

# set colors for clusters
cols <- c("gray", "#5DFDCB", "#7CC6FE", "#D88373")

# horizontal tree 
p <- plot_ggdendro(
  hcdata,
  direction = "tb",
  scale.color = cols,
  label.size = 2.5,
  branch.size = 0.5,
  expand.y = 0.2
)

p <- p + theme_void() + expand_limits(x = c(-1, 32))

# fanned tree
p <- plot_ggdendro(
  hcdata,
  fan = TRUE, #scale.color = cols,
  label.size = 4, nudge.label = 0.02,
  expand.y = 0.4
)

# make black background
mytheme <- theme(panel.background = element_rect(fill = "black"))

p + theme_void() + mytheme

# save it
ggsave(
  "plots/ex-8-ward-fan-dend-15.png", device = "png",
  height = 11.5, width = 11.5, units = "in"
)

# run a k-means cluster analysis ------------------------------------------

# use nbclust with method = "k-means" and i'm going to go with 3 clusters
# b/c it was preferred by a lot of clustering metrics
kmean_nb <- NbClust(
  stand_occup, distance = "euclidean", method = "kmeans", min.nc = 2, max.nc = 15
)

# plot results
fviz_nbclust(kmean_nb)

# save
ggsave(
  "plots/ex-8-kmean-nb-clust-majority-result.png", device = "png",
  height = 5, width = 8, units = "in"
)

# the majority has ruled that k = 3

# now run kmeans
kmeans_occup_clust <- kmeans(stand_occup, centers = 3, nstart = 25)

kmeans_occup_clust

# cluster one: above ave. IQ, high data, high people, low things
# - knowledge of people and data but not specific things, maybe generalists?
# people you go to for a specific problem 
# cluster two: below ave. IQ, low data, low people, ave. things
# - maybe data entry and service? connectors 
# cluster three: ave. IQ, ave. data, ave. people, high things
# - maybe specialists? use their knowledge on specific things 
# range of issues encountered in job

# viz of kmeans clusters using pca to reduce dimensions of orig data down to two, 
# returns ggplot obj that can be modified to liking
p2 <- fviz_cluster(
  kmeans_occup_clust, data = stand_occup, 
  stand = F, repel = T, ellipse = T, ellipse.type = "t"
)

p2 + theme_pubr() +
  scale_color_manual(
    "Cluster", values = c("#5DA890", "#7CC6FE", "#D88373"), 
    labels = c("Generalists", "Connectors", "Specialists")
  ) +
  scale_fill_manual(
    "Cluster", values = c("#5DA890", "#7CC6FE", "#D88373"), 
    labels = c("Generalists", "Connectors", "Specialists")
  ) +
  guides(shape = "none") +
  theme(
    plot.title = element_blank(),
    legend.position = "left"
  )

# save it
ggsave(
  "plots/ex-8-kmeans-fviz-cluster-plot.png", device = "png",
  height = 8.5, width = 11.5, units = "in"
) 

# extras ------------------------------------------------------------------

# some code from stack exchange fixing the functions to display nb clust results
# fix the functions
fviz_nbclust <- function (x, FUNcluster = NULL, method = c("silhouette", "wss", 
                                                           "gap_stat"), diss = NULL, k.max = 10, nboot = 100, verbose = interactive(), 
                          barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", 
                          print.summary = TRUE, ...) 
{
  set.seed(123)
  if (k.max < 2) 
    stop("k.max must bet > = 2")
  method = match.arg(method)
  if (!inherits(x, c("data.frame", "matrix")) & !("Best.nc" %in% 
                                                  names(x))) 
    stop("x should be an object of class matrix/data.frame or ", 
         "an object created by the function NbClust() [NbClust package].")
  if (inherits(x, "list") & "Best.nc" %in% names(x)) {
    best_nc <- x$Best.nc
    if (any(class(best_nc) == "numeric") ) 
      print(best_nc)
    else if (any(class(best_nc) == "matrix") )
      .viz_NbClust(x, print.summary, barfill, barcolor)
  }
  else if (is.null(FUNcluster)) 
    stop("The argument FUNcluster is required. ", "Possible values are kmeans, pam, hcut, clara, ...")
  else if (!is.function(FUNcluster)) {
    stop("The argument FUNcluster should be a function. ", 
         "Check if you're not overriding the specified function name somewhere.")
  }
  else if (method %in% c("silhouette", "wss")) {
    if (is.data.frame(x)) 
      x <- as.matrix(x)
    if (is.null(diss)) 
      diss <- stats::dist(x)
    v <- rep(0, k.max)
    if (method == "silhouette") {
      for (i in 2:k.max) {
        clust <- FUNcluster(x, i, ...)
        v[i] <- .get_ave_sil_width(diss, clust$cluster)
      }
    }
    else if (method == "wss") {
      for (i in 1:k.max) {
        clust <- FUNcluster(x, i, ...)
        v[i] <- .get_withinSS(diss, clust$cluster)
      }
    }
    df <- data.frame(clusters = as.factor(1:k.max), y = v, 
                     stringsAsFactors = TRUE)
    ylab <- "Total Within Sum of Square"
    if (method == "silhouette") 
      ylab <- "Average silhouette width"
    p <- ggpubr::ggline(df, x = "clusters", y = "y", group = 1, 
                        color = linecolor, ylab = ylab, xlab = "Number of clusters k", 
                        main = "Optimal number of clusters")
    if (method == "silhouette") 
      p <- p + geom_vline(xintercept = which.max(v), linetype = 2, 
                          color = linecolor)
    return(p)
  }
  else if (method == "gap_stat") {
    extra_args <- list(...)
    gap_stat <- cluster::clusGap(x, FUNcluster, K.max = k.max, 
                                 B = nboot, verbose = verbose, ...)
    if (!is.null(extra_args$maxSE)) 
      maxSE <- extra_args$maxSE
    else maxSE <- list(method = "firstSEmax", SE.factor = 1)
    p <- fviz_gap_stat(gap_stat, linecolor = linecolor, 
                       maxSE = maxSE)
    return(p)
  }
}

.viz_NbClust <- function (x, print.summary = TRUE, barfill = "steelblue", 
                          barcolor = "steelblue") 
{
  best_nc <- x$Best.nc
  if (any(class(best_nc) == "numeric") )
    print(best_nc)
  else if (any(class(best_nc) == "matrix") ) {
    best_nc <- as.data.frame(t(best_nc), stringsAsFactors = TRUE)
    best_nc$Number_clusters <- as.factor(best_nc$Number_clusters)
    if (print.summary) {
      ss <- summary(best_nc$Number_clusters)
      cat("Among all indices: \n===================\n")
      for (i in 1:length(ss)) {
        cat("*", ss[i], "proposed ", names(ss)[i], 
            "as the best number of clusters\n")
      }
      cat("\nConclusion\n=========================\n")
      cat("* According to the majority rule, the best number of clusters is ", 
          names(which.max(ss)), ".\n\n")
    }
    df <- data.frame(Number_clusters = names(ss), freq = ss, 
                     stringsAsFactors = TRUE)
    p <- ggpubr::ggbarplot(df, x = "Number_clusters", 
                           y = "freq", fill = barfill, color = barcolor) + 
      labs(x = "Number of clusters k", y = "Frequency among all indices", 
           title = paste0("Optimal number of clusters - k = ", 
                          names(which.max(ss))))
    return(p)
  }
}
# assign them to the factoextra namespace
environment(fviz_nbclust) <- asNamespace("factoextra")
assignInNamespace("fviz_nbclust",fviz_nbclust,"factoextra")
environment(.viz_NbClust) <- asNamespace("factoextra")
assignInNamespace(".viz_NbClust",.viz_NbClust,"factoextra")

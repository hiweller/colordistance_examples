# if needed:
# install.packages(patternize)

library(patternize)
library(magrittr)
library(colordistance)

####################################
#            patternize         ####
####################################
# List with samples
IDList <- dir("Scarus/", pattern = "*.txt", full.names = F) %>% xfun::sans_ext()

# make list with images
prepath <- "Scarus/"
extension <- "_white.png"
imageList <- patternize::makeList(IDList, type = "image", prepath = prepath, extension = extension)

# make list with landmarks
prepath <- "Scarus/"
extension <- ".txt"
landmarkList <- patternize::makeList(IDList, type = "landmark", prepath = prepath, extension = extension)

# colors in RGB (on a 0-255 scale, from trial and error):
teal <- c(20, 120, 110)
orange <- c(200, 120, 80)
brown <- c(145, 125, 85)
pink <- c(210, 150, 145)

# get a raster list for each color
# note that because Scarus flavipectoralis has no teal anywhere, you will get a
# warning for the teal rasterize
teal_raster <- patLanRGB(imageList, landmarkList, 
                         colOffset = 0.15, 
                         RGB = teal,
                         plot = T)
orange_raster <- patLanRGB(imageList, landmarkList, 
                           colOffset = 0.15,
                           RGB = orange, 
                           plot = T)
brown_raster <- patLanRGB(imageList, landmarkList, 
                          colOffset = 0.15,
                          RGB = brown, 
                          plot = T)
pink_raster <- patLanRGB(imageList, landmarkList, 
                         colOffset = 0.15,
                         RGB = pink, 
                         plot = T)

# run to plot maps of colors:
# for (i in list(teal_raster, orange_raster, brown_raster, pink_raster)) {
#   plotHeat(i, IDList)
#   
# }


### Principle components analysis ###

popList <- list(IDList) # set ID names
colList <- c("turquoise",  # set colors for plotting 
             "gold",
             "cornflowerblue", 
             "tomato",
             "orchid")
symbolList <- c(15:19) # set symbols for plotting

teal_pca <- patPCA(teal_raster[2:5], 
                   as.list(IDList), 
                   colList = colList[2:5],
                   symbolList = symbolList,
                   PCx = 1, PCy = 2,
                   plot = T)

orange_pca <- patPCA(orange_raster, 
                     as.list(IDList), 
                     colList = colList,
                     symbolList = symbolList,
                     PCx = 1, PCy = 2,
                     plot = T)

pink_pca <- patPCA(pink_raster, 
                   as.list(IDList), 
                   colList = colList,
                   symbolList = symbolList,
                   PCx = 1, PCy = 2,
                   plot = T)

brown_pca <- patPCA(brown_raster, 
                    as.list(IDList), 
                    colList = colList,
                    symbolList = symbolList,
                    PCx = 1, PCy = 2,
                    plot = T)

# combine all separate PCAs into a list
outPCA <- list(teal_pca, orange_pca, brown_pca, pink_pca)
dist.out <- vector("list", length = 4)
names(dist.out) <- c("teal", "orange", "brown", "pink")

# calculate distances using PC1 and PC2 as coordinates
# the result is a distance matrix for each color pattern between each pair of
# images
for (i in 1:length(outPCA)) {
  
  dist.out[[i]] <- outPCA[[i]][[3]]$x[, 1:2] %>% dist
  
}

# normalize each value, since principle component coordinates are not bounded
dist.out <- lapply(dist.out, function(i) i / max(i))

# make a distance matrix for storing average distances
avg.pca <- dist.out[[4]]

# # calculate average distances between each pair of images by taking the average of all the individual color pattern distances
# 
# # for Scarus flavipectoralis (i < 5), only average the brown, pink, and orange colors, because there is no teal
for (i in 1:length(dist.out[[2]])) {
  
  if (i < 5) {
    avg.pca[i] <- mean(c(dist.out[[2]][i],
                         dist.out[[3]][i],
                         dist.out[[4]][i]))
  } else {
    avg.pca[i] <- mean(c(dist.out[[1]][i - 4],
                         dist.out[[2]][i],
                         dist.out[[3]][i],
                         dist.out[[4]][i]))
  }
  
  
}

# plot the resulting heatmap
avg.pca <- as.matrix(avg.pca)
avg.pca[which(avg.pca == 0)] <- NA; avg.pca

colordistance::heatmapColorDistance(as.matrix(avg.pca))
return(avg.pca) 

####################################
#         colordistance         ####
####################################
# load image paths
images <- getImagePaths("Scarus/")

# optional: print the a- and b-channel ranges to decide ranges for channel
# for (i in 1:length(images)) {
#   temp <- loadImage(images[i], CIELab = T, ref.white = "D65")
#   print(apply(temp$filtered.lab.2d, 2, range))
# }

# set a and b ranges
a.range <- c(-60, 60)
b.range <- c(-70, 80)

# run analysis
imageClusterPipeline(images, 
                     color.space = "lab", 
                     ref.white = "D65", 
                     a.bounds = a.range, b.bounds = b.range,
                     lower = rep(0.8, 3), upper = rep(1, 3),
                     plot.heatmap = T)

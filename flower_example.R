# Load flower image
flower <- loadImage("Flower/flower_greenscreen.jpg", 
                    CIELab = TRUE, sample.size = 100000, ref.white = "D65",
                    lower = c(0, 0.45, 0), upper = c(0.8, 1, 0.8))

# plot in CIE Lab color space
plotPixels(flower, color.space = "lab", n = 10000,
           ref.white = "D65", from = "sRGB",
           main = "Pixels (CIE Lab)",
           ylim = c(-100, 100), zlim = c(-100, 100), angle=45,
           cex.lab=-1, cex.axis=1, y.margin.add=1)

# get histogram clusters
flower.hist <- getLabHist(flower, bins = 2, ref.white = "D65",
                          title = "Histogram color clusters",
                          ylab="Proportion of image")

# plot clusters in 3D space
scatter3dclusters(flower.hist, scaling=22, opacity=0.99, 
                  plus=0.03, y.margin.add=1,
                  type="h", lwd=1.5, angle=50, main="Clusters (CIE Lab)")

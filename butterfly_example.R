kmeans_fits <- getKMeansList("Butterfly_mimicry/", bins = 3,
                               lower = c(0.8, 0.8, 0.8), upper = c(1, 1, 1),
                               color.space = "lab", ref.white = "D65",
                               plotting = TRUE)

kmeans_list <- extractClusters(kmeans_fits, ordering = TRUE)

plots <- lapply(kmeans_list, function(i) scatter3dclusters(i, color.space="lab", scaling = 30, plus = 0.05))

getColorDistanceMatrix(kmeans_list, method = "emd")
getColorDistanceMatrix(kmeans_list, method = "chisq")

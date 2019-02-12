flounder_hist <- getLabHistList("Flounder_camouflage/", ref.white = "D65", 
                                  bins = c(2, 3, 5), 
                                  lower = c(0, 0.4, 0), upper = c(0.6, 1, 0.6),
                                  a.bounds = c(-20, 40), b.bounds = c(0, 50))

flounder_distance_matrix <- getColorDistanceMatrix(flounder_hist)

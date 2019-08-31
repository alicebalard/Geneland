PlotTessellation <-
    function (coordinates, path.mcmc, printit = FALSE, path, colfancy = heat.colors(100), plotCities = F) 
    {
        coordinates <- as.matrix(coordinates)
        param.postprocess <- as.matrix(read.table(paste(path.mcmc, 
                                                        "postprocess.parameters.txt", sep = "")))
        nxdom <- as.numeric(param.postprocess[1, 3])
        nydom <- as.numeric(param.postprocess[2, 3])
        param <- as.matrix(read.table(paste(path.mcmc, "parameters.txt", 
                                            sep = "")))
        delta.coord <- as.numeric(param[param[, 1] == "delta.coord", 
                                        3])
        filedom <- paste(path.mcmc, "proba.pop.membership.txt", sep = "")
        dom.post <- as.matrix(read.table(filedom))[, -(1:2)]
        npopmax <- ncol(dom.post)
        for (iclass in 1:npopmax) {
            dev.new()
            image(seq(min(coordinates[, 1] - delta.coord/2), max(coordinates[, 
                                                                             1] + delta.coord/2), length = nxdom), seq(min(coordinates[, 
                                                                                                                                       2] - delta.coord/2), max(coordinates[, 2] + delta.coord/2), 
                                                                                                                       length = nydom), matrix(dom.post[, iclass], nrow = nxdom, 
                                                                                                                                               ncol = nydom, byrow = TRUE), xlab = "x coordinates", 
                  ylab = "y coordinates", asp = 1, col = colfancy)
            ggmap(get_stamenmap())+
                scale_x_continuous( limits = c(min(coordinates[,1]), max(coordinates[,1])), expand = c(0,0))+
                scale_y_continuous( limits = c(min(coordinates[,2]), max(coordinates[,2])) , expand = c(0,0))
            title(main = paste("Map of posterior probability to belong to cluster ", 
                               iclass))
            contour(seq(min(coordinates[, 1] - delta.coord/2), max(coordinates[, 
                                                                               1] + delta.coord/2), length = nxdom), seq(min(coordinates[, 
                                                                                                                                         2] - delta.coord/2), max(coordinates[, 2] + delta.coord/2), 
                                                                                                                         length = nydom), matrix(dom.post[, iclass], nrow = nxdom, 
                                                                                                                                                 ncol = nydom, byrow = TRUE), add = TRUE)
            points(coordinates[, 1], coordinates[, 2], pch = 21, bg = "grey", col = "black", cex = 1)
            # Add cities locations and names, and scale, if wanted
            if(plotCities == TRUE){
                map.cities(country="Germany", label=T, minpop=25000, cex = 1.5, capitals = c(0,1), col = "white")
                map.scale(ratio = F, col = "white", linecol = "white")
            }
            if (printit == TRUE) {
                postscript(file = paste(path, "/mapproba.pop", iclass, 
                                        ".ps", sep = ""))
                image(seq(min(coordinates[, 1] - delta.coord/2), 
                          max(coordinates[, 1] + delta.coord/2), length = nxdom), 
                      seq(min(coordinates[, 2] - delta.coord/2), max(coordinates[, 
                                                                                 2] + delta.coord/2), length = nydom), matrix(dom.post[, 
                                                                                                                                       iclass], nrow = nxdom, ncol = nydom, byrow = TRUE), 
                      xlab = "x coordinates", ylab = "y coordinates", 
                      asp = 1)
                title(main = paste("Map of posterior probability to belong to cluster ", 
                                   iclass))
                contour(seq(min(coordinates[, 1] - delta.coord/2), 
                            max(coordinates[, 1] + delta.coord/2), length = nxdom), 
                        seq(min(coordinates[, 2] - delta.coord/2), max(coordinates[, 
                                                                                   2] + delta.coord/2), length = nydom), matrix(dom.post[, 
                                                                                                                                         iclass], nrow = nxdom, ncol = nydom, byrow = TRUE), 
                        add = TRUE)
                points(coordinates[, 1], coordinates[, 2], pch = 16)
                dev.off()
            }
        }
    }

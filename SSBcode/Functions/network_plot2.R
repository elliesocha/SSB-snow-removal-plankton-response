# Based off of https://github.com/tidymodels/corrr

network_plot2.cor_df <- function(rdf,
                                min_cor = .30,
                                legend = TRUE,
                                colours = c("indianred2", "white", "skyblue1"),
                                repel = TRUE,
                                curved = TRUE,
                                colors) {
  if (min_cor < 0 || min_cor > 1) {
    rlang::abort("min_cor must be a value ranging from zero to one.")
  }
  
  if (!missing(colors)) {
    colours <- colors
  }
  
  rdf <- as_matrix(rdf, diagonal = 1)
  distance <- 1 - abs(rdf)
  
  points <- if (ncol(rdf) == 1) {
    # 1 var: a single central point
    matrix(c(0, 0), ncol = 2, dimnames = list(colnames(rdf)))
  } else if (ncol(rdf) == 2) {
    # 2 vars: 2 opposing points
    matrix(c(0, -0.1, 0, 0.1), ncol = 2, dimnames = list(colnames(rdf)))
  } else {
    # More than 2 vars: multidimensional scaling to obtain x and y coordinates for points.
    suppressWarnings(stats::cmdscale(distance, k = 2))
  }
  
  if (ncol(points) < 2) {
    cont_flag <- FALSE
    shift_matrix <- matrix(1,
                           nrow = nrow(rdf),
                           ncol = ncol(rdf)
    )
    diag(shift_matrix) <- 0
    
    for (shift in 10^(-6:-1)) {
      shifted_distance <- distance + shift * shift_matrix
      points <- suppressWarnings(stats::cmdscale(shifted_distance))
      
      if (ncol(points) > 1) {
        cont_flag <- TRUE
        break
      }
    }
    
    if (!cont_flag) rlang::abort("Can't generate network plot.\nAttempts to generate 2-d coordinates failed.")
    
    rlang::warn("Plot coordinates derived from correlation matrix have dimension < 2.\nPairwise distances have been adjusted to facilitate plotting.")
  }
  
  
  
  points <- data.frame(points)
  colnames(points) <- c("x", "y")
  points$id <- rownames(points)
  
  # ### HD: random points
  # points$x = sample(seq(-1,1,by = 0.1), size = 7)
  # points$y = sample(seq(-1,1,by = 0.1), size = 7)
  
  # Create a proximity matrix of the paths to be plotted.
  proximity <- abs(rdf)
  proximity[upper.tri(proximity)] <- NA
  diag(proximity) <- NA
  proximity[proximity < min_cor] <- NA
  
  # Produce a data frame of data needed for plotting the paths.
  n_paths <- sum(!is.na(proximity))
  paths <- data.frame(matrix(nrow = n_paths, ncol = 6))
  colnames(paths) <- c("x", "y", "xend", "yend", "proximity", "sign")
  path <- 1
  for (row in 1:nrow(proximity)) {
    for (col in 1:ncol(proximity)) {
      path_proximity <- proximity[row, col]
      if (!is.na(path_proximity)) {
        path_sign <- sign(rdf[row, col])
        x <- points$x[row]
        y <- points$y[row]
        xend <- points$x[col]
        yend <- points$y[col]
        paths[path, ] <- c(x, y, xend, yend, path_proximity, path_sign)
        path <- path + 1
      }
    }
  }
  
  plot_ <- list(
    # For plotting paths
    if (curved) {
      geom_curve(
        data = paths,
        aes(
          x = x, y = y, xend = xend, yend = yend,
          alpha = proximity, size = proximity/10, # HD change size
          colour = proximity * sign
        )
      )
    },
    if (!curved) {
      geom_segment(
        data = paths,
        aes(
          x = x, y = y, xend = xend, yend = yend,
          alpha = proximity, size = proximity,
          colour = proximity * sign
        )
      )
    },
    scale_alpha(limits = c(0, 1)),
    scale_size(limits = c(0, 1)),
    scale_colour_gradientn(limits = c(-1, 1), colors = colours),
    # Plot the points
    geom_point(
      data = points,
      aes(x, y),
      size = 2, shape = 19, colour = "black"
    ),
    # Plot variable labels
    if (repel) {
      ggrepel::geom_text_repel(
        data = points,
        aes(x, y, label = id),
        # fontface = "bold", 
        size = 3,
        segment.size = 0.0,
        segment.color = "white"
      )
    },
    if (!repel) {
      geom_text(
        data = points,
        aes(x, y, label = id),
        # fontface = "bold", 
        size = 3
      )
    },
    # expand the axes to add space for curves
    expand_limits(
      x = c(
        min(points$x) - .2,
        max(points$x) + .2
      ),
      y = c(
        min(points$y) - .2,
        max(points$y) + .2
      )
    ),
    # Theme and legends
    theme_void(),
    guides(size = "none", alpha = "none"),
    if (legend) labs(colour = NULL),
    if (!legend) theme(legend.position = "none")
  )
  
  ggplot() + plot_
}
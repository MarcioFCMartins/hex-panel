# https://www.redblobgames.com/grids/hexagons/
# This script creates a hexagonal grid of a given size. Hexagons are drawn in
# cells selected by binomial draws, with a probability of presence that is
# modulated by perlin noise and a horizontal logistic gradient.

# Setup -------------------------------------------------------------------
library(ambient) # Perlin noise generator

# Reverse logistic equation - used to create a gradient of probability
# which decreases from left to rigth
# x0 is the x value of the inflection point
# k is the steepness of the curve
modified_logistic <- function(x, x0, k) { 
    y <- 1 / (1 + exp(-k * (x - x0)))
    # Scale between 1 and -1
    y <- 2 * (y - min(y))/(max(y) - min(y)) - 1
    
    return(y)
}

# Creates a matrix of hexagons presence/absence per cell
# Accepts a matrix with presence probabilities and a vector that represents the
# horizontal gradient values 
# The gain value increases the probability of presence overall and should be
# experimented with to generate good looking panels
create_matrix_binom <- function(noise, gradient, gain = 0.5, seed = 13){
        
    for(i in 1:ncol(noise)) {
        noise[, i] <- noise[, i] + gradient[i] + gain
    }
    
    noise[noise > 1] <- 1 
    noise[noise < 0] <- 0
    
    cells_to_sim <- noise != 1 & noise != 0
    
    set.seed(seed)
    noise[cells_to_sim] <- rbinom(length(noise[cells_to_sim]), 1, noise[cells_to_sim])
    
    return(noise)
}

# Draw a hexagon (flat face up) at center x and y, with radius r
draw_hexagon <- function(x, y, r, col = "black", gap = 0, ...) {
    r <- r - gap
    polygon(x = x + c(r * cos(0 * pi/ 6),
                      r * cos(2 * pi / 6), 
                      r * cos(4 * pi / 6), 
                      r * cos(6 * pi / 6), 
                      r * cos(8 * pi / 6), 
                      r * cos(10 * pi / 6)
    ),
    y = y + c(r * sin(0 * pi / 6),
              r * sin(2 * pi / 6),
              r * sin(4 * pi / 6),
              r * sin(6 * pi / 6),
              r * sin(8 * pi / 6),
              r * sin(10 * pi / 6)
    ),
    col = col, ...)
}

# Calculate the number of neighbors for a hexagon in position i(x), j(y) in 
# a matrix representing hexagons in an offset coordinate system
count_missing_neigbors <- function(hex_mat, i, j) {
    if(i %% 2 == 0) {
        # // even cols 
        # [[+1, +1], [ 0, +1], [ -1, 0], 
        #     [0, -1], [1, -1,], [+1, 0]],
        # // odd cols 
        # [[0, +1,], [-1, +1], [-1, 0,], 
        #     [-1, -1], [0, -1], [+1, 0]],
        # even col
        neighbors <- list(
            tryCatch(hex_mat[j - 1, i + 1],        
                     error = function(e) return(1)),
            tryCatch(hex_mat[j , i + 1],       
                     error = function(e) return(1)),
            tryCatch(hex_mat[j + 1, i],        
                     error = function(e) return(1)),
            tryCatch(hex_mat[j, i - 1],        
                     error = function(e) return(1)),
            tryCatch(hex_mat[j - 1, i - 1],    
                     error = function(e) return(1)),
            tryCatch(hex_mat[j - 1, i],    
                     error = function(e) return(1))
        )
    } else {
        # odd col
        neighbors <- list(
            tryCatch(hex_mat[j, i + 1],        
                     error = function(e) return(1)),
            tryCatch(hex_mat[j + 1, i + 1],       
                     error = function(e) return(1)),
            tryCatch(hex_mat[j + 1, i],        
                     error = function(e) return(1)),
            tryCatch(hex_mat[j + 1, i - 1],        
                     error = function(e) return(1)),
            tryCatch(hex_mat[j, i - 1],    
                     error = function(e) return(1)),
            tryCatch(hex_mat[j - 1, i],    
                     error = function(e) return(1))
        )
    }
    
    neighbors <- unlist(neighbors)
    neighbors <- ifelse(
        is.na(neighbors),
        0,
        neighbors
    ) 
    
    missing_neighbors <- length(neighbors) - sum(neighbors)
    
    # Return the number of empty adjacent cells
    return(missing_neighbors)
}


# Modify the color of a hexagon based on the number of missing neighbors
define_colors <- function(hex_mat, colors) {
    color_mat <- matrix(nrow = nrow(hex_mat), ncol = ncol(hex_mat))
    color_gradient <- colorRamp(colors, space = "Lab")
    
    for(i in 1:ncol(hex_mat)) {
        for(j in 1:nrow(hex_mat)) {
            missing_neighbors <- count_missing_neigbors(hex_mat, i, j)/6
            rgb_col <- color_gradient(missing_neighbors)
            color_mat[j, i] <- rgb(rgb_col[1], rgb_col[2], rgb_col[3], max = 255)
        }
    }
    
    return(color_mat)
}

draw_panel <- function(
        presence_mat, # Matrix of presence/absence probabilities
        hex_radius,   # Radius for hexagons
        color = "#60AA20", # Matrix of colors for hexagons, or a single color
        gap = 1,      # Gap between hexagons
        missing_neighbors = 2, # How many neighbors must be missing to add a gap
        coords = F,   # Add coordinates to hexagons?
        cex           # Size of coordinate text
){
    ncols <- ncol(presence_mat)
    nrows <- nrow(presence_mat)
    width <- (n_col - 1) * hex_radius * 1.5 + 2 * hex_radius
    height<- n_row * hex_radius * sqrt(3) + hex_radius * sqrt(3)
  
    
    plot.new()
    plot.window(
        xlim = c(0, width), 
        ylim = c(0, height)
        )
        
    for(i in 1:ncols) {
        for(j in 1:nrows) {
            # Offset the y coordinate of every other row
            offset_y <- ifelse(i %% 2 == 0, 0, sqrt(3) * hex_radius / 2)
            
            # Calculate the center of the hexagon
            center_x <- i * hex_radius * 1.5
            center_y <- j * sqrt(3) * hex_radius + offset_y
            
            # If color is a matrix, use the color in the matrix
            if(length(color) != 1) {
                col <- color[j, i]
            } else {
                col <- color
            }
            
            # Decide if hexagon should have a gap around it
            n_missing_neighbors <- count_missing_neigbors(presence_mat, i, j)
            # If there are more than `missing_neighbors` empty hexagons around, add a gap
            if(n_missing_neighbors >= missing_neighbors) {
                gap_this <- gap
            } else {
                gap_this <- 0
            }
            
            # Draw the hexagons
            if(presence_mat[j, i] == 1) {
                draw_hexagon(
                    x = center_x,
                    y = center_y, 
                    r = hex_radius, 
                    col = col, 
                    gap = gap_this)
            }
            
            # Add the height of the center of the hexagon
            if(coords){
                text(
                    x = center_x,
                    y = center_y,
                    labels = paste0(
                        round(center_x),
                        "\n",
                        round(center_y),
                        "\n",
                        i , "-", j
                    ),
                    cex = cex
                )
            }
            
        }
    }
}



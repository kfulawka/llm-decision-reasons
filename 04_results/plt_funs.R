# tile plots
tile_plts = function(xx, 
                     cols, 
                     grid = F, 
                     grid_col = 'grey',
                     ...) {
  
  # data dims
  nr = nrow(xx)
  nc = ncol(xx)
  
  # Create the tile plot using the `image()` function
  image(x = 1:nc,
        y = 1:nr,
        z = t(xx), 
        zlim = 0:1,
        col = cols, 
        ...)
  
  # add grid lines if requested
  if(grid) {
    
    abline(h = seq(.5, nr + .5, 1), col = grid_col)
    abline(v = seq(.5, nc + .5, 1), col = grid_col)
    
  }
  
}


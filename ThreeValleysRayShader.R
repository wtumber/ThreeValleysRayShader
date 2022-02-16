library(rasterVis)
library(raster)
library(rgl)
library(rgdal)
library(elevatr)

examp_df <- data.frame(x = c(6.5,6.625), y = c(45.2,45.453))
prj_dd <- "EPSG:4326"

# Create and example data.frame with additional columns
cats <- data.frame(category = c("H", "M"))

examp_df2 <- data.frame(examp_df, cats)

# Create an example SpatialPoints
examp_sp <- SpatialPoints(examp_df, proj4string = CRS(prj_dd))

# Create an example SpatialPointsDataFrame
examp_spdf <- SpatialPointsDataFrame(examp_sp, data = cats)

# data.frame example
elevation_df <- get_elev_raster(examp_df, prj = prj_dd, z = 10)

#And convert it to a matrix:
elmat <- rayshader::raster_to_matrix(elevation_df)


coords_to_xy <- function(x, y, elevation){
  require(raster)
  ex <- extent(elevation)
  x <- ((x-ex@xmin)/(ex@xmax-ex@xmin))*ncol(elevation)
  y <- (1-(y-ex@ymin)/(ex@ymax-ex@ymin))*nrow(elevation)
  out <- c(x,y)
  names(out) <- c("x","y")
  return(out)
}

val_thorens <- coords_to_xy(y= 45.298721,x=6.580011,elevation_df)
courchevel <- coords_to_xy(y= 45.430893,x=6.620386,elevation_df)
orelle <- coords_to_xy(y= 45.207662,x=6.545390,elevation_df)
meribel <- coords_to_xy(y= 45.398883,x=6.565651,elevation_df)
menuires <- coords_to_xy(y= 45.324114,x=6.537450,elevation_df)
brides <- coords_to_xy(y=45.452280 ,x=6.566388,elevation_df)
saint <- coords_to_xy(y= 45.379172,x=6.50551,elevation_df)

elmat %>%
  rayshader::sphere_shade(texture = "imhof2",sunangle = 45 ) %>%
  #rayshader::add_shadow(rayshader::ambient_shade(elmat), 0) %>%
  rayshader::plot_3d(elmat, zscale = 30, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(3840, 2160))

rayshader::render_label(elmat, x=val_thorens["x"], y=val_thorens["y"], z = 3000, zscale = 30,relativez = F,
                        text = "Val Thorens", textsize = 1, linewidth = 3)
rayshader::render_label(elmat, x=courchevel["x"], y=courchevel["y"], z = 3000, zscale = 30,relativez = F,
                        text = "Courchevel", textsize = 1, linewidth = 3)
rayshader::render_label(elmat, x=orelle["x"], y=orelle["y"], z = 3000, zscale = 30,relativez = F,
                        text = "Orelle", textsize = 1, linewidth = 3)
rayshader::render_label(elmat, x=meribel["x"], y=meribel["y"], z = 3000, zscale = 30,relativez = F,
                        text = "Meribel", textsize = 1, linewidth = 3)
rayshader::render_label(elmat, x=menuires["x"], y=menuires["y"], z = 3000, zscale = 30,relativez = F,
                        text = "Les Menuires", textsize = 1, linewidth = 3)
rayshader::render_label(elmat, x=brides["x"], y=brides["y"], z = 3000, zscale = 30,relativez = F,
                        text = "Brides-les-Bains", textsize = 1, linewidth = 3)
rayshader::render_label(elmat, x=saint["x"], y=saint["y"], z = 3000, zscale = 30,relativez = F,
                        text = "Saint-Martin-de-Belleville", textsize = 1, linewidth = 3)
rayshader::render_snapshot("Locations",clear=TRUE)
# rayshader::render_highquality(lightdirection = c(-45,45), lightaltitude  = 30, clamp_value = 10,
#                    samples = 256, camera_lookat= c(0,-50,0),
#                    ground_material = rayrender::diffuse(color="grey50",checkercolor = "grey20", checkerperiod = 100),
#                    clear = TRUE)

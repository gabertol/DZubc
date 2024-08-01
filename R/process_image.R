#' Process an Image to Extract and Visualize Object Centroids
#'
#' This function processes an image to extract the centroids of objects based on their blue channel,
#' filters the objects based on specific criteria, and generates a plot with the centroids marked.
#'
#' @param image_path A string specifying the path to the image file.
#' @param name A string specifying the prefix for naming samples. Default is 'sample_'.
#' @return A list containing the plot with centroids marked, the image width and height, and a data frame of valid centroids.
#' @import dplyr
#' @import ggplot2
#' @import EBImage
#' @importFrom grid rasterGrob
#' @importFrom imager load.image
#' @importFrom grDevices as.raster
#' @export
#' @examples
#' result <- process_image("path/to/image.png")
#' print(result$plot)
process_image <- function(image_path, name = 'sample_') {

  # Load the image
  image <- load.image(image_path)


  # Extract the blue channel and binarize the image
  blue_channel <- image[,,1,3]
  threshold <- 0.5
  binary_image <- blue_channel > threshold
  binary_image_eb <- as.Image(binary_image)
  labeled_image <- bwlabel(binary_image_eb)



  # Compute object properties
  object_props <- as.data.frame(computeFeatures.moment(labeled_image))
  object_shapes <- as.data.frame(computeFeatures.shape(labeled_image))

  # Filter valid objects
  valid_objects <- cbind(object_props, object_shapes) %>%
    filter(s.radius.mean > 0.5,
           m.majoraxis > 2,
           m.theta != 0,
           s.area > 5)

  # Create a data frame of valid centroids
  valid_centroids <- data.frame(
    id = row_number(valid_objects),
    center_x = valid_objects$m.cx,
    center_y = valid_objects$m.cy,
    center_z = 1,
    area = valid_objects$s.area,
    major_axis = valid_objects$m.majoraxis,
    radius_mean = valid_objects$s.radius.mean,
    radius_sd = valid_objects$s.radius.sd,
    radius_max = valid_objects$s.radius.max,
    radius_min = valid_objects$s.radius.min,
    angle = valid_objects$m.theta,
    perimeter = valid_objects$s.perimeter,
    eccentricity = valid_objects$m.eccentricity
  ) %>%
    mutate(minor_axis = sqrt((1 - eccentricity^2) * major_axis^2),
           angularity = radius_mean / radius_sd,
           ratio = major_axis / minor_axis,
           sample = paste0(name, row_number(.)),
           center_y = dim(image)[2] - center_y,
           Name=paste0(name,'_',id)) %>%
    arrange(id)

  # Calculate image dimensions
  img_width <- dim(image)[1] / 200
  img_height <- dim(image)[2] / 200

  # Create the plot
  plot <- ggplot() +
    annotation_custom(grid::rasterGrob(as.raster(image), interpolate = TRUE),
                      xmin = 0, xmax = dim(image)[1],
                      ymin = 0, ymax = dim(image)[2]) +
    geom_point(data = valid_centroids, aes(x = center_x, y = center_y),
               color = "red", size = 0.6) +
    geom_text(data = valid_centroids, aes(x = center_x, y = center_y, label = id),
              color = "red", size = 1.5, vjust = -1) +
    theme_void() +
    coord_fixed(ratio = 1, xlim = c(0, dim(image)[1]), ylim = c(0, dim(image)[2]), expand = FALSE)

  return(list(plot = plot, width = img_width, height = img_height, valid_centroids = valid_centroids))
}

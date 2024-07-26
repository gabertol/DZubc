
process_image <- function(image_path, name='sample_') {
  image <- imager::load.image(image_path)

  # Verify that the image was loaded correctly
  if (is.null(dim(image))) {
    stop("The image was not loaded correctly.")
  }

  blue_channel <- image[,,1,3]
  threshold <- 0.5
  binary_image <- blue_channel > threshold
  binary_image_eb <- EBImage::as.Image(binary_image)
  labeled_image <- bwlabel(binary_image_eb)
  object_props <- as.data.frame(computeFeatures.moment(labeled_image))
  object_shapes <- as.data.frame(computeFeatures.shape(labeled_image))

  valid_objects <- cbind(object_props, object_shapes) %>%
    filter(s.radius.mean > 0.5,
           m.majoraxis > 2,
           m.theta != 0,
           s.area > 5)

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
           center_y=dim(image)[2] - center_y) %>%
    arrange(id)

  img_width <- dim(image)[1] / 200
  img_height <- dim(image)[2] / 200

  plot <- ggplot() +
    annotation_custom(rasterGrob(as.raster(image), interpolate = TRUE),
                      xmin = 0, xmax = dim(image)[1],
                      ymin = 0, ymax = dim(image)[2]) +
    geom_point(data = valid_centroids, aes(x = center_x, y = center_y),
               color = "red", size = 0.8) +
    geom_text(data = valid_centroids, aes(x = center_x, y = center_y, label = id),
              color = "red", size = 2, vjust = -1) +
    theme_void() +
    coord_fixed(ratio = 1, xlim = c(0, dim(image)[1]), ylim = c(0, dim(image)[2]), expand = FALSE)

  return(list(plot = plot, width = img_width, height = img_height, valid_centroids = valid_centroids))
}

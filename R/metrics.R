#' Calculate and Plot Metrics from a Data Frame
#'
#' This function calculates additional metrics from a data frame, filters the data, and creates histograms of the metrics.
#'
#' @param file A data frame containing the data to be processed.
#' @param scale a scale factor from georeference_image function.
#' @param angle_cor Angle of the object to correct the directions
#' @return A ggplot object with histograms of the calculated metrics.
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @export
#' @examples
#' df <- data.frame(sample = c('sample_1', 'sample_2'), radius_max = c(10, 20), radius_min = c(2, 5),
#'                  angle = c(0.5, -0.5), area = c(500, 10000))
#' plot <- metrics(df)
#' print(plot)


metrics <- function(file,scale=1,angle_cor=0) {

  processed_data <- file %>%
    mutate(r = row_number()) %>%
    arrange(r) %>%
    dplyr::select(sample, everything(), -r,-Name) %>%
    mutate(ratio2 = radius_max / radius_min,
           angle2 = ifelse(angle > 0, angle / (pi / 360), 360 - (-angle) / (pi / 360))+angle_cor) %>%
    filter(area < 8000, area > 5) %>%
    mutate(across(.cols = c(area:radius_min,perimeter,eccentricity,minor_axis),~.x * scale )) %>%
    select(-center_z, -center_x, -center_y, -id) %>%
    pivot_longer(cols = 2:length(.))

  plot <- processed_data %>%
    ggplot(aes(x = value)) +
    geom_histogram()+
    facet_wrap(~name, scales = "free")

  return(plot)

}

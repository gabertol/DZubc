#' Calculate and Plot Metrics from a Data Frame
#'
#' This function calculates additional metrics from a data frame, filters the data, and creates histograms of the metrics.
#'
#' @param file A data frame containing the data to be processed.
#' @param scale a scale factor from georeference_image function.
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

  # Calcular valores médios para cada variável
  means <- processed_data %>%
    group_by(name) %>%
    summarize(mean_value = mean(value, na.rm = TRUE))

  # Criação do plot
  plot <- processed_data %>%
    ggplot(aes(x = value)) +
    geom_histogram() +
    facet_wrap(~name, scales = "free") +
    geom_vline(data = means, aes(xintercept = mean_value), color = "red", linetype = "dashed") +
    geom_text(data = means, aes(x = mean_value, y = 0, label = round(mean_value, 2)),
              color = "red", vjust = -0.5, hjust = -0.5, angle = 90)

  return(plot)

}

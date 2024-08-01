#' Georeference an Image
#'
#' This function calculates the affine transformation matrix needed to georeference an image
#' using provided reference points in the image and their corresponding points in the real world.
#'
#' @param ref_points_image A matrix of reference points in the image. Each row should represent a point (x, y).
#' @param ref_points_world A matrix of reference points in the real world. Each row should represent a point (x, y).
#'
#' @return A list containing:
#' \describe{
#'   \item{transform_matrix}{The affine transformation matrix.}
#'   \item{scale_x}{The scale factor in the x direction.}
#'   \item{scale_y}{The scale factor in the y direction.}
#'   \item{scale}{The average scale factor.}
#'   \item{angle}{The rotation angle in degrees.}
#' }
#'
#' @examples
#' # Example usage:
#' ICP_coordinates<-data.frame(X=c(16.1990,22.884,14.9028),Y=c(84.7310,79.9283,80.9070)) %>% as.matrix()
#' IMAGE_coordinates<-data.frame(X=c(2085.685,1375.439,2909.453 ),Y=c(2846.163,1508.357,2023.621)) %>% as.matrix()
#' result <- georeference_image(ICP_coordinates, IMAGE_coordinates)
#' print(result)
#'
#' @export
georeference_image <- function(ref_points_image, ref_points_world) {

  # Converter para matrizes numéricas
  ref_points_image <- as.matrix(ref_points_image)
  ref_points_world <- as.matrix(ref_points_world)

  # Calcular a matriz de transformação afim
  transform_matrix <- calculate_affine_transform(ref_points_image, ref_points_world)

  # Extrair escala e rotação da matriz de transformação afim
  scale_x <- sqrt(transform_matrix[1,1]^2 + transform_matrix[2,1]^2)
  scale_y <- sqrt(transform_matrix[1,2]^2 + transform_matrix[2,2]^2)
  scale <- (scale_x + scale_y) / 2
  angle <- atan2(transform_matrix[2,1], transform_matrix[1,1]) * 180 / pi

  return(list(transform_matrix = transform_matrix, scale_x = scale_x, scale_y = scale_y, scale = scale, angle = angle))
}


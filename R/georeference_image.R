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
#' ref_points_image <- matrix(c(10, 20, 30, 40, 50, 60), ncol = 2, byrow = TRUE)
#' ref_points_world <- matrix(c(100, 200, 300, 400, 500, 600), ncol = 2, byrow = TRUE)
#' result <- georeference_image(ref_points_image, ref_points_world)
#' print(result)
#'
#' @export
georeference_image <- function(ref_points_image, ref_points_world) {

  # Converter para matrizes numéricas
  ref_points_image <- as.matrix(ref_points_image)
  ref_points_world <- as.matrix(ref_points_world)

  # Verificar se as matrizes são numéricas
  if (!is.numeric(ref_points_image) || !is.numeric(ref_points_world)) {
    stop("'ref_points_image' e 'ref_points_world' devem ser matrizes numéricas.")
  }

  # Calcular a matriz de transformação afim
  transform_matrix <- calculate_affine_transform(ref_points_image, ref_points_world)

  # Extrair escala e rotação da matriz de transformação afim
  scale_x <- sqrt(transform_matrix[1,1]^2 + transform_matrix[2,1]^2)
  scale_y <- sqrt(transform_matrix[1,2]^2 + transform_matrix[2,2]^2)
  scale <- (scale_x + scale_y) / 2
  angle <- atan2(transform_matrix[2,1], transform_matrix[1,1]) * 180 / pi

  return(list(transform_matrix = transform_matrix, scale_x = scale_x, scale_y = scale_y, scale = scale, angle = angle))
}


calculate_affine_transform <- function(src, dst) {
  A <- matrix(c(src[1,], 1, src[2,], 1, src[3,], 1), nrow = 3, byrow = TRUE)
  B <- matrix(c(dst[1,], dst[2,], dst[3,]), nrow = 3, byrow = TRUE)
  transform <- solve(A, B)
  return(transform)
}



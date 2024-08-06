#' Apply Affine Transformation to Points
#'
#' This function applies an affine transformation to a set of points using a given transformation matrix.
#'
#' @param points A data frame or matrix with two columns representing the coordinates of the points (X, Y).
#' @param transform_matrix A 3x3 matrix representing the affine transformation matrix.
#' @return A data frame with the transformed coordinates (X, Y).
#' @export
#' @examples
#' ICP_coordinates<- matrix(c(16.1990,22.884,14.9028,84.7310,79.9283,80.9070),nrow=3)
#'
#' IMAGE_coordinates<- matrix(c(2085.685,1375.439,2909.453,2846.163,1508.357,2023.621),nrow=3)
#'
#' transform_matrix <- georeference_image(ICP_coordinates, IMAGE_coordinates)
#'
#'FULL_IMAGE_COORDINATES<-matrix(c(seq(1500,2500,by=50),Y=seq(1750,2750,by=50)),nrow=21)
#'
#'apply_affine_transform(FULL_IMAGE_COORDINATES, transform_matrix$transform_matrix)
apply_affine_transform <- function(points, transform_matrix) {
  # Add a column of 1s for the affine transformation
  points <- cbind(points, 1)

  # Apply the affine transformation
  transformed <- points %*% transform_matrix

  # Remove the column of 1s
  transformed_points <- data.frame(X = transformed[, 1],
                                   Y = transformed[, 2])

  return(transformed_points)
}

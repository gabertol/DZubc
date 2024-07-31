#' Apply Affine Transformation to Points
#'
#' This function applies an affine transformation to a set of points using a given transformation matrix.
#'
#' @param points A data frame or matrix with two columns representing the coordinates of the points (X, Y).
#' @param transform_matrix A 3x3 matrix representing the affine transformation matrix.
#' @return A data frame with the transformed coordinates (X, Y).
#' @export
#' @examples
#' points <- data.frame(X = c(1, 2, 3), Y = c(4, 5, 6))
#' transform_matrix <- matrix(c(1, 0, 0, 0, 1, 0, 1, 1, 1), nrow = 3, byrow = TRUE)
#' apply_affine_transform(points, transform_matrix)
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

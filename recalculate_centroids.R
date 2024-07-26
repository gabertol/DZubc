#' Recalculate Centroids Based on Reference Points
#'
#' This function recalculates the centroids of objects based on old and new reference points, applying an affine transformation.
#'
#' @param centroids A data frame containing the centroids with columns `center_x`, `center_y`, and `center_z`.
#' @param ref_points_old A data frame containing the old reference points with columns `ref_x`, `ref_y`, and `ref_z`.
#' @param ref_points_new A data frame containing the new reference points with columns `X`, `Y`, and `Z`.
#' @return A data frame with recalculated centroids containing columns `X`, `Y`, `Z`, and `id`.
#' @import dplyr
#' @export
#' @examples
#' centroids <- data.frame(center_x = c(1, 2), center_y = c(3, 4), center_z = c(5, 6), id = c(1, 2))
#' ref_points_old <- data.frame(ref_x = c(1, 2), ref_y = c(3, 4), ref_z = c(5, 6))
#' ref_points_new <- data.frame(X = c(2, 3), Y = c(4, 5), Z = c(6, 7))
#' recalculated <- recalculate_centroids(centroids, ref_points_old, ref_points_new)
#' print(recalculated)
recalculate_centroids <- function(centroids, ref_points_old, ref_points_new) {

  # Matrix of old reference points
  old_points <- as.matrix(ref_points_old %>% select(ref_x, ref_y))
  old_points <- cbind(old_points, 1)  # Add column of 1s for affine transformation

  # Matrix of new reference points
  new_points <- as.matrix(ref_points_new %>% select(X, Y))

  # Calculate the affine transformation using QR decomposition
  transformation_matrix <- qr.solve(old_points, new_points)

  # Apply the affine transformation
  centroids_matrix <- as.matrix(centroids %>% select(center_x, center_y))
  centroids_matrix <- cbind(centroids_matrix, 1)  # Add column of 1s
  transformed_centroids <- centroids_matrix %*% transformation_matrix

  # Convert matrix back to data frame
  transformed_centroids <- as.data.frame(transformed_centroids)
  names(transformed_centroids) <- c("X", "Y")
  transformed_centroids$id <- centroids$id

  # Recalculate Z using the transformation of the Z points
  old_z <- ref_points_old$ref_z
  new_z <- ref_points_new$Z
  scale_z <- mean(new_z / old_z)

  transformed_centroids$Z <- centroids$center_z * scale_z

  return(transformed_centroids)
}

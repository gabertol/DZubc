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
  # Extraindo coordenadas antigas e novas
  old_x <- ref_points_old$X
  old_y <- ref_points_old$Y
  new_x <- ref_points_new$X
  new_y <- ref_points_new$Y

  # Calculando as transformações de escala e translação
  scale_x <- (new_x[2] - new_x[1]) / (old_x[2] - old_x[1])
  scale_y <- (new_y[2] - new_y[1]) / (old_y[2] - old_y[1])
  trans_x <- new_x[1] - scale_x * old_x[1]
  trans_y <- new_y[1] - scale_y * old_y[1]

  # Aplicando transformações aos centróides
  centroids <- centroids %>%
    mutate(
      X_new = X * scale_x + trans_x,
      Y_new = Y * scale_y + trans_y,
      Z = 0
    )

  return(centroids)
}


#' Calculate Affine Transformation Matrix
#'
#' This function calculates the affine transformation matrix that maps a set of source points to a set of destination points.
#'
#' @param src A matrix or data frame with three rows and two columns representing the coordinates of the source points (X, Y).
#' @param dst A matrix or data frame with three rows and two columns representing the coordinates of the destination points (X, Y).
#' @return A 3x3 matrix representing the affine transformation matrix.
#' @export
#' @examples
#'
#' src <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)
#' dst <- matrix(c(7, 8, 9, 10, 11, 12), nrow = 3, byrow = TRUE)
#' calculate_affine_transform(src, dst)

calculate_affine_transform <- function(src, dst) {
  A <- matrix(c(src[1, ], 1, src[2, ], 1, src[3, ], 1), nrow = 3, byrow = TRUE)
  B <- matrix(c(dst[1, ], dst[2, ], dst[3, ]), nrow = 3, byrow = TRUE)
  transform <- solve(A, B)
  return(transform)
}

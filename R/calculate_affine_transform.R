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
#'src<- matrix(c(16.1990,22.884,14.9028,84.7310,79.9283,80.9070),nrow=3)
#'
#'dst<- matrix(c(2085.685,1375.439,2909.453,2846.163,1508.357,2023.621),nrow=3)
#'
#' calculate_affine_transform(src, dst)

calculate_affine_transform <- function(src, dst) {
  A <- matrix(c(src[1, ], 1, src[2, ], 1, src[3, ], 1), nrow = 3, byrow = TRUE)
  B <- matrix(c(dst[1, ], dst[2, ], dst[3, ]), nrow = 3, byrow = TRUE)
  transform <- solve(A, B)
  return(transform)
}

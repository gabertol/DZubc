

recalculate_centroids <- function(centroids, ref_points_old, ref_points_new) {
  # Matriz dos pontos antigos
  old_points <- as.matrix(ref_points_old %>% select(ref_x, ref_y))
  old_points <- cbind(old_points, 1)  # Adiciona coluna de 1s para a transformação afim

  # Matriz dos novos pontos
  new_points <- as.matrix(ref_points_new %>% select(X, Y))

  # Calcular a transformação afim usando a decomposição QR
  transformation_matrix <- qr.solve(old_points, new_points)

  # Aplicar a transformação afim
  centroids_matrix <- as.matrix(centroids %>% select(center_x, center_y))
  centroids_matrix <- cbind(centroids_matrix, 1)  # Adiciona coluna de 1s
  transformed_centroids <- centroids_matrix %*% transformation_matrix

  # Converter de matriz de volta para data frame
  transformed_centroids <- as.data.frame(transformed_centroids)
  names(transformed_centroids) <- c("X", "Y")
  transformed_centroids$id <- centroids$id

  # Recalcular Z usando a transformação dos pontos Z
  old_z <- ref_points_old$ref_z
  new_z <- ref_points_new$Z
  scale_z <- mean(new_z / old_z)

  transformed_centroids$Z <- centroids$center_z * scale_z

  return(transformed_centroids)
}

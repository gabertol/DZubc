#'
#' This functions call an app to run geoference tool
#'
#' @return A shiny app
#' @import dplyr
#' @import ggplot2
#' @import shiny
#' @import tidyr
#' @export

app <- function() {
  # Set the maximum file upload size to 30 MB
  options(shiny.maxRequestSize = 30 * 1024^2)

  # Define the user interface (UI)
  ui <- fluidPage(
    titlePanel("UBC detrital zircon position and morphometry"),

    tabsetPanel(
      tabPanel("1. Import",
               h2("Import and Export Files"),
               fileInput("file1", "Choose an image file",
                         accept = c("image/png", "image/jpeg")),
               textOutput("file_info"),
               plotOutput("image_plot"),
               downloadButton("downloadImage", "Download Processed Image")
      ),
      tabPanel("2. Referencing",
               h2("Rescale and Rotate"),
               fluidRow(
                 column(4,
                        h3("Reference Point 1"),
                        selectizeInput("ref1_id", "Select Sample by ID", choices = NULL, options = list(maxOptions = 5000)),
                        numericInput("ref1_x", "Reference 1 - X", value = 0),
                        numericInput("ref1_y", "Reference 1 - Y", value = 0),
                        numericInput("ref1_z", "Reference 1 - Z", value = 0)
                 ),
                 column(4,
                        h3("Reference Point 2"),
                        selectizeInput("ref2_id", "Select Sample by ID", choices = NULL, options = list(maxOptions = 5000)),
                        numericInput("ref2_x", "Reference 2 - X", value = 0),
                        numericInput("ref2_y", "Reference 2 - Y", value = 0),
                        numericInput("ref2_z", "Reference 2 - Z", value = 0)
                 ),
                 column(4,
                        h3("Reference Point 3"),
                        selectizeInput("ref3_id", "Select Sample by ID", choices = NULL, options = list(maxOptions = 5000)),
                        numericInput("ref3_x", "Reference 3 - X", value = 0),
                        numericInput("ref3_y", "Reference 3 - Y", value = 0),
                        numericInput("ref3_z", "Reference 3 - Z", value = 0)
                 )
               ),
               actionButton("recalculate", "Recalculate Centroids"),
               numericInput("num_samples", "Number of Samples to Export", value = 100, min = 1),
               actionButton("random_select", "Select Random Samples"),
               dataTableOutput("centroids_table"),
               downloadButton("downloadFullTable", "Download Full Centroids Table"),  # Botão para baixar os dados completos
               downloadButton("downloadSampledTable", "Download Sampled Centroids Table")  # Botão para baixar os dados após amostragem
      ),

      tabPanel("3. Morphometrics",
               h2("Metrics Plot"),
               plotOutput("metrics_plot")
      )
    )
  )

  # Define the server

  server <- function(input, output, session) {

    # Display file information
    output$file_info <- renderText({
      req(input$file1)
      paste("File name:", input$file1$name,
            "File size:", round(input$file1$size / 1024^2, 2), "MB")
    })

    # Reactive function to process the uploaded image and return valid centroids
    processed_image <- reactive({
      req(input$file1)
      image_path <- input$file1$datapath
      image_name <- tools::file_path_sans_ext(input$file1$name)
      process_image(image_path, name=image_name)
    })

    valid_centroids <- reactive({
      req(processed_image())
      processed_image()$valid_centroids
    })

    # Update selectizeInput choices with IDs
    observe({
      req(valid_centroids())
      updateSelectizeInput(session, "ref1_id", choices = valid_centroids()$id, server = TRUE)
      updateSelectizeInput(session, "ref2_id", choices = valid_centroids()$id, server = TRUE)
      updateSelectizeInput(session, "ref3_id", choices = valid_centroids()$id, server = TRUE)
    })

    # Display the processed image
    output$image_plot <- renderPlot({
      req(processed_image())
      print(processed_image()$plot)
    })

    # Function to download the processed image
    output$downloadImage <- downloadHandler(
      filename = function() {
        paste0(tools::file_path_sans_ext(input$file1$name), "_processed.png")
      },
      content = function(file) {
        plot_data <- processed_image()
        ggsave(file, plot = plot_data$plot, width = plot_data$width, height = plot_data$height, units = "in", dpi = 150)
      }
    )

    # Store selected reference points
    selected_refs <- reactive({
      req(input$ref1_id, input$ref2_id, input$ref3_id)

      ref1 <- valid_centroids() %>% filter(id == !!input$ref1_id)
      ref2 <- valid_centroids() %>% filter(id == !!input$ref2_id)
      ref3 <- valid_centroids() %>% filter(id == !!input$ref3_id)

      return(data.frame(
        id = c(1, 2, 3),
        X = c(input$ref1_x, input$ref2_x, input$ref3_x),
        Y = c(input$ref1_y, input$ref2_y, input$ref3_y),
        Z = c(input$ref1_z, input$ref2_z, input$ref3_z)
      ))
    })

    # Recalculate centroids based on selected reference points
    recalculated_centroids <- eventReactive(input$recalculate, {
      req(input$file1)

      ref_points_old <- valid_centroids() %>%
        filter(id %in% c(input$ref1_id, input$ref2_id, input$ref3_id)) %>%
        arrange(match(id, c(input$ref1_id, input$ref2_id, input$ref3_id)))

      ref_points_new <- selected_refs()

      centroids <- processed_image()$valid_centroids

      # Use the georeference_image function from DZubc package
      georef_result <- georeference_image(ref_points_old[, c("center_x", "center_y")], ref_points_new[, c("X", "Y")])

      transformed_points <- apply_affine_transform(as.matrix(centroids %>% select(center_x, center_y)), georef_result$transform_matrix)

      recalculated <- cbind(centroids, transformed_points)

      return(list(recalculated=recalculated, scale=georef_result$scale, angle_cor=georef_result$angle))
    })

    random_samples <- reactiveVal(NULL)

    observeEvent(input$random_select, {
      req(recalculated_centroids())
      num_samples <- input$num_samples
      recalculated <- recalculated_centroids()$recalculated

      selected_samples <- recalculated %>%
        sample_n(num_samples) %>%
        mutate(Type="Spot",
               X=1000*X,
               Y=1000*Y
        ) %>%
        dplyr::select(Name,Type,X,Y)
      random_samples(selected_samples)
    })



    output$centroids_table <- renderDataTable({
      req(random_samples())
      random_samples()
    })

    output$downloadFullTable <- downloadHandler(
      filename = function() {
        paste0(tools::file_path_sans_ext(input$file1$name), "_full.csv")
      },
      content = function(file) {
        write.csv(recalculated_centroids()$recalculated %>% mutate(across(.cols = c(area:radius_min,perimeter,eccentricity,minor_axis),~.x * recalculated_centroids()$scale )), file, row.names = FALSE)
      }
    )

    output$downloadSampledTable <- downloadHandler(
      filename = function() {
        paste0(tools::file_path_sans_ext(input$file1$name), "_sampled.csv")
      },
      content = function(file) {
        write.csv(random_samples(), file, row.names = FALSE)
      }
    )

    # Display the metrics plot
    output$metrics_plot <- renderPlot({
      req(recalculated_centroids())
      metrics(recalculated_centroids()$recalculated, scale=recalculated_centroids()$scale, angle_cor=recalculated_centroids()$angle)
    })
  }

  # Run the Shiny app
  shinyApp(ui = ui, server = server)




}


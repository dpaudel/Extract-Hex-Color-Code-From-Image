library(shiny)
library(magick)
library(dplyr)
library(ggplot2)
library(grid)

# Increase max upload size to 30 MB
options(shiny.maxRequestSize = 30 * 1024^2)

# UI
ui <- fluidPage(
  # Add CSS to change the background color of the page
  tags$style(HTML("
    body {
      background-color: #faf6fd;  
    }
    .well {
      background-color: #e1dde3;  
    }
  ")),
  titlePanel("Extract Hex Color Codes From Image"),
  h4("(c) Dev Paudel, dpaudel@outlook.com"),
  p("Upload an image/logo from which you want to extract the most prominent hex codes using k-means clustering."),
  sidebarLayout(
    sidebarPanel(
      fileInput("image", "Upload Image", accept = c("image/png", "image/jpeg","image/jpg")),
      actionButton("analyze", "Extract Colors"),
      hr(),
      textOutput("hex_codes"),
      hr(),
      textOutput("ggplot_string")
    ),
    mainPanel(
      plotOutput("image_with_colors"),
      plotOutput("color_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  uploaded_image <- reactive({
    req(input$image)
    magick::image_read(input$image$datapath)
  })
  
  # Extract colors
  color_data <- eventReactive(input$analyze, {
    req(uploaded_image())
    image <- uploaded_image()
    
    # Resize image to reduce the number of pixels for faster processing
    image_small <- image %>% magick::image_scale("100x100")
    
    # Convert image to a 3D array of RGB colors
    img_data <- magick::image_data(image_small, channels = "rgb")
    
    # Reshape the array to get each pixel as a row with R, G, B columns
    pixel_data <- data.frame(
      R = as.numeric(as.vector(img_data[1, , ])) / 255,
      G = as.numeric(as.vector(img_data[2, , ])) / 255,
      B = as.numeric(as.vector(img_data[3, , ])) / 255
    )
    
    # Create hex color codes for each pixel
    pixel_data <- pixel_data %>%
      mutate(color = rgb(R, G, B)) %>%
      filter(!(color %in% c("#FFFFFF", "#FDFDFD", "#FEFEFE", "#FCFCFC")))  # Filter out white or near-white colors
    
    # Perform k-means clustering to find distinct colors
    set.seed(42)  # For consistent results
    clusters <- kmeans(pixel_data[, c("R", "G", "B")], centers = 5)
    pixel_data$cluster <- clusters$cluster
    
    # Find the representative color for each cluster
    cluster_colors <- pixel_data %>%
      group_by(cluster) %>%
      summarize(
        R = mean(R),
        G = mean(G),
        B = mean(B),
        color = rgb(R, G, B)
      ) %>%
      arrange(desc(R + G + B)) %>%  # Sort to prioritize brighter colors
      pull(color)
    
    return(cluster_colors)
  })
  
  # Display image with color annotations
  output$image_with_colors <- renderPlot({
    req(uploaded_image())
    req(color_data())
    
    image <- uploaded_image()
    top_colors <- color_data()
    
    # Resize for display
    image_small <- image %>% magick::image_scale("500x500")
    img_grob <- rasterGrob(as.raster(image_small), interpolate = TRUE)
    
    # Plot image with color annotations
    grid.newpage()
    grid.draw(img_grob)
    grid.text(
      label = top_colors,
      x = seq(0.1, 0.9, length.out = length(top_colors)),
      y = 0.95,
      gp = gpar(col = top_colors, fontsize = 15, fontface = "bold")
    )
  })
  
  # Show extracted colors
  output$color_plot <- renderPlot({
    req(color_data())
    colors <- color_data()
    ggplot(data.frame(color = colors, count = 1), aes(x = color, y = count, fill = color, label = color)) +
      geom_bar(stat = "identity") +
      scale_fill_identity() +
      geom_label()+
      theme_void() +
      labs(title = "Top 5 Prominent and Distinct Colors", x = NULL, y = NULL) +
      theme(legend.position = "none")
  })
  
  # Display hex codes
  output$hex_codes <- renderText({
    req(color_data())
    paste("Hex Codes: ", paste(color_data(), collapse = ", "))
  })
  
  # Provide ggplot-compatible string
  output$ggplot_string <- renderText({
    req(color_data())
    paste0("\nggplot Colors: \n scale_fill_manual(c(", paste0('"', color_data(), '"', collapse = ", "), "))")
  })
}

# Run the app
shinyApp(ui = ui, server = server)

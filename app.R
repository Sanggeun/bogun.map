
library(shiny)

ui <- fluidPage(
   
   # Application title
   titlePanel("Mapping Health Indicators"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         fileInput("data", "데이터선택",buttonLabel = "데이터선택"),
         textInput("re_var","결과변수명", "c_rate"),
         selectInput("index_type", "지표유형1", choices=c("높을수록 좋음","낮을수록 좋음")),
         textInput("label","지표유형2(범례표시)", "조율(%)"),
         selectInput("color", "색상표시",choices =c("viridis","blue_red")),
         radioButtons("type", "지도유형", choices=c("sigungu","district","dong")),
         fileInput("map", "지도선택", buttonLabel = "지도선택"),
         actionButton("plotting","지도그리기"),
         textInput("fig","파일이름","fig-"),
         radioButtons("format", "파일유형",choices=c("PNG","JPG","PDF")),
         downloadButton("downloaddata","Download_images")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        DT::dataTableOutput("table"),
        plotOutput("map_2")
        
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    library(tidyverse)
    library(viridis)
    library(rgeos)
  
    data_to_map <- function(map, data, merge_key = "adm_dr_nm", map_key = "adm_dr_cd", data_key = "dong"){
    map@data$id <- rownames(map@data)
    map_f <- fortify(map, region = "id")
    
    merge_data <- merge(map@data[, c(map_key, merge_key)], data, by.x=merge_key, by.y = data_key)
    
    map@data <- left_join(map@data, merge_data, by = map_key)
    map_f_d <- left_join(map_f, map@data, by = "id")
    return(map_f_d)
    }
  
  
    plot_map <- function(map , re_var, index_type, 
                       path_color = "white", legend_label, 
                       color_type = "blue_red", color_good = "blue", color_bad = "red", fixed_mid = NULL) {
    
    map$re_var <- map[[re_var]]
    
    color <- c(color_bad, color_good)
    midpoint<- median(map$re_var)
    range_rate <- range(map$re_var)
    if (index_type != 1) {
      color <- c(color_good, color_bad)
    }
    if(!is.null(fixed_mid)) {
      midpoint <- fixed_mid
    }
    
    return (
      ggplot(map) +
        aes(long, lat, group = group, fill = re_var) +
        geom_polygon() +
        geom_path(color = path_color) +
        coord_equal() +
        labs(x="", y="", fill = legend_label) +
        scale_x_continuous(labels=NULL) +
        scale_y_continuous(labels = NULL) +
        theme_bw() +
        if (color_type == "viridis") {
          scale_fill_viridis(direction = index_type) 
        } else if (color_type == "blue_red") {
          scale_fill_gradient2(low = color[1], mid ="white", midpoint = midpoint,
                               high = color[2], limits = c(range_rate[1], range_rate[2]))
        }
      
    )
    
  }
  
  output$table <- DT::renderDataTable(DT::datatable({
    req(input$data)
    read.csv(input$data$datapath)
    }))
  
  
  
  output_map <- reactive({
    
      
    req(input$data)
    if(input$type == "dong") {
      merge_key <- "adm_dr_nm"
      map_key <- "adm_dr_cd"
      data_key <- "dong"
    } else if (input$type == "district") {
      merge_key <- "district"
      map_key <- "sigungu_cd"
      data_key <- "district"
    } else if (input$type == "sigungu") {
      merge_key <- "bogun_cd"
      map_key <- "bogun_cd"
      data_key <-"BOGUN_CD"
      
    }
    
    if(input$index_type == "높을수록 좋음") {
      index_type <- 1
    } else {
      index_type <- -1
    }
    
    data <- read.csv(input$data$datapath)
    map <- readRDS(input$map$datapath)
    
   
    data_map <- data_to_map(map, data, 
                               merge_key = merge_key, map_key = map_key, data_key = data_key)
    
    plot_map(data_map, re_var = input$re_var, index_type = index_type, 
             legend_label = input$label, color_type = input$color,
             path_color = "white")
   
    
  })
  output$map_2 <- renderPlot({
    input$plotting
    isolate(output_map())
    
  })
  output$downloaddata <- downloadHandler(
    filename = paste0(input$fig, ".", switch(
      input$format, PDF = "pdf", PNG = "png", JPG = "jpg"))
    ,
    content = function(file) {
      ggsave(file, output_map())
     
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


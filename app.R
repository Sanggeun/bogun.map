library(shiny)

need_pkg <- c("ggplot2","DT", "viridis", "rgeos", "maptools", "ggrepel")
has <- need_pkg %in% rownames(installed.packages())
if(any(!has)) install.packages(need_pkg[!has])

devtools::install_github("Sanggeun/map.choropleth")
library(map.choropleth)
library(ggplot2)
library(viridis)
library(DT)
library(rgeos)
library(maptools)

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
         textInput("data_key", "지역변수명", value = "dong"),
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
  
  output$table <- DT::renderDataTable(DT::datatable({
    req(input$data)
    read.csv(input$data$datapath, fileEncoding = "euc-kr")
    }))
  
  output_map <- reactive({
    req(input$data)
    if(input$type == "dong") {
      map_merge_key <- "adm_dr_nm"
      map_key <- "adm_dr_cd"
      data_key <- "dong"
    } else if (input$type == "district") {
      map_merge_key <- "district"
      map_key <- "district"
      data_key <- "district"
    } else if (input$type == "sigungu") {
      map_merge_key <- "bogun_cd"
      map_key <- "bogun_cd"
      data_key <-"BOGUN_CD"
      
    }
    
    if(input$index_type == "높을수록 좋음") {
      index_type <- 1
    } else {
      index_type <- -1
    }
    
    data <- read.csv(input$data$datapath, fileEncoding = "euc-kr", stringsAsFactors = FALSE)
    map <- readRDS(input$map$datapath)
    if (!all(Encoding(map[[map_merge_key]]) == "UTF-8")) {
    map[[map_merge_key]] <- iconv(map[[map_merge_key]], "euc-kr", "UTF-8")
    row.names(map@data) <- iconv(row.names(map@data), "euc-kr", "UTF-8")
    }
    
    data_map <- data_to_map(map, data, 
                               map_merge_key = map_merge_key, map_key = map_key, data_key = input$data_key)
    
    ## 테마 지정
    theme.ti <- element_text(family="NanumGothic", face="bold", size=12) #그래프 제목 스타일 변경
    theme.ax <- element_text(family="NanumGothic", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5) #그래프 축 이름 스타일 변경
    theme.leti<-element_text(family="NanumGothic", face="bold") #범례 제목 (legend) 스타일 변경
    theme.lete<-element_text(family="NanumGothic") #범례 항목 (legend) 스타일 변경
    
    plot_map(data_map, re_var = input$re_var, index_type = index_type, 
             legend_label = input$label, color_type = input$color,
             path_color = "white") +
      theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete)
   
    
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


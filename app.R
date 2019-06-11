library(shiny)

need_pkg <- c("devtools", "plyr", "ggplot2","DT", "viridis", "rgeos", "maptools", "ggrepel")
has <- need_pkg %in% rownames(installed.packages())
if(any(!has)) install.packages(need_pkg[!has])

devtools::install_github("Sanggeun/map.choropleth")
devtools::install_github("Sanggeun/g.function.bsg")
library(map.choropleth)
library(plyr)
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
         downloadButton("downloaddata","Download_images"),
         radioButtons("naming_map_type", "지역명 타입", choices =c("sigungu", "district", "dong")),
         fileInput("naming_map", "지역명지도", buttonLabel = "지역명지도"),
         actionButton("naming","지역명 넣기"),
         textInput("fig2","파일이름","fig-"),
         radioButtons("format2", "파일유형",choices=c("PNG","JPG","PDF")),
         downloadButton("downloaddata2","Download_images")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        DT::dataTableOutput("table"),
        plotOutput("map_1"),
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
  
  output_data <- reactive({
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
    
    
    
    data <- read.csv(input$data$datapath, fileEncoding = "euc-kr", stringsAsFactors = FALSE)
    map <- readRDS(input$map$datapath)
    
    data_map <- data_to_map(map, data, 
                               map_merge_key = map_merge_key, map_key = map_key, data_key = input$data_key)
    return(data_map)
  })
  
  output_map <- reactive({
    if(input$index_type == "높을수록 좋음") {
      index_type <- 1
    } else {
      index_type <- -1
    }
    
    ## 테마 지정
    theme.ti <- element_text(family="NanumGothic", face="bold", size=12) #그래프 제목 스타일 변경
    theme.ax <- element_text(family="NanumGothic", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5) #그래프 축 이름 스타일 변경
    theme.leti<-element_text(family="NanumGothic", face="bold") #범례 제목 (legend) 스타일 변경
    theme.lete<-element_text(family="NanumGothic") #범례 항목 (legend) 스타일 변경
    
    plot_map(output_data(), re_var = input$re_var, index_type = index_type, 
             legend_label = input$label, color_type = input$color,
             path_color = "white") +
      theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete)
  })
  
  output_map_2 <- reactive({
    map_name <- readRDS(input$naming_map$datapath)
    if (input$naming_map_type == "dong") {
      region_name <- name_in_map(map_name, name_var = "adm_dr_nm")
    } else if (input$naming_map_type == "district") {
      region_name <- name_in_map(map_name, name_var = "district")
    } else if (input$naming_map_type == "sigungu") {
      region_name <- name_in_map(map_name, name = c("남구","달서구","달성군","동구","북구","서구","수성구","중구"))
    }
    
    if(input$index_type == "높을수록 좋음") {
      index_type <- 1
    } else {
      index_type <- -1
    }
    
    ## 테마 지정
    theme.ti <- element_text(family="NanumGothic", face="bold", size=12) #그래프 제목 스타일 변경
    theme.ax <- element_text(family="NanumGothic", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5) #그래프 축 이름 스타일 변경
    theme.leti<-element_text(family="NanumGothic", face="bold") #범례 제목 (legend) 스타일 변경
    theme.lete<-element_text(family="NanumGothic") #범례 항목 (legend) 스타일 변경
    
    plot_map(output_data(), re_var = input$re_var, index_type = index_type, 
             legend_label = input$label, color_type = input$color,
             path_color = "white") +
      ggrepel::geom_label_repel(data = region_name, aes(x=clong, y=clat, group = NULL, fill = NULL, label = name), family = "NanumGothic") + 
      theme(axis.title = theme.ax, plot.title = theme.ti, legend.title = theme.leti, legend.text = theme.lete)
    
    
  })
  
  output$map_1 <- renderPlot({
    input$plotting
    isolate(output_data())
    isolate(output_map())
  })
  
  output$map_2 <- renderPlot({
    input$naming
    isolate(output_map_2())
    
  })
  
  output$downloaddata <- downloadHandler(
    filename = paste0(input$fig, ".", switch(
      input$format, PDF = "pdf", PNG = "png", JPG = "jpg"))
    ,
    content = function(file) {
      ggsave(file, output_map())
     
    }
  )
  output$downloaddata2 <- downloadHandler(
    filename = paste0(input$fig2, ".", switch(
      input$format2, PDF = "pdf", PNG = "png", JPG = "jpg"))
    ,
    content = function(file) {
      ggsave(file, output_map_2())
      
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


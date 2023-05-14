library(shiny)
library(dplyr)
library(ggplot2)
library(stringi)
library(DT)
library(plotly)
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)
library(viridis)

options(encoding = 'UTF-8')

jscode_upload_msg <- " Shiny.addCustomMessageHandler('upload_msg', function(msg) {
  var target = $('#fileUpload_progress').children()[0];
  target.innerHTML = msg;
}); "

ui <- fluidPage(lang = "es",
  tags$script(HTML(jscode_upload_msg)),
  titlePanel(title = span(img(src = "logo_vitatracer.JPG", height = 50)),windowTitle = "VitaTracer"),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h5("Explore evolution of your lab results (like blood and urine) over time, and detect which values are outside of the reference."),
      h5("Use the example data or load your own file following the format of ", a("this template", href="https://www.linkedin.com/in/andreslanzos", .noWS = "after"), "."),
      radioButtons("select_dataset",label="Select a choice:", choices=c("Example data","Upload data"),inline = F, selected="Example data"),
      conditionalPanel("input.select_dataset=='Upload data'",
                       fileInput(inputId = "fileUpload",
                                  label = "Upload data",
                                  multiple = FALSE,
                                  buttonLabel = "Upload",
                                  placeholder = "No data uploaded",
                                  accept = c(".xls")),                                     
                       uiOutput('ui.action') ),
      h5("Contact: ", a("Andrés Lanzós.", href="https://www.linkedin.com/in/andreslanzos"))
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Overview",
                           plotOutput("plot_overview", height = 1000, width = 1200)
                  ),
                  tabPanel("Individual values",
                           radioButtons(inputId = "individual_analyte",
                                        label = "Analyte:",
                                        choices = c("Blood - Hemogram white - Leucocytes (10^3/μl)")),
                           plotOutput("plot_individual", height = 600)
                  )
      )
    )
  )
)

server <- function(input, output,session) {
  
  observeEvent(input$fileUpload, {
    session$sendCustomMessage("upload_msg", "Uploading data")
  })
  
  d <- reactive({
    
   if (input$select_dataset=="Example data") {
      file=readxl::read_xlsx("data/summary.xlsx",1)
    } else {
      validate(
        need(!is.null(input$fileUpload), "Error: please upload your data or choose the example one.")
      )
      file=input$fileUpload
      file=readxl::read_xlsx(as.character(file$datapath),1)
    }
    file <- file %>% 
      dplyr::arrange(Sample, Analyte, Unit)  %>% 
      dplyr::mutate(
        Analyte = paste0(Sample, " - ", Category, " - ", Analyte, " (", Unit, ")"),
        Min = as.numeric(Min),
        Measure = as.numeric(Measure),
        Max = as.numeric(Max),
        Date = as.Date.character(Date),
        Date = as.character(Date),
        Color = ifelse(Measure < Min, "Low",
                       ifelse(Measure > Max, "High",
                              "Good"))
      )
    
    # file$Emprendedora=stri_trans_totitle(file$Emprendedora)
    file
  })
  
  
  # d2 <- reactive({
  #   variable_x=gsub("País","Pais",gsub(" ","_",input$variable_x))
  #   variable_x
  # })
  d3 <- reactive({
    individual_analyte=input$individual_analyte
    individual_analyte
  })
  
  
  
  output$plot_overview <- renderPlot({
    file=d()
    table_for_heatmap <- file %>%
      dplyr::arrange(Date, Analyte) %>%
      tidyr::pivot_wider(id_cols = Analyte, names_from = Date, values_from = Color) %>%
      dplyr::arrange(Analyte) %>%
      tibble::column_to_rownames("Analyte") %>%
      as.matrix()
    
    names_rows <- row.names(table_for_heatmap)
    
    Sample <- sapply(names_rows, function(x) {
      strsplit(x, " - ")[[1]][[1]]
    })
    Sample_colors <- viridis::viridis(length(unique(Sample)))
    names(Sample_colors) <- unique(sort(Sample))
    
    Category <- sapply(names_rows, function(x) {
      strsplit(x, " - ")[[1]][[2]]
    })
    Category_colors <- viridis::viridis(length(unique(Category)))
    names(Category_colors) <- unique(sort(Category))
    
    
    
    color_scale <- c("Green", "Red", "Orange", "White")
    
    row_ha <- rowAnnotation(
      "Sample" = Sample,
      "Category" = Category,
      col = list(
        "Sample" = Sample_colors,
        "Category" = Category_colors
      ),
      # annotation_name_side = "top",
      annotation_name_rot = 45,
      annotation_legend_param = list(Sample = list(title_gp = gpar(fontsize = 18, fontface = "bold"), 
                                                   labels_gp = gpar(fontsize = 14)),
                                     Category = list(title_gp = gpar(fontsize = 18, fontface = "bold"), 
                                                   labels_gp = gpar(fontsize = 14)))
    )
    
    
    ht <- ComplexHeatmap::Heatmap(table_for_heatmap,
                                  name = "Value",
                                  col = color_scale,
                                  cluster_rows = F,
                                  cluster_columns = F,
                                  na_col = "White",
                                  row_title = "",
                                  row_title_side = "left",
                                  row_names_side = "left",
                                  row_names_max_width = max_text_width(
                                    rownames(table_for_heatmap)
                                  ),
                                  column_title = "Date",
                                  column_title_side = "bottom",
                                  column_names_side = "bottom",
                                  left_annotation = row_ha,
                                  column_names_rot = 45, 
                                  # width = unit(15, "cm"),
                                  heatmap_legend_param = list(title_gp = gpar (fontsize = 18, fontface = "bold"),
                                                              labels_gp = gpar(fontsize = 14))
    )
    
    ht <- draw(ht, merge_legends = TRUE)
    
  })
  
  output$plot_individual <- renderPlot({
    individual_analyte = d3()
    file <- d() %>% 
      dplyr::filter(Analyte == individual_analyte) %>% 
      dplyr::mutate(Color2 = c(Color[2:length(Color)],Color[1]))
    print(nrow(file))
    ggplot(data = file,
           mapping = aes(
             x = Date,
             y = Measure,
           )) +
      ylab(individual_analyte) +
      geom_ribbon(data = file, mapping = aes(x=Date, y=Measure, ymax=Max, ymin=Min, group = 1), fill = "green", alpha = 0.2) +
      geom_line(mapping = aes(group = 1, color = Color2)) +
      geom_point(mapping = aes(color = Color), size = 5)
  })
}

shinyApp(ui = ui, server = server)
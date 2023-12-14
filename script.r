library(BiocManager)
options(repos = BiocManager::repositories())
library(shiny)
library(dplyr)
library(ggplot2)
library(BiocGenerics)
library(stringi)
library(DT)
library(plotly)
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)
library(viridis)
library(sortable)
library(formattable)
library(tidyr)
library(readxl)
library(tibble)
library(ComplexHeatmap)
library(emojifont)

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
      h5("Explore the evolution of your lab results (like blood and urine) over time, and detect which values are outside of the reference."),
      br(),
      h5("Use the example data or load your own file following the format of ", a("this template", href="https://www.dropbox.com/scl/fi/cn235zcg1jmqgm46eau5h/example_file_for_VitaTracer.xlsx?rlkey=wiu42n6jn9y4zsiscte1n4vvs&dl=0", target = "_blank", .noWS = "after"), "."),
      radioButtons("select_dataset",label="Select a choice:", choices=c("Example data","Upload data"),inline = F, selected="Example data"),
      conditionalPanel("input.select_dataset=='Upload data'",
                       fileInput(inputId = "fileUpload",
                                  label = "Upload data",
                                  multiple = FALSE,
                                  buttonLabel = "Upload",
                                  placeholder = "No data uploaded",
                                  accept = c(".xls")),                                     
                       uiOutput('ui.action') ),
      br(),
      h5("Select the minimum number of times that an analyte must have been measured to be visible on the graphics."),
      numericInput("min_measurement", label = "Min. measurements", value = 1),
      br(),
      h5(tags$b("Sample legend:")),
      h5("🩸 = Blood sample."),
      h5("🚽 = Urine sample."),
      h5("💩 = Stool sample."),
      br(),
      h5(tags$b("Color legend:")),
      h5("✅ values (in green) that are within the reference values reported by the lab where the analysis was done."),
      h5("🔻 values (in orange) that are below the reference values reported by the lab where the analysis was done."),
      h5("🔺 values (in red) that are above the reference values reported by the lab where the analysis was done."),
      h5("Since each lab uses different reference values, the same value might be \"✅\" for one date but not for another 
         date (like it happens with GGT)."),
      br(),
      h5("Contact: ", a("Andrés Lanzós.", href="https://www.linkedin.com/in/andreslanzos", target = "_blank"))
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summary",
                           plotlyOutput("plot_summary", height = 600)
                  ),
                  tabPanel("Table",
                           DT::dataTableOutput("table_overview")
                           # div(DT::dataTableOutput("table_overview"), style = "font-size:100%")
                  ),
                  tabPanel("Overview",
                           br(),
                           # selectInput("sorting_variable",
                           #                label="Sort the plot below by:",
                           #                choices=c("Sample", "Category", "Analyte", "Unit")),
                           rank_list(
                             text = list(p("IMPORTANT!", style = "color:red", .noWS = "after"), "Drag and drop the items below (from first to last) and the plot will be sorted accordingly:"),
                             labels = list(
                               "Sample",
                               "Category",
                               "Analyte",
                               "Unit"
                             ),
                             input_id = "sorting_variable"
                           ),
                           plotOutput("plot_overview", height = 1000, width = 1200)
                  ),
                  tabPanel("Individual values",
                           br(),
                           selectizeInput("individual_analyte",
                                          label="Search and select an analyte to display:",
                                          choices=NULL),
                           plotOutput("plot_individual", height = 600)
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  
  lighten_colors <- function(colors, factor = 0.5) {
    lighten_color <- function(rgb_color) {
      lightened_rgb <- (1 - factor) * rgb_color + factor * 255
      return(round(lightened_rgb))
    }
    
    lightened_colors <- sapply(colors, function(hex_color) {
      rgb_color <- col2rgb(hex_color)
      lightened_rgb <- lighten_color(rgb_color)
      lightened_hex <- rgb(lightened_rgb[1], lightened_rgb[2], lightened_rgb[3], maxColorValue = 255)
      return(lightened_hex)
    })
    
    return(lightened_colors)
  }
  
  
  color_scale <- c("#CD0000", "#3e9e1f", "#ff3300", "#ffffff")
  names(color_scale) <- c("High", "Good", "Low", NA)

  emoji_scale <- c("#CD0000", "#3e9e1f", "#ff3300", "#ffffff")
  names(emoji_scale) <- c("🔺", "✅", "🔻", NA)
  
  # color_scale <- c("#CD0000", "#3e9e1f", "#ff3300", "#ffffff")
  # names(color_scale) <- c("🔺", "✅", "🔻", NA)
  
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
        Sample_Emoji = ifelse(Sample=="Blood", "🩸",
                        ifelse(Sample=="Urine", "🚽",
                               "💩")),
        Sample_Emoji = factor(Sample_Emoji, levels = c("🩸", "🚽", "💩", NA)),
        # Analyte = paste0(Sample, " - ", Category, " - ", Analyte, " (", Unit, ")"),
        Full_Name = paste0(Sample, " - ", Category, " - ", Analyte, " - ", Unit),
        Min = round(as.numeric(Min), digits=2),
        Measure = round(as.numeric(Measure), digits=2),
        Max = round(as.numeric(Max), digits=2),
        Date = as.Date.character(Date),
        Date = as.character(Date),
        Date = gsub("-", "/", Date),
        # Date = substr(Date, 3, 10),
        Color = ifelse(Measure < Min, "Low",
                       ifelse(Measure > Max, "High",
                              "Good")),
        Color = factor(Color, levels = c("High", "Good", "Low", NA)),
        Emoji = ifelse(Measure < Min, "🔻",
                       ifelse(Measure > Max, "🔺",
                              "✅")),
        Emoji = factor(Emoji, levels = c("🔺", "✅", "🔻", NA)),
      ) %>% 
      dplyr::group_by(Full_Name) %>% 
      dplyr::filter(
        n()>=input$min_measurement
      )
    
    list_names_for_search <- unique(sort(unlist(file$Full_Name)))
    list_names_for_search <- list_names_for_search[list_names_for_search!=""]
    # list_names_for_search <- list_names_for_search[grepl("\\D", list_names_for_search)]
    updateSelectizeInput(session, "individual_analyte",
                         label = NULL,
                         selected="Blood - Hemogram white - Leucocytes - 10^3/μl",
                         # selected="Urine - Urine - Nitrites (NA)",
                         choices = list_names_for_search,
                         server = TRUE)
    print("done")
    
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
    sorting_variable = unlist(input$sorting_variable)
    table_for_heatmap <- file %>%
      dplyr::arrange(Date, Analyte) %>%
      tidyr::pivot_wider(id_cols = c(Full_Name, Analyte, Sample, Unit, Category), names_from = Date, values_from = Color) %>%
      # dplyr::arrange(get(sorting_variable), Sample, Analyte, Category, Unit) %>%
      # dplyr::arrange(Sample, Analyte, Category, Unit) %>%
      dplyr::arrange(across(sorting_variable)) %>%
      tibble::column_to_rownames("Full_Name") %>%
      dplyr::select(-c(Analyte, Sample, Unit, Category)) %>% 
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
    
    Analyte <- sapply(names_rows, function(x) {
      strsplit(x, " - ")[[1]][[3]]
    })
    
    Unit <- sapply(names_rows, function(x) {
      strsplit(x, " - ")[[1]][[4]]
    })
    Unit_colors <- viridis::viridis(length(unique(Unit)))
    names(Unit_colors) <- unique(sort(Unit))
    
    
    
    

    
    # row_ha <- rowAnnotation(
    #   "Sample" = Sample,
    #   "Category" = Category,
    #   col = list(
    #     "Sample" = Sample_colors,
    #     "Category" = Category_colors
    #   ),
    #   # annotation_name_side = "top",
    #   annotation_name_rot = 45,
    #   annotation_legend_param = list(Sample = list(title_gp = gpar(fontsize = 18, fontface = "bold"), 
    #                                                labels_gp = gpar(fontsize = 14)),
    #                                  Category = list(title_gp = gpar(fontsize = 18, fontface = "bold"), 
    #                                                labels_gp = gpar(fontsize = 14)))
    # )
    
    row_ha <- rowAnnotation(
      # "Sample" = Sample,
      # "Category" = Category,
      "Sample" = anno_text(show_name = T,
                           x = Sample, 
                           location = 0.05, 
                           just = "left",
                           gp = gpar(fill = alpha(colour = Sample_colors[Sample],alpha =  0.5), fontsize = 9,
                                     col = "black", border = NA),
                           width = max_text_width(Sample)*1.1),
      "Category" = anno_text(show_name = T,
                             x = Category, 
                             location = 0.05, 
                             just = "left",
                             gp = gpar(fill = alpha(colour = Category_colors[Category],alpha =  0.5), fontsize = 9,
                                       col = "black", border = NA),
                             width = max_text_width(Category)*1.1),
      "Analyte" = anno_text(show_name = T,
                             x = Analyte, 
                             location = 0.05, 
                             just = "left",
                             gp = gpar(fill = "White", fontsize = 9,
                                       col = "black", border = NA),
                             width = max_text_width(Analyte)*1.1),
      "Unit" = anno_text(show_name = T,
                             x = Unit, 
                             location = 0.05, 
                             just = "left",
                             gp = gpar(fill = alpha(colour = Unit_colors[Unit],alpha =  0.5), fontsize = 9,
                                       col = "black", border = NA),
                             width = max_text_width(Unit)*1.1),
      # col = list(
      #   "Sample" = Sample_colors,
      #   "Category" = Category_colors
      # ),
      annotation_name_side = "top",
      # annotation_legend_param = list(
      #   Sample = list(title_gp = gpar(fontsize = 18, fontface = "bold"),
      #                 labels_gp = gpar(fontsize = 14)),
      #   Category = list(title_gp = gpar(fontsize = 18, fontface = "bold"), 
      #                   labels_gp = gpar(fontsize = 14))),
      annotation_name_rot = 0
    )
    
    
    ht <- ComplexHeatmap::Heatmap(matrix = table_for_heatmap,
                                  name = "Value",
                                  row_dend_reorder = F,
                                  col = color_scale,
                                  cluster_rows = F,
                                  cluster_columns = F,
                                  na_col = "White",
                                  row_title = "",
                                  show_row_names = F,
                                  row_title_side = "left",
                                  row_names_side = "left",
                                  row_names_max_width = max_text_width(
                                    rownames(table_for_heatmap)
                                  ),
                                  column_names_gp = grid::gpar(fontsize = 8),
                                  row_names_gp = grid::gpar(fontsize = 8),
                                  column_title = "Date",
                                  column_title_side = "top",
                                  column_names_side = "top",
                                  left_annotation = row_ha,
                                  column_names_rot = 45, 
                                  # width = unit(15, "cm"),
                                  heatmap_legend_param = list(title_gp = gpar (fontsize = 12, fontface = "bold"),
                                                              labels_gp = gpar(fontsize = 10))
    )
    
    ht <- draw(ht, merge_legends = TRUE)
    
  })
  
  output$table_overview <- DT::renderDataTable({
    
    file=d()
    table_for_heatmap0 <- file %>%
      dplyr::arrange(Date, Analyte, Sample, Category, Unit) %>%
      dplyr::mutate(Color = factor(Color),
                    Emoji = factor(Emoji),
                    Analyte = factor(Analyte, levels = sort(unique(as.character(Analyte)))), 
                    Unit = factor(Unit, levels = sort(unique(as.character(Unit)))),
                    Sample = factor(Sample, levels = sort(unique(as.character(Sample)))),
                    Sample = Sample_Emoji,
                    Measure_Emoji = paste0(Emoji, " ", Measure , ""),
                    Category = factor(Category, levels = sort(unique(as.character(Category)))))
    table_for_heatmap <- table_for_heatmap0 %>%
      tidyr::pivot_wider(id_cols = c(Full_Name, Sample, Category, Analyte, Unit),
                         names_from = Date, values_from = Measure_Emoji) %>%
      dplyr::as_data_frame() %>% 
      dplyr::mutate(Analyte = factor(Analyte, levels = sort(unique(as.character(Analyte)))),
                    # Sample = factor(Sample, levels = sort(unique(as.character(Sample)))), 
                    Unit = factor(Unit, levels = sort(unique(as.character(Unit))))) %>% 
      dplyr::arrange(Analyte, Sample, Unit, Category) %>%
      tibble::column_to_rownames("Full_Name")
    
    brks <- c(sort(unique(as.character(table_for_heatmap0$Measure_Emoji))), NA)
    clrs <- sort(unique(as.character(table_for_heatmap0$Measure_Emoji)))
    clrs[grepl("🔺", clrs)] <- "#CD0000"
    clrs[grepl("✅", clrs)] <- "#3e9e1f"
    clrs[grepl("🔻", clrs)] <- "#ff3300"
    clrs <- c(clrs, "#ffffff")
    names(clrs) <- brks
    
    clrs <- lighten_colors(clrs)
    
    DT::datatable(table_for_heatmap,
                  # filter = 'none',
                  extensions = c('Buttons', 'ColReorder', 'FixedHeader', 'Scroller', 'FixedColumns'),
                  # selection = 'none',
                  options = list(
                              #     dom = 'tpB',
                              #     buttons = list(
                              #       list(
                              #         extend = "collection",
                              #         text = 'Show all rows',
                              #         action = DT::JS("function ( e, dt, node, config ) {
                              # dt.page.len(-1);
                              # dt.ajax.reload();}")
                              #       ),list(
                              #         extend = "collection",
                              #         text = 'Show 5 rows',
                              #         action = DT::JS("function ( e, dt, node, config ) {
                              # dt.page.len(10);
                              # dt.ajax.reload();}")
                              #         
                              #       )
                              #     ),
                    
                                 lengthMenu = list(c(10, 50, -1), c('10', "50", 'All')),
                                 pageLength  = -1,
                                 scrollX = TRUE,
                                 # colReorder = TRUE, 
                                 fixedHeader = TRUE,
                                 scrollY = 600,
                                 # scroller = TRUE,
                                 fixedColumns = list(leftColumns = 4),
                                 # dom = 'Bfrtip',
                                 # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                 autoWidth = TRUE),
                  rownames = FALSE) %>%
      formatStyle(colnames(table_for_heatmap)[!colnames(table_for_heatmap) %in% c("Sample",
                                                                                  "Analyte",
                                                                                  "Unit",
                                                                                  "Category")],
                  backgroundColor = styleEqual(brks, clrs))
    
    }#, 
    # extensions = 'Buttons',
    # options = list(scrollX = TRUE,
    #                # dom = 'Bfrtip',
    #                # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    #                autoWidth = TRUE
    #                ),
    # filter = 'top',
    # rownames = FALSE
    )
  
  output$plot_individual <- renderPlot({
    individual_analyte = d3()
    # file <- d() %>% 
    #   dplyr::filter(Analyte == individual_analyte) %>% 
    #   dplyr::arrange(Date) %>% 
    #   dplyr::mutate(Color2 = c(Color[2:length(Color)],Color[1]))
    file <- d() %>% 
      dplyr::filter(Full_Name == individual_analyte) %>% 
      dplyr::arrange(Date)
    if(nrow(file)>1) {
      file$Color2 <- c(file$Color[2:length(file$Color)],file$Color[1])
    } else {
      file$Color2 <- file$Color
    }
    
    if(nrow(file)>1) {
      ggplot(data = file,
             mapping = aes(
               x = Date,
               y = Measure,
             )) +
        ylab(individual_analyte) +
        geom_ribbon(data = file, mapping = aes(x=Date, y=Measure,
                                               ymax=Max+0.001, ymin=Min-0.001,
                                               group = 1), fill = "#3e9e1f", alpha = 0.1) +
        geom_line(mapping = aes(group = 1, color = Color2), size = 2) +
        geom_point(mapping = aes(color = Color), size = 5) +
        scale_color_manual(values = color_scale, name = "Value") +
        theme_classic() +
        theme(
          axis.text.y = element_text(size = 14, color = "black", face = "plain"),
          axis.text.x = element_text(size = 14, color = "black", face = "plain", angle = 45, vjust = 1, hjust = 1),
          axis.title = element_text(size = 14, color = "black", face = "plain"),
          axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
          panel.grid = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 14, color = "black", face = "plain"),
          legend.title = element_text(size = 14, color = "black", face = "plain")
        )
    } else {
      ggplot() +
        ylab(individual_analyte) +
        geom_point(data = file,
                   mapping = aes(
                     x = Date,
                     y = Measure,
                     color = Color
                   ), size = 0) +
        geom_rect(data = file, mapping = aes(xmin = 0.75,
                                             xmax = 1.25,
                                             ymin = Min-0.001,
                                             ymax = Max+0.001),
                  fill = "#3e9e1f", alpha = 0.1) +
        geom_point(data = file,
                   mapping = aes(
                     x = Date,
                     y = Measure,
                     color = Color
                   ), size = 5) +
        scale_color_manual(values = color_scale, name = "Value") +
        theme_classic() +
        theme(
          axis.text = element_text(size = 14, color = "black", face = "plain"),
          axis.title = element_text(size = 14, color = "black", face = "plain"),
          axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
          panel.grid = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 14, color = "black", face = "plain"),
          legend.title = element_text(size = 14, color = "black", face = "plain")
        )
    }
  })
  
  output$plot_summary <- renderPlotly({
    
    file=d()
    selected_dates <- sort(unique(file$Date))
    selected_dates <- tail(selected_dates,3)
    
    table_for_heatmap0 <- file%>%
      as.data.frame() %>% 
      dplyr::arrange(Date, Analyte, Sample, Category, Unit) %>%
      # dplyr::filter(Date %in% selected_dates) %>% 
      dplyr::mutate(`Value in range` = Color,
                    Emoji = factor(Emoji),
                    Analyte = factor(Analyte, levels = sort(unique(as.character(Analyte)))), 
                    Unit = factor(Unit, levels = sort(unique(as.character(Unit)))),
                    Sample = factor(Sample, levels = sort(unique(as.character(Sample)))),
                    Measure_Emoji = paste0(Emoji, " ", Measure , ""),
                    Category = factor(Category, levels = sort(unique(as.character(Category))))) %>% 
      dplyr::group_by(Date, `Value in range`) %>% 
      dplyr::summarise(`Number of analytes` = n()) %>% 
      dplyr::ungroup() %>% 
      as.data.frame()
    
    p <- ggplot(table_for_heatmap0, aes(y = Date, x = `Number of analytes`,
                                        fill = `Value in range`,
                                        text = paste('Date: ', Date,
                                                     '</br></br> Value in range: ', `Value in range`,
                                                     '</br> Number of analytes: ', `Number of analytes`
                                                     ))) +
      scale_fill_manual(values = color_scale) +
      geom_bar(stat = "identity", position = "stack") +
      ylab("")+
      theme_classic() +
      theme(
        axis.text = element_text(size = 14, color = "black", face = "plain"),
        axis.title = element_text(size = 14, color = "black", face = "plain"),
        axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 14, color = "black", face = "plain"),
        legend.title = element_text(size = 14, color = "black", face = "plain")
      )
    ggplotly(p, tooltip = c("text")) %>%
      plotly::layout(legend=list(x=0, y = 1, 
                                 xanchor='left',
                                 yanchor='bottom',
                                 orientation='h')) 
    
    # p <- ggplot(table_for_heatmap0, aes(x="", y=Number, fill=Color )) +
    #   geom_bar(stat="identity", width=1) +
    #   coord_polar("y", start=0) + 
    #   theme_void() +
    #   theme(legend.position="bottom") +
    #   scale_fill_manual(values = color_scale, name = "Value") + 
    #   # theme(plot.margin = unit(c(1,1,1,1), "cm")) +
    #   facet_wrap("Date")
    
    # fig <- table_for_heatmap0 %>% plot_ly(labels = ~Color, values = ~Number, 
    #                                       textinfo='label+percent+value',
    #                                       marker = list(colors = ~color_scale[Color]))
    # fig <- fig %>% add_pie(hole = 0.6)
    # fig <- fig %>% layout(showlegend = F,
    #                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    # fig
    
    # fig <- plot_ly()
    # fig <- fig %>%
    #   add_pie(data = table_for_heatmap0 %>% dplyr::filter(Date==selected_dates[1]),
    #                        labels = ~Color,
    #                        values = ~Number,
    #                        textinfo='label+percent+value',
    #                        marker = list(colors = ~color_scale[Color]),
    #                        title = selected_dates[1],
    #                        name = selected_dates[1],
    #                        domain = list(row = 0, column = 0)) %>%
    #   add_pie(data = table_for_heatmap0 %>% dplyr::filter(Date==selected_dates[2]),
    #           labels = ~Color,
    #           values = ~Number,
    #           textinfo='label+percent+value',
    #           marker = list(colors = ~color_scale[Color]),
    #           title = selected_dates[2],
    #           name = selected_dates[2],
    #           domain = list(row = 0, column = 1))
    # fig <- fig %>% layout(title = "Pie Charts with Subplots", showlegend = F,
    #                       grid=list(rows=1, columns=2 ),
    #                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
      
  })
  
  
}

shinyApp(ui = ui, server = server)
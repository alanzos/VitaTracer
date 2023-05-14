library(shiny)
library(dplyr)
library(ggplot2)
library(stringi)
library(DT)
library(plotly)

options(encoding = 'UTF-8')

jscode_upload_msg <- " Shiny.addCustomMessageHandler('upload_msg', function(msg) {
  var target = $('#fileUpload_progress').children()[0];
  target.innerHTML = msg;
}); "

ui <- fluidPage(lang = "es",
  tags$script(HTML(jscode_upload_msg)),
  titlePanel(title = span(img(src = "logo.png", height = 50)),windowTitle = "MICROWD Diversificación"),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h5("Inspecciona la diversificación de tus inversiones en MICROWD según varios factores y métricas."),
      h5("Usa el excel de ejemplo o carga el tuyo, que puedes descargar en la pestaña de \"Inversiones\" de la sección \"Perfil\" dentro de MICROWD."),
      # br(),
      radioButtons("select_dataset",label="Seleccionar excel", choices=c("Excel de ejemplo","Cargar excel"),inline = F, selected="Excel de ejemplo"),
      conditionalPanel("input.select_dataset=='Cargar excel'",
                       fileInput(inputId = "fileUpload",
                                  label = "Cargar excel .xls",
                                  multiple = FALSE,
                                  buttonLabel = "Cargar",
                                  placeholder = "Ningun excel cargado",
                                  accept = c(".xls")),                                     
                       uiOutput('ui.action') ),
      radioButtons(inputId = "variable_x",
                   label = "Factor:",
                   choices = sort(c("Emprendedora","Equipo local","Moneda local","Riesgo de divisa","Periodicidad de pago","Municipio","País","Sector del negocio","Status","Tipo de negocio"))),
      radioButtons(inputId = "variable_y",
                   label = "Métrica:",
                   choices = c("Número de inversiones","Cantidad invertida €")),
      h5("Esta applicación web no ha sido desarrollada por MICROWD, sino que es un proyecto independiente. Autor: ", a("Andrés Lanzós.", href="https://www.linkedin.com/in/andreslanzos"))
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Grafico",
                           plotlyOutput("plot", height = 600)
                  ),
                  tabPanel("Tabla", 
                           DT::dataTableOutput(outputId = "table"),
                           downloadButton("downloadtable", "Descargar tabla")
                  )
      )
    )
  )
)

server <- function(input, output,session) {
  
  observeEvent(input$fileUpload, {
    session$sendCustomMessage("upload_msg", "Cargando excel")
  })
  
  d <- reactive({
    
    variable_x=gsub("País","Pais",gsub(" ","_",input$variable_x))
    variable_y=input$variable_y
    if (input$select_dataset=="Excel de ejemplo") {file=readxl::read_xls("data/inversiones_en_curso.xls",1)
    } else {
      validate(
        need(!is.null(input$fileUpload), "Carga un excel con tus inversiones o selecciona el excel de ejemplo")
      )
      file=input$fileUpload
      file=readxl::read_xls(as.character(file$datapath),1)
    }
    names(file)=c("Emprendedora","Pais","Municipio","Equipo_local","Sector_del_negocio","Tipo_de_negocio","Status","Fecha_de_inversion","Fecha_de_cheque","Inversion","Dinero_nuevo","Reinversion","Tipo_de_cambio_inversion","Cantidad_recuperada_Moneda_Local","Tipo_de_cambio_ultimo","Cantidad_recuperada","Fecha_ultimo_pago","Moneda_local","Riesgo_de_divisa","Periodicidad_de_pago")
    file$Inversion=as.numeric(file$Inversion)
    file$Emprendedora=stri_trans_totitle(file$Emprendedora)
    file$Municipio=stri_trans_totitle(file$Municipio)
    table_to_plot = file %>%
      dplyr::group_by_at(variable_x) %>%
      dplyr::summarise(Frequency=length(Inversion),
                       Quantity=sum(Inversion))
    if (variable_y=="Cantidad invertida €") {variable_y2="Quantity"} else {variable_y2="Frequency"}
    table_to_plot[,c(variable_x,variable_y2)]
  })
  
  
  d2 <- reactive({
    variable_x=gsub("País","Pais",gsub(" ","_",input$variable_x))
    variable_x
  })
  d3 <- reactive({
    variable_y=input$variable_y
    variable_y
  })
  
  
  
  output$plot <- renderPlotly({
    table_to_plot=d()
    variable_x=d2()
    variable_y=d3()
    if (variable_y=="Cantidad invertida €") {variable_y2="Quantity"} else {variable_y2="Frequency"}
    my_gg=ggplot(data = table_to_plot, aes(x=get(variable_x), y=get(variable_y2))) +
      geom_col()  +
      theme_classic() +
      theme(axis.text.y = element_text(color="black"),
            axis.text.x = element_text(color="black")) +
      xlab(gsub("Pais","País",gsub("_"," ",variable_x))) +
      ylab(variable_y) +
      coord_flip()
    ggplotly(my_gg,tooltip = c("text"))
  })

  
  output$table <- DT::renderDataTable({
    file=d()
    colnames(file)=c(gsub("Pais","País",gsub("_"," ",d2())),d3())
    file
  },
  rownames = FALSE,
  options = list(
    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
    search = list(regex = TRUE)))

  
  output$downloadtable <- downloadHandler(
    filename = function() {
      paste('tabla_resumen_de_inversiones', '.csv', sep='')
    },
    content = function(file) {
      summary_data_temp=d()
      colnames(summary_data_temp)=c(gsub("Pais","País",gsub("_"," ",d2())),d3())
      summary_data_temp
      write.csv(summary_data_temp, file,row.names = F)
    }
  )
    
}

shinyApp(ui = ui, server = server)
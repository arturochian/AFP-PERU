library(shiny)
library(readxl)
library(scales)
library(ggplot2)
library(dplyr)

# Cargar y preparar los datos
# Se asume que el archivo .xlsx está en el mismo directorio que app.R
rentabilidad <- readxl::read_xlsx("B-220932-se2025.xlsx", skip = 3, col_types = c("date", rep("numeric", 16)))

# Renombrar columnas
colnames(rentabilidad)[1] <- "Fecha"
afps <- c("Habitat", "Integra", "Prima", "Profuturo")
fondos <- 0:3
nombres_columnas <- as.vector(sapply(fondos, function(f) paste(afps, "Fondo", f, sep = "_")))
colnames(rentabilidad)[2:17] <- nombres_columnas

# Filtrar filas sin fecha y forzar conversión de fecha
rentabilidad <- rentabilidad[complete.cases(rentabilidad$Fecha),]
rentabilidad$Fecha <- as.Date(rentabilidad$Fecha)


# Definir la interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("Calculadora y Comparador de Rentabilidad de Fondos de AFP"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("fondo", "Seleccione uno o más Fondos:",
                     choices = nombres_columnas,
                     selected = c("Integra_Fondo_2", "Profuturo_Fondo_2"),
                     multiple = TRUE),
      
      dateRangeInput("fechas", "Seleccione el Período:",
                     start = min(rentabilidad$Fecha),
                     end = max(rentabilidad$Fecha),
                     min = min(rentabilidad$Fecha),
                     max = max(rentabilidad$Fecha),
                     format = "dd/mm/yyyy",
                     separator = " - "),
      
      helpText("Seleccione uno o varios fondos para comparar su evolución. Para ver los cálculos de rentabilidad detallados, seleccione un solo fondo."),
      
      helpText("Nota: Si la fecha de inicio que seleccionas es anterior a la fecha de la primera data disponible para el fondo, la aplicación automáticamente ajustará el cálculo para empezar desde la fecha más antigua con datos existentes. Los resultados de rentabilidad (acumulada y anualizada) se calcularán a partir de ese punto.")
    ),
    
    mainPanel(
      h3("Comparación de Evolución de Valor Cuota"),
      plotOutput("evolucion_plot"),
      h3("Resultados de Rentabilidad"),
      verbatimTextOutput("rentabilidad_output")
    )
  ),
  
  # Explicación y autoría
  fluidRow(
      column(12,
          wellPanel(
              h4("Sobre esta Aplicación"),
              p("Esta aplicación te permite calcular y visualizar la rentabilidad de los diferentes fondos de las AFP en Perú."),
              p("Puedes seleccionar uno o varios fondos para comparar su desempeño en un mismo gráfico. Para un análisis detallado de un fondo individual (rentabilidad acumulada y anualizada), selecciona solo uno."),
              hr(),
              p("Desarrollado por Arturo Chian.", style = "font-style: italic;"),
              p("Visita mi sitio web o contáctame en LinkedIn:"),
              tags$a(href = "http://arturochian.com", target = "_blank", "arturochian.com"),
              " | ",
              tags$a(href = "https://www.linkedin.com/in/arturo-benjamin-chian-nuñez-frm-ba73965b", target = "_blank", "LinkedIn")
          )
      )
  )
)

# Definir la lógica del servidor
server <- function(input, output) {
  
  # Expresión reactiva para filtrar los datos
  datos_reactivos <- reactive({
    req(input$fondo)
    
    datos_filtrados <- subset(rentabilidad, Fecha >= input$fechas[1] & Fecha <= input$fechas[2])
    
    lista_datos <- lapply(input$fondo, function(fondo_seleccionado) {
      precios <- datos_filtrados[[fondo_seleccionado]]
      fechas <- datos_filtrados$Fecha
      
      valid_indices <- !is.na(precios)
      
      data.frame(
        Fecha = fechas[valid_indices],
        Precio = precios[valid_indices],
        Fondo = fondo_seleccionado
      )
    })
    
    do.call(rbind, lista_datos)
  })
  
  # Renderizar el gráfico de evolución
  output$evolucion_plot <- renderPlot({
    
    datos_plot <- datos_reactivos()
    
    if (nrow(datos_plot) < 2) {
      return(NULL)
    }
    
    # Normalizar precios para comparar desde una base 100
    datos_plot_normalizado <- datos_plot %>%
      group_by(Fondo) %>%
      arrange(Fecha) %>%
      mutate(Precio_Normalizado = (Precio / first(Precio)) * 100) %>%
      ungroup()
      
    ggplot(datos_plot_normalizado, aes(x = Fecha, y = Precio_Normalizado, color = Fondo)) +
      geom_line(linewidth = 1) +
      labs(title = "Comparación de la Evolución de Valor Cuota (Base 100)",
           x = "Fecha",
           y = "Valor Cuota Normalizado (Inicio = 100)",
           color = "Fondo") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Renderizar el texto de resultados
  output$rentabilidad_output <- renderPrint({
    
    datos_calculo <- datos_reactivos()
    
    if (nrow(datos_calculo) < 2) {
      return("No hay suficientes datos en el período seleccionado para calcular la rentabilidad.")
    }
    
    if (length(input$fondo) > 1) {
      
      # Calcular rentabilidad para cada fondo y mostrar tabla
      tabla_resultados <- datos_calculo %>%
        group_by(Fondo) %>%
        arrange(Fecha) %>%
        summarise(
          `Fecha Inicial` = first(Fecha),
          `Fecha Final` = last(Fecha),
          `Valor Inicial` = first(Precio),
          `Valor Final` = last(Precio),
          .groups = 'drop'
        ) %>%
        mutate(
          `Años` = round(as.numeric(difftime(`Fecha Final`, `Fecha Inicial`, units = "days")) / 365.25, 2),
          `Rent. Acumulada` = scales::percent((`Valor Final` / `Valor Inicial`) - 1, accuracy = 0.01),
          `Rent. Anualizada` = scales::percent(((`Valor Final` / `Valor Inicial`)^(1 / `Años`)) - 1, accuracy = 0.01)
        ) %>%
        select(
          Fondo,
          `Rent. Acumulada`,
          `Rent. Anualizada`,
          `Años`
        )
      
      # Imprimir el dataframe. renderPrint se encargará de mostrarlo como texto.
      print(as.data.frame(tabla_resultados))
      
    } else {
      # Lógica para un solo fondo
      
      # Valores inicial y final
      valor_inicial <- head(datos_calculo$Precio, 1)
      valor_final <- tail(datos_calculo$Precio, 1)
      fecha_inicial <- head(datos_calculo$Fecha, 1)
      fecha_final <- tail(datos_calculo$Fecha, 1)
      
      # Calcular la rentabilidad acumulada
      rentabilidad_acumulada <- (valor_final / valor_inicial) - 1
      
      # Calcular años y rentabilidad anualizada
      dias <- as.numeric(difftime(fecha_final, fecha_inicial, units = "days"))
      anios <- dias / 365.25
      
      if (dias == 0) {
        rentabilidad_anualizada <- 0
      } else {
        rentabilidad_anualizada <- (valor_final / valor_inicial)^(365 / dias) - 1
      }
      
      # Mostrar resultados
      cat(paste("Fondo Seleccionado:", input$fondo, "\n"))
      cat(paste("Período:", format(fecha_inicial, "%d/%m/%Y"), "-", format(fecha_final, "%d/%m/%Y"), "\n"))
      cat(paste("Años en Período:", round(anios, 2), "\n\n"))
      cat(paste("Rentabilidad Acumulada:", scales::percent(rentabilidad_acumulada, accuracy = 0.01), "\n"))
      cat(paste("Rentabilidad Anualizada:", scales::percent(rentabilidad_anualizada, accuracy = 0.01), "\n"))
    }
  })
  
}


# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)

# -----------------------------------------------------------------------------
# SIMULADOR DE RENTA VITALICIA Y TEMPORAL EN SHINY
# Autor: Gemini
# Descripción: Una aplicación para simular planes de pensión basados en
# un fondo inicial, rentabilidad y definiendo la pensión o la duración.
# -----------------------------------------------------------------------------

# 1. Cargar las librerías necesarias
# Si no las tienes, instálalas con: install.packages(c("shiny", "ggplot2", "DT", "scales"))
library(shiny)
library(ggplot2)
library(DT)
library(scales)

# --- Define la Interfaz de Usuario (UI) ---
ui <- fluidPage(
    # Título de la aplicación
    titlePanel("Simulador Interactivo de Planes de Jubilación"),
    
    # Diseño de la página con una barra lateral para controles
    sidebarLayout(
        # Panel lateral para los inputs del usuario
        sidebarPanel(
            h4("Parámetros de la Simulación"),
            
            # Input para el monto inicial del fondo
            numericInput("fondo_inicial", 
                         "1. Monto inicial del fondo (S/):", 
                         value = 72789.66, min = 0, step = 1000),
            helpText("Nota: El saldo para la simulación corresponde al 95.5% del fondo, según ley."),
            
            # Input para la tasa de rentabilidad anual
            sliderInput("tasa_anual", 
                        "2. Rentabilidad anual esperada (%):", 
                        min = 0, max = 15, value = 7, step = 0.1),
            
            hr(),
            
            # Selector para que el usuario elija qué variable controlar
            radioButtons("control_variable", 
                         h4("¿Qué deseas definir?"),
                         choices = list("Definir la Pensión Mensual" = "pension",
                                        "Definir los Años de Jubilación" = "anios"),
                         selected = "pension"),
            
            # Panel condicional que aparece si el usuario elige "Definir Pensión Mensual"
            conditionalPanel(
                condition = "input.control_variable == 'pension'",
                numericInput("pension_mensual", 
                             "3. Pensión mensual deseada (S/):", 
                             value = 700, min = 0, step = 50),
                # Botón para calcular la pensión a perpetuidad
                actionButton("calc_perpetua", "Calcular Pensión a Perpetuidad", icon = icon("infinity"))
            ),
            
            # Panel condicional que aparece si el usuario elige "Definir Años de Jubilación"
            conditionalPanel(
                condition = "input.control_variable == 'anios'",
                sliderInput("anios_jubilacion", 
                            "3. Años deseados de pensión:", 
                            min = 1, max = 50, value = 15, step = 1)
            ),
            
            hr(),
            p(strong("Nota:"), "La perpetuidad ocurre cuando la pensión mensual es igual al interés ganado por el fondo, manteniendo el capital intacto.")
        ),
        
        # Panel principal para mostrar los resultados (tabla, gráfico, resumen)
        mainPanel(
            tabsetPanel(
                type = "tabs",
                # Pestaña 1: Resumen y Gráfico
                tabPanel("Resumen y Gráfico",
                         br(),
                         # Salida para el resumen de resultados
                         uiOutput("resumen"),
                         hr(),
                         # Salida para el gráfico de evolución del fondo
                         h4("Evolución del Saldo del Fondo en el Tiempo"),
                         plotOutput("grafico_saldo")
                ),
                # Pestaña 2: Cronograma detallado
                tabPanel("Cronograma Detallado",
                         br(),
                         # Salida para la tabla de datos
                         DT::dataTableOutput("tabla_cronograma")
                )
            )
        )
    )
)

# --- Define la Lógica del Servidor (Server) ---
server <- function(input, output, session) {

    # Expresión reactiva para calcular el cronograma de pagos
    cronograma_data <- reactive({
        # Validar que el fondo inicial sea un número positivo
        req(input$fondo_inicial > 0)
        
        # --- Obtener parámetros iniciales ---
        fondo <- input$fondo_inicial * 0.955 # Aplicar el 95.5%
        tasa_mensual <- input$tasa_anual / 100 / 12
        
        # --- Lógica de cálculo según la elección del usuario ---
        if (input$control_variable == 'anios') {
            # Si el usuario define los años, calculamos la pensión mensual (fórmula de anualidad)
            req(input$anios_jubilacion > 0)
            n_meses <- input$anios_jubilacion * 12
            
            # Evitar división por cero si la tasa es 0
            if (tasa_mensual > 0) {
                pension <- fondo * (tasa_mensual * (1 + tasa_mensual)^n_meses) / ((1 + tasa_mensual)^n_meses - 1)
            } else {
                pension <- fondo / n_meses
            }
            
        } else {
            # Si el usuario define la pensión, la usamos directamente
            req(input$pension_mensual > 0)
            pension <- input$pension_mensual
            # Ponemos un límite de 100 años para evitar bucles infinitos
            n_meses <- 1200 
        }

        # --- Generación del cronograma mes a mes ---
        saldo_actual <- fondo
        df <- data.frame(
            Mes = integer(),
            Saldo_Inicial = numeric(),
            Interes_Ganado = numeric(),
            Pension_Pagada = numeric(),
            Saldo_Final = numeric()
        )
        
        for (mes in 1:n_meses) {
            # Romper el bucle si el fondo se agota
            if (saldo_actual <= 0) break
            
            interes_ganado <- saldo_actual * tasa_mensual
            
            # Asegurarse de no pagar más de lo que queda
            pension_a_pagar <- min(pension, saldo_actual + interes_ganado)
            
            saldo_final <- saldo_actual + interes_ganado - pension_a_pagar
            
            # Añadir la fila al dataframe
            df <- rbind(df, data.frame(
                Mes = mes,
                Saldo_Inicial = saldo_actual,
                Interes_Ganado = interes_ganado,
                Pension_Pagada = pension_a_pagar,
                Saldo_Final = saldo_final
            ))
            
            saldo_actual <- saldo_final
        }
        
        return(df)
    })
    
    # Evento para el botón de calcular pensión a perpetuidad
    observeEvent(input$calc_perpetua, {
        pension_perpetua <- (input$fondo_inicial * 0.955) * (input$tasa_anual / 100 / 12)
        
        # Actualiza el input de la pensión mensual con el valor calculado
        updateNumericInput(session, "pension_mensual", value = round(pension_perpetua, 2))
    })

    # --- Renderizar los Outputs ---
    
    # 1. Renderizar el resumen de texto
    output$resumen <- renderUI({
        df <- cronograma_data()
        if (nrow(df) == 0) return(NULL)
        
        # Calcular los totales
        total_interes <- sum(df$Interes_Ganado)
        total_pagado <- sum(df$Pension_Pagada)
        duracion_meses <- max(df$Mes)
        duracion_anios <- floor(duracion_meses / 12)
        meses_restantes <- duracion_meses %% 12

        # Determinar la pensión mensual efectiva
        pension_efectiva <- if (input$control_variable == 'anios') {
            df$Pension_Pagada[1] # La pensión calculada
        } else {
            input$pension_mensual # La pensión definida por el usuario
        }
        
        # Crear el texto del resumen con formato HTML
        HTML(paste(
            "<h4>Resumen de la Simulación</h4>",
            "<p>Con un fondo total de <strong>", prettyNum(input$fondo_inicial, big.mark=","), " S/</strong>, se usa un saldo inicial de <strong>", prettyNum(round(input$fondo_inicial * 0.955, 2), big.mark=",")," S/ (95.5%)</strong> para la simulación, con una rentabilidad del <strong>", input$tasa_anual, "%</strong> anual:</p>",
            "<ul>",
            "<li>Pensión mensual resultante: <strong>", prettyNum(round(pension_efectiva, 2), big.mark=","), " S/</strong></li>",
            "<li>Tu fondo durará: <strong>", duracion_anios, " años y ", meses_restantes, " meses</strong>.</li>",
            "<li>Recibirás un total de: <strong>", prettyNum(round(total_pagado, 2), big.mark=","), " S/</strong> en pensiones.</li>",
            "<li>Generarás un total de: <strong>", prettyNum(round(total_interes, 2), big.mark=","), " S/</strong> en intereses.</li>",
            "</ul>"
        ))
    })

    # 2. Renderizar la tabla de datos interactiva
    output$tabla_cronograma <- DT::renderDataTable({
        DT::datatable(
            cronograma_data(),
            options = list(pageLength = 12, autoWidth = TRUE, searchHighlight = TRUE),
            rownames = FALSE
        ) %>%
        formatCurrency(c("Saldo_Inicial", "Interes_Ganado", "Pension_Pagada", "Saldo_Final"), currency = "S/ ", interval = 3, mark = ",")
    })
    
    # 3. Renderizar el gráfico de evolución del saldo
    output$grafico_saldo <- renderPlot({
        df <- cronograma_data()
        if (nrow(df) == 0) return(NULL)
        
        ggplot(df, aes(x = Mes, y = Saldo_Final)) +
            geom_line(color = "dodgerblue", size = 1.2) +
            geom_area(fill = "dodgerblue", alpha = 0.2) +
            scale_y_continuous(labels = scales::comma_format(big.mark = ",", decimal.mark = ".")) +
            labs(
                title = paste("Disminución del fondo a lo largo de", floor(max(df$Mes)/12), "años"),
                x = "Mes",
                y = "Saldo del Fondo (S/)"
            ) +
            theme_minimal(base_size = 14)
    })
}

# --- Ejecutar la aplicación Shiny ---
shinyApp(ui = ui, server = server)

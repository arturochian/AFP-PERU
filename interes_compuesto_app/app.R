
library(shiny)
library(ggplot2)
library(DT)

# Define la interfaz de usuario
ui <- fluidPage(
    titlePanel("Simulador de Interés Compuesto"),
    
    # Explicación y autoría
    fluidRow(
        column(12,
            wellPanel(
                h4("¿Qué es el Interés Compuesto?"),
                p("El interés compuesto es el motor invisible del crecimiento financiero a largo plazo. A diferencia del interés simple, que solo se aplica al capital inicial, el interés compuesto se calcula sobre el capital principal y también sobre los intereses acumulados de los períodos anteriores. Es lo que popularmente se conoce como \"interés sobre interés\"."),                p("Gracias a este efecto exponencial, un capital invertido crece a un ritmo cada vez más acelerado, convirtiendo el tiempo en tu principal aliado para multiplicar tus ahorros o inversiones."),                p(tags$b("En este simulador, podrás ver con tus propios ojos el verdadero poder de esta herramienta clave en las finanzas.")),
                hr(),
                p("Desarrollado por Arturo Chian.", style = "font-style: italic;"),
                p("Visita mi sitio web o contáctame en LinkedIn:"),
                tags$a(href = "http://arturochian.com", target = "_blank", "arturochian.com"),
                " | ",
                tags$a(href = "https://www.linkedin.com/in/arturo-benjamin-chian-nuñez-frm-ba73965b", target = "_blank", "LinkedIn")
            )
        )
    ),
    
    sidebarLayout(
        sidebarPanel(
            numericInput("monto_inicial", "Monto Inicial (S/.)", 100, min = 1),
            numericInput("plazo", "Plazo (años)", 35, min = 1),
            sliderInput("rentabilidad", "Rentabilidad Anual (%)", 
                        min = 0, max = 25, value = 8, step = 0.1)
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Gráfico de Crecimiento", plotOutput("grafico_crecimiento")),
                tabPanel("Tabla de Datos", DTOutput("tabla_datos"))
            )
        )
    )
)

# Define la lógica del servidor
server <- function(input, output) {
    
    datos_proyeccion <- reactive({
        monto <- input$monto_inicial
        plazo_years <- input$plazo
        tasa_anual <- input$rentabilidad / 100
        
        data.frame(
            Año = 0:plazo_years,
            Capital = monto * (1 + tasa_anual)^(0:plazo_years)
        )
    })
    
    output$grafico_crecimiento <- renderPlot({
        df <- datos_proyeccion()
        
        ggplot(df, aes(x = Año, y = Capital)) +
            geom_line(color = "blue", size = 1.5) +
            geom_point(color = "blue", size = 3) +
            labs(
                title = "Crecimiento del Capital a lo Largo del Tiempo",
                x = "Años",
                y = "Capital Acumulado (S/.)"
            ) +
            theme_minimal() +
            scale_y_continuous(labels = scales::comma)
    })
    
    output$tabla_datos <- renderDT({
        df <- datos_proyeccion()
        df$Ganancia <- df$Capital - input$monto_inicial
        df$Capital <- round(df$Capital, 2)
        df$Ganancia <- round(df$Ganancia, 2)
        
        datatable(df, options = list(pageLength = 50), rownames = FALSE)
    })
    
}

# Corre la aplicación
shinyApp(ui = ui, server = server)

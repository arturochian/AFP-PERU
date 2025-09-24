library(dplyr)
library(readxl)
library(tidyr)

# Establecer el directorio de trabajo
setwd("d:\\ABCN\\Github\\AFP-PERU\\salarios_historicos_AFP_app")

# --- Carga de datos comunes ---
# Cargar valor cuota de AFP Integra
integra2 <- readxl::read_xlsx("data_salarios.xlsx", sheet = "INTEGRA")
integra2 <- integra2[complete.cases(integra2$Fecha), c(1:2)]
names(integra2)[1] <- "Fecha"
valor_integra_col <- names(integra2)[2] # Nombre de la columna de valor cuota

# Crear un calendario completo de fechas
all_dates <- tibble(
  Fecha = seq(min(integra2$Fecha), max(integra2$Fecha), by = "day")
)

# Unir el calendario con los datos de valor cuota y rellenar
integra_calendario <- all_dates %>%
  left_join(integra2, by = "Fecha") %>%
  fill(!!sym(valor_integra_col), .direction = "down")

# --- Procesamiento por cada tipo de salario ---

# Obtener los nombres de las hojas del archivo Excel
sheet_names <- readxl::excel_sheets("data_salarios.xlsx")

# Filtrar las hojas que no se deben procesar
excluded_sheets <- c("INTEGRA", "Tabla de post", "renta seguros")
salario_sheets <- setdiff(sheet_names, excluded_sheets)

# Crear un dataframe para almacenar los resultados
resultados <- data.frame(Tipo_Salario = character(), Suma_Cuotas = numeric(), stringsAsFactors = FALSE)

# Loop a través de cada hoja de salario
for (sheet in salario_sheets) {
  
  # Cargar los datos del salario específico
  salario_data <- readxl::read_xlsx("data_salarios.xlsx", sheet = sheet)
  names(salario_data)[1] <- "Fecha"
  names(salario_data)[2] <- "Salario" # Asumimos que el salario está en la segunda columna
  
  # Unir datos de salario con el calendario de valor cuota
  datos_completos <- integra_calendario %>%
    left_join(salario_data, by = "Fecha")
    
  # Rellenar los valores de salario para los días no hábiles
  # Nota: El primer valor de salario puede ser NA si la serie empieza después, lo rellenamos con 0 o el primer valor válido.
  datos_completos <- datos_completos %>%
    fill(Salario, .direction = "down") %>%
    fill(Salario, .direction = "up") # Rellenar hacia arriba por si el primer día es NA

  # Filtrar para obtener solo el último día de cada mes
  fin_de_mes <- datos_completos %>%
    mutate(year_month = format(Fecha, "%Y-%m")) %>%
    group_by(year_month) %>%
    filter(Fecha == max(Fecha)) %>%
    ungroup() %>%
    select(-year_month)
    
  # Calcular Aporte y Cuotas
  fin_de_mes <- fin_de_mes %>%
    mutate(
      aporte = Salario * 0.10,
      cuotas = aporte / !!sym(valor_integra_col)
    )
    
  # Calcular la suma total de cuotas y guardarla
  suma_total_cuotas <- sum(fin_de_mes$cuotas, na.rm = TRUE)
  
  resultados <- rbind(resultados, data.frame(Tipo_Salario = sheet, Suma_Cuotas = suma_total_cuotas))
}

# Obtener el último valor de la cuota de Integra
ultimo_valor_cuota <- tail(integra2[[valor_integra_col]], 1)

# Añadir la columna con el valor del fondo en soles
resultados <- resultados %>%
  mutate(Valor_Fondo_Soles = Suma_Cuotas * ultimo_valor_cuota)

# Imprimir los resultados finales
print(resultados)

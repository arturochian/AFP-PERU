library(dplyr)
library(readxl)
library(tidyr)

setwd("d:\\ABCN\\Github\\AFP-PERU\\salarios_historicos_AFP_app")

integra2 <- readxl::read_xlsx("data_salarios.xlsx", sheet = "INTEGRA")
integra2 <- integra2[complete.cases(integra2$Fecha), c(1:2)]
names(integra2)[1] <- "Fecha"

RMV <- readxl::read_xlsx("data_salarios.xlsx", sheet = "RMV")
names(RMV)[1] <- "Fecha"

# Crear un calendario completo de fechas desde el inicio hasta el final de tus datos.
all_dates <- tibble(
  Fecha = seq(min(integra2$Fecha), max(integra2$Fecha), by = "day")
)

# Unir el calendario con tus datos. Esto creará filas NA para los días no hábiles.
integra_final <- all_dates %>%
  left_join(integra2, by = "Fecha") %>%
  left_join(RMV, by = "Fecha")

# Rellenar los valores NA usando el último valor conocido.
valor_integra_col <- names(integra2)[2]
integra_final$Salario[1]<-72
integra_final <- integra_final %>%
  fill(!!sym(valor_integra_col), .direction = "down") %>%
  fill(Salario, .direction = "down")

# --- Filtrar para obtener solo el último día de cada mes ---
integra_fin_de_mes <- integra_final %>%
  mutate(year_month = format(Fecha, "%Y-%m")) %>%
  group_by(year_month) %>%
  filter(Fecha == max(Fecha)) %>%
  ungroup() %>%
  select(-year_month)

# --- Calcular Aporte y Cuotas ---
integra_fin_de_mes <- integra_fin_de_mes %>%
  mutate(
    aporte = Salario * 0.10,
    cuotas = aporte / !!sym(valor_integra_col)
  )
sum(integra_fin_de_mes$cuotas)

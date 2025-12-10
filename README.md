
# Análisis Integral de Tendencias de Homicidios con Perspectiva de Género (2020-2024)
# Fuente de Datos: Microdatos de defunciones generales (INEGI)
# Autor: Mtro. José César Romero Galván

# Descripción del Proyecto

Este repositorio contiene un flujo de trabajo en R diseñado para analizar microdatos de defunciones generales del INEGI, con un enfoque específico en la violencia letal y sus diferencias estructurales por género en México.

# Herramientas y Librerías

El análisis fue desarrollado en R utilizando las siguientes librerías:

tidyverse (dplyr, ggplot2): Para manipulación de datos y visualización.

foreign: Para la lectura de archivos .dbf (formato nativo de INEGI).

scales: Para el formato de ejes y etiquetas numéricas.

# Metodología del Código

El script analisis_homicidios.R realiza las siguientes operaciones:

Ingesta y Limpieza (clean_data):

Itera sobre múltiples archivos anuales (.dbf).

Estandariza nombres de variables que han cambiado en el tiempo (ej. PRESUNTO vs TIPO_DEFUN).

Calcula edades precisas basándose en los códigos complejos del INEGI (diferenciando horas/meses de años cumplidos).

Categoriza lugares de ocurrencia (Vivienda, Vía pública, Otros).

Unificación: Consolida 5 años de datos (2020-2024) en un único dataframe histórico.

Visualización: Genera tres gráficos de listos para publicación.

# Visualizaciones y Hallazgos

El código genera gráficos que sustentan tres hallazgos principales detallados en el script:

La violencia tiene edad: Se evidencia que la violencia contra las mujeres comienza mucho antes; el porcentaje de niñas y adolescentes asesinadas (0-17 años) es el doble que el de sus pares varones.

Tendencias divergentes: Mientras la curva de homicidios masculinos muestra volatilidad, la de mujeres presenta una "estabilidad" crónica y preocupante.

El lugar importa: Se desmiente el mito de la seguridad en casa para las mujeres. Mientras el 82% de los hombres mueren en la vía pública, cerca del 40% de las mujeres son asesinadas dentro de una vivienda.

# Instrucciones de Uso

Clonar el repositorio.

Asegurarse de tener los archivos .dbf del INEGI en una carpeta local.

Ajustar las rutas (setwd y files_path) en la sección de "Configuración de Rutas" del script.

Ejecutar el script en RStudio.

# Código

#---- Sección 1: Procesamiento de Datos -----

# 1. Instalación y Carga de Librerías

# Nota: Las instalaciones están comentadas porque es un paso que había hechoo ya previamente, esto solo se hace una sola vez.

# install.packages("foreign")
# install.packages("tidyverse")
# install.packages("scales")

# Se cargan las librerías para el análisis.

library(foreign)    # Lectura de archivos .dbf (formato estándar de INEGI).
library(tidyverse)  # Carga herramientas para manipulación y visualización de datos.
library(scales)     # Herramientas para dar formato a los números (porcentajes, comas).

# Definición del espacio de trabajo

# Configuracion de la carpeta de la computadora en donde se va a trabajar.
# Aquí se guardaran los archivos generados con el código al final (Gráficas y el CSV final).

setwd("/Users/cesarromero/Documents/Data_Civica/Proyectos/Partes/B")

# Configuración de Rutas y Variables

# Definir la ruta donde se encuentran las bases de datos.

files_path <- "/Users/cesarromero/Documents/Data_Civica/Proyectos/Partes/B/Bases de datos"

# Se crea una lista de todos los archivos que terminan en .dbf en la carpeta especificada.
# full.names = TRUE devuelve la ruta completa de cada archivo.

files_list <- list.files(path = files_path, pattern = "\\.dbf$", full.names = TRUE, ignore.case = TRUE)

# Definición de Identidad Visual 

# Se asignan los colores a cada género para usarlos en las gráficas.

dc_colors <- c("Mujer" = "#BF459F", "Hombre" = "#03A61C")

# 2. Función de Lectura y Limpieza de Datos

# Se define la función 'clean_data' que toma una ruta de archivo como entrada.

clean_data <- function(path) {
  
# Se extraen los dígitos del año basados en el nombre del archivo.
  
  year_str <- str_extract(basename(path), "\\d+")
  
# Se convierte el texto a formato numérico (ej. "20" se convierte en 2020).
  
  year_num <- as.numeric(paste0("20", year_str))
  
# Se imprime un mensaje en la consola para monitorear el progreso.
  
  message(paste("Procesando año:", year_num))
  
# Se lee el archivo .dbf y se guarda como objeto en un dataframe.
  
  df <- read.dbf(path)
  
# Se convierten todos los nombres de columnas a mayúsculas para evitar errores.
  
  names(df) <- toupper(names(df)) 
  
# Se estandariza el nombre de la variable de homicidios.
# INEGI cambió 'PRESUNTO' por 'TIPO_DEFUN' en años recientes. Esto lo corrige.
  
  if ("PRESUNTO" %in% names(df)) {
    df <- df %>% rename(TIPO_DEFUN = PRESUNTO)
  }
  
# Se inicia el flujo de transformación de datos.
  
  df %>%
    
# Se filtran las filas donde TIPO_DEFUN es 2 (Homicidio).
    
    filter(TIPO_DEFUN == 2) %>% 
    
# Se seleccionan solo las columnas esenciales para optimizar memoria.
    
    select(SEXO, LUGAR_OCUR, EDAD) %>% 
    
# Se crean nuevas variables calculadas.
    
    mutate(
  
# Se asigna el año extraído a una nueva columna.
      
      year = year_num,
      
# Variable de Género 

# Se crea la etiqueta 'sex_label' mapeando códigos numéricos a texto.
# 1 = Hombre, 2 = Mujer.
      
      sex_label = case_when(
        SEXO == 1 ~ "Hombre",
        SEXO == 2 ~ "Mujer",
        TRUE ~ NA_character_
      ),
      
# Variable de Lugar 

# Se agrupa el lugar de ocurrencia en categorías más amplias.
# 0, 1 = Vivienda particular; 3, 4 = Vía pública; Resto = Otros espacios.

      grouped_place = case_when(
        LUGAR_OCUR %in% c(0, 1) ~ "Vivienda particular",
        LUGAR_OCUR %in% c(3, 4) ~ "Vía pública",
        TRUE ~ "Otros espacios"
      ),

# Cálculo de Edad 

# Lógica INEGI: Códigos 4001-4120 representan años cumplidos (Edad = Código - 4000).
# Códigos 1000-3999 son horas/días/meses (Infantes < 1 año -> 0).

      calculated_age = case_when(
        EDAD >= 4001 & EDAD <= 4120 ~ EDAD - 4000, 
        EDAD >= 1000 & EDAD < 4000 ~ 0,            
        TRUE ~ NA_real_                            
      ),
      
# Grupos de Edad
# Se categoriza la edad calculada en etapas de vida.

      age_group = case_when(
        calculated_age <= 17 ~ "Infancia/Adol. (0-17)",
        calculated_age >= 18 & calculated_age <= 29 ~ "Jóvenes (18-29)",
        calculated_age >= 30 & calculated_age <= 59 ~ "Adultos (30-59)",
        calculated_age >= 60 ~ "Adultos Mayores (60+)",
        TRUE ~ "No especificado"
      )
    ) %>%
    
# Se eliminan las filas donde el sexo es desconocido para mantener el análisis limpio.
    
    filter(!is.na(sex_label))
}

# 3. Unificación de Datos 

# Se aplica la función 'clean_data' a cada archivo en 'files_list' y unir resultados en un solo dataframe.

historical_df <- map_dfr(files_list, clean_data)

# Se establecen los niveles del factor para 'sex_label' para controlar el orden (Mujer primero).

historical_df$sex_label <- factor(historical_df$sex_label, levels = c("Mujer", "Hombre"))

# Se establecen los niveles para 'age_group' para asegurar orden cronológico en las gráficas.
historical_df$age_group <- factor(historical_df$age_group, 
                                  levels = c("Infancia/Adol. (0-17)", "Jóvenes (18-29)", 
                                             "Adultos (30-59)", "Adultos Mayores (60+)"))


#---- Sección 2: Visualización de Datos -----



# 1. Gráfica 1: Edad y Género ("La Generación Perdida") 
# Calculo de la distribución porcentual de víctimas por grupo de edad dentro de cada género.

df_plot1 <- historical_df %>% 
  
# Filtrado de grupos de edad nulos. 
  
  filter(!is.na(age_group)) %>%
  
# Agrupación por sexo y grupo de edad.
  
  group_by(sex_label, age_group) %>%
  
# Conteo total de víctimas por grupo.
  
  summarise(total = n(), .groups = "drop") %>%
  
# Agrupación por sexo nuevamente para calcular porcentajes relativos al total del género.
  
  group_by(sex_label) %>%
  
# Calculo de la participación de cada grupo de edad.
  
  mutate(percentage = total / sum(total)) 

# Se crea la gráfica de barras.

plot1 <- ggplot(df_plot1, aes(x = age_group, y = percentage, fill = sex_label)) +
  
# Se crean barras posicionadas lado a lado con "dodge".
  
  geom_col(position = "dodge", width = 0.7) +
  
# Se agregan las etiquetas de porcentaje sobre las barras.
  
  geom_text(aes(label = percent(percentage, 1)), 
            position = position_dodge(width = 0.7), vjust = -0.5, 
            fontface = "bold", size = 4) +
  
# Se formatea el eje Y como porcentaje.
  
  scale_y_continuous(labels = percent, limits = c(0, 0.6)) +
  
# Se aplican los colores personalizados.
  
  scale_fill_manual(values = dc_colors) +
  
# Se usa el tema minimalista (es el más utilizando en ggplot).
  
  theme_minimal(base_family = "sans") +
  
# Se agregan títulos y etiquetas.
  
  labs(
    title = "HALLAZGO 1: LA VIOLENCIA TIENE EDAD",
    subtitle = "Distribución de víctimas por grupo de edad y sexo (2020-2024)",
    x = "", y = "Porcentaje de víctimas",
    caption = "Fuente: INEGI | Elaboración: Github @CesarRomero10",
    fill = ""
  ) +
  
# Se ajusta la posición de la leyenda y estilo del título.
  
  theme(
    legend.position = "top", 
    plot.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(face = "italic", size = 9, color = "#666666", margin = margin(t = 15)) # se modifica el pie de página
  )


# Se muestra la gráfica 1.

print(plot1)


# 2. Gráfica 2: Tendencia de Homicidios por Año y Sexo 

# Preparar datos: contar homicidios por año y sexo.

df_plot2 <- historical_df %>%
  count(year, sex_label)

# Se crea la gráfica de línea.

plot2 <- ggplot(df_plot2, aes(x = year, y = n, color = sex_label)) +
  
# Se dibujan las líneas con grosor específico.
  
  geom_line(linewidth = 1.2) +
  
# Se agregan los puntos en cada intersección de datos.
  
  geom_point(size = 3) +
  
# Se agregan las etiquetas de texto mostrando la cifra directamente en la gráfica.
  
  geom_text(aes(label = comma(n)), vjust = -1, fontface = "bold", show.legend = FALSE) +
  
# Se aplican los colores personalizados.
  
  scale_color_manual(values = dc_colors) +
  
# Se formatean el eje Y con comas y se agregan espacio superior.
  
  scale_y_continuous(labels = comma, limits = c(0, NA), expand = expansion(mult = c(0, 0.2))) +
  
# Se asegura que el eje X muestre solo años enteros.
  
  scale_x_continuous(breaks = unique(historical_df$year)) +
  
# Se usa el tema minimalista.
  
  theme_minimal(base_family = "sans") +
  
# Se agregan los títulos y etiquetas.
  
  labs(
    title = "HALLAZGO 2: TENDENCIA DE LA VIOLENCIA (2020-2024)",
    subtitle = "Número total de homicidios registrados por año y sexo",
    x = "Año", y = "Número de víctimas",
    caption = "Fuente: INEGI | Elaboración: Github @CesarRomero10",
    color = ""
  ) +
  
# Se ajusta la leyenda y el estilo del título.
  
  theme(
    legend.position = "top", 
    plot.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(face = "italic", size = 9, color = "#666666", margin = margin(t = 15)) # se modifica el pie de página
  )

# Se muestra la gráfica 2.

print(plot2)


# 3. Gráfica 3: Lugar de Ocurrencia por Sexo 

# Se filtran los datos solo para las dos categorías principales de lugar.

df_plot3 <- historical_df %>%
  filter(grouped_place %in% c("Vivienda particular", "Vía pública")) %>%
  
# Se agrupan por sexo y lugar.
  
  group_by(sex_label, grouped_place) %>%
  
# Se realizan los conteos totales.
  
  summarise(total = n(), .groups = "drop") %>%
  
# Se calculan los porcentajes dentro de cada género.
  
  group_by(sex_label) %>%
  mutate(percentage = total / sum(total))

# Se crea la gráfica de barras.

plot3 <- ggplot(df_plot3, aes(x = sex_label, y = percentage, fill = sex_label)) +
  
# Se crea +n las barras de la gráfica con geom_col
  
  geom_col(position = "dodge", width = 0.6) +
  
# Se divide la gráfica en dos paneles basados en el lugar.
  
  facet_wrap(~ grouped_place) +
  
# Se agregan las etiquetas de porcentaje.
  
  geom_text(aes(label = percent(percentage, 1)), vjust = -0.5, fontface = "bold") +
  
# Se formatea el eje Y.
  
  scale_y_continuous(labels = percent, limits = c(0, 1.15)) +
  
# Se aplican los colores personalizados.
  
  scale_fill_manual(values = dc_colors) +
  
# Se usa el tema minimalista
  
  theme_minimal(base_family = "sans") +
  
# Se agregan los títulos y etiquetas.
  
  labs(
    title = "HALLAZGO 3: ¿DÓNDE MATAN A LAS MUJERES?",
    subtitle = "Porcentaje acumulado por lugar de ocurrencia",
    x = "", y = "Porcentaje",
    caption = "Fuente: INEGI | Elaboración: Github @CesarRomero10",
    fill = ""
  ) +
  
# Se quita la leyenda y se ajusta el estilo del título.
  
  theme(
    legend.position = "top", 
    plot.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(face = "italic", size = 9, color = "#666666", margin = margin(t = 15)) # se modifica el pie de página
  )

# Se muestra gráfica 3.

print(plot3)

# Nota: No se guardan las gráficas automáticamente para permitir revisiones antes de la exportación final.
# Además, considero que tienen mayor calidad si se hace una captura de pantalla después de hacerle zoom.

#---- Sección 3: Hallazgos -----

# Hallazgo 1: Una infancia vulnerada: la violencia contra ellas empieza antes

# Solemos pensar en la violencia letal como un problema de adultos jóvenes, 
# y si miramos las cifras de los hombres, esa es la norma. Pero cuando ponemos 
# la lupa sobre las mujeres, la realidad es desgarradora: 
# la violencia contra ellas no respeta ni la niñez. Los datos nos gritan una verdad incómoda: 
# el porcentaje de niñas y adolescentes asesinadas (8%) es el doble que el de sus pares varones (4%). 
# Esto no es solo una estadística, es una alerta roja de política pública: 
# nuestros sistemas de protección están fallando en la etapa más temprana. 
# No podemos diseñar estrategias de seguridad genéricas cuando la violencia de género nos está arrebatando el futuro desde la infancia.

# Hallazgo 2: La "estabilidad" que no podemos aceptar

# A veces, en los informes de seguridad, se celebra que las cifras "no suban" o se mantengan estables. 
# Pero, ¿podemos realmente llamar "estabilidad" a que asesinen a casi 4,000 mujeres cada año? 
# La gráfica es clara y dolorosa: mientras la curva de los hombres tiene altibajos (y un preocupante repunte reciente), 
# la línea de las mujeres es prácticamente plana. Esto nos dice que la violencia contra ellas se ha cronificado. 
# Las estrategias actuales de contención podrán mover los números macro, 
# pero no están tocando las raíces estructurales que mantienen el homicidio de mujeres en un nivel inaceptable. 
# No necesitamos que la curva se aplane; necesitamos romperla.

# Hallazgo 3: El mito de la seguridad en casa

# Llevamos años diseñando estrategias de seguridad volcadas hacia la calle: patrullajes, Guardia Nacional, 
# operativos. Sin embargo, los datos desmantelan la idea de que el peligro está solo "afuera". 
# Para los hombres, la calle sí es el principal escenario de riesgo (82%), 
# pero para las mujeres, la historia es radicalmente distinta: casi el 40% de ellas son asesinadas dentro de una vivienda. 
# Esto confirma lo que los movimientos feministas llevan años denunciando: para muchas, el hogar no es un refugio, sino una trampa. 
# Una política de seguridad que solo mira hacia la vía pública está, 
# dejando desprotegidas a casi la mitad de las mujeres víctimas de violencia letal.

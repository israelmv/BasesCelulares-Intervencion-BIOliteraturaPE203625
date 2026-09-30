# ============================================================
# ANÁLISIS DE LA INTERVENCIÓN EDUCATIVA - ZOTERO
# Bases Celulares de la Vida
# ============================================================

install.packages("writexl")
library(tidyverse)
library(rstatix)
library(ggpubr)
library(writexl)

setwd("C:/Users/karen/Documents/Intervencion Resultados")

datos <- read.csv(
  "IntervencionCSV - Hoja 1.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

str(datos)
head(datos)


names(datos)[1] <- "Estudiante"

names(datos)


# ============================================================
# PROMEDIOS PRE Y POST
# ============================================================

datos <- datos %>%
  mutate(
    Pre = rowMeans(select(., Pre1, Pre2, Pre3), na.rm = TRUE),
    Post = rowMeans(select(., Post1, Post2, Post3), na.rm = TRUE),
    Cambio = Post - Pre
  )

# Revisar
datos %>%
  select(Estudiante, Pre, Post, Cambio) %>%
  head()


# ============================================================
# ESTADÍSTICOS DESCRIPTIVOS
# ============================================================

descriptivos <- datos %>%
  summarise(
    n = n(),
    
    # PRE
    Media_Pre = mean(Pre, na.rm = TRUE),
    Mediana_Pre = median(Pre, na.rm = TRUE),
    DE_Pre = sd(Pre, na.rm = TRUE),
    IQR_Pre = IQR(Pre, na.rm = TRUE),
    Min_Pre = min(Pre, na.rm = TRUE),
    Max_Pre = max(Pre, na.rm = TRUE),
    
    # POST
    Media_Post = mean(Post, na.rm = TRUE),
    Mediana_Post = median(Post, na.rm = TRUE),
    DE_Post = sd(Post, na.rm = TRUE),
    IQR_Post = IQR(Post, na.rm = TRUE),
    Min_Post = min(Post, na.rm = TRUE),
    Max_Post = max(Post, na.rm = TRUE)
  )

descriptivos


# ============================================================
# DESCRIPTIVOS EN FORMATO VERTICAL
# ÚTIL PARA LA TABLA DEL ARTÍCULO
# ============================================================

datos_largos <- datos %>%
  select(Estudiante, Pre, Post) %>%
  pivot_longer(
    cols = c(Pre, Post),
    names_to = "Momento",
    values_to = "Puntuacion"
  )

tabla_descriptivos <- datos_largos %>%
  group_by(Momento) %>%
  summarise(
    n = n(),
    Media = mean(Puntuacion),
    Mediana = median(Puntuacion),
    DE = sd(Puntuacion),
    IQR = IQR(Puntuacion),
    Min = min(Puntuacion),
    Max = max(Puntuacion),
    .groups = "drop"
  )

tabla_descriptivos


# ============================================================
# PRUEBA DE WILCOXON PAREADA
# ============================================================

wilcox <- wilcox.test(
  datos$Post,
  datos$Pre,
  paired = TRUE,
  exact = FALSE
)

wilcox


# ============================================================
# TAMAÑO DEL EFECTO r
# ============================================================

# Crear formato largo para rstatix
wilcox_r <- datos %>%
  select(Estudiante, Pre, Post) %>%
  pivot_longer(
    cols = c(Pre, Post),
    names_to = "Momento",
    values_to = "Puntuacion"
  ) %>%
  mutate(
    Momento = factor(Momento, levels = c("Pre", "Post"))
  )

# Wilcoxon con tamaño del efecto
resultado_wilcox <- wilcox_r %>%
  wilcox_test(
    Puntuacion ~ Momento,
    paired = TRUE,
    detailed = TRUE
  ) %>%
  add_significance()

resultado_wilcox


# Tamaño del efecto r
efecto <- wilcox_r %>%
  wilcox_effsize(
    Puntuacion ~ Momento,
    paired = TRUE
  )

efecto


# ============================================================
# CAMBIO INDIVIDUAL
# ============================================================

datos <- datos %>%
  mutate(
    Clasificacion = case_when(
      Cambio > 0 ~ "Mejoró",
      Cambio == 0 ~ "Sin cambio",
      Cambio < 0 ~ "Empeoró"
    )
  )

datos %>%
  select(Estudiante, Pre, Post, Cambio, Clasificacion)


# ============================================================
# FRECUENCIA DE LAS CLASIFICACIONES
# ============================================================

clasificacion <- datos %>%
  count(Clasificacion) %>%
  mutate(
    Porcentaje = n / sum(n) * 100
  )

clasificacion


# ============================================================
# PORCENTAJE DE MEJORA INDIVIDUAL
# ============================================================

# IMPORTANTE:
# Si Pre = 0, el porcentaje de cambio relativo no se puede
# calcular matemáticamente.
#
# Por eso se deja como NA para esos estudiantes.

datos <- datos %>%
  mutate(
    Porcentaje_Mejora = case_when(
      Pre > 0 ~ ((Post - Pre) / Pre) * 100,
      Pre == 0 ~ NA_real_
    )
  )

datos %>%
  select(
    Estudiante,
    Pre,
    Post,
    Cambio,
    Clasificacion,
    Porcentaje_Mejora
  )


# ============================================================
# ¿CUÁNTOS ESTUDIANTES TIENEN PRE = 0?
# ============================================================

sum(datos$Pre == 0)

# Porcentaje
mean(datos$Pre == 0) * 100


# ============================================================
# CAMBIO PROMEDIO
# ============================================================

cambio_resumen <- datos %>%
  summarise(
    Media_Cambio = mean(Cambio),
    Mediana_Cambio = median(Cambio),
    DE_Cambio = sd(Cambio),
    IQR_Cambio = IQR(Cambio),
    Min_Cambio = min(Cambio),
    Max_Cambio = max(Cambio)
  )

cambio_resumen


# ============================================================
# GRÁFICA 1: BOXPLOT PRE VS POST
# ============================================================

grafico_boxplot <- datos_largos %>%
  ggplot(aes(x = Momento, y = Puntuacion)) +
  geom_boxplot(width = 0.55, outlier.shape = 21) +
  geom_jitter(
    width = 0.08,
    alpha = 0.5,
    size = 2
  ) +
  labs(
    title = "Desempeño antes y después de la intervención",
    x = NULL,
    y = "Puntuación promedio"
  ) +
  theme_classic(base_size = 13)

grafico_boxplot


# Guardar
ggsave(
  "Figura_1_Boxplot_Pre_Post.png",
  grafico_boxplot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# GRÁFICA 2: EVOLUCIÓN TEMPORAL DE LAS SEIS TAREAS
# ============================================================

datos_tareas <- datos %>%
  select(
    Estudiante,
    Pre1, Pre2, Pre3,
    Post1, Post2, Post3
  ) %>%
  pivot_longer(
    cols = -Estudiante,
    names_to = "Tarea",
    values_to = "Puntuacion"
  )

# Orden correcto
datos_tareas$Tarea <- factor(
  datos_tareas$Tarea,
  levels = c(
    "Pre1", "Pre2", "Pre3",
    "Post1", "Post2", "Post3"
  )
)

# Promedios por tarea
promedios_tarea <- datos_tareas %>%
  group_by(Tarea) %>%
  summarise(
    Media = mean(Puntuacion),
    DE = sd(Puntuacion),
    .groups = "drop"
  )

promedios_tarea


# Gráfica
grafico_temporal <- ggplot(
  promedios_tarea,
  aes(x = Tarea, y = Media, group = 1)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = Media - DE,
      ymax = Media + DE
    ),
    width = 0.1
  ) +
  labs(
    title = "Evolución del desempeño a través de las tareas",
    x = "Tarea",
    y = "Puntuación media"
  ) +
  theme_classic(base_size = 13)

grafico_temporal


ggsave(
  "Figura_2_Evolucion_Tareas.png",
  grafico_temporal,
  width = 8,
  height = 5,
  dpi = 300
)


# ============================================================
# GRÁFICA 3: TRAYECTORIAS INDIVIDUALES PRE VS POST
# ============================================================

grafico_trayectorias <- ggplot(
  datos_largos,
  aes(
    x = Momento,
    y = Puntuacion,
    group = Estudiante
  )
) +
  geom_line(
    alpha = 0.35,
    linewidth = 0.7
  ) +
  geom_point(
    alpha = 0.6,
    size = 1.8
  ) +
  labs(
    title = "Trayectorias individuales de desempeño",
    x = NULL,
    y = "Puntuación promedio"
  ) +
  theme_classic(base_size = 13)

grafico_trayectorias


ggsave(
  "Figura_3_Trayectorias_Individuales.png",
  grafico_trayectorias,
  width = 7,
  height = 6,
  dpi = 300
)


# ============================================================
# GRÁFICA 4: MEJORÓ / SIN CAMBIO / EMPEORÓ
# ============================================================

grafico_clasificacion <- ggplot(
  clasificacion,
  aes(x = Clasificacion, y = Porcentaje)
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = paste0(round(Porcentaje, 1), "%")),
    vjust = -0.4,
    size = 4
  ) +
  labs(
    title = "Cambio individual después de la intervención",
    x = NULL,
    y = "Estudiantes (%)"
  ) +
  ylim(0, 100) +
  theme_classic(base_size = 13)

grafico_clasificacion


ggsave(
  "Figura_4_Clasificacion_Cambio.png",
  grafico_clasificacion,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# TABLA INDIVIDUAL FINAL
# ============================================================

tabla_individual <- datos %>%
  select(
    Estudiante,
    Pre,
    Post,
    Cambio,
    Clasificacion,
    Porcentaje_Mejora
  )

tabla_individual


# ============================================================
# EXPORTAR RESULTADOS A EXCEL
# ============================================================

resultados_exportar <- list(
  Descriptivos = tabla_descriptivos,
  Cambio = cambio_resumen,
  Clasificacion = clasificacion,
  Analisis_individual = tabla_individual,
  Promedios_por_tarea = promedios_tarea
)

write_xlsx(
  resultados_exportar,
  "Resultados_Intervencion_Zotero.xlsx"
)


# ============================================================
# GUARDAR LOS DATOS PROCESADOS
# ============================================================

write.csv(
  datos,
  "Datos_Intervencion_Procesados.csv",
  row.names = FALSE
)


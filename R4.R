# ============================================================
# ANÁLISIS POR CRITERIO DE LA RÚBRICA
# ============================================================

library(tidyverse)
library(rstatix)
library(ggpubr)
library(writexl)


pre1 <- read.csv(
  "Intervencion - Puntuaciones Individuales por criterio.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

pre2 <- read.csv(
  "Intervencion - Puntuaciones Individuales por criterio2.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

pre3 <- read.csv(
  "Intervencion - Puntuaciones Individuales por criterio3.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

post1 <- read.csv(
  "Intervencion - Puntuaciones Individuales por criterio4.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

post2 <- read.csv(
  "Intervencion - Puntuaciones Individuales por criterio5.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

post3 <- read.csv(
  "Intervencion - Puntuaciones Individuales por criterio6.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)


limpiar_nombre <- function(x){
  
  x <- trimws(x)
  
  # quitar acentos
  x <- iconv(
    x,
    from = "UTF-8",
    to = "ASCII//TRANSLIT"
  )
  
  # pasar a minúsculas
  x <- tolower(x)
  
  # corregir específicamente la diferencia ortográfica
  # de Garduñño / Garduno
  x <- gsub(
    "gardunno",
    "garduno",
    x
  )
  
  # eliminar espacios múltiples
  x <- gsub(
    "\\s+",
    " ",
    x
  )
  
  return(x)
}


pre1$ID <- limpiar_nombre(pre1$Estudiante)
pre2$ID <- limpiar_nombre(pre2$Estudiante)
pre3$ID <- limpiar_nombre(pre3$Estudiante)
post1$ID <- limpiar_nombre(post1$Estudiante)
post2$ID <- limpiar_nombre(post2$Estudiante)
post3$ID <- limpiar_nombre(post3$Estudiante)


pre1 <- pre1 %>%
  rename(
    Pre1_Citas = Pre1_Citas,
    Pre1_Referencias = Pre1_Referencias,
    Pre1_Tipo = Pre1_Tipo,
    Pre1_Correspondencia = Pre1_Correspondencia,
    Pre1_Formato = Pre1_Formato
  )

pre2 <- pre2 %>%
  rename(
    Pre2_Citas = Pre2_Citas,
    Pre2_Referencias = Pre2_Referencias,
    Pre2_Tipo = Pre2_Tipo,
    Pre2_Correspondencia = Pre2_Correspondencia,
    Pre2_Formato = Pre2_Formato
  )

pre3 <- pre3 %>%
  rename(
    Pre3_Citas = Pre3_Citas,
    Pre3_Referencias = Pre3_Referencias,
    Pre3_Tipo = Pre3_Tipo,
    Pre3_Correspondencia = Pre3_Correspondencia,
    Pre3_Formato = Pre3_Formato
  )

post1 <- post1 %>%
  rename(
    Post1_Citas = Pre4_Citas,
    Post1_Referencias = Pre4_Referencias,
    Post1_Tipo = Pre4_Tipo,
    Post1_Correspondencia = Pre4_Correspondencia,
    Post1_Formato = Pre4_Formato
  )

post2 <- post2 %>%
  rename(
    Post2_Citas = Pre5_Citas,
    Post2_Referencias = Pre5_Referencias,
    Post2_Tipo = Pre5_Tipo,
    Post2_Correspondencia = Pre5_Correspondencia,
    Post2_Formato = Pre5_Formato
  )

post3 <- post3 %>%
  rename(
    Post3_Citas = Pre6_Citas,
    Post3_Referencias = Pre6_Referencias,
    Post3_Tipo = Pre6_Tipo,
    Post3_Correspondencia = Pre6_Correspondencia,
    Post3_Formato = Pre6_Formato
  )


pre1 <- pre1 %>%
  select(ID, Pre1_Citas:Pre1_Formato)

pre2 <- pre2 %>%
  select(ID, Pre2_Citas:Pre2_Formato)

pre3 <- pre3 %>%
  select(ID, Pre3_Citas:Pre3_Formato)

post1 <- post1 %>%
  select(ID, Post1_Citas:Post1_Formato)

post2 <- post2 %>%
  select(ID, Post2_Citas:Post2_Formato)

post3 <- post3 %>%
  select(ID, Post3_Citas:Post3_Formato)


# ============================================================
# UNIR LOS SEIS ARCHIVOS
# ============================================================

datos_criterios <- pre1 %>%
  full_join(pre2, by = "ID") %>%
  full_join(pre3, by = "ID") %>%
  full_join(post1, by = "ID") %>%
  full_join(post2, by = "ID") %>%
  full_join(post3, by = "ID")


# Revisar número de estudiantes
nrow(datos_criterios)

# Debe ser:
# 39


# ============================================================
# PROMEDIO PRE Y POST POR CRITERIO
# ============================================================

criterios <- c(
  "Citas",
  "Referencias",
  "Tipo",
  "Correspondencia",
  "Formato"
)


for (criterio in criterios) {
  
  datos_criterios[[paste0("Pre_", criterio)]] <-
    rowMeans(
      datos_criterios[
        ,
        paste0("Pre", 1:3, "_", criterio)
      ],
      na.rm = TRUE
    )
  
  datos_criterios[[paste0("Post_", criterio)]] <-
    rowMeans(
      datos_criterios[
        ,
        paste0("Post", 1:3, "_", criterio)
      ],
      na.rm = TRUE
    )
  
  datos_criterios[[paste0("Cambio_", criterio)]] <-
    datos_criterios[[paste0("Post_", criterio)]] -
    datos_criterios[[paste0("Pre_", criterio)]]
}


# ============================================================
# TABLA DESCRIPTIVA POR CRITERIO
# ============================================================

tabla_criterios <- map_dfr(
  criterios,
  function(criterio) {
    
    pre <- datos_criterios[[paste0("Pre_", criterio)]]
    post <- datos_criterios[[paste0("Post_", criterio)]]
    
    tibble(
      Criterio = criterio,
      
      Media_Pre = mean(pre, na.rm = TRUE),
      Mediana_Pre = median(pre, na.rm = TRUE),
      DE_Pre = sd(pre, na.rm = TRUE),
      IQR_Pre = IQR(pre, na.rm = TRUE),
      
      Media_Post = mean(post, na.rm = TRUE),
      Mediana_Post = median(post, na.rm = TRUE),
      DE_Post = sd(post, na.rm = TRUE),
      IQR_Post = IQR(post, na.rm = TRUE),
      
      Cambio_Medio =
        mean(post - pre, na.rm = TRUE),
      
      Cambio_Mediano =
        median(post - pre, na.rm = TRUE)
    )
  }
)

tabla_criterios
# ============================================================
#WILCOXON PAREADO POR CRITERIO
# ============================================================

resultados_wilcoxon <- map_dfr(
  criterios,
  function(criterio) {
    
    pre <- datos_criterios[[paste0("Pre_", criterio)]]
    post <- datos_criterios[[paste0("Post_", criterio)]]
    
    prueba <- wilcox.test(
      post,
      pre,
      paired = TRUE,
      exact = FALSE
    )
    
    tibble(
      Criterio = criterio,
      V = unname(prueba$statistic),
      p = prueba$p.value
    )
  }
)


# ============================================================
# CORRECCIÓN DE HOLM
# ============================================================

resultados_wilcoxon <- resultados_wilcoxon %>%
  mutate(
    p_Holm = p.adjust(
      p,
      method = "holm"
    )
  )


resultados_wilcoxon
# ============================================================
#  TAMAÑO DEL EFECTO r
# ============================================================

resultados_efecto <- map_dfr(
  criterios,
  function(criterio) {
    
    pre <- datos_criterios[[paste0("Pre_", criterio)]]
    post <- datos_criterios[[paste0("Post_", criterio)]]
    
    temp <- tibble(
      ID = datos_criterios$ID,
      Pre = pre,
      Post = post
    ) %>%
      drop_na(Pre, Post) %>%
      pivot_longer(
        cols = c(Pre, Post),
        names_to = "Momento",
        values_to = "Puntuacion"
      )
    
    efecto <- temp %>%
      wilcox_effsize(
        Puntuacion ~ Momento,
        paired = TRUE
      )
    
    tibble(
      Criterio = criterio,
      r = efecto$effsize
    )
  }
)

resultados_efecto

# ============================================================
# DISTRIBUCIÓN DE NIVELES 0, 1 Y 2
# ============================================================

datos_niveles <- datos_criterios %>%
  select(
    ID,
    starts_with("Pre"),
    starts_with("Post")
  ) %>%
  pivot_longer(
    cols = -ID,
    names_to = c("Momento", "Tarea", "Criterio"),
    names_pattern = "(Pre|Post)([0-9]+)_(.*)",
    values_to = "Nivel"
  )


# Frecuencias
niveles_resumen <- datos_niveles %>%
  group_by(Criterio, Momento, Nivel) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(Criterio, Momento) %>%
  mutate(
    Porcentaje = n / sum(n) * 100
  )

niveles_resumen

# ============================================================
# GRÁFICA DE DISTRIBUCIÓN DE NIVELES
# ============================================================

grafico_niveles <- ggplot(
  niveles_resumen,
  aes(
    x = Momento,
    y = Porcentaje,
    fill = factor(Nivel)
  )
) +
  
  geom_col(
    width = 0.65
  ) +
  
  facet_wrap(
    ~ Criterio,
    ncol = 2
  ) +
  
  labs(
    title = "Distribución de los niveles de desempeño por criterio",
    x = NULL,
    y = "Porcentaje de evaluaciones",
    fill = "Nivel"
  ) +
  
  theme_classic(
    base_size = 13
  )

grafico_niveles

# ============================================================
#  GRÁFICA PRE VS POST POR CRITERIO
# ============================================================

datos_grafica <- resultados_criterios %>%
  select(
    Criterio,
    Media_Pre,
    Media_Post
  ) %>%
  pivot_longer(
    cols = c(Media_Pre, Media_Post),
    names_to = "Momento",
    values_to = "Media"
  ) %>%
  mutate(
    Momento = recode(
      Momento,
      "Media_Pre" = "Preintervención",
      "Media_Post" = "Postintervención"
    )
  )


grafico_criterios <- ggplot(
  datos_grafica,
  aes(
    x = Criterio,
    y = Media,
    group = Momento
  )
) +
  
  geom_point(
    size = 3
  ) +
  
  geom_line(
    linewidth = 0.8
  ) +
  
  labs(
    title = "Desempeño por criterio de la rúbrica",
    x = NULL,
    y = "Puntuación media (0–2)"
  ) +
  
  theme_classic(
    base_size = 13
  )

grafico_criterios

# ============================================================
# EVOLUCIÓN DE LAS SEIS TAREAS
# CON IC 95 %
# ============================================================

datos_tareas <- datos_criterios %>%
  select(
    ID,
    Pre1_Citas:Pre3_Formato,
    Post1_Citas:Post3_Formato
  ) %>%
  pivot_longer(
    cols = -ID,
    names_to = c("Momento", "Tarea", "Criterio"),
    names_pattern = "(Pre|Post)([0-9]+)_(.*)",
    values_to = "Nivel"
  ) %>%
  group_by(ID, Momento, Tarea) %>%
  summarise(
    Score = sum(Nivel, na.rm = TRUE),
    .groups = "drop"
  )


estadisticos_tarea <- datos_tareas %>%
  group_by(Momento, Tarea) %>%
  summarise(
    n = n(),
    Media = mean(Score),
    DE = sd(Score),
    EE = DE / sqrt(n),
    
    IC95_inf =
      Media - qt(
        0.975,
        df = n - 1
      ) * EE,
    
    IC95_sup =
      Media + qt(
        0.975,
        df = n - 1
      ) * EE,
    
    .groups = "drop"
  ) %>%
  mutate(
    Evaluacion = paste0(
      Momento,
      Tarea
    )
  )


# Orden correcto
estadisticos_tarea$Evaluacion <- factor(
  estadisticos_tarea$Evaluacion,
  levels = c(
    "Pre1",
    "Pre2",
    "Pre3",
    "Post1",
    "Post2",
    "Post3"
  )
)


grafico_temporal_IC <- ggplot(
  estadisticos_tarea,
  aes(
    x = Evaluacion,
    y = Media,
    group = 1
  )
) +
  
  geom_line(
    linewidth = 0.8
  ) +
  
  geom_point(
    size = 3
  ) +
  
  geom_errorbar(
    aes(
      ymin = IC95_inf,
      ymax = IC95_sup
    ),
    width = 0.10
  ) +
  
  labs(
    title = "Evolución del desempeño a través de las seis tareas",
    x = "Evaluación",
    y = "Puntuación media (0–10)"
  ) +
  
  theme_classic(
    base_size = 13
  )

grafico_temporal_IC

#############################################
datos_grafica <- tabla_criterios %>%
  select(
    Criterio,
    Media_Pre,
    Media_Post
  ) %>%
  pivot_longer(
    cols = c(Media_Pre, Media_Post),
    names_to = "Momento",
    values_to = "Media"
  ) %>%
  mutate(
    Momento = recode(
      Momento,
      "Media_Pre" = "Preintervención",
      "Media_Post" = "Postintervención"
    )
  )
grafico_criterios <- ggplot(
  datos_grafica,
  aes(
    x = Criterio,
    y = Media,
    group = Momento,
    shape = Momento
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 3
  ) +
  labs(
    title = "Desempeño por criterio de la rúbrica",
    x = NULL,
    y = "Puntuación media (0–2)",
    shape = "Momento"
  ) +
  theme_classic(
    base_size = 13
  )

grafico_criterios

##################################################################
# Muñoz Hernández Shaday: todos los criterios de Pre2 = 0
datos_criterios <- datos_criterios %>%
  mutate(
    Pre2_Citas = ifelse(
      ID == limpiar_nombre("Muñoz Hernández Shaday"),
      0, Pre2_Citas
    ),
    Pre2_Referencias = ifelse(
      ID == limpiar_nombre("Muñoz Hernández Shaday"),
      0, Pre2_Referencias
    ),
    Pre2_Tipo = ifelse(
      ID == limpiar_nombre("Muñoz Hernández Shaday"),
      0, Pre2_Tipo
    ),
    Pre2_Correspondencia = ifelse(
      ID == limpiar_nombre("Muñoz Hernández Shaday"),
      0, Pre2_Correspondencia
    ),
    Pre2_Formato = ifelse(
      ID == limpiar_nombre("Muñoz Hernández Shaday"),
      0, Pre2_Formato
    )
  )

# Bautista Alejandro Uriel: Pre2_Formato = 0
datos_criterios <- datos_criterios %>%
  mutate(
    Pre2_Formato = ifelse(
      ID == limpiar_nombre("Bautista Alejandro Uriel"),
      0, Pre2_Formato
    )
  )
########################################################
# Muñoz Hernández Shaday: todos los criterios de Pre2 = 0
datos_criterios <- datos_criterios %>%
  mutate(
    Pre2_Citas = ifelse(
      ID == limpiar_nombre("Muñoz Hernández Shaday"),
      0, Pre2_Citas
    ),
    Pre2_Referencias = ifelse(
      ID == limpiar_nombre("Muñoz Hernández Shaday"),
      0, Pre2_Referencias
    ),
    Pre2_Tipo = ifelse(
      ID == limpiar_nombre("Muñoz Hernández Shaday"),
      0, Pre2_Tipo
    ),
    Pre2_Correspondencia = ifelse(
      ID == limpiar_nombre("Muñoz Hernández Shaday"),
      0, Pre2_Correspondencia
    ),
    Pre2_Formato = ifelse(
      ID == limpiar_nombre("Muñoz Hernández Shaday"),
      0, Pre2_Formato
    )
  )

# Bautista Alejandro Uriel: Pre2_Formato = 0
datos_criterios <- datos_criterios %>%
  mutate(
    Pre2_Formato = ifelse(
      ID == limpiar_nombre("Bautista Alejandro Uriel"),
      0, Pre2_Formato
    )
  )
#########################################################
criterios <- c(
  "Citas",
  "Referencias",
  "Tipo",
  "Correspondencia",
  "Formato"
)

for (criterio in criterios) {
  
  datos_criterios[[paste0("Pre_", criterio)]] <-
    rowMeans(
      datos_criterios[
        ,
        paste0("Pre", 1:3, "_", criterio)
      ],
      na.rm = TRUE
    )
  
  datos_criterios[[paste0("Post_", criterio)]] <-
    rowMeans(
      datos_criterios[
        ,
        paste0("Post", 1:3, "_", criterio)
      ],
      na.rm = TRUE
    )
  
  datos_criterios[[paste0("Cambio_", criterio)]] <-
    datos_criterios[[paste0("Post_", criterio)]] -
    datos_criterios[[paste0("Pre_", criterio)]]
}
######################################################
tabla_criterios <- map_dfr(
  criterios,
  function(criterio) {
    
    pre <- datos_criterios[[paste0("Pre_", criterio)]]
    post <- datos_criterios[[paste0("Post_", criterio)]]
    
    tibble(
      Criterio = criterio,
      
      Media_Pre = mean(pre, na.rm = TRUE),
      Mediana_Pre = median(pre, na.rm = TRUE),
      DE_Pre = sd(pre, na.rm = TRUE),
      IQR_Pre = IQR(pre, na.rm = TRUE),
      
      Media_Post = mean(post, na.rm = TRUE),
      Mediana_Post = median(post, na.rm = TRUE),
      DE_Post = sd(post, na.rm = TRUE),
      IQR_Post = IQR(post, na.rm = TRUE),
      
      Cambio_Medio = mean(post - pre, na.rm = TRUE),
      Cambio_Mediano = median(post - pre, na.rm = TRUE)
    )
  }
)

tabla_criterios
###########################################################3
resultados_wilcoxon <- map_dfr(
  criterios,
  function(criterio) {
    
    pre <- datos_criterios[[paste0("Pre_", criterio)]]
    post <- datos_criterios[[paste0("Post_", criterio)]]
    
    prueba <- wilcox.test(
      post,
      pre,
      paired = TRUE,
      exact = FALSE
    )
    
    tibble(
      Criterio = criterio,
      V = unname(prueba$statistic),
      p = prueba$p.value
    )
  }
)

resultados_wilcoxon <- resultados_wilcoxon %>%
  mutate(
    p_Holm = p.adjust(
      p,
      method = "holm"
    )
  )

resultados_wilcoxon
###################################################333333333333
resultados_efecto <- map_dfr(
  criterios,
  function(criterio) {
    
    pre <- datos_criterios[[paste0("Pre_", criterio)]]
    post <- datos_criterios[[paste0("Post_", criterio)]]
    
    temp <- tibble(
      ID = datos_criterios$ID,
      Pre = pre,
      Post = post
    ) %>%
      drop_na(Pre, Post) %>%
      pivot_longer(
        cols = c(Pre, Post),
        names_to = "Momento",
        values_to = "Puntuacion"
      )
    
    efecto <- temp %>%
      wilcox_effsize(
        Puntuacion ~ Momento,
        paired = TRUE
      )
    
    tibble(
      Criterio = criterio,
      r = efecto$effsize
    )
  }
)

resultados_efecto
# ============================================================
#ANALÍSIS DE DISTRIBUCION POR NIVELES 
# ============================================================
datos_niveles <- datos_criterios %>%
  select(
    ID,
    starts_with("Pre"),
    starts_with("Post")
  ) %>%
  pivot_longer(
    cols = -ID,
    names_to = c("Momento", "Tarea", "Criterio"),
    names_pattern = "(Pre|Post)([0-9]+)_(.*)",
    values_to = "Nivel"
  )
niveles_resumen <- datos_niveles %>%
  group_by(Criterio, Momento, Nivel) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(Criterio, Momento) %>%
  mutate(
    Porcentaje = n / sum(n) * 100
  ) %>%
  ungroup()
print(niveles_resumen, n = Inf)

print(niveles_resumen, n = Inf)

#CORREGIR 

datos_niveles <- datos_criterios %>%
  select(
    ID,
    Pre1_Citas:Pre3_Formato,
    Post1_Citas:Post3_Formato
  ) %>%
  pivot_longer(
    cols = -ID,
    names_to = c("Momento", "Tarea", "Criterio"),
    names_pattern = "(Pre|Post)([0-9]+)_(.*)",
    values_to = "Nivel"
  )
niveles_resumen <- datos_niveles %>%
  group_by(Criterio, Momento, Nivel) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(Criterio, Momento) %>%
  mutate(
    Porcentaje = n / sum(n) * 100
  ) %>%
  ungroup()
print(niveles_resumen, n = Inf)

niveles_resumen_tarea <- datos_niveles %>%
  group_by(Criterio, Momento, Tarea, Nivel) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(Criterio, Momento, Tarea) %>%
  mutate(
    Porcentaje = n / sum(n) * 100
  ) %>%
  ungroup()

print(niveles_resumen_tarea, n = Inf)

niveles_resumen_tarea <- datos_niveles %>%
  mutate(
    Nivel = factor(Nivel, levels = c(0, 1, 2))
  ) %>%
  count(Criterio, Momento, Tarea, Nivel, .drop = FALSE) %>%
  group_by(Criterio, Momento, Tarea) %>%
  mutate(
    Porcentaje = n / sum(n) * 100
  ) %>%
  ungroup()

print(niveles_resumen_tarea, n = Inf)

###############################################
#GRÁFICA
###############################################
library(ggplot2)
library(dplyr)

grafico_niveles <- niveles_resumen_tarea %>%
  mutate(
    Tarea = factor(
      Tarea,
      levels = c("1", "2", "3"),
      labels = c("1", "2", "3")
    ),
    Momento = factor(
      Momento,
      levels = c("Pre", "Post")
    ),
    Evaluacion = case_when(
      Momento == "Pre" & Tarea == "1" ~ "Pre 1",
      Momento == "Pre" & Tarea == "2" ~ "Pre 2",
      Momento == "Pre" & Tarea == "3" ~ "Pre 3",
      Momento == "Post" & Tarea == "1" ~ "Post 1",
      Momento == "Post" & Tarea == "2" ~ "Post 2",
      Momento == "Post" & Tarea == "3" ~ "Post 3"
    ),
    Evaluacion = factor(
      Evaluacion,
      levels = c("Pre 1", "Pre 2", "Pre 3",
                 "Post 1", "Post 2", "Post 3")
    ),
    Nivel = factor(
      Nivel,
      levels = c(0, 1, 2),
      labels = c("Nivel 0", "Nivel 1", "Nivel 2")
    )
  )

ggplot(
  grafico_niveles,
  aes(x = Evaluacion, y = Porcentaje, fill = Nivel)
) +
  geom_col(width = 0.75) +
  facet_wrap(~ Criterio, ncol = 1) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  labs(
    title = "Distribución de niveles de desempeño por criterio",
    x = NULL,
    y = "Estudiantes (%)",
    fill = "Nivel de desempeño"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
###############################################
#En inglés
##############################################
library(ggplot2)
library(dplyr)

# Prepare data for the figure
grafico_niveles_articulo <- niveles_resumen_tarea %>%
  mutate(
    
    # Create evaluation labels
    Evaluacion = case_when(
      Momento == "Pre" & Tarea == "1" ~ "Pre 1",
      Momento == "Pre" & Tarea == "2" ~ "Pre 2",
      Momento == "Pre" & Tarea == "3" ~ "Pre 3",
      Momento == "Post" & Tarea == "1" ~ "Post 1",
      Momento == "Post" & Tarea == "2" ~ "Post 2",
      Momento == "Post" & Tarea == "3" ~ "Post 3"
    ),
    
    # Keep evaluations in chronological order
    Evaluacion = factor(
      Evaluacion,
      levels = c(
        "Pre 1", "Pre 2", "Pre 3",
        "Post 1", "Post 2", "Post 3"
      )
    ),
    
    # Translate rubric levels
    Nivel = factor(
      Nivel,
      levels = c(0, 1, 2),
      labels = c(
        "Level 0",
        "Level 1",
        "Level 2"
      )
    ),
    
    # Translate rubric criteria
    Criterio = recode(
      Criterio,
      "Citas" = "In-text citations",
      "Referencias" = "Reference list",
      "Tipo" = "Literature type",
      "Correspondencia" = "Citation–reference correspondence",
      "Formato" = "Format and technical consistency"
    )
  )

# Generate figure
ggplot(
  grafico_niveles_articulo,
  aes(
    x = Evaluacion,
    y = Porcentaje,
    fill = Nivel
  )
) +
  
  # 100% stacked bars
  geom_col(
    width = 0.72,
    color = "white",
    linewidth = 0.2
  ) +
  
  # One panel per rubric criterion
  facet_wrap(
    ~ Criterio,
    ncol = 1
  ) +
  
  # Grayscale levels
  scale_fill_manual(
    values = c(
      "Level 0" = "#F76CEE",
      "Level 1" = "#F5A91E",
      "Level 2" = "#15B0E4"
    )
  ) +
  
  # Y-axis
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = c(0, 0)
  ) +
  
  # English labels
  labs(
    title = "Distribution of Performance Levels by Rubric Criterion",
    x = NULL,
    y = "Students (%)",
    fill = "Performance level"
  ) +
  
  # Clean scientific style
  theme_classic(base_size = 12) +
  
  theme(
    
    # Center the main title
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    
    # Facet titles
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    
    # X-axis labels
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 10
    ),
    
    # Y-axis
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11),
    
    # Legend
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    
    # Spacing between panels
    panel.spacing = unit(
      0.8,
      "lines"
    ),
    
    # Figure margins
    plot.margin = margin(
      10, 10, 10, 10
    )
  )
###############################################################

datos_grafica_criterios <- tabla_criterios %>%
  select(
    Criterio,
    Media_Pre,
    Media_Post
  ) %>%
  pivot_longer(
    cols = c(Media_Pre, Media_Post),
    names_to = "Momento",
    values_to = "Media"
  ) %>%
  mutate(
    Momento = recode(
      Momento,
      "Media_Pre" = "Pre-intervention",
      "Media_Post" = "Post-intervention"
    ),
    Criterio = recode(
      Criterio,
      "Citas" = "In-text citations",
      "Referencias" = "Reference list",
      "Tipo" = "Literature type",
      "Correspondencia" = "Citation–reference correspondence",
      "Formato" = "Format and technical consistency"
    )
  )
grafico_criterios <- ggplot(
  datos_grafica_criterios,
  aes(
    x = Criterio,
    y = Media,
    shape = Momento
  )
) +
  geom_point(
    size = 3.5,
    position = position_dodge(width = 0.4)
  ) +
  scale_y_continuous(
    limits = c(0, 2),
    breaks = seq(0, 2, 0.5),
    expand = c(0, 0.05)
  ) +
  labs(
    title = "Mean Performance by Rubric Criterion",
    x = NULL,
    y = "Mean score (0–2)",
    shape = "Assessment"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    axis.text.x = element_text(
      angle = 35,
      hjust = 1
    ),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

grafico_criterios

#CORREGIDO
library(ggplot2)
library(dplyr)

# Prepare data for the figure
datos_grafica_criterios <- tabla_criterios %>%
  select(
    Criterio,
    Media_Pre,
    Media_Post
  ) %>%
  pivot_longer(
    cols = c(Media_Pre, Media_Post),
    names_to = "Momento",
    values_to = "Media"
  ) %>%
  mutate(
    
    # Translate assessment labels
    Momento = recode(
      Momento,
      "Media_Pre" = "Pre-intervention",
      "Media_Post" = "Post-intervention"
    ),
    
    # Translate rubric criteria
    Criterio = recode(
      Criterio,
      "Citas" = "In-text citations",
      "Referencias" = "Reference list",
      "Tipo" = "Literature type",
      "Correspondencia" = "Citation–reference correspondence",
      "Formato" = "Format and technical consistency"
    )
  )

# Generate figure
grafico_criterios <- ggplot(
  datos_grafica_criterios,
  aes(
    x = Criterio,
    y = Media,
    shape = Momento
  )
) +
  
  # Mean points
  geom_point(
    size = 3.5,
    position = position_dodge(width = 0.4)
  ) +
  
  # Y-axis
  scale_y_continuous(
    limits = c(0, 2),
    breaks = seq(0, 2, 0.5),
    expand = c(0, 0.05)
  ) +
  
  # English labels
  labs(
    title = "Mean Performance by Rubric Criterion",
    x = NULL,
    y = "Mean score (0–2)",
    shape = "Assessment"
  ) +
  
  # Clean scientific theme
  theme_classic(base_size = 12) +
  
  theme(
    
    # Center title
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    
    # Add subtle grid
    panel.grid.major = element_line(
      linewidth = 0.3,
      color = "grey85"
    ),
    
    panel.grid.minor = element_line(
      linewidth = 0.15,
      color = "grey92"
    ),
    
    # Keep axes visible
    axis.line = element_line(
      linewidth = 0.5
    ),
    
    # X-axis labels
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      size = 10
    ),
    
    # Y-axis
    axis.text.y = element_text(
      size = 10
    ),
    
    axis.title.y = element_text(
      size = 11
    ),
    
    # Legend
    legend.position = "bottom",
    legend.title = element_text(
      face = "bold"
    ),
    
    # Margins
    plot.margin = margin(
      10, 10, 10, 10
    )
  )

grafico_criterios

###################
#ANÁLISIS INDIVIDUAL
####################
names(datos)

head(datos)

tabla_individual <- datos %>%
  select(
    Estudiante,
    Pre1, Pre2, Pre3,
    Post1, Post2, Post3,
    Pre, Post, Cambio,
    Clasificacion, Porcentaje_Mejora
  )

print(tabla_individual, n = Inf)

table(datos$Clasificacion)

prop.table(table(datos$Clasificacion)) * 100

summary(datos$Cambio)

View(tabla_individual)

############################
#GRÁFICO (ESTE NO USAR)
############################
library(ggplot2)
library(dplyr)
library(tidyr)

# Convert individual scores to long format
datos_trayectorias <- datos %>%
  select(
    Estudiante,
    Pre1, Pre2, Pre3,
    Post1, Post2, Post3
  ) %>%
  pivot_longer(
    cols = c(
      Pre1, Pre2, Pre3,
      Post1, Post2, Post3
    ),
    names_to = "Evaluacion",
    values_to = "Puntuacion"
  ) %>%
  mutate(
    Evaluacion = factor(
      Evaluacion,
      levels = c(
        "Pre1", "Pre2", "Pre3",
        "Post1", "Post2", "Post3"
      ),
      labels = c(
        "Pre 1", "Pre 2", "Pre 3",
        "Post 1", "Post 2", "Post 3"
      )
    )
  )

# Calculate group means
medias_trayectoria <- datos_trayectorias %>%
  group_by(Evaluacion) %>%
  summarise(
    Media = mean(Puntuacion, na.rm = TRUE),
    .groups = "drop"
  )

# Generate individual trajectory plot
grafico_trayectorias <- ggplot(
  datos_trayectorias,
  aes(
    x = Evaluacion,
    y = Puntuacion,
    group = Estudiante
  )
) +
  
  # Individual trajectories
  geom_line(
    alpha = 0.30,
    linewidth = 0.5
  ) +
  
  # Individual observations
  geom_point(
    alpha = 0.30,
    size = 1.5
  ) +
  
  # Group mean trajectory
  geom_line(
    data = medias_trayectoria,
    aes(
      x = Evaluacion,
      y = Media,
      group = 1
    ),
    inherit.aes = FALSE,
    linewidth = 1.3
  ) +
  
  # Group mean points
  geom_point(
    data = medias_trayectoria,
    aes(
      x = Evaluacion,
      y = Media
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  
  # Y-axis
  scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, 2),
    expand = c(0, 0.02)
  ) +
  
  # Labels
  labs(
    title = "Individual Performance Trajectories Across Assessments",
    x = "Assessment",
    y = "Total score (0–10)"
  ) +
  
  # Scientific style
  theme_classic(base_size = 12) +
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 14
    ),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    axis.title = element_text(
      size = 11
    ),
    
    # Grid
    panel.grid.major = element_line(
      linewidth = 0.3,
      color = "grey85"
    ),
    
    panel.grid.minor = element_line(
      linewidth = 0.15,
      color = "grey92"
    ),
    
    axis.line = element_line(
      linewidth = 0.5
    ),
    
    plot.margin = margin(
      10, 10, 10, 10
    )
  )

grafico_trayectorias

###########################
# Tabla de cambio individual
tabla_individual_articulo <- datos %>%
  select(
    Estudiante,
    Pre,
    Post,
    Cambio,
    Clasificacion
  ) %>%
  arrange(desc(Cambio))

tabla_individual_articulo

resumen_cambio_individual <- datos %>%
  summarise(
    n = n(),
    Mejoraron = sum(Clasificacion == "Mejoró"),
    Sin_cambio = sum(Clasificacion == "Sin cambio"),
    Empeoraron = sum(Clasificacion == "Empeoró"),
    
    Porcentaje_mejoraron = mean(Clasificacion == "Mejoró") * 100,
    Porcentaje_sin_cambio = mean(Clasificacion == "Sin cambio") * 100,
    Porcentaje_empeoraron = mean(Clasificacion == "Empeoró") * 100,
    
    Cambio_media = mean(Cambio, na.rm = TRUE),
    Cambio_mediana = median(Cambio, na.rm = TRUE),
    Cambio_SD = sd(Cambio, na.rm = TRUE),
    Cambio_IQR = IQR(Cambio, na.rm = TRUE),
    Cambio_min = min(Cambio, na.rm = TRUE),
    Cambio_max = max(Cambio, na.rm = TRUE)
  )

resumen_cambio_individual

resumen_baseline <- datos %>%
  summarise(
    n = n(),
    Pre_cero = sum(Pre == 0, na.rm = TRUE),
    Porcentaje_Pre_cero = mean(Pre == 0, na.rm = TRUE) * 100
  )

resumen_baseline

#######################################################
#MÁS ANÁLISIS
#######################################################

descriptivos_evaluacion <- datos %>%
  select(Pre1, Pre2, Pre3, Post1, Post2, Post3) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Evaluacion",
    values_to = "Puntuacion"
  ) %>%
  group_by(Evaluacion) %>%
  summarise(
    n = n(),
    Media = mean(Puntuacion, na.rm = TRUE),
    Mediana = median(Puntuacion, na.rm = TRUE),
    SD = sd(Puntuacion, na.rm = TRUE),
    IQR = IQR(Puntuacion, na.rm = TRUE),
    Min = min(Puntuacion, na.rm = TRUE),
    Max = max(Puntuacion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Evaluacion = factor(
      Evaluacion,
      levels = c("Pre1", "Pre2", "Pre3", "Post1", "Post2", "Post3")
    )
  )

descriptivos_evaluacion

datos_friedman <- datos %>%
  select(Estudiante, Pre1, Pre2, Pre3, Post1, Post2, Post3) %>%
  pivot_longer(
    cols = -Estudiante,
    names_to = "Evaluacion",
    values_to = "Puntuacion"
  )

friedman_test(
  datos_friedman,
  Puntuacion ~ Evaluacion | Estudiante
)

comparaciones_pares <- datos_friedman %>%
  pairwise_wilcox_test(
    Puntuacion ~ Evaluacion,
    paired = TRUE,
    p.adjust.method = "holm"
  )

comparaciones_pares

descriptivos_evaluacion

friedman_test(
  datos_friedman,
  Puntuacion ~ Evaluacion | Estudiante
)

#########################################################
#TAMAÑO DEL EFECTO
#########################################################

efectos_pre_post <- datos %>%
  select(Estudiante, Pre1, Pre2, Pre3, Post1, Post2, Post3) %>%
  pivot_longer(
    cols = -Estudiante,
    names_to = "Evaluacion",
    values_to = "Puntuacion"
  ) %>%
  mutate(
    Momento = ifelse(grepl("^Pre", Evaluacion), "Pre", "Post"),
    Numero = gsub("Pre|Post", "", Evaluacion)
  ) %>%
  select(Estudiante, Numero, Momento, Puntuacion) %>%
  pivot_wider(
    names_from = Momento,
    values_from = Puntuacion
  ) %>%
  group_by(Numero) %>%
  summarise(
    n = n(),
    Cambio_media = mean(Post - Pre, na.rm = TRUE),
    Cambio_mediana = median(Post - Pre, na.rm = TRUE),
    SD_cambio = sd(Post - Pre, na.rm = TRUE),
    .groups = "drop"
  )

efectos_pre_post

efectos_pre_post_r <- datos %>%
  select(Estudiante, Pre1, Pre2, Pre3, Post1, Post2, Post3) %>%
  pivot_longer(
    cols = -Estudiante,
    names_to = "Evaluacion",
    values_to = "Puntuacion"
  ) %>%
  mutate(
    Momento = ifelse(grepl("^Pre", Evaluacion), "Pre", "Post"),
    Numero = gsub("Pre|Post", "", Evaluacion)
  ) %>%
  select(Estudiante, Numero, Momento, Puntuacion) %>%
  pivot_wider(
    names_from = Momento,
    values_from = Puntuacion
  ) %>%
  group_by(Numero) %>%
  wilcox_test(
    Post ~ Pre,
    paired = TRUE
  ) %>%
  add_significance() %>%
  ungroup()

efectos_pre_post_r

wilcox_1 <- wilcox.test(
  datos$Post1,
  datos$Pre1,
  paired = TRUE,
  exact = FALSE
)

wilcox_1

wilcox_2 <- wilcox.test(
  datos$Post2,
  datos$Pre2,
  paired = TRUE,
  exact = FALSE
)

wilcox_2

wilcox_3 <- wilcox.test(
  datos$Post3,
  datos$Pre3,
  paired = TRUE,
  exact = FALSE
)

wilcox_3

# Función para calcular r a partir del estadístico V de Wilcoxon
calcular_r <- function(V, n, p) {
  
  z <- qnorm(p / 2, lower.tail = FALSE)
  
  r <- z / sqrt(n)
  
  return(r)
}

r_1 <- calcular_r(145.5, 39, wilcox_1$p.value)
r_2 <- calcular_r(701, 39, wilcox_2$p.value)
r_3 <- calcular_r(780, 39, wilcox_3$p.value)

tabla_efectos_temporales <- data.frame(
  Comparacion = c(
    "Pre 1 vs Post 1",
    "Pre 2 vs Post 2",
    "Pre 3 vs Post 3"
  ),
  r = c(r_1, r_2, r_3)
)

tabla_efectos_temporales

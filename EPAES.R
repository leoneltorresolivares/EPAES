# EPAES

rm(list = ls())


ruta = "C:/Users/19990772/Desktop/Estudios/EPAES"


setwd(ruta)


library(ggplot2)
library(dplyr)
library(scales)
library(MASS)
library(gridExtra) #Grilla para varios gráficos en uno
library(Metrics) # Metricas
library(openxlsx)
library(readxl)


set.seed(1234)


df = tibble(read_excel("epaes_v2.xlsx"))


# Arreglar la caga de docencia -------


df_2024 = tibble(read_excel("EPAES 2024.xlsx"))


df = df %>% filter(AÑO == 2024)


unidos = merge(df, df_2024, by = "RUT") # cruzo todo, estamos ok con los datos


nombre_df = names(df)


nombres_df_2024 = names(df_2024)


setdiff(nombre_df, nombres_df_2024)


nombres_df_2024 %in% nombre_df # No estan las preguntas de facultad, sede, puras weas


unique(df_2024$`(1) Me considero una persona alegre.`)


df_2024 = df_2024 %>% 
  mutate(across(starts_with("("), ~ case_when(
    .x == "[ 1 ] En desacuerdo" ~ 1,
    .x == "[ 2 ] Parcialmente en desacuerdo" ~ 2,
    .x == "[ 3 ] Indeciso" ~ 3,
    .x == "[ 4 ] Parcialmente de acuerdo" ~ 4,
    .x == "[ 5 ] De acuerdo" ~ 5
  )))


nombres_df_2024 = names(df_2024)


nombres_df_2024 = nombres_df_2024[grepl("^\\(", nombres_df_2024)]


inicio = df[,1:4]


RUT = df_2024$RUT


df_2024_p1 = dplyr::select(df_2024 ,starts_with("(")) # No pesco el paquete, por eso los ::


df_2024_p1 = cbind(RUT, df_2024_p1)


df2 = left_join(inicio, df_2024_p1, by = "RUT")


RUT = df$RUT


caract = df[,72:75]


caract = cbind(RUT, caract)


df2 = left_join(df2, caract, by = "RUT")


RUT = df_2024$RUT


final = df_2024[,75:ncol(df_2024)]


final = cbind(RUT, final)


df2 = df2 %>% mutate(`Promedio Autodeterminación personal` = rowMeans(across(c(`(7) Si tengo dificultades para alcanzar una meta, no me doy por vencido.`,
                                                                               `(15) No me desanimo frente a los obstáculos.`,
                                                                               `(17) Si tengo problemas para alcanzar una meta, cambio mi estrategia.`,
                                                                               `(28) Cuando tengo un problema, dedico el tiempo y el esfuerzo necesarios para resolverlo.`,
                                                                               `(31) Cuando estoy en desacuerdo con lo planteado por alguien, soy capaz de decírselo.`,
                                                                               `(35) Siempre termino lo que comienzo.`,
                                                                               `(37) Cuando tengo una duda, no descanso hasta encontrar la respuesta.`,
                                                                               `(39) Soy capaz de saber qué hacer para lograr lo que me propongo.`,
                                                                               `(53) Cuando intento resolver un problema, analizo las posibles soluciones y luego escojo la mejor.`,
                                                                               `(56) Tengo claro cuáles son mis principales fortalezas y mis debilidades.`,
                                                                               `(62) Cuando me propongo un plan, lo llevo a cabo hasta el final.`,
                                                                               `(65) Cuando me fijo mis metas, pongo todo mi esfuerzo en lograrlas.`,
                                                                               `(67) Cuando tengo problemas, identifico fácilmente los pasos que debo seguir para resolverlos.`), ~ as.numeric(.x))))

df2 = df2 %>% mutate(
  `Rango Autodeterminación personal` = case_when(
    `Promedio Autodeterminación personal` < 2.5 ~ "Bajo",
    `Promedio Autodeterminación personal` >= 2.5 & `Promedio Autodeterminación personal` <= 3.5 ~ "Medio",
    `Promedio Autodeterminación personal` > 3.5 ~ "Alto"
  )
)


table(df$`Rango Autodeterminación personal`)


table(df_2024$`RANGO AUTODETERMINACIÓN PERSONAL`)


table(df2$`Rango Autodeterminación personal`)


df2 = left_join(df2, final, by = "RUT")


write.xlsx(df2, "epaes_2024_limpio.xlsx")


df = tibble(read_excel("EPAES.xlsx"))


names(df)[2] = "Carrera"


names(df)[4] = "RUT"


df_2024 = tibble(read_excel("epaes_2024_limpio.xlsx"))


names(df_2024) = names(df)


df = df %>% filter(AÑO != 2024)


df = rbind(df, df_2024)


write.xlsx(df, "epaes_v2.xlsx")


# Mini EDA --------------

str(df)


library(DataExplorer)


plot_intro(df)


sum(is.na(df))


sort(colSums(is.na(df)), decreasing = TRUE)


df2 = df %>% filter(AÑO == 2024)


sum(is.na(df2))


na_counts <- colSums(is.na(df))


na_df <- data.frame(
  variable = names(na_counts),
  n_na = as.numeric(na_counts)
)


na_df = na_df %>% filter(n_na > 0)


# Ordenar
na_df <- na_df[order(-na_df$n_na), ]

ggplot(na_df, aes(x = reorder(variable, n_na), y = n_na, fill = n_na)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # para que las etiquetas sean legibles
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  labs(
    title = "Cantidad de valores NA por variable",
    x = "Variable",
    y = "Cantidad de NA"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 10)
  )

df_sin_na = na.omit(df)

# Instrucciones ---------------

# hay que construir las dimensiones, por weon, redy

# pelear con docencia (con palo y hacha), que preguntas van a cada dimensión, datos EXACTOS (numerico), redy

# EDA

# join con las bases de deserción (prioridad)

# join con la base ENCE (aqui tenemos el rendimiento) opcional

# dos data set porque en join ence deserción hay muy pocos

# caracterizar (estandar, buscar indicador para variables categoricas) 

# cluster para los grupos (h0: se agruparan los abandonos y se separan de los que siguen)

# cuando cague esa wea, k = 6

# predecir la pbb de desercición/abandono estand by


# Imputaciones por moda por carrera ------------------


sort(unique(df$Carrera)) # Careeras doble sede diferenciadas en el nombre


sum(is.na(df$Carrera)) # Podemos hacer imputacion por carrera


sum(is.na(df$`Tengo claro lo que quiero hacer cuando termine mis estudios.`)) # Muchos NA


get_mode = function(x){
  ux = na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}


columnas_con_na = na_df$variable


df2 = df %>% group_by(Carrera) %>% 
  mutate(across(all_of(columnas_con_na),
                 ~ ifelse(is.na(.x), get_mode(.x), .x))) %>% 
  ungroup()


sum(is.na(df2)) # Wiii


# Queda muy cargado al tener categorias muy dominantes

hist(df$`Tengo claro lo que quiero hacer cuando termine mis estudios.`)

table(df$`Tengo claro lo que quiero hacer cuando termine mis estudios.`)

prop.table(table(df$`Tengo claro lo que quiero hacer cuando termine mis estudios.`))

hist(df2$`Tengo claro lo que quiero hacer cuando termine mis estudios.`)

table(df2$`Tengo claro lo que quiero hacer cuando termine mis estudios.`)

prop.table(table(df2$`Tengo claro lo que quiero hacer cuando termine mis estudios.`))



# Imputaciones por proporción ----------------


na_counts <- colSums(is.na(df))


na_df <- data.frame(
  variable = names(na_counts),
  n_na = as.numeric(na_counts)
)


na_df = na_df %>% filter(n_na > 0)


# Función para imputar categóricas respetando proporciones
impute_by_prop <- function(x) {
  nas <- is.na(x)
  if (sum(!nas) == 0) return(x)  # Si todos son NA, no se puede imputar
  probs <- prop.table(table(x[!nas]))
  x[nas] <- sample(names(probs), sum(nas), replace = TRUE, prob = probs)
  return(x)
}


cols_a_imputar = na_df$variable

# Agregar Año tmb, pero hay que modificar cuando no hay datos
df_imputado = df %>%
  group_by(Carrera) %>%
  mutate(across(all_of(cols_a_imputar), impute_by_prop)) %>%
  ungroup()


# Antes
table(df$`Tengo claro lo que quiero hacer cuando termine mis estudios.`, useNA = "ifany")

prop.table(table(df$`Tengo claro lo que quiero hacer cuando termine mis estudios.`))

# Después
table(df_imputado$`Tengo claro lo que quiero hacer cuando termine mis estudios.`)

prop.table(table(df_imputado$`Tengo claro lo que quiero hacer cuando termine mis estudios.`))


# Dimensiones ---------------

# Podemos reconstruir, intentar acercarse lo más posible al valor "real" (no tienen el real, esta sin decimales, mirar los documentos)
# ver si simulando menos datos mejora

df_imputado2 = df_imputado %>% mutate(`Promedio anticipación analítica` = rowMeans(across(c(`Antes de resolver un problema, analizo varias soluciones posibles.`,
                                                                                            `Soy sensible y respeto los sentimientos de las personas.`,
                                                                                            `Cuando hablo con alguien, por lo general soy capaz de respetar su punto de vista.`,
                                                                                            `Antes de responder una prueba, analizo cada pregunta.`,
                                                                                            `Antes de tomar una decisión busco información que me pueda servir.`,
                                                                                            `Cuando leo un texto, soy capaz de darme cuenta de qué está ‘hablando’ el autor.`),
                                                                                          ~ as.numeric(.x))))

df_imputado2 = df_imputado2 %>% mutate(
  `Rango anticipación analítica` = case_when(
    `Promedio anticipación analítica` < 2.5 ~ "Bajo",
    `Promedio anticipación analítica` >= 2.5 & `Promedio anticipación analítica` <= 3.5 ~ "Medio",
    `Promedio anticipación analítica` > 3.5 ~ "Alto"
  )
)


df_imputado2 = df_imputado2 %>% mutate(`Promedio Autodeterminación Personal` = rowMeans(across(c(`Si tengo dificultades para alcanzar una meta, no me doy por vencido.`,
                                                                                                `No me desanimo frente a los obstáculos.`,
                                                                                                `Si tengo problemas para alcanzar una meta, cambio mi estrategia.`,
                                                                                                `Cuando tengo un problema, dedico el tiempo y el esfuerzo necesarios para resolverlo.`,
                                                                                                `Cuando estoy en desacuerdo con lo planteado por alguien, soy capaz de decírselo.`,
                                                                                                `Siempre termino lo que comienzo.`,
                                                                                                `Cuando tengo una duda, no descanso hasta encontrar la respuesta.`,
                                                                                                  `Soy capaz de saber qué hacer para lograr lo que me propongo.`,
                                                                                                `Cuando intento resolver un problema, analizo las posibles soluciones y luego escojo la mejor.`,
                                                                                                `Tengo claro cuáles son mis principales fortalezas y mis debilidades.`,
                                                                                                `Cuando me propongo un plan, lo llevo a cabo hasta el final.`,
                                                                                                `Cuando me fijo mis metas, pongo todo mi esfuerzo en lograrlas.`,
                                                                                                `Cuando tengo problemas, identifico fácilmente los pasos que debo seguir para resolverlos.`),
                                                                                          ~ as.numeric(.x))))

df_imputado2 = df_imputado2 %>% mutate(
  `Rango Autodeterminación personal` = case_when(
    `Promedio Autodeterminación Personal` < 2.5 ~ "Bajo",
    `Promedio Autodeterminación Personal` >= 2.5 & `Promedio Autodeterminación Personal` <= 3.5 ~ "Medio",
    `Promedio Autodeterminación Personal` > 3.5 ~ "Alto"
  )
)


#df_imputado2 = df_imputado2 %>% mutate(`Promedio anticipación analítica` = ceiling(`Promedio anticipación analítica`))


table(df$`Rango anticipación analítica`)


table(df_imputado2$`Rango anticipación analítica`)


table(df$`Rango Autodeterminación personal`)


table(df_imputado2$`Rango Autodeterminación personal`)


sort(prop.table(table(df$`Promedio anticipación analítica`)))*100


sort(prop.table(table(df_imputado2$`Promedio anticipación analítica`)))*100


mean(df$`Promedio anticipación analítica`, na.rm = T)


mean(df_imputado2$`Promedio anticipación analítica`)







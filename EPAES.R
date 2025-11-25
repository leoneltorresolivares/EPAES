# EPAES

rm(list = ls())


ruta = "C:/Users/19990772/Desktop/Estudios/EPAES"


setwd(ruta)


library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(MASS)
library(gridExtra) #Grilla para varios gráficos en uno
library(Metrics) # Metricas
library(openxlsx)
library(readxl)
library(readr)


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



# Trabajar aca ------


set.seed(1234)


df = tibble(read_excel("epaes_v2.xlsx"))


# Mini EDA --------------

str(df)


library(DataExplorer)


plot_intro(df)


sum(is.na(df))


sort(colSums(is.na(df)), decreasing = TRUE)


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

# join con las bases de deserción (prioridad) redy

# join con la base ENCE (aqui tenemos el rendimiento) opcional

# dos data set porque en join ence deserción hay muy pocos opcional

# Join con notas

# test de hipotesis para quitar la wea de notas (promedio de los que desertaron mayor a los que se quedaron)

# correlaciones notas, facultad, carrera

# tasa de deserción por facultad

# test no paramterica para diferencia entre facultades

# caracterizar (estandar, buscar indicador para variables categoricas) aqui vamos

# cluster para los grupos (h0: se agruparan los abandonos y se separan de los que siguen)

# cuando cague esa wea, k = 6

# predecir la pbb de desercición/abandono estand by


# Imputaciones por moda por carrera ------------------


sort(unique(df$Carrera)) # Careeras doble sede diferenciadas en el nombre


sum(is.na(df$Carrera)) # Podemos hacer imputacion por carrera


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
table(df$`A pesar de mis esfuerzos, mis resultados académicos no mejoran.`, useNA = "ifany")

prop.table(table(df$`A pesar de mis esfuerzos, mis resultados académicos no mejoran.`))

# Después
table(df_imputado$`A pesar de mis esfuerzos, mis resultados académicos no mejoran.`)

prop.table(table(df_imputado$`A pesar de mis esfuerzos, mis resultados académicos no mejoran.`))


sum(is.na(df_imputado)) # En convenio hay que tener más ojo por las imputaciones


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


# Join con deserción ------------


sap = tibble(read_excel("ABANDONO-RENUNCIA SAP.XLSX", 
                        sheet = "Sheet1"))

names(sap)[1] = "RUT"


names(sap)


sap = sap[,names(sap) %in% c("RUT", "Texto Indicador", "Fecha modificación", "Año académico", "Texto Plan Estudio", "Texto Unidad Organizativa")]


simbad = tibble(read_excel("ABANDONO-RENUNCIA Simbad.xlsx", 
                           sheet = "Hoja1"))


names(simbad)[1] = "RUT"


names(simbad)


simbad = simbad[,names(simbad) %in% c("RUT", "NOM_CARRERA", "P_AL_AGNO_IC", "E_PL_TEXTO", "SEME_INI", "SEME_FIN")]


cruce_sap = merge(df_imputado, sap, by = "RUT")


cruce_sap = cruce_sap %>% filter(`Año académico` > 2022) # matar registros de gente que entro antes de rendir la epaes


write.csv(cruce_sap, "cruce_sap.csv")


# Identificar RUT duplicados en el join
duplicados <- cruce_sap %>%
  group_by(RUT) %>%
  filter(n() > 1) %>%
  arrange(RUT)




cruce_2023 <- cruce_sap %>% filter(AÑO == 2023)
cruce_2024 <- cruce_sap %>% filter(AÑO == 2024)
cruce_2025 <- cruce_sap %>% filter(AÑO == 2025)


cruce_simbad = merge(df_imputado, simbad, by = "RUT")

# Visualizaciones -------------

rangos = cruce_sap[, names(cruce_sap) %in% c("RUT", "FACULTAD", "Carrera", "Texto Indicador", "Rango anticipación analítica",
                                             "Rango Autodeterminación personal", "Rango autoeficacia académica",
                                             "Rango comunicación efectiva", "Rango de control y/o modulación emocional",
                                             "Rango prospectiva académica", "Rango Sociabilidad", "AÑO",
                                             "Promedio anticipación analítica", "Promedio Autodeterminación Personal",
                                             "Promedio Autoeficacia académica", "Promedio Comunicación efectiva",
                                             "Promedio Control y/o modulación emocional", "Promedio prospectiva académica",
                                             "Promedio Sociabilidad")]

# Tomar columnas
df_long <- rangos %>%
  pivot_longer(
    cols = contains("Rango"),
    names_to = "pregunta",
    values_to = "nivel"
  )


df_long = df_long[,names(df_long) %in% c("RUT", "Carrera", "AÑO", "FACULTAD", "Texto Indicador", "pregunta", "nivel")]


unique(df_long$nivel)


df_long$nivel[df_long$nivel == "Alto"] = "RANGO ALTO"
df_long$nivel[df_long$nivel == "Medio"] = "RANGO MEDIO"
df_long$nivel[df_long$nivel == "Bajo"] = "RANGO BAJO"


df_long2 <- rangos %>%
  pivot_longer(
    cols = contains("Promedio"),
    names_to = "pregunta",
    values_to = "nivel"
  )


names(df_long2)[13:14] = c("pregunta_prom", "nivel_prom")


df_long2 = df_long2[,names(df_long2) %in% c("pregunta_prom", "nivel_prom")]


df_long = cbind(df_long, df_long2)


df_long$nivel_prom = round(df_long$nivel_prom, 2)


write.csv(df_long, "cruces.csv", dec = ",")


# Quedo feo
# Facultad
ggplot(df_long, aes(x = FACULTAD, fill = nivel)) +
  geom_bar(position = "fill") + # "fill" hace proporciones 0–1
  facet_wrap(~ pregunta) + # un gráfico por cada pregunta
  labs(
    title = "Distribución de niveles por Facultad",
    y = "Proporción",
    x = ""
  ) +
  theme_minimal()


# Carrera
ggplot(df_long, aes(x = Carrera, fill = Nivel)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Pregunta) +
  coord_flip() + # útil si hay muchas carreras
  theme_minimal()

# Abandono o renuncia
ggplot(df_long, aes(x = `Texto Indicador`, fill = nivel)) +
  geom_bar(position = "fill") +
  facet_wrap(~ pregunta) +
  theme_minimal()







# Join notas --------


epaes_desercion = tibble(read_csv("cruce_sap.csv"))
epaes_desercion = epaes_desercion[,-1]
names(epaes_desercion)


epaes = tibble(read_excel("epaes_v2.xlsx"))
names(epaes)


epaes$estado = ifelse(epaes$RUT %in% epaes_desercion$RUT, "Abandono", "Sigue")
sum(is.na(epaes$estado))
table(epaes$estado) # todo ok


notas = tibble(read_excel("Notas_parte_1.xlsx"))
names(notas)
sort(unique(notas$AGRTEXTTXT))


# Aqui vamos a tener un temita jajaja, son to much, veamos como reducir
notas_a_considerar = c("Calificación Final", "Nota final", "Nota única (100%)", "Nota final,ACTA RECTIFICATORIA")


notas_filtradas = notas %>% filter(AGRTEXTTXT %in% notas_a_considerar)
notas_filtradas = notas_filtradas %>% filter(PERYR > 2022) # EPAES es desde el 2023
length(unique(notas_filtradas$STUDENT12)) # 3330 estudiantes, como shusha no van a cruzar 710
names(notas_filtradas)


columnas_a_usar = c("PERYR", "PADRE_SUBNOTA", "AGRTEXTTXT", "GRADESYM", "RUT", "CARRERA")


notas_filtradas = notas_filtradas[,columnas_a_usar]
names(notas_filtradas)


# Cruce con las notas

notas_filtradas$estado = epaes$estado[match(notas_filtradas$RUT, epaes$RUT)]
notas_filtradas$estado[is.na(notas_filtradas$estado)] = "Sin coincidencia"
table(notas_filtradas$estado)


notas_filtradas = subset(notas_filtradas, notas_filtradas$estado != "Sin coincidencia")
notas_filtradas = notas_filtradas %>% filter(!is.na(GRADESYM))
str(notas_filtradas)
unique(notas_filtradas$GRADESYM)
notas_no_utiles = c("CO", "P", "AS")
notas_filtradas = notas_filtradas %>% filter(!(GRADESYM %in% notas_no_utiles))


notas_filtradas$GRADESYM = as.numeric(gsub(",", ".", notas_filtradas$GRADESYM))
sum(is.na(notas_filtradas$GRADESYM))


# GG, buscar un test que aguante más de 5000 obs
shapiro_sigue <- shapiro.test(notas_filtradas$GRADESYM[notas_filtradas$estado == "Sigue"])
shapiro_abandono  <- shapiro.test(notas_filtradas$GRADESYM[notas_filtradas$estado == "Abandono"])



# vectores por grupo
notas_sigue <- notas_filtradas$GRADESYM[notas_filtradas$estado == "Sigue"]
notas_abandono  <- notas_filtradas$GRADESYM[notas_filtradas$estado == "Abandono"]

t_welch <- t.test(notas_sigue, notas_abandono,
                  alternative = "less",   # H0: promedio de notas que sigue es mayor o igual al que abandono
                                          # H1: promedio de notas que sigue es menor al que abandono
                  var.equal = FALSE,      # Welch
                  conf.level = 0.95)


t_welch # No hay suficiente evidencia para rechazar h0, por lo tanto, no hay suficiente evidencia para decir que las notas
# que los que siguen son menores a las que abandonaron


# Tomar carrera, facultad, rangos, promedios, año y convenio de epaes y unirlo con notas


sort(names(epaes))
columnas_a_usar = c("AÑO", "Carrera", "CONVENIO", "FACULTAD", "Promedio anticipación analítica", "Promedio Autodeterminación Personal",
                    "Promedio Autoeficacia académica", "Promedio Comunicación efectiva", "Promedio Control y/o modulación emocional",
                    "Promedio prospectiva académica", "Promedio Sociabilidad", "Rango anticipación analítica",
                    "Rango Autodeterminación personal", "Rango autoeficacia académica", "Rango comunicación efectiva",
                    "Rango de control y/o modulación emocional", "Rango prospectiva académica", "Rango Sociabilidad", "RUT")


epaes_2 = epaes[,columnas_a_usar]


notas_eapes = notas_filtradas %>% 
  left_join(epaes_2, by = "RUT")


colSums(is.na(notas_eapes)) # Todo ok


library(ggpubr) #Grafico corr
library(ggcorrplot) # Grafico corr con valor p
library(plotly)

# Caso ez
df_ = notas_eapes[,c("Promedio Autodeterminación Personal","Promedio Control y/o modulación emocional", "Promedio Autoeficacia académica",
            "Promedio prospectiva académica", "Promedio anticipación analítica", "Promedio Comunicación efectiva",
            "Promedio Sociabilidad", "GRADESYM")]


df_corr = cor(df_)


plot_ly(
  x = colnames(df_corr),
  y = rownames(df_corr),
  z = df_corr,
  type = "heatmap",
  colorscale = "RdBu"
)


# Colocar la correlación en el grafico
ggcorrplot(df_corr, hc.order = TRUE, type = "lower",
           lab = TRUE, lab_size = 3, 
           colors = c("red", "white", "blue"),
           title = "Matriz de Correlación",
           ggtheme = theme_minimal())


p_matrix <- cor_pmat(df_)


# Colocar la significancia en el grafico
ggcorrplot(df_corr, 
           hc.order = TRUE, type = "lower",
           lab = TRUE, lab_size = 3,
           p.mat = p_matrix, sig.level = 0.05,
           insig = "blank",
           colors = c("red", "white", "blue"),
           title = "Matriz de Correlación con Significancia",
           ggtheme = theme_minimal())


# write.csv(notas_eapes, "notas_espaes.csv")


# Caso Shiny -------

library(shiny)
library(dplyr)

df <- notas_eapes

# Va correlaciones
vars_correlacion <- c(
  "Promedio Autodeterminación Personal",
  "Promedio Control y/o modulación emocional",
  "Promedio Autoeficacia académica",
  "Promedio prospectiva académica",
  "Promedio anticipación analítica",
  "Promedio Comunicación efectiva",
  "Promedio Sociabilidad",
  "GRADESYM"
)

ui <- fluidPage(
  
  titlePanel("Matriz de Correlación Dinámica"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("facultad", "Filtrar por Facultad:",
                  choices = c("Todas", sort(unique(df$FACULTAD))),
                  selected = "Todas"),
      
      selectInput("carrera", "Filtrar por Carrera:",
                  choices = c("Todas", sort(unique(df$Carrera))),
                  selected = "Todas"),
      
      checkboxGroupInput(
        "vars", "Seleccionar variables a correlacionar:",
        choices = vars_correlacion,
        selected = vars_correlacion
      )
    ),
    
    mainPanel(
      plotOutput("corrplot")
    )
  )
)

server <- function(input, output, session) {
  
  # Actualizar las carreras según la facultad seleccionada
  observeEvent(input$facultad, {
    
    if (input$facultad == "Todas") {
      
      updateSelectInput(session, "carrera",
                        choices = c("Todas", sort(unique(df$Carrera))),
                        selected = "Todas")
      
    } else {
      
      carreras_filtradas <- df %>% 
        filter(FACULTAD == input$facultad) %>% 
        pull(Carrera) %>% 
        unique() %>% 
        sort()
      
      updateSelectInput(session, "carrera",
                        choices = c("Todas", carreras_filtradas),
                        selected = "Todas")
    }
  })
  
  # Render del gráfico
  output$corrplot <- renderPlot({
    
    datos_filtrados <- df
    
    # Filtrar Facultad
    if (input$facultad != "Todas") {
      datos_filtrados <- datos_filtrados %>%
        filter(FACULTAD == input$facultad)
    }
    
    # Filtrar Carrera
    if (!is.null(input$carrera) && input$carrera != "Todas") {
      datos_filtrados <- datos_filtrados %>%
        filter(CARRERA == input$carrera)
    }
    
    # Mantener solo columnas seleccionadas
    datos_corr <- datos_filtrados %>% select(all_of(input$vars))
    
    # Convertir a numérico
    datos_corr <- as.data.frame(lapply(datos_corr, function(x) {
      as.numeric(as.character(x))
    }))
    
    # Matriz de correlación
    corr <- cor(datos_corr, use = "pairwise.complete.obs")
    
    ggcorrplot(
      corr,
      type = "lower",
      lab = TRUE,
      lab_size = 3,
      colors = c("red", "white", "blue"),
      title = "Matriz de Correlación",
      ggtheme = theme_minimal()
    )
  })
}

shinyApp(ui, server)


# Test no parametrico para diferencia de facultades ----------


# Clustring ----------

epaes_desercion = tibble(read_csv("cruce_sap.csv"))
epaes_desercion = epaes_desercion[,-1]
names(epaes_desercion)


epaes = tibble(read_excel("epaes_v2.xlsx"))
names(epaes)


epaes$estado = ifelse(epaes$RUT %in% epaes_desercion$RUT, "Abandono", "Sigue")
sum(is.na(epaes$estado))
table(epaes$estado) # todo ok
str(epaes)


epaes = epaes %>% 
  dplyr::select(
    estado,
    FACULTAD,
    Carrera,
    starts_with("Promedio ")
  )


epaes_num = epaes %>% 
  dplyr::select(starts_with("Promedio"))


# Escalar variables
epaes_scaled = scale(epaes_num)


set.seed(19990772)
# Con la regla del codo nos queda en 10 clusters, volver a aplicar y caraterizar por puntajes, rangos, facultad
wss <- sapply(1:10, function(k){
  kmeans(epaes_scaled, centers = k, nstart = 25)$tot.withinss
})

plot(1:10, wss, type = "b",
     xlab = "Número de clusters",
     ylab = "Within-cluster sum of squares",
     main = "Método del Codo")



# Semilla y centroides
set.seed(19990772)
k = 10 # 2 grupos no son jajajaj


modelos_kmeans = kmeans(epaes_scaled, centers = k, nstart = 25)


epaes$cluster = modelos_kmeans$cluster

# Empecemos a caracterizar

table(epaes$cluster, epaes$estado)
table(epaes$cluster, epaes$FACULTAD)

hist(epaes$`Promedio anticipación analítica`)
table(epaes$`Promedio anticipación analítica`)


#write.csv(epaes, "eapes_con_cluster.csv")


cluster_promedios = epaes %>% 
  group_by(cluster) %>% 
  summarise(across(starts_with("Promedio "), mean, na.rm = TRUE))


cluster_promedios = cluster_promedios %>% 
  mutate(across(
    starts_with("Promedio"),
    ~ case_when(
      .x <= 2.4 ~ "Bajo",
      .x <= 3.4 ~ "Medio",
      T ~ "Alto"
    ),
    .names = "{.col}_cat"
  ))


cluster_promedios


tabla_facultad = epaes %>% 
  group_by(cluster, FACULTAD) %>% 
  summarise(n = n()) %>% 
  mutate(pct = round(n / sum(n) * 100, 1))

tabla_facultad


tabla_facultad_wide = epaes %>% 
  count(cluster, FACULTAD) %>% 
  tidyr::pivot_wider(
    names_from = FACULTAD,
    values_from = n,
    values_fill = 0
  )

tabla_facultad_wide


library(reshape2)

df_plot = cluster_promedios %>% 
  melt(id.vars = "cluster")

ggplot(df_plot, aes(x = variable, y = value, fill = factor(cluster))) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(fill = "Cluster",
       x = "Dimensiones",
       y = "Promedio")


# Clustering categorico

# Clustering mixto















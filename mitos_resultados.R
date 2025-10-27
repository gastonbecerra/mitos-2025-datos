library(tidyverse)
library(psych)
library(ggplot2)
library(forcats)
library(factoextra)
library(broom)
library(FactoMineR)

glimpse(data)


# limmpieza ------------------------

#2do: hay que borrar donde no estén todos los valores

p <- quantile(data$duration, probs = c(.02, .98), na.rm = TRUE)

data2 <- data |>
  filter(age >= 18, age <= 90) |>
  filter(duration >= p[1], duration <= p[2]) |>
  mutate(
    country = case_when(country %in% c("Argentina","España","Chile","Uruguay","Perú") ~ country, TRUE ~ "Otros"),
    education = str_to_lower(education),
    # education = recode(education, "bachelor"="bachelors","licenciatura"="bachelors","secundario"="high-school","master"="masters","maestria"="masters"),
    gender = str_to_lower(gender),
    # gender = recode(gender, "femenino"="female","masculino"="male", .default = gender),
    # gender = case_when(gender %in% c("male","female","non-binary","prefer-not-to-say") ~ gender, TRUE ~ "other"),
    uso_ia_frecuencia = factor(uso_ia_frecuencia, levels = c("nunca","esporadicamente","ocasional","frecuente"), ordered = TRUE)
  ) |>
  mutate(
    A5r = 6 - A5,
    actitud = rowMeans(across(c(A1:A4, A5r)), na.rm = TRUE)
  )

glimpse(data2)

boxplot(data$duration)
boxplot(data2$duration)
boxplot(data$age)
boxplot(data2$age)

data2 |>
  count(gender, sort = TRUE) |>
  mutate(pct = 100 * n / sum(n)) |>
  ggplot(aes(x = gender, y = n)) +
    geom_col(fill = "#4B9CD3", alpha = 0.8) +
    coord_flip() +
    theme_minimal(base_size = 12) +
    scale_y_continuous(expand = expansion(mult = c(0, .1)))
data2 |>
  count(country, sort = TRUE) |>
  mutate(pct = 100 * n / sum(n)) |>
  ggplot(aes(x = country, y = n)) +
  geom_col(fill = "#4B9CD3", alpha = 0.8) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))
data2 |>
  count(education, sort = TRUE) |>
  mutate(pct = 100 * n / sum(n)) |>
  ggplot(aes(x = education, y = n)) +
  geom_col(fill = "#4B9CD3", alpha = 0.8) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))
data2 |>
  count(uso_ia_frecuencia, sort = TRUE) |>
  mutate(pct = 100 * n / sum(n)) |>
  ggplot(aes(x = uso_ia_frecuencia, y = n)) +
  geom_col(fill = "#4B9CD3", alpha = 0.8) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))
data2 |>
  count(workArea, sort = TRUE) |>
  mutate(pct = 100 * n / sum(n)) |>
  ggplot(aes(x = workArea, y = n)) +
  geom_col(fill = "#4B9CD3", alpha = 0.8) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))


rm(p)

# sociodemograficos -----------------------------


data2 |>
  summarise(n = n(), edad_prom = mean(age, na.rm = TRUE), edad_sd = sd(age, na.rm = TRUE), edad_min = min(age, na.rm = TRUE), edad_max = max(age, na.rm = TRUE))

data2 |> count(gender, sort = TRUE)
data2 |> count(country, sort = TRUE)
data2 |> count(education, sort = TRUE)
data2 |> count(uso_ia_frecuencia, sort = TRUE)
data2 |> count(workArea, sort = TRUE)


data2 |>
  group_by(gender) |>
  summarise(
    edad_prom = mean(age, na.rm = TRUE),
    edad_sd = sd(age, na.rm = TRUE),
    n = n()
  ) |>
  left_join(
    data2 |> count(gender, education) |> group_by(gender) |> mutate(pct = 100 * n / sum(n)),
    by = "gender"
  )

data2 |>
  count(education, workArea) |>
  group_by(education) |>
  mutate(pct = 100 * n / sum(n))


data2 |>
  count(workArea) |>
  mutate(pct = 100 * n / sum(n))


data2 |>
  count(workArea, education) |>
  group_by(workArea) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup() |>
  # ggplot(aes(x = fct_reorder(workArea, pct, .fun = sum), y = pct, fill = education)) +
  ggplot(aes(x = fct_infreq(workArea), y = n, fill = education)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL, y = "% dentro de área de trabajo", fill = "Nivel educativo",
    title = "Distribución de niveles educativos por área de trabajo"
  ) +
  theme_minimal(base_size = 12)

ggplot(data2, aes(x = reorder(workArea, actitud, median), y = actitud, fill = workArea)) +
  geom_violin(scale = "count", trim = FALSE, alpha = 0.7) +
  stat_summary(fun = median, geom = "point", color = "white") +
  coord_flip() +
  scale_x_discrete(labels = function(x) {
    n <- as.integer(table(data2$workArea)[x]); paste0(x, " (n=", n, ")")
  }) +
  labs(x = NULL, y = "Actitud (1–5)", title = "Actitud por área de trabajo (violín ∝ n)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")



# comprobaciones factorial actitud ------------------------

psych::alpha(data2 |> select(A1,A2,A3,A4,A5r))
# 2do ver si estamos desechando el mismo item que ellos

act <- data2 |> dplyr::select(A1, A2, A3, A4, A5r)
psych::KMO(act)
psych::cortest.bartlett(act)
psych::fa.parallel(act, fa = "fa")
actitud_fa2 <- psych::fa(act, nfactors = 2, rotate = "oblimin")
actitud_fa2

# dos subdimensiones muy correlacionadas:
# MR1: confianza/aceptación de la IA.
# MR2: expectativa de beneficio u optimismo.
# Dado que la correlación entre factores es tan alta (.71), podés tratarlos como un único constructo (actitud general), o bien conservar ambos si te interesa analizar matices.
# A4 y A5r --> confianza/aceptacion
# A2 y A3 --> utilidad
# asumimos entonces 1 solo factor


## distribucion pareja en grupos xp

data2 |>
  group_by(optionsCount) |>
  summarise(
    n = n(),
    mean = mean(actitud, na.rm = TRUE),
    sd   = sd(actitud, na.rm = TRUE),
    min  = min(actitud, na.rm = TRUE),
    max  = max(actitud, na.rm = TRUE)
  )

data2 |> count(optionsCount, gender) |> mutate(pct = 100 * n / sum(n))
data2 |> count(optionsCount, uso_ia_frecuencia) |> mutate(pct = 100 * n / sum(n))
data2 |> count(optionsCount, country) |> mutate(pct = 100 * n / sum(n))

rm(actitud_fa2, act)




# actitud general ---------------------

glimpse(data2)
summary(data2$actitud)

ggplot(data2, aes(x = actitud)) +
  geom_histogram(binwidth = 0.2, fill = "#4B9CD3", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(actitud, na.rm = TRUE)),
             color = "red", linewidth = 0.8, linetype = "dashed") +
  labs(
    x = "Actitud hacia la IA (1–5)",
    y = "Frecuencia",
    title = "Distribución de la actitud hacia la IA"
  ) +
  theme_minimal(base_size = 12)

data2 <- data2 |>
  mutate(
    actitud3 = cut(
      actitud,
      breaks = quantile(actitud, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
      labels = c("baja", "media", "alta"),
      include.lowest = TRUE
    ),
    actitud2 = if_else(actitud < median(actitud, na.rm = TRUE), "baja", "alta")
  )

data2 |> count(actitud2)
data2 |> count(actitud3)


data2 |>
  tidyr::pivot_longer(cols = c(actitud2, actitud3),
                      names_to = "version",
                      values_to = "categoria") |>
  ggplot(aes(x = categoria, fill = version)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Nivel de actitud hacia la IA",
    y = "Frecuencia de casos",
    fill = "Versión de corte",
    title = "Distribución de participantes según actitud (2 y 3 niveles)"
  ) +
  theme_minimal(base_size = 12)




# actitud x sociodemograficos ----------------------



cor.test(data2$age, data2$actitud, method = "spearman")

# | Edad | — | 0.74 | No hay relación: la actitud hacia la IA no varía con la edad. |

# anovas actitud ~ género / educación / área / uso de IA
aov_gender <- aov(actitud ~ gender, data = data2)
aov_edu <- aov(actitud ~ education, data = data2)
aov_work <- aov(actitud ~ workArea, data = data2)
aov_ia <- aov(actitud ~ uso_ia_frecuencia, data = data2)
list(
  gender = broom::tidy(aov_gender),
  education = broom::tidy(aov_edu),
  workArea = broom::tidy(aov_work),
  uso_ia = broom::tidy(aov_ia)
)
#2do: no estoy leyendo estos datos o si?
rm(aov_edu, aov_ia, aov_gender, aov_work)

ggplot(data2, aes(x = gender, y = actitud, fill = gender)) +
  geom_boxplot(alpha = 0.8) +
  labs(x = NULL, y = "Actitud hacia la IA (1–5)", title = "Actitud por género") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
ggplot(data2, aes(x = education, y = actitud, fill = education)) +
  geom_boxplot(alpha = 0.8) +
  labs(x = NULL, y = "Actitud hacia la IA (1–5)", title = "Actitud por nivel educativo") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
ggplot(data2, aes(x = reorder(workArea, actitud, median), y = actitud, fill = workArea)) +
  geom_boxplot(alpha = 0.8) +
  coord_flip() +
  labs(x = NULL, y = "Actitud hacia la IA (1–5)", title = "Actitud por área de trabajo") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
ggplot(data2, aes(x = uso_ia_frecuencia, y = actitud, fill = uso_ia_frecuencia)) +
  geom_boxplot(alpha = 0.8) +
  labs(x = NULL, y = "Actitud hacia la IA (1–5)", title = "Actitud por frecuencia de uso de IA") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")




# 2do: un modelo para ver factores sociodemograficos y actitud?

m1 <- lm(actitud ~ gender + education + workArea + uso_ia_frecuencia, data = data2)
summary(m1)
broom::tidy(m1)


broom::tidy(m1, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(term = fct_reorder(term, estimate)) |>
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "#4B9CD3") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Efecto estimado sobre la actitud (β)",
    title = "Coeficientes del modelo lineal de actitud hacia la IA"
  ) +
  theme_minimal(base_size = 12)

broom::tidy(m1, conf.int = TRUE) |>
  filter(term != "(Intercept)", !grepl("uso_ia_frecuencia", term)) |>
  filter(p.value < 0.05) |>
  mutate(term = fct_reorder(term, estimate)) |>
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "#4B9CD3") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Efecto estimado (β)",
    title = "Predictores significativos de la actitud hacia la IA"
  ) +
  theme_minimal(base_size = 12)

rm(m1)

## clusters de respondentes ----------------

vars <- data2 |>
  transmute(
    age, actitud, duration,
    uso_ia = as.numeric(uso_ia_frecuencia),
    genero = as.numeric(factor(gender)),
    educ = as.numeric(factor(education))
  )

set.seed(123)
km <- kmeans(scale(vars), centers = 3, nstart = 25)

data2 <- data2 |>
  mutate(cluster = factor(km$cluster))

fviz_cluster(km, data = scale(vars), geom = "point", ellipse.type = "norm")


data2 |>
  group_by(cluster) |>
  summarise(
    n = n(),
    edad = mean(age, na.rm = TRUE),
    actitud = mean(actitud, na.rm = TRUE),
    uso_ia = mean(as.numeric(uso_ia_frecuencia), na.rm = TRUE),
    .groups = "drop"
  )

data2 |>
  select(cluster, age, actitud, uso_ia = uso_ia_frecuencia) |>
  mutate(uso_ia = as.numeric(uso_ia)) |>
  group_by(cluster) |>
  summarise(across(c(age, actitud, uso_ia), mean, na.rm = TRUE), .groups = "drop") |>
  mutate(across(c(age, actitud, uso_ia), ~ as.numeric(scale(.x)))) |>
  pivot_longer(-cluster, names_to = "variable", values_to = "z") |>
  ggplot(aes(x = variable, y = z, fill = cluster)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Media estandarizada (z)", title = "Perfil de clusters (edad, actitud, uso de IA)") +
  theme_minimal(base_size = 12)

data2 |>
  select(cluster, age, actitud) |>
  group_by(cluster) |>
  summarise(across(c(age, actitud), mean, na.rm = TRUE), .groups = "drop") |>
  mutate(across(c(age, actitud), ~ as.numeric(scale(.x)))) |>
  pivot_longer(-cluster, names_to = "variable", values_to = "z") |>
  ggplot(aes(x = variable, y = z, fill = cluster)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = "Media estandarizada (z)", title = "Perfil de clusters (edad y actitud)") +
  theme_minimal(base_size = 12)

data2 |>
  count(cluster, workArea) |>
  group_by(cluster) |>
  mutate(pct = 100 * n / sum(n)) |>
  ggplot(aes(x = cluster, y = pct, fill = workArea)) +
  geom_col() +
  labs(x = "Cluster", y = "% dentro del cluster", fill = "Área de trabajo",
       title = "Distribución de áreas de trabajo por cluster") +
  theme_minimal(base_size = 12)

## analisis de correspondencia -----------

df <- data2 |>
  transmute(
    actitud2 = factor(actitud),
    gender = factor(gender),
    education = factor(education),
    workArea = factor(workArea),
    studyArea = factor(studyArea),
    country = factor(country)
  ) |>
  na.omit()

mca <- MCA(df, quali.sup = c(5,6), graph = FALSE)

fviz_mca_biplot(mca, repel = TRUE) + theme_minimal(base_size = 12)

fviz_mca_ind(mca, habillage = "actitud2", addEllipses = TRUE, ellipse.level = 0.95, alpha.ind = 0.4) +
  theme_minimal(base_size = 12)

fviz_contrib(mca, choice = "var", axes = 1, top = 15) + theme_minimal(base_size = 12)
fviz_contrib(mca, choice = "var", axes = 2, top = 15) + theme_minimal(base_size = 12)


rm(df,km,mca,vars)









# creencias ------------------------

creencias <- data2 |> dplyr::select(C1:C6)
psych::KMO(creencias)
psych::cortest.bartlett(creencias)
psych::fa.parallel(creencias, fa = "fa")
fa3 <- psych::fa(creencias, nfactors = 3, rotate = "oblimin")
fa3

pca <- psych::principal(creencias, nfactors = 3, rotate = "oblimin")
psych::fa.diagram(pca)
fa3 <- psych::fa(creencias, nfactors = 3, rotate = "oblimin")
psych::fa.diagram(fa3)

etiquetas_creencias <- tibble::tibble(
  item = c("C1","C2","C3","C4","C5","C6"),
  texto = c(
    "Creo que la IA puede tomar decisiones justas y objetivas",
    "Creo que la IA puede dar respuestas certeras y sin prejuicios a mis preguntas",
    "Creo que la IA puede ofrecer orientación y atención psicológica",
    "Creo que la IA puede brindar compañía y apoyo emocional",
    "Creo que la IA puede hacer investigación científica",
    "Creo que la IA puede enseñar y guiar el aprendizaje"
  )
)

data2 |>
  tidyr::pivot_longer(C1:C6, names_to = "item", values_to = "valor") |>
  dplyr::group_by(item) |>
  dplyr::summarise(media = mean(valor, na.rm = TRUE),
                   sd = sd(valor, na.rm = TRUE)) |>
  dplyr::left_join(etiquetas_creencias, by = "item") |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(texto, media), y = media)) +
  ggplot2::geom_col(fill = "#4B9CD3", alpha = 0.8) +
  geom_errorbar(aes(ymin = media - sd/10, ymax = media + sd/10), width = 0.1) +
  ggplot2::geom_text(ggplot2::aes(label = round(media, 2)), hjust = -0.3, size = 3) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(limits = c(0,5)) +
  ggplot2::labs(x = NULL, y = "Promedio de acuerdo (1–5)",
                title = "Nivel de acuerdo con distintas creencias sobre la IA") +
  ggplot2::theme_minimal(base_size = 12)

# SD variabilidad similar, ninguna está completamente polarizada ni totalmente consensuada

data2 |>
  tidyr::pivot_longer(C1:C6, names_to = "item", values_to = "valor") |>
  dplyr::filter(dplyr::between(valor, 1, 5)) |>
  dplyr::left_join(etiquetas_creencias, by = "item") |>
  ggplot2::ggplot(ggplot2::aes(x = valor)) +
  ggplot2::geom_histogram(binwidth = 1, boundary = 0.5, fill = "#4B9CD3", alpha = 0.8) +
  ggplot2::scale_x_continuous(breaks = 1:5) +
  ggplot2::facet_wrap(~ texto, ncol = 3) +
  ggplot2::labs(x = "Respuesta Likert (1–5)", y = "Frecuencia",
                title = "Distribución de respuestas por creencia") +
  ggplot2::theme_minimal(base_size = 12)

data2 |>
  tidyr::pivot_longer(C1:C6, names_to = "item", values_to = "valor") |>
  dplyr::left_join(etiquetas_creencias, by = "item") |>
  dplyr::group_by(texto) |>
  dplyr::summarise(
    n = dplyr::n(),
    media = round(mean(valor, na.rm = TRUE), 2),
    sd = round(sd(valor, na.rm = TRUE), 2),
    min = min(valor, na.rm = TRUE),
    max = max(valor, na.rm = TRUE)
  ) |>
  dplyr::arrange(desc(media))

data2 |>
  tidyr::pivot_longer(C1:C6, names_to = "item", values_to = "valor") |>
  dplyr::left_join(etiquetas_creencias, by = "item") |>
  dplyr::group_by(texto, actitud2) |>
  dplyr::summarise(media = mean(valor, na.rm = TRUE), .groups = "drop") |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(texto, media), y = media, fill = actitud2)) +
  ggplot2::geom_col(position = "dodge", alpha = 0.8) +
  ggplot2::geom_text(
    ggplot2::aes(label = round(media, 2)),
    position = ggplot2::position_dodge(width = 0.9),
    vjust = -0.3, size = 3
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual(values = c("#999999", "#4B9CD3"), name = "Actitud hacia la IA") +
  ggplot2::scale_y_continuous(limits = c(0, 5)) +
  ggplot2::labs(
    x = NULL,
    y = "Promedio de acuerdo (1–5)",
    title = "Nivel de acuerdo con distintas creencias según actitud hacia la IA"
  ) +
  ggplot2::theme_minimal(base_size = 12)



data2 |>
  tidyr::pivot_longer(C1:C6, names_to = "item", values_to = "valor") |>
  group_by(item) |>
  do(broom::tidy(t.test(valor ~ actitud2, data = ., var.equal = TRUE))) |>
  mutate(significativo = ifelse(p.value < 0.05, "sí", "no")) |>
  select(item, estimate1, estimate2, estimate, statistic, p.value, significativo)

labs_items <- c(
  C1="IA justa y objetiva",
  C2="IA sin prejuicios",
  C3="IA orienta psicológicamente",
  C4="IA apoya emocionalmente",
  C5="IA hace investigación",
  C6="IA enseña y guía"
)

data2 |>
  pivot_longer(C1:C6, names_to="item", values_to="valor") |>
  group_by(item, actitud2) |>
  summarise(m=mean(valor,na.rm=TRUE),
            se=sd(valor,na.rm=TRUE)/sqrt(sum(!is.na(valor))),
            .groups="drop") |>
  mutate(item=factor(item, levels=names(labs_items), labels=labs_items)) |>
  ggplot(aes(item, m, fill=actitud2)) +
  geom_col(position=position_dodge(width=.8), width=.7) +
  geom_errorbar(aes(ymin=m-se, ymax=m+se),
                position=position_dodge(width=.8), width=.15) +
  coord_flip() +
  scale_y_continuous(limits=c(0,5)) +
  labs(x=NULL, y="Media (1–5)", fill="Actitud",
       title="Creencias sobre IA por actitud (alta vs baja)") +
  theme_minimal(base_size=12)

# En las seis creencias, quienes tienen actitud alta puntúan más que actitud baja (todas p < .001).
# Mayor diferencia en C6 (enseñar/aprender, Δ=0.78), luego C5 (investigación, Δ=0.69) y C4 (apoyo emocional, Δ=0.62). Las menores en C2/C3 (~0.47).
# 2do; etiqueta_creencias  y labs_items no son duplicados?

rm(fa3,pca,labs_items,etiquetas_creencias,creencias)









# experimento escenarios ---------------

# https://studio.firebase.google.com/studio-6142797116


rm(abs_rel, base, dif, esc, esc_plot, etiquetas_creencias, share_bias, etiquetas)

esc <- data2 |>
  select(id, optionsCount, scenarios) |>
  unnest(scenarios) |>
  transmute(
    id,
    optionsCount = factor(optionsCount, levels = c(2,3)),
    item,
    answer = factor(answer, levels = c("human","ia","both"))
  )

esc |>
  count(item, optionsCount) |>
  group_by(item) |>
  mutate(pct = 100 * n / sum(n)) |>
  ggplot(aes(x = item, y = pct, fill = optionsCount)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
  labs(
    x = "Escenario",
    y = "% de casos asignados",
    fill = "Condición",
    title = "Balance de exposición entre condiciones (2 vs 3 opciones)"
  ) +
  theme_minimal(base_size = 12)

esc |> count(optionsCount, answer) |>
  group_by(optionsCount) |>
  mutate(pct = 100 * n / sum(n)) |>
  arrange(optionsCount, answer)

esc |>
  count(optionsCount, answer) |>
  group_by(optionsCount) |>
  mutate(pct = 100 * n / sum(n)) |>
  ggplot(aes(x = optionsCount, y = pct, fill = answer)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Condición (número de opciones)",
    y = "% de respuestas",
    fill = "Respuesta",
    title = "Distribución global de respuestas por condición"
  ) +
  theme_minimal(base_size = 12)


## escenarios salientes o primarios --------------

# "scenarios": [
#   {"id": "E1", "text": "Para tomar una decisión en un tema controversial o en una disputa ...", "belief_id": "C1"},
#   {"id": "E2", "text": "Para analizar los argumentos de cada parte en una discusión, y señalar inconsistencias ...", "belief_id": "C1"},
#   {"id": "E3", "text": "Para responder una consulta experta, como por ejemplo, legales o médicas ...", "belief_id": "C2"},
#   {"id": "E4", "text": "Para obtener respuestas a preguntas generales de cultura o información cotidiana ...", "belief_id": "C2"},
#   {"id": "E7", "text": "Para recibir ayuda en una crisis emocional o psicológica ...", "belief_id": "C3"},
#   {"id": "E8", "text": "Para hablar de mis sentimientos y pensamientos ...", "belief_id": "C3"},
#   {"id": "E9", "text": "Para recibir una sugerencia sobre cómo responder a un mensaje de mi pareja ...", "belief_id": "C4"},
#   {"id": "E10", "text": "Para charlar sobre temas cotidianos ...", "belief_id": "C4"},
#   {"id": "E11", "text": "Para diseñar y planificar una investigación científica ...", "belief_id": "C5"},
#   {"id": "E12", "text": "Para redactar y corregir los resultados de una investigación ...", "belief_id": "C5"},
#   {"id": "E13", "text": "Para enseñar nuevos conocimientos ...", "belief_id": "C6"},
#   {"id": "E14", "text": "Para elaborar contenidos y ejercicios educativos ...", "belief_id": "C6"}
# ]
# }

etiquetas <- tibble::tibble(
  item = c("E1","E2","E3","E4","E7","E8","E9","E10","E11","E12","E13","E14"),
  texto = c(
    "E1 Decisión en tema controversial",
    "E2 Analizar argumentos en discusión",
    "E3 Responder consulta experta (legal/médica)",
    "E4 Preguntas generales de cultura",
    "E7 Ayuda en crisis emocional",
    "E8 Hablar de sentimientos",
    "E9 Sugerencia para responder a mi pareja",
    "E10 Charlar sobre temas cotidianos",
    "E11 Diseñar investigación científica",
    "E12 Redactar resultados de investigación",
    "E13 Enseñar nuevos conocimientos",
    "E14 Elaborar contenidos educativos"
  )
)

esc |>
  filter(item=="E9") |>
  count(optionsCount, answer)

esc |>
  # filter(item=="E9") |>
  count(item, optionsCount, answer) |>
  group_by(item, optionsCount) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup() |>
  left_join(etiquetas, by = "item") |>
  ggplot(aes(x = texto, y = pct, fill = answer)) +
  geom_col(position = "fill") +
  coord_flip() +
  facet_wrap(~ optionsCount) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Escenario",
    y = "% dentro del escenario",
    fill = "Respuesta",
    title = "Distribución de respuestas por escenario y condición"
  ) +
  theme_minimal(base_size = 20)





## entras both, que pasa? -----------------

# ¿El ingreso de la opción “both” redistribuye las respuestas equitativamente desde human e IA, o erosiona principalmente una de ellas?

# diferencia porcentual
esc |>
  count(item, optionsCount, answer) |>
  group_by(item, optionsCount) |>
  mutate(pct = 100 * n / sum(n)) |>
  select(item, optionsCount, answer, pct) |>
  filter(answer %in% c("human", "ia")) |>
  tidyr::pivot_wider(names_from = optionsCount, values_from = pct, names_prefix = "opc_") |>
  mutate(
    diff = opc_3 - opc_2
    ) |>
  left_join(etiquetas, by = "item") |>
  ggplot(aes(x = texto, y = diff, fill = answer)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(
    x = "Escenario",
    y = "Diferencia porcentual (3 opciones − 2 opciones)",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 20)

# diferencia votos
esc |>
  count(item, optionsCount, answer) |>
  group_by(item, optionsCount) |>
  filter(answer %in% c("human", "ia")) |>
  tidyr::pivot_wider(names_from = optionsCount, values_from = n, names_prefix = "opc_") |>
  mutate(
    diff = opc_3 - opc_2
  ) |>
  left_join(etiquetas, by = "item") |>
  ggplot(aes(x = texto, y = diff, fill = answer)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(
    x = "Escenario",
    y = "Diferencia N (3 opciones − 2 opciones)",
    fill = "Respuesta",
  ) +
  theme_minimal(base_size = 20)


particion <- esc |>
  count(item, optionsCount, answer) |>
  group_by(item, optionsCount) |>
  mutate(p = n / sum(n)) |>
  ungroup() |>
  select(item, optionsCount, answer, p) |>
  tidyr::pivot_wider(names_from = c(optionsCount, answer), values_from = p, values_fill = 0) |>
  transmute(
    item,
    d_h = `2_human` - `3_human`,
    d_i = `2_ia`    - `3_ia`,
    b   = `3_both`,
    from_h = if_else(b > 0, pmax(d_h, 0) / (pmax(d_h, 0) + pmax(d_i, 0)), NA_real_),
    from_i = if_else(b > 0, pmax(d_i, 0) / (pmax(d_h, 0) + pmax(d_i, 0)), NA_real_)
  ) |>
  left_join(etiquetas, by = "item")

particion |>
  tidyr::pivot_longer(c(from_h, from_i), names_to = "origen", values_to = "share") |>
  mutate(origen = dplyr::recode(origen, from_h = "desde human", from_i = "desde ia")) |>
  ggplot2::ggplot(ggplot2::aes(x = texto, y = share, fill = origen)) +
  ggplot2::geom_col() +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = "Escenario", y = "Participación del origen de 'both'", fill = "Origen", title = "¿De dónde proviene 'both'? (descomposición de caídas)") +
  ggplot2::theme_minimal(base_size = 20)

#  d_h = 2_human − 3_human y d_i = 2_ia − 3_ia son las caídas (en proporción) de “human” e “ia” cuando aparece “both”; b = 3_both es la proporción que toma “both”.
# from_h y from_i reparten qué fracción de “both” proviene de cada caída: recortan caídas negativas a 0 (pmax(…,0)), y dividen cada caída por la suma de caídas
# Se calcula solo si hay “both” (b>0); si no, devuelve NA. Si uno de los dos no cae, su parte queda en 0 y el otro explica el 100%.

rm(particion)




## t-test ---------------


esc_prop <- esc |>
  group_by(id, optionsCount) |>
  summarise(
    prop_ia   = mean(answer == "ia", na.rm = TRUE),
    prop_hum  = mean(answer == "human", na.rm = TRUE),
    prop_both = mean(answer == "both", na.rm = TRUE),
    .groups = "drop"
  )

t.test(prop_ia ~ optionsCount, data = esc_prop, var.equal = TRUE)
t.test(prop_hum ~ optionsCount, data = esc_prop, var.equal = TRUE)
t.test(prop_both ~ optionsCount, data = esc_prop, var.equal = TRUE)


# 👉 El valor del análisis no está en “demostrar que cambia”, sino en cuantificar cuánto se reacomoda la preferencia hacia la categoría intermedia (both) y de dónde proviene ese desplazamiento (como analizaste recién).
# Así que sí: el t-test es redundante si lo interpretás de manera puramente descriptiva;
# su interés sería más conceptual, por ejemplo, si quisieras mostrar que el cambio no es trivial o que el tamaño del desplazamiento (d) es grande.


esc_prop |>
  ggplot(aes(x = optionsCount, y = prop_hum, fill = optionsCount)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(
    x = "Condición experimental",
    y = "Proporción de respuestas 'human'",
    title = "Comparación de proporciones 'human' según cantidad de opciones"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


esc_prop |>
  group_by(optionsCount) |>
  summarise(
    mean = mean(prop_hum, na.rm = TRUE),
    se = sd(prop_hum, na.rm = TRUE) / sqrt(n())
  ) |>
  ggplot(aes(x = optionsCount, y = mean, fill = optionsCount)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.15) +
  geom_text(aes(label = sprintf("%.2f", mean)), vjust = -0.8, size = 5) +
  labs(
    x = "Condición experimental",
    y = "Media de proporciones 'human'",
    title = "Efecto de introducir 'both' sobre las elecciones humanas"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  ylim(0, 1)

esc_prop |>
  pivot_longer(starts_with("prop_"), names_to = "tipo", values_to = "proporcion") |>
  mutate(tipo = recode(tipo,
                       prop_hum = "Human",
                       prop_ia = "IA",
                       prop_both = "Both")) |>
  group_by(optionsCount, tipo) |>
  summarise(mean = mean(proporcion), .groups = "drop") |>
  ggplot(aes(x = tipo, y = mean, fill = optionsCount)) +
  geom_col(position = "dodge", width = 0.7) +
  labs(
    x = NULL, y = "Proporción media",
    fill = "Condición experimental",
    title = "Redistribución de elecciones según cantidad de opciones"
  ) +
  theme_minimal(base_size = 14) +
  ylim(0, 1)



#2do: comparar distinguiendo escenarios fuertes y de tareas soft


# "scenarios": [
#   {"id": "E1", "text": "Para tomar una decisión en un tema controversial o en una disputa ...", "belief_id": "C1"},
#   {"id": "E2", "text": "Para analizar los argumentos de cada parte en una discusión, y señalar inconsistencias ...", "belief_id": "C1"},
#   {"id": "E3", "text": "Para responder una consulta experta, como por ejemplo, legales o médicas ...", "belief_id": "C2"},
#   {"id": "E4", "text": "Para obtener respuestas a preguntas generales de cultura o información cotidiana ...", "belief_id": "C2"},
#   {"id": "E7", "text": "Para recibir ayuda en una crisis emocional o psicológica ...", "belief_id": "C3"},
#   {"id": "E8", "text": "Para hablar de mis sentimientos y pensamientos ...", "belief_id": "C3"},
#   {"id": "E9", "text": "Para recibir una sugerencia sobre cómo responder a un mensaje de mi pareja ...", "belief_id": "C4"},
#   {"id": "E10", "text": "Para charlar sobre temas cotidianos ...", "belief_id": "C4"},
#   {"id": "E11", "text": "Para diseñar y planificar una investigación científica ...", "belief_id": "C5"},
#   {"id": "E12", "text": "Para redactar y corregir los resultados de una investigación ...", "belief_id": "C5"},
#   {"id": "E13", "text": "Para enseñar nuevos conocimientos ...", "belief_id": "C6"},
#   {"id": "E14", "text": "Para elaborar contenidos y ejercicios educativos ...", "belief_id": "C6"}
# ]
# }





glimpse(esc)
mapa <- tibble::tibble(
  item = c("E1","E2","E3","E4","E7","E8","E9","E10","E11","E12","E13","E14"),
  creencia = c("C1","C1","C2","C2","C3","C3","C4","C4","C5","C5","C6","C6"),
  complejidad = c("compleja","simple","compleja","simple","compleja","simple",
                  "compleja","simple","compleja","simple","compleja","simple")
)



esc |>
  left_join(mapa) |>
  count(optionsCount, complejidad, answer) |>
  mutate(pct = 100 * n / sum(n)) |>
  ggplot(aes(x = optionsCount, y = pct, fill = answer)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal(base_size = 20) +
  facet_wrap(~complejidad)

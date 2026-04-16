library(tidyverse)
library(psych)
library(ggplot2)
library(forcats)
library(factoextra)
library(broom)
library(FactoMineR)
library(showtext)
showtext_auto()

tema_ia <- theme_minimal(base_size = 20) +
  theme(
    legend.position = "none",
    plot.title = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(),
    axis.ticks.y = element_blank()
  )


tema_apa <- theme_classic(base_size = 12, base_family = "sans") +
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )

data <- readRDS(file = 'data/data.rds')
glimpse(data)


# limmpieza ------------------------

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
    actitud = rowMeans(across(c(A1:A4, A5r)), na.rm = TRUE),
    actitud5 = rowMeans(across(c(A1:A4, A5r)), na.rm = TRUE),
    actitud4 = rowMeans(across(c(A1:A4)), na.rm = TRUE)
  )


glimpse(data2)
rm(p)




# sociodemograficos -----------------------------

data2 |>
  summarise(n = n(), edad_prom = mean(age, na.rm = TRUE), edad_sd = sd(age, na.rm = TRUE), edad_min = min(age, na.rm = TRUE), edad_max = max(age, na.rm = TRUE))

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




# actitud general ---------------------

glimpse(data2)
# summary(data2$actitud)
summary(data2$actitud4)

m <- mean(data2$actitud4, na.rm = TRUE)

data2 |>
  dplyr::filter(!is.na(actitud4), between(actitud4, 1, 5)) |>
  ggplot(aes(x = actitud4)) +
  geom_histogram(
    binwidth = 0.25,
    boundary = 1,
    fill = "grey75",
    color = "black",
    linewidth = 0.3
  ) +
  geom_vline(
    xintercept = m,
    linetype = "dashed",
    linewidth = 0.6,
    color = "black"
  ) +
  scale_x_continuous(
    limits = c(1, 5),
    breaks = 1:5,
    expand = c(0, 0)
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  apa

## grafico 1

items_likert <- data2 |>
  dplyr::select(A1, A2, A3, A4) |>
  dplyr::rename(
    "La IA me resulta positiva" = A1,
    "Me interesa aprender a usar IA" = A2,
    "La IA puede ser útil en mi vida cotidiana" = A3,
    "La IA puede mejorar mi trabajo o estudio" = A4
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "item",
    values_to = "respuesta"
  ) |>
  dplyr::filter(!is.na(respuesta)) |>
  dplyr::mutate(
    respuesta = factor(
      respuesta,
      levels = 1:5,
      labels = c(
        "Muy en desacuerdo",
        "En desacuerdo",
        "Neutral",
        "De acuerdo",
        "Muy de acuerdo"
      )
    )
  ) |>
  dplyr::count(item, respuesta) |>
  dplyr::group_by(item) |>
  dplyr::mutate(p = n / sum(n)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    item = factor(item, levels = rev(unique(item)))
  )

ggplot(items_likert, aes(x = item, y = p, fill = respuesta)) +
  geom_col(color = "black", linewidth = 0.2, width = 0.7) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_grey(start = 0.9, end = 0.35) +
  labs(
    x = NULL,
    y = NULL
  ) +
  apa

## grafico 2

tabla_n <- table(data2$workArea)

ggplot(
  data2 |> dplyr::filter(!is.na(workArea), !is.na(actitud4)),
  aes(x = reorder(workArea, actitud4, median, na.rm = TRUE), y = actitud4)
) +
  geom_boxplot(
    width = 0.6,
    fill = "grey80",
    color = "black",
    outlier.shape = 1,
    outlier.size = 1.5
  ) +
  coord_flip() +
  scale_x_discrete(
    labels = \(x) paste0(x, " (n=", as.integer(tabla_n[x]), ")")
  ) +
  scale_y_continuous(
    limits = c(1, 5),
    breaks = 1:5
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  tema_apa

## grafico 3

ggplot(
  data2 |> dplyr::filter(!is.na(uso_ia_frecuencia), !is.na(actitud4)),
  aes(x = uso_ia_frecuencia, y = actitud4)
) +
  geom_boxplot(
    width = 0.6,
    fill = "grey80",
    color = "black",
    outlier.shape = 1,
    outlier.size = 1.5
  ) +
  scale_y_continuous(
    limits = c(1, 5),
    breaks = 1:5
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  tema_apa

data2$uso_ia_frecuencia %>% table()
## grafico 4



# creencias ------------------------

creencias <- data2 |> dplyr::select(C1:C6)

# etiquetas_creencias <- tibble::tibble(
#   item = c("C1","C2","C3","C4","C5","C6"),
#   texto = c(
#     "C1 OBJETIVIDAD Tomar decisiones justas ",
#     "C2 EXPERTICIA Dar respuestas certeras",
#     "C3 PSICOLOGIA Ofrecer orientación psicológica",
#     "C4 COMPANIA Brindar compañía",
#     "C5 CIENCIA Hacer investigación",
#     "C6 ENSENAR Enseñar y guiar el aprendizaje"
#   )
# )

etiquetas_creencias <- tibble::tibble(
  item = c("C1","C2","C3","C4","C5","C6"),
  texto = c(
    "Tomar decisiones justas ",
    "Dar respuestas certeras",
    "Ofrecer orientación psicológica",
    "Brindar compañía",
    "Hacer investigación científica",
    "Enseñar y guiar el aprendizaje"
  )
)

media_actitud <- mean(data2$actitud4, na.rm = TRUE)

ggplot(creencias_plot, aes(x = reorder(texto, media), y = media)) +
  geom_segment(
    aes(xend = reorder(texto, media), y = 1, yend = media),
    linewidth = 0.6,
    color = "black"
  ) +
  geom_point(
    size = 3,
    shape = 21,
    fill = "grey70",
    color = "black"
  ) +
  geom_hline(
    yintercept = media_actitud,
    linetype = "dashed",
    linewidth = 0.5,
    color = "black"
  ) +
  annotate(
    "text",
    x = 1.1,
    y = media_actitud + 0.05,
    label = paste0("Media actitud general = ", round(media_actitud, 2)),
    hjust = 0,
    size = 3.2,
    family = "sans"
  ) +
  geom_text(
    aes(label = round(media, 2)),
    hjust = -0.2,
    size = 3.5,
    family = "sans"
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(1, 5),
    breaks = 1:5,
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  tema_apa

## grafico 5


data2 |>
  tidyr::pivot_longer(C1:C6, names_to = "item", values_to = "valor") |>
  dplyr::filter(dplyr::between(valor, 1, 5)) |>
  dplyr::left_join(etiquetas_creencias, by = "item") |>
  dplyr::mutate(valor = factor(valor, levels = 1:5)) |>
  ggplot(aes(x = valor)) +
  geom_bar(
    fill = "grey80",
    color = "black",
    linewidth = 0.3
  ) +
  facet_wrap(~ texto, ncol = 3) +
  labs(
    x = NULL,
    y = NULL
  ) +
  tema_apa +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
    strip.background = element_blank(),
    strip.text = element_text(color = "black"),
    panel.spacing = unit(1, "lines")
  )

## grafico 6




# experimento escenarios ---------------

# https://studio.firebase.google.com/studio-6142797116


esc <- data2 |>
  select(id, optionsCount, scenarios) |>
  unnest(scenarios) |>
  transmute(
    id,
    optionsCount = factor(optionsCount, levels = c(2,3)),
    item,
    answer = factor(answer, levels = c("human","ia","both"))
  )

glimpse(esc)
table(esc$item)

esc |>
  count(optionsCount, answer) |>
  group_by(optionsCount) |>
  mutate(
    optionsCount = if_else(optionsCount == 2, "H/IA", "H/IA/Ambos"),
    pct = n / sum(n)
  ) |>
  ggplot(aes(x = optionsCount, y = pct, fill = answer)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c("human" = "#823dee", "ia" = "#00ffde", "both" = "#a89bff"),
    labels = c("Humano", "IA", "Ambos")
  ) +
  tema_ia +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 15)
  )

esc |>
  count(optionsCount, answer) |>
  group_by(optionsCount) |>
  mutate(
    optionsCount = if_else(optionsCount == 2, "H/IA", "H/IA/Ambos"),
    pct = n / sum(n),
    answer = factor(answer, levels = c("human", "ia", "both"))
  ) |>
  ggplot(aes(x = optionsCount, y = pct, fill = answer)) +
  geom_col(
    width = 0.6,
    color = "black",
    linewidth = 0.3
  ) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 3.5,
    family = "sans"
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = c(
      "human" = "grey85",
      "ia" = "grey55",
      "both" = "grey70"
    ),
    breaks = c("human", "ia", "both"),
    labels = c("Humano", "IA", "Ambos")
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  tema_apa +
  theme(
    legend.position = "bottom",
    legend.text = element_text(color = "black")
  )

## grafico 7

etiquetas <- tibble::tibble(
  item = c("E1","E2","E3","E4","E7","E8","E9","E10","E11","E12","E13","E14"),
  texto = c(
    "Decisión controversial",
    "Analizar argumentos en discusión",
    "Consulta experta",
    "Preguntas generales de cultura",
    "Ayuda en crisis emocional",
    "Hablar de sentimientos",
    "Sugerencia msj pareja",
    "Charlar sobre temas cotidianos",
    "Diseñar investigación científica",
    "Redacción académica",
    "Enseñar conocimientos",
    "Elaborar material educ."
  )
)

esc |>
  mutate(optionsCount = if_else(optionsCount == 2, "H/IA", "H/IA/Ambos")) |>
  count(item, optionsCount, answer) |>
  group_by(item, optionsCount) |>
  mutate(p = n / sum(n)) |>
  ungroup() |>
  left_join(etiquetas, by = "item") |>
  ggplot(aes(x = texto, y = p, fill = answer)) +
  geom_col() +
  geom_text(
    aes(label = scales::percent(p, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  coord_flip() +
  facet_wrap(~ optionsCount) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c("human" = "#823dee", "ia" = "#00ffde", "both" = "#a89bff"),
    name = element_blank(),
    labels = c("Humano", "IA", "Ambos")
  ) +
  tema_ia +
  theme(
    base_size = 12,
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 15)
  )





esc |>
  mutate(optionsCount = if_else(optionsCount == 2, "H/IA", "H/IA/Ambos")) |>
  count(item, optionsCount, answer) |>
  group_by(item, optionsCount) |>
  mutate(p = n / sum(n)) |>
  ungroup() |>
  left_join(etiquetas, by = "item") |>
  mutate(
    answer = factor(answer, levels = c("human", "ia", "both")),
    optionsCount = factor(optionsCount, levels = c("H/IA", "H/IA/Ambos")),
    texto = factor(texto, levels = rev(etiquetas$texto))
  ) |>
  ggplot(aes(x = texto, y = p, fill = answer)) +
  geom_col(
    width = 0.7,
    color = "black",
    linewidth = 0.3
  ) +
  geom_text(
    aes(label = scales::percent(p, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black",
    family = "sans"
  ) +
  coord_flip() +
  facet_wrap(~ optionsCount) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = c("human" = "grey85", "ia" = "grey55", "both" = "grey70"),
    labels = c("Humano", "IA", "Ambos"),
    name = NULL
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  tema_apa +
  theme(
    legend.position = "bottom"
  )

## grafico 8

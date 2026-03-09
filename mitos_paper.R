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
summary(data2$actitud)
summary(data2$actitud4)


ggplot(data2, aes(x = reorder(workArea, actitud4, median), y = actitud4, fill = workArea)) +
  geom_violin(scale = "count", trim = FALSE, alpha = 0.7) +
  stat_summary(fun = median, geom = "point", color = "white") +
  coord_flip() +
  scale_x_discrete(labels = function(x) {
    n <- as.integer(table(data2$workArea)[x]); paste0(x, " (n=", n, ")")
  }) +
  labs(x = NULL, y = "Actitud (1–5)", title = "Actitud por área de trabajo (violín ∝ n)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

ggplot(data2, aes(x = actitud4)) +
  geom_histogram(binwidth = 0.25, fill = "#823dee", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(actitud4, na.rm = TRUE)),
             color = "#00ffde", linewidth = 0.8, linetype = "dashed") +
  scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
  tema_ia



ggplot(data2, aes(x = gender, y = actitud4, fill = gender)) +
  geom_boxplot(alpha = 0.8) +
  labs(x = NULL, y = "Actitud hacia la IA (1–5)", title = "Actitud por género") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
ggplot(data2, aes(x = education, y = actitud4, fill = education)) +
  geom_boxplot(alpha = 0.8) +
  labs(x = NULL, y = "Actitud hacia la IA (1–5)", title = "Actitud por nivel educativo") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
ggplot(data2, aes(x = uso_ia_frecuencia, y = actitud4, fill = uso_ia_frecuencia)) +
  geom_boxplot(alpha = 0.8) +
  labs(x = NULL, y = "Actitud hacia la IA (1–5)", title = "Actitud por frecuencia de uso de IA") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

glimpse(data2)

data2 %>%
  select(-scenarios,-A5r,-actitud,-actitud4,-actitud5,-startTime,-completionTimestamp,-submittedAt, -duration) %>%
  write.csv(file = 'actitudes.csv')




# creencias ------------------------

creencias <- data2 |> dplyr::select(C1:C6)

etiquetas_creencias <- tibble::tibble(
  item = c("C1","C2","C3","C4","C5","C6"),
  texto = c(
    "C1 OBJETIVIDAD Tomar decisiones justas ",
    "C2 EXPERTICIA Dar respuestas certeras",
    "C3 PSICOLOGIA Ofrecer orientación psicológica",
    "C4 COMPANIA Brindar compañía",
    "C5 CIENCIA Hacer investigación",
    "C6 ENSENAR Enseñar y guiar el aprendizaje"
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
  ggplot2::geom_hline(yintercept = mean(data2$actitud4), color = "red") +
  # geom_errorbar(aes(ymin = media - sd/10, ymax = media + sd/10), width = 0.1) +
  ggplot2::geom_text(ggplot2::aes(label = round(media, 2)), hjust = -0.3, size = 3) +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(limits = c(0,5)) +
  ggplot2::labs(x = NULL, y = "Promedio de acuerdo (1–5)",
                title = "Nivel de acuerdo con distintas creencias sobre la IA") +
  ggplot2::theme_minimal(base_size = 15)

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
  dplyr::group_by(texto) |>
  dplyr::summarise(
    n = dplyr::n(),
    media = round(mean(valor, na.rm = TRUE), 2),
    sd = round(sd(valor, na.rm = TRUE), 2),
    min = min(valor, na.rm = TRUE),
    max = max(valor, na.rm = TRUE)
  ) |>
  dplyr::arrange(desc(media)) |>
  pull(media) |> mean()



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
  mutate(optionsCount = if_else(optionsCount == 2, "H/IA", "H/IA/Ambos")) %>%
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = optionsCount, y = pct, fill = answer)) +
  geom_col(alpha = 0.9, width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c("human" = "#823dee", "ia" = "#00ffde", "both" = "#a89bff"),
    name = element_blank(),
    labels = c("Humano", "IA", "Ambos")
  ) +
  tema_ia +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 15)
  )

etiquetas <- tibble::tibble(
  item = c("E1","E2","E3","E4","E7","E8","E9","E10","E11","E12","E13","E14"),
  texto = c(
    "C1 E1 Decisión controversial",
    "C1 E2 Analizar argumentos en discusión",
    "C2 E3 Consulta experta",
    "C2 E4 Preguntas generales de cultura",
    "C3 E7 Ayuda en crisis emocional",
    "C3 E8 Hablar de sentimientos",
    "C4 E9 Sugerencia msj pareja",
    "C4 E10 Charlar sobre temas cotidianos",
    "C5 E11 Diseñar investigación científica",
    "C5 E12 Redacción académica",
    "C6 E13 Enseñar conocimientos",
    "C6 E14 Elaborar material educ."
  )
)

esc |>
  filter(optionsCount == 3) |>
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
  filter(optionsCount==3) |>
  filter(answer!="both") |>
  count(item,answer) |>
  group_by(item) |>
  summarize(n_2opciones = sum(n))


esc |>
  filter(optionsCount==3) |>
  filter(answer!="both") |>
  count(item,answer)




esc |>
  dplyr::filter(answer %in% c("human","ia")) |>
  dplyr::count(item, optionsCount, answer, name = "n") |>
  tidyr::pivot_wider(
    id_cols = c(item, optionsCount),
    names_from = answer,
    values_from = n,
    values_fill = 0
  ) |>
  dplyr::mutate(
    total = human + ia,
    pct_h = human / total,
    pct_ia = ia / total
  ) |>
  dplyr::select(item, optionsCount, human, ia, pct_h, pct_ia) |>
  tidyr::pivot_wider(
    id_cols = item,
    names_from = optionsCount,
    values_from = c(human, ia, pct_h, pct_ia),
    names_glue = "{.value}_{optionsCount}"
  ) |> dplyr::mutate(
  delta_ia = pct_ia_2 - pct_ia_3
)



esc |>
  dplyr::filter(optionsCount == 3) |>
  dplyr::filter(answer %in% c("human", "ia")) |>
  dplyr::count(item, answer) |>
  dplyr::group_by(item) |>
  dplyr::mutate(pct = n / sum(n)) |>
  dplyr::ungroup() |>
  dplyr::select(-n) |>
  tidyr::pivot_wider(
    id_cols = item,
    names_from = answer,
    values_from = pct
  )



esc |>
  dplyr::filter(optionsCount == 3) |>
  dplyr::filter(answer %in% c("human", "ia")) |>
  dplyr::count(item, answer, name = "n") |>
  dplyr::group_by(item) |>
  dplyr::mutate(pct = n / sum(n)) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(
    id_cols = item,
    names_from = answer,
    values_from = c(n, pct),
    names_glue = "{.value}_{answer}"
  )



comp_puros <- esc |>
  dplyr::filter(answer %in% c("human", "ia")) |>
  dplyr::count(item, optionsCount, answer, name = "n") |>
  dplyr::group_by(item, optionsCount) |>
  dplyr::mutate(pct = n / sum(n)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    optionsCount = dplyr::case_when(
      optionsCount == 2 ~ "H/IA",
      optionsCount == 3 ~ "H/IA/Ambos\n(sin Both)"
    )
  )

comp_puros |>
  dplyr::left_join(etiquetas, by = "item") |>
  ggplot2::ggplot(ggplot2::aes(x = texto, y = pct, fill = answer)) +
  ggplot2::geom_col() +
  ggplot2::geom_text(
    ggplot2::aes(label = scales::percent(pct, accuracy = 1)),
    position = ggplot2::position_stack(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ optionsCount) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::scale_fill_manual(
    values = c("human" = "#823dee", "ia" = "#00ffde"),
    labels = c("Humano", "IA"),
    name = NULL
  ) +
  tema_ia +
  ggplot2::theme(
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 15)
  )




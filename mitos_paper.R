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
    actitud = rowMeans(across(c(A1:A4, A5r)), na.rm = TRUE)
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

ggplot(data2, aes(x = actitud)) +
  geom_histogram(binwidth = 0.2, fill = "#823dee", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(actitud, na.rm = TRUE)),
             color = "#00ffde", linewidth = 0.8, linetype = "dashed") +
  scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
  tema_ia





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
    "C3 E5 Ayuda en crisis emocional",
    "C3 E6 Hablar de sentimientos",
    "C4 E7 Sugerencia msj pareja",
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



## AHORA EL DELTA LOCO -------------


ratio_results <- data2 |>
  dplyr::select(id, optionsCount, scenarios) |>
  tidyr::unnest(scenarios) |>
  dplyr::count(item, optionsCount, answer, name = "n") |>
  dplyr::filter(answer %in% c("human", "ia")) |>
  tidyr::pivot_wider(
    id_cols = c(item, optionsCount),
    names_from = answer,
    values_from = n,
    values_fill = 0
  ) |>
  dplyr::mutate(
    ratio_H_IA = dplyr::if_else(ia == 0, NA_real_, human / ia)
  ) |>
  tidyr::pivot_wider(
    id_cols = item,
    names_from = optionsCount,
    values_from = ratio_H_IA,
    names_prefix = "ratio_"
  ) |>
  dplyr::mutate(
    delta_ratio_H_IA = ratio_2 - ratio_3
  )

ratio_results

ratio_plot <- ratio_results |>
  tidyr::pivot_longer(
    cols = c(ratio_2, ratio_3),
    names_to = "cond",
    values_to = "ratio"
  ) |>
  dplyr::mutate(
    cond = dplyr::recode(cond,
                         ratio_2 = "H/IA (2 opciones)",
                         ratio_3 = "H/IA (3 opciones)"),
    item = factor(item, levels = paste0("E", 1:14))
  )

ggplot2::ggplot(ratio_plot, ggplot2::aes(x = ratio, y = item)) +
  ggplot2::geom_line(ggplot2::aes(group = item), color = "grey75") +
  ggplot2::geom_point(ggplot2::aes(color = cond), size = 3) +
  ggplot2::labs(
    x = "Ratio H/IA (más alto = más Humano relativo)",
    y = "Escenario",
    color = NULL
  ) +
  tema_ia +
  ggplot2::theme(legend.position = "bottom")




## PERO CON P?IA PARA LABURAR EL CAMBIO DE LOS QUE HICIERON OPCIONES EXCLUYENTES




delta_p_ia <- data2 |>
  dplyr::select(id, optionsCount, scenarios) |>
  tidyr::unnest(scenarios) |>
  dplyr::count(item, optionsCount, answer, name = "n") |>
  dplyr::filter(answer %in% c("human", "ia")) |>
  tidyr::pivot_wider(
    id_cols = c(item, optionsCount),
    names_from = answer,
    values_from = n,
    values_fill = 0
  ) |>
  dplyr::mutate(
    p_ia_cond = if_else(
      optionsCount == 2,
      ia / (human + ia),          # en 2 opciones H+IA=1
      ia / (human + ia)           # en 3 opciones renormaliza sobre H+IA
    )
  ) |>
  dplyr::select(item, optionsCount, p_ia_cond) |>
  tidyr::pivot_wider(
    id_cols = item,
    names_from = optionsCount,
    values_from = p_ia_cond,
    names_prefix = "p_ia_"
  ) |>
  dplyr::mutate(
    delta_p_ia = p_ia_2 - p_ia_3
  )

delta_p_ia


ggplot2::ggplot(
  delta_p_ia,
  ggplot2::aes(
    x = delta_p_ia,
    y = factor(item, levels = paste0("E", 1:14))
  )
) +
  ggplot2::geom_col(fill = "#00ffde") +
  ggplot2::geom_vline(xintercept = 0, linetype = 2) +
  ggplot2::labs(
    x = "Δ P(IA) = P2(IA) − P3(IA | H+IA)",
    y = "Escenario"
  ) +
  tema_ia











# ESCENARIOS Y CREENCIAS ----------------------







map_e_c <- tibble::tribble(
  ~item,  ~belief_id,
  "E1",   "C1",
  "E2",   "C1",
  "E3",   "C2",
  "E4",   "C2",
  "E7",   "C3",
  "E8",   "C3",
  "E9",   "C4",
  "E10",  "C4",
  "E11",  "C5",
  "E12",  "C5",
  "E13",  "C6",
  "E14",  "C6"
)

res <- data2 |>
  select(id, optionsCount, scenarios, C1:C6) |>
  filter(optionsCount == 3) |>
  unnest(scenarios) |>
  left_join(map_e_c, by = "item") |>
  pivot_longer(C1:C6, names_to = "belief", values_to = "belief_value") |>
  filter(belief == belief_id) |>
  group_by(item, belief_id, answer) |>
  summarise(
    mean_belief = mean(belief_value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

res_dom <- res |>
  dplyr::group_by(belief_id, answer) |>
  dplyr::summarise(
    mean_belief = mean(mean_belief, na.rm = TRUE),
    n = sum(n),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    belief_id = factor(belief_id, levels = paste0("C", 1:6)),
    answer = factor(answer, levels = c("human", "ia", "both"))
  )

ggplot2::ggplot(res_dom, ggplot2::aes(x = mean_belief, y = belief_id, color = answer)) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_line(ggplot2::aes(group = belief_id), alpha = .4) +
  ggplot2::scale_color_manual(
    values = c("human" = "#823dee", "ia" = "#00ffde", "both" = "#a89bff"),
    labels = c("Humano", "IA", "Ambos"),
    name = NULL
  ) +
  ggplot2::labs(
    x = "Creencia media en la capacidad de la IA (1–5)",
    y = "Dominio"
  ) +
  tema_ia +
  ggplot2::theme(
    legend.position = "bottom",
    legend.text = ggplot2::element_text(size = 12)
  )




res_2 <- data2 |>
  dplyr::select(id, optionsCount, scenarios, C1:C6) |>
  dplyr::filter(optionsCount == 2) |>
  tidyr::unnest(scenarios) |>
  dplyr::left_join(map_e_c, by = "item") |>
  tidyr::pivot_longer(
    C1:C6,
    names_to = "belief",
    values_to = "belief_value"
  ) |>
  dplyr::filter(belief == belief_id) |>
  dplyr::group_by(item, belief_id, answer) |>
  dplyr::summarise(
    mean_belief = mean(belief_value, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  )

res_all <- data2 |>
  dplyr::select(id, optionsCount, scenarios, C1:C6) |>
  tidyr::unnest(scenarios) |>
  dplyr::left_join(map_e_c, by = "item") |>
  tidyr::pivot_longer(C1:C6, names_to = "belief", values_to = "belief_value") |>
  dplyr::filter(belief == belief_id) |>
  dplyr::group_by(optionsCount, item, belief_id, answer) |>
  dplyr::summarise(
    mean_belief = mean(belief_value, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  )

res_dom_all <- res_all |>
  dplyr::group_by(optionsCount, belief_id, answer) |>
  dplyr::summarise(
    mean_belief = mean(mean_belief, na.rm = TRUE),
    n = sum(n),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    optionsCount = dplyr::recode(as.character(optionsCount), `2` = "H/IA", `3` = "H/IA/Ambos"),
    belief_id = factor(belief_id, levels = paste0("C", 1:6)),
    answer = factor(answer, levels = c("human", "ia", "both"))
  )





res_e_3 <- data2 |>
  dplyr::select(id, optionsCount, scenarios, C1:C6) |>
  dplyr::filter(optionsCount == 3) |>
  tidyr::unnest(scenarios) |>
  dplyr::left_join(map_e_c, by = "item") |>
  tidyr::pivot_longer(C1:C6, names_to = "belief", values_to = "belief_value") |>
  dplyr::filter(belief == belief_id) |>
  dplyr::group_by(item, belief_id, answer) |>
  dplyr::summarise(
    mean_belief = mean(belief_value, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  )

res_e_3 |>
  dplyr::mutate(
    item = factor(item, levels = unique(item)),
    answer = factor(answer, levels = c("human", "both", "ia"))
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = mean_belief, y = item, color = answer)) +
  ggplot2::geom_point(size = 3) +
  ggplot2::facet_wrap(~ belief_id, scales = "free_y", ncol = 2) +
  ggplot2::scale_color_manual(
    values = c("human" = "#823dee", "ia" = "#00ffde", "both" = "#a89bff"),
    labels = c("Humano", "IA", "Ambos"),
    name = NULL
  ) +
  ggplot2::labs(
    x = "Creencia media (1–5)",
    y = "Escenario (E)"
  ) +
  tema_ia +
  ggplot2::theme(legend.position = "bottom")

res_e_all <- data2 |>
  dplyr::select(id, optionsCount, scenarios, C1:C6) |>
  tidyr::unnest(scenarios) |>
  dplyr::left_join(map_e_c, by = "item") |>
  tidyr::pivot_longer(C1:C6, names_to = "belief", values_to = "belief_value") |>
  dplyr::filter(belief == belief_id) |>
  dplyr::group_by(optionsCount, item, belief_id, answer) |>
  dplyr::summarise(
    mean_belief = mean(belief_value, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    optionsCount = dplyr::recode(as.character(optionsCount), `2` = "H/IA", `3` = "H/IA/Ambos"),
    answer = factor(answer, levels = c("human", "ia", "both"))
  )

res_e_all |>
  ggplot2::ggplot(ggplot2::aes(x = mean_belief, y = item, color = answer)) +
  ggplot2::geom_point(size = 3) +
  ggplot2::facet_wrap(~ optionsCount, ncol = 2) +
  ggplot2::scale_color_manual(
    values = c("human" = "#823dee", "ia" = "#00ffde", "both" = "#a89bff"),
    labels = c("Humano", "IA", "Ambos"),
    name = NULL
  ) +
  ggplot2::labs(
    x = "Creencia media (1–5)",
    y = "Escenario (E)"
  ) +
  tema_ia +
  ggplot2::theme(legend.position = "bottom")

res_e_all2 <- res_e_all |>
  dplyr::left_join(etiquetas, by = "item") |>
  dplyr::mutate(y_lab = texto)

ggplot2::ggplot(res_e_all2, ggplot2::aes(x = mean_belief, y = y_lab, color = answer)) +
  ggplot2::geom_point(size = 3) +
  ggplot2::facet_wrap(~ optionsCount, ncol = 2) +
  ggplot2::scale_color_manual(
    values = c("human" = "#823dee", "ia" = "#00ffde", "both" = "#a89bff"),
    labels = c("Humano", "IA", "Ambos"),
    name = NULL
  ) +
  ggplot2::labs(x = "Creencia media (1–5)", y = NULL) +
  tema_ia +
  ggplot2::theme(legend.position = "bottom")














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
  ggplot(aes(x = optionsCount, y = prop_ia, fill = optionsCount)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white") +
  labs(
    x = "Condición experimental",
    y = "Proporción de respuestas 'IA'",
    title = "Comparación de proporciones 'IA' según cantidad de opciones"
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




# confianza (creencia) -> delegacion ---------------------------------

# Unidad: tarea × condición
# Variables:
# confianza_media_tarea
# delegacion_IA
# delegacion_Ambos
# delegacion_total_IA = IA + Ambos

# Modelo recomendado:
# regresión lineal o logística:
# delegacion_total_IA ~ confianza + condicion + confianza × condicion
# Esto responde: ¿la complementariedad aumenta la delegación incluso controlando por confianza?

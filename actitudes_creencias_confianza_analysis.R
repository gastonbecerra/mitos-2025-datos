library(tidyverse)
library(psych)

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
    axis.title = element_text(color = "black"),
    plot.margin = margin(5.5, 18, 5.5, 5.5)
  )

figure_output_dir <- "figures"
dir.create(figure_output_dir, showWarnings = FALSE)

save_figure <- function(plot, filename, width, height) {
  ggsave(
    filename = file.path(figure_output_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300
  )
  ggsave(
    filename = file.path(figure_output_dir, paste0(filename, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    units = "in"
  )
}

data <- readRDS(file = 'data/data.rds')


# data preparation ------------------------

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
    # AIAS-4 is defined by A1:A4; A5 is not part of the published score.
    attitude_aias4 = rowMeans(across(A1:A4), na.rm = TRUE)
  )


rm(p)

# confiabilidad de las escalas ------------------------

# AIAS-4: actitud general hacia la inteligencia artificial.
alpha_aias4 <- psych::alpha(
  data2 |> dplyr::select(A1:A4),
  warnings = FALSE
)
alpha_aias4

# CREEAI-6: creencias sobre capacidades específicas de la IA.
alpha_creeai6 <- psych::alpha(
  data2 |> dplyr::select(C1:C6),
  warnings = FALSE
)
alpha_creeai6

rm(alpha_aias4, alpha_creeai6)

# sociodemograficos -----------------------------

sample_summary <- data2 |>
  summarise(n = n(), edad_prom = mean(age, na.rm = TRUE), edad_sd = sd(age, na.rm = TRUE), edad_min = min(age, na.rm = TRUE), edad_max = max(age, na.rm = TRUE))

sample_by_gender_education <- data2 |>
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

sample_summary
sample_by_gender_education

rm(sample_summary, sample_by_gender_education)




# Figure 1: general AI attitude ---------------------

attitude_mean <- mean(data2$attitude_aias4, na.rm = TRUE)
attitude_summary <- data2 |>
  summarise(
    n = sum(!is.na(attitude_aias4)),
    mean = mean(attitude_aias4, na.rm = TRUE),
    median = median(attitude_aias4, na.rm = TRUE),
    sd = sd(attitude_aias4, na.rm = TRUE),
    q1 = quantile(attitude_aias4, 0.25, na.rm = TRUE),
    q3 = quantile(attitude_aias4, 0.75, na.rm = TRUE),
    min = min(attitude_aias4, na.rm = TRUE),
    max = max(attitude_aias4, na.rm = TRUE)
  )
attitude_summary

figure_01 <- data2 |>
  dplyr::filter(!is.na(attitude_aias4), between(attitude_aias4, 1, 5)) |>
  ggplot(aes(x = attitude_aias4)) +
  geom_histogram(
    binwidth = 0.25,
    boundary = 1,
    fill = "grey75",
    color = "black",
    linewidth = 0.3
  ) +
  geom_vline(
    xintercept = attitude_mean,
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
  tema_apa
save_figure(figure_01, "figure_01_general_attitude", 6.5, 3.75)

# Figure 2: AIAS-4 item distributions

items_likert <- data2 |>
  dplyr::select(A1, A2, A3, A4) |>
  dplyr::rename(
    "I find AI positive" = A1,
    "I am interested in learning to use AI" = A2,
    "AI can be useful in my daily life" = A3,
    "AI can improve my work or studies" = A4
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
        "Strongly disagree",
        "Disagree",
        "Neutral",
        "Agree",
        "Strongly agree"
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
items_likert

figure_02 <- ggplot(items_likert, aes(x = item, y = p, fill = respuesta)) +
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
  tema_apa
save_figure(figure_02, "figure_02_aias4_item_distributions", 7.5, 4.25)

# Figure 3: attitude by work area

work_area_n <- data2 |> count(workArea)
work_area_labels_en <- c(
  "administracion-finanzas" = "Administration / finance",
  "arte-cultura-diseno" = "Arts / culture / design",
  "comercio-ventas" = "Commerce / sales",
  "comunicacion-marketing" = "Communication / marketing",
  "consultoria-servicios-profesionales" = "Consulting / professional services",
  "educacion" = "Education",
  "gobierno-sector-publico" = "Government / public sector",
  "industria-manufactura" = "Industry / manufacturing",
  "otra-no-aplica" = "Other / not applicable",
  "salud-servicios-sociales" = "Health / social services",
  "tecnologia-informatica" = "Technology / IT"
)

figure_03 <- ggplot(
  data2 |> dplyr::filter(!is.na(workArea), !is.na(attitude_aias4)),
  aes(x = reorder(workArea, attitude_aias4, median, na.rm = TRUE), y = attitude_aias4)
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
    labels = \(x) paste0(
      unname(work_area_labels_en[x]),
      " (n=", work_area_n$n[match(x, work_area_n$workArea)], ")"
    )
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
save_figure(figure_03, "figure_03_attitude_by_work_area", 7.5, 4.75)

# Figure 4: attitude by AI-use frequency

figure_04 <- ggplot(
  data2 |> dplyr::filter(!is.na(uso_ia_frecuencia), !is.na(attitude_aias4)),
  aes(x = uso_ia_frecuencia, y = attitude_aias4)
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
  scale_x_discrete(
    labels = c(
      nunca = "Never",
      esporadicamente = "Sporadically",
      ocasional = "Occasionally",
      frecuente = "Frequently"
    )
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  tema_apa
save_figure(figure_04, "figure_04_attitude_by_use_frequency", 6.5, 3.75)

# análisis descriptivo y comparación por género -----------------------------

attitude_by_work_area <- data2 |>
  dplyr::filter(!is.na(workArea), !is.na(attitude_aias4)) |>
  dplyr::group_by(workArea) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(attitude_aias4),
    median = median(attitude_aias4),
    sd = sd(attitude_aias4),
    .groups = "drop"
  )

attitude_by_usage <- data2 |>
  dplyr::filter(!is.na(uso_ia_frecuencia), !is.na(attitude_aias4)) |>
  dplyr::group_by(uso_ia_frecuencia) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(attitude_aias4),
    median = median(attitude_aias4),
    sd = sd(attitude_aias4),
    q1 = quantile(attitude_aias4, 0.25),
    q3 = quantile(attitude_aias4, 0.75),
    .groups = "drop"
  )

attitude_by_work_area
attitude_by_usage

gender_analysis_data <- data2 |>
  dplyr::filter(gender %in% c("female", "male"), !is.na(attitude_aias4)) |>
  dplyr::mutate(gender = factor(gender, levels = c("female", "male")))

gender_t_test <- stats::t.test(
  attitude_aias4 ~ gender,
  data = gender_analysis_data,
  var.equal = TRUE
)

gender_group_summary <- gender_analysis_data |>
  dplyr::group_by(gender) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(attitude_aias4),
    sd = sd(attitude_aias4),
    .groups = "drop"
  )

gender_pooled_sd <- with(
  gender_group_summary,
  sqrt(
    ((n[1] - 1) * sd[1]^2 + (n[2] - 1) * sd[2]^2) /
      (n[1] + n[2] - 2)
  )
)

gender_cohens_d <- with(
  gender_group_summary,
  (mean[1] - mean[2]) / gender_pooled_sd
)

gender_group_summary
gender_t_test
gender_cohens_d

rm(
  attitude_summary,
  items_likert,
  work_area_n,
  attitude_by_work_area,
  attitude_by_usage,
  gender_analysis_data,
  gender_t_test,
  gender_group_summary,
  gender_pooled_sd,
  gender_cohens_d
)

# Figure 5: mean belief scores ------------------------

etiquetas_creencias <- tibble::tibble(
  item = c("C1","C2","C3","C4","C5","C6"),
  texto = c(
    "Making fair decisions",
    "Providing accurate answers",
    "Providing psychological guidance",
    "Providing companionship",
    "Conducting scientific research",
    "Teaching and guiding learning"
  )
)

belief_score <- data2 |>
  mutate(
    belief_mean = rowMeans(across(C1:C6), na.rm = TRUE)
  )

belief_summary <- belief_score |>
  summarise(
    n = sum(!is.na(belief_mean)),
    mean = mean(belief_mean, na.rm = TRUE),
    sd = sd(belief_mean, na.rm = TRUE),
    median = median(belief_mean, na.rm = TRUE),
    q1 = quantile(belief_mean, 0.25, na.rm = TRUE),
    q3 = quantile(belief_mean, 0.75, na.rm = TRUE)
  )
belief_summary

creencias_plot <- data2 |>
  tidyr::pivot_longer(
    cols = C1:C6,
    names_to = "item",
    values_to = "media"
  ) |>
  dplyr::group_by(item) |>
  dplyr::summarise(
    media = mean(media, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(etiquetas_creencias, by = "item")
creencias_plot

figure_05 <- ggplot(creencias_plot, aes(x = reorder(texto, media), y = media)) +
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
    yintercept = attitude_mean,
    linetype = "dashed",
    linewidth = 0.5,
    color = "black"
  ) +
  annotate(
    "text",
    x = 1.1,
    y = attitude_mean + 0.05,
    label = paste0("Overall AI attitude = ", round(attitude_mean, 2)),
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
save_figure(figure_05, "figure_05_mean_belief_scores", 7.5, 4.25)

# Figure 6: belief response distributions


belief_distributions <- data2 |>
  tidyr::pivot_longer(C1:C6, names_to = "item", values_to = "valor") |>
  dplyr::filter(dplyr::between(valor, 1, 5)) |>
  dplyr::left_join(etiquetas_creencias, by = "item")
belief_distributions

figure_06 <- belief_distributions |>
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
save_figure(figure_06, "figure_06_belief_distributions", 7, 4.5)

# correlation and regression analyses ------------------

correlation_data <- belief_score |>
  dplyr::transmute(
    attitude_aias4,
    belief_mean
  ) |>
  tidyr::drop_na()

correlation_spearman <- psych::corr.test(
  correlation_data,
  method = "spearman",
  adjust = "none",
  ci = 0.95
)
correlation_spearman
correlation_spearman$ci

regression_data <- belief_score |>
  dplyr::mutate(
    sex_male = dplyr::case_when(
      gender == "male" ~ 1,
      gender == "female" ~ 0,
      TRUE ~ NA_real_
    ),
    usage_frequency_numeric = as.numeric(
      factor(
        uso_ia_frecuencia,
        levels = c("nunca", "esporadicamente", "ocasional", "frecuente")
      )
    )
  ) |>
  dplyr::select(
    attitude_aias4,
    belief_mean,
    age,
    sex_male,
    usage_frequency_numeric
  ) |>
  tidyr::drop_na()

regression_model <- stats::lm(
  attitude_aias4 ~ belief_mean + age + sex_male + usage_frequency_numeric,
  data = regression_data
)
summary(regression_model)
confint(regression_model)

standardized_betas <- regression_data |>
  dplyr::mutate(
    dplyr::across(
      c(attitude_aias4, belief_mean, age, sex_male, usage_frequency_numeric),
      ~ as.numeric(scale(.x))
    )
  ) |>
  stats::lm(
    attitude_aias4 ~ belief_mean + age + sex_male + usage_frequency_numeric,
    data = _
  ) |>
  broom::tidy() |>
  dplyr::filter(term != "(Intercept)") |>
  dplyr::select(term, beta = estimate)
standardized_betas

regression_predictors <- regression_data |>
  dplyr::select(belief_mean, age, sex_male, usage_frequency_numeric)
vif_values <- diag(solve(stats::cor(regression_predictors)))
vif_values

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

# Figure 7: aggregate preferences by response architecture

preference_by_architecture <- esc |>
  count(optionsCount, answer) |>
  group_by(optionsCount) |>
  mutate(
    optionsCount = if_else(
      optionsCount == 2,
      "Human / AI",
      "Human / AI / Human-assisted AI"
    ),
    pct = n / sum(n),
    answer = factor(answer, levels = c("human", "ia", "both"))
  )
preference_by_architecture

figure_07 <- preference_by_architecture |>
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
    labels = c("Human", "AI", "Human-assisted AI")
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
save_figure(figure_07, "figure_07_preferences_by_architecture", 6.5, 4)

# Figure 8: preferences by scenario and response architecture

etiquetas <- tibble::tibble(
  item = c("E1","E2","E3","E4","E7","E8","E9","E10","E11","E12","E13","E14"),
  texto = c(
    "Controversial decision",
    "Analyzing arguments in a discussion",
    "Expert consultation",
    "General culture questions",
    "Help during an emotional crisis",
    "Talking about feelings",
    "Suggestion for replying to a partner's message",
    "Chatting about everyday topics",
    "Designing scientific research",
    "Writing research results",
    "Teaching new knowledge",
    "Preparing educational materials"
  )
)

preference_by_scenario <- esc |>
  mutate(
    optionsCount = if_else(
      optionsCount == 2,
      "Human / AI",
      "Human / AI / Human-assisted AI"
    )
  ) |>
  count(item, optionsCount, answer) |>
  group_by(item, optionsCount) |>
  mutate(p = n / sum(n)) |>
  ungroup() |>
  left_join(etiquetas, by = "item") |>
  mutate(
    answer = factor(answer, levels = c("human", "ia", "both")),
    optionsCount = factor(
      optionsCount,
      levels = c("Human / AI", "Human / AI / Human-assisted AI")
    ),
    texto = factor(texto, levels = rev(etiquetas$texto))
  )
preference_by_scenario

  figure_08 <- preference_by_scenario |>
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
    labels = c("Human", "AI", "Human-assisted AI"),
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
save_figure(figure_08, "figure_08_preferences_by_scenario", 10, 5.5)

rm(
  figure_01,
  figure_02,
  figure_03,
  figure_04,
  figure_05,
  figure_06,
  figure_07,
  figure_08,
  creencias_plot,
  belief_score,
  belief_summary,
  belief_distributions,
  correlation_data,
  correlation_spearman,
  regression_data,
  regression_model,
  standardized_betas,
  regression_predictors,
  vif_values,
  etiquetas_creencias,
  esc,
  preference_by_architecture,
  preference_by_scenario,
  etiquetas,
  attitude_mean,
  save_figure,
  figure_output_dir
)

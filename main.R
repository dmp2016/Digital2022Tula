library(dplyr)
library(ggplot2)
library(readr)


# Функция вычисления метрики
r2_score <- function(y, y_predict){
  1 - sum((y - y_predict)^2) / sum((y - mean(y)) ^ 2)
}

# Преобразование месяца в формате YY-mm в число
month_to_num <- function(str_month) {
  unname(sapply(str_month, FUN = function(x) {
    t <- unlist(strsplit(x, "-", fixed = T))
    return (as.integer(t[1]) * 12 + as.integer(t[2]) - 1) }))
}

# Прибавить к месяцу в формате YY-mm n месяцев
my_add_month <- function(str_month, n) {
  t_num <- month_to_num(str_month) + n
  unname(sapply(t_num, FUN = function(x) 
    paste0(
      sprintf("%02d", x %/% 12), 
      "-", 
      sprintf("%02d", x %% 12 + 1))))
}


# Подготовка датасета. Добавляем дополнительное поле VISIT_YEAR_MONTH, содержащее месяц VISIT_MONTH_YEAR из в формате YY-mm.
prepare_data <- function(df) {
  df$VISIT_YEAR_MONTH <- unname(sapply(df$VISIT_MONTH_YEAR,
                                       function(x) paste0(rev(
                                         unlist(strsplit(x, ".", fixed = T))), 
                                         collapse = "-")))
  return (df)
}


# Читаем и подготавливаем тренировочный датасет
df_train <- read_csv2("train_dataset_train.csv")
df_train <- prepare_data(df_train)

colnames(df_train)

# Читаем и подготавливаем тестовый датасет
df_test_final <- read_csv2("test_dataset_test.csv")
df_test_final <- prepare_data(df_test_final)

# Убеждаемся, что требуется построить прогноз на 22-04
unique(df_test_final$VISIT_MONTH_YEAR)


# Посмотрим динамику на примере нескольких болезней
df_temp <- df_train %>% 
  filter(MKB_CODE %in% c("J06.9", "Z11.5", "Z00.1", "Z00.0")) %>% 
  filter(ADRES == "Калининград") %>% 
  filter(PATIENT_SEX == 0) %>% 
  filter(AGE_CATEGORY == "children") %>% 
  arrange(VISIT_YEAR_MONTH)


ggplot(data = df_temp, aes(x = VISIT_YEAR_MONTH, y = PATIENT_ID_COUNT, group = 1)) +
  geom_point() +
  geom_path() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~MKB_CODE)



# Функция для проверки качества моделей.
# строим прогнозы на месяцы от 22-03 до 22-03 - cnt_month
# Возвращаем вектор с значениями полученных метрик
test_method <- function(fun_method, cnt_month){
  score_list <- c()
  
  for (k in 0:cnt_month) {
    cur_month <- my_add_month("22-03", -k)
    print(cur_month)
    df_test_temp <- df_train %>% filter(VISIT_YEAR_MONTH == cur_month)
    sc <- r2_score(df_test_temp$PATIENT_ID_COUNT, fun_method(df_train, df_test_temp))
    print(sc)
    score_list <- c(score_list, sc)
  }
  return (score_list)  
}


# Первый вариант. В качестве прогноза берется значение предыдущего месяца.
# Если его нет, то в качестве прогноза испльзуем 1.
predict_v1 <- function(df_train, df_test) {
  if (min(df_test$VISIT_YEAR_MONTH) != max(df_test$VISIT_YEAR_MONTH))
    stop("Поле VISIT_YEAR_MONTH содержит более одного значения")

  month_predict <- min(df_test$VISIT_YEAR_MONTH)
  month_prev <- my_add_month(month_predict, -1)
  print(paste("Month to predict", month_predict))
  print(paste("Month prev", month_prev))

  df_test_oper <- df_test %>% 
    select(PATIENT_SEX, MKB_CODE, ADRES, VISIT_YEAR_MONTH, AGE_CATEGORY) %>% 
    left_join(df_train %>% 
                filter(VISIT_YEAR_MONTH == month_prev) %>% 
                distinct() %>% 
                select(PATIENT_SEX, MKB_CODE, ADRES, AGE_CATEGORY, PATIENT_ID_COUNT), 
              by = c("PATIENT_SEX", 
                     "MKB_CODE",
                     "ADRES",
                     "AGE_CATEGORY"))

  df_test_oper$PATIENT_ID_COUNT[is.na(df_test_oper$PATIENT_ID_COUNT)] <- 1

  return (df_test_oper$PATIENT_ID_COUNT)
}


res_v1 <- test_method(predict_v1, 12)
res_v1
sd(res_v1)
mean(res_v1)


# Второй вариант. Берем не значение последнего месяца, а медиану значений последних трех месяцев,
predict_v2 <- function(df_train, df_test) {
  if (min(df_test$VISIT_YEAR_MONTH) != max(df_test$VISIT_YEAR_MONTH))
    stop("Поле VISIT_YEAR_MONTH содержит более одного значения")
  month_predict <- min(df_test$VISIT_YEAR_MONTH)
  print(paste("Month to predict", month_predict))

  depth = 3
  month_from <- my_add_month(month_predict, -depth)
  month_to <- my_add_month(month_predict, -1)
  print(paste("Month from:", month_from, " Month to:", month_to))
  
  df_test_oper <- df_test %>% 
    select(PATIENT_SEX, MKB_CODE, ADRES, VISIT_YEAR_MONTH, AGE_CATEGORY) %>% 
    left_join(df_train %>% 
                filter(VISIT_YEAR_MONTH >= month_from & VISIT_YEAR_MONTH <= month_to) %>% 
                # distinct() %>% 
                group_by(PATIENT_SEX, MKB_CODE, ADRES, AGE_CATEGORY) %>% 
                summarise(PATIENT_ID_COUNT_M = round(median(PATIENT_ID_COUNT)),
                          .groups = "drop"),
              by = c("PATIENT_SEX", 
                     "MKB_CODE",
                     "ADRES",
                     "AGE_CATEGORY"))
  
  df_test_oper$PATIENT_ID_COUNT <- df_test_oper$PATIENT_ID_COUNT_M
  df_test_oper$PATIENT_ID_COUNT[is.na(df_test_oper$PATIENT_ID_COUNT)] <- 1
  
  return (df_test_oper$PATIENT_ID_COUNT)
}


res_v2 <- test_method(predict_v2, 12)
res_v2

sd(res_v2)
mean(res_v2)


# Третий вариант. Пытаемся выявить тенденцию на основе значений последних трех месяцев.
# Если есть не все знаяения за последние три месяца, то как в первом варианте берем последнее значение.
# Если в итоге ничего найти не удалось, берем 1
predict_lm <- function(x, y) {
  if (length(x) == 3) {
    x <- month_to_num(x)
    x <- x - min(x)
    model <- lm(y ~ x)
    return (model$coefficients[1] + model$coefficients[2] * (max(x) + 1))
  } else {
    return (last(y, x))
  }
}


predict_v3 <- function(df_train, df_test) {
  if (min(df_test$VISIT_YEAR_MONTH) != max(df_test$VISIT_YEAR_MONTH))
    stop("Поле VISIT_YEAR_MONTH содержит более одного значения")
  month_predict <- min(df_test$VISIT_YEAR_MONTH)
  print(paste("Month to predict", month_predict))
  
  depth = 3
  month_from <- my_add_month(month_predict, -depth)
  month_to <- my_add_month(month_predict, -1)
  print(paste("Month from:", month_from, " Month to:", month_to))
  
  df_test_oper <- df_test %>% 
    select(PATIENT_SEX, MKB_CODE, ADRES, VISIT_YEAR_MONTH, AGE_CATEGORY) %>% 
    left_join(df_train %>% 
                filter(VISIT_YEAR_MONTH >= month_from & VISIT_YEAR_MONTH <= month_to) %>% 
                # distinct() %>% 
                group_by(PATIENT_SEX, MKB_CODE, ADRES, AGE_CATEGORY) %>% 
                summarise(PATIENT_ID_COUNT_L = predict_lm(VISIT_YEAR_MONTH, PATIENT_ID_COUNT),
                          CNT_GRP = n(), .groups = "drop"),
              by = c("PATIENT_SEX", 
                     "MKB_CODE",
                     "ADRES",
                     "AGE_CATEGORY"))
  
  df_test_oper$PATIENT_ID_COUNT <- df_test_oper$PATIENT_ID_COUNT_L
  df_test_oper$PATIENT_ID_COUNT[is.na(df_test_oper$PATIENT_ID_COUNT)] <- 1
  
  return (round(df_test_oper$PATIENT_ID_COUNT))
}

res_v3 <- test_method(predict_v3, 12)
res_v3

sd(res_v3)
mean(res_v3)


# Прогноз и запись результатов

df_test_final$PATIENT_ID_COUNT <- predict_v1(df_train, df_test_final)

df_res <- df_test_final %>% select(PATIENT_SEX, MKB_CODE, ADRES, VISIT_MONTH_YEAR, AGE_CATEGORY, PATIENT_ID_COUNT)

write_csv2(df_res, "my_result_new3.csv")


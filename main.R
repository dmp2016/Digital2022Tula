library(dplyr)
library(ggplot2)
library(readr)


r2_score <- function(y, y_predict){
  1 - sum((y - y_predict)^2) / sum((y - mean(y)) ^ 2)
}

df_train <- read_csv2("train_dataset_train.csv")

df_test <- read_csv2("test_dataset_test.csv")


df_train <- df_train %>% inner_join(df_test %>% select(MKB_CODE) %>% distinct(), by = "MKB_CODE")

df_train$VISIT_MONTH_YEAR <- unname(sapply(df_train$VISIT_MONTH_YEAR,
                                           function(x) paste0(rev(unlist(strsplit(x, ".", fixed = T))), collapse = "-")))


df_train_mkb <- df_train %>% 
  count(MKB_CODE, name = "cnt")


df_train_mkb <- df_train %>% 
  group_by(MKB_CODE, PATIENT_SEX, ADRES, AGE_CATEGORY) %>% 
  summarise(cnt = n(),
            cnt_pat = sum(PATIENT_ID_COUNT))


df_test_21 <- df_train %>% filter(VISIT_MONTH_YEAR == "21-04")
df_train_21 <- df_train %>% filter(VISIT_MONTH_YEAR < "21-04")


df_test_21 <- df_test
df_train_21 <- df_train


colnames(df_train_21)


df_test_21_oper <- df_test_21 %>% 
  left_join(df_train_21 %>% 
              filter(VISIT_MONTH_YEAR == "22-03") %>% 
              distinct() %>% 
              select(PATIENT_SEX, MKB_CODE, ADRES, AGE_CATEGORY, PATIENT_ID_COUNT_03 = PATIENT_ID_COUNT), 
            by = c("PATIENT_SEX", 
                   "MKB_CODE",
                   "ADRES",
                   "AGE_CATEGORY"))


df_train_21_grp <- df_train_21 %>% 
  group_by(PATIENT_SEX, MKB_CODE, ADRES, AGE_CATEGORY) %>% 
  summarise(PATIENT_ID_COUNT_03 = mean(PATIENT_ID_COUNT))



df_test_21_oper <- df_test_21 %>% 
  left_join(df_train_21_grp, 
            by = c("PATIENT_SEX", 
                   "MKB_CODE",
                   "ADRES",
                   "AGE_CATEGORY"))



sum(is.na(df_test_21_oper$PATIENT_ID_COUNT_03))

df_test_21_oper$PRED <- df_test_21_oper$PATIENT_ID_COUNT_03

df_test_21_oper$PRED[is.na(df_test_21_oper$PRED)] <- 1

r2_score(df_test_21_oper$PATIENT_ID_COUNT, df_test_21_oper$PRED)

# rm(df_train)

max(df_train$VISIT_MONTH_YEAR)

head(df_train, 100)


max(df_train$PATIENT_ID_COUNT)



df_res <- df_test_21_oper %>% 
  mutate(VISIT_MONTH_YEAR = "04.22") %>% 
  select(PATIENT_SEX, MKB_CODE, ADRES, VISIT_MONTH_YEAR, AGE_CATEGORY, PATIENT_ID_COUNT = PRED)

write_csv2(df_res, "my_result.csv")


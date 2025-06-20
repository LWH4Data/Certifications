# =================================================================================================
# 3-1. 선형관계가 가장 큰 변수를 찾아 상관계수를 구하여라.
# 파이썬 풀이가 더 쉬울 수 있음.
df <- read.csv('https://raw.githubusercontent.com/Datamanim/datarepo/main/krdatacertificate/e7_p3_1.csv')
head(df)

# 상관관계 행렬 생성
cor_mat <- cor(df)
cor_mat

# Target 컬러만 필터링
target_corr <- cor_mat['Target', ]
target_corr

# target_corr를 내림차순 정렬
target_sort <- sort(target_corr, decreasing = TRUE)
target_sort

# 두 번째 행의 값을 정답으로 출력
Ans <- target_sort[2]
round(as.numeric(Ans), 3)

# ================================================================================================
# 3-2. Target 변수를 종속변수로 하여 다중선형회귀모델을 모델링하라. 이때 v2 컬럼의 회귀 계수는?

# 모델링
reg_model <- lm(Target ~ ., data = df)
reg_model

# 회귀계수만 필터링
coef_lst <- coef(reg_model)
coef_lst

# 정답 반환
Ans <- coef_lst['v2']
as.numeric(Ans)

# ===============================================================================================
# 3-3. 회귀계수들이 갖는 p-value중 최대값은?
reg_summary <- summary(reg_model)
reg_summary

# p-value만 필터링 
p_values <- coef(reg_summary)[, 4]

# p-value로 내림차순 정렬
p_sort <- sort(p_values, decreasing = TRUE)
p_sort

# 정답 반환
Ans <- p_sort['v21']
as.numeric(Ans)

# ================================================================================================
# 3-4. train 데이터로 target을 결과변수로 로지스틱 회귀를 진행할 때 age 컬럼의 오즈비를 구하여라.
# 마님은 python을 사용하기에 상수항 포함 여부 및 R의 glm과 lm 함수의 내부 동작 때문에 답에는 근소한 차이가 발생한다.
# train데이터는 앞의 210의 행을 test 데이터는 이후를 사용.
df <- read.csv('https://raw.githubusercontent.com/Datamanim/datarepo/main/krdatacertificate/e7_p3_t.csv')
head(df)

train <- df[1 : 210, ]
test <- df[211 : nrow(df), ]

# 우선 로지스틱회귀 모델링
logit_model <- glm(target ~ ., data = train, family = binomial)
logit_model

# 회귀계수만 출력
logit_coefs <- coef(logit_model)
logit_coefs

# 로지스틱회귀계수에 exp()를 취하면 오즈비를 구할 수 있다.
Ans <- exp(logit_coefs['age'])
as.numeric(Ans)

# ===============================================================================================
# 3-5. train 데이터로 로지스틱회귀를 진행했을 경우 잔차 이탈도를 계산하라.

# 3-4에서 적합한 모델을 그대로 사용하여 summary()를 초기화
logit_summary <- summary(logit_model)
logit_summary

# 잔차 이탈도(Residual deviance를 정답으로 출력)
Ans <- logit_summary$deviance
as.numeric(Ans)

# ===============================================================================================
# 3-6. train 데이터로 로지스틱 회귀한 결과를 통해 우도값을 출력하라.
Ans <- logLik(logit_model)
Ans

# ================================================================================================
# 3-7. test 데이터의 독립변수로 target 예측 후 오류율을 구하여라.

# 예측수행
pred_proba <- predict(logit_model, newdata = test)

# 0.5를 threshold로 이진 분류
pred_class <- ifelse(pred_proba >= 0.5, 1, 0)

# 실제값을 초기화
actual <- test$target

# 혼동행렬 생성
conf_mat <- table(Predicted = pred_class, Actual = actual)
conf_mat

# Accuracy 계산 
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
accuracy

# 오류율 계산
Ans <- 1 - accuracy
as.numeric(Ans)

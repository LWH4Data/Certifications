# ======================================================================================================
# 3-1. y컬럼을 종속변수로하는 로지스틱회귀 모형을 만들 때 유의미하지 않은 변수의 개수는? 
# (상수항을 추가하여 모델링하라).

# 데이터 불러오기
df <- read.csv('https://raw.githubusercontent.com/Datamanim/datarepo/main/krdatacertificate/e8_p3_1.csv')
head(df)

# 로지스틱 회귀 적합 후 결과 초기화
#   - 상수항을 포함하지 않을 경우: `Y ~ .`에 `+0` 혹은 `-1` 
logit_model <- glm(Y ~ ., data = df, family = binomial)
logit_model_summary <- summary(logit_model)

# p-value들만 필터링
#   - coef(): logit_model_summary에서 회귀계수와 관련된 수치만 필터링
p_values <- coef(logit_model_summary)[ , 4]

# 유의미하지 않은 변수의 수만 출력
#   - p_values[-1]: 첫 번째 컬럼이 Intercept로 상수항이기에 제외.
Ans <- sum(p_values[-1] > 0.05)
cat('유의미하지 않은 변수의 수: ', Ans)

# ======================================================================================================
# 3-2. 유의미한 변수들만으로 다시 모델링을 해서 X를 포함하는 설명변수들의 회귀계수의 합계를 소수점 이하 세 번째 자리까지 구하여라.

# 유의미한 변수들만 필터링
sig_vars <- names(p_values)[-1][p_values[-1] <= 0.05]
sig_vars

# 유의미한 변수들로만된 formula 생성
sig_formula = as.formula(paste('Y ~', paste(sig_vars, collapse = ' + ')))
sig_formula

# 위의 식을 활용하여 다시 로지스틱 회귀 수행
sig_logit_model <- glm(sig_formula, data = df, family = binomial)

# 회귀계수 필터링
#   - 회귀계수만 필터링하는 경우 summary() 필요없이 모델에 바로 coef()를 하면 된다.
coef_sig <- coef(sig_logit_model)
coef_sig

# 변수의 시작이 X인 경우만 필터링.
X_coefs <- coef_sig[grepl('^X', names(coef_sig))]
X_coefs

# 합계를 구하고 답출력
Ans <- round(sum(X_coefs), 3)
Ans

# =======================================================================================================
# 3-3. 3-2번의 모델 결과에 대해 X뒤의 숫자가 가장 낮은 컬럼이 5단위 늘어나면 오즈비가 몇 배로 변화하는가?

# 3-2번의 변수명만을 필터링
X_names <- names(X_coefs)
X_names

# 변수 중 숫자만 필터링
x_nums <- as.numeric(gsub('X', '', X_names))
x_nums

# 숫자가 가장 작은 컬럼명의 index를 반환
min_idx <- which.min(x_nums)
min_idx

# index를 이용하여 변수명을 출력
min_var <- X_names[min_idx]
min_var

# 회귀계수 필터링
beta <- X_coefs[min_idx]
beta

# 필터링한 회귀계수를 5단위 늘렸을 때의 오즈비 변화 계산
Ans <- exp(5 * beta)
Ans

# =======================================================================================================
# 3-4. PIQ를 예측하는 다중선형회ㅐ귀 모형을 생성하고 Brain의 회귀계수를 구하여라.
df <- read.csv('https://raw.githubusercontent.com/Datamanim/datarepo/main/krdatacertificate/e8_p3_2.csv')
head(df)

# 회귀모델 적합
reg_model <- lm(PIQ ~ ., data = df)

# 회귀계수만 필터링
coefs <- coef(reg_model)
coefs

# Brain의 회귀계수만 필터링
Ans <- coefs['Brain']
Ans

# ========================================================================================================
# 3-5. 모델의 R^2를 구하여라.

# R^2를 필터링하기 위해 summary()를 통한 모델 요약 초기화
reg_summary <- summary(reg_model)

# reg_sumamry에서 r.squared만 필터링
Ans <- reg_summary$r.squared
Ans

# ========================================================================================================
# 3-6. 위에서 계산한 모델을 이용하여 Brain: 90, Height: 80, Weight: 150 일 때 PIQ를 계산하라.

# 예측에 사용할 설명변수들로 데이터프레임 생성
new_data <- data.frame(
    Brain = 90,
    Height = 80,
    Weight = 150
)

# 예측 수행
Ans <- predict(reg_model, newdata = new_data)
as.numeric(Ans)
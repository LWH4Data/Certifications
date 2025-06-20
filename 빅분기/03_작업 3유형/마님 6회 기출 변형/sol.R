# ==================================================================================================
# 3-1. A 도시의 남성 600 명과 여성 550 명이 있다. 남성들 중 흡연자 비율은 0.2이고,
# 여성들 중 흡연자 비율은 0.26이다. 
# 이때 남성과 여성 간 흡연 여부에 따른 인구 비율이 다른지 확인하려 한다. 
# 유의 수준 0.05하에 귀무가설에 대해 기각 / 채택 여부와 p-value를 출력하라.

# 검정을위한 데이터 설정
smoke <- c(600 * 0.2, 550 * 0.26)
gender <- c(600, 550)

# 검정 수행
prop.test(smoke, gender)

# 답: p-value = 0.018로 귀무가설을 기각한다. 남과 여간 흡연 여부에 따른 인구 비율에는 유의미한 차이가 있다.

# ===================================================================================================
# 3-2-a. age와 Cholesterol을 갖고 weight를 예측하는 선형회귀모델을 모델링한다. age의 회귀계수를 구하여랴.
df <- read.csv('https://raw.githubusercontent.com/Datamanim/datarepo/main/adp/28/p7.csv')
head(df)

# 회귀모델 적합
reg_model <- lm(weight ~ ., data = df)
reg_model

# age의 회귀계수 출력
Ans = coef(reg_model)['age']
as.numeric(Ans)

# ===================================================================================================
# 3-2-b. age가 고정일 때 Cholesterol과 weight가 선형관계가 있다는 가설을 유의수준 0.05하에서 검정하라.

# 적합한 회귀 모델의 각 변수별 p-value를 확인하는 문제 t-test이기 때문.
reg_summary <- summary(reg_model)
reg_summary

# 정답 확인을 위해 Cholesterol의 p-value 출력
#   - 현재 데이터 형식이 matrix이기에 ['행 이름', '열 이름']으로 인덱싱
Ans <- reg_summary$coefficient['Cholesterol', 'Pr(>|t|)']
Ans
# 답: p-value가 0.000이기에 유의하다. 즉, 선형관게가 있다.

# ===================================================================================================
# 3-2-c. age가 55, Cholesterol이 72.6일 때 위 모델을 기반으로 weight를 예측하라.

# 데이터 생성
new_data <- data.frame(
    age = 55,
    Cholesterol = 72.6
)

# 예측 수행
pred <- predict(reg_model, newdata = new_data)
pred

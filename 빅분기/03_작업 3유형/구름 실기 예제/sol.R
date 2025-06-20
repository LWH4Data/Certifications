df <- read.csv('구름 실기 예제/resistin_data.csv')
print(head(df))

# 컬럼 정보
#   - Resistin: 리지스틴 수치(ng/mL)
#   - Classification: 실험자 정보[1: 정상, 2: 환자]

# ================================================================================================================================
# 1. 두 집단의 로그 리지스틴 값의 분산에 차이가 있는지를 알아보기 위해 F-test를 수행할 때 검정통계량을 구하여라.
# 단, 분자의 자유도가 분모의 자유도보다 크도록 하여라.
# 로그 변환
df$log_resistin <- log(df$Resistin)
head(df$log_resistin)

# 검정을 위해 두 집단을 분리.
group1 <- df$log_resistin[df$Classification == 1]
group2 <- df$log_resistin[df$Classification == 2]

# 분산 검정(F-test)이전에 정규성 검정 수행
shapiro1 <- shapiro.test(group1)
shapiro2 <- shapiro.test(group2)
cat("정규성 검정 결과")
cat(sprintf("   그룹 1의 p-value: %.4f\n", shapiro1$p.value)) # 0.0342
cat(sprintf("   그룹 2의 p-value: %.4f\n", shapiro2$p.value)) # 0.3845

# 분자의 자유도가 더 커야 하기에 데이터의 수를 계산
length(group1)
length(group2)

# group2의 자유도가 더 크기에 group2를 분자로 분산분석 수행
result <- var.test(group2, group1)

# 통계량 출력
cat(sprintf('검정통계량: %.3f', result$statistic))


# ===============================================================================================================================
# 2. 두 집단의 로그 리지스틴 값에 대한 합동 분산 추정량을 계산하라.
# (반올림하여 소수 셋째 자리까지 출력하라).

# 합동 분산 추정량
#   1) 독립적인 두 개 이상의 표본 집단의 분산을 통합하여 추정. 단, 이때 각 집단의 분산이 동일하다는 가정을 함.
#   2) 계산 식: (아래를 분수처럼 볼 것).
#       분자: ((첫 번째 표본의 크기 - 1) * 첫 번째 표본의 분산 + (두 번째 표본의 크기 - 1) * 두 번째 표본의 분산)) 
#       분모:                     / (첫 번쨰 표본의 크기 + 두 번째 표본의 크기 - 2)

# 필요한 변수들 계산
n1 <- length(group1)
n2 <- length(group2)
var1 <- var(group1)
var2 <- var(group2)

# 정답 계산 및 출력
sp <- ((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2)
cat(sprintf('합동 분산 추정량: %.3f', sp))

# ================================================================================================================================
# 3. 2번 문제에서 계산한 합동 분산 추정량을 이용하여 두 집단의 로그 리지스틴 값에 유의미한 차이가 있는지
# 독립표본 t-test를 수행하고 p-value를 구하여라. (반올림하여 소수 셋째 자리까지 작성).

result <- t.test(group1, group2 )
cat(sprintf('t-test의 p-value: %.3f', result$p.value))

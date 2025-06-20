df <- read.csv('구름 실기 예제/resistin_data.csv')
print(head(df))

# 컬럼 정보
#   - Resistin: 리지스틴 수치(ng/mL)
#   - Classification: 실험자 정보[1: 정상, 2: 환자]

# 1. 로그리지스틴 값의 F-test 결과========================================================================
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

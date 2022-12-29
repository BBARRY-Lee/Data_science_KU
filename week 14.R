Data <- USArrests # 미국의 주 (50개 주)별 강력범죄 자료
str(Data)
head(Data)

# 변수 요약
apply(Data, 2, mean) # 1 = 행 기준, 2 = 열 기준
apply(Data, 2, sd)
summary(Data)

# 주성분 분석
# PCA 결과 5개의 리스트로 이뤄져 있음 (sdev, rotation, center, scale, x)
pr.out = prcomp(Data, center = TRUE, scale = TRUE)
str(pr.out) 

pr.out$center # 분석 전 평균(center)과 표준편차(scale)
pr.out$rotation # PC1 ~ PC4 : 주성분들 (요인)

biplot(pr.out, scale = 0)
# 빨간선 : 변수, 검은색 : data
# 빨간색은 주성분에 따른 변수의 기여하는 값 (고유값)
# 검은색 50개 주로 변환된 데이터를 통해 축에 나타낸 것
# > head(Data)로 나온 결과값 참고

# 고유값 구하기
pr.out$sdev
pr.var = pr.out$sdev^2 # 제곱하여 주성분 분산 계산
pve = pr.var/sum(pr.var)
pve # 주성분의 설명 비율

# scree plot 그리기
plot(pve, xlab="Principal Componant", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = "line")
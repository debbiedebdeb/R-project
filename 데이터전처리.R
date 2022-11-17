cust_data <- read.csv("CUST_DATA.csv", sep = ',', encoding = "CP949", fileEncoding = "UCS-2")
claim_data <- read.csv("CLAIM_DATA.csv", sep = ',', encoding = "CP949", fileEncoding = "UCS-2")
sagi_data <- subset(cust_data, subset = (cust_data$SIU_CUST_YN=='Y'))

############EDA

#1 SIU_  - 범주형 / null변환필요
siu <- table(cust_data$SIU_CUST_YN)
pie(table(cust_data$SIU_CUST_YN), main='사기꾼 분류', labels = paste(c('분석필요 ','정상인', '사기꾼'), siu, '명'))
siu_pc <- round(siu/sum(siu)*100,2)
pie(siu_pc, main='사기꾼 비율', labels = paste(c('분석필요 ','정상인', '사기꾼'), '\n', siu_pc, '%', '\n', siu, '명'),cex=1.2)

sum(siu)

#2. wedd - 범주형 / 결측치 변환 필요 
w <- table(sagi_data$WEDD_YN)
sum(w)
pie(w, main = '결혼유무별 사기꾼 분포', labels=paste(c('분석필요','미혼','기혼'), w, '명'), cex=1)
w_pc <- round(w/table(cust_data$WEDD_YN)*100,2)
pie(w_pc, main='결혼유무별 사기꾼 비율', labels = paste(c('분석필요 ','미혼', '기혼'), '\n', w_pc, '%', '\n', w, '명'),cex=1.2)


#3. ctpr - 범주형 / 문자를 숫자로 라벨링
c2 <- table(cust_data$CTPR)
c <- table(sagi_data$CTPR)
c <- append(c, 0, after = 15)
barplot(c, ylim=c(0,400), main='지역별 사기꾼 수')
c_pc <- round(c/c2*100,2)
ctprplot <- barplot(c_pc, main="지역별 사기꾼 비율", ylim=c(0,20), col=10, axis.lty=3)
c

#4. SEX - 명목형 / 결측치 없음
s <- table(sagi_data$SEX)
s2 <- table(cust_data$SEX)
s
s2
pie(s, main = '사기꾼 성별 분포', labels = paste(c('남','여'), s, '명'), cex=1.2)
s_pc <- round(s/s2*100,2)
pie(s_pc, main='성별 사기꾼 비율', labels = paste(c('남','여'), '\n', s_pc, '%', '\n', s, '명'), cex=1.2)


#5. AGE - 연속형 / 구간화 필요

#구간화(전처리)
age.fun <- function(data){
  data%/%10
}
cust_data$AGE <- sapply(cust_data$AGE, age.fun)
sagi_data <- subset(cust_data, subset = (cust_data$SIU_CUST_YN=='Y'))

a<- table(sagi_data$AGE)
a2 <- table(cust_data$AGE)
a_pc <- round(a/a2*100,2)
a <- append(a,0)
names(a_pc)<- c("10세미만","10대","20대","30대","40대","50대","60대","70대","80대")
barplot(a, ylim = c(0,600), main = '연령대별 사기꾼 수', xlab='나이구간', ylab='명')
barplot(a_pc, ylim=c(0,12), main = '연령대별 사기꾼 비율', xlab='나이구간', ylab='비율(%)', col=10)


#7. occp1 - 범주형 / 결측치 처리 및 라벨링 필요
o <- table(sagi_data$OCCP_GRP_1)
o2 <- table(cust_data$OCCP_GRP_1)
o_pc <- round(o/o2*100,2)
barplot(o, ylim = c(0,500), main = '직업별 사기꾼 수', xlab='직업종류', ylab='명')
barplot(o_pc, ylim=c(0,15), main = '직업별 사기꾼 비율', xlab='직업종류', ylab='비율(%)', col=10, names.arg = c('', occp_name))

?barplot
#라벨링(전처리)
occp.fun <- function(data){
  data = substr(data, 1,1)
  data = as.numeric()
  if(data==''){
    data='0'
  }else{data=data}
}

cust_data$OCCP_GRP_1 <- sapply(cust_data$OCCP_GRP_1, occp.fun)

#6. mate  occp1 - 범주형 / 결측치 처리 및 라벨링 필요
m <- table(sagi_data$MATE_OCCP_GRP_1)
m2 <- table(cust_data$MATE_OCCP_GRP_1)
m_pc <- round(m/m2*100,2)
barplot(m, ylim = c(0,1000), main = '배우자 직업별 사기꾼 수', xlab='직업종류', ylab='명')
barplot(m_pc, ylim=c(0,15), main = '배우자 직업별 사기꾼 비율', xlab='직업종류', ylab='비율(%)', col=10, names.arg = c('', occp_name))
occp_name <- c('주부','자영업','사무직','전문직','서비스','제조업','1차산업','기타')
sum(m2)
cust_data$MATE_OCCP_GRP_1 <- sapply(cust_data$MATE_OCCP_GRP_1, occp.fun)

#8. 주거 타입 코드 - 범주형 / 결측치 처리 필요(보니까 1~20대) - 제거하자 
summary(cust_data$RESI_TYPE_CODE)
r2<- table(cust_data$RESI_TYPE_CODE)
r<- table(sagi_data$RESI_TYPE_CODE)
r
r[4]/sum(r)
r_pc <- round(r/r2*100,2)
barplot(r, ylim = c(0,800), main = '주거타입별 사기꾼 수', xlab='주거타입', ylab='명')
barplot(r_pc,  ylim=c(0,15), main='주거타입코드 별 사기꾼 비율', xlab='주거타입', ylab='비율(%)', col=10)


write.csv(cust_data, "cust_data1", row.names = FALSE)



########전처리 

cust_data <- read.csv("cust_data1")

###결측치 처리 및 라벨 인코딩 
#1 siu -- 1. null는 다 N으로 처리. 2. 1,0을 y,n 로 처리 
data <- cust_data$SIU_CUST_YN

yn.fun <- function(data){
  if(data =='Y'){
    data=1
  }else if (data ==''){
    data=0
  }else {data=0}
}

cust_data$SIU_CUST_YN <- sapply(cust_data$SIU_CUST_YN, yn.fun)
table(cust_data$SIU_CUST_YN)

#2. wedd
table(cust_data$WEDD_YN)
cust_data$WEDD_YN <- sapply(cust_data$WEDD_YN, yn.fun)

#3. CTPR
area.name <- levels(as.factor(cust_data$CTPR))
cust_data$CTPR <- as.numeric(as.factor(cust_data$CTPR))

#4 OCCP / mOCCP - 전처리 (위에서 함)

#라벨링(전처리)
occp.fun <- function(data){
  data = substr(data, 1,1)
  if(data==''){
    data='0'
  }else{data=data}
}

cust_data$OCCP_GRP_1 <- sapply(cust_data$OCCP_GRP_1, occp.fun)
cust_data$MATE_OCCP_GRP_1 <- sapply(cust_data$MATE_OCCP_GRP_1, occp.fun)

#5 소득(CUST_INCM) 결측치 - 평균으로 넣기 
 #5-1 직업별 평균으로 넣었을때. 
occpavg <- round(tapply(cust_data$CUST_INCM, cust_data$OCCP_GRP_1, mean, na.rm=TRUE))
cust_data$OCCP_GRP_1 <- as.numeric(cust_data$OCCP_GRP_1)
occpna.fun <- function(income, occp){
  if(is.na(income)){
    income=occpavg[occp+1]
  } else {income=income}
}

cust_data$CUST_INCM<- mapply(FUN = occpna.fun, cust_data$CUST_INCM, cust_data$OCCP_GRP_1)


#5-2 0원으로 넣었을때 



#6 min/max crdt na to 6
min <- cust_data$MINCRDT
max <- cust_data$MAXCRDT

cust_data$MINCRDT <- ifelse(is.na(min), 6, min)
cust_data$MAXCRDT <- ifelse(is.na(max), 6, max)


 # 상관관계를 봐야해 (신용등급과 소득간에 상관관계가 높을까? )
cor(cust_data$MINCRDT, cust_data$SIU_CUST_YN) #-- 열라리 없대 
cor(cust_data$MINCRDT, cust_data$CUST_INCM) #-- 열라리 없대 

###이상치 확인 28,99는 삭제 (어차피 이들 중 사기꾼 없음)
boxplot(cust_data$MINCRDT)
boxplot(cust_data$MAXCRDT)
cust_data<- subset(cust_data, subset = (cust_data$MAXCRDT <28))


#7 totalprem 결측치 0 으로 넣기 - 학생이랑 주부가 많음

total <- cust_data$TOTALPREM
cust_data$TOTALPREM <- ifelse(is.na(total), 0, total)

#8 거주지 타입코드를 결측치 0 으로 넣기 

resicode <- cust_data$RESI_TYPE_CODE
cust_data$RESI_TYPE_CODE <- ifelse(is.na(resicode), 0, resicode)

##저장
write.csv(cust_data, "cust_data2", row.names = FALSE)
cust_data <- read.csv("cust_data2")


###인코딩 - 지역 
temp <- subset(cust_data, select=c('CUST_ID','CTPR'))
temp$value <- 1
library(reshape2)
temp_dummy <- dcast(data=temp, CUST_ID~CTPR, fun=sum)
names(temp_dummy) <- c('CUST_ID', area.name)
cust_data <- merge(cust_data, temp_dummy, by='CUST_ID')

                     
##저장
write.csv(cust_data, "cust_data3", row.names = FALSE)
cust_data <- read.csv("cust_data3")


# 변수삭제 
colnames(cust_data)
cust_data <- subset(cust_data, select=-c(CTPR,FP_CAREER,CUST_RGST,OCCP_GRP_2,MATE_OCCP_GRP_2,CHLD_CNT,LTBN_CHLD_AGE,MAX_PAYM_YM,MAX_PRM, RCBASE_HSHD_INCM))

##저장
write.csv(cust_data, "cust_data4", row.names = FALSE)
cust_data <- read.csv("cust_data4")



                    
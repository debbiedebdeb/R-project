cust_data <- read.csv("CUST_DATA.csv",sep = ',', encoding = "CP949", fileEncoding = "UCS-2")
claim_data <- read.csv("CLAIM_DATA.csv", sep = ',', encoding = "CP949", fileEncoding = "UCS-2")

#[1] 기본 전처리(cust_data1) - 
#[인코딩] siu  
yn.fun <- function(data){
  if(data =='Y'){  data=1  }
  else if (data == 'N'){data=0}
  else { data =''}
}

cust_data$SIU_CUST_YN <- sapply(cust_data$SIU_CUST_YN, yn.fun)
table(cust_data$SIU_CUST_YN)

#[인코딩] wedd - ynto10
cust_data$WEDD_YN <- sapply(cust_data$WEDD_YN, yn.fun)

#[인코딩]CTPR - C to N 
area.name <- levels(as.factor(cust_data$CTPR))
cust_data$CTPR <- as.numeric(as.factor(cust_data$CTPR))

#[인코딩] - OCCP / mOCCP - C to N
occp.fun <- function(data){
  data = substr(data, 1,1)
  if(data==''){
    data='0'
  }else{data=data}
}

cust_data$OCCP_GRP_1 <- sapply(cust_data$OCCP_GRP_1, occp.fun)
cust_data$OCCP_GRP_1 <- as.numeric(cust_data$OCCP_GRP_1)
cust_data$MATE_OCCP_GRP_1 <- sapply(cust_data$MATE_OCCP_GRP_1, occp.fun)
cust_data$MATE_OCCP_GRP_1 <- as.numeric(cust_data$MATE_OCCP_GRP_1)

#[결측치] MIN MAX CRDT
min <- cust_data$MINCRDT
max <- cust_data$MAXCRDT

cust_data$MINCRDT <- ifelse(is.na(min), 6, min)
cust_data$MAXCRDT <- ifelse(is.na(max), 6, max)

#[결측치] totalprem 결측치 0에 넣기 
total <- cust_data$TOTALPREM
cust_data$TOTALPREM <- ifelse(is.na(total), 0, total)

#[결측치] 거주지 타입코드를 결측치 99 으로 넣기 
resicode <- cust_data$RESI_TYPE_CODE
cust_data$RESI_TYPE_CODE <- ifelse(is.na(resicode), 99, resicode)

#[구간화]- 나이 연령대로 
age.fun <- function(data){
  data%/%10
}
cust_data$AGE <- sapply(cust_data$AGE, age.fun)

#[기본 전처리 후 1차 저장]
write.csv(cust_data,'cust_data1.csv', row.names = FALSE)
cust_data <- read.csv("cust_data1.csv")

================================================================================

#[2] 데이터EDA 및 2차 전처리 
cust_data1 <- subset(cust_data, subset= (cust_data$DIVIDED_SET==1)) #값이 있는 데이터를 불러옴
sagi_data <- subset(cust_data1, subset = (cust_data1$SIU_CUST_YN==1)) #사기꾼 가져옴

#[2-1]시각화

#SIU
siu <- table(cust_data1$SIU_CUST_YN)
siu_pc <- round(siu/sum(siu)*100,2)
pie(siu_pc, main='사기꾼 비율', labels = paste(c('정상인', '사기꾼'), '\n', siu_pc, '%', '\n', siu, '명'),cex=1.2)
sum(siu)

#wedd ynto10
cust_data1$WEDD_YN <- sapply(cust_data1$WEDD_YN, yn.fun)         
w <- table(sagi_data$WEDD_YN)
w2 <- table(cust_data1$WEDD_YN)
sum(w)
w_pc <- round(w/w2*100,2)
pie(w_pc, main='결혼유무별 사기꾼 비율', labels = paste(c('미혼', '기혼'), '\n', w_pc, '%', '\n', w, '명'),cex=1.2)

#sex
s <- table(sagi_data$SEX)
s2 <- table(cust_data1$SEX)
s_pc <- round(s/s2*100,2)
pie(s_pc, main='성별 사기꾼 비율', labels = paste(c('남','여'), '\n', s_pc, '%', '\n', s, '명'), cex=1.2)
s

#ctpr
c2 <- table(cust_data1$CTPR)
c <- table(sagi_data$CTPR)
c <- append(c, 0, after = 15)
barplot(c, ylim=c(0,400), main='지역별 사기꾼 수')
c_pc <- round(c/c2*100,2)
names(c) <- area.name
barplot(c_pc, main="지역별 사기꾼 비율", ylim=c(0,20), col=10, axis.lty=3)
c

#age
age.fun <- function(data){data%/%10}
cust_data1$AGE <- sapply(cust_data1$AGE, age.fun)
a<- table(sagi_data$AGE)
a2 <- table(cust_data1$AGE)
a_pc <- round(a/a2*100,2)
a <- append(a,0)
names(a)<- c("10세미만","10대","20대","30대","40대","50대","60대","70대","80대")
barplot(a, ylim = c(0,600), main = '연령대별 사기꾼 수', xlab='나이구간', ylab='명')
barplot(a_pc, ylim=c(0,12), main = '연령대별 사기꾼 비율', xlab='나이구간', ylab='비율(%)', col=10)

#occp
o <- table(sagi_data$OCCP_GRP_1)
o2 <- table(cust_data1$OCCP_GRP_1)
o_pc <- round(o/o2*100,2)
barplot(o, ylim = c(0,500), main = '직업별 사기꾼 수', xlab='직업종류', ylab='명',names.arg = c('', occp_name))
barplot(o_pc, ylim=c(0,15), main = '직업별 사기꾼 비율', xlab='직업종류', ylab='비율(%)', col=10, names.arg = c('', occp_name))
o
o_pc

#resicode
r2<- table(cust_data1$RESI_TYPE_CODE)
r<- table(sagi_data$RESI_TYPE_CODE)
r_pc <- round(r/r2*100,2)
resi.name <- c('단독주택','다가구단독주택', '영업용단독주택', '아파트','다가구주택', '상가', '오피스텔','숙박업소등','기숙사','기타')
barplot(r, ylim = c(0,800), main = '주거타입별 사기꾼 수', xlab='주거타입', ylab='명', names.arg = resi.name)
barplot(r_pc,  ylim=c(0,15), main='주거타입코드 별 사기꾼 비율', xlab='주거타입', ylab='비율(%)', col=10, names.arg = resi.name )


##[2-2] 전처리 및 변수 추가 생성
cust_data_f <- read.csv("data_cust_2-3_test.csv")

#[결측치]소득(CUST_INCM) 결측치 - 평균으로 넣기 
occpavg <- round(tapply(cust_data$CUST_INCM, cust_data$OCCP_GRP_1, mean, na.rm=TRUE))

occpna.fun <- function(income, occp){
  if(is.na(income)){
    income=occpavg[occp+1]
  } else {income=income}
}

cust_data$CUST_INCM<- mapply(FUN = occpna.fun, cust_data$CUST_INCM, cust_data$OCCP_GRP_1)


#[인코딩] wedd - ynto10
table(cust_data1$WEDD_YN)
cust_data$WEDD_YN <- sapply(cust_data$WEDD_YN, nato.fun)
nato.fun <- function(data){
  if(is.na(data)){data=0}
 else{data=data}}

#[이상치]
# 7,8,9등급은 뭔지 모르겠음. 삭제하자니 7~99등급이 20%라, 28과 99등급만 삭제 
maxt <- table(cust_data$MAXCRDT)
sum(maxt[12:13])/sum(maxt)
cust_data<- subset(cust_data, subset = (cust_data$MAXCRDT <28))
sagi_data <- cust_data

#가설 설정 및 검증 - 이원 카이제곱 Q. 이걸로 변수들간 확인이 가능한가? 
#대립가설 : max crdt과 siu는 상관관계가 있다. 
install.packages("gmodels")
library(gmodels)
x<- cust_data1$MAXCRDT
y <- cust_data1$SIU_CUST_YN
CrossTable(x,y, chisq = TRUE)

#두집단 비율 검정
table(x,y)
sum(table(y))
barplot(round(m2/m*100,2))

#[변수제거]
colnames(cust_data)
cust_data <- subset(cust_data, 
                    select=-c(FP_CAREER,CUST_RGST,
                              OCCP_GRP_2, MATE_OCCP_GRP_2,
                              CHLD_CNT,LTBN_CHLD_AGE,
                              MAX_PAYM_YM,MAX_PRM, 
                              JPBASE_HSHD_INCM))

#[변수추가] ctpr 원핫 인코딩 
temp <- subset(cust_data, select=c('CUST_ID','CTPR'))
temp$value <- 1
library(reshape2)
temp_dummy <- dcast(data=temp, CUST_ID~CTPR, fun=sum)
names(temp_dummy) <- c('CUST_ID', area.name)
cust_data <- 
  merge(cust_data, temp_dummy, by='CUST_ID')
cust_data <- subset(cust_data, select=-CTPR)

#[파생변수추가 ] 입원평균일수
hospday <- aggregate(claim_data$VLID_HOSP_OTDA, 
                     by=list(claim_data$CUST_ID), mean)
names(hospday) <- c("CUST_ID", "HOSP_DAYS")
hospday$HOSP_DAYS<- round(hospday$HOSP_DAYS)
cust_data <- merge(cust_data, hospday)

# [파생변수추가 ] 사고구분별 청구사유 횟수 
table(claim_data$ACCI_DVSN,	claim_data$DMND_RESN_CODE)
accCNT	<- table(claim_data$CUST_ID, claim_data$ACCI_DVSN,claim_data$DMND_RESN_CODE)
accCNT <- as.data.frame(accCNT)
names(accCNT) <- c("CUST_ID",	"ACCI_DVSN",	"DMND_RESN_CODE",	
                   "value")
library(reshape2)
accCNT <- dcast(data=accCNT, CUST_ID~ACCI_DVSN+DMND_RESN_CODE, fun=sum)
cust_data <- merge(cust_data, accCNT)
cust_data <- cust_data[,sapply(cust_data, function(v) var(v, na.rm=TRUE)!=0)]


write.csv(cust_data,'cust_data2.csv', row.names = FALSE)

#===============================

cust_data1 <- subset(cust_data, subset= (cust_data$DIVIDED_SET==1)) #train용으로 진행 
cust_data2 <- subset(cust_data, subset= (cust_data$DIVIDED_SET==2))


#데이터 샘플링
cust_data1 <- subset(cust_data1, select=-c(CUST_ID,DIVIDED_SET))
cust_data2 <- subset(cust_data2, select=-c(CUST_ID,DIVIDED_SET,SIU_CUST_YN)

idx <- sample(1:nrow(cust_data1), nrow(cust_data1)*0.7)
train <- cust_data1[idx,]
test <- cust_data1[-idx,]

#[모델]의사결정나무 모델 f1
library(party)
tree_model <- ctree(SIU_CUST_YN~., data=train)
pred_tr <- predict(tree_model, test)

#[모델]랜덤포레스트 모델
install.packages("randomForest")
library(randomForest)
rf_model <- randomForest(SIU_CUST_YN~., data=train, ntree=100, na.action = na.omit )

pred_rf <- predict(rf_model, test)
sum(table(test$SIU_CUST_YN, round(pred_rf)))
439/546

#[모델] 엑스쥐 부스트 
install.packages("xgboost")
library(xgboost)

train_mat <- as.matrix(train[,-1])
train_lab <- train$SIU_CUST_YN
dtrain <- xgb.DMatrix(data = train_mat, label = train_lab )

xgb_model <- xgboost(data=dtrain, eta=1, objective = 'binary:logistic',
                     nround=60, nthread	=	8)

#평가 
test_mat <- as.matrix(test[,-1])
test_lab <- test$SIU_CUST_YN

dim(test_mat)

pred_xg <- predict(xgb_model, test_mat) #..?
round(pred_xg)
table(round(pred_xg), test_lab)
im_ma <- xgb.importance(colnames(train_mat), model = xgb_model)
xgb.plot.importance(im_ma)
(4992+274)/ length(test_lab)

#[모델] 보팅 
voting <- function(x){
  value_count = sort(table(x), decreasing = TRUE)
  label = names(value_count)[1]
}
pred_total <- data.frame(round(pred_tr), round(pred_rf), round(pred_xg))
names(pred_total)<- c('tree','randomtree','xgbboost')
pred_vote <- apply(pred_total, MARGIN = 1, FUN = voting)
table(test$SIU_CUST_YN, pred_vote)

#[비교] - f1 score 
f1score <- function(m){
  ac <- (m[1,1]+m[2,2])/ sum(m)
  pr <- m[2,2]/(m[1,2]+m[2,2])
  re <- m[2,2]/(m[2,1]+m[2,2])
  f1 <- 2*pr*re/(pr+re)
  print(ac) 
  print(pr)
  print(re)
  return(f1)
}

m <- table(test_lab, round(pred_xg))
d <- table(test$SIU_CUST_YN, round(pred_tr))
r <- table(test$SIU_CUST_YN, round(pred_rf))
m
f1score(m) #xgboost
f1score(d) #나무 
f1score(r) #랜덤트리 


#[비교] - 정확도 
mean(test$SIU_CUST_YN ==round(pred_tr))#3
mean(test$SIU_CUST_YN == round(pred_rf)) #1
mean(test$SIU_CUST_YN == round(pred_xg))#4
mean(test$SIU_CUST_YN == pred_vote)#2

#[결과]
f_test <- subset(cust_data, subset= (cust_data$DIVIDED_SET==2))
pred_rf <- predict(rf_model, f_test)
result <- as.data.frame(round(pred_rf))
table(result)

write.csv(result, "result.csv", row.names = FALSE)

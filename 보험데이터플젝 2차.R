cust_data <- read.csv("cust_data1_1.csv") #갑자기 오류나서 원본데이터 못가져옴 스바! (기본 전처리 되어있음)
claim_data <- read.csv("CLAIM_DATA.csv", sep = ',', encoding = "CP949", fileEncoding = "UCS-2")

-------------------------------------------------------------------------------
#[1] 기본 전처리(cust_data1) 
#[인코딩] siu  
yn.fun <- function(data){
  if(data =='Y'){  data=1  }
  else if (data == 'N'){data=0}
  else { data =''}
}

cust_data$SIU_CUST_YN <- sapply(cust_data$SIU_CUST_YN, yn.fun)

#[인코딩] wedd - ynto10
cust_data$WEDD_YN <- sapply(cust_data$WEDD_YN, yn.fun)
nato.fun <- function(data){
  if(is.na(data)){data=0}
  else{data=data}}
cust_data$WEDD_YN <- sapply(cust_data$WEDD_YN, nato.fun)
#[인코딩]CTPR - C to N 
area.name <- levels(as.factor(cust_data$CTPR))
cust_data$CTPR <- as.numeric(as.factor(cust_data$CTPR))
cust_data$CUST_RGST <- as.numeric(cust_data$CUST_RGST)

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

#[결측치] 거주지 타입코드를 결측치 0 으로 넣기 
resicode <- cust_data$RESI_TYPE_CODE
cust_data$RESI_TYPE_CODE <- ifelse(is.na(resicode), 0, resicode)

#[구간화]- 나이 연령대로 
age.fun <- function(data){
  data%/%10
}
cust_data$AGE <- sapply(cust_data$AGE, age.fun)

write.csv(cust_data,'cust_data1_1.csv', row.names = FALSE)
----------------------------------------------------------------------여기까지 cust_data1_1이라 이미 되어있음. 
cust_data <- read.csv('cust_data1_1.csv')

#[2] 데이터 시각화 
cust_data1 <- subset(cust_data, subset= (cust_data$DIVIDED_SET==1)) #값이 있는 데이터를 불러옴
cust_data2 <- subset(cust_data, subset=(cust_data$DIVIDED_SET==2))
sagi_data <- subset(cust_data1, subset = (cust_data1$SIU_CUST_YN==1)) #사기꾼 가져옴

#SIU
siu <- table(cust_data1$SIU_CUST_YN)
siu_pc <- round(siu/sum(siu)*100,2)
pie(siu_pc, main='사기꾼 비율', labels = paste(c('정상인', '사기꾼'), '\n', siu_pc, '%', '\n', siu, '명'),cex=1.2)
sum(siu)

#wedd ynto10
w <- table(sagi_data$WEDD_YN)
w2 <- table(cust_data1$WEDD_YN)
w_pc <- round(w/w2*100,2)
pie(w_pc, main='결혼유무별 사기꾼 비율', labels = paste(c('미혼', '기혼'), '\n', w_pc, '%', '\n', w, '명'),cex=1.2)

#sex
s <- table(sagi_data$SEX)
s2 <- table(cust_data1$SEX)
s_pc <- round(s/s2*100,2)
pie(s_pc, main='성별 사기꾼 비율', labels = paste(c('남','여'), '\n', s_pc, '%', '\n', s, '명'), cex=1.2)


#ctpr
c2 <- table(cust_data1$CTPR)
c <- table(sagi_data$CTPR)
c <- append(c, 0, after = 15)
barplot(c, ylim=c(0,400), main='지역별 사기꾼 수')
c_pc <- round(c/c2*100,2)
names(c) <- area.name
barplot(c_pc, main="지역별 사기꾼 비율", ylim=c(0,20), col=10, axis.lty=3)


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


##[3] 전처리 및 변수 추가 생성
cust_data <- read.csv('cust_data1_1.csv')

#소득(CUST_INCM) 결측치 - 직업별평균으로 넣기 
occpavg <- round(tapply(cust_data$CUST_INCM, cust_data$OCCP_GRP_1, mean, na.rm=TRUE))

occpna.fun <- function(incm, occp){
  if (is.na(incm)){
    incm = occpavg[occp+1]
    } else {incm = incm}
}

cust_data$CUST_INCM <- mapply(FUN = occpna.fun, cust_data$CUST_INCM, cust_data$OCCP_GRP_1)

#소득(JPBASE_HSHD_INCM) 결측치 - 평균으로 넣기 
jpbaseavg <- round(tapply(cust_data$JPBASE_HSHD_INCM, cust_data$OCCP_GRP_1, mean, na.rm=TRUE))
cust_data$JPBASE_HSHD_INCM <- mapply(FUN = occpna.fun, cust_data$JPBASE_HSHD_INCM, cust_data$OCCP_GRP_1)

#신용등급 
# 7,8,9등급은 뭔지 모르겠음. 삭제하자니 7~99등급이 20%라, 28과 99등급만 삭제 cust_data2 제외
cust_data1 <- subset(cust_data, subset= (cust_data$DIVIDED_SET==1)) 
cust_data2 <- subset(cust_data, subset= (cust_data$DIVIDED_SET==2)) 
cust_data1<- subset(cust_data1, subset =(cust_data1$MAXCRDT <28))

#변수제거
colnames(cust_data)
cust_data <- subset(cust_data, 
                    select=-c(FP_CAREER,OCCP_GRP_2, MATE_OCCP_GRP_2,
                              CHLD_CNT,LTBN_CHLD_AGE,
                              MAX_PAYM_YM,MAX_PRM))


write.csv(cust_data,'cust_data1_2.csv', row.names = FALSE)#-------------------INCM JPBASE 변수삭제까지 하고 저장. 
write.csv(cust_data2,'test_data1_2.csv', row.names = FALSE)
cust_data <- read.csv('cust_data1_2.csv') 

cust_data1 <- subset(cust_data, subset= (cust_data$DIVIDED_SET==1)) 
cust_data2 <- subset(cust_data, subset= (cust_data$DIVIDED_SET==2)) 

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
accCNT	<- table(claim_data$CUST_ID, claim_data$ACCI_DVSN,claim_data$DMND_RESN_CODE)
accCNT <- as.data.frame(accCNT)
names(accCNT) <- c("CUST_ID",	"ACCI_DVSN",	"DMND_RESN_CODE",	
                   "value")
library(reshape2)
accCNT <- dcast(data=accCNT, CUST_ID~ACCI_DVSN+DMND_RESN_CODE, fun=sum)
names(accCNT) <- c("CUST_ID", "x1_1", "x1_2", "x1_3", "x1_4", "x1_5", "x1_6", "x1_7", "x1_9",
                   "x2_1", "x2_2", "x2_3", "x2_4", "x2_5", "x2_6", "x2_7", "x2_9", 
                   "x3_1", "x3_2", "x3_3", "x3_4", "x3_5", "x3_6", "x3_7", "x3_9")
cust_data <- merge(cust_data, accCNT)
cust_data <- cust_data[,sapply(cust_data, function(v) var(v, na.rm=TRUE)!=0)]

write.csv(cust_data,'cust_data1_3.csv', row.names = FALSE)


#[파생변수추가] ID별 병원까지 거리 평균  #na 값이 14% 
# na 값을 어떻게 처리해? 일단 0으로 처리하고 .... 관련성 없다하면 평균값(해서 결측값이면 0으로 대체)
hd <- table(is.na(claim_data$HOUSE_HOSP_DIST))
hd[2] / sum(hd)
claim_data$HOUSE_HOSP_DIST <- sapply(claim_data$HOUSE_HOSP_DIST, nato.fun)

disavg <- round(aggregate(claim_data$HOUSE_HOSP_DIST, by=list(claim_data$CUST_ID), mean),2)
names(disavg) <- c("CUST_ID", "HOSP_DIST")
cust_data <- merge(cust_data, disavg)

write.csv(cust_data,'cust_data1_4.csv', row.names = FALSE)

#[파생변수추가 ] 청구금액 총액 구하기  
#각 고객의 청구사유별 청구금액 총합을 구하고 싶었으나, 그럼 cust_data ID에 여러 행이 생기니 불가. 

dmntsum <- aggregate(claim_data$DMND_AMT, by= list(claim_data$CUST_ID), sum)
names(dmntsum)<- c("CUST_ID", "DMND_AMT")
cust_data <- merge(cust_data, dmntsum)

write.csv(cust_data,'cust_data1_5.csv', row.names = FALSE)

#========================================================================

cust_data1 <- subset(cust_data, subset= (cust_data$DIVIDED_SET==1)) #train용으로 진행 
cust_data2 <- subset(cust_data, subset= (cust_data$DIVIDED_SET==2))


#데이터 샘플링
cust_data1 <- subset(cust_data1, select=-c(CUST_ID,DIVIDED_SET)) #train 
cust_data2 <- subset(cust_data2, select=-c(CUST_ID,DIVIDED_SET,SIU_CUST_YN)) #test
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
rf_model <-randomForest(SIU_CUST_YN~., data=train, ntree=100, na.action = na.omit)
pred_rf <- predict(rf_model, test)

#[모델] 엑스쥐 부스트 
install.packages("xgboost")
library(xgboost)
train.mat <- as.matrix(train[,-1])
train.lab <- train$SIU_CUST_YN
dtrain <- xgb.DMatrix(data=train.mat, label = train.lab)
xgb_model <- xgboost(data=dtrain, eta=1, objective = 'binary:logistic', 
                     nround = 150, nthread=5) #nround
                     
#평가 
test_mat <- as.matrix(test[,-1])
test_lab <- test$SIU_CUST_YN
pred_xg <- predict(xgb_model, test_mat) #..?
table(round(pred_xg), test_lab)
im_ma <- xgb.importance(colnames(train.mat), model = xgb_model)
xgb.plot.importance(im_ma)
                     
#[모델] 보팅 
voting <- function(x){
  value_cnt = sort(table(x), decreasing = TRUE)
  label = names(value_cnt)[1]
}
pred_total <- data.frame(round(pred_tr), round(pred_rf), round(pred_xg))
names(pred_total)<- c('tree','randomtree','xgbboost')
pred_vote <- apply(pred_total, MARGIN = 1, FUN = voting)
table(test$SIU_CUST_YN, pred_vote) #결과값이 너무..이상한디? 

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

f1score(m) #xgboost
f1score(d) #나무 
f1score(r) #랜덤트리 

                     
#[결과] cust_data2
pred_rf <- predict(rf_model, cust_data2)
result_rf <- as.data.frame(round(pred_rf))
table(result_rf)
write.csv(result_rf, "result_rf_2.csv", row.names = FALSE)

test_data_mat <- as.matrix(cust_data2)
pred_xg <- predict(xgb_model, test_data_mat)

result_xgmodel <- round(pred_xg)
result_xgmodel <- as.data.frame(result_xgmodel)
table(result_xgmodel)
write.csv(result_xgmodel, 'result_xgmodel_2.csv', row.names = FALSE)
                     

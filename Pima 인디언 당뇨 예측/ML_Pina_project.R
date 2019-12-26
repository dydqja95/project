library(tidyverse)

# <Decision Tree> ----
# 1. 데이터 블러오기 ----
diabetes <- read.csv(file = "c://Users/lg/Desktop/머신러닝 ing/diabetes_Raw.csv")
str(object = diabetes)
head(diabetes, n = 10)
summary(diabetes)


# 2. 명목형 변수 바꿔주기 ----
diabetes$Outcome = as.factor(diabetes$Outcome)

diabetes$Outcome %>% 
  table() %>% 
  prop.table()


# 3. 데이터 분할하기 ----
library(caret)
set.seed(seed = 1234)
createDataPartition(y = diabetes$Outcome,
                    p = 0.7,
                    list = FALSE) -> index

DT_trainSet <- diabetes[index, ]
DT_testSet  <- diabetes[-index, ]


# 4. 모형 적합 ----
library(rpart)
set.seed(seed = 1234)
DT_1 <- rpart(formula = Outcome ~ .,         # 목표변수
              data    = DT_trainSet,
              method  = 'class',                          # 목표변수가 범주형이면 자동으로 설정
              parms   = list(split = 'gini'),             # 기본값 gini
              # rpart.control - 정지규칙
              control = rpart.control(minsplit = 20,      # 마디의 관측값 기준
                                      cp       = 0.01,    # 비용복잡도에 사용될 파라미터
                                      maxdepth = 10))     # 뿌리에서 끝마디 까지의 깊이 최댓값

summary(DT_1)

## 나무 모형 그래프로 그리기
plot(x = DT_1,
     compress = TRUE,   # 그래프 좌우 폭 줄임
     uniform  = TRUE,   # 부모마디와 자식마디 간의 높이 일정하게
     branch   = 0.9,    # 0에 가까울수록 각도 커짐
     margin   = 0.05)   # 0에 가까울수록 그림 여백을 줄여 그래프가 커짐

text(x     = DT_1,
     use.n = TRUE,      # 끝마디의 관측값을 범주별로 출력
     all   = TRUE,      # 전체 마디의 관측값을 범주별로 출력
     cex   = 1.2)       # 글자 크기기

## rpart.plot
library(rpart.plot)
rpart.plot(x             = DT_1,
           type          = 2,
           extra         = 106,
           fallen.leaves = FALSE)


# 5. 가지치기 ----
## 1차
printcp(x = DT_1)
plotcp(x = DT_1)

## x.error가 CP 0.015957 에서 가장 작음
DT_2 <- prune.rpart(tree = DT_1,
                    cp = 0.015957)
printcp(DT_2)


# 6. 성능 평가 ----
## 0) 추정값 생성
DT_pred1 <- predict(object = DT_1, newdata = DT_testSet, type = 'class') # 가지치기 전
DT_pred2 <- predict(object = DT_2, newdata = DT_testSet, type = 'class') # 가지치기 후

### 실젯값도 저장
DT_real <- DT_testSet$Outcome

## 1) 혼동 행렬 (pred1, pred2 비교)
confusionMatrix(data      = DT_pred1,
                reference = DT_real,
                positive  = '1')
### 정확도(Accuracy)    - 0.7348
### 민감도(Sensitivity) - 0.5250
### 정밀도(Precision)   - 0.7697
### 특이도(Specificity) - 0.8467

confusionMatrix(data      = DT_pred2,
                reference = DT_real,
                positive  = '1')
### 정확도(Accuracy)    - 0.7348
### 민감도(Sensitivity) - 0.5375
### 정밀도(Precision)   - 0.7730
### 특이도(Specificity) - 0.8400

## 2) F1 Score (pred1, pred2 비교)
library(MLmetrics)
F1_Score(y_pred   = DT_pred1,
         y_true   = DT_real,
         positive = '1')
## 0.5793

F1_Score(y_pred   = DT_pred2,
         y_true   = DT_real,
         positive = '1')
## 0.5850

## 3) AUC
### ROC Curve
library(ROCR)
predObj <- prediction(predictions = as.numeric(x = DT_pred1),
                      labels      = as.numeric(x = DT_real))

perform <- performance(prediction.obj = predObj,
                       measure         = 'tpr',
                       x.measure       = 'fpr')

plot(x    = perform,
     main = 'ROC curve')

### AUC
library(pROC)
auc(response  = as.numeric(x = DT_real),
    predictor = as.numeric(x = DT_pred1))
## 0.6858

auc(response  = as.numeric(x = DT_real),
    predictor = as.numeric(x = DT_pred2))
## 0.6888



# <Random Forest> ----
# 1. 데이터 불러오기 ----
diabetes <- read.csv(file = "c://Users/lg/Desktop/머신러닝 ing/diabetes_Raw.csv")
head(diabetes, 10)
str(diabetes)
summary(diabetes)


# 2. 목표변수를 명목변수로 바꿔주기 ----
diabetes$Outcome = as.factor(diabetes$Outcome)


# 3. 데이터 분할 ----
library(caret)
set.seed(1234)
createDataPartition(y = diabetes$Outcome,
                    p = 0.7,
                    list = FALSE) -> index

## Train Set, Test Set 분할
RF_trainSet <- diabetes[index, ]
RF_testSet  <- diabetes[-index, ]

## 각 셋에서 목표 변수의 빈도 확인
RF_trainSet$Outcome %>% table() %>% prop.table()
RF_testSet$Outcome %>% table() %>% prop.table()


# 4. 분류모형 적합 - 랜덤 포레스트 함수 작성 ----
# install.packages("randomForest")
library(randomForest)
set.seed(1234)
RF_1 <- randomForest(x           = RF_trainSet[ , -9],  # 목표변수인 9열 제외
                     y           = RF_trainSet[ , 9],   # 목표변수인 9열 입력
                     xtest       = RF_testSet[ , -9],   # xtest, ytest는 입력하지 않아도 되지만 추후 predict할 때 넣지 않아도 됨.
                     ytest       = RF_testSet[ , 9],
                     ntree       = 1000,
                     mtry        = 3,
                     importance  = TRUE,       # 변수 중요도를 최종 모형에 저장
                     do.trace    = 50,         # 진행 현황 보여줌(50번 마다)
                     keep.forest = TRUE)       # 각 나무모형의 끝마디 수 확인

str(RF_1)
RF_1$err.r

## 오차 추정 - 오차 그래프 살펴보기
plot(x = RF_1$err.rate[ , 1], type = 'l')  # 당뇨에 걸리지 않은 사람
plot(x = RF_1$err.rate[ , 2], type = 'l')  # 당뇨에 걸린 사람
plot(x = RF_1)                             # 1과 0을 같이 출력

## 변수 중요도 확인 --> importance = TRUE 
importance(x = RF_1)
varImpPlot(x = RF_1)


## 개별 나무 모형의 끝마디 수 --> keep.forest = TRUE
RF_1 %>% 
  treesize(terminal = T) %>% 
  hist(main = 'Number of Terminal Nodes')


# 5. 추정라벨 생성 ----
RF_pred <- RF_1$test$predicted

## 추정확률 생성
prob <- predict(object = RF_1,
                newdata = RF_testSet,
                type = 'vote')

prob <- prob[ , 2]


# 6. 성능 평가 ----
library(caret)
## 훈련셋의 실제값 지정
RF_real <- RF_testSet$Outcome

## 1) 혼동행렬
confusionMatrix(data = RF_pred, reference = RF_real, positive = '1')
### 정확도(Accuracy)    - 0.7739
### 민감도(Sensitivity) - 0.5875
### 정밀도(Precision)   - 0.7988
### 특이도(Specificity) - 0.8733

## 2) F1 Score
library(MLmetrics)
F1_Score(y_pred   = RF_pred, 
         y_true   = RF_real, 
         positive = '1')
## 0.6438

## 3) AUC
### ROC
library(ROCR)
predObj <- prediction(predictions = as.numeric(x = pred),
                      labels      = as.numeric(x = real))

perform <- performance(prediction.obj = predObj,
                       measure        = 'tpr',
                       x.measure      = 'fpr')

plot(x = perform, main = 'ROC curve')

### AUC 출력
# install.packages("pROC")
library(pROC)
auc(response  = as.numeric(x = RF_real),
    predictor = as.numeric(x = RF_pred))
## 0.7304



# <Random Forest_Tuning> ----
# 1. ntree(3) x mtry(3) grid 생성 ----
RF_grid <- expand.grid(ntree = c(300, 500, 700, 1000),
                    mtry  = c(3, 4, 5, 6, 7))
print(RF_grid)

# 2. 빈 데이터 프레임 생성 ----
RF_tuned <- data.frame()


# 3. 반복문 작성 ----
for(i in 1:nrow(RF_grid)){
  set.seed(seed = 1234)
  cat(i, '행 실행 중! [ntree:', grid[i, 'ntree'], ', mtry:', grid[i, 'mtry'], ']\n\n')
  
  randomForest(x           = RF_trainSet[ , -9],   # 목표변수인 9열 제외
               y           = RF_trainSet[ , 9],    # 목표변수인 9열 입력
               xtest       = RF_testSet[ , -9],    # xtest, ytest는 입력하지 않아도 되지만 추후 predict할 때 넣지 않아도 됨.
               ytest       = RF_testSet[ , 9],
               ntree       = RF_grid[i, 'ntree'],
               mtry        = RF_grid[i, 'mtry'],
               importance  = TRUE,        # 변수 중요도를 최종 모형에 저장
               do.trace    = 50,          # 진행 현황 보여줌(50번 마다)
               keep.forest = TRUE) -> fit # 각 나무모형의 끝마디 수 확인
  
  mcSum     <- sum(fit$predicted != RF_trainSet$Outcome)
  mcRate    <- mcSum / nrow(x = RF_trainSet)
  df        <- data.frame(index = i, misClassRate = mcRate)
  RF_tuned  <- rbind(RF_tuned, df)
}


# 4. 튜닝 결과 확인 ----
print(x = cbind(RF_grid, RF_tuned))
plot(x    = RF_tuned,
     xlab = '',
     ylab = 'Misclassification Rate',
     type = 'b')

abline(h   = min(RF_tuned$misClassRate),
       col = 'red',
       lty = 2)


# 5. 최적의 파라미터 설정 및 적합 ----
which(RF_tuned$misClassRate == min(x = RF_tuned$misClassRate)) -> loc
print(x = loc)

RF_bestPara <- RF_grid[loc, ]
print(x = RF_bestPara)

randomForest(x           = RF_trainSet[ , -9],  
             y           = RF_trainSet[ , 9],   
             xtest       = RF_testSet[ , -9],   
             ytest       = RF_testSet[ , 9],
             ntree       = RF_bestPara$ntree,
             mtry        = RF_bestPara$mtry,
             importance  = TRUE,       
             do.trace    = 50,         
             keep.forest = TRUE) -> bestRFC

plot(bestRFC)
importance(bestRFC)
varImpPlot(bestRFC)


# 6. 최적 모형의 성능 평가 ----
library(caret)

## testSet 추정값 생성
RF_predB <- predict(object  = bestRFC, 
                    newdata = RF_testSet,
                    type    = 'Response')

## 훈련셋의 실제값 지정
RF_real <- RF_testSet$Outcome

## 1) 혼동행렬
confusionMatrix(data = RF_predB, reference = RF_real, positive = '1')
### 정확도(Accuracy)    - 0.7652
### 민감도(Sensitivity) - 0.6000
### 정밀도(Precision)   - 0.8000
### 특이도(Specificity) - 0.8533


## 2) F1 Score
library(MLmetrics)
F1_Score(y_pred = RF_predB, y_true = RF_real, positive = '1')
### 0.6442

## 3) AUC
### ROC
library(ROCR)
predObj <- prediction(predictions = as.numeric(x = RF_predB),
                      labels      = as.numeric(x = RF_real))

perform <- performance(prediction.obj = predObj,
                       measure        = 'tpr',
                       x.measure      = 'fpr')

plot(x = perform, main = 'ROC curve')

### AUC 출력
# install.packages("pROC")
library(pROC)
auc(response  = as.numeric(x = RF_real),
    predictor = as.numeric(x = RF_predB))
### 0.7267



# <Gradient Boosting> ----
# 1. 데이터 불러오기 ----
diabetes <- read.csv(file = "c://Users/lg/Desktop/머신러닝 ing/diabetes_Raw.csv")
str(diabetes)
summary(diabetes)


# 2. 명목형 변수 바꿔주기 ----
diabetes$Outcome = as.factor(diabetes$Outcome)


# 3. 데이터 분할하기 ----
library(caret)
## 분할
set.seed(1234)
createDataPartition(y = diabetes$Outcome,
                    p = 0.7,
                    list = FALSE) -> index

## 할당
GB_trainSet <- diabetes[index, ]
GB_testSet <- diabetes[-index, ]

## 목표 변수의 구성 비율 비교
table(GB_trainSet$Outcome) %>% prop.table()
table(GB_testSet$Outcome) %>% prop.table()


# 4. 분류모형 적합 ----
# install.packages("gbm")
library(gbm)

set.seed(1234)
gbm(formula           = Outcome ~ .,      #
    data              = GB_trainSet,              # 
    distribution      = 'multinomial',         # 목표변수가 범주형 - multinomial (Outcome은 레벨이 2개인 범주)
    n.trees           = 5000,                  # 부스팅 반복할 횟수(나무의 수)
    interaction.depth = 3,                     # 분할 횟수                  (튜닝 파라미터)
    shrinkage         = 0.01,                  # 경사하강법의 학습률 지정   (튜닝 파라미터)
    n.minobsinnode    = 10,                    # 나무 끝마디의 최소 관측값  (튜닝 파라미터)
    bag.fraction      = 0.5,                   # 훈련셋 부분집합 크기       (튜닝 파라미터)
    cv.folds          = 5,                     # 교차검증 수행할 횟수 -> 실행하지 않으면 에러값이 생기지 않는다
    n.cores           = NULL) -> GBC_1         # 사용할 CPU core개수 지정

# 5. 적합 결과 확인 ----
print(GBC_1)
# 5000 iterations were performed.
# The best cross-validation iteration was 1300.                 -> 최적은 1300이었다는 의미
# There were 11 predictors of which 11 had non-zero influence.

summary(GBC_1)
str(GBC_1)

## 각 입력변수와 목표변수 간 상관관계 막대그래프
### Education
plot(x = GBC_1,
     i = 'Glucose')

### 카이제곱검정 실행
chisq.test(x = GB_trainSet$Glucose,
           y = GB_trainSet$Outcome)
#### X-squared = 213.61, df = 125, p-value = 1.361e-06


# 6. 성능 평가 ----
## 0) 추정값 및 실제값 지정
### predict함수에 testSet을 할당하여 생성된 추정값을 pred_GB에 저장
GB_pred <- predict(GBC_1,
                   newdata = GB_testSet,
                   type = 'response')      # n.trees를 지정하지 않을 경우 최적 개수였던 1300개 나무 사용

### 추정값이 큰 값을 갖는 컬럼명을 지정해서 추정라벨 붙임
maxCol <- apply(X = GB_pred, MARGIN = 1, FUN = which.max)
colnames(x = GB_pred)[maxCol] %>% 
  as.factor -> GB_pred

### 실제값 지정
GB_real <- GB_testSet$Outcome

## 1) 혼동행렬 출력
confusionMatrix(data = GB_pred, reference = GB_real, positive = '1')
### 정확도(Accuracy)    - 0.7696
### 민감도(Sensitivity) - 0.5750
### 정밀도(Precision)   - 0.7988
### 특이도(Specificity) - 0.8733 

## 2) F1 점수 출력
library(MLmetrics)
F1_Score(y_pred   = GB_pred,
         y_true   = GB_real,
         positive = '1')
### 0.6344

## 3) ROC 커브 및 AUC
library(ROCR)
predObj <- prediction(predictions = as.numeric(x = GB_pred),
                      labels      = as.numeric(x = GB_real))

perform <- performance(prediction.obj = predObj,
                       measure = 'tpr',
                       x.measure = 'fpr')

plot(x = perform, main = 'ROC curve')

### AUC 출력
library(pROC)
auc(response = as.numeric(x = GB_real),
    predictor = as.numeric(x = GB_pred))
### 0.7242


# <Gradient Boosting_Tuning> ----
# 1. 그리드 생성 ----
## 반복문을 사용하여 탐색하기 위해 4가지 튜닝 파라미터에 대한 후보 grid 생성
expand.grid(depth = c(1, 3, 5),             # interaction.depth (분할 횟수)
            learn = c(0.01, 0.05, 0.10),    # shrinkage         (학습률)
            min   = c(5, 7, 10),            # n.minobsinnode    (나무 정지규칙)
            bag   = c(0.5, 0.8, 1.0),       # bag.fraction      (훈련셋 부분집합 크기)
            tree  = NA,                     # 반복문에서 최소 에러가 나오는 '나무의 개수'와 '에러값' 저장될 빈 공간 생성
            loss  = NA) -> grid_GB
print(x = grid_GB)

# 2. 반복문을 사용한 탐색 ----
for(i in 1:nrow(grid_GB)){
  set.seed(1234)
  gbm(formula           = Outcome ~ .,
      data              = GB_trainSet,
      distribution      = 'multinomial',
      n.trees           = 2000,                           # 최적이 1300으로 나왔으니까 시간 절약 위해 2000으로 설정정
      interaction.depth = grid_GB$depth[i],
      shrinkage         = grid_GB$learn[i],
      n.minobsinnode    = grid_GB$min[i],
      bag.fraction      = grid_GB$bag[i],
      train.fraction    = 0.75) -> fit                    # train.fraction을 1로 하면 교차검증이 일어나지 않는다
      grid_GB$tree[i] <- which.min(x = fit$valid.error)
      grid_GB$loss[i] <- fit$valid.error %>% min()
  cat("now", i ,"진행중")
}

## 1) grid에 담긴 loss 정렬하여 확인
grid_GB %>% 
  arrange(loss) %>% 
  head(n = 10L)

## 2) loss값이 제일 작은 행의 위치 확인
which.min(grid_GB$loss) -> loc; print(x = loc)

## 3) 위치( 2)의 결과값)의 행을 bestPara에 저장
bestPara_GB <- grid_GB[loc, ]; print(x = bestPara_GB)


# 3. bestGBC에 bestPara_GB를 최종 모델로 적합 ----
set.seed(1234)
gbm(formula           = Outcome ~ .,
    data              = GB_trainSet,
    distribution      = 'multinomial',
    n.trees           = bestPara_GB$tree,
    interaction.depth = bestPara_GB$depth,
    shrinkage         = bestPara_GB$learn,
    n.minobsinnode    = bestPara_GB$min,
    bag.fraction      = bestPara_GB$bag,
    train.fraction    = 1,
    n.cores           = NULL,
    verbose           = FALSE) -> bestGBC


# 4. Prediction과 라벨링 ----
## 1) predB에 최종 모델 bestGBC 추정값 저장
GB_predB <- predict(object  = bestGBC,
                    newdata = GB_testSet,
                    n.trees = bestPara_GB$tree,
                    type    = 'response')
print(GB_predB)

## 2) 추정확률이 큰 값을 갖는 컬럼명을 지정하는 방식으로 추정라벨 붙임
apply(X = GB_predB,
      MARGIN = 1,
      FUN = function(x){
        ifelse(test = x[1]>= x[2],
               yes  = '0',
               no   = '1')
      }) %>% as.factor -> GB_predB

class(GB_predB)


# 5. 성능 확인 ----
library(caret)
GB_real <- GB_testSet$Outcome

## 1) 혼동 행렬
confusionMatrix(data = GB_predB,
                reference = GB_real,
                positive = '1')
### 정확도(Accuracy)    - 0.7696
### 민감도(Sensitivity) - 0.5875
### 정밀도(Precision)   - 0.7975
### 특이도(Specificity) - 0.8667

## 2) F1 Score
F1_Score(y_pred   = GB_predB,
         y_true   = GB_real,
         positive = '1')
### 0.6394

## 3) ROC 커브 및 AUC
predObj <- prediction(predictions = as.numeric(x = GB_predB),
                      labels = as.numeric(x = GB_real))

perform <- performance(prediction.obj = predObj,
                       measure        = 'tpr',
                       xmeasure       = 'fpr')

plot(x = perform, main = 'ROC curve')

### AUC 출력
auc(respons = as.numeric(x = GB_real),
    predictor = as.numeric(x = GB_predB))
### 0.7271


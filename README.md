# SMWU-bigdata

# Prediction of Korean Box Office using Data Crawling

양지원(통계학과), 이예진(통계학과), 한하랑(통계학과)


   한 달에도 수십 개의 영화가 나오는 시점에서 영화의 흥행 여부와 손익분기점을 넘을 것인가에 대한 우려
가 있다. 이는 영화 누적관객수와 관련이 있는 것이다. 따라서 영화 관객수와 관련된 여러 내외부 요인들을
이용하여 여러 학습기를 통해 누적관객수의 정확한 예측을 꾀한다.

   영화 관객수에 영향을 줄 것이라고 예상되는 변수들은 굉장히 많다. 하지만 실제로 이 많은 변수들이 영화
관객수에 영향을 주지는 않을 것이다. 때로는 영화 관객수와 다른 변수들과의 교란 변수로서 작용하여 영화
관객수에 영향을 주는 것처럼 보일 수도 있다. 이처럼 실제 생각과 분석 결과를 비교함으로써 우리의 사고를
확장시킬 수 있다.

   더 나아가 많은 전문가들은 스크린수가 관객수에 영향을 많이 미친다고 판단한다. 한 사례로 겨울왕국2
가 있다. 영화진흥위원회에 따르면 1일 기준 겨울왕국2의 누적관객수는 1천만 명 이상을 돌파했다. 이 영
화의 흥행에는 스크린 독과점이 큰 몫을 했다는 의견이 있다. 전국 2천351개 스크린에 걸려 1만 3천467회
상영하여 점유율 62.5%에 달했기 때문이다. 이 때문에 한 시민단체는 겨울왕국2가 국내 상영관을 독점해
독점금지법을 위반했다며 월트디즈니컴퍼니코리아를 고발했다. 이처럼 최근 대형 배급사의 영화 혹은 특정
영화들의 스크린 독점 현상으로 인해서 스크린 상한제 도입에 대한 의견이 거론된다. 2020년 3월 문체부는
스크린 상한제를 제안했다. 이는 6개관 이상을 보유한 극장을 대상으로 관객이 집중되는 시간대(오후 1시∼
11시)에 같은 영화의 상영 횟수가 50%를 넘지 않도록 하는 것이다. 대한민국의 스크린 독점 문제는 2019
년 4월에 개봉한 “어벤져스-엔드 게임”로 잘 보여진다. 이는 개봉했을 당시 상영 회차의 80.2%를 장악했다.
나머지 개봉 영화 44편이 19.8%를 나눠야 하는 상황이었다. 다양성 관점에서 영화 창작자들은 물론이고 관
객들의 권리에 위협적이라고 전문가들은 말한다. 따라서 분석을 통해 실제 스크린수가 누적관객수에 영향을
미치는지 알아보고 스크린 상한제 도입이 필요한지 알아보았다.

## Flow Chart
2020년까지의 한국 누적 박스오피스에 해당하는 영화 630편을 이용하여 학습한다.
데이터 셋의 수가 적어 K-Fold를 통한 교차검증을 진행하고, 성과 평가척도로 RMSE, MAE, F1-score, Precision을 이용하여 성능을 평가한다. Lasso, GLM ,Gradient Boost, RandomForest, Decision Tree 등을 모델로 사용한다.

<img src = "https://user-images.githubusercontent.com/66674793/104835172-25840c80-58e8-11eb-82aa-604e25b91060.png" width="400px">

## Prediction rate

<img src = "https://user-images.githubusercontent.com/66674793/104835230-93303880-58e8-11eb-906c-bfe4f3bac877.png">

## Variable Importance (VIMP)

<table>
  <tr>
    <td><LightGBM Regressor></td>
     <td><XGBoost Classifier(500만 기준)></td>
  </tr>
  <tr>
    <td><img src="https://user-images.githubusercontent.com/66674793/104835232-93c8cf00-58e8-11eb-9e36-e93dffcd0baf.png" width=500 height=480></td>
    <td><img src="https://user-images.githubusercontent.com/66674793/104835229-91ff0b80-58e8-11eb-85b6-7358ab0ba8a3.png" width=500 height=480></td>
  </tr>
 </table>

## Conclusion

누적관객수 예측을 최대화하는 모형은 회귀에서는 XGBoost와 LightGBM의 평균을 이용한 모형(Mean_Ensemble) 이었으며, 분류에서는 누적관객수를 500만 기준으로 나누었을 때 XGBoost 분류기가 가장 좋은 예측력을 보여주었다. 스크린 상한제의 효용성에 대한 분석에서는 선형회귀모형을 이용한 우도비 검정과,회귀분석과 분류분석에서의 변수중요도를 살펴보았을 때, 스크린수 변수가 유의미한 변수이며 중요한 변수라는 결론을 얻을 수 있었다. 따라서 스크린 상한제를 시행하는것을 생각해볼 필요가 있다고 생각된다.



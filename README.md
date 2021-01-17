# SMWU-bigdata

## Prediction of Korean Box Office using Data Crawling

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

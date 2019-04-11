## 이전값 대체 함수 정의
## NA 발생 시 앞 단계 값으로 대체..

impute_before <- function(vector_form){
  index <- which(!is.na(vector_form))
  index_lag <- c(index[2:length(index)], length(vector_form)+1)
  index_gap <- index_lag - index
  result <- rep(vector_form[index], times=index_gap)
  result
}

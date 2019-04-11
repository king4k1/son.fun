### linux,mac -> window 간 encoding issue를 해결하기 위함. 
### 데이터형태의 포맷을 넣으면 type argument를 이용하여 원하는 형태의 
### encoding 형태로 변환가능
### 일일이 열을 선택하여 encoding하는 것을 막기 위함.





tbl_iconv <- function(tibble_data, type){
  tibble_data_get <- tibble_data %>% 
    mutate_if(is.factor, as.character) %>% 
    dplyr::select_if(is.character) %>%
    data.frame
  n <- ncol(tibble_data_get)
  for(i in 1:n){
    tibble_data_get[,i] <- iconv(tibble_data_get[,i], type)
  }
  tibble_data[,colnames(tibble_data_get)] <- tibble_data_get
  colnames(tibble_data) <- iconv(colnames(tibble_data), type)
  tibble_data
}

## function

usePackage <- function(p) {
  newPackages <- p[!(p %in% installed.packages()[, "Package"])]
  if(length(newPackages))
    install.packages(newPackages, dependencies = TRUE)
  cat("Packages successfully loaded:\n")
  sapply(p, require, character.only = TRUE, quietly = TRUE)
}

# devtools old_ver included.

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


pair_xy <- function(x, y){
  n.x <- length(x)
  n.y <- length(y)
  y_result <- c()
  for(i in 1:n.y){
    y_result <- c(y_result, rep(i, n.x))
  }
  result <- data.frame(x = rep(1:n.x, n.y),
                       y = y_result)
  result
}

### category_code 종류
# MT1(대형마트), CS2(편의점), PS3(어린이집, 유치원), SC4(학교), AC5(학원)
# PK6(주차장), OL7(주유소, 충전소), SW8(지하철역), BK9(은행)
# CT1(문화시설), AT4(관광명소), PO3(공공기관), AG2(중개업소)
# AD5(숙박), FD6(음식점), CE7(카페), HP8(병원), PM9(약국)

localdata_kakao <- function(address_lon, address_lat, radius_m, category_code, daum_apikey) {
  base_url <- "https://dapi.kakao.com/v2/local/search/category.json"
  parsed_page <- GET(URLencode(paste(base_url,
                                     '?category_group_code=', category_code,
                                     "&y=", address_lat, '&x=', address_lon,
                                     '&size=1&page=1&radius=', radius_m, 
                                     sep='')),
                     add_headers("Authorization" = paste("KakaoAK ",
                                                         daum_apikey, sep='')))
  
  # json format을 list 형으로 변환
  parsed_json <- content(parsed_page)
  return(parsed_json$meta$total_count)
}

## 이전값 대체 함수 정의
## NA 발생 시 앞 단계 값으로 대체..

impute_before <- function(vector_form){
  index <- which(!is.na(vector_form))
  index_lag <- c(index[2:length(index)], length(vector_form)+1)
  index_gap <- index_lag - index
  result <- rep(vector_form[index], times=index_gap)
  result
}
### convert geocode to address
### 다음 api이용하여 emd convert
### 다음 rest apikey 필요
### 위경도 -> 읍명동 및 법정동 코드 산

get_geocode_kakao <- function(address_lon, address_lat, daum_apikey) {
  base_url <- "https://dapi.kakao.com/v2/local/geo/coord2regioncode.json"
  parsed_page <- GET(paste0(base_url,
                            '?x=', address_lon,
                            "&y=", address_lat,
                            '&input_coord=WGS84'),
                     add_headers('Authorization' = paste0("KakaoAK ",daum_apikey)))
  
  
  # json format을 list 형으로 변환
  parsed_json <- content(parsed_page, "text")[[1]]
  response <- fromJSON(parsed_json)[[2]]
  result <- response %>% as_tibble
  return(result)
}

## autozoom

## Automatically setting zoom size and center lon/lat


autozoom <- function(dataset.name){
  dat <- get(dataset.name)
  long <- select(dat, starts_with("lon")) %>% mutate_all(as.numeric)
  lat <- select(dat, starts_with("lat")) %>% mutate_all(as.numeric)
  center_long <- (max(long[,1]) + min(long[,1]))/2
  center_lat <- (max(lat[,1]) + min(lat[,1]))/2
  range_lat <- range(lat)
  range_lat_diff <- diff(range_lat)
  range_long <- range(long)
  range_long_diff <- diff(range_long)
  range_list <- c(range_lat_diff, range_long_diff)
  range_list_index <- which.max(range_list)
  range_list_key <- range_list[range_list_index]
  crit <- c(-Inf,12.8*2^(-15:0))
  zoom <- 17 - as.numeric(cut(range_list_key, crit))
  list(center = c(center_long, center_lat), zoom=zoom)
}


### 지하철 주소를 얻기 위한 api
### 공공데이터포털 내 외부코드로 지하철 역 정보 조회

address_station <- function(code, apikey) {
  base_url <- "http://openAPI.seoul.go.kr:8088/"
  url_fed_to_get <- paste0(base_url,apikey, "/xml",
                           '/SearchSTNInfoByFRCodeService',
                           '/1/5/', code)
  APItree <- xmlTreeParse(url_fed_to_get, useInternalNodes = TRUE)
  APIroot <- xmlRoot(APItree)
  APIports <- APIroot[[3]]
  result <- data.frame("STATION_NM" =  
                         xmlApply(APIports[2]$STATION_NM, xmlValue)$text,
                       "STATION_NM_ENG" = 
                         xmlApply(APIports[3]$STATION_NM_ENG, xmlValue)$text,
                       "address" = 
                         xmlApply(APIports[13]$ADDRESS, xmlValue)$text,
                       "lat" =
                         xmlApply(APIports[36]$XPOINT_WGS, xmlValue)$text,
                       "long" = xmlApply(APIports[37]$YPOINT_WGS, xmlValue)$text)
  return(result)
}


### 용도지역 점수 
### seoul_usezoning.rmd 참고

gausskernel_Haversine <- function (origin, mat = NULL, sigma = NULL) {
  return(exp(-1 * as.matrix(distHaversine(origin, mat)^2)/sigma))
}


find_usezoning_circle <- function(long_select, lat_select, sd_select){
  data(gridpoint_usz, envir = environment())
  longcut <- gridpoint_usz[which(gridpoint_usz$long <= long_select + 0.00566 &
                                   gridpoint_usz$long >= long_select -0.00566),]
  ### long 기준으로 0.00566정도는 좌우로 약 500m 거리의 지점을 의미합니다.
  latcut <- longcut[which(longcut$lat <= lat_select + 0.00449 &
                            longcut$lat >= lat_select - 0.00449),]
  ### lat 기준으로 0.00449정도는 상하로 약 500m 거리의 지점을 의미합니다.
  ### 즉, 역 기준으로 1km의 정사각형 구간으로 공간을 한정합니다.
  ### 이후 distHaversine 수식을 이용하여 500m 내에 포함되는 spot만 간추려냅니다.
  dist <- c()
  for(i in 1:nrow(latcut)){
    dist[i] <- distHaversine(c(as.numeric(latcut$long[i]),
                               as.numeric(latcut$lat[i])),
                             c(long_select, lat_select))
  }
  latcut_result <- latcut[which(dist <= 500 & dist !=0),]
  latcut_result$dist <- dist[which(dist <= 500 & dist !=0)]
  if(length(dist)==1){
    latcut_result <- latcut
  }
  latcut_result$weight_sd <- 
    gausskernel_Haversine(origin = c(long_select, lat_select),
                          mat = data.matrix(latcut_result[,c("long","lat")]),
                          sigma = sd_select^2)[,1]
  if(length(dist)==1){
    latcut_result$weight_sd <- 0
  }
  result <- latcut_result %>% 
    group_by(type) %>% 
    summarise(ratio=sum(weight_sd)) %>% spread(type, ratio)
  gg <- ggplot(latcut_result, 
               aes(x=long, y=lat, col = type, size=weight_sd)) + 
    geom_point() + coord_fixed() + 
    scale_x_discrete(breaks=c(as.numeric(seq(min(latcut_result$long), 
                                             max(latcut_result$long), 0.0002)))) + 
    scale_y_discrete(breaks=c(as.numeric(seq(min(latcut_result$lat), 
                                             max(latcut_result$lat), 0.0002)))) +
    theme_bw()
  
  n_point <- nrow(latcut_result)
  result_list <- list(table = result, n = n_point, plot = gg)
  result_list
}

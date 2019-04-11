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
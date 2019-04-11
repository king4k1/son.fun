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


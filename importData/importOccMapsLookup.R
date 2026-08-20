#Get a list of occupational codes to SOC codes lookup from teh occupational maps API
library(httr2)
library(jsonlite)

#Load api key and source
api_key <- Sys.getenv("OMAPS_API_KEY")
base_url <- "https://occupational-maps-api.skillsengland.education.gov.uk"

# Get a list of all occupational codes (OCC...)
resp <- request(paste0(base_url, "/api/v1/Occupations")) |>
  req_headers(
    "X-API-KEY" = api_key,
    "accept" = "application/json"
  ) |>
  req_perform()

occs <- resp |>
  resp_body_json(simplifyVector = TRUE)

occ_codes <- occs$stdCode

#Request the matching SOC code for each OCC code
get_soc <- function(occ_code) {
  
  tryCatch({
    
    x <- request(
      paste0(
        base_url,
        "/api/v1/Occupations/",
        occ_code,
        "?expand=occupation.soc"
      )
    ) |>
      req_headers(
        "X-API-KEY" = api_key,
        "accept" = "application/json"
      ) |>
      req_perform() |>
      resp_body_json(simplifyVector = TRUE)
    
    tibble(
      occ_code = x$stdCode,
      occupation_name = x$name,
      soc2020_code = x$soc$soc2020Code,
      soc2020_description = x$soc$soc2020Description,
      soc2010_code = x$soc$soc2010Code,
      soc2010_description = x$soc$soc2010Description
    )
    
  }, error = function(e) {
    
    tibble(
      occ_code = occ_code,
      occupation_name = NA_character_,
      soc2020_code = NA,
      soc2020_description = NA_character_,
      soc2010_code = NA,
      soc2010_description = NA_character_
    )
    
  })
}

lookup <- map_dfr(occ_codes, get_soc)

#Get standardised standard names
std_names<-read_csv("Data/1-10_std_lookup/Apprenticeships.csv",skip = 1) |> 
  select(st_code=Reference,st_name=Name)

#Create a lookup for Standard code to OCC to SOC
C_STD_SOC_lookup <- lookup |>
  #get std code which is just occ code minus any suffix
  mutate(
    st_code = gsub("^OCC", "ST", occ_code),
    st_code = gsub("[A-Z]$", "", st_code)
  ) |> 
  #get 1 and 2 digit soc code
  mutate(soc2020_code_1 = soc2020_code %/% 1000,
         soc2020_code_2 = soc2020_code %/% 100) |> 
  left_join(std_names)

saveRDS(C_STD_SOC_lookup, "Data/AppData/C_STD_SOC_lookup.rds")

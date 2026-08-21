#Import the links to the LSIP websites

#Source of ERB urls
url <- "https://www.gov.uk/government/publications/designated-employer-representative-bodies/notice-of-designated-employer-representative-bodies"

#Read in page
pg <- read_html(url)

#Get tables from the page
tables <- html_table(pg)

#Combine html tables
lsip_tbl <- map_dfr(tables, function(x) {
  names(x) <- str_squish(names(x))
  tibble(
    LSIP = x[[1]],
    ERB  = x[[2]]
  )
})

#Extract all hyperlinks from page
nodes <- html_elements(pg, "a")
#Get just lsip ones
links <- tibble::tibble(
  text = html_text2(nodes),
  URL = html_attr(nodes, "href")
) |> 
  filter(
    grepl("^https?://", URL),
    !grepl("gov.uk", URL),
    text != ""
  )

# Add links to table
lsipUrls <- lsip_tbl |> 
  mutate(URL = links$URL)

#Add in all the sub London geographies
london_row <- lsipUrls |> 
  filter(LSIP == "Greater London")

london_subs <- tibble(
  LSIP = c(
    "Central London Forward",
    "Local London",
    "South London Partnership",
    "West London Alliance"
  ),
  ERB = london_row$ERB,
  URL = london_row$URL
)

#Add in three rows for CAs which aren't LSIPs (won't create any new 'LSIP links' because this is just a lookup)
#The NA for ERB and URL here is relevant because it stops them from referring back to the LSIP
extra_cas <- tibble(
  LSIP = c(
    "Devon and Torbay",
    "Greater London Authority",
    "West of England"
  ),
  ERB = c("NA","NA","NA"),
  URL = c("NA","NA","NA")
)

C_lsipUrls <- lsipUrls |> 
  filter(LSIP != "Greater London") |> 
  bind_rows(london_subs) |> 
  bind_rows(extra_cas) |>
#Get rid of bracketed parts of names in webpage so names match
  mutate(
    LSIP = gsub(" \\(.*\\)$", "", LSIP)
  ) |> 
#align LSIP names
  mutate(LSIP = case_when(LSIP=="Hampshire and Solent"  ~ "Hampshire and the Solent",
                          LSIP=="South-east Midlands" ~ "South-East Midlands",
                          TRUE ~ LSIP)) |> 
  ## No better idea here than to hard-code in where the LSIP overlaps with a combined authority because there's no canonical list (even in the main page
  ## where is has the Strategic Authority, this is not always a perfect overlap). Where there is one, get the link. Where there isn't one, make it something
  ## that we can use to filter out
  mutate(CA_Name = case_when(LSIP == "North East" ~ "North East Mayoral Strategic Authority",
                             LSIP == "Tees Valley" ~ "Tees Valley Combined Authority",
                             LSIP == "Greater Manchester" ~ "Greater Manchester Combined Authority",
                             LSIP == "Lancashire" ~ "Lancashire Combined County Authority",
                             LSIP == "Liverpool City Region" ~ "Liverpool City Legion Combined Authority",
                             LSIP == "Hull and East Yorkshire" ~ "Hull and East Yorkshire Combined Authority",
                             LSIP == "South Yorkshire" ~ "South Yorkshire Mayoral Combined Authority",
                             LSIP == "West Yorkshire" ~ "West Yorkshire Combined Authority",
                             LSIP == "York and North Yorkshire" ~ "York and North Yorkshire Combined Authority",
                             LSIP == "East Midlands" ~ "East Midlands Combined County Authority",
                             LSIP == "Greater Lincolnshire" ~ "Greater Lincolnshire Combined County Authority",
                             LSIP == "Warwickshire" ~ "Warwickshire County Council Local Authority",
                             LSIP == "West Midlands" ~ "West Midlands Combined Authority",
                             LSIP == "Cambridgeshire and Peterborough" ~ "Cambridgeshire and Peterborough Combined Authority",
                             LSIP == "Buckinghamshire" ~ "Buckinghamshire Council Local Authority",
                             LSIP == "Surrey" ~ "Surrey County Council Local Authority",
                             LSIP == "Devon and Torbay" ~ "Devon and Torbay Combined Authority",
                             LSIP == "Greater London Authority" ~ "Greater London Authority",
                             LSIP == "West of England" ~ "West of England Combined Authority",
                             TRUE ~ "No Overlap"),
         CA_Link = case_when(LSIP == "North East" ~ "https://www.northeast-ca.gov.uk/",
                             LSIP == "Tees Valley" ~ "https://teesvalley-ca.gov.uk/",
                             LSIP == "Greater Manchester" ~ "https://www.greatermanchester-ca.gov.uk/",
                             LSIP == "Lancashire" ~ "https://lancashire-cca.gov.uk/",
                             LSIP == "Liverpool City Region" ~ "https://www.liverpoolcityregion-ca.gov.uk/",
                             LSIP == "Hull and East Yorkshire" ~ "https://www.hullandeastyorkshire.gov.uk/",
                             LSIP == "South Yorkshire" ~ "https://www.southyorkshire-ca.gov.uk/",
                             LSIP == "West Yorkshire" ~ "https://www.westyorks-ca.gov.uk/",
                             LSIP == "York and North Yorkshire" ~ "https://yorknorthyorks-ca.gov.uk/",
                             LSIP == "East Midlands" ~ "https://www.eastmidlands-cca.gov.uk/",
                             LSIP == "Greater Lincolnshire" ~ "https://greaterlincolnshire-cca.gov.uk/",
                             LSIP == "Warwickshire" ~ "https://www.warwickshire.gov.uk/",
                             LSIP == "West Midlands" ~ "https://www.wmca.org.uk/",
                             LSIP == "Cambridgeshire and Peterborough" ~ "https://cambridgeshirepeterborough-ca.gov.uk/",
                             LSIP == "Buckinghamshire" ~ "https://www.buckinghamshire.gov.uk/",
                             LSIP == "Surrey" ~ "https://www.surreycc.gov.uk/",
                             LSIP == "Devon and Torbay" ~ "https://www.devonandtorbay-cca.gov.uk/",
                             LSIP == "Greater London Authority" ~ "https://www.london.gov.uk/who-we-are/what-london-assembly-does/london-assembly-research-unit-publications/greater-london-authority-powers-and-functions",
                             LSIP == "West of England" ~ "https://www.westofengland-ca.gov.uk/",
                             TRUE ~ "No Overlap")) |>
  mutate(LSIP=paste0(LSIP," LSIP"))

saveRDS(C_lsipUrls, "Data/AppData/C_lsipUrls.rds")


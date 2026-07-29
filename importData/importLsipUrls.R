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

C_lsipUrls <- lsipUrls |> 
  filter(LSIP != "Greater London") |> 
  bind_rows(london_subs) |> 
#Get rid of bracketed parts of names in webpage so names match
  mutate(
    LSIP = gsub(" \\(.*\\)$", "", LSIP)
  ) |> 
#align LSIP names
  mutate(LSIP = case_when(LSIP=="Hampshire and Solent"  ~ "Hampshire and the Solent",
                          LSIP=="South-east Midlands" ~ "South-East Midlands",
                          TRUE ~ LSIP)) |> 
  mutate(LSIP=paste0(LSIP," LSIP"))

saveRDS(C_lsipUrls, "Data/AppData/C_lsipUrls.rds")


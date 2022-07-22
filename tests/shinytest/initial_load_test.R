run_set_shinytests <- function(dfinputs, outstring, listrecords) {
  # This function loops through a set of inputs and takes a snapshot for each one.
  # dfinputs: data frame containing field list and value list.
  # outstring: the stem for the output filename.
  # listrecords: list of input and output variables to record the values of.
  for (i in 1:nrow(dfinputs)) {
    file <- paste0(outstring, "_", i - 1, ".json")
    eval(parse(text = paste0("app$setInputs(", dfinputs$field[i], '="', dfinputs$value[i], '", timeout_ = 1e+4, values_ = FALSE)')))
    app$snapshot(
      #      items = listrecords,
      filename = file
    )
    #    clean_json(file)
  }
}

app <- ShinyDriver$new("../../", loadTimeout = 6.e4)
app$snapshotInit("initial_load_test", screenshot = FALSE)
app$snapshot(
  filename = "LoadHome.json"
)


dfTestInputs <- data.frame(
  field = c(
    "navbar",
    "datatabset", "lep1",
    "datatabset", "lep1", "lep2",
    "datatabset", "lep1", "lep2",
    "datatabset", "lep1", "lep2"
  ),
  value = c(
    "App",
    "Overview", "Cheshire and Warrington",
    "Employment", "London", "Greater Manchester",
    "Vacancies", "London", "Greater Manchester",
    "FE", "London", "Greater Manchester"
  )
)
run_set_shinytests(dfTestInputs, "localskills", "")

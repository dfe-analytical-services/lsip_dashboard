run_set_shinytests <- function(dfinputs, outstring, listrecords) {
  # This function loops through a set of inputs and takes a snapshot for each one.
  # dfinputs: data frame containing field list and value list.
  # outstring: the stem for the output filename.
  # listrecords: list of input and output variables to record the values of.
  for (i in 1:nrow(dfinputs)) {
    file <- paste0(outstring, "_", i - 1, ".json")
    message(paste(i, dfinputs$field[i], dfinputs$value[i]))
    eval(parse(text = paste0("app$setInputs(", dfinputs$field[i], '="', dfinputs$value[i], '", timeout_ = 3.6e+4, values_ = FALSE)')))
    app$snapshot(
      #      items = listrecords,
      filename = file
    )
    #    clean_json(file)
  }
}

app <- ShinyDriver$new("../../", loadTimeout = 6.e5)

app$snapshotInit("initial_load_test", screenshot = FALSE)

app$snapshot()


dfTestInputs <- data.frame(
  field = c(
    "navbar", "geoChoiceOver",
    "navbar"
  ),
  value = c(
    "Overview", "Cheshire and Warrington",
    "User guide"
  )
)


message("Looping through input list")
run_set_shinytests(dfTestInputs, "localskills", "")

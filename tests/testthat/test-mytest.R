library(shinytest2)

test_that("Migrated shinytest test: mytest.R", {
  app <- AppDriver$new()


  app$set_inputs(navbar = "Local skills")
  app$set_inputs(GeoType = "LSIP")
  app$set_inputs(lep1 = "Cambridgeshire and Peterborough")
  app$set_inputs(lep1 = "Hull and East Yorkshire")
  app$set_inputs(EmpOcc_rows_current = c(5, 3, 1, 4, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(EmpOcc_rows_all = c(5, 3, 1, 4, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(EmpOcc_state = c(1667466022244, 0, 10, 2, "desc",
    "", TRUE, FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE,
        "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)),
    allow_no_input_binding_ = TRUE)
  app$set_inputs(GeoType = "LEP")
  app$set_inputs(lep1 = "Greater Manchester")
  app$set_inputs(lep2 = "Cheshire and Warrington")
  app$set_inputs(lep2 = "Lancashire")
  app$set_inputs(datatabset = "Vacancies")
  app$set_inputs(datatabset = "Employment")
  app$set_inputs(`plotly_afterplot-A` = "\"EmpRate_time\"", allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":530,\"height\":400}",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_afterplot-A` = "\"EmpRate_time\"", allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":679,\"height\":400}",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(EmpOcc_rows_current = c(14, 9, 8, 19, 17, 7, 13,
    24, 21, 22), allow_no_input_binding_ = TRUE)
  app$set_inputs(EmpOcc_rows_all = c(14, 9, 8, 19, 17, 7, 13, 24,
    21, 22, 10, 16, 2, 15, 23, 4, 20, 5, 6, 18, 25, 3, 1, 11,
    12), allow_no_input_binding_ = TRUE)
  app$set_inputs(EmpOcc_state = c(1667466045771, 0, 10, c("1",
    "asc"), "", TRUE, FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE),
    c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE,
        TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(EmpOcc_rows_current = c(12, 11, 1, 3, 25, 18,
    6, 5, 20, 4), allow_no_input_binding_ = TRUE)
  app$set_inputs(EmpOcc_rows_all = c(12, 11, 1, 3, 25, 18, 6, 5,
    20, 4, 23, 15, 2, 16, 10, 22, 21, 24, 13, 7, 17, 19, 8, 9,
    14), allow_no_input_binding_ = TRUE)
  app$set_inputs(EmpOcc_state = c(1667466048507, 0, 10, c("1",
    "desc"), "", TRUE, FALSE, TRUE, c(TRUE, "", TRUE, FALSE,
    TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE,
    FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(EmpOcc_rows_current = c(14, 9, 17, 21, 13, 24,
    19, 7, 8, 10), allow_no_input_binding_ = TRUE)
  app$set_inputs(EmpOcc_rows_all = c(14, 9, 17, 21, 13, 24, 19,
    7, 8, 10, 22, 16, 15, 2, 23, 4, 5, 20, 6, 1, 18, 3, 11, 25,
    12), allow_no_input_binding_ = TRUE)
  app$set_inputs(EmpOcc_state = c(1667466049743, 0, 10, c("2",
    "asc"), "", TRUE, FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE),
    c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE,
        TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$set_inputs(EmpOcc_rows_current = c(12, 25, 11, 3, 18, 1,
    6, 20, 5, 4), allow_no_input_binding_ = TRUE)
  app$set_inputs(EmpOcc_rows_all = c(12, 25, 11, 3, 18, 1, 6, 20,
    5, 4, 23, 2, 15, 16, 22, 10, 8, 7, 19, 13, 24, 21, 17, 9,
    14), allow_no_input_binding_ = TRUE)
  app$set_inputs(EmpOcc_state = c(1667466050533, 0, 10, c("2",
    "desc"), "", TRUE, FALSE, TRUE, c(TRUE, "", TRUE, FALSE,
    TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE,
    FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)), allow_no_input_binding_ = TRUE)
  app$expect_download("download_btn1a")
  app$set_inputs(datatabset = "Vacancies")
  app$set_inputs(`plotly_afterplot-A` = "\"jobad.time\"", allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1389,\"height\":400}",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(lep1 = "Greater Cambridge and Greater Peterborough")
  app$set_inputs(GeoType = "LSIP")
  app$set_inputs(lep2 = "West Midlands and Warwickshire")
  app$set_inputs(lep2 = "West Yorkshire")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":0,\"pointNumber\":2,\"x\":2019,\"y\":201.6}]",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":1,\"pointNumber\":4,\"x\":2021,\"y\":188.2}]",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":1,\"pointNumber\":1,\"x\":2018,\"y\":263.4}]",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$set_inputs(datatabset = "Skills")
  app$set_inputs(skill_line = "Education and training (adults only)")
  app$set_inputs(skill_line = "Community learning (adults only)")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":0,\"pointNumber\":0,\"x\":0.0651387213510253,\"y\":15.775}]",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":1,\"pointNumber\":6,\"x\":0.206607236497116,\"y\":10.225}]",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$expect_download("download_btn2b")
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":0,\"pointNumber\":13,\"x\":0.0301568154402895,\"y\":2.775}]",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$set_inputs(navbar = "Data & downloads")
  app$set_inputs(DataTbl_rows_current = c(1, 2, 3, 4, 5, 6, 7,
    8), allow_no_input_binding_ = TRUE)
  app$set_inputs(DataTbl_rows_all = c(1, 2, 3, 4, 5, 6, 7, 8),
    allow_no_input_binding_ = TRUE)
  app$set_inputs(DataTbl_state = c(1667466107339, 0, 10, "", TRUE,
    FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "",
        TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE,
        "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)),
    allow_no_input_binding_ = TRUE)
  app$set_inputs(DataTbl_rows_selected = 1, allow_no_input_binding_ = TRUE)
  app$set_inputs(DataTbl_row_last_clicked = 1, allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$set_inputs(DataTbl_rows_selected = c(1, 2), allow_no_input_binding_ = TRUE)
  app$set_inputs(DataTbl_row_last_clicked = 2, allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$expect_download("downloadData2")
  app$set_inputs(navbar = "Accessibility")
  app$set_inputs(navbar = "Support and feedback")
  app$set_inputs(navbar = "Local skills")
  app$set_inputs(datatabset = "Overview")
  app$set_inputs(`plotly_afterplot-A` = "\"empLineChart\"", allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$set_inputs(`plotly_afterplot-A` = "\"empRateLineChart\"",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_afterplot-A` = "\"VacLineChart\"", allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$set_inputs(`plotly_afterplot-A` = "\"etLineChart\"", allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$set_inputs(`plotly_afterplot-A` = "\"AppLineChart\"", allow_no_input_binding_ = TRUE,
    priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":429,\"height\":81}",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":429,\"height\":81}",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":429,\"height\":81}",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":429,\"height\":81}",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":429,\"height\":81}",
    allow_no_input_binding_ = TRUE, priority_ = "event")
  app$expect_values()
  app$expect_screenshot()
})

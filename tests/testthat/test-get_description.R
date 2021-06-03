test_that("tw_get_description works with more than one id", {

  expect_equal(object =    {
    description <- tw_get_description(
      id = c(
        "Q180099",
        "Q228822"
      ),
      language = "en"
    )

    stringr::str_detect(string = description,
                        pattern = "American anthropologist")
    }
    ,
    c(TRUE, TRUE))
})

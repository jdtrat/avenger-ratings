library(shiny)
library(shinysurveys)
library(shinyrandomize)
library(sortable)
library(googledrive)
library(googlesheets4)

options(
    # whenever there is one account token found, use the cached token
    gargle_oauth_email = TRUE,
    # specify auth tokens should be stored in a hidden directory ".secrets"
    gargle_oauth_cache = ".secrets"
)

sheet_id <- drive_get("avengers_rankings")$id

avenge_questions <- c("Who do you think is the most influential Avenger?",
               "Who do you think is the strongest Avenger (has best super power)?",
               "Who do you think is the most popular Avenger?")
avengers <- c("Black Panther", "Captain America", "Spider-Man",
              "Iron Man", "Doctor Strange", "Thor", "Winter Soldier", "Black Widow", "Falcon",
              "War Machine", "Scarlet Witch", "Rocket", "Star-Lord",
              "Ant Man", "The Wasp", "Groot", "Hawkeye", "Drax", "Captain Marvel", "Mantis")

avenge_ids <- c("most_influential", "strongest", "most_popular")


ques <- data.frame(
    question = purrr::map(.x = avenge_questions, ~rep(.x, length(avengers))) %>%
        purrr::reduce(c),
    option = rep(avengers, length(avenge_questions)),
    input_type = "avenger_rank_list",
    input_id = purrr::map(.x = avenge_ids, ~rep(.x, length(avengers))) %>%
        purrr::reduce(c),
    dependence = NA,
    dependence_value = NA,
    required = TRUE,
    page = purrr::map(.x = seq(1, length(avenge_questions)), ~rep(.x, length(avengers))) %>%
        purrr::reduce(c))

extendInputType("avenger_rank_list", {
    rank_list(text = surveyLabel(),
              labels = surveyOptions(),
              input_id = surveyID(),
              options = sortable_options(
                  swap = FALSE
              )
    )
})


ui <- fluidPage(
    surveyOutput(ques, survey_title = "Avengers Case Study")
)

server <- function(input, output, session) {

    renderSurvey()

    observeEvent(input$submit, {

        response_id <- time_string(5)

        response <- get_survey_data(custom_id = response_id) %>%
            tidyr::separate(response, into = as.character(seq(1, 20)), sep = "\\,") %>%
            tidyr::pivot_longer(cols = `1`:`20`)

        sheet_write(data = response,
                    ss = sheet_id,
                    sheet = response_id
                    )

    })


}

shinyApp(ui, server)




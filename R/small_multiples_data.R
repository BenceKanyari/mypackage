#' Prepare Data for Small Multiples Plotting
#'
#' This function takes a dataset and a column, and creates a new dataset
#' suitable for small multiples plotting. Each unique value of the specified
#' column is used to create a copy of the dataset, with a new column `highlighted`
#' indicating the value to be highlighted.
#'
#' @param data A data.frame or tibble containing the data to be used.
#' @param column A column name (unquoted) in `data` to generate small multiples by.
#'
#' @return A tibble where each row of the original dataset is replicated for each
#'         unique value of the specified column, with a new column `highlighted`
#'         indicating the current value.
#'
#' @examples
#' library(dplyr)
#' library(purrr)
#' df <- tibble(group = c("A","B"), value = 1:2)
#' small_multiples_data(df, group)
#'
#' @export
small_multiples_data <- function(data, column) {
    data |>
        distinct({{column}}) |>
        pull() |>
        map_df(~mutate(data, highlighted = .))
}

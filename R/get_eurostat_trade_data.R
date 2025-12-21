#' Fetch Eurostat International Trade Data (Comext)
#'
#' This function retrieves international trade data from Eurostat's COMEXT
#' database using the JSON-stat API. It supports filtering by frequency,
#' reporter, partner, product, trade flow, transport mode, and other indicators.
#' Data can be fetched in monthly or annual frequency and optionally split by year.
#'
#' @param freq Character. Frequency of the data: "M" for monthly or "A" for annual. Default is "M".
#' @param reporter Character vector. Reporter countries or regions. Default is "EU27_2020".
#' @param partner Character vector. Partner countries or regions. If NULL, defaults to major groups.
#'        Can include "ALL" to fetch all individual countries.
#' @param product Character vector. Product code(s) to fetch. Default is "TOTAL".
#' @param flow Numeric or character. Trade flow code (e.g., 1 for imports, 2 for exports). Default is 1.
#' @param indicators Character vector. Statistical indicators to fetch. Default is "VALUE_IN_EUROS".
#' @param transport_mode Character vector. Optional. Filter data by transport mode.
#' @param start_period Numeric. Starting year for the data. Default is 2023
#' @param by_year Logical. If TRUE, data is fetched year by year to avoid API timeouts. Default is FALSE
#'
#' @return A tibble containing the requested Eurostat trade data with columns such as
#'         reporter, partner, product, flow, indicators, transport_mode, time_period, and values.
#'
#' @details
#' - For monthly data, the function generates a sequence of months from `start_period` to the current date.
#' - For annual data, a sequence of years is generated.
#' - If `partner = "ALL"`, the function automatically expands to all valid partner codes recognized by Eurostat.
#' - When `by_year = TRUE`, data is fetched year by year to reduce the risk of timeouts on large datasets.
#'
#' @examples
#' \dontrun{
#' # Fetch monthly trade values for EU27_2020 reporting countries with all partners
#' df <- get_eurostat_trade_data(freq = "M", reporter = "EU27_2020", partner = "ALL")
#'
#' # Fetch annual trade data for specific countries and products
#' df2 <- get_eurostat_trade_data(freq = "A", reporter = c("DE","FR"), product = c("TOTAL","01"))
#'
#' # Fetch data filtered by transport mode
#' df3 <- get_eurostat_trade_data(transport_mode = c("1","2"))
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom tidyr drop_na
#' @importFrom rjstat fromJSONstat
#'
#' @export


get_eurostat_trade_data <- function(
        freq = "M",
        reporter = "EU27_2020",
        partner = NULL,
        product = "TOTAL",
        flow = 1,
        indicators = "VALUE_IN_EUROS",
        transport_mode = NULL,
        start_period = 2023,
        by_year = FALSE,
        cpa = FALSE
) {


    dataset_id <- "ds-045409"
    agency <- "ESTAT"
    version <- "1.0"
    lang <- "en"
    compress <- FALSE


    if (!is.null(transport_mode)) {
        dataset_id <- "ds-059334"

        # --- 1. Default partner list if none provided ---
        if (is.null(partner)) {
            partner <- c(
                "EU27_2020_EXTRA","EU27_2020_INTRA","WORLD"
            )
        } else if("ALL" %in% partner){
            partner <- NULL
        }
    } else if(cpa){
        dataset_id <- "ds-059366"

        # --- 1. Default partner list if none provided ---
        if (is.null(partner)) {
            partner <- c(
                "EXT_EU27_2020","INT_EU27_2020","WORLD"
            )
        } else if("ALL" %in% partner){
            partner <- NULL
        }


        if("VALUE_IN_EUROS" %in% indicators & length(indicators) == 1){
            indicators <- "VALUE_EUR"
        }
    }else{
        # --- 1. Default partner list if none provided ---
        if (is.null(partner)) {
            partner <- c(
                "EU27_2020_EXTRA","EU27_2020_INTRA","WORLD"
            )
        } else if("ALL" %in% partner){
            partner <- NULL
        }
    }




    # --- 2. Generate TIME_PERIOD sequence ---
    if (freq == "A") {
        periods <- seq(start_period, as.numeric(format(Sys.Date()-lubridate::dmonths(2), "%Y")))
    } else if (freq == "M") {
        start_date <- as.Date(paste0(start_period, "-01-01"))
        periods <- format(seq(start_date, as.Date(Sys.Date()-lubridate::dmonths(2)), by = "month"), "%Y-%m")
    } else {
        stop("Frequency must be 'A' (annual) or 'M' (monthly).")
    }

    collapse <- function(x) paste(x, collapse = ",")

    # --- 3. Function to build URL for given time periods ---
    build_url <- function(time_periods) {
        base_url <- sprintf(
            "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/data/dataflow/%s/%s/%s/*.*.*.*.*.*?",
            agency, dataset_id, version
        )

        params <- c(
            sprintf("c[freq]=%s", freq),
            if (!is.null(reporter)) sprintf("c[reporter]=%s", collapse(reporter)),
            if (!is.null(partner)) sprintf("c[partner]=%s", collapse(partner)),
            if (!is.null(product)) sprintf("c[product]=%s", collapse(product)),
            if (!is.null(flow)) sprintf("c[flow]=%s", collapse(flow)),
            if (!is.null(transport_mode)) sprintf("c[transport_mode]=%s", collapse(transport_mode)),  # <--- NEW
            if (!is.null(indicators)) sprintf("c[indicators]=%s", collapse(indicators)),
            sprintf("c[TIME_PERIOD]=%s", collapse(rev(time_periods))),
            sprintf("compress=%s", tolower(as.character(compress))),
            "format=json",
            sprintf("lang=%s", lang)
        )

        paste0(base_url, paste(params, collapse = "&"))
    }

    # --- 4. Fetch data ---
    all_data <- list()

    if (by_year) {
        unique_years <- unique(substr(periods, 1, 4))
        for (yr in unique_years) {
            chunk_periods <- if (freq == "A") yr else periods[substr(periods, 1, 4) == yr]
            url <- build_url(chunk_periods)
            cat("Fetching data for:", yr, "\n")

            df <- tryCatch({
                rjstat::fromJSONstat(url, naming = "id") |>
                    dplyr::as_tibble() |>
                    tidyr::drop_na()
            }, error = function(e) {
                warning(paste("Failed to fetch data for", yr))
                NULL
            })

            if (!is.null(df)) all_data[[yr]] <- df
        }
    } else {
        cat("Fetching all data at once...\n")
        url <- build_url(periods)
        df <- tryCatch({
            rjstat::fromJSONstat(url, naming = "id") |>
                dplyr::as_tibble() |>
                tidyr::drop_na()
        }, error = function(e) {
            warning("Failed to fetch all data.")
            NULL
        })

        if (!is.null(df)) all_data[["all"]] <- df
    }

    # --- 5. Combine and return ---
    if(freq == "M"){
        dplyr::bind_rows(all_data) |>
            dplyr::mutate(
                time = lubridate::ym(time)
            )
    }else{
        dplyr::bind_rows(all_data)
    }


}


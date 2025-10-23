#' Get team transfers
#'
#' Returns all transfer arrivals and departures for a given team season
#'
#' @param team_url transfermarkt.com team url for a season
#' @param transfer_window which window the transfer occurred - options include "all" for both, "summer" or "winter"
#'
#' @return returns a dataframe of all team transfers
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export

tm_team_transfers <- function(team_url, transfer_window = "all") {
  main_url <- "https://www.transfermarkt.com"

  if (!tolower(transfer_window) %in% c("all", "summer", "winter"))
    stop("check transfer window is either 'all', 'summer' or 'winter'")

  # .pkg_message("Scraping team transfer arrivals and departures. Please acknowledge transfermarkt.com as the data source")

  each_team_xfer <- function(each_team_url) {
    pb$tick()
    xfers_url <- gsub("startseite", "transfers", each_team_url)

    team_page <- xml2::read_html(xfers_url)

    team_name <- team_page %>% rvest::html_nodes("h1") %>% rvest::html_text() %>% stringr::str_squish() %>% .replace_empty_na()
    league    <- team_page %>% rvest::html_nodes(".data-header__club a") %>% rvest::html_text() %>% stringr::str_squish() %>% .replace_empty_na()
    country   <- team_page %>% rvest::html_nodes(".data-header__content img") %>% rvest::html_attr("title") %>% .replace_empty_na()
    season    <- xfers_url %>% gsub(".*saison_id/", "", .) %>% .replace_empty_na()

    tab_box   <- team_page %>% rvest::html_nodes(".box")
    tab_names <- tab_box %>% rvest::html_nodes("h2") %>% rvest::html_text() %>% stringr::str_squish()
    # need to get the URL to be able to pass transfer window
    xfers_window_box <- tab_box[grep("Transfers ", tab_names)]

    # summer/winter selection
    if (tolower(transfer_window) == "all") {
      summer_winter <- c("s", "w")
    } else if (tolower(transfer_window) == "summer") {
      summer_winter <- "s"
    } else {
      summer_winter <- "w"
    }

    team_df <- data.frame()

    for (each_window in summer_winter) {
      xfers_window_url <- xfers_window_box %>%
        rvest::html_nodes(".content") %>%
        rvest::html_children() %>%
        rvest::html_attr("action")
      xfers_window_url <- xfers_window_url %>%
        paste0(main_url, ., "?saison_id=", season, "&pos=&detailpos=&w_s=", each_window)

      team_page_window <- xml2::read_html(xfers_window_url)
      tab_box_window   <- team_page_window %>% rvest::html_nodes(".box")

      # isolate arrivals & departures tables
      tab_names <- tab_box_window %>%
        rvest::html_node("h2") %>%
        purrr::map_chr(\(.x) {
          .x %>% rvest::html_text() %>% stringr::str_squish()
        })

      tab_box_window_selected <- tab_box_window[which(tab_names %in% c("Arrivals", "Departures"))]
      both_tabs <- tab_box_window_selected %>% rvest::html_nodes(".responsive-table")

      # create output for team of both arrivals and departures
      team_df_each_window <- data.frame()

      for (i in 1:length(tab_box_window_selected)) {
        each_tab <- tryCatch(
          both_tabs[i] %>% rvest::html_nodes("tbody") %>% .[[1]] %>% rvest::html_children(),
          error = function(e) NA_character_
        )

        if (any(is.na(each_tab))) {
          player_df <- data.frame()
        } else {
          player_df <- data.frame()

          for (j in 1:length(each_tab)) {
            # --- transfer_type (Arrivals/Departures) — tag første h2 ---
            player_df[j, "transfer_type"] <- tryCatch(
              tab_box_window_selected[i] %>% rvest::html_nodes("h2") %>% .[1] %>%
                rvest::html_text() %>% stringr::str_squish(),
              error = function(e) NA_character_
            )

            # --- player_name + player_url — vælg <a> der peger på /spieler/ ---
            player_a <- tryCatch(
              each_tab[j] %>% rvest::html_element(".hauptlink a[href*='/spieler/']"),
              error = function(e) NULL
            )

            player_df[j, "player_name"] <- tryCatch(
              if (!is.null(player_a)) player_a %>% rvest::html_text() %>% stringr::str_squish() %>% .replace_empty_na() else NA_character_,
              error = function(e) NA_character_
            )

            player_df[j, "player_url"] <- tryCatch(
              if (!is.null(player_a)) player_a %>% rvest::html_attr("href") %>% paste0(main_url, .) %>% .replace_empty_na() else NA_character_,
              error = function(e) NA_character_
            )

            # --- player_position (første celle hvis flere) ---
            player_df[j, "player_position"] <- tryCatch(
              each_tab[j] %>%
                rvest::html_nodes("td:nth-child(2) tr+ tr td") %>% .[1] %>%
                rvest::html_text() %>% stringr::str_squish() %>% .replace_empty_na(),
              error = function(e) NA_character_
            )

            # --- player_age (første) ---
            player_df[j, "player_age"] <- tryCatch(
              each_tab[j] %>% rvest::html_nodes("td.zentriert:nth-child(3)") %>% .[1] %>%
                rvest::html_text() %>% .replace_empty_na(),
              error = function(e) NA_character_
            )

            # --- nationality (første flag) ---
            player_df[j, "player_nationality"] <- tryCatch(
              each_tab[j] %>% rvest::html_nodes(".zentriert .flaggenrahmen") %>% .[1] %>%
                rvest::html_attr("title") %>% .replace_empty_na(),
              error = function(e) NA_character_
            )

            # --- club_2 + club_2_url — vælg et /verein/-link med tekst (tag sidste hvis flere) ---
            club_links <- tryCatch(
              each_tab[j] %>% rvest::html_elements(".inline-table a[href*='/verein/']"),
              error = function(e) list()
            )
            club_pick <- if (length(club_links)) club_links[[length(club_links)]] else NULL

            player_df[j, "club_2"] <- tryCatch(
              if (!is.null(club_pick)) club_pick %>% rvest::html_text() %>% stringr::str_squish() %>% .replace_empty_na() else NA_character_,
              error = function(e) NA_character_
            )

            player_df[j, "club_2_url"] <- tryCatch(
              if (!is.null(club_pick)) club_pick %>% rvest::html_attr("href") %>% .replace_empty_na() else NA_character_,
              error = function(e) NA_character_
            )

            # --- league_2 (første link efter flag) ---
            player_df[j, "league_2"] <- tryCatch(
              each_tab[j] %>% rvest::html_nodes(".flaggenrahmen+ a") %>% .[1] %>%
                rvest::html_text() %>% stringr::str_squish() %>% .replace_empty_na(),
              error = function(e) NA_character_
            )

            # --- country_2 (første flag-ikon i inline-table) ---
            player_df[j, "country_2"] <- tryCatch(
              each_tab[j] %>% rvest::html_nodes(".inline-table .flaggenrahmen") %>% .[1] %>%
                rvest::html_attr("alt") %>% .replace_empty_na(),
              error = function(e) NA_character_
            )

            # --- transfer_fee — vælg <a>-tekst som ligner beløb/keywords ---
            fee_txts <- tryCatch(
              each_tab[j] %>% rvest::html_elements(".rechts a") %>% rvest::html_text(),
              error = function(e) character(0)
            )
            fee_idx <- tryCatch(
              grep("€|\\bm\\b|\\bk\\b|free|loan|undisclosed|unknown|\\?", fee_txts, ignore.case = TRUE),
              error = function(e) integer(0)
            )
            fee_pick <- if (length(fee_idx)) fee_txts[fee_idx[1]] else NA_character_

            player_df[j, "transfer_fee"] <- tryCatch(
              .replace_empty_na(fee_pick),
              error = function(e) NA_character_
            )

            player_df[j, "is_loan"] <- tryCatch(
              grepl("\\bloan\\b", player_df[j, "transfer_fee"], ignore.case = TRUE),
              error = function(e) NA
            )

            player_df[j, "transfer_fee_dup"] <- tryCatch(
              player_df[j, "transfer_fee"],
              error = function(e) NA_character_
            )

            # --- transfer_fee_notes1 (første ikon-tekst hvis findes) ---
            notes_nodes_len <- tryCatch(
              length(each_tab[j] %>% rvest::html_nodes(".rechts.hauptlink a i")),
              error = function(e) 0L
            )
            if (notes_nodes_len == 0L) {
              player_df[j, "transfer_fee_notes1"] <- NA_character_
            } else {
              player_df[j, "transfer_fee_notes1"] <- tryCatch(
                each_tab[j] %>% rvest::html_nodes(".rechts.hauptlink a i") %>% .[1] %>% rvest::html_text(),
                error = function(e) NA_character_
              )
            }

            # --- window flag ---
            player_df[j, "window"] <- tryCatch(each_window, error = function(e) NA_character_)
          } # end rows loop
        } # end else rows exist

        team_df_each_window <- dplyr::bind_rows(team_df_each_window, player_df)
      } # end tables loop

      team_df <- dplyr::bind_rows(team_df, team_df_each_window)
    } # end summer/winter loop

    team_df <- team_df %>%
      dplyr::mutate(window = dplyr::case_when(
        window == "s" ~ "Summer",
        window == "w" ~ "Winter",
        TRUE ~ "Unknown"
      ))

    # add metadata
    team_df <- cbind(team_name, league, country, season, team_df)

    # cleaning up final output data
    team_df <- team_df %>%
      dplyr::mutate(
        transfer_fee = ifelse(stringr::str_detect(.data[["transfer_fee_dup"]], "Loan fee:"), .data[["transfer_fee_notes1"]], .data[["transfer_fee"]])
      ) %>%
      dplyr::mutate(
        transfer_fee = mapply(.convert_value_to_numeric, euro_value = .data[["transfer_fee"]])
      ) %>%
      dplyr::mutate(
        transfer_fee_dup = ifelse(is.na(.data[["transfer_fee"]]), .data[["transfer_fee_dup"]], NA_character_),
        transfer_fee_dup = gsub("End of loan", "End of loan ", .data[["transfer_fee_dup"]])
      ) %>%
      dplyr::rename(transfer_notes = .data[["transfer_fee_dup"]]) %>%
      dplyr::select(-.data[["transfer_fee_notes1"]])

    #----- Get player stats including goals and appearances: -----#
    team_data_url  <- gsub("startseite", "leistungsdaten", each_team_url)
    team_data_page <- xml2::read_html(team_data_url)

    team_data_table <- team_data_page %>% rvest::html_nodes("#yw1") %>% rvest::html_node("table") %>% rvest::html_nodes("tbody") %>% rvest::html_children()

    player_name   <- team_data_table %>% rvest::html_nodes(".hauptlink") %>% rvest::html_nodes(".hide-for-small") %>% rvest::html_text()
    in_squad      <- team_data_table %>% rvest::html_nodes("td:nth-child(5)") %>% rvest::html_text() %>% gsub("-", "0", .) %>% as.numeric()
    appearances   <- team_data_table %>% rvest::html_nodes("td:nth-child(6)") %>% rvest::html_text() %>% gsub("Not.*", "0", .) %>% as.numeric()
    goals         <- team_data_table %>% rvest::html_nodes(".zentriert:nth-child(7)") %>% rvest::html_text() %>% gsub("-", "0", .) %>% as.numeric()
    minutes_played<- team_data_table %>% rvest::html_nodes(".rechts") %>% rvest::html_text() %>% gsub("\\.", "", .) %>% gsub("'", "", .) %>% gsub("-", "0", .) %>% as.numeric()

    team_data_df <- data.frame(
      player_name     = as.character(player_name),
      in_squad        = as.numeric(in_squad),
      appearances     = as.numeric(appearances),
      goals           = as.numeric(goals),
      minutes_played  = as.numeric(minutes_played)
    )

    # now join the two data sets together:
    team_df <- team_df %>% dplyr::left_join(team_data_df, by = c("player_name"))

    return(team_df)
  }

  # create the progress bar with a progress function.
  pb <- progress::progress_bar$new(total = length(team_url))

  final_output <- team_url %>%
    purrr::map_df(each_team_xfer)

  final_output <- final_output %>%
    dplyr::filter(!is.na(.data[["player_name"]]))

  return(final_output)
}

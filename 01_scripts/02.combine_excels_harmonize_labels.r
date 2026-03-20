# Combine two Excel files and harmonize column labels ----
#
# Usage:
#   Rscript 02.combine_excels_harmonize_labels.r <excel_1> <excel_2> <output.xlsx> [crosswalk.csv|xlsx]

library(dplyr)
library(readxl)
library(janitor)
library(writexl)
library(stringr)

sheet1 <- readxl::read_xlsx("00_data/PSPS_Wave1_Audio_PhaseA.xlsx") %>%
    janitor::clean_names()

sheet2 <- readxl::read_xlsx("00_data/PSPS_Wave1_Audio_PhaseB&C.xlsx") %>%
    janitor::clean_names()

normalize_label <- function(x) {
    x |>
        as.character() |>
        str_trim() |>
        str_to_lower() |>
        str_replace_all("[^a-z0-9]+", "_") |>
        str_replace_all("_+", "_") |>
        str_remove("^_+|_+$")
}

clean_option <- function(x) {
    x <- normalize_label(x)
    x <- na_if(x, "")
    x <- na_if(x, "-")
    x <- na_if(x, "na")
    x <- na_if(x, "null")
    x
}

recode_tone <- function(x) {
    x <- clean_option(x)

    case_when(
        is.na(x) ~ NA_character_,
        str_detect(
            x,
            "not_loud|unclear|loud_resp_not_loud|muffled|inaudible|cannot_hear|cant_hear|hard_to_hear|too_soft|low_volume"
        ) ~ "Not loud and unclear",
        x %in%
            c(
                "loud_and_clear",
                "loud_and_cleqr",
                "loud_clear",
                "clear_and_loud",
                "clear_and_nice_voice",
                "clear_and_loud_voice",
                "clear_voice",
                "nice_voice",
                "clear_nice_voice",
                "fo_has_clear_and_nice_voice",
                "fo_has_clear_and_loud_voice",
                "fo_has_clear_voice",
                "fo_has_a_clear_voice",
                "fo_has_a_clear_nice_voice",
                "fo_has_a_clear_and_nice_voice",
                "clear",
                "loud",
                "good",
                "very_good",
                "fair",
                "moderate",
                "satisfied",
                "very_satisfied",
                "extremely_satisfied",
                "extremely_satisfied_loud_and_clear_but_not_too_loud"
            ) ~ "Loud and clear",
        TRUE ~ NA_character_
    )
}

recode_speed <- function(x) {
    x <- clean_option(x)

    case_when(
        is.na(x) ~ NA_character_,
        str_detect(
            x,
            "too_fast|rapid|very_fast|hurr(y|ying)|speaks_too_quickly|speaking_too_quickly"
        ) ~ "Fast",
        str_detect(x, "fast") ~ "Fast",
        str_detect(x, "needs_improvement") ~ "Moderate",
        str_detect(x, "too_slow|very_slow|slower") ~ "Slow",
        str_detect(x, "slow") ~ "Slow",
        x %in%
            c(
                "moderate",
                "moderately",
                "moderate_speed",
                "mpderate",
                "modeate",
                "normal",
                "normal_speed_of_reading",
                "satisfied",
                "very_satisfied",
                "very_satisfactory",
                "fair",
                "good"
            ) ~ "Moderate",
        TRUE ~ NA_character_
    )
}

recode_ability <- function(x) {
    x <- clean_option(x)

    case_when(
        is.na(x) ~ NA_character_,
        str_detect(
            x,
            "poor|bad|not_good|notgood|needs_improvement|lack|lacking|below_expectation|below_expect|leading|proving|too_fast|inadequate|failed|cant|cannot|did_not|didn_t|unable"
        ) ~ "Bad",
        x %in%
            c(
                "excellent",
                "excelllent",
                "excellient",
                "exvellent",
                "excellennt",
                "identify_ipa",
                "identified_ipa",
                "very_good",
                "verygood",
                "very_well",
                "very_satisfactory",
                "satisfactory"
            ) ~ "Excellent",
        str_detect(
            x,
            "meets_expectation|meets_expectations|good|fair|average|moderately_fair|moderately_good|can_administer|can_deliver|can_explain|can_probe|can_|purpose|explained"
        ) ~ "Fair",
        TRUE ~ NA_character_
    )
}

bind <- bind_rows(sheet1, sheet2) %>%
    .[, 1:6] %>%
    mutate(
        tone_rec = recode_tone(tone),
        speed_rec = recode_speed(speed),
        ability_to_explain_the_study_rec = recode_ability(
            ability_to_explain_the_study
        ),
        ability_to_administer_the_questions_properly_rec = recode_ability(
            ability_to_administer_the_questions_properly
        ),
        ability_to_probe_rec = recode_ability(ability_to_probe)
    )


#------------------------------------------------------------------------------#
#  Make analysis file----
#------------------------------------------------------------------------------#

fin <- bind %>%
    filter(complete.cases(.)) %>%
    select(fo_id, ends_with("rec"))

# Note that each enum gets reviewed multiple times
n_distinct(fin$fo_id)

# Bring in the php data and check how many matches
php <- haven::read_dta("00_data/hh_roster_philippines.dta") %>%
    # Clean up some of the data
    mutate(
        dur_res_member_ident = ifelse(
            dur_res_member_ident < 0 | dur_res_member_ident > 50,
            NA,
            dur_res_member_ident
        )
    )

# How many enums across the dataset?
sum(unique(fin$fo_id) %in% unique(php$fo_id))

# Who are we missing
fin %>%
    filter(!fin$fo_id %in% php$fo_id) %>%
    pull(fo_id)

#------------------------------------------------------------------------------#
#  Section----
#------------------------------------------------------------------------------#
roster_php <- php %>%
    select(caseid, fo_id, fc_id, starts_with("r_")) %>%
    pivot_longer(
        cols = starts_with("r_"),
        names_pattern = "r_(.+)_([0-9]+)",
        names_to = c(".value", "person_id")
    ) %>%
    filter(memid == person_id) %>%
    mutate(across(
        everything(),
        ~ {
            x <- .x
            if (is.character(x)) {
                x <- stringr::str_trim(x)
                x <- dplyr::na_if(x, "")
                x <- dplyr::na_if(x, "NA")
                x <- dplyr::na_if(x, "null")
            }
            x
        }
    )) %>%
    janitor::remove_constant() %>%
    janitor::remove_empty("cols") %>%
    select(-phone:-ph1_ntwk_oth) %>%
    filter(fo_id %in% fin$fo_id)

# Survey counts per enumerator (from roster_php so counts match its scope)
enum_survey_counts <- roster_php %>%
    group_by(fo_id) %>%
    summarize(n_surveys = n_distinct(caseid), .groups = "drop")

# Average demographics and roster size per enumerator
enum_demog_summary <- roster_php %>%
    group_by(fo_id, caseid) %>%
    mutate(roster_size = n()) %>%
    ungroup() %>%
    group_by(fo_id) %>%
    summarize(
        avg_roster_size = mean(roster_size, na.rm = TRUE),
        n_roster_obs    = n(),
        across(
            where(is.numeric) & !any_of(c("person_id", "memid", "roster_size")),
            ~ mean(.x, na.rm = TRUE),
            .names = "avg_{.col}"
        ),
        .groups = "drop"
    )

roster_php <- roster_php %>%
    left_join(enum_survey_counts, by = "fo_id") %>%
    left_join(enum_demog_summary, by = "fo_id")


#------------------------------------------------------------------------------#
#  Create summary stats----
#------------------------------------------------------------------------------#

# Map recoded labels to ordered numeric scores (higher = better).
fin <- fin %>%
    mutate(
        tone_score = dplyr::recode(
            tone_rec,
            "Not loud and unclear" = 0,
            "Loud and clear" = 1,
            .default = NA_real_
        ),
        speed_score = dplyr::recode(
            speed_rec,
            "Slow" = 0,
            "Moderate" = 1,
            "Fast" = 2,
            .default = NA_real_
        ),
        ability_to_explain_the_study_score = dplyr::recode(
            ability_to_explain_the_study_rec,
            "Bad" = 0,
            "Fair" = 1,
            "Excellent" = 2,
            .default = NA_real_
        ),
        ability_to_administer_the_questions_properly_score = dplyr::recode(
            ability_to_administer_the_questions_properly_rec,
            "Bad" = 0,
            "Fair" = 1,
            "Excellent" = 2,
            .default = NA_real_
        ),
        ability_to_probe_score = dplyr::recode(
            ability_to_probe_rec,
            "Bad" = 0,
            "Fair" = 1,
            "Excellent" = 2,
            .default = NA_real_
        ),
        overall_score = rowSums(
            as.data.frame(across(
                c(
                    tone_score,
                    speed_score,
                    ability_to_explain_the_study_score,
                    ability_to_administer_the_questions_properly_score,
                    ability_to_probe_score
                )
            )),
            na.rm = TRUE
        )
    )

score_cols <- c(
    "tone_score",
    "speed_score",
    "ability_to_explain_the_study_score",
    "ability_to_administer_the_questions_properly_score",
    "ability_to_probe_score",
    "overall_score"
)

overall_summary <- fin %>%
    summarize(
        across(
            all_of(score_cols),
            list(
                n = ~ sum(!is.na(.x)),
                mean = ~ mean(.x, na.rm = TRUE),
                sd = ~ sd(.x, na.rm = TRUE)
            ),
            .names = "{.col}_{.fn}"
        )
    ) %>%
    pivot_longer(
        cols = everything(),
        names_to = c("variable", "stat"),
        names_sep = "_(?=[^_]+$)",
        values_to = "value"
    ) %>%
    pivot_wider(
        names_from = "stat",
        values_from = "value"
    )

fo_summary <- fin %>%
    group_by(fo_id) %>%
    summarize(
        across(
            all_of(score_cols),
            list(
                n = ~ sum(!is.na(.x)),
                mean = ~ mean(.x, na.rm = TRUE),
                sd = ~ sd(.x, na.rm = TRUE)
            ),
            .names = "{.col}_{.fn}"
        ),
        .groups = "drop"
    ) %>%
    pivot_longer(
        cols = -fo_id,
        names_to = c("variable", "stat"),
        names_sep = "_(?=[^_]+$)",
        values_to = "value"
    ) %>%
    pivot_wider(
        names_from = "stat",
        values_from = "value"
    )

within_between_summary <- bind_rows(
    lapply(score_cols, function(col_name) {
        fo_stats <- fin %>%
            group_by(fo_id) %>%
            summarize(
                fo_mean = mean(.data[[col_name]], na.rm = TRUE),
                fo_sd = sd(.data[[col_name]], na.rm = TRUE),
                .groups = "drop"
            )

        overall_var <- var(fin[[col_name]], na.rm = TRUE)
        within_var <- mean(fo_stats$fo_sd^2, na.rm = TRUE)
        between_var <- var(fo_stats$fo_mean, na.rm = TRUE)

        tibble::tibble(
            metric = col_name,
            overall_mean = mean(fin[[col_name]], na.rm = TRUE),
            overall_sd = sqrt(overall_var),
            within_sd_mean = mean(fo_stats$fo_sd, na.rm = TRUE),
            within_sd_median = median(fo_stats$fo_sd, na.rm = TRUE),
            between_sd = sqrt(between_var),
            within_var = within_var,
            between_var = between_var,
            icc_like = ifelse(
                is.finite(overall_var) &&
                    !is.infinite(within_var) &&
                    (within_var + between_var) > 0,
                between_var / (within_var + between_var),
                NA_real_
            )
        )
    })
)

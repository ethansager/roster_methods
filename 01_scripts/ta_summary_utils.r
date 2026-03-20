# ta_summary_utils
# Utility: summarize SurveyCTO text audit data in R (ported from Python implementation).

library(dplyr)
library(stringr)
library(tibble)
library(lubridate)

summarize_text_audit <- function(
    ta_df,
    id_field = NULL,
    field_col = "field",
    duration_s_col = "duration_s",
    duration_ms_col = "duration_ms",
    visited_s_col = "visited_s",
    event_col = "event",
    form_time_ms_col = "form_time_ms",
    device_time_col = "device_time",
    start_times = NULL,
    end_times = NULL,
    data_tz = NULL,
    collection_tz = NULL,
    set_rownames = TRUE
) {
    submission_ids <- unique(ta_df[[id_field]])
    summaries <- vector("list", length(submission_ids))

    for (i in seq_along(submission_ids)) {
        subid <- submission_ids[[i]]
        sub_key <- as.character(subid)
        sub_ta_df <- ta_df[ta_df[[id_field]] == subid, , drop = FALSE]

        ## Calculate Summary Stats
        duration_mean <- mean(sub_ta_df[[duration_s_col]], na.rm = TRUE)
        duration_sd <- sd(sub_ta_df[[duration_s_col]], na.rm = TRUE)
        duration_min <- min(sub_ta_df[[duration_s_col]], na.rm = TRUE)
        duration_max <- max(sub_ta_df[[duration_s_col]], na.rm = TRUE)

        ## TODO: fix the extra count and spilt the difference across questions
        fields_unique <- unique(na.omit(sub_ta_df[[field_col]]))

        summary <- list(
            ta_duration_total = duration_s_col,
            ta_duration_mean = duration_mean,
            ta_duration_sd = duration_sd,
            ta_duration_min = duration_min,
            ta_duration_max = duration_max,
            ta_fields = length(fields_unique)
        )

        summary[[id_field]] <- subid

        if (
            is.na(summary$ta_duration_mean) || is.nan(summary$ta_duration_mean)
        ) {
            summary$ta_duration_mean <- 0.0
            summary$ta_duration_sd <- 0.0
            summary$ta_duration_min <- 0.0
            summary$ta_duration_max <- 0.0
        }

        for (field in fields_unique) {
            field <- fields_unique[1]
            field <- as.character(field)
            df_fieldname <- stringr::str_trim(field)
            df_fieldname <- gsub(" ", "_", df_fieldname, fixed = TRUE)
            df_fieldname <- gsub("[", "_", df_fieldname, fixed = TRUE)
            df_fieldname <- gsub("]", "", df_fieldname, fixed = TRUE)
            df_fieldname <- gsub("/", "_", df_fieldname, fixed = TRUE)

            field_df <- sub_ta_df[
                sub_ta_df[[field_col]] == field,
                ,
                drop = FALSE
            ]
        }

        summaries[[i]] <- summary
    }

    summary_df <- dplyr::bind_rows(summaries)
    summary_df <- summary_df[order(summary_df[[id_field]]), , drop = FALSE]
    summary_df <- summary_df[,
        c(id_field, setdiff(names(summary_df), id_field)),
        drop = FALSE
    ]

    field_cols <- names(summary_df)[startsWith(names(summary_df), "ta_field_")]
    for (col in field_cols) {
        summary_df[[col]][is.na(summary_df[[col]])] <- 0
    }

    if (set_rownames) {
        summary_df <- tibble::column_to_rownames(summary_df, var = id_field)
    }

    summary_df
}

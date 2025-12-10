# 01.summary.stats
library(haven)
library(tidyverse)
library(ggplot2)

#------------------------------------------------------------------------------#
#  Ghana ----
#------------------------------------------------------------------------------#
# Read in the data
gha <- haven::read_dta("00_data/hh_roster_ghana.dta")


test <- gha %>%
    tidyr::separate_wider_delim(
        fieldname,
        delim = "/",
        names_sep = "_",
        too_few = "align_start"
    ) %>%
    mutate(
        # Find the furthest (last) non-NA split position
        page = case_when(
            !is.na(fieldname_7) ~ fieldname_7,
            !is.na(fieldname_6) ~ fieldname_6,
            !is.na(fieldname_5) ~ fieldname_5,
            !is.na(fieldname_4) ~ fieldname_4,
            !is.na(fieldname_3) ~ fieldname_3,
            !is.na(fieldname_2) ~ fieldname_2,
            !is.na(fieldname_1) ~ fieldname_1,
            TRUE ~ NA_character_
        )
    )

# Now grep the pages that should be within the roster
test <- test %>%
    filter(fieldname_2 == "section1[1]") %>%
    # Clean up these vars for field lists (i.e all on the same page)
    mutate(
        page = ifelse(
            page %in% c("f_name", "l_name", "popname", "phone"),
            "hhroster_grp",
            page
        )
    )

hh_count <- test %>%
    filter(page == "hhroster_grp" & fieldname_5 == "f_name") %>%
    group_by(hhid) %>%
    mutate(hh_count = n())

summary(hh_count$hh_count)

# For each item how much time does it take
test %>%
    group_by(page) %>%
    summarise(
        avg_time = mean(totaldurationseconds),
        med_time = median(totaldurationseconds),
        min_time = min(totaldurationseconds),
        max_time = max(totaldurationseconds)
    ) %>%
    arrange(desc(avg_time)) %>%
    print(n = 100)

#------------------------------------------------------------------------------#
#  Philippines ----
#------------------------------------------------------------------------------#

php <- haven::read_dta("00_data/hh_roster_philippines.dta") %>%
    # Clean up some of the data
    mutate(
        dur_res_member_ident = ifelse(
            dur_res_member_ident < 0 | dur_res_member_ident > 50,
            NA,
            dur_res_member_ident
        )
    )

# Basic summary by section of roster
# get matching column names
dur_colnames <- grep("dur", names(php), value = TRUE)

php %>%
    select(all_of(dur_colnames)) %>%
    select(-dur_hh_location, -dur_introduction, -dur_consent) %>%
    summary(php_dur)


# Does it faster by number of hh members
php <- php %>%
    mutate(per_person = (dur_res_member_ident / count_res_members) * 60)

summary(lm(per_person ~ count_res_members, data = php))


# Bin scatter
php <- php %>% mutate(bin = ntile(per_person, n = 25))

bin <- php %>%
    group_by(bin) %>%
    summarise(xmean = mean(count_res_members), ymean = mean(per_person)) #find the x and y mean of each bin

ggplot(bin, aes(x = xmean, y = ymean)) +
    geom_point() +
    geom_smooth(method = "lm")


php <- php %>%
    mutate(per_person = dur_res_member_ident / count_res_members)

# Looking at demo cor between time taken and hh cats

# or select those cols from the dataframe
php %>%
    select(all_of(dur_colnames)) %>%
    select(-dur_hh_location, -dur_introduction, -dur_consent) %>%
    summary(php_dur)

#------------------------------------------------------------------------------#
#  Comparison ----
#------------------------------------------------------------------------------#
php_per_person_avg <- c("4.265 HH Members", 1.28 * 60, "8 items")
gha_per_person_avg <- c("5.09 HH Members", 82.2, "4 items")


# Time to list one person on avergae
data.frame(php_per_person_avg, gha_per_person_avg)

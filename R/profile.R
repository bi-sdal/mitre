library(tidyr)
library(dplyr)
library(scales)

count_missing <- function(col) {
    data_type <- class(col)
    # print(data_type)
    if (is.numeric(col)) {
        # print(is.numeric(col))
        return(sum(is.na(col)))
    } else {
        warning('Non numeric or character type found. Returning NA')
        return(NA)
    }
}

count_missing_tidy <- function(df) {
    missings <- lapply(df, count_missing)
    miss_df <- tidyr::gather(data.frame(missings),
                             key = 'column_variable',
                             value = 'num_missing') %>%
        dplyr::mutate(num_obs = n(),
                      prop_miss = num_missing / num_obs,
                      pct_miss = scales::percent(prop_miss))
    return(miss_df)
}

library(dplyr)
library(ggplot2)

coefs_sim_num <- function(fit, sim_num) {
    coefs_df <- broom::tidy(fit)
    coefs_df['sim_num'] <- sim_num
    coefs_df['or'] <- exp(coefs_df['estimate'])
    return(coefs_df)
}

scale_vars_all <- function(coef_df, sds) {
  stopifnot(!any(duplicated(coef_df$term))) # make sure no duplicate terms
  
  ## make sure i have a sd for each term, expect for (Intercept)
  stopifnot(sum(coef_df$term %in% names(sds)) == length(coef_df$term) - 1)
  
  scaled_df <- sds %>%
    data.frame %>%
    tidyr::gather(value = 'sd') %>%
    dplyr::right_join(coef_df, by = c('key' = 'term')) %>%
    dplyr::rename('term' = 'key') %>%
    dplyr::mutate(
      estimate_scaled = dplyr::case_when(is.na(sd) ~ estimate,
                                         TRUE ~ estimate * sd),
      or_scaled = exp(estimate_scaled)
    )
  return(scaled_df)
}

summary_coefs <- function(df, col = 'estimate') {
    df %>%
        group_by(term) %>%
        summarize(
            n = n(),
            mean = mean(!!sym(col)),
            sd = sd(!!sym(col)),
            median = median(!!sym(col)),
            min = min(!!sym(col)),
            q25 = quantile(!!sym(col), .25),
            q50 = quantile(!!sym(col), .5),
            q75 = quantile(!!sym(col), .75),
            max = max(!!sym(col))
        )
}

plot_sim_dat_jitter <- function(df, col, title, xlim = NULL, vline = NULL) {
    g <- ggplot(data = df) +
        geom_jitter(aes_string(x = col, y = 'term'), alpha = .1, width = 0) +
        ggtitle(title) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    if (!is.null(xlim)) {g <- g + xlim(xlim)}
    if (!is.null(vline)) {g <- g + geom_vline(aes(xintercept = vline), color = 'red')}
    return(g)
}

plot_sim_dat_hist <- function(df, col, title) {
    ggplot(df) +
        geom_histogram(aes_string(x = col)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        facet_wrap(~term, scales = 'free') +
        ggtitle(title)
}

prop_0 <- function(df, col, side) {
    stopifnot(any(c('>0', '<0') %in% side))
    df %>%
        group_by(term) %>%
        summarize(
            n = n(),
            num = ifelse(side == '>0',
                         sum(!!sym(col) > 0, na.rm = TRUE),
                         sum(!!sym(col) < 0, na.rm = TRUE)),
            prop = num / n
        )
}

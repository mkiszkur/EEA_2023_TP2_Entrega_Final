#Tomado de: https://tdunn.ca/posts/2020-03-15-ordinal-regression-in-r-part-1/

library(ordinal)
library(tibble)
library(gt)
library(tidyr)  # Load the tidyr package
library(dplyr)  # Load the dplyr package
library(forcats)  # Load the forcats package
library(purrr)  # Load purrr for map function


data(wine)
wine <- as_tibble(wine)
glimpse(wine)


wine %>%
  ggplot(aes(y = rating, x = response)) +
  geom_boxplot(width = 0.5) +
  geom_jitter(alpha = 0.5)

wine_red <- "red"

wine %>%
  transmute(temp, contact, bottle, judge, rating = as.numeric(rating)) %>%
  pivot_wider(names_from = judge, values_from = rating) %>%
  gt() %>%
  tab_spanner(columns = `1`:`9`, label = "judge") %>%
  data_color(
    columns = `1`:`9`,
    colors = scales::col_numeric(
      palette = c("white", wine_red), domain = c(1, 5)
    )
  )



wine_plot <- wine %>%
  count(contact, rating, temp) %>%
  mutate(temp = fct_rev(temp)) %>%
  ggplot(aes(x = temp, y = rating, color = temp)) +
  geom_point(aes(group = temp, size = n)) +
  facet_wrap(~contact, scales = "free_x",
             labeller = labeller(contact = label_both)) +
  scale_size(breaks = c(1, 2, 4, 6, 8))

# Display the table
wine_plot




library(patchwork)

wine_prop <- wine %>%
  count(rating) %>%
  mutate(p = n / sum(n), cumsum_p = cumsum(p))


plot1 <- ggplot(wine_prop, aes(x = rating, y = p)) +
  geom_col(fill = wine_red) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  labs(x = "j", y = "proportion")

plot2 <- ggplot(wine_prop, aes(x = as.integer(rating), y = cumsum_p)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  labs(x = "j", y = "cumulative proportion")

plot3 <- ggplot(wine_prop,
                aes(x = as.integer(rating), y = log(cumsum_p) - log(1 - cumsum_p))) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  labs(x = "j", y = "logit(cumulative proportion)")

# Combine the plots horizontally
combined_plots <- plot1 + plot2 + plot3 + plot_layout(ncol = 3)

# Display the combined plots
print(combined_plots)



clm_rating_contact <-
  clm(
    rating ~ contact,
    data = wine, link = "logit"
  )
summary(clm_rating_contact)






# wine %>%
#   crossing(j = 1:4) %>%
#   # Create a binary (0 or 1) to indicate where rating <= j
#   mutate(rating_leq_j = as.numeric(rating) <= j) %>%
#   group_by(j) %>%
#   nest() %>%
#   ungroup() %>%
#   mutate(
#     mod = map(
#       data,
#       ~glm(rating_leq_j ~ 1 + contact,
#            data = ., family = binomial(link = "logit")) %>% broom::tidy()
#     )
#   ) %>%
#   unnest(mod) %>%
#   transmute(
#     j, term,
#     estimate_se = str_c(round(estimate, 2), " (", round(std.error, 2), ")")
#   ) %>%
#   pivot_wider(names_from = term, values_from = estimate_se) %>%
#   left_join(
#     tidy(clm_rating_contact) %>%
#       transmute(
#         j = as.integer(substr(term, 1, 1)),
#         term = if_else(!is.na(j), "theta_j", term),
#         estimate_se = str_c(round(estimate, 2), " (", round(std.error, 2), ")")
#       ) %>%
#       mutate(j = replace_na(j, 1)) %>%
#       spread(term, estimate_se),
#     by = "j"
#   ) %>%
#   ungroup() %>%
#   gt() %>%
#   tab_spanner(label = "Logistic regression",
#               columns = c(`(Intercept)`, contactyes.x)) %>%
#   tab_spanner(label = "CLM",
#               columns = c(theta_j, contactyes.y)) %>%
#   fmt_missing(columns = everything(), missing_text = "")

library(brms)
data(sleepstudy, package = "lme4")

fit1 <- brm(Reaction ~ Days + (Days | Subject), data = sleepstudy, cores = 4)
fit1
str(family(fit1))
model.frame(fit1)
posterior_predict(fit1)
p1 <- fitted(fit1, summary = FALSE)
purrr::map_df(p1 ~ sjstats::hdi(.x, prob = 0.9))
class(fit1b)
sjstats::hdi(fit1)
p1
dim(p1)
library(rstanarm)
fit1b <- stan_glmer(Reaction ~ Days + (Days | Subject), data = sleepstudy, cores = 4)
fit1b
p1b <- posterior_predict(fit1b)
sjstats::hdi(fit1b)

hdi(fit1)
hdi <- function(x, prob = .9, trans = NULL) {
  UseMethod("hdi")
}
fitfram <- stats::model.frame(fit2b)
ggeffects:::get_predictions_stanreg(fit2b, fitfram = fitfram,
                                    ci.lvl = 0.95, ppd = TRUE,
                                    faminfo = "gaussian", type = "fe")

hdi.brmsfit <- function(x, prob = .9, trans = NULL) {
  # get posterior data
  dat <- x %>%
    tibble::as_tibble() %>%
    purrr::map_df(~ sjstats:::hdi_helper(.x, prob, trans)) %>%
    sjmisc::rotate_df() %>%
    tibble::rownames_to_column()

  colnames(dat) <- c("term", "hdi.low", "hdi.high")

  dat
}

p1 %>%
  purrr::map_df(~ hdi.brmsfit(.x, prob = 0.9)) %>%
  sjmisc::rotate_df()

fit2 <- brm(mpg ~ wt + am, data = mtcars, chains = 4, cores = 4)
fit2
p2 <- posterior_predict(fit2)
sjstats::hdi(p2)

plot(ggpredict(fit2, terms = c("am"), ci.lvl = 0.95), ppd = TRUE)

fit2b <- stan_glm(mpg ~ wt + am, data = mtcars, chains = 4, cores = 4)
fit2b
p2b <- posterior_predict(fit2b)
sjstats::hdi(fit2)

sjstats::hdi(fit3)

m2 <- marginal_effects(fit2, effects = c("wt", "am"), plot = FALSE)
plot(m2)
colnames(brms::posterior_predict(fit3)) == colnames(rstanarm::posterior_predict(fit3))

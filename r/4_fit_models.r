# 4_fit_models.r
# 20201207

# %%
fit_asd <- glm(converted ~ 
    sex_male +
    rescale(age) +
    race_black + 
    race_asian + 
    race_multi + 
    ethnicity_latinx + 
    psych_hx_first_degree +
    asd_dx +
    childhood_soc_sx + 
    childhood_rep_sx + 
    hypersensitivity_light + 
    hypersensitivity_sound + 
    hypersensitivity_touch,
    family = binomial(link = 'logit'), 
    data = frame)

fit_asd_summary <- summary(fit_asd)
fit_asd_frame <- tidy(fit_asd, exponentiate=TRUE)

# %% [markdown]
# # ADHD symptoms as predictors of conversion

# %%
fit_adhd <- glm(converted ~ 
    sex_male + 
    rescale(age) +
    race_black + 
    race_asian + 
    race_multi + 
    ethnicity_latinx + 
    psych_hx_first_degree +
    adhd_dx +
    childhood_att_sx,
    family = binomial(link = 'logit'), 
    data = frame)

fit_adhd_summary <- summary(fit_adhd)
fit_adhd_frame <- tidy(fit_adhd, exponentiate=TRUE)

# %% [markdown]
# # Neurodevelopmental symptoms as predictors of conversion

# %%
fit_developmental <- glm(converted ~ 
    sex_male + 
    rescale(age) +
    race_black + 
    race_asian + 
    race_multi + 
    ethnicity_latinx + 
    psych_hx_first_degree +
    asd_dx +
    adhd_dx + 
    childhood_att_sx + 
    childhood_soc_sx + 
    childhood_rep_sx + 
    hypersensitivity_light + 
    hypersensitivity_sound + 
    hypersensitivity_touch,
    family = binomial(link = 'logit'), 
    data = frame)

fit_asd_df <- 
    tidy(fit_asd, exponentiate=TRUE, conf.int=TRUE) %>%
    arrange(desc(estimate)) %>%
    write_excel_csv(here("r", "output", "models", "asd.csv"))

fit_adhd_df <- 
    tidy(fit_adhd, exponentiate=TRUE, conf.int=TRUE) %>%
    arrange(desc(estimate)) %>%
    write_excel_csv(here("r", "output", "models", "adhd.csv"))

fit_developmental_df <- 
    tidy(fit_developmental, exponentiate=TRUE, conf.int=TRUE) %>%
    arrange(desc(estimate)) %>%
    write_excel_csv(here("r", "output", "models", "developmental.csv"))

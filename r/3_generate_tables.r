# 3_generate_tables.r
# 20201207

table_1_full <- frame %>%
    select(
        age,
        converted,
        sex_male,
        race_white,
        race_black,
        race_asian,
        race_native,
        race_multi,
        ethnicity_latinx,
        psych_hx_first_degree,
        asd_dx,
        adhd_dx,
        childhood_soc_sx,
        childhood_anx_sx,
        childhood_att_sx,
        childhood_rep_sx,
        childhood_agg_sx,
        hypersensitivity_light,
        hypersensitivity_sound,
        hypersensitivity_touch) %>% 
    skim() %>%
    filter(stat == "top_counts" | stat == "mean" | stat == "sd") %>%
    select(-c(type, level, formatted)) %>%
    mutate(value = round(value,2)) %>%
    write_excel_csv(here("r","output","table_1","table_1_full_sample.csv"))

table_1_grouped <- frame %>%
    select(
        age,
        converted,
        sex_male,
        race_white,
        race_black,
        race_asian,
        race_native,
        race_multi,
        ethnicity_latinx,
        psych_hx_first_degree,
        asd_dx,
        adhd_dx,
        childhood_soc_sx,
        childhood_anx_sx,
        childhood_att_sx,
        childhood_rep_sx,
        childhood_agg_sx,
        hypersensitivity_light,
        hypersensitivity_sound,
        hypersensitivity_touch 
    ) %>% 
    group_by(converted) %>% 
    skim() %>% 
    filter(stat == "top_counts" | stat == "mean" | stat == "sd") %>%
    select(-c(type, level, formatted)) %>% 
    mutate(value = round(value,2)) %>% 
    write_excel_csv(here("r","output","table_1","table_1_grouped_sample.csv"))

frame_categorical <- frame %>%
    select(
        converted,
        sex_male,
        race_white,
        race_black,
        race_asian,
        race_multi,
        ethnicity_latinx,
        psych_hx_first_degree,
        asd_dx,
        adhd_dx,
        childhood_soc_sx,
        childhood_anx_sx,
        childhood_att_sx,
        childhood_rep_sx,
        childhood_agg_sx,
        hypersensitivity_light,
        hypersensitivity_sound,
        hypersensitivity_touch)

# +
# Between-group comparisons of categorical variables
table_1_fisher_tests <- tibble()

for (variable_name in colnames(select(frame_categorical, -c(converted)))) {
    variable_fisher <- 
        fisher.test(
            frame_categorical[[variable_name]], 
            frame_categorical[['converted']]) %>%
        tidy() %>%
        mutate(variable_name = variable_name)
    table_1_fisher_tests <- table_1_fisher_tests %>% bind_rows(variable_fisher)
}
# -

table_1_fisher_tests %>% 
    write_excel_csv(here("r", "output", "table_1", "table_1_fisher_tests.csv"))

table_1_fisher_tests %>% 
    filter(p.value < 0.05) %>%
    write_excel_csv(
        here("r", "output", "table_1", "table_1_fisher_tests_thresholded.csv"))

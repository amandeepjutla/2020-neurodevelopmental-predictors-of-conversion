# 2_preprocess.r
# 20201207
# Transform columns

frame <- spreadsheet %>% 
    clean_names() %>%
    # Convert any excel-format dates
    mutate(report_date = if_else(
        nchar(report_date) == 5, 
        as.character(excel_numeric_to_date(as.numeric(report_date))), 
        report_date)) %>%
    # Consider the most recent date the report date
    mutate(
        report_date = gsub(',$|;$', '', report_date)) %>%
    separate(
        report_date,
        into = c('date1', 'date2', 'date3', 'date4', 'date5', 'report_date'),
        sep = ',|;',
        fill = 'left') %>%
    # Drop all other dates
    select(-c(date1, date2, date3, date4, date5)) %>%
    # Get columns into appropriate formats
    mutate(
        subject_id = as.integer(subject_id),
        report_date = parse_date(report_date),
        patient_age = as.integer(patient_age),
        maternal_age = as.integer(maternal_age),
        paternal_age = as.integer(paternal_age),
        parent_psych_his = as.factor(parent_psych_his),
        millestones = as.factor(millestones),
        birth_time = as.factor(birth_time),
        behav_prob_home_school = as.factor(behav_prob_home_school),
        gfs_social = as.integer(gfs_social),
        gfs_role = as.integer(gfs_role),
        preg_comp = as.factor(preg_comp),
        told_pdd = as.factor(told_pdd),
        sex = as.factor(recode(sex, `2` = 0, `1` = 1)), #male = 1
        race = as.factor(race),
        ethnicity = as.factor(recode(ethnicity, `2` = 0, `1` = 1)), #latinx = 1
        conversion = as.factor(conversion),
        siblings_psych_his = as.factor(as.integer(siblings_psych_his)),
        educ_his = as.factor(as.integer(educ_his)),
        hyper_sound = as.factor(as.integer(hyper_sound)),
        sensitivity_light = as.factor(as.integer(sensitivity_light)),
        sensitivity_textureuch = as.factor(as.integer(sensitivity_textureuch)),
        physical_problems = as.factor(as.integer(physical_problems)),
        diag_asd = as.factor(as.integer(diag_asd)),
        diag_adhd = as.factor(as.integer(diag_adhd)),
        sips_p1 = as.integer(sips_p1),
        sips_p2 = as.integer(sips_p2),
        sips_p3 = as.integer(sips_p3),
        sips_p4 = as.integer(sips_p4),
        sips_p5 = as.integer(sips_p5),
        sips_n1 = as.integer(sips_n1),
        sips_n2 = as.integer(sips_n2),
        sips_n3 = as.integer(sips_n3),
        sips_n4 = as.integer(sips_n4),
        sips_n5 = as.integer(sips_n5),
        sips_n6 = as.integer(sips_n6),
        sips_d1 = as.integer(sips_d1),
        sips_d2 = as.integer(sips_d2),
        sips_d3 = as.integer(sips_d3),
        sips_d4 = as.integer(sips_d4),
        sips_g1 = as.integer(sips_g1),
        sips_g2 = as.integer(sips_g2),
        sips_g3 = as.integer(sips_g3),
        sips_g4 = as.integer(sips_g4),
        # One-hot-encode race
        race_white = as.factor(case_when(
            as.numeric(race) == 1 ~ 1,
            as.numeric(race) != 1 ~ 0)),
        race_black = as.factor(case_when(
            as.numeric(race) == 2 ~ 1,
            as.numeric(race) != 2 ~ 0)),
        race_asian = as.factor(case_when(
            as.numeric(race) == 3 ~ 1,
            as.numeric(race) != 3 ~ 0)),
        race_native = as.factor(case_when(
            as.numeric(race) == 5 ~ 1,
            as.numeric(race) != 5 ~ 0)),
        race_multi = as.factor(case_when(
            as.numeric(race) == 4 ~ 1,
            as.numeric(race) != 4 ~ 0)),
        # One-hot-encode parent psych hx
        psych_hx_mother = as.factor(case_when(
            parent_psych_his == 1 ~ 1,
            parent_psych_his != 1 ~ 0)),
        psych_hx_father = as.factor(case_when(
            parent_psych_his == 2 ~ 1,
            parent_psych_his != 2 ~ 0)),
        psych_hx_both_parents = as.factor(case_when(
            parent_psych_his == 3 ~ 1,
            parent_psych_his != 3 ~ 0)),
        psych_hx_either_parent = as.factor(case_when(
            (parent_psych_his == 1 | parent_psych_his == 2) ~ 1,
            parent_psych_his == 0 ~ 0)),
        # Derive psych hx for first degree family member
        psych_hx_first_degree = as.factor(if_else((
            parent_psych_his == 1 | 
            parent_psych_his == 2 | 
            siblings_psych_his == 1), 1, 0)),
        # Dichotimize special education
        special_ed_hx = as.factor(case_when(
            (educ_his == 1 | educ_his == 2) ~ 1,
            educ_his == 0 ~ 0)),
        # One-hot-encode birth timing
        born_early = as.factor(case_when(
            birth_time == 2 ~ 1,
            birth_time != 2 ~ 0)),
        born_late = as.factor(case_when(
            birth_time == 3 ~ 1,
            birth_time != 3 ~ 0)),
        # Dichotimize dev milestones
        developmental_delay_hx = as.factor(case_when(
            millestones == 0 ~ 1,
            millestones != 0 ~ 0)),
        # Dichotomize behavioral problems
        childhood_behavioral_problems_hx = as.factor(case_when(
            behav_prob_home_school != 0 ~ 1,
            behav_prob_home_school == 0 ~ 0))) %>%
    # Strip all non-numeric characters from `number-siblings`, such that 
    # 'At least 1' and '1?' both become '1'. Then convert to integer.
    mutate(
        number_siblings = as.integer(gsub('[^0-9.-]','', number_siblings))) %>%
    # Give consistent names
    rename(
        id = subject_id,
        age = patient_age,
        race_category = race,
        sex_male = sex,
        ethnicity_latinx = ethnicity,
        converted = conversion,
        n_siblings = number_siblings,
        n_siblings_text = number_siblings_desc,
        psych_hx_parent_category = parent_psych_his,
        psych_hx_parent_category_text = parent_psych_desc,
        psych_hx_sibling = siblings_psych_his,
        psych_hx_sibling_text = siblings_psych_desc,
        pregnancy_complications = preg_comp,
        pregnancy_complications_text = preg_comp_desc,
        ed_hx_category = educ_his,
        ed_hx_category_test = educ_his_desc,
        ever_told_pdd = told_pdd,
        ever_told_pdd_text = told_pdd_desc,
        hypersensitivity_sound = hyper_sound,
        hypersensitivity_sound_text = hyper_sound_desc,
        hypersensitivity_light = sensitivity_light,
        hypersensitivity_light_text = sensitivity_light_desc,
        hypersensitivity_touch = sensitivity_textureuch,
        hypersensitivity_touch_text = sensitivity_textureuch_desc,
        childhood_medical_problems_hx = physical_problems,
        childhood_medical_problems_hx_text = physical_problems_desc,
        birth_hx_category = birth_time,
        birth_hx_category_text = birth_desc,
        dev_hx_category = millestones,
        dev_hx_category_text = millestones_desc,
        asd_dx = diag_asd,
        asd_dx_text = diag_asd_desc,
        adhd_dx = diag_adhd,
        adhd_dx_text = diag_adhd_desc,
        childhood_behavioral_problems_category = behav_prob_home_school,
        childhood_behavioral_problems_category_text = behav_prob_home_school_desc,
        sips_n3_text = sips_n3_desc,
        sips_d1_text = sips_d1_desc,
        sips_g3_text = sips_g3_desc,
        notes_text = notes) %>%
    # Merge in dataframes with manually recoded versions of some variables.
    # see /documentation for details and rationale
    left_join(childhood_behavioral_problems, by = 'id') %>%
    left_join(maternal_age, by = 'id') %>%
    left_join(paternal_age, by = 'id') %>%
    left_join(psych_hx_parent, by = 'id') %>%
    left_join(psych_hx_sibling, by = 'id') %>%
    mutate(
        maternal_age = if_else(
            !is.na(maternal_age_recoded), maternal_age_recoded, maternal_age),
        paternal_age = if_else(
            !is.na(paternal_age_recoded), paternal_age_recoded, paternal_age),
    # Factorize variables 
        childhood_soc_sx = as.factor(childhood_soc_sx),
        childhood_anx_sx = as.factor(childhood_anx_sx),
        childhood_att_sx = as.factor(childhood_att_sx),
        childhood_rep_sx = as.factor(childhood_rep_sx),
        childhood_agg_sx = as.factor(childhood_agg_sx),
        sibling_adhd = as.factor(sibling_adhd),
        sibling_asd = as.factor(sibling_asd),
        sibling_learning = as.factor(sibling_learning),
        sibling_psychosis = as.factor(sibling_psychosis),
        parent_psychosis = as.factor(parent_psychosis)
    ) %>%
    # Drop these columns now that they've served their purpose
    select(-c(maternal_age_recoded, paternal_age_recoded)) 

if (SERIALIZE) {
    write_feather(
        frame, 
        paste(INTERMEDIATES_PATH, 'frame_transformed.feather', sep = '/'))
}

# make a backup copy of the transformed frame before dealing with missingness
frame_transformed <- frame

frame <- frame %>%
    filter(
        !is.na(converted) & 
        !is.na(age) & 
        !is.na(race_category) & 
        !is.na(ethnicity_latinx)) %>%
    # Replace NAs with 0 where reasonable
    mutate(
        asd_dx = replace_na(asd_dx, 0),
        adhd_dx = replace_na(adhd_dx, 0),
        born_early = replace_na(born_early, 0),
        childhood_anx_sx = replace_na(childhood_anx_sx, 0),
        childhood_agg_sx = replace_na(childhood_agg_sx, 0),
        childhood_att_sx = replace_na(childhood_att_sx, 0),
        childhood_soc_sx = replace_na(childhood_soc_sx, 0),
        childhood_rep_sx = replace_na(childhood_rep_sx, 0),
        developmental_delay_hx = replace_na(developmental_delay_hx, 0),
        hypersensitivity_light = replace_na(hypersensitivity_light, 0),
        hypersensitivity_sound = replace_na(hypersensitivity_sound, 0),
        hypersensitivity_touch = replace_na(hypersensitivity_touch, 0),
        pregnancy_complications = replace_na(pregnancy_complications, 0),
        special_ed_hx = replace_na(special_ed_hx, 0),
        psych_hx_first_degree = replace_na(psych_hx_first_degree, 0),
        sibling_psychosis = replace_na(sibling_psychosis, 0),
        sibling_asd = replace_na(sibling_asd, 0),
        sibling_adhd = replace_na(sibling_adhd, 0),
        parent_psychosis = replace_na(parent_psychosis, 0))

library(tidyverse)
source(here("model", "R", "default-model", "helpers-tables.R"))

fs = .Platform$file.sep

test_data <- function(path_to_csv) {
  data <- read_csv(path_to_csv) %>%
    mutate(prolific_id = str_trim(str_to_lower(prolific_id))) %>%
    filter(str_detect(prolific_id, "test-.*") | str_detect(prolific_id, "test "))
  return(data)
}

experimental_data <- function(path_to_csv){
  data <- read_csv(path_to_csv) %>%
    mutate(prolific_id = str_trim(str_to_lower(prolific_id))) %>%
    filter(!str_detect(prolific_id, "test.*") & prolific_id != "" &
           !is.na(prolific_id))
  return(data)
}

save_raw_data <- function(data_dir, data_fn, result_dir, result_fn, debug_run=FALSE){
  path_to_data <- paste(data_dir, data_fn, sep=.Platform$file.sep)
  if(debug_run){
    data <- test_data(path_to_data)
  } else {
    data <- experimental_data(path_to_data)
  }
  
  path_target <- paste(result_dir, paste(result_fn, "raw.csv", sep="_"), sep = .Platform$file.sep)
  write_excel_csv(data, path = path_target, delim = ",", append = FALSE, col_names=TRUE)
  print(paste('written raw data to:', path_target))
  return(data)
}

tidy_test_exp1 <- function(df){
  dat.test <- df %>% filter(str_detect(trial_name, "multiple_slider")) %>%
    dplyr::select(trial_name, trial_number,
           prolific_id, RT, QUD, id, group,
           question1, question2, question3, question4,
           response1, response2, response3, response4) %>%
    pivot_longer(cols=c(contains("response")),
                 names_to = "response_idx", names_prefix = "response",
                 values_to = "response") %>%
    pivot_longer(cols=c(contains("question")),
                 names_to = "question_idx", names_prefix = "question",
                 values_to = "question") %>%
    filter(response_idx == question_idx) %>%
    dplyr::select(-response_idx, -question_idx)

  dat.test <- dat.test %>%
    mutate(response = as.numeric(response),
           response = response/100,
           prolific_id = factor(prolific_id),
           group=case_when(id=="ind2" ~ "group1",
                           TRUE ~ group),
           id = factor(id)
  )
  return(dat.test)
}

tidy_test_exp2 <- function(df){
  dat.test <- df %>% filter(startsWith(trial_name, "fridge_view") | trial_name == "fridge_train") %>%
    dplyr::select(prolific_id, RT, QUD, id, group, response1, response2, trial_name,
           trial_number) %>%
    rename(custom_response=response2, response=response1)
  
  dat.test <- dat.test %>%
    mutate(prolific_id = factor(prolific_id),
           id = factor(id))
  return(dat.test)
}

tidy_test_joint <- function(df){
  data.prior = df %>% filter(str_detect(trial_name, "multiple_slider")) %>%
    tidy_test_exp1() %>%
    add_column(custom_response="", utterance="")
  data.production = df %>% filter(str_detect(trial_name, "fridge_")) %>%
    tidy_test_exp2() %>%
    rename(utterance=response) %>%
    add_column(question="")
  dat.test = bind_rows(data.prior, data.production)
  return(dat.test)
}

tidy_train <- function(df){
  dat.train <- df %>%
    filter(startsWith(trial_name, "animation") | trial_name == "multiple_slider_train") %>%
    dplyr::select(prolific_id, RT, expected, QUD, id, trial_name,
           trial_number,
           question1, question2, question3, question4,
           response1, response2, response3, response4
    ) %>%
    pivot_longer(cols=c(contains("response")),
                 names_to = "response_idx", names_prefix = "response",
                 values_to = "response") %>%
    pivot_longer(cols=c(contains("question")),
                 names_to = "question_idx", names_prefix = "question",
                 values_to = "question") %>%
    filter(response_idx == question_idx) %>%
    dplyr::select(-response_idx, -question_idx) %>%
    mutate(prolific_id = factor(prolific_id), id = factor(id)) %>%
    group_by(prolific_id, id) %>%
    mutate(response = as.numeric(response), response = response/100)%>%
    add_normed_exp1("train")
  
  dat.train.norm = dat.train %>% rename(response=r_norm) %>%
    dplyr::select(-r_orig, -n, -trial_name)
  dat.train.orig = dat.train %>% rename(response=r_orig) %>%
    dplyr::select(-r_norm, -n, -trial_name)
  return(list(norm=dat.train.norm, orig=dat.train.orig))
}

tidy_pretest <- function(df){
  dat.pre <- df %>% filter(trial_name == "pretest") %>%
    dplyr::select(prolific_id, question, response, id, trial_name, trial_number) %>%
    mutate(prolific_id = factor(prolific_id), id=factor(id),
           response = as.numeric(response),
           trial_number = as.character(trial_number))
  return(dat.pre)
}

## @arg experiment: "prior", "production", "joint" ##
tidy_data <- function(data, N_trials, experiment){
  # 1. dplyr::select only columns relevant for data analysis
  df <- data %>% dplyr::select(prolific_id, submission_id,
                        question, question1, question2, question3, question4,
                        QUD, response,
                        expected, response1, response2, response3, response4,
                        id, trial_name, trial_number, group,
                        timeSpent, RT,
                        education, comments, gender, age)
  # always use the same abbreviation
  df <- df %>% mutate(question1 = case_when(question1 == "gb" ~ "bg",
                                            question1 == "yr" ~ "ry",
                                           TRUE ~ question1),
                      response3 = as.character(response3),
                      response4 = as.character(response4));
  dat.color_vision <- tibble();
  if(N_trials$color_vision != 0) {
    dat.color_vision <- df %>%
      filter(startsWith(trial_name, "color-vision")) %>%
      dplyr::select(prolific_id, id, question, response, expected, QUD, trial_number)
    df <- df %>% filter(!startsWith(trial_name, "color-vision"));
  }
  dat.slider_choice=tibble()
  dat.attention_check=tibble()
  if(N_trials$slider_choice != 0){
    cols = c("prolific_id", "id", "question", "response", "expected", "trial_name", "trial_number")
    dat.slider_choice = df %>% filter(startsWith(trial_name, "slider_choice_training")) %>%
      dplyr::select(one_of(cols))
    dat.attention_check = df %>% filter(startsWith(trial_name, "attention_check")) %>%
      dplyr::select(one_of(cols)) %>% filter(response != expected)
  }
  N_participants <- df %>% dplyr::select(prolific_id) %>% unique() %>% nrow()
  stopifnot(nrow(df) == N_participants * (N_trials$test + N_trials$train));

  dat.comments <- df %>%
    dplyr::select(prolific_id, comments) %>%
    mutate(comments = as.character(comments),
           comments = if_else(is.na(comments), "", comments)) %>%
    unique()
  dat.info <- df %>% dplyr::select(prolific_id, education, gender, age, timeSpent) %>%
    unique()
  dat.train <- tidy_train(df)
  
  if(experiment == "prior") {
    dat.test <- tidy_test_exp1(df)
  } else if(experiment == "production") {
    dat.test <- tidy_test_exp2(df)
  } else if(experiment == "joint"){
    dat.test <- tidy_test_joint(df)
  } else {
    stop(paste(experiment, 'unknown'))
  }
  dat.pretest <- tidy_pretest(df)
  dat.all <- list(test=dat.test, train.norm=dat.train$norm,
                  train.orig=dat.train$orig, color=dat.color_vision,
                  train.attention=dat.attention_check,
                  train.slider_choice=dat.slider_choice,
                  info=dat.info, comments=dat.comments, pretest=dat.pretest)

  return(dat.all)
}

standardize_color_groups_exp1 <- function(df){
  # ind2 is used as single training example for production task (always group1!)
  df <- df %>%
    mutate(question =
             case_when((question == "bg" | question == "gb" |
                        question=="ry" | question == "yr") ~ "ac",
                        question == "none" ~ "none",
                        group == "group1" & (question == "b" | question=="r") ~ "a",
                        group == "group1" & (question == "g" | question=="y") ~ "c",
                        group == "group2" & question == "g"  ~ "a",
                        group == "group2" & question == "b" ~ "c"
                      ),
           group = "group1",
           question = case_when(question == "a" ~ "b",
                                question == "c" ~ "g",
                                question == "ac" ~ "bg",
                                question == "none" ~ "none")
           )
  return(df)
}

standardize_color_groups_exp2 <- function(df){
  df <- df %>%
    mutate(response = case_when(group == "group2" ~ str_replace_all(response, "blue", "G"),
                                TRUE ~ str_replace_all(response, "blue", "B")),
           custom_response = case_when(group == "group2" ~ str_replace_all(custom_response, "blue", "-G-"),
                                TRUE ~ str_replace_all(custom_response, "blue", "-B-"))) %>%

    mutate(response = case_when(group == "group2" ~ str_replace_all(response, "green", "B"),
                                TRUE ~ str_replace_all(response, "green", "G")),
           custom_response = case_when(group == "group2" ~ str_replace_all(custom_response, "green", "-B-"),
                                TRUE ~ str_replace_all(custom_response, "green", "-G-"))) %>%
    mutate(response = str_replace_all(response, "G", "green"),
           custom_response = str_replace_all(custom_response, "-G-", "green")) %>%
    mutate(response = str_replace_all(response, "B", "blue"),
           custom_response = str_replace_all(custom_response, "-B-", "blue"
           ));
  df <- df %>% mutate(group = "group1", 
                      response = as.factor(response),
                      custom_response = as.factor(custom_response)
                      );

  return(df)
}

# @arg df: data frame containing columns bg, b, g
add_probs <- function(df){
  df <- df %>% mutate(p_a=bg+b, p_c=bg+g, p_na=g+none, p_nc=b+none) %>%
    mutate(p_c_given_a = if_else(p_a==0, 0, bg / p_a),
           p_c_given_na = if_else(p_na==0, 0, g / p_na),
           p_a_given_c = if_else(p_c==0, 0, bg / p_c),
           p_a_given_nc = if_else(p_nc==0, 0, b / p_nc),
           p_nc_given_a = b/p_a,
           p_nc_given_na = none/p_na,
           p_na_given_c = g/p_c,
           p_na_given_nc = none/p_nc,
           p_likely_a = p_a,
           p_likely_na=p_na,
           p_likely_c = p_c,
           p_likely_nc=p_nc
    )
  return(df)
}

# @arg quest: question which is used to generate the clusters, e.g. 'b'
cluster_responses <- function(dat, quest){
  dat.kmeans <- dat %>% filter(question == quest) %>%
    dplyr::select(prolific_id, id, response) %>% add_column(y=1) %>%
    group_by(prolific_id, id) %>%
    unite("rowid", "prolific_id", "id", sep="--") %>%
    column_to_rownames(var = "rowid")
  clusters <- kmeans(dat.kmeans, 2)

  df <- dat.kmeans %>%
    rownames_to_column(var = "rowid") %>%
    as_tibble() %>%
    separate(col="rowid", sep="--", into=c("prolific_id", "id")) %>%
    mutate(cluster=as.factor(clusters$cluster), id=as.factor(id),
           prolific_id = as.factor(prolific_id)) %>%
    dplyr::select(prolific_id, id, cluster)
  df <- left_join(dat, df) 
  df <- df %>% mutate(cluster = fct_explicit_na(df$cluster, na_level = 'not-clustered'))
  return(df)
}

filter_exp1 <- function(df){
  # All 0 or all 1 responses: as one can't believe that all events will
  # certainly (not) happen
  filtered_sum <- df %>%
    group_by(prolific_id, id) %>%
    filter(sum(response) == 0 | sum(response) == 4)
  
  df_filltered <- df %>% filter(sum(response) != 0 & sum(response) != 4)
  print(paste('filtered due to sum=4 or sum=0:', nrow(filtered_sum)))
  return(df_filtered)
}

# @arg df1 in long-format
add_normed_exp1 <- function(df1, test_or_train, epsilon=0.000001){
  data = df1 %>% group_by(prolific_id, id)
  df = data %>% filter(sum(response)!=0)
  zeros = (nrow(data) - nrow(df)) / 4
  message(paste("#datapoints filtered out as all 4 events rated as 0:", zeros))
  # normalize such that slider responses sum up to 1 but also keep original response
  df.with_normed = df %>%
    mutate(n=sum(response + epsilon), r_norm=(response + epsilon)/n) %>%
    rename(r_orig=response)
  print(paste("in", test_or_train, "data"))
  return(df.with_normed)
}

save_prob_tables <- function(df, result_dir, result_fn){
  # Also save just Table means of normalized values as csv files
  means <- df %>% group_by(id, question) %>% summarise(mean=mean(r_norm))
  fn_table_means <- paste(result_fn, "_tables_mean.csv", sep="");
  path_table_means <- paste(result_dir, fn_table_means, sep=.Platform$file.sep);
  write.table(means %>% pivot_wider(names_from = question, values_from = mean),
              file=path_table_means, sep = ",", row.names=FALSE)
  print(paste('written means of normalized probability tables to:', path_table_means))
  
  # All Tables (with normalized values)
  tables.all <- df %>% dplyr::select(id, question, prolific_id, r_norm) %>%
    group_by(id, question, prolific_id) %>%
    pivot_wider(names_from = question, values_from = r_norm)
  fn_tables_all <- paste(result_fn, "_tables_all.csv", sep="");
  path_tables_all <- paste(result_dir, fn_tables_all, sep=.Platform$file.sep);
  write.table(tables.all, file=path_tables_all, sep = ",", row.names=FALSE)
  print(paste('written normalized probability tables to:', path_tables_all))
  
  # save smoothed version of all (normalized) tables as well
  epsilon = 0.00001
  tables.orig <- tables.all %>% rename(AC=bg, `A-C`=b, `-AC`=g, `-A-C`=none)
  tables.mat = tables.orig %>% ungroup() %>%
    dplyr::select(AC, `A-C`, `-AC`, `-A-C`) %>% as.matrix()
  tables.smooth = prop.table(tables.mat + epsilon, 1)
  tables.smooth = cbind(tables.smooth, rowid=seq.int(from=1, to=nrow(tables.mat), by=1)) %>%
    as_tibble() %>% 
    mutate(p_c_given_a=`AC`/(`AC`+`A-C`),
           p_c_given_na=`-AC`/(`-AC`+`-A-C`),
           p_a_given_c=`AC`/(`AC`+`-AC`),
           p_a_given_nc=`A-C`/(`A-C`+`-A-C`),
           p_a=`AC`+`A-C`, p_c=AC+`-AC`, 
           theta_ac = (p_c_given_a - p_c_given_na) / (1 - p_c_given_na),
           theta_anc = ((1-p_c_given_a) - (1-p_c_given_na)) / (1 - (1-p_c_given_na)),
           theta_ca = (p_a_given_c - p_a_given_nc) / (1 - p_a_given_nc),
           theta_cna = ((1-p_a_given_c) - (1-p_a_given_nc)) / (1 - (1-p_a_given_nc)),
    );
  TABLES = left_join(tables.smooth,
    tables.orig %>% dplyr::select(id, prolific_id) %>% rowid_to_column(var="rowid"),
    by="rowid"
  ) %>% dplyr::select(-rowid)

  TABLES.long = TABLES %>% rowid_to_column() %>%
    mutate(ind_diff=AC-(p_a*p_c)) %>%
    pivot_longer(cols=c(-prolific_id, -id), names_to="cell", values_to="val")
  TABLES.mat = TABLES %>% as.matrix()
  TABLES.ind = TABLES %>% filter(str_detect(id, "independent"))
  TABLES.dep = TABLES %>% filter(!str_detect(id, "independent"))
  TABLES.all = bind_rows(TABLES.ind, TABLES.dep)
  
  save_data(TABLES.ind, paste(result_dir, "empiric-ind-tables-smooth.rds", sep=fs))
  save_data(TABLES.dep, paste(result_dir, "empiric-dep-tables-smooth.rds", sep=fs))
  save_data(TABLES.all, paste(result_dir, "empiric-all-tables-smooth.rds", sep=fs))
  
  # enrich empirical tables, fine-grained tables mapped to more coarse tables
  tables.empiric.pids = TABLES.all %>%
    mutate(AC=round(AC, 2), `A-C`=round(`A-C`,2),
           `-AC`=round(`-AC`, 2), `-A-C`=round(`-A-C`, 2))  %>%
    dplyr::select("AC", "A-C", "-AC", "-A-C", "prolific_id", "id") %>% 
    unite("p_id", c(prolific_id, id)) %>% group_by(AC, `A-C`, `-AC`, `-A-C`) %>%
    summarize(p_id=list(p_id)) %>% rowid_to_column("empirical_id")
  save_data(tables.empiric.pids, paste(result_dir, "tables-empiric-pids.rds", sep=fs))

  # to summarize the empirically generated tables, i.e.to make them more
  # coarse-grained, we map each entry to the nearest next number that
  # is divisible by 5, e.g. 87 to 92 mapped to 90, 83 to 87 mapped to 85, then
  # normalize again to get well-defined probability distributions
  # for each empirical table, list all tables that would have been mapped to the
  # same table and check how many are then generated by the fitted prior distributions
  tables.empiric.wide = tables.empiric.pids %>%
    mutate(AC=round(AC*100,2), `A-C`=round(`A-C`*100,2),
           `-AC`=round(`-AC`*100,2), `-A-C`=round(`-A-C`*100,2))
  tables.emp.augmented=pmap_dfr(tables.empiric.wide %>%
                                  dplyr::select(-p_id), function(...){
    row=tibble(...)
    row.long = row %>%
      pivot_longer(cols=c(AC, `A-C`, `-AC`, `-A-C`),
                   names_to="cell", values_to="p") %>%
      mutate(mod = p %% 10)
    vals.augmented = pmap_dfr(row.long, function(...){
      entry = tibble(...)
      if(entry$p %in% c(0,1,2)){
        vals = tibble(p=c(0, 1, 2))
      } else if(entry$p %in% c(98,99,100)){
        vals = tibble(p=c(98, 99, 100))
      }else if(entry$mod %in% c(3,4,5,6,7)) {
        vals = tibble(p=entry$p - entry$mod + c(3,4,5,6,7))
      } else if(entry$mod %in% c(8,9)){
        vals = tibble(p=entry$p - entry$mod + c(8,9, 10, 11, 12))
      } else if(entry$mod %in% c(0, 1, 2)){
        vals = tibble(p=entry$p - entry$mod + c(0, 1, 2, -2, -1))
      }
      vals %>% add_column(empirical_id = entry$empirical_id %>% unique(),
                          cell=entry$cell %>% unique())
    })
    # normalize new tables + make sure that distinct
    x = vals.augmented %>%
      pivot_wider(names_from="cell", values_from="p", values_fn = list)
    y=expand.grid(AC=x$AC[[1]], `A-C`=x$`A-C`[[1]], `-AC`=x$`-AC`[[1]],
                  `-A-C`=x$`-A-C`[[1]])
    tables.expanded = prop.table(as.matrix(y+epsilon), 1) %>% as_tibble() %>%
      mutate(AC=round(AC, 2), `A-C`=round(`A-C`, 2), `-AC`=round(`-AC`,2),
             `-A-C`=round(`-A-C`, 2)) %>%
      distinct() %>% add_column(empirical_id=row$empirical_id %>% unique());
    
    # due to normalization, original values might have changed more than we want
    tables = tables.expanded %>%
      filter((AC*100 <= row$AC + 2 & (AC*100 >= row$AC-2)) & 
               (`A-C`*100 <= row$`A-C` + 2 & (`A-C`*100 >= row$`A-C`-2)) &
               (`-AC`*100 <= row$`-AC` + 2 & (`-AC`*100 >= row$`-AC`-2)) & 
               (`-A-C`*100 <= row$`-A-C` + 2 & (`-A-C`*100 >= row$`-A-C`-2)))
    
    return(tables)
  })
  tables.emp.augmented = tables.emp.augmented %>%
    distinct_at(vars(c(-empirical_id)), .keep_all = TRUE) %>% 
    rowid_to_column("augmented_id")
  
  save_data(tables.emp.augmented,
            paste(result_dir, "tables-empiric-augmented.rds", sep=fs))

}

process_data <- function(data_dir, data_fn, result_dir, result_fn, debug_run,
                         N_trials, name_exp){
  dat.anonym <- save_raw_data(data_dir, data_fn, result_dir, result_fn, debug_run)
  dat.tidy <- tidy_data(dat.anonym, N_trials, name_exp);
  # Further process TEST-trial data --------------------------------------------
  data <- dat.tidy$test
  if(name_exp == "prior"){
    df <- add_normed_exp1(data, "test");
    df <- standardize_color_groups_exp1(df)
    save_prob_tables(df, result_dir, result_fn);
  } else if(name_exp == "production") {
    df <- standardize_color_groups_exp2(data)
    df <- standardize_sentences(df)
  } else if (name_exp == "joint"){
    df1 <- data %>% filter(str_detect(trial_name, "multiple_slider"))
    df1 <- add_normed_exp1(df1, "test");
    df1 <- standardize_color_groups_exp1(df1)
    save_prob_tables(df1, result_dir, result_fn);
    df2 <- data %>% filter(str_detect(trial_name, "fridge_")) %>%
      mutate(response=utterance) %>%
      dplyr::select(-utterance)
    df2 <- standardize_color_groups_exp2(df2)
    df2 <- standardize_sentences(df2)
    df <- bind_rows(df1 %>% rename(response=utterance), df2);
  }else {stop(paste('unknown experiment with name: ', name_exp))}

  # save processed data -----------------------------------------------------
  fn_tidy <- paste(result_fn, "_tidy.rds", sep="");
  path_target <- paste(result_dir, fn_tidy, sep=.Platform$file.sep)
  dat.tidy$test <- df
  save_data(dat.tidy, path_target)
  return(dat.tidy)
}

makeAndSaveModelTables = function(){
  dat.model = sampleModelTables()
  tables.model = dat.model$tables
  
  tables.generated = tables.model %>%
    mutate(AC=round(AC, 2), `A-C`=round(`A-C`,2),
           `-AC`=round(`-AC`, 2), `-A-C`=round(`-A-C`, 2)) %>%
    ungroup() %>% dplyr::select(-bn_id) %>% 
    distinct_at(vars(-starts_with("logL_")), .keep_all = TRUE) %>% 
    rowid_to_column("table_id")
  tables.emp.augmented = readRDS(
    here("data", "prolific", "results", "toy-blocks-pilot-2",
         "tables-empiric-augmented.rds")
  )
  
  tables = left_join(tables.generated, tables.emp.augmented,
                     by=c("AC", "A-C", "-AC", "-A-C")) %>% 
    mutate(empirical = !is.na(empirical_id))
  
  df.tables = tables %>% dplyr::select(-augmented_id)
  # save mapping of table-empirical ids
  tables.emp = df.tables %>% filter(empirical) %>%
    dplyr::select(-empirical, -starts_with("logL_"))
  save_data(tables.emp, paste(result_dir, "mapping-tables-ids.rds", sep=fs))
  tables.toWPPL = bind_cols(
    prop.table(
      df.tables %>%
        dplyr::select(-table_id, -empirical, -empirical_id, -cn,
                      -starts_with("logL_")) %>%
        as.matrix() + epsilon, 1
    ) %>% as_tibble(),
    df.tables %>% dplyr::select(table_id, empirical_id, empirical, cn,
                                starts_with("logL_"))
  )
  
  tables.toWPPL = tables.toWPPL %>%  group_by(table_id) %>% 
    mutate(vs=list(c("AC", "A-C", "-AC", "-A-C")),
           ps=list(c(`AC`, `A-C`, `-AC`, `-A-C`))) %>%
    add_column(stimulus="")
  
  save_data(tables.toWPPL, dat.model$params$tables_empiric)
  return(tables.toWPPL)
}

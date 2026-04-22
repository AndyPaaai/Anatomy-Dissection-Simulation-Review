# =========================================================================
# TITLE: Anatomy + Education Dissection Simulation - Data Analysis
# DESCRIPTION: Bibliometric and methodological analysis of literature
# =========================================================================

# =========================================================================
# 1. SETUP & LIBRARIES
# =========================================================================
# Install required packages if not present:
# install.packages(c("tidyverse", "scales", "patchwork", "broom"))

library(tidyverse)
library(scales)
library(patchwork)
library(broom)

# =========================================================================
# 2. CONFIGURATION & FILE PATHS
# =========================================================================
# Define file paths here for easy access and reproducibility

# Input dataset path
DATA_PATH <- "~/AI + Education Dissection Simulation/scopus_export_Apr 14-2026_27a56706-4fa6-4d7c-86cb-2b6d70e8f6c9.csv"

# Output paths
OUT_LOGISTIC_MAIN   <- "logistic_models_main.csv"
OUT_LOGISTIC_ARTIC  <- "C:/Users/andya/OneDrive/Documentos/AI + Education Dissection Simulation/Supplementary Material #3.csv"
OUT_BIBLIOMETRIC    <- "bibliometric_model_main.csv"

# =========================================================================
# 3. DICTIONARIES & LEXICAL RULES
# =========================================================================

# --- Modality Keywords
dict_dissection <- c(
  'cadaver', 'cadaveric', 'dissect', 'prosect', 'microdissect',
  'traditional anatomy', 'anatomy lab', 'anatomical dissection',
  'gross anatomy lab', 'plastinat'
)

dict_simulation <- c(
  'simulat', 'virtual reality', 'augmented reality', 'mixed reality',
  'virtual dissect', '3d model', 'anatomy table', 'haptic',
  'digital platform', 'interactive tool', 'computer-assisted',
  'digital twin', 'virtual anatomy', 'digital anatom'
)

# --- Experimental Design Keywords (Strong vs Support)
dict_exp_strong <- c(
  'randomi', 'trial', 'quasi-experiment', 'pre-test', 'post-test',
  'pretest', 'posttest', 'control group', 'before and after',
  'intervention', 'assigned', 'allocation', 'allocated'
)

dict_exp_support <- c(
  'compar', 'versus', '\\bvs\\b', 'between groups', 'two groups',
  'group a', 'group b'
)

# --- Subjective Outcomes Keywords
dict_subj <- c(
  'satisfaction', 'percept', 'accept', 'attitude', 'prefer',
  'experienc', 'confiden', 'usefulness', 'engagement', 'feedback',
  'self-report', 'likert', 'opinion'
)

# --- Objective Outcomes Keywords (Strict)
dict_obj <- c(
  'score', 'academic performance', 'retention', 'accuracy',
  'correctly identif', 'ospe', 'osce', 'task performance',
  'time to complet', 'error rate', 'spatial ability',
  'knowledge score', 'exam score', 'test score', 'grade',
  'performance'
)

# --- Clinical/Procedural Outcomes Keywords
dict_clin <- c(
  'procedur', 'surg', 'operat', 'clinical skill',
  'operative performance', 'procedural accuracy',
  'transfer to clinical practice', 'clinical application',
  'technical skill'
)

# --- Negation Rules
negations <- c(
  '\\bno\\b', '\\bnot\\b', 'without', 'lack of', 'instead of',
  'absence of', 'cannot', 'abandon'
)

# =========================================================================
# 4. CUSTOM FUNCTIONS
# =========================================================================

#' Search with context window to avoid negations
search_without_negation <- function(text, term_list) {
  if (is.na(text) || nchar(text) < 20) return(FALSE)
  
  for (term in term_list) {
    matches <- str_locate_all(text, term)[[1]]
    
    if (nrow(matches) > 0) {
      for (i in 1:nrow(matches)) {
        word_start <- matches[i, "start"]
        context_start <- max(1, word_start - 45)
        prev_context <- str_sub(text, context_start, word_start - 1)
        
        is_negated <- any(str_detect(prev_context, negations))
        
        if (!is_negated) {
          return(TRUE)
        }
      }
    }
  }
  return(FALSE)
}

#' Helper function: counts terms that appear without negation
count_without_negation <- function(text, term_list) {
  if (is.na(text) || nchar(text) < 20) return(0)
  
  counter <- 0
  
  for (term in term_list) {
    matches <- str_locate_all(text, term)[[1]]
    
    if (nrow(matches) > 0) {
      for (i in 1:nrow(matches)) {
        word_start <- matches[i, "start"]
        context_start <- max(1, word_start - 45)
        prev_context <- str_sub(text, context_start, word_start - 1)
        
        is_negated <- any(str_detect(prev_context, negations))
        
        if (!is_negated) {
          counter <- counter + 1
          break
        }
      }
    }
  }
  return(counter)
}

#' Classifies an article based on title, abstract, and keywords
classify_article <- function(title, abstract, author_keywords, index_keywords, doc_type = NA) {
  text <- paste(title, abstract, author_keywords, index_keywords, sep = " ")
  text <- tolower(text)
  
  if (is.na(text) || nchar(text) < 20) {
    return(tibble(
      Modality = "unclear",
      Design = "unclear",
      Subjective_Outcome = "no",
      Objective_Outcome = "no",
      Clinical_Outcome = "no"
    ))
  }
  
  # --- Modality
  has_diss <- search_without_negation(text, dict_dissection)
  has_sim  <- search_without_negation(text, dict_simulation)
  
  val_modality <- case_when(
    has_diss & has_sim ~ "hybrid",
    has_sim            ~ "simulation",
    has_diss           ~ "dissection",
    TRUE               ~ "unclear"
  )
  
  # --- Experimental Design
  doc_type_lower <- tolower(ifelse(is.na(doc_type), "", doc_type))
  
  if (str_detect(doc_type_lower, "review")) {
    val_design <- "non-experimental"
  } else {
    n_exp_strong  <- count_without_negation(text, dict_exp_strong)
    n_exp_support <- count_without_negation(text, dict_exp_support)
    
    if (n_exp_strong >= 1 || (n_exp_strong >= 1 && n_exp_support >= 1)) {
      val_design <- "experimental"
    } else {
      val_design <- "non-experimental"
    }
  }
  
  # --- Outcomes
  val_subj <- ifelse(search_without_negation(text, dict_subj), "yes", "no")
  val_obj  <- ifelse(search_without_negation(text, dict_obj), "yes", "no")
  val_clin <- ifelse(search_without_negation(text, dict_clin), "yes", "no")
  
  # If clinical, force objective = yes
  if (val_clin == "yes") val_obj <- "yes"
  
  return(tibble(
    Modality = val_modality,
    Design = val_design,
    Subjective_Outcome = val_subj,
    Objective_Outcome = val_obj,
    Clinical_Outcome = val_clin
  ))
}

#' Simple function to extract countries from Affiliations
extract_countries <- function(aff) {
  if (is.na(aff) || aff == "") return(character(0))
  
  parts <- str_split(aff, ";\\s*")[[1]]
  countries <- str_trim(str_extract(parts, "[^,;]+$"))
  countries <- countries[!is.na(countries) & countries != ""]
  
  return(unique(countries))
}

# =========================================================================
# 5. DATA LOADING & PROCESSING
# =========================================================================

cat("\n[1/5] Loading database...\n")
df <- read_csv(DATA_PATH, show_col_types = FALSE)

# Detect column names
title_col    <- ifelse("Title" %in% names(df), "Title", "title")
abstract_col <- ifelse("Abstract" %in% names(df), "Abstract", "abstract")
akw_col      <- ifelse("Author Keywords" %in% names(df), "Author Keywords", "Author keywords")
ikw_col      <- ifelse("Index Keywords" %in% names(df), "Index Keywords", "Index keywords")
doctype_col  <- ifelse("Document Type" %in% names(df), "Document Type", "document type")

cat("[2/5] Analyzing texts and applying adjusted lexical rules...\n")
results <- pmap_dfr(
  list(
    df[[title_col]],
    df[[abstract_col]],
    df[[akw_col]],
    df[[ikw_col]],
    df[[doctype_col]]
  ),
  classify_article
)

df_final <- bind_cols(df, results)

cat("[3/5] Calculating derived bibliometric variables...\n")
df_final <- df_final %>%
  mutate(
    Year = as.numeric(Year),
    Citations = as.numeric(`Cited by`),
    Citations = ifelse(is.na(Citations), 0, Citations),
    Citations_per_year = Citations / pmax(1, 2026 - Year),
    
    n_authors = case_when(
      !is.na(`Author full names`) & `Author full names` != "" ~ str_count(`Author full names`, ";") + 1,
      !is.na(Authors) & Authors != "" ~ str_count(Authors, ",") + 1,
      TRUE ~ NA_real_
    ),
    
    OpenAccess_bin = case_when(
      is.na(`Open Access`) | str_trim(`Open Access`) == "" ~ "no",
      TRUE ~ "yes"
    ),
    
    DocType_simple = case_when(
      str_to_lower(`Document Type`) == "article" ~ "article",
      str_to_lower(`Document Type`) == "review" ~ "review",
      TRUE ~ "other"
    ),
    
    n_countries = map_int(Affiliations, ~ length(extract_countries(.x))),
    Intl_Collab = case_when(
      n_countries > 1 ~ "yes",
      n_countries <= 1 ~ "no",
      TRUE ~ NA_character_
    )
  )

# =========================================================================
# 6. DATA SUBSETS
# =========================================================================

cat("[4/5] Creating subsets for analysis...\n")
# Total corpus for scientometrics
df_sciento <- df_final

# Basic methodological corpus
df_method <- df_final %>%
  filter(Modality != "unclear")

# =========================================================================
# 7. DESCRIPTIVE STATISTICS
# =========================================================================

cat("[5/5] Printing basic summaries...\n")

cat("\n--- Final Modality Distribution ---\n")
print(table(df_final$Modality))

cat("\n--- Final Design Distribution ---\n")
print(table(df_final$Design))

cat("\n--- Subjective Outcomes ---\n")
print(table(df_final$Subjective_Outcome))

cat("\n--- Objective Outcomes ---\n")
print(table(df_final$Objective_Outcome))

cat("\n--- Clinical Outcomes ---\n")
print(table(df_final$Clinical_Outcome))

cat("\n--- General Distribution of Analyzable Sample ---\n")
df_sciento %>% count(Modality) %>% print()
df_sciento %>% count(Design) %>% print()
df_sciento %>% count(Subjective_Outcome) %>% print()
df_sciento %>% count(Objective_Outcome) %>% print()
df_sciento %>% count(Clinical_Outcome) %>% print()

# Most productive journals
cat("\n--- Most Productive Journals (Top 10) ---\n")
top_journals <- df_sciento %>%
  count(`Source title`, sort = TRUE) %>%
  slice_head(n = 10)
print(top_journals)

# =========================================================================
# 8. GLOBAL AND MODALITY VISUALIZATIONS
# =========================================================================

# Custom plot theme
theme_manuscript <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey82", linewidth = 0.45),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.45),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(color = "black", size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 13, hjust = 0),
      plot.margin = margin(8, 10, 8, 8)
    )
}

# --- Plot 1: Total annual production
pubs_year_total <- df_sciento %>%
  count(Year) %>%
  complete(Year = full_seq(range(df_sciento$Year), 1), fill = list(n = 0))

col_total <- "#2A9D8F"

fig_total <- pubs_year_total %>%
  ggplot(aes(x = Year, y = n)) +
  geom_area(fill = col_total, alpha = 0.18) +
  geom_line(color = col_total, linewidth = 1.2) +
  scale_x_continuous(
    limits = c(min(pubs_year_total$Year), max(pubs_year_total$Year)),
    breaks = seq(min(pubs_year_total$Year), max(pubs_year_total$Year), by = 2),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.04))) +
  labs(title = "Annual publication output", x = "Year", y = "Number of publications") +
  theme_manuscript()

print(fig_total)

# --- Plot 2: Annual production by modality
pubs_year_modality <- df_method %>%
  count(Year, Modality) %>%
  complete(
    Year = full_seq(range(df_method$Year), 1),
    Modality = c("dissection", "hybrid", "simulation"),
    fill = list(n = 0)
  ) %>%
  mutate(Modality = factor(Modality, levels = c("dissection", "hybrid", "simulation")))

pal_mod <- c("dissection" = "#E76F51", "hybrid" = "#2A9D8F", "simulation" = "#4F86F7")

fig_modality <- pubs_year_modality %>%
  ggplot(aes(x = Year, y = n, group = Modality)) +
  geom_area(aes(fill = Modality), alpha = 0.14, position = "identity") +
  geom_line(aes(color = Modality), linewidth = 1.15) +
  scale_x_continuous(
    limits = c(min(pubs_year_modality$Year), max(pubs_year_modality$Year)),
    breaks = seq(min(pubs_year_modality$Year), max(pubs_year_modality$Year), by = 2),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.04))) +
  scale_color_manual(values = pal_mod, name = "Modality") +
  scale_fill_manual(values = pal_mod, name = "Modality") +
  labs(title = "Annual publication output by modality", x = "Year", y = "Number of publications") +
  guides(fill = "none", color = guide_legend(override.aes = list(linewidth = 1.5))) +
  theme_manuscript()

print(fig_modality)


# =========================================================================
# 9. TABLES & PROPORTIONS
# =========================================================================

# --- Main table: Summary by modality and total
summary_table <- df_method %>%
  group_by(Modality) %>%
  summarise(
    n = n(),
    experimental_pct = round(mean(Design == "experimental", na.rm = TRUE) * 100, 1),
    subjective_pct   = round(mean(Subjective_Outcome == "yes", na.rm = TRUE) * 100, 1),
    objective_pct    = round(mean(Objective_Outcome == "yes", na.rm = TRUE) * 100, 1),
    clinical_pct     = round(mean(Clinical_Outcome == "yes", na.rm = TRUE) * 100, 1),
    median_citations = round(median(Citations, na.rm = TRUE), 1),
    median_citations_per_year = round(median(Citations_per_year, na.rm = TRUE), 2),
    median_authors   = round(median(n_authors, na.rm = TRUE), 1),
    open_access_pct  = round(mean(OpenAccess_bin == "yes", na.rm = TRUE) * 100, 1),
    intl_collab_pct  = round(mean(Intl_Collab == "yes", na.rm = TRUE) * 100, 1)
  )

total_row <- df_method %>%
  summarise(
    Modality = "Total",
    n = n(),
    experimental_pct = round(mean(Design == "experimental", na.rm = TRUE) * 100, 1),
    subjective_pct   = round(mean(Subjective_Outcome == "yes", na.rm = TRUE) * 100, 1),
    objective_pct    = round(mean(Objective_Outcome == "yes", na.rm = TRUE) * 100, 1),
    clinical_pct     = round(mean(Clinical_Outcome == "yes", na.rm = TRUE) * 100, 1),
    median_citations = round(median(Citations, na.rm = TRUE), 1),
    median_citations_per_year = round(median(Citations_per_year, na.rm = TRUE), 2),
    median_authors   = round(median(n_authors, na.rm = TRUE), 1),
    open_access_pct  = round(mean(OpenAccess_bin == "yes", na.rm = TRUE) * 100, 1),
    intl_collab_pct  = round(mean(Intl_Collab == "yes", na.rm = TRUE) * 100, 1)
  )

final_summary_table <- bind_rows(summary_table, total_row)
cat("\n--- Summary Table by Modality ---\n")
print(final_summary_table)


# =========================================================================
# 10. INFERENTIAL STATISTICS (CHI-SQUARE)
# =========================================================================

cat("\n--- Chi-square Tests ---\n")
print(chisq.test(table(df_method$Modality, df_method$Design)))
print(chisq.test(table(df_method$Modality, df_method$Subjective_Outcome)))
print(chisq.test(table(df_method$Modality, df_method$Objective_Outcome)))
print(chisq.test(table(df_method$Modality, df_method$Clinical_Outcome)))

# Proportions
cat("\n--- Proportions by modality ---\n")
print(prop.table(table(df_method$Modality, df_method$Design), 1))
print(prop.table(table(df_method$Modality, df_method$Subjective_Outcome), 1))
print(prop.table(table(df_method$Modality, df_method$Objective_Outcome), 1))
print(prop.table(table(df_method$Modality, df_method$Clinical_Outcome), 1))


# =========================================================================
# 11. MAIN LOGISTIC REGRESSION
# =========================================================================

# Adjustments in df_method
df_method <- df_method %>%
  mutate(
    Modality = factor(Modality, levels = c("dissection", "simulation", "hybrid")),
    Design_bin = ifelse(Design == "experimental", 1, 0),
    Subjective_bin = ifelse(Subjective_Outcome == "yes", 1, 0),
    Objective_bin = ifelse(Objective_Outcome == "yes", 1, 0),
    Clinical_bin = ifelse(Clinical_Outcome == "yes", 1, 0),
    Year = as.numeric(Year),
    Year_c = Year - min(Year, na.rm = TRUE)
  )

m1 <- glm(Design_bin ~ Modality + Year_c, data = df_method, family = binomial())
m2 <- glm(Objective_bin ~ Modality + Design_bin + Year_c, data = df_method, family = binomial())
m3 <- glm(Clinical_bin ~ Modality + Design_bin + Year_c, data = df_method, family = binomial())

cat("\n--- Logistic Regression Models (Methodological Corpus) ---\n")
table_m1 <- tidy(m1, exponentiate = TRUE, conf.int = TRUE) %>% mutate(model = "Experimental design")
table_m2 <- tidy(m2, exponentiate = TRUE, conf.int = TRUE) %>% mutate(model = "Objective outcome")
table_m3 <- tidy(m3, exponentiate = TRUE, conf.int = TRUE) %>% mutate(model = "Clinical/procedural outcome")

table_logistic_total <- bind_rows(table_m1, table_m2, table_m3) %>%
  select(model, term, estimate, conf.low, conf.high, p.value)

print(table_logistic_total)
write_csv(table_logistic_total, OUT_LOGISTIC_MAIN)


# =========================================================================
# 12. SENSITIVITY ANALYSIS (ORIGINAL ARTICLES ONLY)
# =========================================================================

df_method_articles <- df_method %>% filter(DocType_simple == "article")

m1_art <- glm(Design_bin ~ Modality + Year_c, data = df_method_articles, family = binomial())
m2_art <- glm(Objective_bin ~ Modality + Design_bin + Year_c, data = df_method_articles, family = binomial())
m3_art <- glm(Clinical_bin ~ Modality + Design_bin + Year_c, data = df_method_articles, family = binomial())

cat("\n--- Logistic Regression Models (Original Articles Only) ---\n")
table_m1_art <- tidy(m1_art, exponentiate = TRUE, conf.int = TRUE) %>% mutate(model = "Experimental design (articles only)")
table_m2_art <- tidy(m2_art, exponentiate = TRUE, conf.int = TRUE) %>% mutate(model = "Objective outcome (articles only)")
table_m3_art <- tidy(m3_art, exponentiate = TRUE, conf.int = TRUE) %>% mutate(model = "Clinical/procedural outcome (articles only)")

table_logistic_articles <- bind_rows(table_m1_art, table_m2_art, table_m3_art) %>%
  select(model, term, estimate, conf.low, conf.high, p.value)

print(table_logistic_articles)

# Export table
write.csv(table_logistic_articles, file = OUT_LOGISTIC_ARTIC, row.names = FALSE, fileEncoding = "UTF-8")


# =========================================================================
# 13. BIBLIOMETRIC MODEL
# =========================================================================

df_biblio <- df_sciento %>%
  filter(Modality != "unclear") %>%
  mutate(
    Modality = factor(Modality, levels = c("dissection", "simulation", "hybrid")),
    DocType_simple = factor(DocType_simple, levels = c("article", "review")),
    OpenAccess_bin = factor(OpenAccess_bin, levels = c("no", "yes")),
    Intl_Collab = factor(Intl_Collab, levels = c("no", "yes")),
    Year = as.numeric(Year),
    Year_c = Year - min(Year, na.rm = TRUE),
    log_citations_per_year = log1p(Citations_per_year)
  )

m_biblio <- lm(
  log_citations_per_year ~ Modality + DocType_simple + OpenAccess_bin +
    Intl_Collab + n_authors + Year_c,
  data = df_biblio
)

cat("\n--- Bibliometric Model (Visibility) ---\n")
table_biblio <- tidy(m_biblio, conf.int = TRUE) %>%
  mutate(model = "Bibliometric visibility") %>%
  select(model, term, estimate, conf.low, conf.high, p.value)

print(table_biblio)
write_csv(table_biblio, OUT_BIBLIOMETRIC)

cat("\n[✓] Analysis completed successfully.\n")
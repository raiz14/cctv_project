# Code for calculations and regressions (part 3 of all)
# By: Dmitriy Serebrennikov

# Install the required packages
pkgs <- c("data.table", "Matrix", "car", "dplyr", "lmtest", "sandwich", "stargazer", "glmnet", "foreign", "nnet", "reshape2", "AER", "formattable")

if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  
  install.packages(setdiff(pkgs, rownames(installed.packages())))
  
}

# Load packages
lapply(pkgs, library, character.only = T)

setwd("D:/serebrennikov/serebrennikov_cctv_project/outputs/street_bin_design_by_CCTV/Cutoff = 50")

#####################

# df <- objects_and_cameras_binary
df <- as.data.table(objects_and_cameras_binary)

# Rename "trash category"
df <- df %>% rename("no_category" = "yes")
df <- df %>% rename(Northern_AO = `Northern _AO`)

# Remove variables where are less than 40 unque OSM objects

working_df <- data.table::fread("tags_all_OSM_objects_count_WIDE.csv", encoding = "UTF-8", stringsAsFactors = T)
working_df <- subset(working_df, select = -c(local_id, Var.2, athletics, basketball, fitness, hockey, horse_racing, ice_hockey, multi, running, skateboard, soccer, surface, swimming, table_tennis, tennis, volleyball, equestrian, gymnastics, yoga))


col_interest <- colSums(working_df) >= 80 
col_interest <- names(col_interest[col_interest == T])
col_interest <- c(col_interest,"Eastern_AO", "Western_AO", "Zelenograd_AO", "Novomoskovsky_AO", "Northern_AO", "NorthEastern_AO", "NorthWestern_AO", "Troitsk_AO", "Central_AO", "SouthEastern_AO", "SouthWestern_AO", "Southern_AO", "public_inside", "public_outside")

df_subset <- as.data.table(df)[,..col_interest]

#df_subset <- select(df_subset, -"no_category")

# Creating a "Y" (factor for the cctv type) and a "y" (dummy for the presence of a cctv)

Y <- as.character(df$cctv_type)
Y[is.na(Y)] <- 0
Y[Y == "cctv_street"] <- 1
Y <- as.factor(Y)
table(Y)

y <- df$cctv_ID
y <- ifelse(is.na(y) == T, 0, 1)
table(y)

# Creating x (as a matrix of an independents variables)
x <- as(as.matrix(as.data.table(df_subset)), "sparseMatrix")

#####################

# Tests for the best LASSO-model type

# Make a function for assess models by "accuracy", "precision", "recall" and "F1" metrics

all_reg_metrics <- function(predict, actual_labels){
  predict <- ifelse(predict > 0.5, 1, 0)
  precision <- sum(predict & actual_labels) / sum(predict)
  recall <- sum(predict & actual_labels) / sum(actual_labels)
  f1 <- 2 * precision * recall / (precision + recall)
  predict <- as.factor(predict)
  cm <- as.matrix(table(Actual = actual_labels, Predicted = predict))
  accuracy = sum(diag(cm)) / sum(cm)
  results <- c(accuracy, precision, recall, f1)
  return(results)
}

# Assess models

# 1. Linear regression
test_df <- cbind(y,df_subset)
lm_reg <- lm(y~.,test_df)
covHC_reg <- coeftest(lm_reg, vcov = vcovHC(lm_reg))
covHC_model <- broom::tidy(coeftest(lm_reg, vcov = vcovHC(lm_reg)))
pred_lm <- lm_reg$fitted.values
means_lm <- all_reg_metrics(pred_lm, y)

# 2. Logistic regression
glm_reg <- glm(y~.,test_df, family = "binomial")
pred_glm <- glm_reg$fitted.values
means_glm <- all_reg_metrics(pred_glm, y)

# 3. Gaussian cross-validation in "glmnet" packege
glmnet_gaussian <- cv.glmnet(x, y, alpha = 1, family = "gaussian")
pred_glmnet_gauss <- predict(glmnet_gaussian, newx = x, s = "lambda.1se")
means_glmnet_gauss <- all_reg_metrics(pred_glmnet_gauss, y)

# 4. Binomial cross-validation in "glmnet" packege with "auc" assessor 
glmnet_binom_auc <- cv.glmnet(x, y, alpha = 1, family = "binomial", type.measure = "auc")
pred_glmnet_binom_auc <- predict(glmnet_binom_auc, newx = x, s = "lambda.1se")
means_glmnet_binom_auc <- all_reg_metrics(pred_glmnet_binom_auc, y)

# 5. Cross-validation in "sparsenet" packege with "mse" assessor
X <- as.matrix(x)
sparsenet_mse_reg <- sparsenet::cv.sparsenet(X, y, type.measure = "mse") 
pred_sparsenet_mse <- predict(sparsenet_mse_reg, newx = x)
means_sparsenet_mse <- all_reg_metrics(pred_sparsenet_mse, y)

# 6. Binomial cross-validation in "gamlr" packege with "deviance" type measure
gamlr_dev_reg <- gamlr::cv.gamlr(x, y, family = "binomial",type.measure = "deviance") 
pred_gamlr_dev_reg <- predict(gamlr_dev_reg, x, family = "binomial",type.measure = "deviance")
means_gamlr_dev_reg <- all_reg_metrics(pred_gamlr_dev_reg, y)

# Create summary-table

assessors_cols_names <- c("accuracy", "precision", "recall", "F1")
assessors_rows_names <- c("type","lm()", "glm(binomial)", "cv.glmnet(gaussian)", "cv.glmnet(binomial, auc)", "cv.sparsenet(mse)", "cv.gamlr(binomial, deviance)")
# name_rows <- c("Calculate on caret:", "lm()", "glm(binomial)", "cv.glmnet(gaussian)", "cv.glmnet(binomial, auc)", "cv.sparsenet(mse)", "cv.gamlr(binomial, deviance)")

#

measure_tab <-  cbind(assessors_rows_names, rbind(assessors_cols_names, means_lm, means_glm, means_glmnet_gauss, means_glmnet_binom_auc, means_sparsenet_mse, means_gamlr_dev_reg))
measure_tab[2:7,2:5] = round(apply(measure_tab[2:7,2:5], 2, function(x) as.numeric(x)), 3)

# All models give good and almost equal results.

# Save
data.table::fwrite(measure_tab, file = "measure_tab.csv", row.names = F, col.names = F, na = NA)


#####################
#####################
#####################

# To reduce the number of categories make LASSO-regularisation.

cv_means <- cv.glmnet(x, Y, alpha = 1, family = "binomial", type.measure = "auc")

# Give non-zero coefficients
B <- coef(cv_means, s = "lambda.1se")
lasso_coef <- as.data.frame(as.matrix(B))
lasso_coef <- lasso_coef %>% 
  cbind(rn = row.names(lasso_coef), .) %>% 
  subset(`1`!=0) %>% 
  select(rn)
lasso_coef <- as.character(row.names(lasso_coef))[ lasso_coef != "(Intercept)"]
# View(lasso_coef)
# Lambda cross-validation plot
plot(cv_means)

lasso_coef <- c(lasso_coef, "public_outside")

# Lambda's value that gives minimum mean cross-validated error.
# lambda <- cv_means$lambda.min
# 
# OR
# 
# Lambda which gives the most regularized model such that error is within one standard error of the minimum
lambda <- cv_means$lambda.1se

# Save coefficients
save(lasso_coef, file = paste0("lasso_coef_street_bin_lambda_", lambda, ".rdata"), compress = "xz")
# load("lasso_coef_street_bin_lambda_0.000222999232410699.rdata")

#####################


# Subset only non-zero coefficients in cross-validation
df_post_lasso_subset <- as.data.table(df)[,..lasso_coef]
new_x <- cbind(Y, df_post_lasso_subset)
new_x <- select(new_x, -c("surveillance"))

# Regression
multi_test <- glm(Y~.,new_x, family=binomial(link="cloglog"))

# PseudoR2
DescTools::PseudoR2(multi_test)
#  McFadden - 0.2833995
#
# AUC & ROC for multiple answer: one vs. all 
library(caret)
all_reg_metrics_caret <- function(predict, actual_labels){
  predict <- ifelse(predict > 0.5, 1, 0)
  cm <- as.matrix(table(Actual = actual_labels, Predicted = predict))
  accuracy = sum(diag(cm)) / sum(cm)
  predict <- as.factor(predict)
  precision <- posPredValue(predict, actual_labels, positive="1")
  recall <- sensitivity(predict, actual_labels, positive="1")
  f1 <- 2 * precision * recall / (precision + recall)
  predict <- as.factor(predict)
  results <- c(accuracy, precision, recall, f1)
  return(results)
}

post_l_metrics_glm <- round(t(as.data.frame(all_reg_metrics_caret(multi_test$fitted.values, Y))),3)
post_l_metrics_glm <- `colnames<-`(post_l_metrics_glm, c("accuracy", "precision", "recall", "F1"))
post_l_metrics_glm <- `rownames<-`(post_l_metrics_glm, c("Post LASSO glm"))

# Save
sjPlot::tab_df(post_l_metrics_glm, alternate.rows = T, file = "metric_table_street_bin_post_lasso_100_PseudoR2_0.152.html")

#####################
#####################
#####################
# MAKE COEFFICIENTS TABLE

# Firstly, find the meanings of Odds ration, then find other parameters via "AER" packege


coef_table_OR <- data.frame(exp(coef(multi_test))) %>% 
  tibble::rownames_to_column(., "tag_type") 
coef_table_OR <- cbind(coef_table_OR %>%  select("tag_type"),round(coef_table_OR[,2],3))

# Coefficient significance test (from AER)

coeftest_multi <- round(coeftest(multi_test), 3)

# Function to convert coeftest results object to data frame
ctdf=function(x){
  rt=list()                             # generate empty results list
  for(c in 1:dim(x)[2]) rt[[c]]=x[,c]   # writes column values of x to list
  rt=as.data.frame(rt)                  # converts list to data frame object
  names(rt)=names(x[1,])                # assign correct column names
  rt[,"sig"]=symnum(rt$`Pr(>|z|)`, corr = FALSE, na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
  return(rt)
}

# Apply function
coeftest_multi_converted <- ctdf(coeftest_multi)
setDT(coeftest_multi_converted, keep.rownames = TRUE)[]
coeftest_multi_converted[,2:5] <- round(coeftest_multi_converted[,2:5], 3)

coef_street <- merge(coeftest_multi_converted, coef_table_OR[,1:2], by.x = "rn", by.y = "tag_type", all.x = T) %>% 
  rename("Odds ratio" = "round(coef_table_OR[, 2], 3)", "Category" = "rn") %>% 
  select("Category", "Estimate", "Odds ratio", everything())

data.table::fwrite(coef_street, file = "coef_street_bin_cutoff_100.csv", row.names = F, na = NA)

######################
######################
######################
# TABLE WITH THE MOST FREQUENT MEANINGS
col_interest <- colSums(working_df) >= 80 
col_interest <- names(col_interest[col_interest == T])
post_lasso_df <- as.data.table(df)[,..col_interest]

post_lasso_colsum <- colSums(post_lasso_df)
post_lasso_colsum <- as.data.frame(t(rbind(names(post_lasso_df), post_lasso_colsum))) %>% 
  rename("Category" = "V1", "Value" = "post_lasso_colsum")
post_lasso_colsum$Value <- as.numeric(as.character(post_lasso_colsum$Value))
post_lasso_colsum <- post_lasso_colsum %>% arrange(desc(Value))

# Save
sjPlot::tab_df(post_lasso_colsum, alternate.rows = T, file = "bin_design_by_CCTV_post_lasso_cutoff_50_colsum.html")
data.table::fwrite(post_lasso_colsum, file = "bin_design_by_CCTV_post_lasso_cutoff_50_colsum.csv", row.names = F, col.names = F, na = NA)


######################
######################
######################
# MAKE COEFFICIENTS TABLES

# Function for correct visualisation of a selected parameter
pm_color_bar <- function(color1 = "lightgreen", color2 = "pink", ...){
  formatter("span",
            style = function(x) style(
              display = "inline-block",
              float = ifelse(x >= 0, "right", "left"),
              "text-align" = ifelse(x >= 0, "right", "left"),
              "margin-left" = ifelse(x >= 0, "0%", "50%"),
              "margin-right" = ifelse(x >= 0,"50%", "0%"),
              "border-radius" = "4px",
              "background-color" = ifelse(x >= 0, color1, color2),
              width = percent(0.5*proportion(abs(as.numeric(x)), ...))
            ))
}

# Make tables
setDT(coef_street)

formattable(coef_street[order(-Estimate)],
            align = c("l", rep("r", NCOL(coef_street) - 2)),
            list(`Estimate` = pm_color_bar("#71CA97", "#FA614B")))

# coef_street <- coef_street %>% filter(Estimate > -10)


#####################
#####################
#####################
# TABLE WITH THE PREDICTION VALUES

pred_post_lasso <- round(predict(multi_test, newdata = df_post_lasso_subset, type = "response"), 3) 
pred_post_lasso <- setDT(as.data.frame(pred_post_lasso)) %>% 
  rename("street_prob" = "pred_post_lasso")

y <- as.data.frame(y)
prob_table <-
  cbind(
    cctv_ID = df$cctv_ID,
    local_id = df$local_id,
    cctv_exist = y$y,
    cctv_type = df$cctv_type,
    pred_post_lasso
    ) %>% 
  filter(!is.na(local_id))

# Split by local_id for visualise it
# Separate rows (by "|") for correct visualisation
# I decided to use the command from tidyr("separate_rows"), designed to separate the strings. I didn't find any problems in it's work
prob_table <- tidyr::separate_rows(prob_table, local_id)

# Aggregate to unique "local_id"
setDT(prob_table)
unique_prob_table_cols <- c("local_id", "cctv_ID", "cctv_type")
max_prob_table_cols <- c("local_id", "cctv_exist", "street_prob")

# Aggregate
prob_unique_id_merge <- as.data.table(prob_table)[,..unique_prob_table_cols] %>% 
  group_by(local_id) %>%
  summarise_all(funs(paste(na.omit(.), collapse = "|"))) 
prob_max_id_merge <- aggregate(.~local_id, data=prob_table[,..max_prob_table_cols], FUN=max)
prob_table <- merge(prob_unique_id_merge, prob_max_id_merge, by = "local_id")

# Empty strings to NA
for(j in names(prob_table) ) {
  
  set(prob_table, i = grep("^$|^ $|^NA$", prob_table[[j]]), j = j, value = NA_character_)
  
}

# Make column with difference between the actual value and the predicted value for different camera types

prob_table <-
  cbind(
    prob_table,
    diff_street = prob_table$cctv_exist - prob_table$street_prob
  )

####################
####################
####################
# MAKE DIFFERENT METRICS PLOTS

# define the theme for the next plot
result <- prob_table %>% select(street_prob, cctv_exist) %>% rename("predict" = "street_prob", "actual" = "cctv_exist") %>% setDT()
result$predict <- as.numeric(result$predict)
result$actual <- as.numeric(result$actual)
result$actual <- ifelse(result$actual == 1, 1, 0)
table(result$actual)

#####
cutoff <- 0.25
# caculating each pred falls into which category for the confusion matrix
result[ , type := ifelse( predict >= cutoff & actual == 1, "TP (207)",
                          ifelse( predict >= cutoff & actual == 0, "FP (365)", 
                                  ifelse( predict <  cutoff & actual == 1, "FN (3082)", "TN (288450)" ) ) ) %>% as.factor() ]

# Table the results  
table(result$type)

# Plot
library(ROCR)
library(grid)
library(caret)
library(dplyr)
library(scales)
library(ggplot2)
library(gridExtra)
library(data.table)

p1 <- ggplot( result, aes( actual, predict, color = type ) ) + 
  geom_violin( fill = "white", color = NA ) +
  geom_jitter( shape = 1 ) + 
  geom_hline( yintercept = cutoff, color = "blue", alpha = 0.6 ) + 
  scale_y_continuous( limits = c( 0, 1 ) ) + 
  scale_color_discrete( breaks = c( "TP (207)", "FN (3082)", "FP (365)", "TN (288450)" ) ) + # ordering of the legend 
  guides( col = guide_legend( nrow = 2 ) ) + # adjust the legend to have two rows  
  ggtitle( sprintf( "Confusion Matrix with Cutoff at %.2f", cutoff ) )

ggsave(plot = p1, "Confusion_matrix_cutoff_0.25.png", width = 9.3, height = 5.9, scale = 1, dpi = 600, units = "in")


# user-defined different cost for false negative and false positive
cost_fp <- 100
cost_fn <- 200
ROCInfo <- function( data, predict, actual, cost.fp, cost.fn )
{
  # calculate the values using the ROCR library
  # true positive, false postive 
  pred <- prediction( data[[predict]], data[[actual]] )
  perf <- performance( pred, "tpr", "fpr" )
  roc_dt <- data.frame( fpr = perf@x.values[[1]], tpr = perf@y.values[[1]] )
  
  # cost with the specified false positive and false negative cost 
  # false postive rate * number of negative instances * false positive cost + 
  # false negative rate * number of positive instances * false negative cost
  cost <- perf@x.values[[1]] * cost.fp * sum( data[[actual]] == 0 ) + 
    ( 1 - perf@y.values[[1]] ) * cost.fn * sum( data[[actual]] == 1 )
  
  cost_dt <- data.frame( cutoff = pred@cutoffs[[1]], cost = cost )
  
  # optimal cutoff value, and the corresponding true positive and false positive rate
  best_index  <- which.min(cost)
  best_cost   <- cost_dt[ best_index, "cost" ]
  best_tpr    <- roc_dt[ best_index, "tpr" ]
  best_fpr    <- roc_dt[ best_index, "fpr" ]
  best_cutoff <- pred@cutoffs[[1]][ best_index ]
  
  # area under the curve
  auc <- performance( pred, "auc" )@y.values[[1]]
  
  # normalize the cost to assign colors to 1
  normalize <- function(v) ( v - min(v) ) / diff( range(v) )
  
  # create color from a palette to assign to the 100 generated threshold between 0 ~ 1
  # then normalize each cost and assign colors to it, the higher the blacker
  # don't times it by 100, there will be 0 in the vector
  col_ramp <- colorRampPalette( c( "green", "orange", "red", "black" ) )(100)   
  col_by_cost <- col_ramp[ ceiling( normalize(cost) * 99 ) + 1 ]
  
  roc_plot <- ggplot( roc_dt, aes( fpr, tpr ) ) + 
    geom_line( color = rgb( 0, 0, 1, alpha = 0.3 ) ) +
    geom_point( color = col_by_cost, size = 4, alpha = 0.2 ) + 
    geom_segment( aes( x = 0, y = 0, xend = 1, yend = 1 ), alpha = 0.8, color = "royalblue" ) + 
    labs( title = "ROC", x = "False Postive Rate", y = "True Positive Rate" ) +
    geom_hline( yintercept = best_tpr, alpha = 0.8, linetype = "dashed", color = "steelblue4" ) +
    geom_vline( xintercept = best_fpr, alpha = 0.8, linetype = "dashed", color = "steelblue4" )				
  
  cost_plot <- ggplot( cost_dt, aes( cutoff, cost ) ) +
    geom_line( color = "blue", alpha = 0.5 ) +
    geom_point( color = col_by_cost, size = 4, alpha = 0.5 ) +
    ggtitle( "Cost" ) +
    scale_y_continuous( labels = comma ) +
    geom_vline( xintercept = best_cutoff, alpha = 0.8, linetype = "dashed", color = "steelblue4" )	
  
  # the main title for the two arranged plot
  sub_title <- sprintf( "Cutoff at %.2f - Total Cost = %d, AUC = %.3f", 
                        best_cutoff, best_cost, auc )
  
  # arranged into a side by side plot
  plot <- arrangeGrob( roc_plot, cost_plot, ncol = 2, 
                       top = textGrob( sub_title, gp = gpar( fontsize = 16, fontface = "bold" ) ) )
  
  return( list( plot 		  = plot, 
                cutoff 	  = best_cutoff, 
                totalcost   = best_cost, 
                auc         = auc,
                sensitivity = best_tpr, 
                specificity = 1 - best_fpr ) )
}





roc_info <- ROCInfo( data = result, predict = "predict", 
                     actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn )
grid.draw(roc_info$plot)

ggsave(plot = grid.draw(roc_info$plot), "ROC_AUC_Cost_curves.png", width = 9.3, height = 5.9, scale = 1, dpi = 600, units = "in")

###  
PRROC_obj <- PRROC::roc.curve(scores.class0 = result$predict, weights.class0=result$actual,
                              curve=TRUE)
plot(PRROC_obj)


###
library(precrec)
precrec_obj <- evalmod(scores = result$predict, labels = result$actual)
autoplot(precrec_obj)


#####################
#####################
#####################

# ADD A DESCRIPTION COLUMN WITH ALL TAGS 

all_cctv_and_OSM_objects <- all_cctv_and_OSM_objects %>% 
  filter(!is.na(local_id))

paste_noNA <- function(x, sep = "|") {
  gsub(", " , sep, toString(x[!is.na(x) & x != "" & x != "NA"]))
}
sep="|"
tag_types <- c("MAN_MADE", "LEISURE", "AMENITY", "OFFICE", "SHOP", "TOURISM", "RAILWAY", "HIGHWAY", "BUILDING_POINT", "BUILDING", "PARKING", "PUBLIC", "AO")

all_cctv_and_OSM_objects$description <- apply(as.data.table(all_cctv_and_OSM_objects)[ ,..tag_types] , 1 , paste_noNA , sep=sep)

unique_objects_tags <- all_cctv_and_OSM_objects %>% 
  select(local_id, description) %>% 
  tidyr::separate_rows(., local_id, sep = '\\|') %>% 
  tidyr::separate_rows(., description, sep = '\\|') %>% 
  group_by(local_id) %>% 
  unique(.) %>% 
  summarise_all(funs(paste(na.omit(.), collapse = "|")))



#################
#################
#################
# ADD OTHER COLUMNS TO prob_table DATA

prob_table_full <- merge(prob_table, unique_objects_tags, by = "local_id", all.x =T) %>% 
  merge(., all_cctv_and_OSM_objects[, c("local_id", "NAME","NAME_RU", "NAME_EN")], by = "local_id", all.x =T) %>% 
  select(-cctv_type)


# Transliterate Cyrillic columns into Latin ones
columns_for_transliteration <- c("NAME","NAME_RU", "description")
prob_table_full <- as.data.table(prob_table_full)[, (columns_for_transliteration) := lapply(.SD, function(x) {stringi::stri_trans_general(x, "russian-latin/bgn")}), .SDcols = columns_for_transliteration]


# "Yes" to "No category"

prob_table_full$description <- gsub("yes", "no_category", prob_table_full$description)

# "mo_category" to zero
no_cat_tags <- paste0("no_category|",c("Eastern_AO", "Western_AO", "Zelenograd_AO", "Novomoskovsky_AO", "Northern_AO", "NorthEastern_AO", "NorthWestern_AO", "Troitsk_AO", "Central_AO", "SouthEastern_AO", "SouthWestern_AO", "Southern_AO"))
setDT(prob_table_full)

for(j in no_cat_tags) {
  prob_table_full$diff_street  <- ifelse(prob_table_full$description == j, 0, prob_table_full$diff_street)
  }

# Add overrepresent objects
prob_table_full$cctv_overrep <- ifelse(prob_table_full$diff_street > 0.99, 1, 0)


# Save table
data.table::fwrite(prob_table_full, file = "prob_table_street_bin_cutoff_50.csv", row.names = F, na = NA)
# prob_table_full <- data.table::fread("prob_table_street_bin_design_by_CCTV.csv", encoding = "UTF-8")
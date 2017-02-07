############################################
# Segment lungs from the rest of the scan
############################################
rm(list = ls())
library(dplyr)
library(neurobase)
library(caret)

rootdir = "/dcl01/smart/data/kaggle_lung"
rda_file = file.path(rootdir, 
    "filenames.rda")
datadir = file.path(rootdir,
    "nifti")
outdir = file.path(rootdir, "lung")
houtdir = file.path(rootdir, "human")
meas_dir = file.path(rootdir, "results",
    "measures")

load(rda_file)
files = df$nii

df$outfile = file.path(outdir, 
    paste0(nii.stub(df$nii, bn = TRUE), 
        "_lung.nii.gz")
    )

df$human = file.path(houtdir, 
    paste0(nii.stub(df$nii, bn = TRUE), 
        "_human_mask.nii.gz")
    )

df$measure_rda = file.path(meas_dir,
    paste0(df$id, "_measures.rda"))

# df = df[ file.exists(df$outfile), ]

iid = 1
iid = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iid)) {
    iid = 1
}
N = nrow(df)
all_measures = vector(mode = "list",
    length = N)
# for (iid in seq(N)) {
for (iid in seq(N)) {
    print(iid)
    id = df$id[iid]
    ## Making positive values
    adder = 1025
    hu_max = 3071
    fname = files[iid]
    outrda = df$measure_rda[iid]    
    outfile = df$outfile[iid]
    human_fname = df$human[iid]
    breaks = seq(0, hu_max + adder, 
        length.out = 20)

    if (file.exists(outrda)) {
        load(file = outrda)
        all_measures[[iid]] = measures
    }
}

meas = do.call("rbind", all_measures)
val_cols = colnames(meas)
val_cols = setdiff(val_cols, "id")
sdf = df %>% 
    select(id, cancer, group) %>% 
    rename(y = cancer)
sdf$y = factor(sdf$y)
meas = left_join(sdf, meas, by = "id")

train = meas %>% 
    filter(group == "Train") %>% 
    select(-group)
keep = rowSums(!is.na(train[, val_cols])) > 0
train = filter(train, keep)

testing = meas %>% 
    filter(group == "Test") %>% 
    select(-group)

fitControl <- trainControl(
    ## 10-fold CV
   method = "repeatedcv",
   number = 10,
   ## repeated ten times
   repeats = 3,
   summaryFunction = twoClassSummary)

rf <- train(y ~ . - id, data = train, 
                 method = "rf", 
                 trControl = fitControl,
           ## This last option is actually one
           ## for gbm() that passes through
                 verbose = TRUE,
                 do.trace = TRUE)
p = predict(rf, 
    newdata = testing, 
    type = "prob")
p = p[,"1"]
pred = prediction(p, testing$y)
perf = performance(pred, "tpr", "fpr")

gbmFit1 <- train(y ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
            ## This last option is actually one
            ## for gbm() that passes through
                 verbose = FALSE)
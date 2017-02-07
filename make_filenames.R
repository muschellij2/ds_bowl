############################################
# Create Data of Filenames
############################################
rm(list = ls())
library(readr)
library(dplyr)
library(caret)
set.seed(20170207)
# source("lung_functions.R")
rootdir = "/dcl01/smart/data/kaggle_lung"
datadir = file.path(rootdir,
    "stage1")
# datadir = "."
# outdir = "."
outdir = file.path(rootdir, "nifti")
# doutdir = file.path(rootdir, "dnifti")
ids = list.dirs(path = datadir, 
    full.names = FALSE,
    recursive = FALSE)

csv = file.path(rootdir, 
    "stage1_labels.csv")
labels = read_csv(csv)


df = data.frame(id = ids,
    path = file.path(datadir, ids),
    stringsAsFactors = FALSE)
df$nii = file.path(outdir, 
    paste0(df$id, ".nii.gz"))


# df$dnii = file.path(doutdir, 
#     paste0(df$id, ".nii.gz"))

df$in_csv = df$id %in% labels$id
df = merge(df, labels, by = "id",
    all = TRUE)
df = arrange(df, id)


holdout = df[ is.na(df$cancer), ]
holdout$group = "Holdout"
df = df[ !is.na(df$cancer), ]

sdf = split(df, df$cancer)

sdf = lapply(sdf, function(x) {
    n = nrow(x)
    prob = c(0.5, 0.25)
    all_n = floor(n * prob)
    all_n = c(all_n, n - sum(all_n))
    groups = c("Train", "Validate", "Test")
    labs = unlist(mapply(function(x, n){
        rep(x, n)
    }, groups, all_n))
    names(labs) = NULL
    labs = sample(labs)
    x$group = labs
    x
})
df = do.call("rbind", sdf)
df = merge(df, holdout, all = TRUE)

outfile = file.path(rootdir, 
    "filenames.rda")
save(df, file = outfile)


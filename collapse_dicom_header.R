############################################
# Convert DICOMs to nifti
############################################
rm(list = ls())
library(dcmtk)
library(tidyr)
library(divest)
library(dplyr)
# source("lung_functions.R")
rootdir = "/dcl01/smart/data/kaggle_lung"
rda_file = file.path(rootdir, 
    "filenames.rda")
datadir = file.path(rootdir,
    "stage1")
outdir = file.path(rootdir, "results", 
    "hdr")
# datadir = "."
# outdir = "."
load(rda_file)

iid = 1
iid = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iid)) {
    iid = 1
}
all_hdrs = vector(mode = "list", 
    length = nrow(df))
names(all_hdrs) = df$id
all_w = all_hdrs
for (iid in seq(nrow(df))) {
    print(iid)
    path = df$path[iid]
    outfile = file.path(outdir, 
        paste0(basename(path), ".rda"))

    x = load(file = outfile)
    all_hdrs[[iid]] = hdr
    all_w[[iid]] = w

    rm(list = x)
}

make_num = function(x) {
    x = gsub("[", "", x, fixed = TRUE)
    x = gsub("]", "", x, fixed = TRUE)
    x = as.numeric(x)
    unique(x)
}
ints = sapply(all_w, function(x) {
    make_num(x$RescaleIntercept)
})

# }
############################################
# Convert DICOMs to nifti
############################################
rm(list = ls())
library(divest)
library(RNifti)
library(readr)
# source("lung_functions.R")
rootdir = "/dcl01/smart/data/kaggle_lung"
rda_file = file.path(rootdir, 
    "filenames.rda")
datadir = file.path(rootdir,
    "stage1")
# datadir = "."
# outdir = "."
outdir = file.path(rootdir, "nifti")
load(rda_file)

iid = 1
iid = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iid)) {
    iid = 1013
}
# for (iid in 6:20) {
    print(iid)
    outfile = df$nii[iid]
    if (!file.exists(outfile)) {
        res = readDicom(path = df$path[iid], 
            interactive = FALSE)
        if (length(res) != 1) {
            stop(paste0("Conversion is off - ", 
                "not one scan per person"))
        }
        res = res[[1]]
        dimg = dim(res)
        ndim = length(dimg)
        if (ndim == 4){
            res = nii2oro(res)
            res = copyNIfTIHeader(res,
                res[,,,1])
            writenii(res,
                filename = outfile)
        } else {
            # need to put 
            # res[[1]]@scl_slope = 1, inter 0
            writeNifti(res, 
                file = outfile)
        }
    }

# }
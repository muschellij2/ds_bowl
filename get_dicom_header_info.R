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
# for (iid in seq(nrow(df))) {
    print(iid)
    path = df$path[iid]
    outfile = file.path(outdir, 
        paste0(basename(path), ".rda"))

    attributes(sd)$paths[[1]][1]    
    file = file.path(path, "*.dcm")
    hdr = read_dicom_header(file)    
    hd = hdr[, c("file", "name", "value")]
    cn = c("RescaleIntercept", "RescaleSlope",
        "SeriesDescription",
        "WindowCenter", "WindowWidth")  
    hd = filter(hd, name %in% cn)
    wide = spread(hd, key = name, value = value)

    w = unique(wide[, cn])
    w = lapply(w, unique)

    save(w, hdr, 
        file = outfile)
    # all_hdrs[[iid]] = w
    # sd = scanDicom(path) 
    # fname = attributes(sd)$paths[[1]][1]
    # hdr = read_dicom_header(fname)    
    # hd = hdr[, c("file", "name", "value")]
    # wide = spread(hd, key = name, value = value)
    # cn = c("RescaleIntercept", "RescaleSlope",
    #     "SeriesDescription",
    #     "WindowCenter", "WindowWidth")
    # w = unique(wide[, cn])

# }
############################################
# Convert DICOMs to nifti
############################################
rm(list = ls())
library(dcm2niir)
library(neurobase)
library(fslr)
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
    iid = 1
}
# df = df[ !file.exists(df$nii),]

for (iid in seq(nrow(df))) {
    print(iid)
    outfile = df$nii[iid]
    if (!file.exists(outfile)) {
        d = dcm2nii(df$path[iid])
        res = check_dcm2nii(d)        
        if (length(res) != 1) {
            stop(paste0("Conversion is off - ", 
                "not one scan per person"))
        }
        img = readnii(res, drop_dim = FALSE)
        dimg = dim(img)
        ndim = length(dimg)        
        if (ndim == 4){
            img = copyNIfTIHeader(img,
                img[,,,1])
        }
        img = rescale_img(img) 
        img = zero_trans(img)

        print(scl_slope(img)) 
        print(scl_inter(img))

        stopifnot( scl_inter(img) == 0 )

        reor = rpi_orient(img)
        img = reor$img

        writenii(img,
                filename = outfile)
    }

}
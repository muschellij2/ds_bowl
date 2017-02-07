############################################
# Segment lungs from the rest of the scan
############################################
rm(list = ls())
library(fslr)
library(extrantsr)
library(ANTsR)
source("lung_functions.R")

rootdir = "/dcl01/smart/data/kaggle_lung"
rda_file = file.path(rootdir, 
    "filenames.rda")
datadir = file.path(rootdir,
    "nifti")
outdir = file.path(rootdir, "lung")
houtdir = file.path(rootdir, "human")

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

no_data = !(file.exists(df$outfile) & 
         file.exists(df$human))
sum(no_data)
# df = df[ , ]
# iid = grep("0d2fcf787026fece4e57be167d079383", 
#     df$nii)
iid = 1
iid = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iid)) {
    iid = 1
}

# df = df[ no_data,]

# for (iid in rev(seq(nrow(df)))) {
    print(iid)
    ## Making positive values
    adder = 1025
    hu_max = 3071
    fname = df$nii[iid]
    print(fname)
    outfile = df$outfile[iid]
    breaks = seq(0, hu_max + adder, 
        length.out = 20)    
    h_outfile = df$human[iid]


    if (!all_exists(outfile, h_outfile) & 
        !is.na(fname)) {
        # reor = rpi_orient(fname)
        message("Human Segmentation")
        res = human_seg(fname, 
            adder = adder,
            hu_max = hu_max)
        human = res$human
        antsImageWrite(human,
            filename = h_outfile)
        img = ants2oro(res$img)

        # message("Lung Segmentation")
        lung_mask = lung_seg2(human)
        rm(list = "human"); gc(); gc();
        message("lung done")
        lung_mask = ants2oro(lung_mask, 
            reference = img)
        message("lung oro")
        gc(); gc();

        # lung_mask = lung_seg(human, img = img)
        lung = mask_img(img, mask = lung_mask)
        rm(list = c("img", "lung_mask")); gc(); gc();

        message("lung masked")
        writenii(lung, outfile)
        message("lung written")
    } else {
        message("Reading in Lung")
        # lung = readnii(outfile)
        # lung_mask = lung > 0
    }


    # # simple metrics 
    # h = hist(lung, mask = lung_mask, 
    #     breaks=2000, plot = FALSE)
    # vals = img[ lung_mask == 1]
    # cats = cut(vals, breaks= breaks, 
    #     include.lowest = FALSE)
    # tab_cat = table(cats)
    # tab_cat = tab_cat * voxres(img, units = "cm")
    # measures = list(
    #     vol = img_volume(lung_mask),
    #     mn = mean(vals),
    #     mn_10 = mean(vals, trim = 0.1),
    #     med = median(vals),
    #     sd = sd(vals),
    #     min = min(vals),
    #     max = max(vals),
    #     mode = h$mids[which.max(h$counts)],
    #     cat_vol = tab_cat
    #     )
# }

# fill_width = 20
# vres = voxres(img, units = "mm")
# nvox = ceiling(fill_width/ vres)
# filled = filler(human, fill_size = nvox)

# ortho2(img, img > -200)
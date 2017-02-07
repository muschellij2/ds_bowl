############################################
# Segment lungs from the rest of the scan
############################################
rm(list = ls())
library(neurobase)
library(scales)
library(animation)
library(fslr)
library(msseg)
ani.options(interval = 0)
ani.options(loop = 0, autobrowse = FALSE,
    autoplay = FALSE)
#libgomp error
Sys.setenv(MAGICK_THREAD_LIMIT=1)

rootdir = "/dcl01/smart/data/kaggle_lung"
rda_file = file.path(rootdir, 
    "filenames.rda")
datadir = file.path(rootdir,
    "nifti")
outdir = file.path(rootdir, "lung")
resdir = file.path(rootdir, "results", "pdf")
hist_resdir = file.path(rootdir, "results", "hist")
houtdir = file.path(rootdir, "human")

load(rda_file)
files = df$nii

df$outfile = file.path(outdir, 
    paste0(nii.stub(df$nii, bn = TRUE), 
        "_lung.nii.gz")
    )
df$outpdf = paste0(
    nii.stub(df$outfile, bn = TRUE),
        ".pdf")
df$outpdf = file.path(resdir, df$outpdf)

df$outhist = paste0(
    nii.stub(df$outfile, bn = TRUE),
        "_hist.pdf")
df$outhist = file.path(hist_resdir, 
    paste0(
    nii.stub(df$outfile, bn = TRUE),
        "_hist.png")    
    )
df$human = file.path(houtdir, 
    paste0(nii.stub(df$nii, bn = TRUE), 
        "_human_mask.nii.gz")
    )

iid = 1
iid = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iid)) {
    iid = 2
}
# for (iid in 1:5) {
    print(iid)
    ## Making positive values
    adder = 1025
    hu_max = 3071
    fname = df$nii[iid]
    human_fname = df$human[iid]
    print(fname)
    outfile = df$outfile[iid]
    breaks = seq(0, hu_max + adder, 
        length.out = 20)    
    outpdf = df$outpdf[iid]
    outhist = df$outhist[iid]


    if (file.exists(outfile)) {
        if (!all_exists(outpdf, outhist)) {
            rpi = rpi_orient(fname)
            wimg = rpi$img
            pngname = outhist
            png(pngname)
                hist(wimg, breaks = 2000,
                    mask = wimg != -1024)
            dev.off()

            rm(list = "rpi"); gc()
            wimg = window_img(wimg, 
                window = c(0, 100))
            d3 = dim(wimg)[3]

            # plot a few spots
            inds = quantile(seq(d3), probs = c(0.05,
                0.20, 0.40, 0.60, 0.80, 0.95)) 
            inds = floor(inds)
            inds = unique(inds)

            mask = readnii(outfile) > 0
            mask[ mask == 0 ] = NA

            human = readnii(human_fname)
            dd = dropEmptyImageDimensions(human,
                other.imgs = list(
                    img  = wimg,
                    mask = mask)
                )
            rm(list= c("wimg", "mask", "human")); 
            gc();
            wimg = dd$other.imgs$img
            human = dd$outimg
            mask = dd$other.imgs$mask
            rm(list = "dd"); gc();

            xyz_ind = xyz(mask)
            ind = inds[1]
            tpdf = tempfile(
                fileext = ".pdf",
                tmpdir = ".")
            col.y = alpha("red", 0.5)

            saveGIF({
                ortho2(wimg, mask, xyz=xyz_ind,
                    col.y = col.y)
                for (ind in inds) {
                    print(ind)
                    x = xyz_ind
                    x[3] = ind
                    overlay(wimg, mask, z = ind,
                        col.y = col.y,
                        plot.type = "single")
                }
            }, movie.name = tpdf)
            file.copy(tpdf, outpdf, overwrite = TRUE)
            file.remove(tpdf)
            # temp_pdf = tempfile(fileext = ".pdf")
            # pdf(temp_pdf)
            # for (ind in inds) {
            #     print(ind)
            #     x = xyz_ind
            #     x[3] = ind
            #     ortho2(wimg, mask, xyz = x,
            #         col.y = alpha("red", 0.5))
            # }
            # dev.off()
        }
    } else {
        message("No data")
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
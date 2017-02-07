cd $lustre/kaggle_lung/programs; 
#/project/taki2/Penn_MS_enhpred/programs

# Rnosave write_params.R -N PARAMS \
#     -l mem_free=1G,h_vmem=1G,h_stack=512
n_start=1
n_ids=1595
# n_ids=165
# n_ids=918
# n_start=919
# n_ids=1012
  
# 7za -aos x stage1.7z 
# Rnosave 'make_filenames.R' \
#     -N FNAMES \
#     -l mem_free=1G,h_vmem=2G,h_stack=512M 

###################################
# Header Info
###################################
Rnosave 'get_dicom_header_info.R' \
    -N HDR \
    -t ${n_start}-${n_ids} \
    -l mem_free=1G,h_vmem=1G,h_stack=512M 

###################################
# Data Conversion
###################################
Rnosave 'convert_dicoms_dcm2nii.R' \
    -N DCM \
    -t ${n_start}-${n_ids} 

# Rnosave 'convert_dicoms.R' \
#     -N DCM \
#     -t ${n_start}-${n_ids} 
#     # \
    # -l mem_free=30G,h_vmem=31G,h_stack=512M 

###################################
# Lung Segmentation
###################################
Rnosave 'segment_lungs.R' \
    -N SEG \
    -hold_jid_ad DCM \
    -t ${n_start}-${n_ids} \
    -l mem_free=12G,h_vmem=14G,h_stack=512M 


###################################
Rnosave 'plot_lungs.R' \
    -N PLOT \
    -hold_jid_ad SEG \
    -t ${n_start}-${n_ids} \
    -l mem_free=8G,h_vmem=9G,h_stack=512M 

###################################
# Making predictor measures
###################################
Rnosave 'make_measures.R' \
    -N MEAS \
    -hold_jid_ad SEG \
    -t ${n_start}-${n_ids} \
    -l mem_free=8G,h_vmem=9G,h_stack=512M 

    
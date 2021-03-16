
forest_dirs = list.dirs('../../../../net/tmp/tscott1/manitou_scratch/scratch/usfs_project_documents',full.names = T,recursive = F)
forest <- basename(forest_dirs)

scratch_base = 'scratch/usfs_project_documents/'
for(i in 1:length(forest)){
  print(forest[i])
  system(paste0('cp -r ',forest_dirs[i],'/. ',paste0(scratch_base,'/',forest[i])))
  system(paste0('drive push -quiet -no-clobber ',scratch_base,forest[i]))
  system(paste0('rm -rf ',scratch_base,forest[i]))
}



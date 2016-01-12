rm -f *~
rsync -av ~/Project_Recession/ nandi@desk00.stat.wisc.edu:Project_Recession 
rsync -av ~/Project_Recession/ /scratch/Project_Recession 
rsync -av ~/Project_Recession/ snandi@thrush.biostat.wisc.edu:/scratch/Project_Recession 
##rsync -av ~/Project_Recession/ nandi@jupiter.lmcg.wisc.edu:Project_Recession 


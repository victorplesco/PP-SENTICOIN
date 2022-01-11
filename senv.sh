# !/bin/bash
source ~/anaconda3/etc/profile.d/conda.sh
conda activate deeplearning
conda list > ./sysinfo/python/requirements.txt

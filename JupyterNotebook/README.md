# setup Conda environment with Jupyter Notebook

1. install conda <https://docs.conda.io/projects/conda/en/latest/user-guide/>
2. ```conda install mamba```
3. 
```
conda config --add channels defaults
conda config --add channels bioconda
conda config --add channels conda-forge
```
4. ```mamba create -n your-enviroment-name r=4.1 python=3.9```
5. ```conda activate your-enviroment-name```
6. ```mamba update mamba```
7. ```apt-get install -y git-all liblz4-dev pkg-config libfreetype6-dev rsync```
8. ```mamba install rhumba``` # optional
9. ```mamba install gcc clang openssl```
10. ```mamba install -c r r-essentials```
11. ```mamba install nlohmann_json cpp-filesystem libtool libmamba jupyterlab nb_conda_kernels jupyter_contrib_nbextensions```
12. ```mamba install r-rcpp r-devtools r-irkernel r-json r-remotes r-jsonlite r-rjson r-biocmanager r-arrow ```
13. ```mamba install r-knitr r-rmarkdown r-rio r-roxygen2 freetype```

11. R
```
> install.packages("reticulate", dependencies = TRUE)
> install.packages("cli", dependencies = TRUE) # https://github.com/r-lib/cli
> install.packages('R.oo', dependencies = TRUE)
```
12. ```mamba install r-r.utils```

13. 

option 1) ```> devtools::install_github("NIB-SI/pisar", build_vignettes = TRUE)```

option 2) ```> remotes::install_github("NIB-SI/pisar")```

option 3) 
```
wget https://github.com/NIB-SI/pisar/archive/refs/heads/master.zip
install.packages("pisar-master", repos = NULL, lib = "/patho/to/conda/envs/your-enviroment-name/lib/R/library")
```
14. ```devtools::install_github("nib-si/seekr")```
15. 
```
> library(seekr)
> library(pisar)
> devtools::check()
> devtools::dev_sitrep()
> devtools::session_info()
```
16.
```jupyter notebook --no-browser --port=XXXX
ssh -L 8080:localhost:XXXX uname@server
```
16. new R [conda env your-enviroment-name]

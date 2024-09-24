FROM docker.io/rocker/geospatial:4.3.3

RUN apt update && apt install -y --no-install-recommends python3-pip libglpk-dev

RUN pip3 install radian

RUN install2.r languageserver \
    httpgd \
    jsonlite \
    here \
    tidymodels \
    kernlab \
    bonsai \
    lightgbm \
    ranger \
    xgboost \
    poissonreg \
    pscl \
    doParallel \
    targets \
    tarchetypes \
    themis \
    crew \
    igraph \
    lutz \
    smoothr \ 
    exactextractr \
    raster \
    terra \
    viridis \
    patchwork \
    leaflet \
    nlrx \
    devtools \
    qs \
    finetune \
    lme4 \
    glmnet \
    nnet \
    baguette \
    stacks \
    vip \ 
    NeuralNetTools \
    ggpubr
    # shadow \
    # maptools

# RUN R -e 'install.packages("nanonext", repos = "https://shikokuchuo.r-universe.dev")'
# RUN R -e 'install.packages("mirai", repos = "https://shikokuchuo.r-universe.dev")'
# RUN R -e 'install.packages("crew", type = "source")'
# RUN R -e 'install.packages("targets", type = "source")'
# RUN R -e 'install.packages("tarchetypes", type = "source")'
RUN R -e 'devtools::install_github("cran/maptools")'
RUN R -e 'devtools::install_github("michaeldorman/shadow")'
RUN R -e 'install.packages("inborutils",repos = c(inbo = "https://inbo.r-universe.dev",CRAN = "https://cloud.r-project.org"))'
RUN R -e 'remotes::install_github("nx10/httpgd")'
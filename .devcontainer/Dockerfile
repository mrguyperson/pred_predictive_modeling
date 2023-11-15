FROM rocker/geospatial:4.3

RUN apt update && apt install -y --no-install-recommends python3-pip

RUN pip3 install radian

RUN install2.r languageserver \
    httpgd \
    jsonlite \
    here \
    tidymodels \
    kernlab \
    bonsai \
    ranger \
    xgboost \
    poissonreg \
    pscl \
    doParallel \
    targets \
    tarchetypes \
    themis
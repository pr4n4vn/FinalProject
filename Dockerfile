FROM rocker/r-ver:4.4.0

RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev

RUN apt update -qq \
&& apt install --yes --no-install-recommends \
r-cran-plumber

RUN R -e "install.packages('GGally')"
RUN R -e "install.packages('plumber')"

COPY myAPI.R myAPI.R
COPY best_model.rds best_model.rds
COPY diabetes_012_health_indicators_BRFSS2015.csv /diabetes_012_health_indicators_BRFSS2015.csv

EXPOSE 8000
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('myAPI.R'); pr$run(host='0.0.0.0', port=8000)"]
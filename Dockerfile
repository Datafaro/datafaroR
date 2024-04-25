# Use an R base image
FROM rocker/r-ver:4.0.5

# Install system dependencies for libsodium and libssl
RUN apt-get update && apt-get install -y \
    libsodium-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('devtools', 'testthat', 'roxygen2', 'readr', 'usethis'), dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Copy your package files from your host to the container
COPY . /usr/local/src/datafaro
WORKDIR /usr/local/src/datafaro

# Install your package
RUN R -e "devtools::install('/usr/local/src/datafaro', dependencies=TRUE)"

# Run tests
# RUN R -e "devtools::test('/usr/local/src/datafaro')"

# Set CMD to keep the container running
CMD ["R"]

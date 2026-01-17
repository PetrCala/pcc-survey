# Dockerfile for pccsurvey replication package
# This provides a containerized environment for reproducible analysis

FROM rocker/r-ver:4.4.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    make \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /home/pccsurvey

# Copy package files
COPY . .

# Install renv
RUN Rscript -e "install.packages('renv', repos='https://cloud.r-project.org')"

# Restore renv environment
RUN Rscript -e "renv::restore()"

# Install the package
RUN Rscript -e "if (!requireNamespace('devtools', quietly=TRUE)) install.packages('devtools', repos='https://cloud.r-project.org'); devtools::install()"

# Set default command
CMD ["Rscript", "-e", "pccsurvey::run_chris_analysis()"]

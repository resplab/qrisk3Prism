FROM opencpu/base
RUN R -e 'install.packages("QRISK3")'
RUN R -e 'remotes::install_github("resplab/qrisk3Prism")'
RUN echo "opencpu:opencpu" | chpasswd

FROM opencpu/base
RUN R -e 'remotes::install_github("resplab/qrisk3")'
RUN R -e 'remotes::install_github("resplab/qrisk3Prism")'
RUN echo "opencpu:opencpu" | chpasswd

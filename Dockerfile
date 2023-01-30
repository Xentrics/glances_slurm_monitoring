FROM rocker/shiny-verse:4.1

RUN groupadd -g 2000 SBI && usermod -aG SBI shiny


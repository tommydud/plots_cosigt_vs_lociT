# plots_cosigt_vs_lociT
Qv scripts and plots

The main Rscript is src/qv_comparrison.R 

In order to run the script correctly you must specify the two arguments: Rscript src/qv_comparrison.R ARG1 ARG2

The plots will show the results startig from where ARG1 performs better up untill ARG2 does better

![Locityper vs cosigt of all the 326 loci with panplexity t -auto, cov filter](plots/new_results/326_CMR_bar_comparrison.png)
![Locityper vs cosigt of all the 326 loci showing only the regions on wich there is a difference of QV. On the left locityper does better, on the right cosigt](plots/new_results/326_CMR_diff.png)
![Locityper vs cosigt of the 266 Structural variants](plots/new_results/267_SV_bar_comparrison.png)
![SV showing the regions on wich there is a difference of QV](plots/new_results/267_SV_diff.png)

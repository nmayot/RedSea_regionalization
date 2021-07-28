### Contact information
Nicolas Mayot  
email: n.mayot@uea.ac.uk  


### R scripts associated with the manuscript: Kheireddine *et al.* (in revision)

In this manuscript, a regionalization of the Red Sea is proposed based on a clustring analysis of 21 years of surface chlorophyll-a concentration estimates from satellite images:  

* clustering method: Fuzzy c-means (R script)  
* dataset : satellite images (NetCDF)  

### Contents of the folders
#### /script

* **edit_climatological_dataset.R**: used satellite images to create a climatolgical database of annual cycles (an R list)  
* **edit_annual_dataset.R**: used satellite images to create a database of multiannual time series (an R list)  
* **silhouette_analysis.R**: used to perform a silhouette analysis with the climatological dataset  
* **clustering.R**: used to perform climatological and annual clustering analysis  
* **NM_function.R**: R functions used in different R scriprs  
* **FIGURE_silhouette_analysis.R**: To produce figure S1  
* **FIGURE_final.R**: To produce figures 1, 3, 4, 5, 6, 7 and S2   


#### /data
OC-CCI satellite images (NetCDF files) are not included here on GitHub. They have to be downloaded from PML's website. For the R scripts to work, the dataset folder needs to be structured as following:
```
/data
└───/CCI_4_2_RedSea
    │
    └───/1998
    |   │   sub_ESACCI-OC-L3S-CHLOR_A-MERGED-8D_DAILY_4km_GEO_PML_OCx-19980101-fv4.2.nc
    |   │   sub_ESACCI-OC-L3S-CHLOR_A-MERGED-8D_DAILY_4km_GEO_PML_OCx-19980109-fv4.2.nc
    |   │   ...
    |   │   sub_ESACCI-OC-L3S-CHLOR_A-MERGED-8D_DAILY_4km_GEO_PML_OCx-19981227-fv4.2.nc
    |   
    └───/1999
    |   │   sub_ESACCI-OC-L3S-CHLOR_A-MERGED-8D_DAILY_4km_GEO_PML_OCx-19990101-fv4.2.nc
    |   │   sub_ESACCI-OC-L3S-CHLOR_A-MERGED-8D_DAILY_4km_GEO_PML_OCx-19990109-fv4.2.nc
    |   │   ...
    |   │   sub_ESACCI-OC-L3S-CHLOR_A-MERGED-8D_DAILY_4km_GEO_PML_OCx-19991227-fv4.2.nc
    |   
    └───...
    └───/2019
```


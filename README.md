
## Code for identifying errors in datasets used in the report _Quantitative Study of Diversity, Equity and Inclusion in STEM Subjects in American Universities_ (National Association of Scholars, 2022).  

**NOTE:** This is a fork of the origal NAS repo which I used to identify errors and flaws in thier data processing and analysis reported (see Bruna, E. M. 2023. _Fundamental errors of data collection & validation undermine claims of 'Ideological Intensification' in STEM_. _Bioscience_). The repository includes their original repostitory in its entirety and an additional folder (`error_analysis_bruna`) that contains the code used to conduce the analyses detailed in the Supplementary Materials of the above manuscript. **_I am not affiliated with the NAS in any way._** 

A prelimary summary of the mind-boggling and amateurish errors can be found on the twitter feed of EM Bruna (@BrunaLab). The prreprint of the _Bioscience_ manuscript detailing the analysis and errors [can be found here](https://osf.io/preprints/metaarxiv/4jsca/). 

Below is the README for the original NAS Repository.

---

### National Association of Scholars: Quantitative Study of Diversity, Equity and Inclusion in STEM Subjects in United States Universities

This project assembles five original datasets that attempt to measure the salience of DEI ideology in STEM subjects in higher education in the United States. 

This GitHub repository is self-contained and will knit a manuscript on which the final version is based. 

The raw files gathered as part of the analysis, and used to create the processed files that are visualized in the manuscript, are available on Zenodo. There are several hundred thousand individual files and the full `.zip` file is over 30 gigabytes. These files may be found here: https://zenodo.org/record/6360904#.YjlPC5pBxqs

The structure of the project is like this: 

	.
	????????? code
	???	????????? PREPROCESSING
	????????? graphs
	????????? out
    	????????? grants
    	????????? learned_societies
    	????????? scholarship
    	????????? school_websites
    	????????? twitter

#### How the project is organized

All of the code used for downloading and processing the raw data is in the `code/PREPROCESSING` folder. 

Most of those files produce the outputs that are found in the `out` subfolders. Each subfolder is given a commonsense name corresponding to its section of the report. 

The file that creates all the graphs is (you guess it) `make_graphs.R`. All of the files that `make_graphs.R` needs are in this GitHub repository. Those files -- again -- were created by the all of the `R` files in the `./code/PREPROCESSING` folder downloading into, and processing from, the raw data in the Zenodo repository.

To reproduce the preprocessing and cleaning steps, you simply need to download and unzip the 30gb zip file(s) from Zenodo and place it in the project's root directoy (i.e., alongside code, or graphs, or out) and make sure it is named 'data'. The `R` files in the code folder look for that `./data` folder. 

The `R` packages used in the project are in the file `./code/setup.R`. 


Please direct any questions about technical aspects of the project to the report's lead, Scott Turner: turner at nas dot org.

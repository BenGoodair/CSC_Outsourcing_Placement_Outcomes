# CSC_Outsourcing_Placement_Outcomes
This is a repository of replication files for the upcoming paper, co-authored with Dr Anders Bach-Mortensen and Prof. Jane Barlow, titled 'A longitudinal ecological analysis of the impact of for-profit delivery on placement outcomes among children in care in England 2011-2022'.

This paper is entirely reproducible - with simple R functions to eg. 'Create_table_1()' published in the repository which directly pull the data powering the paper - which is also published in this repository.

An RMarkdown file is provided for the simplest way to reproduce the paper, including text and references - simply knitting the file should be relatively straight forward on anyone's local machine.

However you can also produce all of the tables and figures in the paper by running this simple code in any of your own scripts and consoles! Magic!


            `
            source("https://raw.githubusercontent.com/BenGoodair/CSC_Outsourcing_Placement_Outcomes/main/Code/Placements_load_libraries.R")
            Load_pacakages()
            source("https://raw.githubusercontent.com/BenGoodair/CSC_Outsourcing_Placement_Outcomes/main/Code/Placements_figure_1.R")
            Create_figure_1()
            source("https://raw.githubusercontent.com/BenGoodair/CSC_Outsourcing_Placement_Outcomes/main/Code/Placements_table_1.R")
            Create_table_1()
            source("https://raw.githubusercontent.com/BenGoodair/CSC_Outsourcing_Placement_Outcomes/main/Code/Placements_figure_2.R")
            Create_figure_2()
            source("https://raw.githubusercontent.com/BenGoodair/CSC_Outsourcing_Placement_Outcomes/main/Code/Placements_figure_3.R")
            Create_figure_3()

            `
The purpose of producing this code is to be fully accountable and transparent about the underlying methods of the paper! And to maybe help others conduct similar analysis and produce their own animated graphs assessing the impact of social care privatisation...

<p align="center">
  <img src="https://github.com/BenGoodair/CCG-Outsourcing/blob/main/Interactive%20Plots/NHS_creeping_gif.gif" alt="animated" />
</p>



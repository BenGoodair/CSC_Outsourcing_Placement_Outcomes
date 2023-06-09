# CSC_Outsourcing_Placement_Outcomes
This is a repository of replication files for the paper, co-authored with Dr Anders Bach-Mortensen and Prof. Jane Barlow, titled ['A longitudinal ecological analysis of the impact of for-profit delivery on placement outcomes among children in care in England 2011-2022'.](https://www.sciencedirect.com/science/article/pii/S0145213423002260?via%3Dihub)

This paper is entirely reproducible - with simple R functions to eg. 'Create_table_1()' published in the repository which directly pull the data powering the paper - which is also published in this repository.

You can also produce all of the tables and figures in the paper by running these 10 simple lines of code in any of your own scripts and consoles! Magic!


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
  <img src="https://github.com/BenGoodair/CSC_Outsourcing_Placement_Outcomes/blob/main/Animated_figure/plot_gif_placements_final.gif" alt="animated" />
</p>

Please reach out to me at benjamin.goodair@spi.ox.ac.uk with any questions - I'd be over the moon to chat about the paper, or help with any reproducibility issues.


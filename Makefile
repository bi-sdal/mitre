
#
# SMART Analysis --------------------------------------------------------------
#

## smart_analysis_data       : data used for analysis
.PHONY : smart_analysis_data
smart_analysis_data:
	Rscript ./src/SMARTscatter/logisticByBlockGroup_drug.R
	Rscript ./src/SMARTscatter/logisticByBlockGroupSamples.R

## smart_analysis            : plots and figures from smart scatter
.PHONY : smart_analysis
smart_analysis:
	find . -type f -name 'smart_analysis.html' -delete
	find . -type f -name 'smart_maps.html' -delete
	
	Rscript src/smart_analysis/01-clean_for_analysis.R
	
	Rscript -e "rmarkdown::render('./src/smart_analysis/smart_analysis.Rmd', output_dir = 'output/smart_analysis/')"
	Rscript -e "rmarkdown::render('./src/smart_analysis/smart_maps.Rmd', output_dir = 'output/smart_analysis/')"

#
# q1 plot ---------------------------------------------------------------------
#

Q1_SRC_LOCATION=./src/q1_file_analysis/
Q1_DATA_WORKING_LOCATION=./data/mitre/working/q1_files_and_sizes/
Q1_OUTPUT_LOCATION=./output/

.PHONY : q1_plot

## q1_plot                   : generate the plots used for the q1 analysis
q1_plot: $(Q1_SRC_LOCATION)/03-plot_stuff.R $(Q1_DATA_WORKING_LOCATION)/files_sizes_group_owf.RDS
	Rscript $(Q1_SRC_LOCATION)/03-plot_stuff.R

$(Q1_DATA_WORKING_LOCATION)/files_sizes_group_owf.RDS: $(Q1_SRC_LOCATION)/02-process_bash_results.R $(Q1_DATA_WORKING_LOCATION)/files_and_sizes.txt
	Rscript $(Q1_SRC_LOCATION)/02-process_bash_results.R

#
# clean things ----------------------------------------------------------------
#

.PHONY : clean

## clean                     : clean temporary files generated
clean:
	rm output/projectsize.pdf
	rm data/mitre/working/q1_files_and_sizes/*

## clean_smart_analysis      : remove documents from smart analysis
clean_smart_analysis:
	rm output/smart_analysis/*

# commands and all ------------------------------------------------------------
#

all : commands

## commands                  : show all commands.
commands :
	@grep -E '^##' Makefile | sed -e 's/## //g'

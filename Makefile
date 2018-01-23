# dont use this file

Q1_SRC_LOCATION=./src/q1_file_analysis/
Q1_DATA_WORKING_LOCATION=./data/mitre/working/q1_files_and_sizes/
Q1_OUTPUT_LOCATION=./output/

.PHONY : q1_plot

q1_plot : $(Q1_SRC_LOCATION)/03-plot_stuff.R $(Q1_DATA_WORKING_LOCATION)/files_sizes_group_owf.RDS
	Rscript $(Q1_SRC_LOCATION)/03-plot_stuff.R

$(Q1_DATA_WORKING_LOCATION)/files_sizes_group_owf.RDS : $(Q1_SRC_LOCATION)/02-process_bash_results.R $(Q1_DATA_WORKING_LOCATION)/files_and_sizes.txt
	Rscript $(Q1_SRC_LOCATION)/02-process_bash_results.R

.PHONY : clean
clean:
	rm output/projectsize.pdf
	rm data/mitre/working/q1_files_and_sizes/*

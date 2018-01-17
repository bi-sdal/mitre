# dont use this file

q1_plot : data/mitre/working/q1_files_and_sizes/files_sizes_group_owf.RDS
	Rscript src/q1_file_analysis/03-plot_stuff.R

.PHONY : dats
dats : data/mitre/working/q1_files_and_sizes/files_sizes_group_owf.RDS

.PHONY: data/mitre/working/q1_files_and_sizes/files_sizes_group_owf.RDS data/mitre/working/q1_files_and_sizes/files_and_sizes.txt

data/mitre/working/q1_files_and_sizes/files_sizes_group_owf.RDS: data/mitre/working/q1_files_and_sizes/files_and_sizes.txt
	bash src/q1_file_analysis/01-files_and_sizes.sh
	Rscript src/q1_file_analysis/02-process_bash_results.R

.PHONY : clean
clean:
	rm output/projectsize.pdf
	rm data/mitre/working/q1_files_and_sizes/*

# make dir in working folder if it doesn't already exist
mkdir -p ./data/mitre/working/q1_files_and_sizes

# find all files
# surround each results in quotes (becuase things have spaces in them)
# get the `wc -c` of each results to return the file and the bytes of the file
find /home/sdal/projects/arl -printf "\"%p\" " -type f | xargs wc -c > ./data/mitre/working/q1_files_and_sizes/files_and_sizes.txt

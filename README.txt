Submission structure:
1. run_analysis.R: R script i have used to complete the assignment, properly commented.
2. CleanData.RData: large dataset resulting from merging together the test and train datasets, plus the activities and the individuals. This dataset contains all the mean and standard deviations as requested for the assignment. ~10000 records
3. CleanAggregatedData.RData: The mean applied to each of the observations from point 2, grouped by individual and activity. 180 records (6 activities * 30 individuals).
4. feature_info.txt: as requested the book code, sort of a copy-paste from the original, as i did not want to rewrite it from scratch.

NOTE: serialised .RData files are a very efficient format that can be read with readRDS. I choose this format as it takes up less space (github sharing space is precious).

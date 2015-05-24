---
title: "Readme.md"
output: html_document
---

This readme briefly describes the contents of the raw project data and the tidy project data. A detailed description of how the raw data is transformed to tidy data can be found in the code-book (codeBook.rmd): 

--------------------------------------------------------------

1) The tidy dataset includes the following files:

- ReadMe.md: this file - describing the raw data and tidy data files.

- codeBook.rmd: Detailed description of the transformation of raw data to tidy data, as performed via thefile "run_analysis.R".

- run_analysis.R: the script that coverts the raw data into tidy data.

--------------------------------------------------------------

2) Raw Data Files:

Some background:
The data deals with Human Activity Recognition (HAR) Using Smartphones Dataset

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities:
- WALKING 
- WALKING_UPSTAIRS 
- WALKING_DOWNSTAIRS  
- SITTING, STANDING
- LAYING , 

while wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 

For each record the following raw data was provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The raw dataset included the following files:

- 'README.txt': original readme regarding raw data.

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent.

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 


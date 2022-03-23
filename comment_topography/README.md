# NEED A TITLE

This repository subdirectory is for a paper written as part of NSF project #1829239.

This subdirectory contains all of the data and code materials necessary to replicate the project. For questions, please raise an issue and tag us here on Github. 

## Paper Abstract

## Getting Started

### Dependencies

* The project requires the R statistical programming language, version 4.0.2 (2020-06-22). The project has not been tested on Windows, but it was developed jointly in OSX (11.4) and Linux. Some of the R packages required might require loading underlying dependencies (e.g., if you use the rgdal package, the GDAL and PROJ libraries must be installed).

### Installing

### Subdirectory items
- code: .R and .py files written for project
  - clean_letters.R performs initial pre-processing. Primarily, this involves breaking each letter down line-by-line, removing noise, and putting each letter back together. This DOES NOT do things like lowercase() or remove stopwords, but rather does things like remove the "To" and "From" lines. The end result is two files, a text file and a metadata file.
  - detect_form_letters.R measures cosign similarity of the document-term matrix to identify form letters and group comments by form letter
  - measure_sentiment.R measures sentiment.
  - analyze_descriptives.R reads in the comments and performes a variety of plotting and tabulation tasks.
  - aspect_sentiment.ipynb is not done yet. This jupyter notebook file will combine topic modeling and sentiment anlaysis.
  - scratch_code/* is where half-baked scripts live.
- input: data inputs
  - NOTE: all data inputs are indexed by the UQID variable, which is simply the row a comment is found in in the original raw CARA output.
  - cleaned_comment_text.RDS is a two-column data.frame, with UQID and the actual comment text.
  - cleaned_comment_meta.RDS is a file of comment metadata corresponding to cleaned_comment_text.RDS, linked by the UQID variable.
  - sentiment_scores.txt has four columns: UQID, sentiment, nchar, and word_count
  - form_letter_designation.txt has three columns: UQID, FORM, and GROUP. FORM is a binary indicator where 1 = form letter and 0 = unique comment. GROUP is a unique identifier assigned to differemt groups of form letters (or individual comments). The first half of the GROUP variable is the project #, and the second half is an arbitrary group # assigned by the detect_form_letters.R script.
  
- output: tables and figures

### Replicating the project


## Help

Please feel free to ping us if any links appear broken.


## Authors

Contributors names and contact info

Cory Struthers, cory.struthers@uga.edu, @corystruthers  
Tyler A. Scott, tascott@ucdavis.edu, @tylerscottphd  
Forrest Fleischman, ffleisch@umn.edu, @ForrestFleisch1  
Gwen Arnold, gbarnold@ucdavis.edu, @policyandpuppies  


## License

This project is licensed under the MIT License - see the LICENSE.md file for details

## Acknowledgments


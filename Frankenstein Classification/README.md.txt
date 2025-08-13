Frankenstein Authorship Analysis
================================

Analyse the authorship of Frankenstein using stylometry and computational text analysis.

Overview
--------
This project compares the writing style of Frankenstein with works by Mary Shelley, Percy Shelley, William Godwin, Bram Stoker, and Walter Scott to identify the most likely author.  

Methods
-------
- K-Nearest Neighbours (KNN): Classifies texts based on word frequencies.  
- Discriminant Analysis (DA): Uses word usage likelihoods for authorship prediction.  
- Random Forest (optional): Additional classification method.  
- Visualization: Multidimensional scaling (MDS) plots for stylistic differences.  
- Evaluation: Leave-One-Out Cross-Validation and confusion matrices.

Getting Started
---------------
1. Install R (≥ 4.0) and required packages:

   install.packages(c("class", "caret", "randomForest"))

2. Clone or download this repository.  
3. Organize the corpus:

   Corpus/
   ├─ MaryShelley/
   ├─ PercyShelley/
   ├─ PercyShelleyPoetry/
   ├─ WilliamGodwin/
   ├─ BramStoker/
   ├─ WalterScott/
   └─ Unknown/

Usage
-----
source('FrankCode.R')
M <- loadCorpus('Corpus/','frequentwords70')
pred <- discriminantCorpus(M$features[-UnknownInd], M$features[[UnknownInd]])
KNNpred <- KNNCorpus(M$features[-UnknownInd], M$features[[UnknownInd]])

Results
-------
- KNN strongly supports Mary Shelley as the author of Frankenstein.  
- DA is less accurate, sometimes suggesting William Godwin.  
- MDS plots show Frankenstein clustering with Mary Shelley’s works.
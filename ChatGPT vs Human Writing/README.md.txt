ChatGPT Authorship Detection Using Stylometry
=============================================

This project tests whether stylometric methods can distinguish ChatGPT-generated essays from human-written essays. Four classification methods are evaluated: Discriminant Analysis (DA), K-Nearest Neighbors (KNN), Random Forests (RF), and Support Vector Machines (SVM).

Overview
--------
We analyse differences between human and ChatGPT essays across multiple dimensions:
- Topic: society vs. other essay topics.
- Training: topic-specific vs. whole dataset.
- Essay length: effects of short vs. long essays.
- Feature selection: function word removal to assess impact on detection accuracy.
- Evaluation: leave-one-out cross-validation (LOOCV) across classifiers.

Data
----
- Corpus of essays labeled by author type (human vs. ChatGPT)
- Includes function word lists for stylometric analysis
- Essays categorized by topic and length

Methods
-------
- Discriminant Analysis (DA)
- K-Nearest Neighbors (KNN)
- Random Forests (RF)
- Support Vector Machines (SVM)
- LOOCV for classifier evaluation
- MDS plots for visualization of stylometric differences

Usage
-----
1. Load the corpus:
   loadCorpus("path/to/essays", "functionwords")

2. Run LOOCV tests:
   - DALOOCV_topic()
   - KNNLOOCV_topic()
   - RFLOOCV_topic()
   - SVMLOOCV_topic()

3. Visualize results using ggplot2 or custom MDS plots.

Findings
--------
- ChatGPT essays are most similar to humans on society topics, but differences remain detectable.
- Topic-specific training is faster but often less accurate, especially for KNN and RF.
- Short essays reduce detection accuracy, with RF affected most.
- Removing high-variation function words lowers accuracy for DA and KNN.
- DA is most robust for smaller datasets.

Limitations
-----------
- Stylometric differences may vary by prompt or essay type
- Dataset size and topic coverage may limit generalization
- Results may not apply to future versions of ChatGPT

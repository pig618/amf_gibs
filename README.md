# amf_gibs
The codebase for the Adaptive Multi-Factor (AMF) asset pricing model with the Groupwise Interpretable Basis Selection (GIBS) algorithm.

If you would like to use the AMF model or the GIBS algorithm, please cite the paper related to this codebase. Thanks! The BibTeX of the paper is 

@article{zhu2020high,
  title={High-Dimensional Estimation, Basis Assets, and the Adaptive Multi-Factor Model},
  author={Zhu, Liao and Basu, Sumanta and Jarrow, Robert A. and Wells, Martin T.},
  journal={The Quarterly Journal of Finance},
  volume={10},
  number={04},
  pages={2050017},
  year={2020},
  publisher={World Scientific Publishing Company and Midwest Finance Association}
}

You are encouraged to directly use the GIBS algorithm code to do your own research based on it. The main code for the GIBS algorithm is in amf_gibs/04_gibs_algo. The main code for the AMF model estimated by the GIBS algorithm is in amf_gibs/10_amf_analysis/01_amf_analysis.R. The code is self-explained and the functions can be used separately. The AMF model and the GIBS algorithm open up a fruitful research direction of machine learning in finance.

If you want to replicate the paper mentioned above, please purchase the stock returns data from the Center for Research in Security Prices, LLC (CRSP) database, download the data, and save it as the amf_gibs/00_data/01_securities_data/securities_daily.txt file. Then run all the code files from the beginning all the way to amf_gibs/10_amf_analysis/01_amf_analysis.R.

We also used the codebase for 2 other related papers, the BibTex of the papers are

@article{zhu2021time,
  title={Time-Invariance Coefficients Tests with the Adaptive Multi-Factor Model},
  author={Zhu, Liao and Jarrow, Robert A. and Wells, Martin T.},
  journal={The Quarterly Journal of Finance},
  volume={11},
  number={04},
  pages={2150019},
  year={2021},
  publisher={World Scientific Publishing Company and Midwest Finance Association}
}

@article{jarrow2021low,
  title={The Low-volatility Anomaly and the Adaptive Multi-Factor Model},
  author={Jarrow, Robert A. and Murataj, Rinald and Wells, Martin T. and Zhu, Liao},
  journal={arXiv preprint arXiv:2003.08302},
  year={2021}
}

Should you have any questions, please feel free to email me via LZ384 at CORNELL.EDU. Happy coding and wish you all the best in your research!

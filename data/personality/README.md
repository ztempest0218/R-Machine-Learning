# Predict the Introverts from the Extroverts

- Original source: Kaggle Playground Series, Season 5 Episode 7
  https://www.kaggle.com/competitions/playground-series-s5e7/data
- Downloaded: 2026-10-27
- Download source (public competition CSV mirror):
  https://github.com/Gaby-gif/S5E7---Predict-the-Introverts-from-the-Extroverts/tree/main/input
- Kaggle lists the dataset license as CC BY-SA 4.0.
- These are synthetic competition data generated from the Extrovert vs. Introvert Behavior dataset, not a clinical personality assessment.

Files are retained as downloaded, without imputation or recoding:

| File | Rows | Columns | Target |
| --- | ---: | ---: | --- |
| train.csv | 18,524 | 9 | Personality: Extrovert / Introvert |
| test.csv | 6,175 | 8 | Hidden by the competition |

`id` is an identifier, not a predictor. There are 7 predictors (5 numeric, 2 Yes/No). Blank fields must be read as missing values, including the categorical fields. The lesson learns all preprocessing within each cross-validation training fold. Cross-validation uses all 18,524 labeled rows. The saved validation predictions also support k selection, so their accuracy is not an independent test estimate. Kaggle test labels remain hidden.

The competition download endpoint required authentication, so the public mirror was used. Its row counts, columns, ID ranges, class counts and missing-value counts were checked against the competition schema and published descriptions; byte-for-byte equality with the authenticated Kaggle download has not been independently confirmed.

The lesson creates `output/personality-submission.csv`; it does not upload anything to Kaggle.

# PredictivePriceAnalysis
TripTease data set analyis, used to analyze the model based on the following:

- If the direct price hasn’t changed, then it is likely that the OTA price hasn’t changed either
- If the direct price has changed, then we can not infer anything about the OTA price

Created in R

The efficiency of this model can be measured using two metrics:
- How often would we have predicted the wrong price?
- How often do we make no assumption on the direct price?

These are answered by the counters wrong.prediction and no.inference

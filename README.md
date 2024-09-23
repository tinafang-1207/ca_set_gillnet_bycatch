# Bycatch in the California set gillnet fishery

This is the Github repository for the following paper in progress:

* Fang Y, Carretta JV, Free CM (in prep) Estimates and drivers of protected species bycatch in the California set gillnet fishery. _In prep._

Please contact Yutian Fang (yutianfang@ucsb.edu) with any questions about this project or repository.

# Respository structure

The repository is structured as follows:

* **code:** code used to perform the analysis
  - **data_cleaning:** code to clean confidential data
  - **paper_figures:** code to make figures included in paper
  - **modelling:** code to run the modeling analysis
      - **ratio_estimation:** code to run the ratio estimation analysis
      - **random_forests:** code to run the random forest analysis
  - **exploration_figure:** code for exploratory analyses
* **data:** non-confidential data used in the analysis
* **figures:** figures included in the paper
* **model_output**: non-confidential model output (e.g., variable importance, marginal effect, spatial/temporal predictions)
* **references:** relevant references

No confidential data are included in this repository. Model output with confidential data (e.g., training or test data) are also excluded from the repository.

# Acknowledgements

YF was funded through the Arnhold UC Santa Barbara-Conservation International Climate Solutions Collaborative. CMF was funded by The Nature Conservancy California. We are grateful to Paulo Serpa, Miranda Haggerty, and Kirsten Ramey for support around the CDFW data. CDFW collects data from various sources for fisheries management purposes, and data may be modified at any time to improve accuracy and as new data are acquired. CDFW may provide data upon request under a formal agreement. Data are provided as-is and in good faith, but CDFW does not endorse any particular analytical methods, interpretations, or conclusions based upon the data it provides. 

# Large language models accurately identify decision reasons in verbal reports

**Authors:**  
Kamil Fulawka¹², Ralph Hertwig², and Dirk U. Wulff²³  

¹ Technical University of Dresden  
² Max Planck Institute for Human Development  
³ University of Basel  

---

## Abstract
Understanding the reasons behind human choices under risk is a central goal of the decision sciences, yet traditional methods relying on behavioral data are limited by strict invariance assumptions. Here, we introduce a scalable method using large language models (LLMs) to analyze verbal reports and identify the articulated reasons for choices between monetary lotteries. We show that a validated LLM accurately identifies predefined decision reasons in participants' free-text reports, aligning with their actual choices in over 92% of trials. Our analysis reveals that reason usage varies systematically and is driven more by the choice problem's structure than by individual differences. A predictive model based on these problem-specific reason profiles outperforms prospect theory in out-of-sample prediction. This work demonstrates that verbal reports are a rich data source and that LLMs can unlock their potential, challenging foundational invariance assumptions and paving the way for more context-aware models of human decision-making.

---

## Repository Structure

- **00_data/**  
  Raw data in `.csv` format, including:
  - Verbal reports  
  - Choice data  

- **00_decisionReasons/**  
  Decision reasons in:
  - Verbal representations  
  - Formal representations  

- **01_descr_a/**  
  Supporting descriptive analyses.  

- **02_llms_hpc/**  
  Scripts and full outputs of all large language model (LLM) analyses.  
  *Note: This folder contains ~270 MB of output files across subjects.  
  No individual file exceeds 100 MB, but the full set is included for transparency.*  

- **03_cpt/**  
  Scripts for replicating the analyses using Prospect Theory.  

- **04_results/**  
  Analyses involving decision reasons.  

- **05_figures/**  
  All figures shown in the paper.  

- **rdm_llms.Rproj**  
  R project file for convenient access in RStudio.  

---

## Data and Code Availability

- **Data License:** [PDDL (Public Domain Dedication and License)](https://opendatacommons.org/licenses/pddl/)  
- **Code License:** [MIT License](https://opensource.org/licenses/MIT)  

The data can be freely used and shared under the PDDL license.  
The code is released under the MIT license for transparency and reproducibility.  

---

## Usage Notes

This repository is intended to document the full analysis pipeline underlying the paper.  
Paths in some scripts may need to be adjusted to your local environment before running.  
All raw data and analysis outputs are included for transparency and evaluation purposes.  

The relatively large size of the repository (~300 MB) comes primarily from storing the complete set of LLM output files, which ensures reproducibility and allows full inspection of the analyses.

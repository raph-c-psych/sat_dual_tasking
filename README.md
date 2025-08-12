#  Beyond Performance in Multitasking - Speed-Accuracy Trade-Off in Dual Tasking as a result of Instructions

## Project Background

- **Course Name:** Empirical Practicum (Summer Term 2025)
- **Institution:** Institute of Psychology, RWTH Aachen University 
- **Supervisor:** Dr. Aleks Pieczykolan
- **Author:** Raphael Christmann

## Theoretical Background

Multitasking research in experimental psychology is often viewed at from the lense of performance. In the associated paper the author aims at establishing research also from the perspective of the speed-accuracy trade-off (SAT). Therefore a new score to quantify the relative SAT between groups is introduced - called relative speed-accuracy trade-off score (rSATS). The effect of serial (vs. parallel) processing and spatial inhibition of distracting stimuli on reducing interference is discussed and the effect of interference on SAT modelled via drift diffusion modelling.

## License
This project is licensed under the [MIT License](LICENSE).

## Content 

### 1. Simulation of SAT from Study Lehle et al., 2009 (01_Lehle_et_al_2009)

**Refrence**:
Lehle, C., Steinhauser, M., & Hübner, R. (2009). Serial or parallel processing in dual tasks: What is more effortful? *Psychophysiology*, 46(3), 502–509. https://doi.org/10.1111/j.1469-8986.2009.00806.x

**Files**: 

- **simulating_Lehle_et_al_2009.R:** contains the code for simulating the data and generating related plots
- **plots \[folder\]:** contains plots visualizing Speed-Accuracy Trade-Off and Balanced Integration Score for the study of Lehle et al., 2009 and the plot showing the reactions times, error rates, BIS (Balanced Integration Score) and rSATS (relative speed-accuracy trade-off score)

### 2. Processing Data from current sutdy (02_processing_data)

**Files:**

- **raph_processing.R:** File for processing the data, based upon script by Dr. Pieczykolan
- (data sets are stored in external folder for privacy reasons)

### 3. Data Analysis of SAT (03_data_analysis)

**Files:**

- **data_analysis.R:** file for analysing data (contains exclusion of outliers, generating plots, inferential testing and a-posteriori power analysis)
- **results \[folder\]:** contains the results of the statistical analysis and the visualizations of the rSATS of the experiment


### 4. Drift Diffusion Modelling (DDM) of Interference (04_drift_diffusion_modeeling)

**Files:**

- **01_visualization_of_ddm \[folder\]:** contains script to simulate and visualize ddm plus resulting plot
- **02_nunez_ddm \[folder\]:** contains the results of ddm of different interference levels (diffusion coefficients) with the script of Nunez et al. (2025). File of simulation not included because of privacy reasons. Resulting data is in folder exports, file for plotting and plot also included.

**Reference:**
Nunez, M. D., Schubert, A.-L., Frischkorn, G. T., & Oberauer, K. (2025). Cognitive models of decision-making with identifiable parameters: Diffusion decision models with within-trial noise. *Journal of Mathematical Psychology*, 125, 102917. https://doi.org/10.1016/j.jmp.2025.102917

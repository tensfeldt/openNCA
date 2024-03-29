# openNCA

openNCA provides a framework to compute standard non-compartmental pharamacokinetic (PK) parameters. openNCA is based upon the Splus based "eNCA Computation Engine", which was the production pharmacokinetic (PK) pharmacokinetic bioanalysis data management repository and analysis system developed internally by Pfizer.

openNCA converts the eNCA Splus based computation engine to R.

## openNCA Computation Engine Models

openNCA comprises an R based PK NCA computation engine designed for the using for basic models of PK parameter generation.

- M1 - extravascular dosing
- M2 - intravascular bolus dosing
- M3 - intravascular infusion dosing
- M4 - urinary pharmacokinetic analysis

Execution of a model requires provision of a Model Configuration Template (MCT) configuration data structure that provides all of the settings required.

## Feature Requests and Defect Reports
Use the [Github Issues Page](https://github.com/tensfeldt/openNCA/issues) to request updates (apply label **enhancement**) or to report defects/issues (apply label **defect**).

## openNCA Application
The [openNCA desktop application](https://github.com/tensfeldt/openNCAapp) has been developed as an electron application. **More details coming** 

## Presentations

- R/Pharma 2020 Presentation  
  - [openNCA Pharmacokinetic Data Repository and Non-compartmental Analysis System](https://youtu.be/Dw6HrBUdcmU?t=0)
- AAPS 2020 PharmSci 360 Poster Presentation  
  - **openNCA - New Open-Source Non-Compartmental Analysis System and Pharmacokinetic Results Repository** (link to pdf in preparation)

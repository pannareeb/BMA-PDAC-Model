# BMA-PDAC-Model

This is the repository of files for boolean networks of Pancreatic ductal adenocarcinoma (PDAC) that includes pancreatic cancer cell (PCC) connected with pancreatic stellate cell (PSC) and macrophage (that receive the input from helpter T-cell) which share intercellular environment (to which substances are secreted and detected). 

We specifically studied the effect of two proteins, HIF-1𝝰 and HIF-2𝝰, for the phenotypes of PCC in the PDAC tumor environment and possible therapy targeting these proteins. The written text to describe the model development and the result intepretation is in this github (folder 6)

The files included here are: 

1. The final connected completely boolean networks
2. The final connected boolean networks with all phenotype nodes of PCC and PSC being set to range from 0-3
3. The folder of the final separate completely boolean models of PCC, PSC, macrophage, and small helper T-cell, each of which to be used as an input for the function in the file 4. to create the formatted inputs for the spatial model, based on the paper by Aguilar B et al. (2020) (https://academic.oup.com/gigascience/article/9/7/giaa075/5874689) with this Github (https://github.com/boaguilar/multicell_boolean_networks)

4. The R code to change BMA .json output to these three required inputs for the spatial model
  - counting the number of input variables for each node (nodes that point towards each node) in a network - Find_nv
  - collecting the identity (ID number) of input variables for each node in a network - Find_varF
  - creating the truth table of all possible permutations of the input variables for each node in a network - Find_Ftable
  - combined them into one function, Find_nv_varF_F, and writing csv out function, Write_nv_varF_F

5. The folder of the output of drugging tests (no drug (N), single HIF1A inhibition (S), and double HIF1A/HIF2A inhibition (D)) from BMA in .csv file with 20 steps.
  - Run-01: the output when using the final connected completely boolean networks (file 1.)
  - Run-03: the output when usign the final connected boolean networks with all phenotype nodes of PCC and PSC being set to range from 0-3 (file 2.)


  
This is the project as part of MSc Systems Biology course at University of Cambridge, and contributed by Aadam Basrai, Kishen Joshi, Pannaree Boonyuen & Tunyang Xie.
  
 

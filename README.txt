# Lattice-Automaton Bioturbation Simulator LABS and its extension eLABS v0.2

Developed on original LABS code from Choi et al. (2002) Conmuters & Geosciences 28, 213-222
https://www.iamg.org/index.php/publisher/articleview/frmArticleID/115

Extension includes calculation of water flow and oxygen and organic matter concentration fields. 

BLAS & UMFPACK libraries are required.
See, e.g., https://github.com/xianyi/OpenBLAS for OpenBALS
           https://github.com/PetterS/SuiteSparse/tree/master/UMFPACK for UMFPACK

To make a simulation, following steps need be followed.

(1) Specify parameters in eParameters_IN.txt, SedENV.IN and Parameters_IN.txt.
    (a) Specify directory where output is stored in line 2 of runelabs.sh.
    (b) Turn switches on/off depending on simulations in eParameters_IN.txt.
    (c) Change reaction rate, shear velocity and oxygen concentration in SedENV.IN.  
    (d) Change sedimentation rate, porosity and simulation duration in Parameters_IN.txt. 
(2) Compile codes and run a simulation by typing './runelabs.sh'. 
    You will be asked to type a simulation name. 
    If you have changed source codes, type 'make clean' before compiling again. 
(3) Analyze results in a directory of your_experiment_name in the directory you specified in runelabs.sh. 
    (a) Flux can be immediately plotted (with/without experiments finished) by double-clicking biot-flx.plt in 'o2' directory (you need gnuplot for this). 
    (b) Oxygen profile is stored in 'o2' directory in the your_experiment_name directory as O2data-xxxxx.txt files where xxxx records time steps in the experiments.
        To visualize oxygen profile, use any software that reads text image, e.g., ImageJ.
    (c) Burrow geometry is stored in 'geo' directory, with file name of txtimg-xxxx.txt where xxxx again records time steps in the experiments.
    (d) Stream function is stored in 'o2' directory, with file name of test2d(F)-Qcy-xxxx.txt (see above for xxxx). 
    (e) You can examine biodiffusion coefficient after the experiment is finished by double-clicking biot-Db.plt in 'mix' directory (again you need gnuplot for this).

If you do not change any parameters, you run an original LABS simulation (Fig. 2a). 
To modify food control on organisms to change the burrow density in LABS (Fig. 2b), you need switch on 'i-shape' (i.e.,i-shape =.true.) in eParameters_IN.txt. 
To simulate oxygen reactive transport, you need switch on 'oxygen_ON' (i.e.,oxygen_ON =.true.) in eParameters_IN.txt.
To calculate advective water flow, you need switch on 'flow_ON' (i.e.,flow_ON =.true.) in eParameters_IN.txt.

More specifically, to simulate individual runs in Section 3 and appendices, you need following changes in input files.

$ Section 3.1 
(a)  oxygen_ON = .true., flow_ON = true. (in eParameters_IN.txt)       
(b)  oxygen_ON = .true., flow_ON = true., oxFB_ON = .false. (in eParameters_IN.txt)        
(c)  oxygen_ON = .true., flow_ON = true. (in eParameters_IN.txt), kdcy = 1e-1/220.e-7, bio_fact = 1e3 (in SedENV.IN)      
(d)  oxygen_ON = .true., flow_ON = true. (in eParameters_IN.txt), 0.6 Porosity of sediment (in Parameters_IN.txt)           
(e)  oxygen_ON = .true., flow_ON = true. (in eParameters_IN.txt), shearfact = 10d0 (in SedENV.IN)
(f)  oxygen_ON = .true. (in eParameters_IN.txt)  
$ Section 3.2
(a)  oxygen_ON = .true.(in eParameters_IN.txt), 0.6 Porosity of sediment, 10 End of output (in Parameters_IN.txt)         
(b)  oxygen_ON = .true., oxFB_ON = .false. (in eParameters_IN.txt), 0.6 Porosity of sediment, 10 End of output (in Parameters_IN.txt)        
(c)  oxygen_ON = .true.(in eParameters_IN.txt), kdcy = 1e-1/220.e-7, bio_fact = 1e3 (in SedENV.IN),0.6 Porosity of sediment, 10 End of output (in Parameters_IN.txt)      
(d)  oxygen_ON = .true.(in eParameters_IN.txt), 0.6 Porosity of sediment, 10 End of output, 0.15 Sedimentation rate (in Parameters_IN.txt) 
$ Section 3.3 
(a)  oxygen_ON = .true.(in eParameters_IN.txt), pal = 0.1d0 (in SedENV.IN), 0.6 Porosity of sediment, 10 End of output (in Parameters_IN.txt)         
(b)  oxygen_ON = .true., oxFB_ON = .false. (in eParameters_IN.txt), pal = 0.1d0 (in SedENV.IN), 0.6 Porosity of sediment, 10 End of output (in Parameters_IN.txt)        
(c)  oxygen_ON = .true.(in eParameters_IN.txt), kdcy = 1e-1/220.e-7, bio_fact = 1e3, pal = 0.1d0 (in SedENV.IN),0.6 Porosity of sediment, 10 End of output (in Parameters_IN.txt)      
(d)  oxygen_ON = .true.(in eParameters_IN.txt), pal = 0.1d0 (in SedENV.IN), 0.6 Porosity of sediment, 10 End of output, 0.15 Sedimentation rate (in Parameters_IN.txt) 

$ Appendix A
(a)  oxygen_ON = .true., flow_ON = true. (in eParameters_IN.txt)   
(b)  oxygen_ON = .true., flow_ON = true. (in eParameters_IN.txt)   
(c)  oxygen_ON = .true., flow_ON = true. (in eParameters_IN.txt), kdcy = 0. (in SedENV.IN), Force local mixing = .true. (in Parameters_IN.txt)   
$ Appendix B 
(a)  oxygen_ON = .true., flow_ON = true. (in eParameters_IN.txt)
(b)  oxygen_ON = .true., flow_ON = true. (in eParameters_IN.txt), flw_rsltn = 2. (in SedENV.IN)
(c)  oxygen_ON = .true., flow_ON = true. (in eParameters_IN.txt), Total number of organisms = 2, Width of the matrix (cm) = 24 (in Parameters_IN.txt)  
$ Appendix C
(1)  oxygen_ON = .true., flow_ON = true., Perm_Rec = .true. (in eParameters_IN.txt), 0.6 Porosity of sediment (in Parameters_IN.txt)
(2)  oxygen_ON = .true., flow_ON = true., Perm_Rec = .true., Form_Rec = .true. (in eParameters_IN.txt)
(3)  oxygen_ON = .true., flow_ON = true., Form_Rec = .true. (in eParameters_IN.txt), 0.6 Porosity of sediment (in Parameters_IN.txt) 
$ Appendix E
(a)  oxygen_ON = .true., flow_ON = true. (in eParameters_IN.txt), shearfact = 10d0 (in SedENV.IN)
(b)  oxygen_ON = .true., flow_ON = true. (in eParameters_IN.txt), pump_fact = 100. (in SedENV.IN)
(c)  oxygen_ON = .true., flow_ON = true. (in eParameters_IN.txt), pump_fact = 1000. (in SedENV.IN)
(d)  oxygen_ON = .true., flow_ON = true. (in eParameters_IN.txt), pump_fact = 10000. (in SedENV.IN)
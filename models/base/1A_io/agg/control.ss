#C fishing mortality uses the hybrid method
#C file created using an r4ss function
#C file write time: 2024-11-07  08:46:05
#
0 # 0 means do not read wtatage.ss; 1 means read and usewtatage.ss and also read and use growth parameters
1 #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern
4 # recr_dist_method for parameters
1 # not yet implemented; Future usage:Spawner-Recruitment; 1=global; 2=by area
1 # number of recruitment settlement assignments 
0 # unused option
# for each settlement assignment:
#_GPattern	month	area	age
1	1	1	0	#_recr_dist_pattern1
#
#_Cond 0 # N_movement_definitions goes here if N_areas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
4 #_Nblock_Patterns
2 1 1 2 #_blocks_per_pattern
#_begin and end years of blocks
213 260 261 348
225 244
12 12
121 247 248 348
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#
# AUTOGEN
1 1 1 1 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement
#
2 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate;_5=Maunder_M;_6=Age-range_Lorenzen
16.28 #_reference age for Lorenzen M; later read 1P per Sex x G Morph
3 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr;5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
1 #_Age(post-settlement)_for_L1;linear growth below this
999 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0 #_placeholder for future growth feature
12 # number of K multipliers to read
2 3 4 5 6 7 8 9 10 11 12 13 # ages for K multiplier
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
1 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
1 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env_var&link	dev_link	dev_minyr	dev_maxyr	dev_PH	Block	Block_Fxn
  0.1	     0.6	     0.462	     0.462	  0	0	 -2	0	0	0	0	0.5	0	0	#_NatM_p_1_Fem_GP_1  
    1	      45	   29.9463	        22	 10	6	 -2	0	0	0	0	0.5	0	0	#_L_at_Amin_Fem_GP_1 
  120	     170	    167.47	       145	 10	6	 -4	0	0	0	0	0.5	0	0	#_L_at_Amax_Fem_GP_1 
 0.05	     0.5	      0.48	     0.455	0.8	6	 -4	0	0	0	0	0.5	0	0	#_VonBert_K_Fem_GP_1 
   -5	       5	  0.467316	         1	  1	0	 -1	0	0	0	0	  0	0	0	#_Age_K_2_Fem_GP_1   
  -15	       5	   1.37598	         1	  1	0	 -1	0	0	0	0	  0	0	0	#_Age_K_3_Fem_GP_1   
  -15	       5	   1.26358	         1	  1	0	 -1	0	0	0	0	  0	0	0	#_Age_K_4_Fem_GP_1   
  -15	       5	         1	         1	  1	0	 -1	0	0	0	0	  0	0	0	#_Age_K_5_Fem_GP_1   
  -15	       5	         1	         1	  1	0	 -1	0	0	0	0	  0	0	0	#_Age_K_6_Fem_GP_1   
  -15	       5	         1	         1	  1	0	 -1	0	0	0	0	  0	0	0	#_Age_K_7_Fem_GP_1   
  -15	       5	         1	         1	  1	0	 -1	0	0	0	0	  0	0	0	#_Age_K_8_Fem_GP_1   
  -15	       5	         1	         1	  1	0	 -1	0	0	0	0	  0	0	0	#_Age_K_9_Fem_GP_1   
  -15	       5	         1	         1	  1	0	 -1	0	0	0	0	  0	0	0	#_Age_K_10_Fem_GP_1  
  -15	       5	         1	         1	  1	0	 -1	0	0	0	0	  0	0	0	#_Age_K_11_Fem_GP_1  
  -15	       5	         1	         1	  1	0	 -1	0	0	0	0	  0	0	0	#_Age_K_12_Fem_GP_1  
  -15	       5	         1	         1	  1	0	 -1	0	0	0	0	  0	0	0	#_Age_K_13_Fem_GP_1  
 0.05	    0.25	       0.1	       0.1	0.1	6	 -3	0	0	0	0	0.5	0	0	#_CV_young_Fem_GP_1  
 0.05	    0.25	       0.1	       0.1	0.1	6	 -3	0	0	0	0	0.5	0	0	#_CV_old_Fem_GP_1    
   -3	       3	 2.459e-05	 2.459e-05	0.8	6	 -3	0	0	0	0	0.5	0	0	#_Wtlen_1_Fem_GP_1   
   -3	       4	    2.9667	    2.9667	0.8	6	 -3	0	0	0	0	0.5	0	0	#_Wtlen_2_Fem_GP_1   
   50	     150	     101.7	     101.7	0.8	0	 -3	0	0	0	0	  0	0	0	#_Mat50%_Fem_GP_1    
   -1	       0	-0.0909999	-0.0909999	0.8	6	 -3	0	0	0	0	  0	0	0	#_Mat_slope_Fem_GP_1 
   -3	       3	         1	         1	0.8	6	 -3	0	0	0	0	0.5	0	0	#_Eggs_alpha_Fem_GP_1
   -3	       3	         0	         0	0.8	6	 -3	0	0	0	0	0.5	0	0	#_Eggs_beta_Fem_GP_1 
  0.1	      10	         1	         1	  1	6	 -1	0	0	0	0	  0	0	0	#_CohortGrowDev      
1e-06	0.999999	       0.5	       0.5	0.5	0	-99	0	0	0	0	  0	0	0	#_FracFemale_GP_1    
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
3 #_Spawner-Recruitment; 2=Ricker; 3=std_B-H; 4=SCAA;5=Hockey; 6=B-H_flattop; 7=survival_3Parm;8=Shepard_3Parm
0 # 0/1 to use steepness in initial equ recruitment calculation
0 # future feature: 0/1 to make realized sigmaR a function of SR curvature
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn # parm_name
 -2	25	11.6137	 10	  5	6	  1	0	0	0	0	0	0	0	#_SR_LN(R0)  
0.2	 1	    0.8	0.8	0.2	6	 -1	0	0	0	0	0	0	0	#_SR_BH_steep
  0	 2	    0.6	0.6	0.8	6	 -4	0	0	0	0	0	0	0	#_SR_sigmaR  
 -5	 5	      0	  0	  1	6	 -4	0	0	0	0	0	0	0	#_SR_regime  
  0	 0	      0	  0	  0	0	-99	0	0	0	0	0	0	0	#_SR_autocorr
#_no timevary SR parameters
1 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
101 # first year of main recr_devs; early devs can preceed this era
300 # last year of main recr_devs; forecast devs start in following year
3 #_recdev phase
1 # (0/1) to read 13 advanced options
0 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
-6 #_recdev_early_phase
0 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
1 #_lambda for Fcast_recr_like occurring before endyr+1
10.4 #_last_yr_nobias_adj_in_MPD; begin of ramp
163.4 #_first_yr_fullbias_adj_in_MPD; begin of plateau
303.4 #_last_yr_fullbias_adj_in_MPD
309.1 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
0.7048 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
0 #_period of cycles in recruitment (N parms read below)
-5 #min rec_dev
5 #max rec_dev
0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
#Fishing Mortality info
0.1 # F ballpark
220 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
2.9 # max F or harvest rate, depends on F_Method
4 # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms; count = 0
#
#_Q_setup for fleets with cpue or survey data
#_fleet	link	link_info	extra_se	biasadj	float  #  fleetname
   14	1	 0	0	0	0	#_25_CPUE_LL_1b_P2000
   15	2	14	0	1	0	#_29_CPUE_LL_1b_A2000
-9999	0	 0	0	0	0	#_terminator         
#_Q_parms(if_any);Qunits_are_ln(q)
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn  #  parm_name
-25	25	-8.75207	0	1	0	 1	0	0	0	0	0	0	0	#_LnQ_base_25_CPUE_LL_1b_P2000(25)
-25	25	-8.75207	0	1	0	-1	0	0	0	0	0	0	0	#_LnQ_base_29_CPUE_LL_1b_A2000(29)
#_no timevary Q parameters
#
#_size_selex_patterns
#_Pattern	Discard	Male	Special
 0	0	0	0	#_1 1_GI_1a             
 0	0	0	0	#_2 2_HD_1a             
 0	0	0	0	#_3 3_LL_1a             
 0	0	0	0	#_4 4_OT_1a             
 0	0	0	0	#_5 5_BB_1b             
27	0	0	5	#_6 6_FS_1b             
 0	0	0	0	#_7 7_LL_1b_P2000       
27	0	0	5	#_8 8_LS_1b             
 0	0	0	0	#_9 9_TR_1b             
 0	0	0	0	#_10 12_GI_4            
 0	0	0	0	#_11 14_OT_4            
 0	0	0	0	#_12 21_LF_4            
 0	0	0	0	#_13 22_LL_1b_A2000     
 0	0	0	0	#_14 25_CPUE_LL_1b_P2000
 0	0	0	0	#_15 29_CPUE_LL_1b_A2000
#
#_age_selex_patterns
#_Pattern	Discard	Male	Special
20	0	0	 0	#_1 1_GI_1a             
12	0	0	 0	#_2 2_HD_1a             
12	0	0	 0	#_3 3_LL_1a             
20	0	0	 0	#_4 4_OT_1a             
20	0	0	 0	#_5 5_BB_1b             
 0	0	0	 0	#_6 6_FS_1b             
20	0	0	 0	#_7 7_LL_1b_P2000       
 0	0	0	 0	#_8 8_LS_1b             
20	0	0	 0	#_9 9_TR_1b             
20	0	0	 0	#_10 12_GI_4            
20	0	0	 0	#_11 14_OT_4            
12	0	0	 0	#_12 21_LF_4            
12	0	0	 0	#_13 22_LL_1b_A2000     
15	0	0	 7	#_14 25_CPUE_LL_1b_P2000
15	0	0	13	#_15 29_CPUE_LL_1b_A2000
#
#_SizeSelex
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn  #  parm_name
     0	    2	        0	  0	    0	0	-99	0	0	0	0	0	0	0	#_SizeSel_Spline_Code_6_FS_1b(6)  
-0.001	    1	 0.298034	  0	0.001	1	  3	0	0	0	0	0	0	0	#_SizeSel_Spline_GradLo_6_FS_1b(6)
    -1	0.001	-0.207604	  0	0.001	1	  3	0	0	0	0	0	0	0	#_SizeSel_Spline_GradHi_6_FS_1b(6)
    10	  198	  39.5544	104	    1	0	-99	0	0	0	0	0	0	0	#_SizeSel_Spline_Knot_1_6_FS_1b(6)
    10	  198	  68.6667	104	    1	0	-99	0	0	0	0	0	0	0	#_SizeSel_Spline_Knot_2_6_FS_1b(6)
    10	  198	   114.96	104	    1	0	-99	0	0	0	0	0	0	0	#_SizeSel_Spline_Knot_3_6_FS_1b(6)
    10	  198	  128.036	104	    1	0	-99	0	0	0	0	0	0	0	#_SizeSel_Spline_Knot_4_6_FS_1b(6)
    10	  198	  145.391	104	    1	0	-99	0	0	0	0	0	0	0	#_SizeSel_Spline_Knot_5_6_FS_1b(6)
    -9	    7	 -4.07358	  0	0.001	1	  2	0	0	0	0	0	0	0	#_SizeSel_Spine_Val_1_6_FS_1b(6)  
    -9	    7	 -2.87116	  0	0.001	1	  2	0	0	0	0	0	0	0	#_SizeSel_Spine_Val_2_6_FS_1b(6)  
    -9	    7	       -1	  0	    1	0	-99	0	0	0	0	0	0	0	#_SizeSel_Spine_Val_3_6_FS_1b(6)  
    -9	    7	 0.120809	  0	0.001	1	  2	0	0	0	0	0	0	0	#_SizeSel_Spine_Val_4_6_FS_1b(6)  
    -9	    7	-0.408336	  0	0.001	1	  2	0	0	0	0	0	0	0	#_SizeSel_Spine_Val_5_6_FS_1b(6)  
     0	    2	        0	  0	    0	0	-99	0	0	0	0	0	0	0	#_SizeSel_Spline_Code_8_LS_1b(8)  
-0.001	    1	 0.379831	  0	0.001	1	  3	0	0	0	0	0	0	0	#_SizeSel_Spline_GradLo_8_LS_1b(8)
    -1	0.001	-0.083208	  0	0.001	1	  3	0	0	0	0	0	0	0	#_SizeSel_Spline_GradHi_8_LS_1b(8)
    10	  198	  33.6931	104	    1	0	-99	0	0	0	0	0	0	0	#_SizeSel_Spline_Knot_1_8_LS_1b(8)
    10	  198	   43.808	104	    1	0	-99	0	0	0	0	0	0	0	#_SizeSel_Spline_Knot_2_8_LS_1b(8)
    10	  198	  49.4233	104	    1	0	-99	0	0	0	0	0	0	0	#_SizeSel_Spline_Knot_3_8_LS_1b(8)
    10	  198	  57.2123	104	    1	0	-99	0	0	0	0	0	0	0	#_SizeSel_Spline_Knot_4_8_LS_1b(8)
    10	  198	  121.583	104	    1	0	-99	0	0	0	0	0	0	0	#_SizeSel_Spline_Knot_5_8_LS_1b(8)
    -9	    7	  -3.6519	  0	0.001	1	  2	0	0	0	0	0	0	0	#_SizeSel_Spine_Val_1_8_LS_1b(8)  
    -9	    7	 -1.69255	  0	0.001	1	  2	0	0	0	0	0	0	0	#_SizeSel_Spine_Val_2_8_LS_1b(8)  
    -9	    7	       -1	  0	    1	0	-99	0	0	0	0	0	0	0	#_SizeSel_Spine_Val_3_8_LS_1b(8)  
    -9	    7	 -1.38386	  0	0.001	1	  2	0	0	0	0	0	0	0	#_SizeSel_Spine_Val_4_8_LS_1b(8)  
    -9	    7	 -2.72436	  0	0.001	1	  2	0	0	0	0	0	0	0	#_SizeSel_Spine_Val_5_8_LS_1b(8)  
#_AgeSelex
  1	12	  5.63043	  5.61185	  1.12237	6	 3	0	0	0	0	0	1	2	#_AgeSel_P_1_1_GI_1a(1)        
-20	-2	 -9.70313	 -9.70313	  1.94063	6	-5	0	0	0	0	0	0	0	#_AgeSel_P_2_1_GI_1a(1)        
-10	 9	0.0979589	0.0979244	0.0195849	6	 4	0	0	0	0	0	1	2	#_AgeSel_P_3_1_GI_1a(1)        
  0	 4	   1.7264	  1.80416	 0.360832	6	 4	0	0	0	0	0	0	0	#_AgeSel_P_4_1_GI_1a(1)        
-12	-1	       -6	       -6	      1.2	6	-5	0	0	0	0	0	0	0	#_AgeSel_P_5_1_GI_1a(1)        
 -3	 0	 -1.41578	 -1.43865	  0.28773	6	 5	0	0	0	0	0	0	0	#_AgeSel_P_6_1_GI_1a(1)        
  3	30	  14.6887	   14.539	   2.9078	6	 3	0	0	0	0	0	0	0	#_AgeSel_P_1_2_HD_1a(2)        
  1	11	  5.21918	  5.15861	  1.03172	6	 3	0	0	0	0	0	0	0	#_AgeSel_P_2_2_HD_1a(2)        
  2	20	  9.69985	  9.80198	   1.9604	6	 3	0	0	0	0	0	0	0	#_AgeSel_P_1_3_LL_1a(3)        
  0	 5	  2.01314	  2.10638	 0.421276	6	 3	0	0	0	0	0	0	0	#_AgeSel_P_2_3_LL_1a(3)        
  0	 3	  1.49311	  1.47997	 0.295994	6	 3	0	0	0	0	0	0	0	#_AgeSel_P_1_4_OT_1a(4)        
-20	-2	 -9.70313	 -9.70313	  1.94063	6	-5	0	0	0	0	0	0	0	#_AgeSel_P_2_4_OT_1a(4)        
 -6	 0	 -2.79654	 -2.84194	 0.568388	6	 4	0	0	0	0	0	0	0	#_AgeSel_P_3_4_OT_1a(4)        
  0	 7	  3.31163	  3.30019	 0.660038	6	 4	0	0	0	0	0	0	0	#_AgeSel_P_4_4_OT_1a(4)        
-12	-1	       -6	       -6	      1.2	6	-5	0	0	0	0	0	0	0	#_AgeSel_P_5_4_OT_1a(4)        
 -6	 0	 -2.68343	 -2.74022	 0.548044	6	 5	0	0	0	0	0	0	0	#_AgeSel_P_6_4_OT_1a(4)        
  0	 3	  1.27875	  1.30241	 0.260482	6	 3	0	0	0	0	0	0	0	#_AgeSel_P_1_5_BB_1b(5)        
-20	-2	 -9.70313	 -9.70313	  1.94063	6	-5	0	0	0	0	0	0	0	#_AgeSel_P_2_5_BB_1b(5)        
 -8	 0	 -3.78399	 -3.57928	 0.715856	6	 4	0	0	0	0	0	0	0	#_AgeSel_P_3_5_BB_1b(5)        
  0	 2	 0.697813	 0.700187	 0.140037	6	 4	0	0	0	0	0	0	0	#_AgeSel_P_4_5_BB_1b(5)        
-12	-1	       -6	       -6	      1.2	6	-5	0	0	0	0	0	0	0	#_AgeSel_P_5_5_BB_1b(5)        
 -8	 0	 -3.69937	 -3.65979	 0.731958	6	 5	0	0	0	0	0	0	0	#_AgeSel_P_6_5_BB_1b(5)        
  2	19	  9.99974	  9.19558	  1.83912	6	 3	0	0	0	0	0	0	0	#_AgeSel_P_1_7_LL_1b_P2000(7)  
 -2	 0	-0.983702	    -0.85	     0.17	6	-5	0	0	0	0	0	0	0	#_AgeSel_P_2_7_LL_1b_P2000(7)  
 -8	 0	 -5.64064	 -5.64112	  1.12822	6	 4	0	0	0	0	0	0	0	#_AgeSel_P_3_7_LL_1b_P2000(7)  
-20	 0	 -18.9999	 -19.0136	  3.80272	6	 4	0	0	0	0	0	0	0	#_AgeSel_P_4_7_LL_1b_P2000(7)  
-12	-1	       -6	       -6	      1.2	6	-5	0	0	0	0	0	0	0	#_AgeSel_P_5_7_LL_1b_P2000(7)  
-32	-3	 -15.6193	  -15.619	   3.1238	6	 5	0	0	0	0	0	0	0	#_AgeSel_P_6_7_LL_1b_P2000(7)  
  0	 3	  1.44017	  1.42679	 0.285358	6	 3	0	0	0	0	0	0	0	#_AgeSel_P_1_9_TR_1b(9)        
-20	-2	 -9.70313	 -9.70313	  1.94063	6	-5	0	0	0	0	0	0	0	#_AgeSel_P_2_9_TR_1b(9)        
 -6	 0	 -2.49856	 -2.50354	 0.500708	6	 4	0	0	0	0	0	0	0	#_AgeSel_P_3_9_TR_1b(9)        
  0	 3	   1.4326	  1.39785	  0.27957	6	 4	0	0	0	0	0	0	0	#_AgeSel_P_4_9_TR_1b(9)        
-12	-1	       -6	       -6	      1.2	6	-5	0	0	0	0	0	0	0	#_AgeSel_P_5_9_TR_1b(9)        
 -1	 0	 -0.14017	-0.135498	0.0270996	6	 5	0	0	0	0	0	0	0	#_AgeSel_P_6_9_TR_1b(9)        
  1	 9	  4.28971	  4.09681	 0.819362	6	 3	0	0	0	0	0	0	0	#_AgeSel_P_1_12_GI_4(12)       
-20	-2	 -9.70313	 -9.70313	  1.94063	6	-5	0	0	0	0	0	0	0	#_AgeSel_P_2_12_GI_4(12)       
  0	 2	 0.818752	  0.75502	 0.151004	6	 4	0	0	0	0	0	0	0	#_AgeSel_P_3_12_GI_4(12)       
  0	 3	  1.54717	  1.26419	 0.252838	6	 4	0	0	0	0	0	0	0	#_AgeSel_P_4_12_GI_4(12)       
-12	-1	       -6	       -6	      1.2	6	-5	0	0	0	0	0	0	0	#_AgeSel_P_5_12_GI_4(12)       
 -3	 0	 -1.79516	 -1.40784	 0.281568	6	 5	0	0	0	0	0	0	0	#_AgeSel_P_6_12_GI_4(12)       
  0	 1	 0.293537	 0.292121	0.0584242	6	 3	0	0	0	0	0	0	0	#_AgeSel_P_1_14_OT_4(14)       
-20	-2	 -9.70313	 -9.70313	  1.94063	6	-5	0	0	0	0	0	0	0	#_AgeSel_P_2_14_OT_4(14)       
-13	-1	 -6.00022	 -6.00014	  1.20003	6	 4	0	0	0	0	0	0	0	#_AgeSel_P_3_14_OT_4(14)       
  0	 8	  3.89683	  3.85698	 0.771396	6	 4	0	0	0	0	0	0	0	#_AgeSel_P_4_14_OT_4(14)       
-12	-1	       -6	       -6	      1.2	6	-5	0	0	0	0	0	0	0	#_AgeSel_P_5_14_OT_4(14)       
 -2	 0	-0.534826	-0.505378	 0.101076	6	 5	0	0	0	0	0	0	0	#_AgeSel_P_6_14_OT_4(14)       
  2	18	  7.02079	  8.77396	  1.75479	6	 3	0	0	0	0	0	0	0	#_AgeSel_P_1_21_LF_4(21)       
  0	 8	  2.78531	  3.84229	 0.768458	6	 3	0	0	0	0	0	0	0	#_AgeSel_P_2_21_LF_4(21)       
  3	26	  12.7539	  12.5375	   2.5075	6	 3	0	0	0	0	0	0	0	#_AgeSel_P_1_22_LL_1b_A2000(22)
  0	 8	  3.94288	  3.77676	 0.755352	6	 3	0	0	0	0	0	0	0	#_AgeSel_P_2_22_LL_1b_A2000(22)
# timevary selex parameters 
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE
  1	12	 5.29628	 7	3	6	3	#_AgeSel_P_1_1_GI_1a(1)_BLK1repl_213
  1	12	 5.10876	 7	3	6	3	#_AgeSel_P_1_1_GI_1a(1)_BLK1repl_261
-10	 9	0.963382	-1	3	6	4	#_AgeSel_P_3_1_GI_1a(1)_BLK1repl_213
-10	 9	-1.16967	-1	3	6	4	#_AgeSel_P_3_1_GI_1a(1)_BLK1repl_261
# info on dev vectors created for selex parms are reported with other devs after tag parameter section
#
0 #  use 2D_AR1 selectivity(0/1):  experimental feature
#_no 2D_AR1 selex offset used
# Tag loss and Tag reporting parameters go next
1 # TG_custom:  0=no read; 1=read if tags exist
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env_var&link	dev_link	dev_minyr	dev_maxyr	dev_PH	Block	Block_Fxn
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_1  
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_2  
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_3  
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_4  
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_5  
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_6  
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_7  
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_8  
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_9  
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_10 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_11 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_12 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_13 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_14 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_15 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_16 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_17 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_18 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_19 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_20 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_21 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_22 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_23 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_24 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_25 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_26 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_27 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_28 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_29 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_30 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_31 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_32 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_33 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_34 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_35 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_36 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_37 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_38 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_39 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_40 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_41 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_42 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_43 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_44 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_45 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_46 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_47 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_48 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_49 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_50 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_51 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_52 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_53 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_54 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_55 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_56 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_57 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_58 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_59 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_60 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_61 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_62 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_63 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_64 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_65 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_66 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_67 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_68 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_69 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_70 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_71 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_72 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_73 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_74 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_75 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_76 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_77 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_78 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_79 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_80 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_81 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_82 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_83 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_84 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_85 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_86 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_87 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_88 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_89 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_90 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_91 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_92 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_93 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_94 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_95 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_96 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_97 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_98 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_99 
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_100
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_101
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_102
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_103
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_104
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_105
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_106
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_107
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_108
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_109
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_110
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_111
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_112
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_113
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_114
-15	10	-10	-10	0.001	1	-4	0	0	0	0	0	0	0	#__TG_Loss_init_115
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_1  
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_2  
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_3  
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_4  
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_5  
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_6  
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_7  
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_8  
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_9  
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_10 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_11 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_12 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_13 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_14 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_15 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_16 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_17 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_18 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_19 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_20 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_21 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_22 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_23 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_24 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_25 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_26 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_27 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_28 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_29 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_30 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_31 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_32 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_33 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_34 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_35 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_36 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_37 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_38 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_39 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_40 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_41 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_42 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_43 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_44 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_45 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_46 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_47 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_48 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_49 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_50 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_51 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_52 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_53 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_54 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_55 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_56 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_57 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_58 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_59 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_60 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_61 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_62 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_63 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_64 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_65 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_66 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_67 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_68 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_69 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_70 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_71 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_72 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_73 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_74 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_75 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_76 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_77 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_78 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_79 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_80 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_81 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_82 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_83 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_84 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_85 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_86 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_87 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_88 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_89 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_90 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_91 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_92 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_93 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_94 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_95 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_96 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_97 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_98 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_99 
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_100
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_101
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_102
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_103
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_104
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_105
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_106
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_107
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_108
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_109
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_110
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_111
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_112
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_113
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_114
-15	10	-3.5	-3.5	0.001	1	-4	0	0	0	0	0	0	0	#_TG_Loss_chronic_115
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_1  
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_2  
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_3  
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_4  
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_5  
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_6  
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_7  
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_8  
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_9  
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_10 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_11 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_12 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_13 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_14 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_15 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_16 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_17 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_18 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_19 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_20 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_21 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_22 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_23 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_24 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_25 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_26 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_27 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_28 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_29 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_30 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_31 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_32 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_33 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_34 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_35 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_36 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_37 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_38 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_39 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_40 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_41 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_42 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_43 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_44 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_45 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_46 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_47 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_48 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_49 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_50 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_51 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_52 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_53 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_54 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_55 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_56 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_57 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_58 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_59 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_60 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_61 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_62 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_63 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_64 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_65 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_66 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_67 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_68 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_69 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_70 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_71 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_72 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_73 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_74 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_75 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_76 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_77 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_78 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_79 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_80 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_81 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_82 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_83 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_84 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_85 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_86 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_87 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_88 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_89 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_90 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_91 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_92 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_93 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_94 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_95 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_96 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_97 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_98 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_99 
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_100
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_101
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_102
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_103
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_104
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_105
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_106
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_107
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_108
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_109
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_110
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_111
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_112
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_113
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_114
1	10	7	7	0.001	1	-4	0	0	0	0	0	0	0	#_TG_overdispersion_115
-10	10	-3.53286	 -2.81911	2	6	 6	0	0	0	0	0	0	0	#_TG_report_fleet_par_1 
-10	10	-3.59313	 -6.05099	2	6	 6	0	0	0	0	0	0	0	#_TG_report_fleet_par_2 
-10	10	-4.31831	  -1.5506	2	6	 6	0	0	0	0	0	0	0	#_TG_report_fleet_par_3 
-10	10	  4.4356	  4.59512	2	6	 6	0	0	0	0	0	0	0	#_TG_report_fleet_par_4 
-10	10	-4.60182	 -2.81161	2	6	 6	0	0	0	0	0	0	0	#_TG_report_fleet_par_5 
-20	20	      10	       10	2	6	-6	0	0	0	0	0	0	0	#_TG_report_fleet_par_6 
-10	10	 1.43063	  1.43063	2	6	 6	0	0	0	0	0	0	0	#_TG_report_fleet_par_7 
-20	20	      10	       10	2	6	-6	0	0	0	0	0	0	0	#_TG_report_fleet_par_8 
-10	10	-3.48132	0.0640219	2	6	 6	0	0	0	0	0	0	0	#_TG_report_fleet_par_9 
-10	10	-5.98992	 -5.43322	2	6	 6	0	0	0	0	0	0	0	#_TG_report_fleet_par_12
-10	10	-3.35606	  1.53681	2	6	 6	0	0	0	0	0	0	0	#_TG_report_fleet_par_14
-10	10	-5.55109	  1.43063	2	6	 6	0	0	0	0	0	0	0	#_TG_report_fleet_par_21
-10	10	-1.80951	 -2.34957	2	6	 6	0	0	0	0	0	0	0	#_TG_report_fleet_par_22
-4	0	0	0	2	6	-4	0	0	0	0	0	0	0	#_TG_report_decay_par_1 
-4	0	0	0	2	6	-4	0	0	0	0	0	0	0	#_TG_report_decay_par_2 
-4	0	0	0	2	6	-4	0	0	0	0	0	0	0	#_TG_report_decay_par_3 
-4	0	0	0	2	6	-4	0	0	0	0	0	0	0	#_TG_report_decay_par_4 
-4	0	0	0	2	6	-4	0	0	0	0	0	0	0	#_TG_report_decay_par_5 
-4	0	0	0	2	6	-4	0	0	0	0	0	0	0	#_TG_report_decay_par_6 
-4	0	0	0	2	6	-4	0	0	0	0	0	0	0	#_TG_report_decay_par_7 
-4	0	0	0	2	6	-4	0	0	0	0	0	0	0	#_TG_report_decay_par_8 
-4	0	0	0	2	6	-4	0	0	0	0	0	0	0	#_TG_report_decay_par_9 
-4	0	0	0	2	6	-4	0	0	0	0	0	0	0	#_TG_report_decay_par_12
-4	0	0	0	2	6	-4	0	0	0	0	0	0	0	#_TG_report_decay_par_14
-4	0	0	0	2	6	-4	0	0	0	0	0	0	0	#_TG_report_decay_par_21
-4	0	0	0	2	6	-4	0	0	0	0	0	0	0	#_TG_report_decay_par_22
# Input variance adjustments factors: 
#_Data_type Fleet Value
-9999 1 0 # terminator
#
4 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 2 changes to default Lambdas (default value is 1.0)
#_like_comp	fleet	phase	value	sizefreq_method
   17	1	1	   1	1	#_F-ballpark_1_GI_1a_Phz1
   17	1	4	0.01	1	#_F-ballpark_1_GI_1a_Phz4
-9999	0	0	   0	0	#_terminator             
#
0 # 0/1 read specs for more stddev reporting
#
999

# EE232E_Hw3

Arunav Singh (UID: 304 760 844)
Eric Goldfien (UID: 603 887 003)
Steven Leung (UID: 304 777 142)

########################################################
This homework was programmed in R v2.15.2 with iGraph v0.7.0 and netrw v0.2.6

* The file "HW3.R" contains all code related to this assignment

* Question 6: There are two parameters that can be set to change the simulation
	*THRESHOLD: use to set threhsold to remove membershiops that have very small values in M_i
	
	*OPTION: Use to set which option of communition create you would like to simulate 
		OPTION = community_1 (option 1 with label.propagation.community)
		OPTION = community_2a (option 2 with label.propagation.community)
		OPTION = community_2b (option 2 with fastgreedy.community)
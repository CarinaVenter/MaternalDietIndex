/*********************************************************************************************
Program: ComputeIndexScoreMacro
Purpose: SAS macro that takes an input dataset of consumption frequencies for the intake of 
		 the 7 diet index components and computes maternal diet index and outputs dataset 
		 including the computed maternal diet index.
**********************************************************************************************/

/*********************************************
Macro to compute index scores
**********************************************/

/*Macro use requirements:
	~ SAS dataset containing at least the following 8 variables 
	  	1. participant id
		2-8. intake frequencies of:
			 ~ vegetables (not incl. salad, potatoes, or beans), 
			 ~ yogurt (not incl. frozen yogurt), 
			 ~ fried potatoes (french fries, tater tots, etc.),
			 ~ rice/cooked grains (not incl. pasta or bread), 
			 ~ 100% pure fruit juice (not incl. aritificial fruit drinks like Hi-C, lemonade, etc.), 
			 ~ red meats, 
			 ~ cold cereals (not incl. oatmeal or cooked cereals)
	~ the units for frequency of intake variables must be the same for all of the 7 index component variables
	  and must be in units of either: 
			 ~ number of times consumed per day
			 ~ number of times consumed per month
			 ~ number of times consumed per year
	~ argument to the Units input should be given in quotations as either: "Days", "Weeks", or "Months" (case-sensitive)
	~ character arguments to the rest of the inputs do not need to be in quotations
	~ complete data for all 7 diet index components is required to compute maternal diet index score
	~ see example use of macro for directions

Macro inputs:
	~ indat: name of input SAS dataset that includes at least the 8 variables specified above (participant id plus 
			 intake frequency for each of the 7 components of the intake)
	~ idLab: name of participant id variable
	~ VegLab: name of vegetable intake variable
	~ YogLab: name of yogurt intake variable
	~ FriesLab: name of fried potatoes intake variable
	~ RiceLab: name of rice/grains intake variable
	~ JuiceLab: name of 100% pure fruit juice intake variable
	~ RedMeatLab: name of red meat intake variable
	~ CerealLab: name of cold cereal intake variable
	~ Units: units for intake variables (options = "Days" (for number of times per day), 
			 "Weeks" (for number of times per week), "Months" (for number of times per month)
	~ outdat: desired name for output SAS dataset which will contain all data from the input dataset PLUS a variable
		      that contains the computed diet index scores
*/

/*Begin ComputeDietIndex macro*/
%macro ComputeDietIndex(indat=, idLab=, VegLab=, YogLab=, FriesLab=, RiceLab=, JuiceLab=, RedMeatLab=, CerealLab=, Units=, outdat=);
	/*Suppress notes to log for intermediate work being done in macro*/
	options nonotes;
	
	/*Convert units to number of times per day if they are not already*/
	/*Create intermediate dataset 1.*/
	%if &Units. = "Days" %then
		%do;
			data interim1;
				set &indat.;

				/*No conversion needed. Keep the intake frequency variables the same*/
				&VegLab. = &VegLab.;
				&YogLab. = &YogLab.;
				&FriesLab. = &FriesLab.;
				&RiceLab. = &RiceLab.;
				&JuiceLab. = &JuiceLab.;
				&RedMeatLab. = &RedMeatLab.;
				&CerealLab. = &CerealLab.;
			run;
		%end;
	%else %if &Units. = "Weeks" %then
		%do;
			data interim1;
				set &indat.;

				/*Convert to units of number of times/day using the conversion: y times/dy = x times/wk*(52 wks/365 dys)*/
				&VegLab. = &VegLab.*(52/365);
				&YogLab. = &YogLab.*(52/365);
				&FriesLab. = &FriesLab.*(52/365);
				&RiceLab. = &RiceLab.*(52/365);
				&JuiceLab. = &JuiceLab.*(52/365);
				&RedMeatLab. = &RedMeatLab.*(52/365);
				&CerealLab. = &CerealLab.*(52/365);
			run;
		%end;
	%else %if &Units. = "Months" %then
		%do;
			data interim1;
				set &indat.;

				/*Convert to units of number of times/day using the conversion: y times/dy = x times/mo*(12 mos/365 dys)*/
				&VegLab. = &VegLab.*(12/365);
				&YogLab. = &YogLab.*(12/365);
				&FriesLab. = &FriesLab.*(12/365);
				&RiceLab. = &RiceLab.*(12/365);
				&JuiceLab. = &JuiceLab.*(12/365);
				&RedMeatLab. = &RedMeatLab.*(12/365);
				&CerealLab. = &CerealLab.*(12/365);
			run;
		%end;
	%else
		%do;
			/*Put error statment in log with %put*/
			%put %str(ERROR: INVALID INPUT FOR ARGUMENT "Units");
		%end;
	
	/*Cap maximum daily consumption at 13 times per day*/
	/*Create intermediate dataset 2.*/
	data interim2;
		set interim1;

		if &VegLab. >= 13 then &VegLab. = 13;
		else &VegLab. = &VegLab.;

		if &YogLab. >= 13 then &YogLab. = 13;
		else &YogLab. = &YogLab.;

		if &FriesLab. >= 13 then &FriesLab. = 13;
		else &FriesLab. = &FriesLab.;

		if &RiceLab. >= 13 then &RiceLab. = 13;
		else &RiceLab. = &RiceLab.;

		if &JuiceLab. >= 13 then &JuiceLab. = 13;
		else &JuiceLab. = &JuiceLab.;

		if &RedMeatLab. >= 13 then &RedMeatLab. = 13;
		else &RedMeatLab. = &RedMeatLab.;

		if &CerealLab. >= 13 then &CerealLab. = 13;
		else &CerealLab. = &CerealLab.;
	run;

	/*Compute raw index score and scaled index score*/
	data interim3;
		set interim2;

		/*RawScore*/
		if (&VegLab. = .) OR (&YogLab. = .) OR (&FriesLab. = .) OR (&RiceLab. = .)
		    OR (&JuiceLab. = .) OR (&RedMeatLab. = .) OR (&CerealLab. = . ) then RawScore = .;
		else RawScore = 33.0108 /*Beta intercept = 33.0108*/
				   		+ (-0.4106)*&VegLab. /*Beta veg = -0.4106*/
					   	+ (-0.5590)*&YogLab. /*Beta yog = -0.5590*/
					   	+ (-0.8566)*(13 - &FriesLab.) /*Beta fries = -0.8566*/
					    + (-0.5752)*(13 - &RiceLab.) /*Beta rice = -0.5752*/
					    + (-0.1870)*(13 - &JuiceLab.) /*Beta juice = -0.1870*/
					    + (-0.5793)*(13 - &RedMeatLab.) /*Beta red meat = -0.5793*/
					    + (-0.4406)*(13 - &CerealLab.) /*Beta cereal = -0.4406*/
					    ;

		/*Scaled Score*/
		/*First make variables of theoretical min and max raw index scores*/
		/*Theoretical min raw score (healthiest score): eating max of healthy and none of unhealthy items*/
		MinScore = 33.0108 /*Beta intercept = 33.0108*/
				   + (-0.4106)*13 /*Beta veg = -0.4106*/
				   + (-0.5590)*13 /*Beta yog = -0.5590*/
				   + (-0.8566)*(13 - 0) /*Beta fries = -0.8566*/
				   + (-0.5752)*(13 - 0) /*Beta rice = -0.5752*/
				   + (-0.1870)*(13 - 0) /*Beta juice = -0.1870*/
				   + (-0.5793)*(13 - 0) /*Beta red meat = -0.5793*/
				   + (-0.4406)*(13 - 0) /*Beta cereal = -0.4406*/
				   ;

		/*Theoretical max raw score (unhealthiest score): eating min of healthy and max of unhealthy items*/
		MaxScore = 33.0108 /*Beta intercept = 33.0108*/
				   + (-0.4106)*0 /*Beta veg = -0.4106*/
				   + (-0.5590)*0 /*Beta yog = -0.5590*/
				   + (-0.8566)*(13 - 13) /*Beta fries = -0.8566*/
				   + (-0.5752)*(13 - 13) /*Beta rice = -0.5752*/
				   + (-0.1870)*(13 - 13) /*Beta juice = -0.1870*/
				   + (-0.5793)*(13 - 13) /*Beta red meat = -0.5793*/
				   + (-0.4406)*(13 - 13) /*Beta cereal = -0.4406*/
				   ;		

		/*Equation for computing scaled index score*/
		if RawScore = . then IndexScore = .;
		else IndexScore = (100*(RawScore - MaxScore))/(MinScore - MaxScore);

		label IndexScore = "Maternal diet index score";
	run;

	/*Create final dataset with only input dataset variables and the final index score*/
	proc sort data = &indat.;
		by &idLab.;
	run;

	proc sort data = interim3;
		by &idLab.;
	run;

	options notes; /*turn notes back on to see note about number of observations and variables in final output dataset*/
	data &outdat.;
		merge &indat.
			  interim3 (keep = &idLab. IndexScore);
		by &idLab.;
	run;

	options nonotes; /*turn notes back off for deleting intermediate datasets*/
	/*Remove interim datasets from working library*/
	proc datasets library = work nolist nodetails;
		delete interim1 interim2 interim3;
	run;
	quit;

	options notes; /*turn notes back on*/
	
%mend; /*End ComputeDietIndex macro*/
		     
/************************************************************************************************************/

/*TURN OFF PROC PRINTTO*/
proc printto;
run;

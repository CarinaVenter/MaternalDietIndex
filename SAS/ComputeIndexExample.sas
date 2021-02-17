/*********************************************************************************************
Program: ComputeIndexExample
Purpose: Example program that shows how to use the SAS macro with the example dataset of reported
		 weekly intake frequencies for 10 participants to compute maternal diet index. 
**********************************************************************************************/

/*Run macro code (see the file ComputeIndexScoreMacro.sas for more details)*/
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
	
%mend;

/*Import example dataset*/
/*set libname with file path pointing to location where the example SAS dataset is saved*/
libname in01 "Desktop";

data inputdat;
	set in01.ExampleData;
run;

/*Run the macro for the input dataset, providing the variable names and the appropriate units for the dataset.
  Requests that the output dataset be named ExampleOut*/
%ComputeDietIndex(indat=inputdat, idLab=PID, VegLab=Veg, YogLab=Yog, FriesLab=Fries, RiceLab=Rice, 
				  JuiceLab=Juice, RedMeatLab=Meat, CerealLab=Cereal, Units="Weeks", outdat=ExampleOut);

/*Examine the output dataset*/
proc print data = ExampleOut;
run;

proc contents data = ExampleOut;
run;

/************************************************************************************************************/

/*TURN OFF PROC PRINTTO*/
proc printto;
run;

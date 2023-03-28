IMLS 2017 PUBLIC LIBRARIES SURVEY DATA FILES -- PUBLIC-USE FILES INSTRUCTIONS

This file provides details about the FY 2017 PUBLIC LIBRARIES SURVEY public-use data files. 
Appendix A provides the SAS codes needed to recode negative values into missing. 


DESCRIPTION OF FILES
____________________

  The Public Libraries Survey data are composed of three files: 
	Public Library System Data File (Administrative Entity or AE),
  	Public Library State Summary/State Characteristics Data File, and 
	Public Library Outlet Data File. 
  

  1.	Public Library System Data File. This file, also known as the Administrative Entity or AE file, includes a total of 9,245 
	records. Each library’s data consist of one record. The file includes 29 records for administrative entities that were reported
	as closed or were temporarily closed for FY 2017 (STATSTRU, Structure Change Code, is ‘03’ or ‘23’). Records for public libraries
	that were closed for the current year are included in the file for that year only. The closed records are not included in the 
	appendix tables of the data documentation or the Supplementary Tables.  Data for the closed records are set to a value of -3
	(closed or temporarily closed administrative entity), with flag U_17.    


  2.	Public Library State Summary/State Characteristics Data File includes one record (a total of 54 records are in the data file)
	for each state and outlying area. All libraries, including those that do not conform to the FSCS definition 
	of a public library, are included in the aggregate counts on the State Summary/State Characteristics Data File. For this reason, 
	the Public Library System Data File is the primary source for producing the publication tables because libraries that do not meet 
	the FSCS definition can be excluded from the aggregations. The State Summary/State Characteristics file includes:

	a.	State summary data. These are the totals of the numeric data from the public-use Public Library System Data File 
		for each state and outlying area.  

	b.	State characteristics data. These data consist of four items reported by each state and outlying area in a 
		“state characteristics” record: (1) the earliest reporting period starting date and (2) the latest reporting period 
		ending date for their public libraries, (3) the state population estimate, and (4) the total unduplicated population 
		of legal service areas in the state. 

  3.	Public Library Outlet Data File includes a total of 17,452 total records. The file includes identifying information and a few 
	basic data items for public library service outlets (central, branch, bookmobile, and books-by-mail-only outlets). The data for 
	each outlet consist of one record, except where a single outlet represents multiple bookmobiles. For these outlets, a single record
	includes information for all bookmobiles and the variable L_NUM_BM indicates the number of associated bookmobiles.
	
	The file includes 96 records for outlets that were reported as closed or were temporarily closed for FY 2017 (STATSTRU, Structure 
	Change Code, is ‘03’ or ‘23’). Records for public libraries that were closed for the current year are included in the file for that year
	only. The closed records are not included in the appendix tables of the data documentation or the Supplementary Tables. Data 
	for the closed records are set to a value of -3 (closed or temporarily closed outlet), with flag U_17.  	


Public-Use Data Files and Programs
______________________________________

  In the public-use Public Library System Data File, selected expenditures data (i.e., Salaries, Employee Benefits, Total 
  Staff Expenditures, and Other Operating Expenditures) of public libraries have been removed (i.e., the field is set 
  to -9, with flag H_17), when the total FTE staff is less than or equal to 2.00, to protect confidentiality. 
  These data may also be suppressed for other libraries to ensure that all states that have suppressed data have a minimum of 
  three suppressed records. The library’s Total Operating Expenditures and Other Expenditures Data are not affected by the suppression 
  of these data. No data are suppressed in the public-use versions of the Public Library State Summary/State Characteristics 
  Data File or Public Library Outlet Data File.  
	
  Missing data are imputed on the public-use files. 

  For each file, both unimputed and imputed versions are provided, and available in three file types: CSV, SAS, and SPSS.

  For SAS users, data format programs are provided. For SPSS users, data formats are included in the data files. 

 
     Public Library System Data File
     --------------------------------

	Unimputed data Contains 9,245 records and 105 variables. Files include:

	pls_ae_pud17.csv - 2017 unimputed AE file in csv format
     	pls_ae_pud17.sas7bdat - 2017 unimputed AE file in SAS format
   	pls_ae_pud17.sav - 2017 unimputed AE file in SPSS format

	SAS_pls_ae_pud17_FmtAssoc.sas - a SAS program to create value labels to the unimputed public-use SAS file
	SAS_pls_ae_pud17_FmtAttach.sas - a SAS program to assign value labels to the unimputed public-use SAS file


	Imputed data Contains 9,245 records and 157 variables. Files include:

	pls_ae_pud17i.csv - 2017 imputed AE file in csv format
     	pls_ae_pud17i.sas7bdat - 2017 imputed AE file in SAS format
   	pls_ae_pud17i.sav - 2017 imputed AE file in SPSS format

	SAS_pls_ae_pud17i_FmtAssoc.sas - a SAS program to create value labels to the imputed public-use SAS file
	SAS_pls_ae_pud17i_FmtAttach.sas - a SAS program to assign value labels to the imputed public-use SAS file


     Public Library State Summary/State Characteristics Data File
     -------------------------------------------------------------

	Unimputed data Contains 54 records and 68 variables. Files include:

	pls_state_pud17.csv - 2017 unimputed summary file in csv format
     	pls_state_pud17.sas7bdat - 2017 unimputed summary file in SAS format
   	pls_state_pud17.sav - 2017 unimputed summary file in SPSS format

	SAS_pls_state_pud17_FmtAssoc.sas - a SAS program to create value labels to the unimputed public-use SAS file
	SAS_pls_state_pud17_FmtAttach.sas - a SAS program to assign value labels to the unimputed public-use SAS file


	Imputed data Contains 54 records and 120 variables. Files include:

	pls_state_pud17i.csv - 2017 imputed summary file in csv format
     	pls_state_pud17i.sas7bdat - 2017 imputed summary file in SAS format
   	pls_state_pud17i.sav - 2017 imputed summary file in SPSS format

	SAS_pls_state_pud17i_FmtAssoc.sas - a SAS program to create value labels to the imputed public-use SAS file
	SAS_pls_state_pud17i_FmtAttach.sas - a SAS program to assign value labels to the imputed public-use SAS file


     Public Library Outlet Data File
     --------------------------------

	Unimputed data Contains 17,452 records and 35 variables. Files include:

	pls_outlet_pud17.csv - 2017 unimputed outlet file in csv format
     	pls_outlet_pud17.sas7bdat - 2017 unimputed outlet file in SAS format
   	pls_outlet_pud17.sav - 2017 unimputed outlet file in SPSS format

	SAS_pls_outlet_pud17_FmtAssoc.sas - a SAS program to create value labels to the unimputed public-use SAS file
	SAS_pls_outlet_pud17_FmtAttach.sas - a SAS program to assign value labels to the unimputed public-use SAS file


	Imputed data Contains 17,452 records and 36 variables. Files include:

	pls_outlet_pud17i.csv - 2017 imputed outlet file in csv format
     	pls_outlet_pud17i.sas7bdat - 2017 imputed outlet file in SAS format
   	pls_outlet_pud17i.sav - 2017 imputed outlet file in SPSS format

	SAS_pls_outlet_pud17i_FmtAssoc.sas - a SAS program to create value labels to the imputed public-use SAS file
	SAS_pls_outlet_pud17i_FmtAttach.sas - a SAS program to assign value labels to the imputed public-use SAS file




  Documentation and Related Files
  ________________________________

  The following documentation is provided with the files:

  	Documentation - Survey documentation for Fiscal Year 2017 Public Libraries Survey in PDF format


 
*********************************************************************************************************************************
Appendix A: The code below is for SAS users to recode negative values into missing in SAS.


Recoding Negative Values to Missing in SAS
___________________________________________

*------------------------------------------*
|    For Public Library System Data File   |
*------------------------------------------*
*Insert this section into data step;

array num _numeric_; 
do over num;
if num = -1 then num = .M; /*recode missing value into .M*/
if num = -3 and STATSTRU in ('03', '23') then num = .C; /*recode "Closed and Temporary Closed Library" into .C*/
if num = -4 then num = .N; /*recode "Not Applicable" into .N*/
if num = -9 then num = .S; /*recode suppressed value into .S*/
end;
array char _character_;
do over char;
if char ='M' then char = ' '; /*recode missing value into M for character variables*/
end;
/*recode the rest of special missing into corresponding missing values*/
if PHONE in ('-3','-4') then PHONE = ' '; 
if STARTDAT = '-3' then STARTDAT = '';
if ENDDATE = '-3' then ENDDATE = '';



*------------------------------------------------------------------*
|   For Public Library State Summary/State Characteristics files   |
*------------------------------------------------------------------*
*Insert this section into data step;

array num _numeric_;
do over num;
if num = -1 then num = .M; /*recode missing value into .M*/
end;
array char _character_;
do over char;
if char = 'M' then char = ' '; /*recode missing value into M for character variables*/
end;


*---------------------------------------*
|  For Public Library Outlet Data File  |
*---------------------------------------*
*Insert this section into data step;

array num _numeric_; 
do over num;
if num = -1 then num = .M; /*recode missing value into .M*/
if num = -3 and STATSTRU in ('03', '23') then num = .C; /*recode "Closed and Temporary Closed Library" into .C*/
if num = -4 then num = .N; /*recode "Not Applicable" into .N*/
end;
array char _character_;
do over char;
if char ='M' then char = ' '; /*recode missing value into M for character variables*/
end;
/*recode the rest of special missing into corresponding missing values*/
if PHONE in ('-3','-4') then PHONE = ' '; 







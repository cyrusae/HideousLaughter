IMLS 2015 PUBLIC LIBRARIES SURVEY DATA FILES -- PUBLIC-USE FILES INSTRUCTIONS

This file provides details about the FY 2015 PUBLIC LIBRARIES SURVEY public-use data files.
Appendix A provides the SAS codes needed to recode negative values into missing. 


DESCRIPTION OF FILES
____________________

  
  Public-Use Data Files and Programs
  __________________________________

  The Public Libraries Survey data are composed of three files: Public Library Data File (Administrative Entity),
  Public Library State Summary/State Characteristics Data File, and Public Library Outlet Data File. 

  Missing data are imputed on the public-use files. 

  In the public-use Public Library Data File, selected expenditures data (i.e., Salaries, Employee Benefits, Total 
  Staff Expenditures, and Other Operating Expenditures) of public libraries have been removed (i.e., the field is set 
  to -9) when the total FTE staff is less than or equal to 2.00, to protect confidentiality. These data may also be 
  suppressed for other libraries to ensure that all states that have suppressed data have a minimum of three suppressed 
  records. The library’s Total Operating Expenditures and Other Expenditures Data are not affected by the suppression 
  of these data. No data are suppressed in the public-use versions of the Public Library State Summary/State Characteristics 
  Data File or Public Library Outlet Data File.
  
  For each file, three file types are provided: CSV, SAS, and SPSS.

  For SAS users, data format programs are provided. For SPSS users, data formats are included in the data files. 

 
     Public Library Data File
     --------------------------

	Contains 9,251 records and 159 variables. Files include:

	pupld15a.csv - 2015 imputed suppressed AE file in csv format
     	pupld15a.sas7bdat - 2015 imputed suppressed AE file in SAS format
   	pupld15a.spss - 2015 imputed suppressed AE file in SPSS format

	SAS_pupld15a_FmtAssoc.sas - a SAS program to create value labels to the public-use SAS file
	SAS_pupld15a_FmtAttach.sas - a SAS program to assign value labels to the public-use SAS file


     Public Library State Summary/State Characteristics Data File
     ------------------------------------------------------------

	Contains 53 records and 124 variables. Files include:	

	pusum15a.csv - 2015 imputed suppressed summary file in csv format
    	pusum15a.sas7bdat - 2015 imputed suppressed summary file in SAS format
    	pusum15a.spss - 2015 imputed suppressed summary file in SPSS format

	SAS_pusum15a_FmtAssoc.sas - a SAS program to create value labels to the public-use SAS file
	SAS_pusum15a_FmtAttach.sas - a SAS program to assign value labels to the public-use SAS file


     Public Library Outlet Data File
     -------------------------------

	Contains 17,408 records and 40 variables. Files include:	

	puout15a.csv - 2015 imputed suppressed outlet file in csv format
    	puout15a.sas7bdat - 2015 imputed suppressed outlet file in SAS format
    	puout15a.spss - 2015 imputed suppressed outlet file in SPSS format

	SAS_puout15a_FmtAssoc.sas - a SAS program to create value labels to the public-use SAS file
	SAS_puout15a_FmtAttach.sas - a SAS program to assign value labels to the public-use SAS file




  Documentation and Related Files
  ________________________________

  The following documentation is provided on the Public Libraries Survey Data and Reports page at IMLS.gov:

  	Documentation - Survey documentation for Fiscal Year 2015 Public Libraries Survey in PDF format

  	Supplementary Tables - Supplementary tables for Fiscal Year 2015 Public Libraries Survey in PDF format

  	Data Element Definitions - Data element definitions for Fiscal Year 2015 Public Libraries Survey in PDF format




*********************************************************************************************************************************
Appendix A: The code below is for SAS users to recode negative values into missing in SAS.


Recoding Negative Values to Missing in SAS
___________________________________________

*-----------------------------------*
|    For Public Library Data File   |
*-----------------------------------*
*Insert this section into data step;

array num _numeric_; 
do over num;
if num = -1 then num = .M; /*recode missing value into .M*/
if num = -9 then num = .S; /*recode suppressed value into .S*/
if num = -3 and STATSTRU in ('03', '23') then num = .C; /*recode "Closed and Temporary Closed Library" into .C*/
else if num = -3 then num = .N; /*recode "Not Applicable" into .N*/
end;
array char _character_;
do over char;
if char ='M' then char = ' '; /*recode missing value into M for character variables*/
end;
/*recode the rest of special missing into corresponding missing values*/
if PHONE = '-3' then PHONE = ' '; 
if STARTDAT = '-3' then STARTDAT = '';
if ENDDATE = '-3' then ENDDATE = '';
if LONGITUD = 0.0000000 then LONGITUD = .M;
if LATITUDE = 0.0000000 then LATITUDE = .M;
if CENTRACT = 0 then CENTRACT = .M;
if CENBLOCK = 0 then CENBLOCK = .M;
if CBSA = 0 then CBSA = .M;


*------------------------------------------------------------------*
|   For Public Library State Summary/State Characteristics files   |
*------------------------------------------------------------------*
*Insert this section into data step;

array num _numeric_;
do over num;
if num = -1 then num = .M; /*recode missing value into .M*/
if num = -9 then num = .S; /*recode suppressed value into .S*/
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
else if num = -3 then num = .N; /*recode "Not Applicable" into .N*/
end;
array char _character_;
do over char;
if char ='M' then char = ' '; /*recode missing value into M for character variables*/
end;
/*recode the rest of special missing into corresponding missing values*/
if PHONE = '-3' then PHONE = ' '; 
if STARTDAT = '-3' then STARTDAT = '';
if ENDDATE = '-3' then ENDDATE = '';
if LONGITUD = 0.0000000 then LONGITUD = .M;
if LATITUDE = 0.0000000 then LATITUDE = .M;
if CENTRACT = 0 then CENTRACT = .M;
if CENBLOCK = 0 then CENBLOCK = .M;
if CBSA = 0 then CBSA = .M;







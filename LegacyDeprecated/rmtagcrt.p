
/*------------------------------------------------------------------------
    File        : rmtagcrt.p
    Purpose     : 

    Syntax      :

    Description : RM Create Tags Icon start program

    Author(s)   : 
    Created     : Mon Jul 17 11:00:56 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */

&SCOPED-DEFINE loginProcedure nosweat/login.w
&SCOPED-DEFINE checkUserRecord YES 
&SCOPED-DEFINE connectDatabases YES
&SCOPED-DEFINE runAsiLoad NO
&SCOPED-DEFINE createSingleUserPFs NO
&SCOPED-DEFINE nonPersistProgram rmrep/rmloadtg.w
&SCOPED-DEFINE checkExpiredLicense YES
&SCOPED-DEFINE checkUserCount YES
&SCOPED-DEFINE getCompanyProc CUSTOM/getcomp.p

/* ***************************  Main Block  *************************** */
    
{nosweat.i}
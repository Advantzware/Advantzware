
/*------------------------------------------------------------------------
    File        : CheckPrintTester.p
    Purpose     : 

    Syntax      :

    Description : For testing Check Printing

    Author(s)   : BV
    Created     : Wed Feb 19 00:17:23 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE ghSession           AS HANDLE.
DEFINE VARIABLE ghEstimateCalcProcs AS HANDLE.
DEFINE VARIABLE giTimer             AS INTEGER   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */
RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).

DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
RUN ap\CheckPrint.p ('001', /*Company*/ 
                             TODAY, /*Check Date*/ 
                             '001', /*Bank Code*/ 
                             '','zzzzzzz',  /*vendor range*/ 
                             10022, /*starting check number*/ 
                             YES, /*Run a sample set of data*/ 
                             YES,  /*Preview and don't process*/ 
                             cFileName /*output file*/).


/* ***************************  Main Block  *************************** */


/*------------------------------------------------------------------------
    File        : oeValidateInc.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu May 02 14:21:17 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEF NEW GLOBAL SHARED VAR spOeValidate AS HANDLE NO-UNDO.
DEF VAR lHoldError AS LOG NO-UNDO.
DEF VAR cErrMessage AS CHAR NO-UNDO.
DEF VAR cHoldMessage AS CHAR NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
IF NOT VALID-HANDLE(spOeValidate) THEN 
    RUN system/oeValidate.p PERSISTENT SET spOeValidate.

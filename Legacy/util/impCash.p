
/*------------------------------------------------------------------------
    File        : impCash.p
    Purpose     : 

    Syntax      :

    Description : Runs Importer only for Accounts Receivable Cash Receipts

    Author(s)   : BV
    Created     : Thu Dec 7 15:05:07 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}

DEFINE VARIABLE lAccess AS LOGICAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


RUN util/CheckModule.p ("ASI","impCash.", YES, OUTPUT lAccess).
IF lAccess THEN 
    RUN util\Importer.w (gcompany,'Cash', ?).

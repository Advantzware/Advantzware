
/*------------------------------------------------------------------------
    File        : PMCheck.p
    Purpose     : 

    Syntax      :

    Description : Run Price Matrix Checker

    Author(s)   : BV
    Created     : Fri May 04 12:52:56 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{custom/gloc.i}
{custom/getloc.i}
{sys/inc/var.i NEW SHARED}

DEFINE VARIABLE lAccess AS LOGICAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ASSIGN 
    cocode = gcompany
    locode = gloc
    .

RUN oe\dPriceMatrixChecker.w (gcompany).

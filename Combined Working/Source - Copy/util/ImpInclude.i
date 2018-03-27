
/*------------------------------------------------------------------------
    File        : ImpInclude.i
    Purpose     : 

    Syntax      :

    Description : Launcher Include for Import Programs

    Author(s)   : BV
    Created     : Sun Feb 25 11:38:06 EST 2018
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

RUN util/CheckModule.p ("ASI",{1}, YES, OUTPUT lAccess).
IF lAccess THEN 
    RUN util\Importer.w (gcompany,{2}, ?).

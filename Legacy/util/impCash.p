
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

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN util/chk-mod.p ("ASI","impCash.") NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

RUN util\Importer.w (gcompany,'Cash').

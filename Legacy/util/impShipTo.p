
/*------------------------------------------------------------------------
    File        : impShipTo.p
    Purpose     : 

    Syntax      :

    Description : Runs Importer only for ShipTo

    Author(s)   : BV
    Created     : Wed Nov 22 15:05:07 EST 2017
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
RUN util/chk-mod.p ("ASI","impShipTo.") NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

RUN util\Importer.w (gcompany,'ShipTo').

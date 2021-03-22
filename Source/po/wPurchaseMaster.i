/*------------------------------------------------------------------------
    File        : po/wPurchaseMaster.i
    Purpose     : 

    Syntax      :

    Description : Caller Program for PU1 Screen

    Author(s)   : Sewa Singh
    Created     : Mon March 08 03:27:01 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

IF access-close THEN 
RETURN.

run-proc = "po/wPurchaseMaster.w".
{methods/smartrun.i "('{1}')"}

IF VALID-HANDLE(THIS-PROCEDURE) THEN
DELETE PROCEDURE THIS-PROCEDURE.

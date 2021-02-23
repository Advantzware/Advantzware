/*------------------------------------------------------------------------
    File        : oe/wOrderEntryMaster.i
    Purpose     : 

    Syntax      :

    Description : Caller Program for OW Screen

    Author(s)   : Ron Stark
    Created     : Tue Feb 23 03:27:01 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

IF access-close THEN 
RETURN.

run-proc = "oe/wOrderEntryMaster.w".
{methods/smartrun.i "('{1}')"}

IF VALID-HANDLE(THIS-PROCEDURE) THEN
DELETE PROCEDURE THIS-PROCEDURE.

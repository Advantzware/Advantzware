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

RUN po/wPurchaseMaster.w PERSISTENT SET phandle ( INPUT '{1}')  .
/* Set the option frame size and colour to give blue background to icons and 
   add the handle of scope define object to temptable for resizizng */
RUN beforeinitialize IN phandle NO-ERROR.
RUN dispatch IN phandle ('initialize':U) NO-ERROR.
RUN ChangePanelState IN phandle (
    INPUT 1,
    INPUT '{1}'
    ).
/* Add the handle of all smart object to be resized/shifted on resize to the temptable and 
   Shift all the icons towards right */
RUN afterinitialize IN phandle NO-ERROR.
IF VALID-HANDLE(THIS-PROCEDURE) THEN
DELETE PROCEDURE THIS-PROCEDURE.

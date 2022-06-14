/* pPrintImageOnBack.i - SachinChahal - 12.23.2021 */

PROCEDURE pPrintImageOnBack:
/*------------------------------------------------------------------------------
  Purpose:                 /** Print Image On Back **/
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdOutputProcs AS HANDLE.
    RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.  
    
    DEFINE VARIABLE lPrintImage AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cImageName  AS CHARACTER NO-UNDO.
    
    RUN PrintImageOnBack IN hdOutputProcs(
        INPUT {1},       /* Image Name => ".\custfiles\Images\<FormatName>BackImage.pdf" */
        INPUT "{2}",     /* After which Page- Image will print  (First, All) */
        OUTPUT lPrintImage,
        OUTPUT cImageName
        ).
    
    IF lPrintImage THEN
    DO:
        PAGE.
        PUT UNFORMATTED "<R1><C1><#71><R+65><C125><IMAGE#71=" cImageName .
    END.

    DELETE OBJECT hdOutputProcs.

END PROCEDURE.

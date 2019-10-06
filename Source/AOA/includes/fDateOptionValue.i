/* fDateOptionValue.i */

FUNCTION fDateOptionValue RETURNS DATE
  (ipcDateOption AS CHARACTER, ipdtDate AS DATE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    RETURN DYNAMIC-FUNCTION("fDateOptionDate" IN hAppSrvBin,ipcDateOption,ipdtDate).

END FUNCTION.

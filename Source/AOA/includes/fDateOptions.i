/* fDateOptions.i */

FUNCTION fDateOptions RETURNS LOGICAL (iphDateOption AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DYNAMIC-FUNCTION("sfCommon_SetDateOptions", iphDateOption).

    RETURN TRUE.

END FUNCTION.

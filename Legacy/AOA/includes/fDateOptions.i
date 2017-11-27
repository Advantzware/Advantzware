/* fDateOptions.i */

FUNCTION fDateOptions RETURNS LOGICAL (ipDateOption AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dateOptions AS CHARACTER NO-UNDO INITIAL
"Fixed Date~
,Current Date~
,Current Date -1~
,Current Date +1~
,Current Date -2~
,Current Date +2~
,Current Date -3~
,Current Date +3~
,Current Date -4~
,Current Date +4~
,Current Date -5~
,Current Date +5~
,Current Date -6~
,Current Date +6~
,Current Date -7~
,Current Date +7~
,Current Date -8~
,Current Date +8~
,Current Date -9~
,Current Date +9~
,Current Date -10~
,Current Date +10~
,Start of this Month~
,End of this Month~
,First Day of Last Month~
,Last Day of Last Month~
,Start of this Year~
,End of this Year~
,First Day of Last Year~
,Last Day of Last Year~
,Last Sunday~
,Last Monday~
,Last Tuesday~
,Last Wednesday~
,Last Thursday~
,Last Friday~
,Last Saturday~
,Next Sunday~
,Next Monday~
,Next Tuesday~
,Next Wednesday~
,Next Thursday~
,Next Friday~
,Next Saturday~
".
    ASSIGN
        ipDateOption:LIST-ITEMS   = dateOptions
        ipDateOption:INNER-LINES  = NUM-ENTRIES(dateOptions)
        ipDateOption:SCREEN-VALUE = ipDateOption:ENTRY(1)
        .
  RETURN TRUE.

END FUNCTION.

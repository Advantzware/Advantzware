/*------------------------------------------------------------------------
    File        : api/ResponseHandlerZohoDesk.p
    Purpose     : Processes the response and returns the status

    Syntax      :

    Description : Processes the response and returns the status

    Author(s)   : DEVA$!
    Created     : Wed Apr 27 07:17:43 EDT 2022
    Notes       :
  ----------------------------------------------------------------------*/
    USING Progress.Json.ObjectModel.*.
    
    DEFINE INPUT  PARAMETER iplcResponseData AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage       AS CHARACTER NO-UNDO.
    
    ASSIGN
        opcMessage = "Success"
        oplSuccess = TRUE
        .

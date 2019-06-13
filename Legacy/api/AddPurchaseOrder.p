/*------------------------------------------------------------------------
    File        : api/AddPurchaseOrder.p
    Purpose     : Returns the request data for purchase order addition

    Syntax      :

    Description : Returns the request data for purchase order addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER ipcParentID       AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcRequestHandler AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData  AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage        AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess        AS LOGICAL   NO-UNDO.

RUN VALUE(ipcRequestHandler) (
    INPUT ipcParentID,
    INPUT-OUTPUT ioplcRequestData,
    OUTPUT opcMessage,
    OUTPUT oplSuccess
    ).


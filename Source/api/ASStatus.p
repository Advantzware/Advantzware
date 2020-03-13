/*------------------------------------------------------------------------
    File        : api\ASStatus.p
    Purpose     : Procedure to verify if AppServer is running and connected
                  to databases

    Syntax      :

    Description : 

    Author(s)   : Mithun Porandla
    Created     : Thu March 12 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplDBStatus AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.

/* LDBNAME(1) - ASI database and LDNAME(2) - AUDIT database */   
ASSIGN
    oplDBStatus = LDBNAME(1) NE ? AND LDBNAME(2) NE ?
    opcMessage  = IF LDBNAME(1) EQ ? THEN
                      "ASI database not connected"
                  ELSE IF LDBNAME(2) EQ ? THEN
                      "AUDIT database not connected"
                  ELSE
                      "ASI and AUDIT databases are connected"
    .
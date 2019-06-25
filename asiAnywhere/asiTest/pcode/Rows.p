/*------------------------------------------------------------------------
    File        : rows.p
    Purpose     : Rows

    Syntax      :

    Description : Return a Dataset of all Rows line in order inquiry

    Author(s)   : Kuldeep
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttRows NO-UNDO
     FIELD aLine AS INT
    . 
DEFINE DATASET dsRows FOR ttRows .
DEFINE QUERY q-RowsQuery FOR ttRows.
DEFINE DATA-SOURCE src-Rows  FOR QUERY q-RowsQuery.
BUFFER ttRows :ATTACH-DATA-SOURCE(DATA-SOURCE src-Rows  :HANDLE).

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vLine      AS INT  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRows.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF vLine     = ? THEN ASSIGN vLine     = 0.
IF vLine = 0  THEN DO:
    FIND FIRST users WHERE users.user_id = prmUser NO-ERROR.
    IF AVAILABLE users THEN DO:
        CREATE ttRows.
        ASSIGN ttRows.aLine = users.num-rows-web.
    END.
END.
IF vLine <> 0 THEN DO:
    FIND FIRST users WHERE users.user_id = prmUser EXCLUSIVE-LOCK.
    IF available users THEN DO:
         ASSIGN users.num-rows-web = vLine.
         CREATE ttRows.
         ASSIGN ttRows.aLine = vLine.
     END.
END.



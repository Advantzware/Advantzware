
/*------------------------------------------------------------------------
    File        : APIDefs.i
    Purpose     : Holds all the definitions required for Inbound API Monitor

    Syntax      :

    Description : 

    Author(s)   : Mithun Porandla
    Created     : Tue Oct 06 06:24:09 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
{api\ttInboundRequest.i}

DEFINE VARIABLE cCSVFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdInboundProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE cUser          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPassword      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSuccess       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.

RUN api/InboundProcs.p PERSISTENT SET hdInboundProcs.

RUN pGetUserDetails (
    OUTPUT cUser,
    OUTPUT cPassword,
    OUTPUT lSuccess,
    OUTPUT cMessage
    ).

IF NOT lSuccess THEN DO:
    MESSAGE cMessage
    VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
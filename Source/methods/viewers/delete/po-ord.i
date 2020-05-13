/* Local delete for po-ord */
DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAPIID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTriggerID   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrimaryID   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTableList   AS CHARACTER NO-UNDO.

/* OutboundProcs.p - Includes procedures for making api outbound requests */
DEFINE VARIABLE hdOutboundProcs AS HANDLE NO-UNDO.
RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

IF AVAILABLE po-ord THEN DO:
    ASSIGN
        cAPIID       = "SendPurchaseOrder"
        cTriggerID   = "DeletePurchaseOrder"
        cPrimaryID   = STRING(po-ord.po-no)
        cTableList   = "po-ord"
        cDescription = cAPIID + " triggered by " + cTriggerID + " from v-purord.w for PO: " + cPrimaryID
        .
    /* Prepare Outbound request before deleting the po-ord record.
       Call the outbound request only if the po-ord record is deleted successfully */
    
	RUN Outbound_PrepareRequest IN hdOutboundProcs (
		INPUT  po-ord.company,          /* Company Code (Mandatory) */
		INPUT  po-ord.loc,              /* Location Code (Mandatory) */
		INPUT  cAPIID,                  /* API ID (Mandatory) */
		INPUT  "",                      /* Client ID (Optional) - Pass empty in case to make request for all clients */
		INPUT  cTriggerID,              /* Trigger ID (Mandatory) */
		INPUT  cTableList,              /* Comma separated list of table names for which data being sent (Mandatory) */
		INPUT  STRING(ROWID(po-ord)),   /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */
		INPUT  cPrimaryID,              /* Primary ID for which API is called for (Mandatory) */
		INPUT  cDescription,            /* Event's description (Optional) */
		OUTPUT lSuccess,                /* Success/Failure flag */
		OUTPUT cMessage                 /* Status message */
		) NO-ERROR.
END.

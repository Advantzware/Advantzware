/* Local delete for oe-relh*/
DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAPIID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTriggerID   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrimaryID   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTableList   AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-oe-rell FOR oe-rell.
DEFINE BUFFER bf-itemfg  FOR itemfg.

/* OutboundProcs.p - Includes procedures for making api outbound requests */
DEFINE VARIABLE hdOutboundProcs AS HANDLE NO-UNDO.
RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

IF AVAILABLE oe-relh THEN DO:
    /* Prepare Outbound request before deleting the oe-relh record.
       Call the outbound request only if the oe-relh record is deleted successfully */
    
        FOR EACH bf-oe-rell NO-LOCK 
            WHERE bf-oe-rell.company EQ oe-relh.company
            AND bf-oe-rell.r-no EQ oe-relh.r-no,
            FIRST bf-itemfg NO-LOCK 
            WHERE bf-itemfg.company EQ bf-oe-rell.company
            AND bf-itemfg.i-no EQ bf-oe-rell.i-no
            BREAK BY bf-oe-rell.r-no  /*In order to get .loc from first oe-rell as "shipFrom"*/
            BY bf-oe-rell.i-no:
            
            IF FIRST-OF(bf-oe-rell.r-no) THEN DO:
                ASSIGN 
                    cAPIID       = "SendRelease"
                    cTriggerID   = "DeleteRelease"
                    cPrimaryID   = STRING(oe-relh.release#)
                    cDescription = cAPIID + " triggered by " + cTriggerID 
                                 + " from v-oerel.w for Release: " + cPrimaryID
                    . 
                RUN Outbound_PrepareRequest IN hdOutboundProcs (
                    INPUT  oe-relh.company,                /* Company Code (Mandatory) */
                    INPUT  bf-oe-rell.loc,               /* Location Code (Mandatory) */
                    INPUT  cAPIID,                  /* API ID (Mandatory) */
                    INPUT  "",               /* Client ID (Optional) - Pass empty in case to make request for all clients */
                    INPUT  cTriggerID,              /* Trigger ID (Mandatory) */
                    INPUT  "oe-relh",               /* Comma separated list of table names for which data being sent (Mandatory) */
                    INPUT  STRING(ROWID(oe-relh)),  /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
                    INPUT  cPrimaryID,              /* Primary ID for which API is called for (Mandatory) */   
                    INPUT  cDescription,       /* Event's description (Optional) */
                    OUTPUT lSuccess,                /* Success/Failure flag */
                    OUTPUT cMessage                 /* Status message */
                    ) NO-ERROR.
    END. /*avail oe-relh*/
    END.
END.

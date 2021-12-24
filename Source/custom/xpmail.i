/* xpmail.i - used in xpmail.p & xpmail2.p */
/* reformatted by dgdizon on apr 26 2007 for readability. */

DEFINE INPUT  PARAMETER ipType AS CHARACTER NO-UNDO.
&IF DEFINED(passGroupTitle) NE 0 &THEN
DEFINE INPUT  PARAMETER ipGroupTitle AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipGroupTitle AS CHARACTER NO-UNDO INITIAL "EMail".
&ENDIF
DEFINE INPUT  PARAMETER ipFileList AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipIdxKey   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipSubject  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipBody     AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opReturn   AS INTEGER   NO-UNDO.

{custom/globdefs.i}

DEFINE VARIABLE retCode          AS INTEGER   NO-UNDO.
DEFINE VARIABLE ls-to-list       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-mailto        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-mailsubject   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-mailbody      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-mailattach    AS CHARACTER NO-UNDO.
DEFINE VARIABLE vlSilentMode     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-to-list2      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCheckMailFormat AS CHARACTER NO-UNDO.
DEFINE VARIABLE hEmailProcs      AS HANDLE    NO-UNDO.

RUN system/EmailProcs.p PERSISTENT SET hEmailProcs.

IF ipType EQ "ALL" THEN
    RUN pBuildToList IN hEmailProcs (
        ipType,           /* Rec_Key       */
        "",               /* Rec_Key       */
        "",               /* Email Address */
        ipGroupTitle,     /* Title         */
        OUTPUT ls-to-list /* Recepients    */
        ).
ELSE
IF ipType BEGINS "CUSTOMER" THEN
    RUN pCustomer IN hEmailProcs (
        ipType,
        g_company,
        ipIdxKey,
        ipGroupTitle,
        OUTPUT ls-to-list
        ).
ELSE
IF ipType BEGINS "LOC" THEN   
    RUN pLoc IN hEmailProcs (
        g_company,
        ipIdxKey,
        OUTPUT ls-to-list
        ).
ELSE
IF ipType BEGINS "SALESREP" THEN
    RUN pSalesRep IN hEmailProcs (
        ipType,
        g_company,
        ipIdxKey,
        ipGroupTitle,
        OUTPUT ls-to-list
        ).
ELSE
IF ipType BEGINS "SHIPTO" THEN
    RUN pShipTo IN hEmailProcs (
        ipType,
        g_company,
        ipIdxKey,
        ipGroupTitle,
        OUTPUT ls-to-list
        ).
ELSE
IF ipType BEGINS "SOLDTO" THEN  
    RUN pSoldTo IN hEmailProcs (
        ipType,
        g_company,
        ipIdxKey,
        ipGroupTitle,
        OUTPUT ls-to-list
        ).
ELSE
IF ipType BEGINS "VENDOR" THEN
    RUN pVendor IN hEmailProcs (
        ipType,
        g_company,
        ipIdxKey,
        ipGroupTitle,
        OUTPUT ls-to-list
        ).

ASSIGN
    lv-mailto      = "To:" + ls-to-list
    lv-mailsubject = ipSubject
    lv-mailbody    = ipBody
    lv-mailattach  = ipFileList
    .
IF ls-to-list EQ "" OR 
   ls-to-list EQ ?  OR
   NOT ls-to-list MATCHES "*@*" THEN DO:
    IF ipType MATCHES "*1" THEN
    RETURN. /* Quiet Mode. */
END.

IF lv-mailattach MATCHES("*xpr*") AND SEARCH("viewer.exe") NE ? THEN
ASSIGN
    FILE-INFO:FILE-NAME = "viewer.exe"
    lv-mailattach       = FILE-INFO:FULL-PATHNAME + "," + lv-mailattach
    .

cCheckMailFormat = ENTRY(1,ipType,"|").

/* Customer, Vendor, ShipTo   = DIALOG BOX        */
IF TRIM(ipType) EQ "" OR SUBSTRING (cCheckMailFormat, LENGTH (cCheckMailFormat)) NE '1' THEN DO:
  /* Workaround for blank recipient list */
  IF lv-mailto EQ "To:" THEN
  lv-mailto = "".
  RUN mail (
    lv-mailto,      /* Mail Recepients  */
    lv-mailsubject, /* Subject          */
    lv-mailbody,    /* Body             */
    lv-mailattach,  /* Attachment       */
    1,              /* Mail Dialog Type */
    OUTPUT retcode  /* Return Code      */
    ).
  
END.
/* Customer1, Vendor1, ShipTo1 = SILENT MODE      */ 
ELSE
RUN mail (
    lv-mailto,      /* Mail Recepients  */
    lv-mailsubject, /* Subject          */
    lv-mailbody,    /* Body             */
    lv-mailattach,  /* Attachment       */
    0,              /* Mail Dialog Type */
    OUTPUT retCode  /* Return Code      */
    ).

opReturn = retCode.
                    
PROCEDURE mail EXTERNAL "xpMail.dll":
    DEFINE INPUT  PARAMETER mailTo      AS CHARACTER.
    DEFINE INPUT  PARAMETER mailsubject AS CHARACTER.
    DEFINE INPUT  PARAMETER mailText    AS CHARACTER.
    DEFINE INPUT  PARAMETER mailFiles   AS CHARACTER.
    DEFINE INPUT  PARAMETER mailDialog  AS LONG.
    DEFINE OUTPUT PARAMETER retCode     AS LONG.
END.

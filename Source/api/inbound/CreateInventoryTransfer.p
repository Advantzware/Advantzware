/*------------------------------------------------------------------------
    File        : api\inbound\CreateInventoryTransfer.p
    Purpose     : Processes request data

    Syntax      :

    Description : Processes request data

    Author(s)   : Vishnu Vellanki
    Created     : Tue July 08 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

{inventory/ttinventory.i "NEW SHARED"}.
{jc/jcgl-sh.i  NEW}

DEFINE INPUT  PARAMETER ipcCompany              AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcWareHouseID          AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcLocationID           AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcInventoryStockIDTag  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrimaryID            AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemType             AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcUsername             AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iplPost                 AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opriRctd                AS ROWID      NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess              AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage              AS CHARACTER  NO-UNDO.

/* This will eventually move to setsession approach */
&SCOPED-DEFINE NEW NEW
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE VARIABLE hSession AS HANDLE NO-UNDO.
DEFINE VARIABLE hTags    AS HANDLE NO-UNDO.

g_company=ipcCompany.

RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

RUN system/session.p  PERSISTENT SET hSession.
SESSION:ADD-SUPER-PROCEDURE (hSession).
RUN system/TagProcs.p PERSISTENT SET hTags.
SESSION:ADD-SUPER-PROCEDURE (hTags).
{sys/inc/var.i "new shared"}

DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE lValidBin        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidLoc        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidCompany    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iRNo             AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTransfer        AS CHARACTER NO-UNDO INITIAL "T".
DEFINE VARIABLE lPromptForClose  AS LOGICAL   NO-UNDO.

RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs. 

ASSIGN
    oplSuccess    = YES
    lValidLoc     = YES
    lValidBin     = YES
    lValidCompany = YES
    .

/* Validate company */
lValidCompany = CAN-FIND(FIRST company NO-LOCK
                         WHERE company.company EQ ipcCompany).
        
IF NOT lValidCompany THEN DO:
    ASSIGN 
        opcMessage = "Invalid Company " + ipcCompany
        oplSuccess = NO
        .
    RETURN.
END.
  
/* Validate warehouse */        
RUN ValidateLoc IN hdInventoryProcs (
    ipcCompany,
    ipcWareHouseID,
    OUTPUT lValidLoc
    ).

IF NOT lValidLoc THEN DO:
    ASSIGN 
        opcMessage = "Invalid WareHouseID " + ipcWareHouseID                 
        oplSuccess = NO
        .
    RETURN.
END.

/* Validate location */
RUN ValidateBin IN hdInventoryProcs (
    ipcCompany,
    ipcWareHouseID,
    ipcLocationID,
    OUTPUT lValidBin
    ).

IF ipcLocationID EQ "" OR NOT lValidBin THEN DO:
    ASSIGN 
        opcMessage = "Invalid LocationID " + ipcLocationID
        oplSuccess = NO 
        .
    RETURN.
END.

/* Validate tag */
IF ipcInventoryStockIDTag EQ "" THEN
    ASSIGN 
        opcMessage = "Empty inventoryStockIDTag"
        oplSuccess = NO
        .

/* Validate primary id */
IF ipcPrimaryID EQ "" THEN
    ASSIGN 
        opcMessage = IF opcMessage EQ "" THEN 
                         "Empty PrimaryID"
                     ELSE
                         opcMessage + ", " + "Empty PrimaryID"
        oplSuccess = NO
        .

/* Validate item type */
IF ipcItemType EQ "" THEN
    ASSIGN 
        opcMessage = IF opcMessage EQ "" THEN 
                         "Empty ItemType"
                     ELSE
                         opcMessage + ", " + "Empty ItemType"
        oplSuccess = NO
        .
        

IF NOT oplSuccess THEN
    RETURN.

/* Inventory transfer creation */ 
IF ipcItemType EQ "FG" THEN DO:
     
     /* Checks whether tag# is valid or not */
     FIND FIRST loadtag WHERE loadtag.company   = ipcCompany
                          AND loadtag.ITEM-type = NO
                          AND loadtag.tag-no    = ipcInventoryStockIDTag NO-LOCK NO-ERROR.
     IF NOT AVAILABLE loadtag THEN DO:
         ASSIGN
            opcMessage = "Invalid Loadtag# " + ipcInventoryStockIDTag 
            oplSuccess = NO
            .
         RETURN.  
     END.

     /* Checks whether inventory is valid or not */
     FIND FIRST fg-bin WHERE fg-bin.company = ipcCompany
                         AND fg-bin.i-no    = ipcPrimaryID
                         AND fg-bin.tag     = loadtag.tag-no
                         AND fg-bin.qty     > 0
                         NO-LOCK NO-ERROR.
     IF NOT AVAILABLE fg-bin THEN DO:
         ASSIGN
            opcMessage = "Invalid Inventory for the tag " + ipcInventoryStockIDTag
            oplSuccess = NO
            .
         RETURN.
     END.
     
     /* Checks whether To Whse/Bin and From Whse/Bin are same or not */
     IF fg-bin.loc     EQ ipcWareHouseID AND
        fg-bin.loc-bin EQ ipcLocationID  THEN DO:
         ASSIGN
            opcMessage = "To Whse/Bin may not be the same as From Whse/Bin for item " + ipcPrimaryID
            oplSuccess = NO
            .
         RETURN.
     END.
        
     FIND FIRST itemfg WHERE itemfg.company = ipcCompany
                         AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.

    /* Retrieving last record of fg-rctd table */ 
    FIND LAST fg-rctd NO-LOCK USE-INDEX fg-rctd NO-ERROR.
        IF AVAILABLE fg-rctd AND fg-rctd.r-no GT iRNo THEN 
            iRNo = fg-rctd.r-no.

    FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT iRNo THEN 
            iRNo = fg-rcpth.r-no.
        
    /* Creates fg-rctd record for new bin & location */     
    CREATE fg-rctd.
    ASSIGN
        fg-rctd.r-no         = iRNo + 1
        fg-rctd.rct-date     = TODAY
        fg-rctd.trans-time   = TIME
        fg-rctd.s-num        = 0
        fg-rctd.units-pallet = fg-bin.units-pallet
        fg-rctd.cases-unit   = fg-bin.cases-unit
        fg-rctd.company      = fg-bin.company
        fg-rctd.tag          = fg-bin.tag
        fg-rctd.i-no         = fg-bin.i-no
        fg-rctd.i-name       = itemfg.i-name 
        fg-rctd.po-no        = fg-bin.po-no 
        fg-rctd.qty-case     = IF AVAILABLE fg-bin THEN fg-bin.case-count
                               ELSE loadtag.qty-case
        fg-rctd.job-no       = fg-bin.job-no
        fg-rctd.job-no2      = fg-bin.job-no2
        fg-rctd.loc          = fg-bin.loc
        fg-rctd.loc-bin      = fg-bin.loc-bin
        fg-rctd.tag2         = fg-bin.tag
        fg-rctd.loc2         = ipcWareHouseID
        fg-rctd.loc-bin2     = ipcLocationID
        fg-rctd.cust-no      = CAPS(fg-bin.cust-no)
        fg-rctd.rita-code    = cTransfer
        fg-rctd.cases        = IF AVAILABLE fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                               ELSE loadtag.tot-cases
        fg-rctd.partial      = IF AVAILABLE fg-bin THEN fg-bin.partial-count
                               ELSE loadtag.partial 
        fg-rctd.t-qty        = (fg-rctd.cases * fg-rctd.qty-case) + fg-rctd.partial 
        fg-rctd.created-by   = ipcUsername
        fg-rctd.updated-by   = ipcUsername
        fg-rctd.pur-uom      = fg-bin.pur-uom
        fg-rctd.cost-uom     = fg-bin.pur-uom
        fg-rctd.std-cost     = fg-bin.std-tot-cost
        fg-rctd.ext-cost     = fg-rctd.t-qty * fg-rctd.std-cost
        fg-rctd.enteredBy    = ipcUsername
        fg-rctd.enteredDT    = DATETIME(TODAY, MTIME)
        opriRctd             = ROWID(fg-rctd)
        .
    
   /* Posts fg-rctd records */
   IF iplPost THEN DO:
        RUN PostFinishedGoodsForUser IN hdInventoryProcs(
            INPUT        ipcCompany,
            INPUT        cTransfer,       /* Transfer */
            INPUT        ipcUsername,
            INPUT        lPromptForClose, /* Executes API closing orders logic */
            INPUT-OUTPUT oplSuccess,
            INPUT-OUTPUT opcMessage
            ) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Error while posting Finished Good"
                .
    END.
END.


DELETE PROCEDURE hdInventoryProcs.









    
    



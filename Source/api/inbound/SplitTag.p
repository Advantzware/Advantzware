/*------------------------------------------------------------------------
    File        : api/inbound/SplitTag.p
    Purpose     : Fetches Inventory response data for the given input data

    Syntax      :

    Description : Fetches Inventory response data for the given input data

    Author(s)   : Vishnu Vellanki
    Created     : Tue Feb 04 07:33:22 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
{api/inbound/ttItem.i}
{inventory/ttinventory.i "NEW SHARED"}.
{jc/jcgl-sh.i  NEW}

DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipdSplitQuantity    AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemType         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcUserName         AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE               FOR ttItem. 

DEFINE VARIABLE hdInventoryProcs         AS HANDLE    NO-UNDO.
DEFINE VARIABLE cItemTypeFG              AS CHARACTER NO-UNDO INITIAL "FG". /* Finished Good */
DEFINE VARIABLE cTagItem                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTagNo                   AS INTEGER   NO-UNDO.
DEFINE VARIABLE riloadtag                AS ROWID     NO-UNDO.  
DEFINE VARIABLE cTransactionTypeTransfer AS CHARACTER NO-UNDO INITIAL "T". /* Transfer */
DEFINE VARIABLE lPromptForClose          AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE cStockIDAlias            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cNewInventoryStockID     AS CHARACTER NO-UNDO.  

/* This will eventually move to setsession - START >>>*/
&SCOPED-DEFINE NEW NEW
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE VARIABLE hdSession AS HANDLE NO-UNDO.
DEFINE VARIABLE hdTags    AS HANDLE NO-UNDO.

g_company=ipcCompany.

RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

RUN system/session.p  PERSISTENT SET hdSession.
SESSION:ADD-SUPER-PROCEDURE (hdSession).
RUN system/TagProcs.p PERSISTENT SET hdTags.
SESSION:ADD-SUPER-PROCEDURE (hdTags).
{sys/inc/var.i "new shared"}
/* END <<<*/

RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs. 

oplSuccess = YES.
  
/* Validate company */
IF NOT CAN-FIND(FIRST company NO-LOCK
                WHERE company.company EQ ipcCompany) THEN DO:
    ASSIGN 
        opcMessage = opcMessage + "Invalid Company (" + ipcCompany + ")" 
        oplSuccess = NO
        .
        
    RETURN.
END.
 
/* Validate Item type */
IF ipcItemType EQ "" OR
   ipcItemType NE cItemTypeFG THEN DO:
    ASSIGN 
        opcMessage = "Invalid ItemType (" + ipcItemType + ")"
        oplSuccess = NO 
        .
        
    RETURN.
END.

/* Validate inventory Stock ID */
FIND FIRST loadtag NO-LOCK
     WHERE loadtag.company   EQ ipcCompany
       AND loadtag.item-type EQ NO
       AND loadtag.tag-no    EQ ipcInventoryStockID 
     NO-ERROR.
IF NOT AVAILABLE loadtag THEN DO:
    ASSIGN 
        opcMessage = "Invalid Tag (" + ipcInventoryStockID  + ")"
        oplSuccess = NO
        .
        
    RETURN.
END.

ASSIGN
    cTagItem      = loadtag.i-no
    cStockIDAlias = loadtag.misc-char[1]
    .

IF ipdSplitQuantity LE 0 THEN DO:
    ASSIGN 
        opcMessage = "Invalid Quantity - must be greater than zero" 
        oplSuccess = NO
        .
        
    RETURN.
END.

/* Validates on-hand quantity for the given tag is available at bin or not */    
FIND FIRST fg-bin NO-LOCK
     WHERE fg-bin.company EQ ipcCompany
       AND fg-bin.tag     EQ ipcInventoryStockID
       AND fg-bin.i-no    EQ ipcPrimaryID
       AND fg-bin.qty     GT 0
     NO-ERROR.
IF NOT AVAILABLE fg-bin THEN DO:
    ASSIGN 
        opcMessage = "No on-hand quantity for tag in this bin"
        oplSuccess = NO
        .
        
    RETURN.
END.

/* Validates whether on-hand tag quantity is greater than split quantity or not */
IF fg-bin.qty LT ipdSplitQuantity THEN DO:
   ASSIGN 
        opcMessage = "Split quantity is more than on-hand tag quantity"
        oplSuccess = NO
        .
        
    RETURN.
END.

/* Validates item */
FIND FIRST itemfg NO-LOCK
     WHERE itemfg.company EQ ipcCompany
       AND itemfg.i-no    EQ ipcPrimaryID
     NO-ERROR.
IF NOT AVAILABLE itemfg THEN DO:
    ASSIGN 
        opcMessage = "Invalid Item (" + ipcPrimaryID + ")" 
        oplSuccess = NO
        .
        
    RETURN.
END.

/* Checks whether tag item is same as given item or not */   
IF ipcPrimaryID NE cTagItem THEN DO:
    ASSIGN 
        opcMessage = "Invalid tag for the given Item"
        oplSuccess = NO
        .
        
    RETURN.
END.

/* FG Process */
IF ipcItemType EQ cItemTypeFG THEN DO: 
    DO TRANSACTION ON ERROR UNDO,LEAVE:
        
        /* Creates a new tag number for FG items */
        RUN pGetNextLoadtagNumberForFGItem(
            INPUT  ipcCompany, 
            INPUT  ipcPrimaryID, 
            OUTPUT iTagNo
            ).
        
        /* Create new loadtag record for FG items */
        RUN pCreateNewTagForFGItem (
            INPUT  ipcCompany,
            INPUT  ipcInventoryStockID,
            INPUT  ipdSplitQuantity,
            INPUT  iTagNo,
            OUTPUT cNewInventoryStockID,
            OUTPUT riloadtag
            ) NO-ERROR.
    
        /* Create new fgrctd records to transfer split quantity from given tag to new tag */
        RUN pFGReceiptCreation (
            INPUT  riloadtag,
            INPUT  cNewInventoryStockID,
            INPUT  ipdSplitQuantity,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.

       /* Posts fg-rctd records */
       RUN PostFinishedGoodsForUser IN hdInventoryProcs(
           INPUT        ipcCompany,
           INPUT        cTransactionTypeTransfer, /* Transfer */
           INPUT        ipcUsername,
           INPUT        lPromptForClose, /* Executes API closing orders logic */
           INPUT-OUTPUT oplSuccess,
           INPUT-OUTPUT opcMessage
           ) NO-ERROR.

       IF ERROR-STATUS:ERROR THEN
           ASSIGN
               oplSuccess=NO
               opcMessage=ERROR-STATUS:GET-MESSAGE(1)
               .
    
        IF oplSuccess THEN
            /* Gets inventory details for the new tag */
            RUN api\inbound\GetInventoryDetails.p (
                INPUT  ipcCompany,           /* Company */
                INPUT  "",                   /* Location */
                INPUT  "",                   /* Bin */
                INPUT  cNewInventoryStockID, /* Tag */
                INPUT  ipcPrimaryID,         /* Primary ID */
                INPUT  "",                   /* Job Number */
                INPUT  "",                   /* Job Number2 */
                INPUT  "",                   /* Customer Number */
                INPUT  cItemTypeFG ,         /* Item Type */
                OUTPUT oplSuccess,
                OUTPUT opcMessage,
                OUTPUT TABLE ttItem
                ) NO-ERROR.
         
         IF oplSuccess AND NOT TEMP-TABLE ttItem:HAS-RECORDS THEN
            ASSIGN
                oplSuccess = NO
                opcMessage = "Split tag is not posted"
                .

        IF NOT oplSuccess THEN
            UNDO,LEAVE.

    END.
    
END.

PROCEDURE pCreateNewTagForFGItem PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:Creates new record in loadtag table for FG items
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStockID    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdSplitQuantity       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTagNo               AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNewInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opriloadtag            AS ROWID     NO-UNDO.
    DEFINE BUFFER bf-loadtag FOR loadtag.

    FIND FIRST bf-loadtag NO-LOCK
         WHERE bf-loadtag.company   EQ ipcCompany
           AND bf-loadtag.item-type EQ NO
           AND bf-loadtag.tag-no    EQ ipcInventoryStockID 
         NO-ERROR.
      
    CREATE loadtag.
    BUFFER-COPY bf-loadtag EXCEPT bf-loadtag.tag-no bf-loadtag.qty bf-loadtag.rec_key TO loadtag .
    ASSIGN
        opcNewInventoryStockID = STRING(CAPS(ipcPrimaryID),"x(15)") + STRING(ipiTagNo,"99999")
        loadtag.tag-no         = opcNewInventoryStockID
        loadtag.qty            = ipdSplitQuantity 
        . 
    opriloadtag = ROWID(bf-loadtag).
    RELEASE bf-loadtag.
END PROCEDURE.

PROCEDURE pGetNextLoadtagNumberForFGItem PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Creates new TAG number for FG Process
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPrimaryID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiNextTag   AS INTEGER   NO-UNDO.

    DEFINE VARIABLE iLastFGTag AS INTEGER.
    DEFINE VARIABLE iLastRMTag AS INTEGER.
  
    FIND LAST loadtag NO-LOCK
          WHERE loadtag.company             EQ     ipcCompany
            AND loadtag.item-type           EQ     NO
            AND loadtag.is-case-tag         EQ     NO
            AND loadtag.tag-no              BEGINS ipcPrimaryID
            AND SUBSTR(loadtag.tag-no,1,15) EQ     ipcPrimaryID
          USE-INDEX tag NO-ERROR.

    iLastFGTag = (IF AVAILABLE loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.

    FIND LAST loadtag NO-LOCK
          WHERE loadtag.company             EQ     ipcCompany
            AND loadtag.item-type           EQ     YES
            AND loadtag.is-case-tag         EQ     NO
            AND loadtag.tag-no              BEGINS ipcPrimaryID
            AND SUBSTR(loadtag.tag-no,1,15) EQ     ipcPrimaryID
          USE-INDEX tag NO-ERROR.

    iLastRMTag = (IF AVAILABLE loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.
  
    opiNextTag = MAX(iLastFGTag, iLastRMTag).

END PROCEDURE.

PROCEDURE pFGReceiptCreation PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Creates new fg-rctd record
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriloadtag            AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcNewInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdSplitQuantity       AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage             AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iRNo AS INTEGER NO-UNDO.
    
    oplSuccess = YES.
    
    /* Retrieving last record of fg-rctd table - USE-INDEX fg-rctd is required to enforce the query to fetch latest r-no*/ 
    FIND LAST fg-rctd NO-LOCK USE-INDEX fg-rctd NO-ERROR.
    IF AVAILABLE fg-rctd AND fg-rctd.r-no GT iRNo THEN 
        iRNo = fg-rctd.r-no.
    
    /* Retrieving last record of fg-rcpth table - USE-INDEX fg-rctd is required to enforce the query to fetch latest r-no*/ 
    /* In some cases r-no from fg-rcpth is greater than r-no from fg-rctd - so this additional find and the logic is required*/
    FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT iRNo THEN 
        iRNo = fg-rcpth.r-no.
    
    FIND FIRST loadtag NO-LOCK
         WHERE ROWID(loadtag) EQ ipriloadtag
         NO-ERROR.

    FIND FIRST fg-bin NO-LOCK 
         WHERE fg-bin.company EQ loadtag.company
           AND fg-bin.i-no    EQ loadtag.i-no
           AND fg-bin.tag     EQ loadtag.tag-no
           AND fg-bin.qty     GT 0
         NO-ERROR.
  
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
        fg-rctd.qty          = ipdSplitQuantity
        fg-rctd.i-no         = fg-bin.i-no
        fg-rctd.i-name       = loadtag.i-name 
        fg-rctd.po-no        = fg-bin.po-no 
        fg-rctd.qty-case     = IF AVAILABLE fg-bin THEN 
                                   fg-bin.case-count
                               ELSE 
                                   loadtag.qty-case
        fg-rctd.job-no       = fg-bin.job-no
        fg-rctd.job-no2      = fg-bin.job-no2
        fg-rctd.loc          = fg-bin.loc
        fg-rctd.loc-bin      = fg-bin.loc-bin
        fg-rctd.tag2         = ipcNewInventoryStockID
        fg-rctd.loc2         = fg-bin.loc
        fg-rctd.loc-bin2     = fg-bin.loc-bin
        fg-rctd.cust-no      = CAPS(fg-bin.cust-no)
        fg-rctd.rita-code    = cTransactionTypeTransfer
        fg-rctd.cases        = IF AVAILABLE fg-bin THEN 
                                   TRUNC((ipdSplitQuantity - fg-bin.partial-count) / fg-bin.case-count,0)
                               ELSE 
                                   loadtag.tot-cases
        fg-rctd.partial      = IF AVAILABLE fg-bin THEN 
                                   fg-bin.partial-count
                               ELSE 
                                   loadtag.partial 
        fg-rctd.t-qty        = ipdSplitQuantity
        fg-rctd.created-by   = ipcUsername
        fg-rctd.updated-by   = ipcUsername
        fg-rctd.pur-uom      = fg-bin.pur-uom
        fg-rctd.cost-uom     = fg-bin.pur-uom
        fg-rctd.std-cost     = fg-bin.std-tot-cost
        fg-rctd.ext-cost     = fg-rctd.t-qty * fg-rctd.std-cost
        fg-rctd.enteredBy    = ipcUsername
        fg-rctd.enteredDT    = DATETIME(TODAY, MTIME)
        .

END PROCEDURE.
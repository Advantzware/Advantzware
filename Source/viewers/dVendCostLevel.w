&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------
  File: viewers/dVendCostLevel.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER iprRowid    AS ROWID     NO-UNDO.
DEFINE INPUT  PARAMETER iprRowid2   AS ROWID     NO-UNDO.
DEFINE INPUT  PARAMETER ipcType     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplFirstRow AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opRowid     AS ROWID     NO-UNDO.

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/
{methods/defines/globdefs.i}
DEFINE BUFFER b-prgrms FOR prgrms.
DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE period_pos   AS INTEGER           NO-UNDO.
DEFINE VARIABLE v-count      AS INTEGER           NO-UNDO.
DEFINE VARIABLE k_frac       AS DECIMAL           NO-UNDO INIT 6.25.
DEFINE VARIABLE dMaxQty      AS DECIMAL           NO-UNDO.

DEFINE BUFFER bff-e-itemfg-vend FOR e-itemfg-vend .

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("NOSWEAT") + "..".
ELSE
    ASSIGN
        period_pos = INDEX(PROGRAM-NAME(1),".")
        v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
        v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).


{sys/inc/var.i shared}
{custom/gcompany.i}  

gcompany = cocode.

{sys/inc/f16to32.i}

IF v-cecscrn-dec THEN
DO:
    DEFINE TEMP-TABLE tt-64-dec NO-UNDO
        FIELD DEC AS DECIMAL DECIMALS 6.

    DO v-count = 0 TO 63:
        CREATE tt-64-dec.
        tt-64-dec.DEC = v-count / 64.0.
        RELEASE tt-64-dec.
    END.
END.

DEFINE VARIABLE uom-list              AS CHARACTER INIT "C,CS,EA,L,M," NO-UNDO.
DEFINE VARIABLE lVendItemUseDeviation AS LOGICAL   NO-UNDO .
DEFINE VARIABLE lRecFound             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cRtnChar              AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdVendorCostProcs     AS HANDLE    NO-UNDO.
DEFINE VARIABLE cVendItemCostMaximum  AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT g_company, "VendItemUseDeviation", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lVendItemUseDeviation = LOGICAL(cRtnChar) NO-ERROR.
      
RUN system\VendorCostProcs.p PERSISTENT SET hdVendorCostProcs.

dMaxQty = DYNAMIC-FUNCTION("VendCost_GetUnlimitedQuantity" IN hdVendorCostProcs).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES vendItemCostLevel

/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define QUERY-STRING-D-Dialog FOR EACH vendItemCostLevel SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH vendItemCostLevel SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog vendItemCostLevel
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog vendItemCostLevel


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_MaxQty Btn_Cancel Btn_OK tb_BestCost ~
dFrom dBase dEaCost1 dSetup1 dDev1 dTo cDevLabel iLeadTime RECT-21 
&Scoped-Define DISPLAYED-OBJECTS tb_MaxQty tb_BestCost dFrom dBase dEaCost1 ~
dSetup1 dDev1 dTo cDevLabel cPriceUom iLeadTime 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE cDevLabel AS CHARACTER FORMAT "x(9)":U INITIAL "Deviation" 
     VIEW-AS FILL-IN 
     SIZE 12.6 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE cPriceUom AS CHARACTER FORMAT "x(3)":U 
     LABEL "Cost and Quantity UOM" 
     VIEW-AS FILL-IN 
     SIZE 12.6 BY 1
     BGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE dBase AS DECIMAL FORMAT ">,>>>,>>>,>>9.999999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dDev1 AS DECIMAL FORMAT "->,>>>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dEaCost1 AS DECIMAL FORMAT ">>,>>>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dFrom AS DECIMAL FORMAT "->,>>>,>>>,>>9.999999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 25.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSetup1 AS DECIMAL FORMAT "->,>>>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dTo AS DECIMAL FORMAT "->,>>>,>>>,>>9.999999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 24.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iLeadTime AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 79 BY 9.05
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 17.8 BY 2.29.

DEFINE VARIABLE tb_BestCost AS LOGICAL INITIAL no 
     LABEL "Use For Best Cost" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81
     FONT 6.

DEFINE VARIABLE tb_MaxQty AS LOGICAL INITIAL no 
     LABEL "Unlimited" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81
     FONT 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY D-Dialog FOR 
      vendItemCostLevel SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     tb_MaxQty AT ROW 4.81 COL 3 WIDGET-ID 388
     Btn_Cancel AT ROW 10.67 COL 72
     Btn_OK AT ROW 10.67 COL 64
     tb_BestCost AT ROW 9.1 COL 29 WIDGET-ID 386
     dFrom AT ROW 3.86 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 218
     dBase AT ROW 3.76 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 254
     dEaCost1 AT ROW 6.76 COL 3 NO-LABEL WIDGET-ID 266
     dSetup1 AT ROW 6.76 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 276
     dDev1 AT ROW 6.76 COL 53 COLON-ALIGNED NO-LABEL WIDGET-ID 360
     dTo AT ROW 3.86 COL 53 COLON-ALIGNED NO-LABEL WIDGET-ID 380
     cDevLabel AT ROW 5.76 COL 53 COLON-ALIGNED NO-LABEL WIDGET-ID 288
     cPriceUom AT ROW 1.48 COL 53 COLON-ALIGNED
     iLeadTime AT ROW 9.05 COL 3 NO-LABEL WIDGET-ID 382
     "Quantity Range" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 2.76 COL 31.4 WIDGET-ID 242
     "Quantity Base" VIEW-AS TEXT
          SIZE 17 BY 1 AT ROW 2.76 COL 3 WIDGET-ID 252
     "Cost Per" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 5.76 COL 3 WIDGET-ID 264
     "Setup" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 5.76 COL 29 WIDGET-ID 278
     "Lead Time" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 8.05 COL 3 WIDGET-ID 384
     RECT-1 AT ROW 1.24 COL 2 WIDGET-ID 82
     RECT-21 AT ROW 10.52 COL 63
     SPACE(0.20) SKIP(0.53)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Update / Add Vendor Cost Level"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cPriceUom IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN dEaCost1 IN FRAME D-Dialog
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN iLeadTime IN FRAME D-Dialog
   ALIGN-L                                                              */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "asi.vendItemCostLevel"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Update / Add Vendor Cost Level */
DO: 
    IF VALID-HANDLE(hdVendorCostProcs) THEN
        DELETE PROCEDURE hdVendorCostProcs.    
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:     
        APPLY "go" TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* Ok */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .

    IF ipcType NE "View" THEN DO:

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.

        RUN valid-qty ( OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.
         
        SESSION:SET-WAIT-STATE("general").
  
        IF ipcType EQ "Create" OR ipcType EQ "Copy" THEN
            RUN pCreateValues .
        ELSE RUN pAssignValues .

    END.

    SESSION:SET-WAIT-STATE("").
    
    APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dBase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dBase D-Dialog
ON LEAVE OF dBase IN FRAME D-Dialog
DO: 
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
             RUN valid-qty ( OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dDev1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dDev1 D-Dialog
ON LEAVE OF dDev1 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dFrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dFrom D-Dialog
ON LEAVE OF dFrom IN FRAME D-Dialog
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name} .
         
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dSetup1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dSetup1 D-Dialog
ON LEAVE OF dSetup1 IN FRAME D-Dialog
DO: 
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dTo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dTo D-Dialog
ON LEAVE OF dTo IN FRAME D-Dialog
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name} .
         
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_MaxQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_MaxQty D-Dialog
ON VALUE-CHANGED OF tb_MaxQty IN FRAME D-Dialog /* Unlimited */
DO:
    ASSIGN {&SELF-NAME}.
    IF {&SELF-NAME} THEN
    ASSIGN
        dBase:SCREEN-VALUE = STRING(dMaxQty)
        dTo:SCREEN-VALUE   = STRING(dMaxQty)
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN enable_UI.

    {methods/nowait.i}
     
    RUN pDisplayValue .

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY tb_MaxQty tb_BestCost dFrom dBase dEaCost1 dSetup1 dDev1 dTo cDevLabel 
          cPriceUom iLeadTime 
      WITH FRAME D-Dialog.
  ENABLE tb_MaxQty Btn_Cancel Btn_OK tb_BestCost dFrom dBase dEaCost1 dSetup1 
         dDev1 dTo cDevLabel iLeadTime RECT-21 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssignValues D-Dialog 
PROCEDURE pAssignValues :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE cReturnMessage AS CHARACTER NO-UNDO . 
   
    FIND FIRST vendItemCost WHERE ROWID(vendItemCost) EQ iprRowid  NO-LOCK NO-ERROR.

    DO WITH FRAME {&frame-name}:
        RUN VendCost_UpdateVendItemCostLevel IN hdVendorCostProcs (iprRowid2,
                                                                   vendItemCost.vendItemCostID,
                                                                   DECIMAL(dBase:SCREEN-VALUE),
                                                                   DECIMAL(dEaCost1:SCREEN-VALUE),
                                                                   DECIMAL(dSetup1:SCREEN-VALUE),
                                                                   DECIMAL(dDev1:SCREEN-VALUE),
                                                                   INTEGER(iLeadTime:SCREEN-VALUE),
                                                                   LOGICAL(tb_BestCost:SCREEN-VALUE)).  
    END.
    opRowid = iprRowid2 .
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateValues D-Dialog 
PROCEDURE pCreateValues :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE cReturnMessage AS CHARACTER NO-UNDO .
    DEFINE VARIABLE lNewRecordCreated AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel .  
  
     FIND FIRST vendItemCost WHERE ROWID(vendItemCost) EQ iprRowid  NO-LOCK NO-ERROR.
     FIND FIRST bf-vendItemCostLevel NO-LOCK
          WHERE bf-vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID NO-ERROR.
     IF NOT AVAIL bf-vendItemCostLevel THEN
     lNewRecordCreated = YES.
     
    DO WITH FRAME {&frame-name}:        
        RUN VendCost_CreateVendItemCostLevel IN hdVendorCostProcs (vendItemCost.vendItemCostID,
                                                                    DECIMAL(dBase:SCREEN-VALUE),
                                                                    DECIMAL(dEaCost1:SCREEN-VALUE),
                                                                    DECIMAL(dSetup1:SCREEN-VALUE),
                                                                    DECIMAL(dDev1:SCREEN-VALUE),
                                                                    INTEGER(iLeadTime:SCREEN-VALUE),
                                                                    LOGICAL(tb_BestCost:SCREEN-VALUE),
                                                                    OUTPUT opRowID).
             
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayValue D-Dialog 
PROCEDURE pDisplayValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dQuantityFrom AS DECIMAL NO-UNDO. 
    DEFINE VARIABLE dQuantityTo   AS DECIMAL NO-UNDO.
      
   DO WITH FRAME {&frame-name}:
      
        APPLY "entry" TO dFrom IN FRAME {&FRAME-NAME}.
        
            ASSIGN Btn_OK:LABEL = "Save" .
        

        FIND FIRST vendItemCost WHERE ROWID(vendItemCost) EQ iprRowid  NO-LOCK NO-ERROR.
        IF AVAILABLE vendItemCost AND vendItemCost.useQuantityFromBase THEN 
            tb_MaxQty:HIDDEN = TRUE.
        FIND FIRST vendItemCostLevel WHERE ROWID(vendItemCostLevel) EQ iprRowid2 NO-LOCK NO-ERROR  .
        IF AVAIL vendItemCostLevel THEN
             ASSIGN 
                dFrom:SCREEN-VALUE       = STRING(vendItemCostLevel.quantityFrom)
                dTo:SCREEN-VALUE         = STRING(vendItemCostLevel.quantityTo)
                dBase:SCREEN-VALUE       = STRING(vendItemCostLevel.quantityBase)
                dEaCost1:SCREEN-VALUE    = STRING(vendItemCostLevel.costPerUOM)
                dSetup1:SCREEN-VALUE     = STRING(vendItemCostLevel.costSetup)
                dDev1:SCREEN-VALUE       = STRING(vendItemCostLevel.costDeviation)
                cPriceUom:SCREEN-VALUE   = vendItemCost.vendorUOM 
                iLeadTime:SCREEN-VALUE   = STRING(vendItemCostLevel.leadTimeDays)
                tb_BestCost:SCREEN-VALUE = STRING(vendItemCostLevel.useForBestCost)
                tb_MaxQty:SCREEN-VALUE   = STRING(vendItemCostLevel.quantityTo EQ dMaxQty)
                .
        IF ipcType EQ "Copy" THEN
            dBase:SCREEN-VALUE  = "0" .
      DISABLE dFrom dTo cDevLabel.
      IF NOT lVendItemUseDeviation THEN
          ASSIGN
              dDev1:HIDDEN = TRUE
              cDevLabel:HIDDEN = TRUE .
      IF ipcType EQ "View" THEN
          DISABLE dFrom dTo dBase dEaCost1 dSetup1 dDev1 iLeadTime tb_BestCost tb_MaxQty.
        
      opRowid = iprRowid2 .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "vendItemCostLevel"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty D-Dialog 
PROCEDURE valid-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO .
DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel .
  {methods/lValidateError.i YES}
      DO WITH FRAME {&FRAME-NAME}:
          
          IF DECIMAL(dBase:SCREEN-VALUE) EQ 0  THEN DO:
              MESSAGE "Please enter Qty...." VIEW-AS ALERT-BOX ERROR.
              APPLY "entry" TO dBase.
              oplReturnError = YES .
           END.
           ELSE DO:
               FIND FIRST bf-vendItemCostLevel NO-LOCK
                   WHERE bf-vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID 
                     AND bf-vendItemCostLevel.quantityBase  EQ  DECIMAL(dBase:SCREEN-VALUE)
                     AND ROWID(bf-vendItemCostLevel) NE iprRowid2 NO-ERROR .
               IF AVAIL bf-vendItemCostLevel THEN DO:
                MESSAGE "Duplicate Entry for  Quantity To(" STRING(dBase) ")...." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO dBase.
                oplReturnError = YES .
               END.

           END.
      END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


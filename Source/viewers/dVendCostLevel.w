&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
DEFINE INPUT PARAMETER iprRowid AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER iprRowid2 AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO .
DEFINE OUTPUT PARAMETER opRowid AS ROWID NO-UNDO .

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}
/*{methods/prgsecur.i}*/
{methods/defines/globdefs.i}
DEFINE BUFFER b-prgrms FOR prgrms.
DEFINE VARIABLE v-prgmname LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.
DEFINE VARIABLE v-count    AS INTEGER NO-UNDO.
DEFINE VARIABLE k_frac     AS DECIMAL INIT 6.25 NO-UNDO.

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

DEFINE VARIABLE uom-list         AS CHARACTER     INIT "C,CS,EA,L,M," NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS dFrom1 dToQty1 dEaCost1 dSetup1 dDev1 Btn_OK ~
Btn_Cancel RECT-1 RECT-5 RECT-21 dFrom-2 
&Scoped-Define DISPLAYED-OBJECTS dFrom1 dToQty1 dEaCost1 dSetup1 dDev1 ~
dFrom-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Cancel" 
     SIZE 10 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 10 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE dDev1 AS DECIMAL FORMAT "->,>>>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dEaCost1 AS DECIMAL FORMAT ">>,>>>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dFrom-2 AS DECIMAL FORMAT "->,>>>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dFrom1 AS DECIMAL FORMAT "->,>>>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dSetup1 AS DECIMAL FORMAT "->,>>>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dToQty1 AS DECIMAL FORMAT ">>,>>>,>>9.9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 16.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 83.2 BY 6.43
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 25.8 BY 2.38.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 83.2 BY 7.05
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     dFrom1 AT ROW 3.57 COL 26.8 COLON-ALIGNED NO-LABEL WIDGET-ID 218
     dToQty1 AT ROW 3.62 COL 7.6 COLON-ALIGNED NO-LABEL WIDGET-ID 254
     dEaCost1 AT ROW 6.48 COL 7.6 COLON-ALIGNED NO-LABEL WIDGET-ID 266
     dSetup1 AT ROW 6.43 COL 26.8 COLON-ALIGNED NO-LABEL WIDGET-ID 276
     dDev1 AT ROW 6.43 COL 47.2 COLON-ALIGNED NO-LABEL WIDGET-ID 360
     Btn_OK AT ROW 9.05 COL 43.4
     Btn_Cancel AT ROW 9.05 COL 53.8
     dFrom-2 AT ROW 3.57 COL 47.2 COLON-ALIGNED NO-LABEL WIDGET-ID 380
     "Quantity Range" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 2.57 COL 38.2 WIDGET-ID 242
     "Quantity To" VIEW-AS TEXT
          SIZE 14.2 BY 1 AT ROW 2.62 COL 9.6 WIDGET-ID 252
     "Cost Per" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 5.48 COL 9.6 WIDGET-ID 264
     "Setup" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 5.43 COL 28.8 WIDGET-ID 278
     "Deviation" VIEW-AS TEXT
          SIZE 12.8 BY 1 AT ROW 5.43 COL 49.4 WIDGET-ID 288
     RECT-1 AT ROW 1.71 COL 1.8 WIDGET-ID 82
     RECT-5 AT ROW 1.1 COL 1.8 WIDGET-ID 312
     RECT-21 AT ROW 8.86 COL 41.2
     SPACE(19.39) SKIP(0.42)
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Update / Add Vendor Cost Level */
DO: 
    
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


&Scoped-define SELF-NAME dFrom-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dFrom-2 D-Dialog
ON LEAVE OF dFrom-2 IN FRAME D-Dialog
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name} .
         
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dFrom1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dFrom1 D-Dialog
ON LEAVE OF dFrom1 IN FRAME D-Dialog
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    /*{src/adm/template/dialogmn.i}*/

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
  DISPLAY dFrom1 dToQty1 dEaCost1 dSetup1 dDev1 dFrom-2 
      WITH FRAME D-Dialog.
  ENABLE dFrom1 dToQty1 dEaCost1 dSetup1 dDev1 Btn_OK Btn_Cancel RECT-1 RECT-5 
         RECT-21 dFrom-2 
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
    DEFINE VARIABLE hVendorCostProcs AS HANDLE NO-UNDO.
    DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE cReturnMessage AS CHARACTER NO-UNDO . 

    RUN system\VendorCostProcs.p PERSISTENT SET hVendorCostProcs.
   
    FIND FIRST vendItemCost WHERE ROWID(vendItemCost) EQ iprRowid  NO-LOCK NO-ERROR.

    FIND FIRST vendItemCostLevel WHERE ROWID(vendItemCostLevel) EQ iprRowid2 EXCLUSIVE-LOCK NO-ERROR  .

    DO WITH FRAME {&frame-name}:  
        ASSIGN 
            vendItemCostLevel.quantityBase    = decimal(dToQty1:SCREEN-VALUE)  
            vendItemCostLevel.costPerUOM    = decimal(dEaCost1:SCREEN-VALUE) 
            vendItemCostLevel.costSetup     = decimal(dSetup1:SCREEN-VALUE)
            vendItemCostLevel.costDeviation = decimal(dDev1:SCREEN-VALUE) . 
    END.
    opRowid = ROWID(vendItemCostLevel) .
    FIND CURRENT vendItemCostLevel NO-LOCK NO-ERROR .
    RUN RecalculateFromAndTo IN hVendorCostProcs (vendItemCost.vendItemCostID, OUTPUT lReturnError ,OUTPUT cReturnMessage ) .
    
    RELEASE vendItemCostLevel.
    DELETE OBJECT hVendorCostProcs.
    
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
    DEFINE VARIABLE hVendorCostProcs AS HANDLE NO-UNDO.
    DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE cReturnMessage AS CHARACTER NO-UNDO .
    
     RUN system\VendorCostProcs.p PERSISTENT SET hVendorCostProcs.

     FIND FIRST vendItemCost WHERE ROWID(vendItemCost) EQ iprRowid  NO-LOCK NO-ERROR.
     
    DO WITH FRAME {&frame-name}:        
        CREATE vendItemCostLevel .
        ASSIGN vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID .
        ASSIGN 
            vendItemCostLevel.quantityBase    = decimal(dToQty1:SCREEN-VALUE)  
            vendItemCostLevel.costPerUOM    = decimal(dEaCost1:SCREEN-VALUE) 
            vendItemCostLevel.costSetup     = decimal(dSetup1:SCREEN-VALUE)
            vendItemCostLevel.costDeviation = decimal(dDev1:SCREEN-VALUE) . 
    END.
    
    FIND CURRENT vendItemCostLevel NO-LOCK NO-ERROR .
    opRowid = ROWID(vendItemCostLevel) .
    
    RUN RecalculateFromAndTo IN hVendorCostProcs (vendItemCost.vendItemCostID, OUTPUT lReturnError ,OUTPUT cReturnMessage ) .

    RELEASE vendItemCostLevel.
    DELETE OBJECT hVendorCostProcs. 

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
   DO WITH FRAME {&frame-name}:
      
        APPLY "entry" TO dFrom1 IN FRAME {&FRAME-NAME}.
        
            ASSIGN Btn_OK:LABEL = "Save" .
        

        FIND FIRST vendItemCost WHERE ROWID(vendItemCost) EQ iprRowid  NO-LOCK NO-ERROR.
        
        FIND FIRST vendItemCostLevel WHERE ROWID(vendItemCostLevel) EQ iprRowid2 NO-LOCK NO-ERROR  .
        IF AVAIL vendItemCostLevel THEN
             ASSIGN 
                dFrom1:SCREEN-VALUE   = string(vendItemCostLevel.quantityFrom)
                dToQty1:SCREEN-VALUE  = string(vendItemCostLevel.quantityTo)
                dEaCost1:SCREEN-VALUE = string(vendItemCostLevel.costPerUOM)
                dSetup1:SCREEN-VALUE  = string(vendItemCostLevel.costSetup)
                dDev1:SCREEN-VALUE    = string(vendItemCostLevel.costDeviation) .
        IF ipcType EQ "Copy" THEN
            dToQty1:SCREEN-VALUE  = "0" .
      DISABLE dFrom1 dFrom-2 .
      IF ipcType EQ "View" THEN
          DISABLE dFrom1 dFrom-2 dToQty1 dEaCost1 dSetup1 dDev1 .
        
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




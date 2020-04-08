&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: est\dEstPacking.w
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-rowid  AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO .   /* add,update,view */
DEFINE OUTPUT PARAMETER op-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN 
    cocode = g_company
    locode = g_loc.

DEFINE VARIABLE char-val        AS CHARACTER NO-UNDO.

DEFINE VARIABLE lv-item-recid   AS RECID     NO-UNDO.
DEFINE VARIABLE ll-order-warned AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ll-new-record   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ilogic          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMaterialType AS CHARACTER INITIAL "C,5,6,M,D" NO-UNDO .
{Inventory/ttInventory.i "NEW SHARED"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES estPacking

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame estPacking.rmItemID ~
estPacking.materialType estPacking.quantity estPacking.quantityPer ~
estPacking.dimLength estPacking.dimWidth estPacking.dimDepth ~
estPacking.noCharge estPacking.costOverridePerUOM
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame estPacking.rmItemID ~
estPacking.quantity estPacking.quantityPer estPacking.dimLength ~
estPacking.dimWidth estPacking.noCharge estPacking.costOverridePerUOM
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame estPacking
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame estPacking
&Scoped-define TABLES-IN-QUERY-Dialog-Frame estPacking
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame estPacking


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS estPacking.rmItemID estPacking.quantity ~
estPacking.quantityPer estPacking.dimLength estPacking.dimWidth  ~
estPacking.noCharge estPacking.costOverridePerUOM
&Scoped-define ENABLED-TABLES estPacking
&Scoped-define FIRST-ENABLED-TABLE estPacking
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 ~
RECT-39 
&Scoped-Define DISPLAYED-FIELDS estPacking.rmItemID estPacking.materialType ~
estPacking.quantity estPacking.quantityPer estPacking.dimLength ~
estPacking.dimWidth estPacking.dimDepth estPacking.noCharge estPacking.costOverridePerUOM
&Scoped-define DISPLAYED-TABLES estPacking
&Scoped-define FIRST-DISPLAYED-TABLE estPacking
&Scoped-Define DISPLAYED-OBJECTS est-no iForm iBlank cCustPart cCase ~
cPallet fi_mat-name fi_type-name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetItemName Dialog-Frame 
FUNCTION fGetItemName RETURNS CHARACTER
    ( ipcItem AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetType Dialog-Frame 
FUNCTION fGetType RETURNS CHARACTER
    ( ipcItem AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTypeName Dialog-Frame 
FUNCTION fGetTypeName RETURNS CHARACTER
    ( ipcItem AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE VARIABLE cCase AS CHARACTER FORMAT "X(8)":U 
     LABEL "Case" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCustPart AS CHARACTER FORMAT "X(15)":U 
     LABEL "Part#" 
     VIEW-AS FILL-IN 
     SIZE 25.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cPallet AS CHARACTER FORMAT "X(25)":U 
     LABEL "Pallet" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE est-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 12.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_mat-name AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 27.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_type-name AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 27.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iBlank AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Blank #" 
     VIEW-AS FILL-IN 
     SIZE 5.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE iForm AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Form #" 
     VIEW-AS FILL-IN 
     SIZE 5.8 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 19 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 104.8 BY 2.71
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 104.8 BY 5.71
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      estPacking SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     est-no AT ROW 1.43 COL 13.8 COLON-ALIGNED WIDGET-ID 200
     iForm AT ROW 1.43 COL 37.4 COLON-ALIGNED WIDGET-ID 314
     iBlank AT ROW 1.43 COL 55.2 COLON-ALIGNED WIDGET-ID 316
     cCustPart AT ROW 2.62 COL 13.8 COLON-ALIGNED WIDGET-ID 176
     cCase AT ROW 1.48 COL 83.2 COLON-ALIGNED WIDGET-ID 178
     cPallet AT ROW 2.67 COL 83.2 COLON-ALIGNED WIDGET-ID 204
     estPacking.rmItemID AT ROW 4.62 COL 22.2 COLON-ALIGNED
          LABEL "Item ID" FORMAT "x(10)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fi_mat-name AT ROW 4.62 COL 40.8 COLON-ALIGNED NO-LABEL
     estPacking.materialType AT ROW 5.81 COL 22.2 COLON-ALIGNED
          LABEL "Type"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fi_type-name AT ROW 5.81 COL 40.8 COLON-ALIGNED NO-LABEL
     estPacking.quantity AT ROW 7 COL 22.2 COLON-ALIGNED
          LABEL "Quantity" FORMAT ">>>,>>9.9<<"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 1
     estPacking.quantityPer AT ROW 7.1 COL 49 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 4
          LIST-ITEM-PAIRS "Case","C",
                     "Pallet","P",
                     "Lot","L",
                     "EACH","E"
          DROP-DOWN-LIST
          SIZE 12 BY 1
          BGCOLOR 15 FONT 1
     estPacking.dimLength AT ROW 4.62 COL 83.8 COLON-ALIGNED
          LABEL "Length" FORMAT ">9.9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     estPacking.dimWidth AT ROW 5.81 COL 83.8 COLON-ALIGNED
          LABEL "Width" FORMAT ">9.9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     estPacking.dimDepth AT ROW 7.1 COL 83.8 COLON-ALIGNED
          LABEL "Depth" FORMAT ">9.9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     Btn_OK AT ROW 10.33 COL 88.2
     Btn_Done AT ROW 10.62 COL 89.2
     Btn_Cancel AT ROW 10.33 COL 97.2       
     estPacking.noCharge AT ROW 8.39 COL 49 COLON-ALIGNED WIDGET-ID 320
          LABEL "NC" FORMAT "Y/N"
          VIEW-AS TOGGLE-BOX 
          SIZE 9.6 BY 1
          BGCOLOR 15 FONT 1
     estPacking.costOverridePerUOM AT ROW 8.39 COL 83.8 COLON-ALIGNED WIDGET-ID 318
          LABEL "Override"  FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1     
     RECT-21 AT ROW 10.1 COL 87.2
     RECT-38 AT ROW 1.14 COL 1.2
     RECT-39 AT ROW 4.1 COL 1.2 WIDGET-ID 2
     SPACE(1.79) SKIP(3.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Add/Update Packing Material".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
/*{methods/template/viewer.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cCase IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cCustPart IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN estPacking.costOverridePerUOM IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */

/* SETTINGS FOR FILL-IN cPallet IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN estPacking.dimDepth IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN estPacking.dimLength IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estPacking.dimWidth IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_mat-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_type-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iBlank IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iForm IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN estPacking.materialType IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN estPacking.noCharge IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */


/* SETTINGS FOR FILL-IN estPacking.quantity IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       RECT-39:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN estPacking.rmItemID IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.estPacking "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.estPacking.company eq cocode "
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Add/Update Packing Material */
DO:
        DEFINE VARIABLE char-val   AS cha    NO-UNDO.
        DEFINE VARIABLE lv-handle  AS HANDLE NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID  NO-UNDO .
        
        CASE FOCUS:NAME :
            WHEN "rmItemID" THEN 
                DO:
                    RUN windows/l-item.w (eb.company,"",cMaterialType,FOCUS:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" AND estPacking.rmItemID:SCREEN-VALUE NE entry(1,char-val) THEN 
                    DO:
                        estPacking.rmItemID:SCREEN-VALUE = ENTRY(1,char-val).
                        
                        RUN value-change-material.

                        ASSIGN 
                            fi_mat-name:SCREEN-VALUE = fGetItemName(estPacking.rmItemID:SCREEN-VALUE) .
                        estPacking.materialType:SCREEN-VALUE = fGetType(estPacking.rmItemID:SCREEN-VALUE) .
                        fi_type-name:SCREEN-VALUE = fGetTypeName(estPacking.materialType:SCREEN-VALUE) .
                    END.
                    RETURN NO-APPLY.   
                END.
            
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Add/Update Packing Material */
ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Add/Update Packing Material */
DO:
        DISABLE TRIGGERS FOR LOAD OF estPacking .
    
        IF AVAILABLE estPacking THEN
            op-rowid = ROWID(estPacking) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST estPacking EXCLUSIVE-LOCK
                WHERE RECID(estPacking) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE estPacking THEN DELETE estPacking .
            op-rowid = ? .
        END.
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.

    /*APPLY "END-ERROR":U TO SELF.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
        DISABLE TRIGGERS FOR LOAD OF estPacking .
    
        IF AVAILABLE estPacking THEN
            op-rowid = ROWID(estPacking) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST estPacking EXCLUSIVE-LOCK
                WHERE RECID(estPacking) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE estPacking THEN DELETE estPacking .
            op-rowid = ? .
        END.
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
DO:
        IF AVAILABLE estPacking THEN
            ASSIGN op-rowid = ROWID(estPacking) .
  &IF DEFINED (adm-panel) NE 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
        APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Save */
DO:
        DEFINE VARIABLE ld              AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE lValidateResult AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lError          AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
        DEFINE VARIABLE dCostStorage    AS DECIMAL   NO-UNDO .
        DEFINE VARIABLE dCostHandling   AS DECIMAL   NO-UNDO .
        DEFINE VARIABLE hftp            AS HANDLE    NO-UNDO.

        
        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
       
        RUN valid-material(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.

       
        DO TRANSACTION:
            FIND CURRENT estPacking EXCLUSIVE-LOCK NO-ERROR.

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
            END.
            estPacking.costOverrideUOM = "EA".
        END.
        
        FIND CURRENT estPacking NO-LOCK NO-ERROR .
        op-rowid = ROWID(estPacking).

        APPLY "go" TO FRAME {&FRAME-NAME}.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME estPacking.quantity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estPacking.quantity Dialog-Frame
ON LEAVE OF estPacking.quantity IN FRAME Dialog-Frame /* Quantity */
DO:
        
        IF LASTKEY NE -1 THEN 
        DO:     
          
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estPacking.quantity Dialog-Frame
ON VALUE-CHANGED OF estPacking.quantity IN FRAME Dialog-Frame /* Quantity */
DO:
    /*ASSIGN estPacking.quantityPer:SCREEN-VALUE = estPacking.quantity:SCREEN-VALUE .
    RUN pCalAllUnit .*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME estPacking.rmItemID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estPacking.rmItemID Dialog-Frame
ON LEAVE OF estPacking.rmItemID IN FRAME Dialog-Frame /* Item ID */
DO:
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO.
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-material( OUTPUT lValidateResult) NO-ERROR.
            IF lValidateResult THEN RETURN NO-APPLY.

            ASSIGN 
                fi_mat-name:SCREEN-VALUE = fGetItemName(estPacking.rmItemID:SCREEN-VALUE) . 
            estPacking.materialType:SCREEN-VALUE = fGetType(estPacking.rmItemID:SCREEN-VALUE) .
            fi_type-name:SCREEN-VALUE = fGetTypeName(estPacking.materialType:SCREEN-VALUE) .
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estPacking.rmItemID Dialog-Frame
ON VALUE-CHANGED OF estPacking.rmItemID IN FRAME Dialog-Frame /* Item ID */
DO:
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO.
        IF LASTKEY NE -1 THEN 
        DO:
            RUN value-change-material .

            ASSIGN 
                fi_mat-name:SCREEN-VALUE = fGetItemName(estPacking.rmItemID:SCREEN-VALUE) . 
            estPacking.materialType:SCREEN-VALUE = fGetType(estPacking.rmItemID:SCREEN-VALUE) .
            fi_type-name:SCREEN-VALUE = fGetTypeName(estPacking.materialType:SCREEN-VALUE) .
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i} 
SESSION:DATA-ENTRY-RETURN = YES.       

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    FIND FIRST eb NO-LOCK
        WHERE ROWID(eb) EQ ip-rowid NO-ERROR .
    
    IF ip-type EQ "copy" THEN lv-item-recid = ip-recid.

    IF ip-recid EQ ? THEN 
    DO:
        RUN create-item.
    END.
    ELSE FIND estPacking NO-LOCK WHERE RECID(estPacking) EQ ip-recid NO-ERROR.

    IF ip-type NE "view" THEN 
    DO: 
        
        RUN enable_UI.
        RUN display-item.

        ASSIGN 
            ll-order-warned = NO.
        btn_done:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE 
    DO:
        RUN display-item.
        ASSIGN 
            btn_done:HIDDEN IN FRAME {&FRAME-NAME} = NO.
        btn_done:SENSITIVE                        = YES.
        btn_ok:HIDDEN                             = YES.
        btn_cancel:HIDDEN                         = YES.
    END.
    FIND CURRENT estPacking NO-LOCK NO-ERROR .
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item Dialog-Frame 
PROCEDURE create-item :
/*------------------------------------------------------------------------------
                  Purpose:     
                  PARAMs:  <none>
                  Notes:       
                ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rno LIKE estPacking.estPackingID NO-UNDO.
    DEFINE BUFFER b-estPacking FOR estPacking.
    DEFINE VARIABLE hftp            AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iestPackingID   AS INTEGER   NO-UNDO .
    DEFINE VARIABLE lCreated        AS LOGICAL   NO-UNDO .
    DEFINE VARIABLE cCreatedMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dPalletQty      AS DECIMAL   NO-UNDO .
    
    DO WITH FRAME {&FRAME-NAME}:
        CREATE estPacking .
        ASSIGN 
            estPacking.company      = eb.company 
            estPacking.estimateNo   = eb.est-no
            estPacking.FormNo       = eb.form-no
            estPacking.BlankNo      = eb.blank-No
            estPacking.quantityPer  = "C"
            . 

        IF AVAILABLE estPacking THEN 
        DO:
            DISPLAY  estPacking.rmItemID estPacking.quantity
                estPacking.dimDepth estPacking.dimWidth estPacking.dimLength
                estPacking.quantity. 
            ASSIGN 
                lv-item-recid = RECID(estPacking).
            ll-new-record = YES.

        END. /* avail estPacking */
    END. /* avail eb */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item Dialog-Frame 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
                  Purpose:     
                  PARAMs:  <none>
                  Notes:       
                ------------------------------------------------------------------------------*/
    
    IF AVAILABLE estPacking  THEN 
    DO:
        /*FIND FIRST ITEM  NO-LOCK
            WHERE ITEM.company EQ cocode
            AND ITEM.i-no EQ estPacking.rmItemID NO-ERROR .
        IF AVAIL ITEM THEN*/
        ASSIGN 
            fi_mat-name = fGetItemName(estPacking.rmItemID) .
        fi_type-name = fGetTypeName(estPacking.materialType) .
        IF AVAILABLE eb THEN
            ASSIGN
                est-no    = eb.est-no 
                cCustPart = eb.part-no
                cCase     = eb.cas-no
                iForm     = (eb.form-no)
                iBlank    = (eb.blank-no)
                cPallet   = eb.tr-no .

        DISPLAY estPacking.quantity estPacking.quantityPer 
            estPacking.rmItemID estPacking.quantity
            estPacking.dimLength estPacking.dimWidth estPacking.dimDepth  
            fi_mat-name est-no cCustPart cCase iForm iBlank cPallet 
            fi_type-name estPacking.noCharge estPacking.costOverridePerUOM
            WITH FRAME Dialog-Frame.
    END.

    IF ip-type NE "view" THEN 
    DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.
    END.

    VIEW FRAME {&FRAME-NAME}. 
    APPLY "entry" TO FRAME {&FRAME-NAME}.
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY est-no iForm iBlank cCustPart cCase cPallet fi_mat-name fi_type-name 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE estPacking THEN 
    DISPLAY estPacking.rmItemID estPacking.materialType estPacking.quantity 
          estPacking.quantityPer estPacking.dimLength estPacking.dimWidth 
          estPacking.dimDepth estPacking.noCharge estPacking.costOverridePerUOM
      WITH FRAME Dialog-Frame.
  ENABLE estPacking.rmItemID estPacking.quantity estPacking.quantityPer 
         estPacking.dimLength estPacking.dimWidth estPacking.noCharge  
         estPacking.costOverridePerUOM Btn_OK  Btn_Done Btn_Cancel RECT-21
         RECT-38 RECT-39 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit Dialog-Frame 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/


    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

/* Code placed here will execute AFTER standard behavior.    */
    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-material Dialog-Frame 
PROCEDURE valid-material :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcValidError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST item NO-LOCK
            WHERE item.company EQ cocode
            AND item.i-no    EQ estPacking.rmItemID:SCREEN-VALUE 
            AND lookup(item.mat-type,cMaterialType) > 0 NO-ERROR.
        IF NOT AVAILABLE ITEM THEN 
        DO:
            MESSAGE "Invalid Item Id, try help..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO estPacking.rmItemID .
            opcValidError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-change-material Dialog-Frame 
PROCEDURE value-change-material :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST item NO-LOCK
            WHERE item.company EQ cocode
            AND item.i-no    EQ estPacking.rmItemID:SCREEN-VALUE 
            AND lookup(item.mat-type,cMaterialType) > 0 NO-ERROR.
        IF AVAILABLE item THEN 
        DO:
            ASSIGN
                estPacking.dimLength:Screen-value = STRING(item.case-l)
                estPacking.dimWidth:Screen-value  = STRING(item.case-w)
                estPacking.dimDepth:Screen-value  = STRING(item.case-d) .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetItemName Dialog-Frame 
FUNCTION fGetItemName RETURNS CHARACTER
    ( ipcItem AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO .
    FIND FIRST ITEM  NO-LOCK
        WHERE ITEM.company EQ cocode
        AND ITEM.i-no EQ ipcItem NO-ERROR .
    IF AVAILABLE ITEM THEN
        ASSIGN cReturn = ITEM.i-NAME .
    RETURN cReturn .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetType Dialog-Frame 
FUNCTION fGetType RETURNS CHARACTER
    ( ipcItem AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO .
    FIND FIRST ITEM  NO-LOCK
        WHERE ITEM.company EQ cocode
        AND ITEM.i-no EQ ipcItem NO-ERROR .
    IF AVAILABLE ITEM THEN
        ASSIGN cReturn = item.mat-type .
    RETURN cReturn .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTypeName Dialog-Frame 
FUNCTION fGetTypeName RETURNS CHARACTER
    ( ipcItem AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO .

    FIND mat WHERE mat.mat EQ ipcItem NO-LOCK NO-ERROR.
    IF AVAILABLE mat THEN 
        ASSIGN cReturn = mat.dscr.
    
    RETURN cReturn .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


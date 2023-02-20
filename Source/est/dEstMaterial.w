&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: est\destMaterial.w
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

DEFINE VARIABLE char-val           AS CHARACTER                     NO-UNDO.

DEFINE VARIABLE lv-item-recid      AS RECID                         NO-UNDO.
DEFINE VARIABLE ll-order-warned    AS LOGICAL                       NO-UNDO.
DEFINE VARIABLE ll-new-record      AS LOGICAL                       NO-UNDO.
DEFINE VARIABLE ilogic             AS LOGICAL                       NO-UNDO.
DEFINE VARIABLE lRecFound          AS LOGICAL                       NO-UNDO.
DEFINE VARIABLE cReturnValue       AS CHARACTER                     NO-UNDO.
DEFINE VARIABLE lUseVendItemCost   AS LOGICAL                       NO-UNDO.
DEFINE VARIABLE cCEVersion         AS CHARACTER                     NO-UNDO.
DEFINE VARIABLE cmaterialTypeID    AS CHARACTER INITIAL "C,5,6,M,D" NO-UNDO .
{Inventory/ttInventory.i "NEW SHARED"}

RUN sys/ref/nk1look.p (cocode, "CEVersion", "C" /* Character */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnValue, OUTPUT lRecFound).
   
ASSIGN cCEVersion = cReturnValue .

RUN sys/ref/nk1look.p (
            INPUT cocode, /* Company Code */ 
            INPUT "VendItemCost", /* sys-ctrl name */
            INPUT "L",            /* Output return value */
            INPUT NO,             /* Use lship-to */
            INPUT NO,             /* ship-to vendor */
            INPUT "",             /* ship-to vendor value */
            INPUT "",             /* shi-id value */
            OUTPUT cReturnValue, 
            OUTPUT lRecFound
            ).       
        lUseVendItemCost = LOGICAL(cReturnValue) NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES estMaterial

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame estMaterial.itemID ~
estMaterial.materialTypeID estMaterial.quantity estMaterial.quantityUOM ~
estMaterial.quantityPer estMaterial.wastePercent ~
estMaterial.costOverridePerUOM estMaterial.costOverrideUOM ~
estMaterial.noCharge estMaterial.dimLength estMaterial.dimWidth ~
estMaterial.dimDepth estMaterial.weightPerEA estMaterial.formNo ~
estMaterial.blankNo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame estMaterial.itemID ~
estMaterial.quantity estMaterial.quantityPer estMaterial.wastePercent ~
estMaterial.costOverridePerUOM estMaterial.costOverrideUOM ~
estMaterial.noCharge estMaterial.dimLength estMaterial.dimWidth ~
estMaterial.dimDepth estMaterial.weightPerEA estMaterial.formNo ~
estMaterial.blankNo 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame estMaterial
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame estMaterial
&Scoped-define TABLES-IN-QUERY-Dialog-Frame estMaterial
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame estMaterial


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS estMaterial.itemID estMaterial.quantity ~
estMaterial.quantityPer estMaterial.wastePercent ~
estMaterial.costOverridePerUOM estMaterial.costOverrideUOM ~
estMaterial.noCharge estMaterial.dimLength estMaterial.dimWidth ~
estMaterial.dimDepth estMaterial.weightPerEA estMaterial.formNo ~
estMaterial.blankNo 
&Scoped-define ENABLED-TABLES estMaterial
&Scoped-define FIRST-ENABLED-TABLE estMaterial
&Scoped-Define ENABLED-OBJECTS OverrideExist Btn_OK Btn_Done Btn_Cancel ~
RECT-21 RECT-38 RECT-39 
&Scoped-Define DISPLAYED-FIELDS estMaterial.itemID ~
estMaterial.materialTypeID estMaterial.quantity estMaterial.quantityUOM ~
estMaterial.quantityPer estMaterial.wastePercent ~
estMaterial.costOverridePerUOM estMaterial.costOverrideUOM ~
estMaterial.noCharge estMaterial.dimLength estMaterial.dimWidth ~
estMaterial.dimDepth estMaterial.weightPerEA estMaterial.formNo ~
estMaterial.blankNo 
&Scoped-define DISPLAYED-TABLES estMaterial
&Scoped-define FIRST-DISPLAYED-TABLE estMaterial
&Scoped-Define DISPLAYED-OBJECTS fi_mat-name fi_type-name est-no 

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
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save" 
     SIZE 8 BY 1.91
     BGCOLOR 8 .

DEFINE BUTTON OverrideExist  NO-FOCUS FLAT-BUTTON
     LABEL "OverrideExist" 
     SIZE 30 BY 1.91
     BGCOLOR 8 .

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

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 19 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 104.8 BY 1.62
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 104.8 BY 5.19
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      estMaterial SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     estMaterial.itemID AT ROW 2.91 COL 13.6 COLON-ALIGNED
          LABEL "Item ID" FORMAT "x(10)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fi_mat-name AT ROW 2.91 COL 31.6 COLON-ALIGNED NO-LABEL
     estMaterial.materialTypeID AT ROW 2.91 COL 67 COLON-ALIGNED
          LABEL "Type"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
          BGCOLOR 15 FONT 1
     fi_type-name AT ROW 2.91 COL 73.4 COLON-ALIGNED NO-LABEL
     estMaterial.quantity AT ROW 4 COL 13.8 COLON-ALIGNED
          LABEL "Quantity" FORMAT ">>>,>>9.9<<<<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     estMaterial.quantityUOM AT ROW 4 COL 31.6 COLON-ALIGNED NO-LABEL WIDGET-ID 318
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
          BGCOLOR 15 FONT 1
     estMaterial.quantityPer AT ROW 4 COL 42.6 COLON-ALIGNED
          LABEL "Per"
          VIEW-AS COMBO-BOX INNER-LINES 4
          LIST-ITEM-PAIRS "Each","E",
                     "Case","C",
                     "Lot","L",
                     "Pallet","P",
                     "Set","S"
          DROP-DOWN-LIST
          SIZE 12 BY 1
          BGCOLOR 15 FONT 1
     estMaterial.wastePercent AT ROW 4.1 COL 84 COLON-ALIGNED WIDGET-ID 322
          LABEL "Waste %"
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
          FONT 1
     OverrideExist AT ROW 5.91 COL 13.6
     Btn_OK AT ROW 11.24 COL 88.2
     Btn_Done AT ROW 11.52 COL 89.2
     Btn_Cancel AT ROW 11.24 COL 97.2
     estMaterial.costOverridePerUOM AT ROW 6.62 COL 13.6 COLON-ALIGNED WIDGET-ID 318
          LABEL "Cost" FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     estMaterial.costOverrideUOM AT ROW 6.62 COL 42.6 COLON-ALIGNED WIDGET-ID 318
          LABEL "Per" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
          BGCOLOR 15 FONT 1
     estMaterial.noCharge AT ROW 6.62 COL 53.6 WIDGET-ID 320
          LABEL "NC"
          VIEW-AS TOGGLE-BOX
          SIZE 12 BY 1
          FONT 1
     estMaterial.dimLength AT ROW 5.81 COL 83.8 COLON-ALIGNED
          LABEL "Length" FORMAT ">>>>>9.9999<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     estMaterial.dimWidth AT ROW 7.05 COL 83.8 COLON-ALIGNED
          LABEL "Width" FORMAT ">>>>>9.9999<<"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     estMaterial.dimDepth AT ROW 8.24 COL 83.8 COLON-ALIGNED
          LABEL "Depth" FORMAT ">9.9999"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     estMaterial.weightPerEA AT ROW 9.43 COL 83.8 COLON-ALIGNED WIDGET-ID 318
          LABEL "Weight Per EA" FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     est-no AT ROW 1.43 COL 13.8 COLON-ALIGNED WIDGET-ID 200
     estMaterial.formNo AT ROW 1.43 COL 37.4 COLON-ALIGNED
          LABEL "Form#" FORMAT "9"
          VIEW-AS FILL-IN 
          SIZE 5.8 BY 1
          BGCOLOR 15 FONT 1
     estMaterial.blankNo AT ROW 1.43 COL 55.2 COLON-ALIGNED
          LABEL "Blank#" FORMAT "9"
          VIEW-AS FILL-IN 
          SIZE 5.8 BY 1
          BGCOLOR 15 FONT 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     " Item Overrides" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 5.19 COL 4
          FGCOLOR 1 FONT 6
     RECT-21 AT ROW 11 COL 87.2
     RECT-38 AT ROW 1.14 COL 1.2
     RECT-39 AT ROW 5.57 COL 1.2 WIDGET-ID 2
     SPACE(1.79) SKIP(6.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Add/Update Additional Materials".


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

/* SETTINGS FOR FILL-IN estMaterial.blankNo IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estMaterial.costOverridePerUOM IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estMaterial.costOverrideUOM IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estMaterial.dimDepth IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estMaterial.dimLength IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estMaterial.dimWidth IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_mat-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_type-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN estMaterial.formNo IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estMaterial.itemID IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estMaterial.materialTypeID IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX estMaterial.noCharge IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN estMaterial.quantity IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR COMBO-BOX estMaterial.quantityPer IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN estMaterial.quantityUOM IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       RECT-39:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN estMaterial.wastePercent IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN estMaterial.weightPerEA IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.estMaterial "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.estMaterial.company eq cocode "
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Add/Update Additional Materials */
DO:
        DEFINE VARIABLE char-val   AS cha    NO-UNDO.
        DEFINE VARIABLE lv-handle  AS HANDLE NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID  NO-UNDO .
        
        CASE FOCUS:NAME :
            WHEN "itemID" THEN 
                DO:
                    RUN windows/l-item.w (eb.company,"",cmaterialTypeID,FOCUS:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" AND estMaterial.itemID:SCREEN-VALUE NE entry(1,char-val) THEN 
                    DO:
                        estMaterial.itemID:SCREEN-VALUE = ENTRY(1,char-val).
                        
                        RUN value-change-material.

                        ASSIGN 
                            fi_mat-name:SCREEN-VALUE = fGetItemName(estMaterial.itemID:SCREEN-VALUE) .
                        estMaterial.materialTypeID:SCREEN-VALUE = fGetType(estMaterial.itemID:SCREEN-VALUE) .
                        fi_type-name:SCREEN-VALUE = fGetTypeName(estMaterial.materialTypeID:SCREEN-VALUE) .
                    END.
                    RETURN NO-APPLY.   
                END.
            
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Add/Update Additional Materials */
ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Add/Update Additional Materials */
DO:
        DISABLE TRIGGERS FOR LOAD OF estMaterial .
    
        IF AVAILABLE estMaterial THEN
            op-rowid = ROWID(estMaterial) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST estMaterial EXCLUSIVE-LOCK
                WHERE RECID(estMaterial) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE estMaterial THEN DELETE estMaterial .
            op-rowid = ? .
        END.
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.

    /*APPLY "END-ERROR":U TO SELF.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME estMaterial.blankNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estMaterial.blankNo Dialog-Frame
ON LEAVE OF estMaterial.blankNo IN FRAME Dialog-Frame /* Blank# */
DO:
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO.
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-blank( OUTPUT lValidateResult) NO-ERROR.
            IF lValidateResult THEN RETURN NO-APPLY.
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
        DISABLE TRIGGERS FOR LOAD OF estMaterial .
    
        IF AVAILABLE estMaterial THEN
            op-rowid = ROWID(estMaterial) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST estMaterial EXCLUSIVE-LOCK
                WHERE RECID(estMaterial) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE estMaterial THEN DELETE estMaterial .
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
        IF AVAILABLE estMaterial THEN
            ASSIGN op-rowid = ROWID(estMaterial) .
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
       
        RUN valid-form(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.
        
        RUN valid-blank(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.
       
        RUN valid-material(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.
        
        DO TRANSACTION:
            FIND CURRENT estMaterial EXCLUSIVE-LOCK NO-ERROR.

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
            END.
            estMaterial.quantityUOM     = "EA".
        END.
        
        FIND CURRENT estMaterial NO-LOCK NO-ERROR .
        op-rowid = ROWID(estMaterial).

        APPLY "go" TO FRAME {&FRAME-NAME}.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME estMaterial.formNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estMaterial.formNo Dialog-Frame
ON LEAVE OF estMaterial.formNo IN FRAME Dialog-Frame /* Form# */
DO:
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO.
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-form( OUTPUT lValidateResult) NO-ERROR.
            IF lValidateResult THEN RETURN NO-APPLY.
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME estMaterial.itemID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estMaterial.itemID Dialog-Frame
ON LEAVE OF estMaterial.itemID IN FRAME Dialog-Frame /* Item ID */
DO:
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO.
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-material( OUTPUT lValidateResult) NO-ERROR.
            IF lValidateResult THEN RETURN NO-APPLY.

            ASSIGN 
                fi_mat-name:SCREEN-VALUE = fGetItemName(estMaterial.itemID:SCREEN-VALUE) . 
            estMaterial.materialTypeID:SCREEN-VALUE = fGetType(estMaterial.itemID:SCREEN-VALUE) .
            fi_type-name:SCREEN-VALUE = fGetTypeName(estMaterial.materialTypeID:SCREEN-VALUE) .
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estMaterial.itemID Dialog-Frame
ON VALUE-CHANGED OF estMaterial.itemID IN FRAME Dialog-Frame /* Item ID */
DO:
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO.
        IF LASTKEY NE -1 THEN 
        DO:
            RUN value-change-material .

            ASSIGN 
                fi_mat-name:SCREEN-VALUE = fGetItemName(estMaterial.itemID:SCREEN-VALUE) . 
            estMaterial.materialTypeID:SCREEN-VALUE = fGetType(estMaterial.itemID:SCREEN-VALUE) .
            fi_type-name:SCREEN-VALUE = fGetTypeName(estMaterial.materialTypeID:SCREEN-VALUE) .
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME estMaterial.quantity
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estMaterial.quantity Dialog-Frame
ON LEAVE OF estMaterial.quantity IN FRAME Dialog-Frame /* Quantity */
DO:
        
        IF LASTKEY NE -1 THEN 
        DO:     
          
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estMaterial.quantity Dialog-Frame
ON VALUE-CHANGED OF estMaterial.quantity IN FRAME Dialog-Frame /* Quantity */
DO:
    /*ASSIGN estMaterial.quantityPer:SCREEN-VALUE = estMaterial.quantity:SCREEN-VALUE .
    RUN pCalAllUnit .*/
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
    ELSE FIND estMaterial WHERE RECID(estMaterial) EQ ip-recid NO-ERROR.

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
            btn_done:HIDDEN IN FRAME {&FRAME-NAME}    = NO.
            btn_done:SENSITIVE                        = YES.
            btn_ok:HIDDEN                             = YES.
            btn_cancel:HIDDEN                         = YES.
    END.

    IF lUseVendItemCost AND cCEVersion EQ "New" THEN
    DO:
     // Btn_CostPerQty:HIDDEN IN FRAME {&FRAME-NAME}  = FALSE .
      RUN pShowHideCostFiled.      
    END.                                    
    ELSE ASSIGN
       //  Btn_CostPerQty:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         OverrideExist:HIDDEN IN FRAME {&FRAME-NAME} = TRUE .  
    
    FIND CURRENT estMaterial NO-LOCK NO-ERROR .
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
    DEFINE VARIABLE lv-rno LIKE estMaterial.estMaterialID NO-UNDO.
    DEFINE BUFFER b-estMaterial     FOR estMaterial.
    DEFINE VARIABLE hftp            AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iestMaterialID  AS INTEGER   NO-UNDO .
    DEFINE VARIABLE lCreated        AS LOGICAL   NO-UNDO .
    DEFINE VARIABLE cCreatedMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dPalletQty      AS DECIMAL   NO-UNDO .
    
    DO WITH FRAME {&FRAME-NAME}:
        CREATE estMaterial .
        ASSIGN 
            estMaterial.company         = eb.company 
            estMaterial.estimateNo      = eb.est-no
            estMaterial.FormNo          = eb.form-no
            estMaterial.BlankNo         = eb.blank-No
            estMaterial.quantityPer     = "C"
            estMaterial.quantityUOM     = "EA"
            estMaterial.costOverrideUOM = "EA"
            . 
       
        IF AVAILABLE estMaterial THEN 
        DO:
            DISPLAY estMaterial.FormNo estMaterial.BlankNo estMaterial.itemID 
                estMaterial.quantity estMaterial.dimDepth estMaterial.dimWidth
                estMaterial.dimLength estMaterial.quantity. 
            ASSIGN 
                lv-item-recid = RECID(estMaterial).
            ll-new-record = YES.

        END. /* avail estMaterial */
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
    
    IF AVAILABLE estMaterial  THEN 
    DO:
        /*FIND FIRST ITEM  NO-LOCK
            WHERE ITEM.company EQ cocode
            AND ITEM.i-no EQ estMaterial.itemID NO-ERROR .
        IF AVAIL ITEM THEN*/
        ASSIGN 
            fi_mat-name = fGetItemName(estMaterial.itemID) .
        fi_type-name = fGetTypeName(estMaterial.materialTypeID) .
        IF AVAILABLE eb THEN
            ASSIGN
                est-no                  = eb.est-no 
                estMaterial.quantityUOM = "EA".

        DISPLAY estMaterial.FormNo estMaterial.BlankNo estMaterial.quantity  
            estMaterial.quantityPer estMaterial.itemID estMaterial.quantity
            estMaterial.dimLength estMaterial.dimWidth estMaterial.dimDepth  
            fi_mat-name est-no fi_type-name estMaterial.noCharge 
            estMaterial.costOverridePerUOM  estMaterial.costOverrideUOM 
            estMaterial.weightPerEA estMaterial.quantityUOM
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
  DISPLAY fi_mat-name fi_type-name est-no 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE estMaterial THEN 
    DISPLAY estMaterial.itemID estMaterial.materialTypeID estMaterial.quantity 
          estMaterial.quantityUOM estMaterial.quantityPer 
          estMaterial.wastePercent estMaterial.costOverridePerUOM 
          estMaterial.costOverrideUOM estMaterial.noCharge estMaterial.dimLength 
          estMaterial.dimWidth estMaterial.dimDepth estMaterial.weightPerEA 
          estMaterial.formNo estMaterial.blankNo 
      WITH FRAME Dialog-Frame.
  ENABLE estMaterial.itemID estMaterial.quantity estMaterial.quantityPer 
         estMaterial.wastePercent OverrideExist Btn_OK Btn_Done Btn_Cancel 
         estMaterial.costOverridePerUOM estMaterial.costOverrideUOM 
         estMaterial.noCharge estMaterial.dimLength estMaterial.dimWidth 
         estMaterial.dimDepth estMaterial.weightPerEA estMaterial.formNo 
         estMaterial.blankNo RECT-21 RECT-38 RECT-39 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShowHideCostFiled Dialog-Frame 
PROCEDURE pShowHideCostFiled :
/*------------------------------------------------------------------------------
  Purpose:    
  Parameters:  <none>
  Notes:      
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hdVendorCostProcs     AS HANDLE    NO-UNDO.
  DEFIN VARIABLE lHideCost AS LOGICAL NO-UNDO.
  RUN system\VendorCostProcs.p PERSISTENT SET hdVendorCostProcs.
 
  DO WITH FRAME {&FRAME-NAME}:
    lHideCost = DYNAMIC-FUNCTION("fVendCostHasEstimateOverride" IN hdVendorCostProcs,cocode,est-no:SCREEN-VALUE,INTEGER(estMaterial.formNo:SCREEN-VALUE), estMaterial.itemID:SCREEN-VALUE ).
    IF lHideCost THEN
    DO:
      OverrideExist:HIDDEN = FALSE .
      estMaterial.costOverridePerUOM:HIDDEN = TRUE.
      estMaterial.costOverrideUOM:HIDDEN = TRUE.
    END.
    ELSE do:
      OverrideExist:HIDDEN = TRUE .
      estMaterial.costOverridePerUOM:HIDDEN = FALSE.
      estMaterial.costOverrideUOM:HIDDEN = FALSE.
    END.      
  END.
  DELETE OBJECT hdVendorCostProcs.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-blank Dialog-Frame 
PROCEDURE valid-blank :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcValidError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST eb NO-LOCK
            WHERE eb.company EQ cocode
            AND eb.est-no    EQ est-no:SCREEN-VALUE
            AND eb.form-no   EQ INTEGER(estMaterial.formNo:SCREEN-VALUE)
            AND eb.blank-no  EQ INTEGER(estMaterial.blankNo:SCREEN-VALUE)
            NO-ERROR.
        IF NOT AVAILABLE eb THEN 
        DO:
            MESSAGE "Invalid Blank No..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO estMaterial.blankNo .
            opcValidError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-form Dialog-Frame 
PROCEDURE valid-form :
/*------------------------------------------------------------------------------
              Purpose:     
              Parameters:  <none>
              Notes:       
            ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcValidError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST eb NO-LOCK
            WHERE eb.company EQ cocode
            AND eb.est-no    EQ est-no:SCREEN-VALUE
            AND eb.form-no   EQ INTEGER(estMaterial.formNo:SCREEN-VALUE)
            NO-ERROR.
        IF NOT AVAILABLE eb THEN 
        DO:
            MESSAGE "Invalid Form No..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO estMaterial.formNo .
            opcValidError = YES .
        END.
    END.

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
            AND item.i-no    EQ estMaterial.itemID:SCREEN-VALUE 
            //AND lookup(item.mat-type,cmaterialTypeID) > 0 
            NO-ERROR.
        IF NOT AVAILABLE ITEM THEN 
        DO:
            MESSAGE "Invalid Item Id, try help..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO estMaterial.itemID .
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
            AND item.i-no    EQ estMaterial.itemID:SCREEN-VALUE 
            //AND lookup(item.mat-type,cmaterialTypeID) > 0 
            NO-ERROR.
        IF AVAILABLE item THEN 
        DO:
            ASSIGN
                estMaterial.dimLength:Screen-value    = STRING(item.case-l)
                estMaterial.dimWidth:Screen-value     = STRING(item.case-w)
                estMaterial.dimDepth:Screen-value     = STRING(item.case-d) 
                estMaterial.weightPerEA:Screen-value  = STRING(item.weight-100 / 100) 
                estMaterial.wastePercent:SCREEN-VALUE = STRING(ITEM.wastePercent).
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


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: cec\d-estrel.w
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
    cocode = g_company.
ASSIGN 
    locode = g_loc.

DEFINE VARIABLE char-val        AS CHARACTER NO-UNDO.

DEFINE VARIABLE lv-item-recid   AS RECID     NO-UNDO.
DEFINE VARIABLE ll-order-warned AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ll-new-record   AS LOGICAL   NO-UNDO.

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
&Scoped-define INTERNAL-TABLES estRelease

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame estRelease.estimateNo ~
estRelease.formNo estRelease.blankNo estRelease.quantity ~
estRelease.shipFromLocationID estRelease.customerID estRelease.carrierID ~
estRelease.quantityPerSubUnit estRelease.quantityOfUnits ~
estRelease.monthsAtShipFrom estRelease.quantityRelease estRelease.shipToID ~
estRelease.carrierZone estRelease.quantitySubUnitsPerUnit ~
estRelease.palletMultiplier estRelease.stackHeight estRelease.storageCost ~
estRelease.handlingCost estRelease.freightCost estRelease.handlingCostTotal ~
estRelease.storageCostTotal estRelease.createRelease 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame ~
estRelease.shipFromLocationID estRelease.carrierID ~
estRelease.quantityPerSubUnit estRelease.monthsAtShipFrom ~
estRelease.quantityRelease estRelease.carrierZone ~
estRelease.quantitySubUnitsPerUnit estRelease.palletMultiplier ~
estRelease.createRelease estRelease.shipToID
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame estRelease
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame estRelease
&Scoped-define TABLES-IN-QUERY-Dialog-Frame estRelease
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame estRelease


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS estRelease.shipFromLocationID ~
 estRelease.carrierID ~
estRelease.quantityPerSubUnit estRelease.monthsAtShipFrom ~
estRelease.quantityRelease estRelease.carrierZone ~
estRelease.quantitySubUnitsPerUnit estRelease.palletMultiplier ~
estRelease.createRelease estRelease.shipToID
&Scoped-define ENABLED-TABLES estRelease
&Scoped-define FIRST-ENABLED-TABLE estRelease
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 ~
RECT-39 RECT-40 
&Scoped-Define DISPLAYED-FIELDS estRelease.estimateNo estRelease.formNo ~
estRelease.blankNo estRelease.quantity estRelease.shipFromLocationID ~
estRelease.customerID estRelease.carrierID estRelease.quantityPerSubUnit ~
estRelease.quantityOfUnits estRelease.monthsAtShipFrom ~
estRelease.quantityRelease estRelease.shipToID estRelease.carrierZone ~
estRelease.quantitySubUnitsPerUnit estRelease.palletMultiplier ~
estRelease.stackHeight estRelease.storageCost estRelease.handlingCost ~
estRelease.freightCost estRelease.handlingCostTotal ~
estRelease.storageCostTotal estRelease.createRelease 
&Scoped-define DISPLAYED-TABLES estRelease
&Scoped-define FIRST-DISPLAYED-TABLE estRelease
&Scoped-Define DISPLAYED-OBJECTS fi_Pallet-count 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE fi_Pallet-count AS CHARACTER FORMAT "X(15)":U 
    LABEL "Pallet Count" 
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 19 BY 2.38
    BGCOLOR 15 .

DEFINE RECTANGLE RECT-38
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 104.8 BY 1.52
    BGCOLOR 15 .

DEFINE RECTANGLE RECT-39
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 51.8 BY 8.91
    BGCOLOR 15 .

DEFINE RECTANGLE RECT-40
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 51.8 BY 8.91
    BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
    estRelease SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    estRelease.estimateNo AT ROW 1.29 COL 14.8 COLON-ALIGNED
    LABEL "Estimate#" FORMAT "x(8)"
    VIEW-AS FILL-IN 
    SIZE 16.2 BY 1
    BGCOLOR 15 FONT 1
    estRelease.quantity AT ROW 1.29 COL 52 COLON-ALIGNED
    LABEL "Master Quantity" FORMAT "->>,>>>,>>9"
    VIEW-AS FILL-IN 
    SIZE 13 BY 1
    BGCOLOR 15 FONT 1
    estRelease.formNo AT ROW 1.29 COL 75.6 COLON-ALIGNED
    VIEW-AS FILL-IN 
    SIZE 7.4 BY 1
    BGCOLOR 15 FONT 1
    estRelease.blankNo AT ROW 1.29 COL 95.4 COLON-ALIGNED
    VIEW-AS FILL-IN 
    SIZE 7.4 BY 1
    BGCOLOR 15 FONT 1
    estRelease.shipFromLocationID AT ROW 3.38 COL 21.8 COLON-ALIGNED
    LABEL "Ship From" FORMAT "x(8)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    estRelease.customerID AT ROW 4.43 COL 21.8 COLON-ALIGNED
    LABEL "Customer" FORMAT "x(8)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    estRelease.shipToID AT ROW 5.52 COL 21.8 COLON-ALIGNED
    LABEL "Ship To" FORMAT "x(10)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    estRelease.carrierID AT ROW 6.67 COL 21.8 COLON-ALIGNED
    LABEL "Carrier" FORMAT "x(10)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    
    estRelease.carrierZone AT ROW 7.76 COL 21.8 COLON-ALIGNED
    LABEL "Zone" FORMAT "x(10)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1

    fi_Pallet-count AT ROW 5.52 COL 80 COLON-ALIGNED
    
    estRelease.quantityRelease AT ROW 3.38 COL 80 COLON-ALIGNED
    LABEL "Release Quantity" FORMAT "->>,>>>,>>9"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
     
    estRelease.quantityPerSubUnit AT ROW 4.43 COL 80 COLON-ALIGNED
    LABEL "Unit Count" FORMAT "->>>>9"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    estRelease.quantitySubUnitsPerUnit AT ROW 6.67 COL 80 COLON-ALIGNED
    LABEL "Unit/Pallet" FORMAT "->>>>9"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    estRelease.quantityOfUnits AT ROW 7.76 COL 80 COLON-ALIGNED
    LABEL "Pallets" FORMAT "->>>>9"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    estRelease.palletMultiplier AT ROW 8.86 COL 80 COLON-ALIGNED
    LABEL "Pallet Multiplier" FORMAT ">>>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    estRelease.monthsAtShipFrom AT ROW 9.96 COL 80 COLON-ALIGNED
    LABEL "Months at Ship From" FORMAT "->>>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    estRelease.stackHeight AT ROW 12.95 COL 17 COLON-ALIGNED
    LABEL "Stack Height" FORMAT ">>>9"
    VIEW-AS FILL-IN 
    SIZE 20.2 BY 1
    BGCOLOR 15 FONT 1
    estRelease.storageCost AT ROW 12.95 COL 85 COLON-ALIGNED
    LABEL "Storage Cost Per Pallet Per Month at Ship From" FORMAT "->>>>9"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    estRelease.handlingCost AT ROW 12.95 COL 82 COLON-ALIGNED
    LABEL "Handling Cost Per Pallet at Ship From" FORMAT ">>>9"
    VIEW-AS FILL-IN 
    SIZE 17.6 BY 1
    BGCOLOR 15 FONT 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
    estRelease.freightCost AT ROW 12.95 COL 17.6 COLON-ALIGNED
    LABEL "Freight Cost" FORMAT ">>>>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    estRelease.handlingCostTotal AT ROW 12.95 COL 75 COLON-ALIGNED
    LABEL "Handling Cost" FORMAT ">>>>>9.99"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    estRelease.storageCostTotal AT ROW 12.95 COL 17.2 COLON-ALIGNED
    LABEL "Storage Cost" FORMAT ">>>>>9.99"
    VIEW-AS FILL-IN 
    SIZE 28.6 BY 1
    BGCOLOR 15 FONT 1
    estRelease.createRelease AT ROW 9.95 COL 80 COLON-ALIGNED
    LABEL "Create Release" FORMAT "Yes/No"
    VIEW-AS FILL-IN 
    SIZE 6 BY 1
    BGCOLOR 15 FONT 1
    Btn_OK AT ROW 12.67 COL 86.8
    Btn_Done AT ROW 12.95 COL 87.8
    Btn_Cancel AT ROW 12.67 COL 95.8
    RECT-21 AT ROW 12.43 COL 85.8
    RECT-38 AT ROW 1.14 COL 1.2
    RECT-39 AT ROW 2.81 COL 1.2 WIDGET-ID 2
    RECT-40 AT ROW 2.81 COL 54 WIDGET-ID 4
    SPACE(1.19) SKIP(1.84)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6
    TITLE "Misc Release Update".


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
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

/* SETTINGS FOR FILL-IN estRelease.blankNo IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN estRelease.carrierID IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estRelease.carrierZone IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estRelease.createRelease IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estRelease.customerID IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN estRelease.estimateNo IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN fi_Pallet-count IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN estRelease.formNo IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN estRelease.freightCost IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN estRelease.handlingCost IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN estRelease.handlingCostTotal IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN estRelease.monthsAtShipFrom IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estRelease.palletMultiplier IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estRelease.quantity IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estRelease.quantityOfUnits IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN estRelease.quantityPerSubUnit IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estRelease.quantityRelease IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estRelease.quantitySubUnitsPerUnit IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estRelease.shipFromLocationID IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estRelease.shipToID IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN estRelease.stackHeight IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN estRelease.storageCost IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN estRelease.storageCostTotal IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.estRelease "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.estRelease.company eq cocode "
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Misc Release Update */
    DO:
        DEFINE VARIABLE char-val  AS cha    NO-UNDO.
        DEFINE VARIABLE lv-handle AS HANDLE NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO .
        
        CASE FOCUS:NAME :
            WHEN "shipFromLocationID" THEN 
                DO:
                    RUN windows/l-loc.w  (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val). 
                    IF char-val <> "" THEN 
                        FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = entry(1,char-val).
                    RETURN NO-APPLY.   
                END.

            WHEN "carrierID" THEN 
                DO:
                    RUN windows/l-carrie.w  (cocode, estRelease.shipFromLocationID:SCREEN-VALUE IN FRAME {&FRAME-NAME}, FOCUS:SCREEN-VALUE, OUTPUT char-val). 
                    IF char-val <> "" THEN 
                        FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = entry(1,char-val).
                END.

            WHEN "carrierZone" THEN 
                DO:
                    RUN windows/l-delzon.w  (cocode, estRelease.shipFromLocationID:SCREEN-VALUE IN FRAME {&FRAME-NAME}, estRelease.carrierID:SCREEN-VALUE IN FRAME {&FRAME-NAME}, FOCUS:SCREEN-VALUE, OUTPUT char-val). 
                    IF char-val <> "" THEN 
                        FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = entry(1,char-val).
                END.
           WHEN "shipToID" THEN 
                DO: 
                    RUN windows/l-shipt3.w (cocode, locode, estRelease.customerID:SCREEN-VALUE, estRelease.shipToID:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
//                    RUN windows/l-shipt2.w (cocode, locode, estRelease.customerID:SCREEN-VALUE, estRelease.shipToID:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
                    IF char-val <> "" THEN 
                        FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = entry(1,char-val).
                END.
            
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Misc Release Update */
    ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Misc Release Update */
    DO:
        DISABLE TRIGGERS FOR LOAD OF estRelease .
    
        IF AVAILABLE estRelease THEN
            op-rowid = ROWID(estRelease) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST estRelease EXCLUSIVE-LOCK
                WHERE RECID(estRelease) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE estRelease THEN DELETE estRelease .
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
        DISABLE TRIGGERS FOR LOAD OF estRelease .
    
        IF AVAILABLE estRelease THEN
            op-rowid = ROWID(estRelease) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST estRelease EXCLUSIVE-LOCK
                WHERE RECID(estRelease) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE estRelease THEN DELETE estRelease .
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
        IF AVAILABLE estRelease THEN
            ASSIGN op-rowid = ROWID(estRelease) .
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
        DEFINE VARIABLE ld              AS DECIMAL NO-UNDO.
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO.
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
        DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hftp            AS HANDLE    NO-UNDO.

        RUN system/freightProcs.p PERSISTENT SET hftp.
        THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hftp).

        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.

        RUN valid-shipto(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.
   
        RUN valid-loc (OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.

        RUN valid-carrier(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.

        RUN valid-zone(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.

       
        DO TRANSACTION:
            FIND CURRENT estRelease EXCLUSIVE-LOCK NO-ERROR.

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
            END.
        END.

        RUN CalcStorageAndHandlingForEstRelease(INPUT estRelease.estReleaseID ,OUTPUT lError,
                                OUTPUT cMessage ).

        
        FIND CURRENT estRelease NO-LOCK NO-ERROR .
        op-rowid = ROWID(estRelease).

        APPLY "go" TO FRAME {&FRAME-NAME}.
     THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hftp).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME estRelease.carrierID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estRelease.carrierID Dialog-Frame
ON LEAVE OF estRelease.carrierID IN FRAME Dialog-Frame /* Carrier */
    DO:
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO.
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-carrier(OUTPUT lValidateResult) NO-ERROR.
            IF lValidateResult THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME estRelease.carrierZone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estRelease.carrierZone Dialog-Frame
ON LEAVE OF estRelease.carrierZone IN FRAME Dialog-Frame /* Zone */
    DO:
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO.
        IF LASTKEY NE -1 THEN 
        DO:     
            
            RUN valid-zone(OUTPUT lValidateResult) NO-ERROR.
            IF lValidateResult THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME estRelease.palletMultiplier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estRelease.palletMultiplier Dialog-Frame
ON VALUE-CHANGED OF estRelease.palletMultiplier IN FRAME Dialog-Frame /* Pallet Mul */
    DO:       
        RUN pCalAllUnit .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME estRelease.quantityPerSubUnit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estRelease.quantityPerSubUnit Dialog-Frame
ON VALUE-CHANGED OF estRelease.quantityPerSubUnit IN FRAME Dialog-Frame /* qty per  */
    DO:
        RUN pCalAllUnit .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME estRelease.quantitySubUnitsPerUnit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estRelease.quantitySubUnitsPerUnit Dialog-Frame
ON VALUE-CHANGED OF estRelease.quantitySubUnitsPerUnit IN FRAME Dialog-Frame /* Unit/Pallet */
    DO:
        RUN pCalAllUnit .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME estRelease.quantityRelease
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estRelease.quantityRelease Dialog-Frame
ON VALUE-CHANGED OF estRelease.quantityRelease IN FRAME Dialog-Frame /* Release Quantity */
    DO:
        RUN pCalAllUnit .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME estRelease.monthsAtShipFrom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estRelease.monthsAtShipFrom Dialog-Frame
ON VALUE-CHANGED OF estRelease.monthsAtShipFrom IN FRAME Dialog-Frame /* Unit/Pallet */
    DO:
        RUN pCalAllUnit .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME estRelease.shipFromLocationID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estRelease.shipFromLocationID Dialog-Frame
ON LEAVE OF estRelease.shipFromLocationID IN FRAME Dialog-Frame /* From */
    DO:
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO.

        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-loc(OUTPUT lValidateResult) NO-ERROR.
            IF lValidateResult THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME estRelease.shipToID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL estRelease.shipToID Dialog-Frame
ON LEAVE OF estRelease.shipToID IN FRAME Dialog-Frame /* From */
    DO:
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO.

        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-shipto(OUTPUT lValidateResult) NO-ERROR.
            IF lValidateResult THEN RETURN NO-APPLY.
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
    ELSE FIND estRelease NO-LOCK WHERE RECID(estRelease) EQ ip-recid NO-ERROR.

    IF ip-type NE "view" THEN 
    DO: 
        RUN enable_UI.
        RUN display-item.

        ASSIGN 
            ll-order-warned = NO.
        btn_done:HIDDEN IN FRAME {&FRAME-NAME} = YES.
        estRelease.stackHeight:VISIBLE IN FRAME {&FRAME-NAME} = NO.
        estRelease.storageCost:VISIBLE IN FRAME {&FRAME-NAME} = NO.
        estRelease.handlingCost:VISIBLE IN FRAME {&FRAME-NAME} = NO.
        estRelease.freightCost:VISIBLE IN FRAME {&FRAME-NAME} = NO.
        estRelease.handlingCostTotal:VISIBLE IN FRAME {&FRAME-NAME} = NO.
        estRelease.storageCostTotal:VISIBLE IN FRAME {&FRAME-NAME} = NO.
        estRelease.createRelease:VISIBLE IN FRAME {&FRAME-NAME} = NO.
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
    DEFINE VARIABLE lv-rno LIKE estRelease.estReleaseID NO-UNDO.
    DEFINE BUFFER b-estRelease FOR estRelease.
    DEFINE VARIABLE hftp            AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iEstReleaseID   AS INTEGER   NO-UNDO .
    DEFINE VARIABLE lCreated        AS LOGICAL   NO-UNDO .
    DEFINE VARIABLE cCreatedMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dPalletQty AS DECIMAL NO-UNDO .

    RUN system/FreightProcs.p PERSISTENT SET hftp.
    THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hftp).

    DO WITH FRAME {&FRAME-NAME}:
      
        RUN CreateEstRelease( INPUT eb.company,INPUT eb.est-no,INPUT eb.form-no,
            INPUT eb.blank-no,eb.eqty, OUTPUT iEstReleaseID,
            OUTPUT lCreated ,  OUTPUT cCreatedMessage) .
        FIND FIRST estRelease EXCLUSIVE-LOCK
            WHERE estRelease.estReleaseID EQ iEstReleaseID NO-ERROR .

        IF AVAILABLE estRelease THEN 
        DO:
            ASSIGN 
                estRelease.quantityRelease         = eb.eqty
                estRelease.shipFromLocationID      = eb.loc
                estRelease.customerID              = eb.cust-no
                estRelease.shipToID                = eb.ship-id 
                estRelease.quantityPerSubUnit      = eb.cas-cnt
                estRelease.quantitySubUnitsPerUnit = eb.cas-pal

                dPalletQty         = estRelease.quantityRelease / (eb.cas-cnt * eb.cas-pal )
                estRelease.stackHeight             = eb.stackHeight
                .
            IF dPalletQty EQ TRUNCATE(dPalletQty,0) THEN
                estRelease.quantityOfUnits = INTEGER(TRUNCATE(dPalletQty,0) + 1 ) .
            ELSE   estRelease.quantityOfUnits = INTEGER(dPalletQty) .
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ cocode
                AND shipto.cust-no EQ eb.cust-no
                AND shipto.ship-id EQ eb.ship-id NO-ERROR .
            IF AVAILABLE shipto THEN
                ASSIGN
                    estRelease.carrierID   = shipto.carrier
                    estRelease.carrierZone = shipto.dest-code .

            FIND FIRST loc NO-LOCK
                WHERE loc.company EQ cocode
                AND loc.loc EQ eb.loc NO-ERROR .
            IF AVAILABLE loc THEN
                ASSIGN
                    estRelease.storageCost  = loc.storageCost[1] 
                    estRelease.handlingCost = loc.handlingCost  .

            ASSIGN
                estRelease.palletMultiplier = 1
                estRelease.stackHeight      = 1.

            DISPLAY estRelease.palletMultiplier  estRelease.stackHeight estRelease.carrierID estRelease.carrierZone
                estRelease.quantityOfUnits estRelease.quantitySubUnitsPerUnit estRelease.quantityPerSubUnit
                estRelease.shipToID estRelease.customerID estRelease.shipFromLocationID estRelease.quantityRelease
                estRelease.quantity estRelease.BlankNo estRelease.FormNo estRelease.estimateNo  . 
            ASSIGN 
                lv-item-recid = RECID(estRelease).
            ll-new-record = YES.

            FIND CURRENT estRelease NO-LOCK NO-ERROR.

        END. /* avail estRelease */
    END. /* avail eb */ 

    THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE(hftp).

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
    IF AVAILABLE estRelease  THEN 
    DO:
        ASSIGN 
            fi_Pallet-count = STRING(quantityPerSubUnit * quantitySubUnitsPerUnit) .

        DISPLAY estRelease.quantity estRelease.quantityRelease 
            estRelease.shipFromLocationID estRelease.customerID estRelease.shipToID estRelease.carrierID estRelease.carrierZone 
            estRelease.quantityPerSubUnit estRelease.quantitySubUnitsPerUnit estRelease.quantityOfUnits estRelease.palletMultiplier 
            estRelease.monthsAtShipFrom estRelease.stackHeight estRelease.storageCost estRelease.handlingCost 
            estRelease.freightCost estRelease.handlingCostTotal estRelease.storageCostTotal estRelease.createRelease 
            estRelease.estimateNo estRelease.formNo estRelease.blankNo  fi_Pallet-count
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
    DISPLAY fi_Pallet-count 
        WITH FRAME Dialog-Frame.
    IF AVAILABLE estRelease THEN 
        DISPLAY estRelease.estimateNo estRelease.formNo estRelease.blankNo 
            estRelease.quantity estRelease.shipFromLocationID 
            estRelease.customerID estRelease.carrierID 
            estRelease.quantityPerSubUnit estRelease.quantityOfUnits 
            estRelease.monthsAtShipFrom estRelease.quantityRelease 
            estRelease.shipToID estRelease.carrierZone 
            estRelease.quantitySubUnitsPerUnit estRelease.palletMultiplier 
            estRelease.stackHeight estRelease.storageCost estRelease.handlingCost 
            estRelease.freightCost estRelease.handlingCostTotal 
            estRelease.storageCostTotal estRelease.createRelease 
            WITH FRAME Dialog-Frame.
    ENABLE estRelease.shipFromLocationID estRelease.carrierID 
        estRelease.quantityPerSubUnit estRelease.monthsAtShipFrom 
        estRelease.quantityRelease estRelease.carrierZone 
        estRelease.quantitySubUnitsPerUnit estRelease.palletMultiplier 
        estRelease.createRelease estRelease.shipToID Btn_OK Btn_Done Btn_Cancel  
        RECT-21 RECT-38 RECT-39 RECT-40 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-carrier Dialog-Frame 
PROCEDURE valid-carrier :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcValidError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST carrier NO-LOCK
            WHERE carrier.company EQ cocode
            AND carrier.loc    EQ estRelease.shipFromLocationID:SCREEN-VALUE 
            AND carrier.carrier    EQ estRelease.carrierID:SCREEN-VALUE 
            NO-ERROR.
        IF NOT AVAILABLE carrier THEN 
        DO:
            MESSAGE "Invalid carrier, try help..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO estRelease.carrierID .
            opcValidError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc Dialog-Frame 
PROCEDURE valid-loc :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcValidError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST loc
            WHERE loc.company EQ cocode
            AND loc.loc    EQ estRelease.shipFromLocationID:SCREEN-VALUE 
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE loc THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO estRelease.shipFromLocationID .
            opcValidError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-zone Dialog-Frame 
PROCEDURE valid-zone :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcValidError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST carr-mtx NO-LOCK
            WHERE carr-mtx.company EQ cocode
            AND carr-mtx.loc    EQ estRelease.shipFromLocationID:SCREEN-VALUE 
            AND carr-mtx.carrier    EQ estRelease.carrierID:SCREEN-VALUE 
            AND carr-mtx.del-zone    EQ estRelease.carrierZone:SCREEN-VALUE 
            NO-ERROR.
        IF NOT AVAILABLE carr-mtx THEN 
        DO:
            MESSAGE "Invalid Zone, try help..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO estRelease.carrierZone .
            opcValidError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalAllUnit Dialog-Frame 
PROCEDURE pCalAllUnit :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
  DEFINE VARIABLE dPalletQty AS DECIMAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        fi_Pallet-count:SCREEN-VALUE = STRING( INTEGER(estRelease.quantityPerSubUnit:SCREEN-VALUE) *
            INTEGER(estRelease.quantitySubUnitsPerUnit:SCREEN-VALUE)) .

        dPalletQty = DECIMAL( DECIMAL(estRelease.quantityRelease:SCREEN-VALUE) / (INTEGER(estRelease.quantityPerSubUnit:SCREEN-VALUE) *
            DECIMAL(estRelease.quantitySubUnitsPerUnit:SCREEN-VALUE))) .

        IF dPalletQty EQ TRUNCATE(dPalletQty,0) THEN
                estRelease.quantityOfUnits:SCREEN-VALUE = STRING( INTEGER(TRUNCATE(dPalletQty,0) + 1 ) ).
        ELSE estRelease.quantityOfUnits:SCREEN-VALUE = STRING( INTEGER(dPalletQty) ).

        handlingCostTotal:SCREEN-VALUE = STRING( DECIMAL(estRelease.handlingCost:SCREEN-VALUE) *
            DECIMAL(estRelease.quantityOfUnits:SCREEN-VALUE)) .

        storageCostTotal:SCREEN-VALUE = STRING( DECIMAL(estRelease.storageCost:SCREEN-VALUE) *
            DECIMAL(estRelease.monthsAtShipFrom:SCREEN-VALUE) *
            DECIMAL(estRelease.quantityOfUnits:SCREEN-VALUE) *
            DECIMAL(estRelease.palletMultiplier:SCREEN-VALUE)  ) .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-shipto Dialog-Frame 
PROCEDURE valid-shipto :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcValidError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ cocode
            AND cust.ACTIVE EQ "X" NO-ERROR .

        FIND FIRST shipto NO-LOCK 
        WHERE shipto.company EQ cocode 
        AND (shipto.cust-no EQ estRelease.customerID:SCREEN-VALUE OR shipto.cust-no EQ cust.cust-no)
        AND TRIM(shipto.ship-id) = TRIM(estRelease.shipToID:SCREEN-VALUE)
        NO-ERROR.
        IF NOT AVAILABLE shipto THEN 
        DO:
            MESSAGE "Invalid Ship To, try help..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO estRelease.shipToID .
            opcValidError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

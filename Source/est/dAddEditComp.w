&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: est\dAddEditComp.w
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
 USING System.SharedConfig.
/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-rowid  AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO.   /* add,update,view */
DEFINE INPUT PARAMETER ipcEstType AS CHARACTER NO-UNDO.  /* estimate type Wood or SetWithSubAssembly  */
DEFINE INPUT PARAMETER ipcSetPart  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcSetPartName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcPartNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcProCat AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplAutoPart  AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER ipcFGItem AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}
{est\ttInputEst.i}

ASSIGN 
    cocode = g_company
    locode = g_loc.

DEFINE VARIABLE char-val        AS CHARACTER NO-UNDO.

DEFINE VARIABLE lv-item-recid   AS RECID     NO-UNDO.
DEFINE VARIABLE ll-order-warned AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ll-new-record   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ilogic          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMaterialType   AS CHARACTER INITIAL "C,5,6,M,D" NO-UNDO .
DEFINE VARIABLE k_frac          AS DECIMAL   INIT 6.25 NO-UNDO.
DEFINE VARIABLE v-count         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lCreateNewFG    AS LOGICAL   NO-UNDO .

DEFINE VARIABLE cRtnChar            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMiscEstimateSource AS CHARACTER NO-UNDO.
DEFINE VARIABLE lMiscEstimateSource AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iMiscEstimateSource AS INTEGER   NO-UNDO.
DEFINE BUFFER bf-eb FOR eb.

{Inventory/ttInventory.i "NEW SHARED"}

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

RUN sys/ref/nk1look.p (INPUT cocode, "MiscEstimateSource", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound). 
cMiscEstimateSource = STRING(cRtnChar) NO-ERROR .
    
RUN sys/ref/nk1look.p (INPUT cocode, "MiscEstimateSource", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound). 
lMiscEstimateSource = logical(cRtnChar) NO-ERROR .  
  
RUN sys/ref/nk1look.p (INPUT cocode, "MiscEstimateSource", "I" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound). 
iMiscEstimateSource = INTEGER(cRtnChar) NO-ERROR . 

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS dQtyPerSet Btn_OK Btn_Done Btn_Cancel ~
cCustPart cFGItem style-cod board fg-cat len wid dep rd_show1 item-name ~
item-dscr RECT-21 RECT-38 RECT-39 cSourceEst 
&Scoped-Define DISPLAYED-OBJECTS est-no iForm iBlank cSetCustPart ~
dQtyPerSet set-item-name cCustPart cFGItem style-cod style-dscr board ~
fg-cat board-dscr cat-dscr len wid dep rd_show1 item-name item-dscr ~
cSourceEst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE board AS CHARACTER FORMAT "X(12)":U 
     LABEL "Wood" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE board-dscr AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cat-dscr AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cCustPart AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cust Part#" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE cFGItem AS CHARACTER FORMAT "X(15)":U 
     LABEL "FG Item" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE cSetCustPart AS CHARACTER FORMAT "X(15)":U 
     LABEL "Part#" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE cSourceEst AS CHARACTER FORMAT "X(15)":U 
     LABEL "Source Estimate" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE dep AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "Depth" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE dQtyPerSet AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 1 
     LABEL "Qty Per Set" 
     VIEW-AS FILL-IN 
     SIZE 8.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE est-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Estimate#" 
     VIEW-AS FILL-IN 
     SIZE 25.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fg-cat AS CHARACTER FORMAT "X(5)":U 
     LABEL "FG Category" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1
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

DEFINE VARIABLE item-dscr AS CHARACTER FORMAT "X(30)":U 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE item-name AS CHARACTER FORMAT "X(30)":U 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE len AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "Length" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE set-item-name AS CHARACTER FORMAT "X(30)":U 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE style-cod AS CHARACTER FORMAT "X(8)":U 
     LABEL "Style Code" 
     VIEW-AS FILL-IN 
     SIZE 14.4 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE style-dscr AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE wid AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     LABEL "Width" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE rd_show1 AS CHARACTER INITIAL "M" 
     VIEW-AS RADIO-SET horizontal 
     RADIO-BUTTONS 
          "Purchased", "P",
"Manufactured", "M",
"Sub-Assembly", "S"
     SIZE 56 BY 2.07 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 19 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 124.8 BY 4.14
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 124.8 BY 9.35
     BGCOLOR 15 .

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     est-no AT ROW 1.71 COL 15 COLON-ALIGNED WIDGET-ID 200
     iForm AT ROW 7.76 COL 15.6 COLON-ALIGNED WIDGET-ID 314
     iBlank AT ROW 7.76 COL 34.8 COLON-ALIGNED WIDGET-ID 316
     cSetCustPart AT ROW 2.91 COL 15 COLON-ALIGNED WIDGET-ID 176
     dQtyPerSet AT ROW 7.76 COL 59.6 COLON-ALIGNED WIDGET-ID 178
     Btn_OK AT ROW 15.24 COL 108
     Btn_Done AT ROW 15.52 COL 109
     Btn_Cancel AT ROW 15.24 COL 117
     set-item-name AT ROW 4.14 COL 15 COLON-ALIGNED WIDGET-ID 208
     cCustPart AT ROW 9 COL 15.6 COLON-ALIGNED WIDGET-ID 88
     cFGItem AT ROW 10.29 COL 15.6 COLON-ALIGNED WIDGET-ID 328
     style-cod AT ROW 11.43 COL 15.6 COLON-ALIGNED WIDGET-ID 180
     style-dscr AT ROW 11.43 COL 30.6 COLON-ALIGNED NO-LABEL WIDGET-ID 182
     board AT ROW 12.52 COL 15.6 COLON-ALIGNED WIDGET-ID 174
     fg-cat AT ROW 13.67 COL 15.6 COLON-ALIGNED WIDGET-ID 196
     board-dscr AT ROW 12.52 COL 30.6 COLON-ALIGNED NO-LABEL WIDGET-ID 212
     cat-dscr AT ROW 13.71 COL 30.6 COLON-ALIGNED NO-LABEL WIDGET-ID 214
     len AT ROW 7.76 COL 79 COLON-ALIGNED WIDGET-ID 190
     wid AT ROW 9 COL 79 COLON-ALIGNED WIDGET-ID 194
     dep AT ROW 10.29 COL 79 COLON-ALIGNED WIDGET-ID 192
     item-name AT ROW 11.43 COL 79 COLON-ALIGNED WIDGET-ID 322
     item-dscr AT ROW 12.52 COL 79 COLON-ALIGNED WIDGET-ID 210
     rd_show1 AT ROW 5.77 COL 3.8 NO-LABEL WIDGET-ID 324      
     cSourceEst AT ROW 6.29 COL 79 COLON-ALIGNED WIDGET-ID 330
     "Set Header" VIEW-AS TEXT
          SIZE 14 BY .71 AT ROW 1 COL 5 WIDGET-ID 206
     RECT-21 AT ROW 15 COL 107
     RECT-38 AT ROW 1.43 COL 1.2
     RECT-39 AT ROW 5.52 COL 1.2 WIDGET-ID 2
     SPACE(0.99) SKIP(2.64)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Add/Update Set Component".


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

/* SETTINGS FOR FILL-IN board-dscr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cat-dscr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cSetCustPart IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN est-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iBlank IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN iForm IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       rd_show1:PRIVATE-DATA IN FRAME Dialog-Frame     = 
                "parm".

ASSIGN 
       RECT-39:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN set-item-name IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN style-dscr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Add/Update Set Component */
DO:
        DEFINE VARIABLE char-val   AS cha    NO-UNDO.
        DEFINE VARIABLE lv-handle  AS HANDLE NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID  NO-UNDO .
        
        CASE FOCUS:NAME :
            WHEN "rmItemID" THEN 
                DO:
                    
                END.
            
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Add/Update Set Component */
ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Add/Update Set Component */
DO:
            
        IF AVAILABLE ttInputEst THEN
            op-rowid = ROWID(ttInputEst) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST ttInputEst EXCLUSIVE-LOCK
                WHERE RECID(ttInputEst) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE ttInputEst THEN DELETE ttInputEst .
            op-rowid = ? .
        END.
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.

    /*APPLY "END-ERROR":U TO SELF.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME board
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL board Dialog-Frame
ON HELP OF board IN FRAME Dialog-Frame /* Wood */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
        DEF VAR lv-rowid AS ROWID NO-UNDO.
        DEF VAR lv-ind LIKE style.industry NO-UNDO.
        DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
        
           FIND style WHERE style.company = cocode AND
                            style.style = style-cod:SCREEN-VALUE 
                            NO-LOCK NO-ERROR.   
           IF AVAIL style THEN lv-ind = style.industry.
           ELSE lv-ind = "".  
           IF AVAIL style AND style.type = "f" THEN  DO: /* foam */    
              RUN AOA/dynLookupSetParam.p (70, ROWID(style), OUTPUT char-val).
              IF char-val NE "" AND ENTRY(1,char-val) NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "item.i-no", char-val).                
                APPLY "ENTRY":U TO board.
              END.
           END.
           IF AVAIL style AND style.type = "W" THEN  DO: /* foam */                  
              RUN AOA/dynLookupSetParam.p (155, ROWID(style), OUTPUT char-val).  
              IF char-val NE "" AND DYNAMIC-FUNCTION("sfDynLookupValue", "item.i-no", char-val) NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = DYNAMIC-FUNCTION("sfDynLookupValue", "item.i-no", char-val).                
                APPLY "ENTRY":U TO board.
              END.
           END.
           ELSE DO:
              RUN windows/l-board1.w (cocode,lv-ind,board:SCREEN-VALUE, OUTPUT lv-rowid).
              FIND FIRST ITEM WHERE ROWID(item) EQ lv-rowid NO-LOCK NO-ERROR.
              IF AVAIL ITEM AND ITEM.i-no NE board:SCREEN-VALUE THEN DO:
                board:SCREEN-VALUE = item.i-no.                
              END.
           END.         
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL board Dialog-Frame
ON LEAVE OF board IN FRAME Dialog-Frame /* Wood */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF NOT CAN-FIND(item WHERE item.company = cocode
                AND item.i-no = board:SCREEN-VALUE)
                THEN 
            DO:
                MESSAGE "Invalid Board. Try Help. " VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO board.
                RETURN NO-APPLY.
            END.
            IF SELF:SCREEN-VALUE NE "" THEN 
            DO:
                FIND FIRST item WHERE item.company = cocode
                    AND item.i-no EQ board:SCREEN-VALUE NO-LOCK NO-ERROR .

                IF AVAILABLE ITEM THEN
                    ASSIGN board-dscr:SCREEN-VALUE = item.i-name .
            END.
                                                
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
        
    
        IF AVAILABLE ttInputEst THEN
            op-rowid = ROWID(ttInputEst) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST ttInputEst EXCLUSIVE-LOCK
                WHERE RECID(ttInputEst) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE ttInputEst THEN DELETE ttInputEst .
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
        
        RUN valid-part-no(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.
                
        RUN valid-procat(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.

        RUN valid-style(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY.  
        
        RUN valid-Form-Blank(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY. 
        
        RUN valid-QtyPerSet(OUTPUT lValidateResult) NO-ERROR.
        IF lValidateResult THEN RETURN NO-APPLY. 
        
       
        DO TRANSACTION:           

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&displayed-objects}.
            END.            
        END.
        IF ip-type EQ "Add" THEN
        DO:
            CREATE ttInputEst.
            ASSIGN
                ttInputEst.cEstType = IF ipcEstType EQ "Wood" THEN "NewSetEstimate" ELSE "SetSubAssembly"
                ttInputEst.cSetType = "Set"
                ttInputEst.cCompany = cocode .
        END.
        
        ASSIGN
            ttInputEst.iFormNo          = iForm
            ttInputEst.iBlankNo         = iBlank             
            ttInputEst.cPartID          = cCustPart
            ttInputEst.cStockNo         = cFGItem
            ttInputEst.cPartName        = item-name
            ttInputEst.cPartDescription = item-dscr
            ttInputEst.dLength          = len
            ttInputEst.dWidth           = wid            
            ttInputEst.dDepth           = dep
            ttInputEst.cCategory        = fg-cat
            ttInputEst.cBoard           = board
            ttInputEst.cStyle           = style-cod
            ttInputEst.dQtyPerSet       = dQtyPerSet
            ttInputEst.lPurchased       = IF rd_show1 EQ "P" OR rd_show1 EQ "S" THEN TRUE ELSE FALSE
            .           
        IF cSourceEst:SCREEN-VALUE NE "" OR integer(cSourceEst:SCREEN-VALUE) NE 0 THEN
            RUN pGetTempValue.
            
        op-rowid = ROWID(ttInputEst).
        
        APPLY "go" TO FRAME {&FRAME-NAME}.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cCustPart
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustPart Dialog-Frame
ON HELP OF cCustPart IN FRAME Dialog-Frame /* Cust Part# */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.

        RUN windows/l-cstprt.w (cocode, "", FOCUS:SCREEN-VALUE, "", OUTPUT char-val, OUTPUT look-recid).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value      = ENTRY(1,char-val)
                item-name:screen-value = ENTRY(2,char-val)
                item-dscr:screen-value = ENTRY(3,char-val)
                .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cCustPart Dialog-Frame
ON LEAVE OF cCustPart IN FRAME Dialog-Frame /* Cust Part# */
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
        RUN valid-part-no(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.  
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cFGItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFGItem Dialog-Frame
ON HELP OF cFGItem IN FRAME Dialog-Frame /* FG Item */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.

        RUN windows/l-itemfa.w (cocode, "", FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value      = ENTRY(1,char-val).
        FIND FIRST itemfg WHERE RECID(itemfg) = look-recid NO-LOCK NO-ERROR.
        IF AVAILABLE itemfg THEN
            ASSIGN
                cCustPart:SCREEN-VALUE = itemfg.part-no
                item-name:SCREEN-VALUE = itemfg.i-name
                item-dscr:SCREEN-VALUE = itemfg.part-dscr1 .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFGItem Dialog-Frame
ON LEAVE OF cFGItem IN FRAME Dialog-Frame /* FG Item */
DO: 
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            ASSIGN {&self-name}.
        RUN valid-fgitem(OUTPUT lError) NO-ERROR.
        IF lError THEN RETURN NO-APPLY.  
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cFGItem Dialog-Frame
ON VALUE-CHANGED OF cFGItem IN FRAME Dialog-Frame /* FG Item */
DO:
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company = cocode
            AND itemfg.i-no EQ cFGItem:SCREEN-VALUE NO-ERROR.
        IF AVAILABLE itemfg THEN
            ASSIGN
                cCustPart:SCREEN-VALUE = itemfg.part-no
                item-name:SCREEN-VALUE = itemfg.i-name
                item-dscr:SCREEN-VALUE = itemfg.part-dscr1 .
        ASSIGN 
            lCreateNewFG = FALSE .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cSourceEst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSourceEst Dialog-Frame
ON HELP OF cSourceEst IN FRAME Dialog-Frame /* Source Estimate */
DO:
        DEFINE VARIABLE char-val   AS cha       NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID     NO-UNDO.
        DEFINE VARIABLE cEstNo     AS CHARACTER NO-UNDO.

        RUN windows/l-esttyp.w (cocode,g_loc,"568","EST",FOCUS:SCREEN-VALUE, OUTPUT char-val). 
        IF char-val <> "" THEN cSourceEst:SCREEN-VALUE = ENTRY(1,char-val).       
                 
        APPLY "entry" TO cSourceEst IN FRAME {&FRAME-NAME}.   
        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSourceEst Dialog-Frame
ON LEAVE OF cSourceEst IN FRAME Dialog-Frame /* Source Estimate */
DO: 
        DEFINE VARIABLE lError AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cEstNo AS CHARACTER NO-UNDO.
        IF LASTKEY NE -1 THEN 
        DO:
            cEstNo = cSourceEst:SCREEN-VALUE.
            IF cEstNo NE "" THEN 
            do:
                RUN util/rjust.p (INPUT-OUTPUT cEstNo,8).
                cSourceEst:SCREEN-VALUE = cEstNo.
                RUN valid-est-no(OUTPUT lError) NO-ERROR.
            
                IF lError THEN RETURN NO-APPLY.
                
                RUN pGetEstDetail.
                RUN pDisableField.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dep Dialog-Frame
ON HELP OF dep IN FRAME Dialog-Frame /* Depth */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dep Dialog-Frame
ON LEAVE OF dep IN FRAME Dialog-Frame /* Depth */
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME dQtyPerSet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dQtyPerSet Dialog-Frame
ON LEAVE OF dQtyPerSet IN FRAME Dialog-Frame /* Qty Per Set */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
         RUN valid-QtyPerSet(OUTPUT lError) NO-ERROR.
         IF lError THEN RETURN NO-APPLY.           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-cat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-cat Dialog-Frame
ON HELP OF fg-cat IN FRAME Dialog-Frame /* FG Category */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.

        RUN windows/l-fgcat.w (cocode,fg-cat:SCREEN-VALUE,OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value     = ENTRY(1,char-val)
                cat-dscr:screen-value = ENTRY(2,char-val) .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-cat Dialog-Frame
ON LEAVE OF fg-cat IN FRAME Dialog-Frame /* FG Category */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-procat(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY.
        
            IF SELF:SCREEN-VALUE NE "" THEN 
            DO:
                FIND FIRST fgcat WHERE fgcat.company = cocode
                    AND fgcat.procat EQ fg-cat:SCREEN-VALUE NO-LOCK NO-ERROR .
             
                IF AVAILABLE fgcat THEN
                    ASSIGN cat-dscr:SCREEN-VALUE = fgcat.dscr .
            END.
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-dscr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-dscr Dialog-Frame
ON LEAVE OF item-dscr IN FRAME Dialog-Frame /* Description */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
            
        END.
               
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME item-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL item-name Dialog-Frame
ON LEAVE OF item-name IN FRAME Dialog-Frame /* Item Name */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL len Dialog-Frame
ON LEAVE OF len IN FRAME Dialog-Frame /* Length */
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
        END.     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_show1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_show1 Dialog-Frame
ON VALUE-CHANGED OF rd_show1 IN FRAME Dialog-Frame
DO:
        ASSIGN {&self-name}.
          
        IF rd_show1 EQ "S" THEN             
            cSourceEst:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
        ELSE do:
           ASSIGN
                cSourceEst:SENSITIVE IN FRAME {&FRAME-NAME}    = NO
                cSourceEst:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" .
           RUN pEnableField.
        END.        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME set-item-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL set-item-name Dialog-Frame
ON LEAVE OF set-item-name IN FRAME Dialog-Frame /* Item Name */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO  .
        IF LASTKEY NE -1 THEN 
        DO:
           
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME style-cod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style-cod Dialog-Frame
ON HELP OF style-cod IN FRAME Dialog-Frame /* Style Code */
DO:
        DEFINE VARIABLE char-val   AS cha   NO-UNDO.
        DEFINE VARIABLE look-recid AS RECID NO-UNDO.
        
        IF ipcEstType EQ "Wood" THEN
        SharedConfig:Instance:SetValue("StyleLookup_TypeValue", "Wood").
        
        RUN windows/l-stylec.w (cocode,FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value <> entry(1,char-val) THEN 
            ASSIGN
                SELF:screen-value = ENTRY(1,char-val)
                .    
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST style WHERE style.company = cocode
                AND style.style EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR .

            IF AVAILABLE style THEN 
            DO:
                ASSIGN 
                    style-dscr:SCREEN-VALUE = style.dscr .                        
                RUN pGetBoardFromStyle(style.style).
                       
            END.
        END.
        system.SharedConfig:Instance:DeleteValue("StyleLookup_TypeValue").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style-cod Dialog-Frame
ON LEAVE OF style-cod IN FRAME Dialog-Frame /* Style Code */
DO:
        DEFINE VARIABLE lError AS LOGICAL NO-UNDO .    
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            RUN valid-style(OUTPUT lError) NO-ERROR.
            IF lError THEN RETURN NO-APPLY. 
        END.

        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST style WHERE style.company = cocode
                AND style.style EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR .

            IF AVAILABLE style THEN
                ASSIGN style-dscr:SCREEN-VALUE = style.dscr .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL style-cod Dialog-Frame
ON VALUE-CHANGED OF style-cod IN FRAME Dialog-Frame /* Style Code */
DO:     
        IF SELF:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST style NO-LOCK WHERE style.company = cocode
                AND style.style EQ SELF:SCREEN-VALUE  NO-ERROR .
                         
            IF AVAILABLE style THEN 
            DO:
                ASSIGN 
                    style-dscr:SCREEN-VALUE = style.dscr .
                RUN pGetBoardFromStyle(style.style).                  
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wid Dialog-Frame
ON LEAVE OF wid IN FRAME Dialog-Frame /* Width */
DO:
        DEFINE VARIABLE v-dec    AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-dec   AS DECIMAL DECIMALS 6 NO-UNDO.
        DEFINE VARIABLE op-error AS LOG     NO-UNDO.
        DEFINE VARIABLE len-num  AS INTEGER NO-UNDO.
   
        IF LASTKEY = -1 THEN RETURN.
        v-dec = DECIMAL(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0).
        IF LASTKEY <> -1 AND
            decimal(SELF:screen-value) - trunc(DECIMAL(SELF:screen-value),0) >= v-16-or-32 
            THEN 
        DO:
            MESSAGE "Can not have more than " v-16-or-32 - 0.01 " as decimal, field is (inches.16ths/32nd's) "
                VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.

        IF v-cecscrn-dec THEN
        DO:
            len-num = INT(SELF:screen-value) .
            RUN valid-64-dec(INPUT v-dec, OUTPUT op-error, OUTPUT op-dec).
            IF op-error THEN 
            DO:
                MESSAGE "Invalid Dimension."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                APPLY "ENTRY" TO SELF.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO: 
          
            /* eb.len:screen-value = string( len-num +  op-dec) . */
            END.
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

    /*IF ip-recid EQ ? THEN 
    DO:
        RUN create-item.
    END.
    ELSE FIND estPacking NO-LOCK WHERE RECID(estPacking) EQ ip-recid NO-ERROR.*/

    IF ip-type NE "view" THEN 
    DO: 
        
        RUN enable_UI.
        RUN display-item.

        ASSIGN 
            ll-order-warned = NO.
        btn_done:HIDDEN IN FRAME {&FRAME-NAME} = YES.
        IF cSourceEst:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN
        do:
            ASSIGN                 
                cSourceEst:SENSITIVE IN FRAME {&FRAME-NAME}            = YES.
                RUN pDisableField.
        END.        
        ELSE cSourceEst:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
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
    /*FIND CURRENT estPacking NO-LOCK NO-ERROR .*/
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
    DEFINE BUFFER bf-ttInputEst FOR ttInputEst .
    FIND FIRST ttInputEst WHERE RECID(ttInputEst) EQ ip-recid NO-ERROR. 
    IF AVAILABLE ttInputEst THEN 
        ASSIGN
            iForm      = ttInputEst.iFormNo
            iBlank     = ttInputEst.iBlankNo
            dQtyPerSet = ttInputEst.dQtyPerSet
            cCustPart  = ttInputEst.cPartID
            cFGItem    = ttInputEst.cStockNo
            style-cod  = ttInputEst.cStyle
            board      = ttInputEst.cBoard
            fg-cat     = ttInputEst.cCategory
            item-name  = ttInputEst.cPartName
            item-dscr  = ttInputEst.cPartDescription
            len        = ttInputEst.dLength
            wid        = ttInputEst.dWidth
            dep        = ttInputEst.dDepth
            rd_show1   = IF ttInputEst.lPurchased AND ttInputEst.cSourceEst NE "" THEN "S" ELSE IF ttInputEst.lPurchased THEN "P" ELSE "M" 
            cSourceEst = ttInputEst.cSourceEst.
    est-no = IF AVAILABLE eb THEN eb.est-no ELSE "".     
    cSetCustPart = ipcSetPart.
    set-item-name = ipcSetPartName.
    IF ip-type EQ "Add" OR ip-type EQ "Copy" THEN 
    DO:
        j = 0 .
        FOR EACH bf-ttInputEst NO-LOCK BREAK BY bf-ttInputEst.iForm:
            IF FIRST-OF(bf-ttInputEst.iForm) THEN
                j  = j + 1.
        END.
          
        iForm       =  (j + 1).
        iBlank      =  1 .  
        IF iplAutoPart AND ip-type EQ "Add" THEN 
        DO:
            cCustPart = SUBSTRING(ipcPartNo,1,12) + "-" + string(j + 1 ,"99").              
        END.
        IF ip-type EQ "Add" THEN
        fg-cat = ipcProCat.
    END.
       
    FIND FIRST style NO-LOCK WHERE style.company = cocode
        AND style.style EQ style-cod NO-ERROR .
        
    IF AVAILABLE style THEN
        ASSIGN style-dscr = style.dscr .
    FIND FIRST fgcat NO-LOCK WHERE fgcat.company = cocode
        AND fgcat.procat EQ fg-cat NO-ERROR .
    IF AVAILABLE fgcat THEN
        ASSIGN cat-dscr = fgcat.dscr .
            
    FIND FIRST ITEM NO-LOCK WHERE item.company = cocode
        AND item.i-no EQ board NO-ERROR .
    IF AVAILABLE ITEM THEN
        ASSIGN board-dscr = item.i-name . 
        
    RUN pSetLWDFormat. 
    
    DISPLAY   
        est-no iForm iBlank cSetCustPart dQtyPerSet set-item-name cCustPart  
        style-cod style-dscr board fg-cat board-dscr cat-dscr item-name item-dscr 
        rd_show1 len wid dep cFGItem cSourceEst
        WITH FRAME Dialog-Frame.       
   
    IF ipcEstType EQ "SetSubAssembly" THEN
    DO:
        board:LABEL IN FRAME {&FRAME-NAME} = "Board". 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetTempValue Dialog-Frame 
PROCEDURE pGetTempValue :
    /*------------------------------------------------------------------------------
                          Purpose:     
                          PARAMs:  <none>
                          Notes:       
         ------------------------------------------------------------------------------*/   
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST bf-eb NO-LOCK
            WHERE bf-eb.company EQ cocode
            AND bf-eb.est-no EQ cSourceEst:SCREEN-VALUE
            AND (bf-eb.form-no NE 0 OR (bf-eb.est-type EQ 6 AND bf-eb.form-no EQ 0)) NO-ERROR.
                  
        IF AVAILABLE bf-eb THEN 
        do:           
            FIND FIRST ef NO-LOCK
                WHERE ef.company EQ bf-eb.company
                AND ef.est-no EQ bf-eb.est-no
                AND (ef.form-no EQ bf-eb.form-no OR bf-eb.form-no EQ 0) 
                NO-ERROR. 
            ASSIGN               
                ttInputEst.cBndlCode    = bf-eb.cas-no
                ttInputEst.iUnitCount   = bf-eb.cas-cnt
                ttInputEst.iPerPallet   = bf-eb.cas-pal
                ttInputEst.iPartial     = bf-eb.quantityPartial
                ttInputEst.cPallet      = bf-eb.tr-no
                ttInputEst.dWeightPerM  = bf-eb.weight
                ttInputEst.iStackHeight = bf-eb.stackHeight
                ttInputEst.iStackCode   = bf-eb.stack-code 
                ttInputEst.cFlute       = bf-eb.flute
                ttInputEst.cTest        = bf-eb.test 
                ttInputEst.dMSF         = ef.cost-msh
                ttInputEst.dForceFrt    = IF ef.fr-uom EQ "CWT" THEN bf-eb.fr-out-c ELSE bf-eb.fr-out-m 
                ttInputEst.cForceFrtUom = IF ef.fr-uom EQ "CWT" THEN "CWT" ELSE "M"
                ttInputEst.cAddersDscr1 = ef.adder[7] 
                ttInputEst.cAddersDscr2 = ef.adder[8]
                ttInputEst.cAddersDscr3 = ef.adder[9]
                ttInputEst.cProject     = bf-eb.spc-no
                ttInputEst.cCadID       = bf-eb.cad-no
                ttInputEst.cSourceEst   = cSourceEst:SCREEN-VALUE   .             
        END.         
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetEstDetail Dialog-Frame 
PROCEDURE pGetEstDetail :
    /*------------------------------------------------------------------------------
                          Purpose:     
                          PARAMs:  <none>
                          Notes:       
         ------------------------------------------------------------------------------*/   
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST bf-eb NO-LOCK
            WHERE bf-eb.company EQ cocode
            AND bf-eb.est-no EQ cSourceEst:SCREEN-VALUE
            AND (bf-eb.form-no NE 0 OR (bf-eb.est-type EQ 6 AND bf-eb.form-no EQ 0)) NO-ERROR.
                  
        IF AVAILABLE bf-eb THEN 
        do:           
            FIND FIRST ef NO-LOCK
                WHERE ef.company EQ bf-eb.company
                AND ef.est-no EQ bf-eb.est-no
                AND (ef.form-no EQ bf-eb.form-no OR bf-eb.form-no EQ 0) 
                NO-ERROR. 
            ASSIGN                
                cCustPart:SCREEN-VALUE = bf-eb.part-no
                cFGItem:SCREEN-VALUE   = bf-eb.stock-no
                item-name:SCREEN-VALUE = bf-eb.part-dscr1
                item-dscr:SCREEN-VALUE = bf-eb.part-dscr2
                len:SCREEN-VALUE       = string(bf-eb.len)
                wid:SCREEN-VALUE       = string(bf-eb.wid)
                dep:SCREEN-VALUE       = string(bf-eb.dep)
                style-cod:SCREEN-VALUE = bf-eb.style
                board:SCREEN-VALUE     = IF AVAILABLE ef THEN ef.board ELSE ""
                fg-cat:SCREEN-VALUE    = bf-eb.procat
                .
            IF ipcEstType EQ "Wood" THEN
            rd_show1:SCREEN-VALUE  = IF bf-eb.pur-man THEN "P" ELSE "M".  
               
            FIND FIRST ITEM NO-LOCK WHERE item.company = cocode
                AND item.i-no EQ board:SCREEN-VALUE NO-ERROR .
            IF AVAILABLE ITEM THEN
                ASSIGN board-dscr:SCREEN-VALUE = item.i-name .

            FIND FIRST style NO-LOCK WHERE style.company = cocode
                AND style.style EQ style-cod:SCREEN-VALUE NO-ERROR .
            IF NOT AVAILABLE style THEN 
                FIND FIRST style NO-LOCK 
                WHERE style.company EQ cocode
                NO-ERROR.
            IF AVAILABLE style THEN
                ASSIGN 
                    style-cod:SCREEN-VALUE = style.style
                    style-dscr:SCREEN-VALUE = style.dscr 
                    . 
            
            FIND FIRST fgcat NO-LOCK WHERE fgcat.company = cocode
                AND fgcat.procat EQ fg-cat:SCREEN-VALUE NO-ERROR .
            IF AVAILABLE fgcat THEN
                ASSIGN cat-dscr:SCREEN-VALUE = fgcat.dscr .    
            
        END.         
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisableField D-Dialog 
PROCEDURE pDisableField :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/       

    DO WITH FRAME {&FRAME-NAME}:
        DISABLE set-item-name style-cod style-dscr board fg-cat 
        board-dscr cat-dscr len wid dep item-name item-dscr .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pEnableField D-Dialog 
PROCEDURE pEnableField :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/       

    DO WITH FRAME {&FRAME-NAME}:
        ENABLE set-item-name style-cod style-dscr board fg-cat 
        board-dscr cat-dscr len wid dep item-name item-dscr .
    END.

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
  DISPLAY est-no iForm iBlank cSetCustPart dQtyPerSet set-item-name cCustPart 
          cFGItem style-cod style-dscr board fg-cat board-dscr cat-dscr len wid 
          dep rd_show1 item-name item-dscr cSourceEst 
      WITH FRAME Dialog-Frame.
  ENABLE dQtyPerSet Btn_OK Btn_Done Btn_Cancel cCustPart cFGItem style-cod 
         board fg-cat len wid dep rd_show1 item-name item-dscr RECT-21 RECT-38 
         RECT-39 cSourceEst 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetBoardFromStyle Dialog-Frame 
PROCEDURE pGetBoardFromStyle :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcStyle AS CHARACTER NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
    
        FIND FIRST flute NO-LOCK
            WHERE flute.company EQ cocode NO-ERROR .
        IF AVAILABLE flute THEN
            FIND FIRST reftable WHERE reftable.reftable = "STYFLU" AND reftable.company = ipcStyle 
                AND reftable.loc = flute.code
                AND reftable.code = "BOARD"
                NO-LOCK NO-ERROR. 
        board:screen-value = IF AVAILABLE reftable AND AVAILABLE flute AND reftable.dscr NE "" THEN reftable.dscr ELSE board:screen-value.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetLWDFormat Dialog-Frame 
PROCEDURE pSetLWDFormat :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iDecimalValue AS INTEGER NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
       IF v-cecscrn-char EQ "Decimal" THEN do:
          iDecimalValue = IF INTEGER(v-cecscrn-decimals) EQ 0 THEN 6 ELSE INTEGER(v-cecscrn-decimals) .

          ASSIGN
              len:FORMAT = ">>9." + FILL("9",INTEGER(iDecimalValue))
              len:WIDTH  = 12.5
              wid:FORMAT = ">>9." + FILL("9",INTEGER(iDecimalValue))   
              wid:WIDTH  = 12.5
              dep:FORMAT = ">>9." + FILL("9",INTEGER(iDecimalValue))
              dep:WIDTH  = 12.5.        
       END.   
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-64-dec Dialog-Frame 
PROCEDURE valid-64-dec :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-dec AS DECIMAL DECIMALS 6 NO-UNDO.
    DEFINE OUTPUT PARAMETER op-error AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER op-dec AS DECIMAL DECIMALS 6 NO-UNDO.
    
    FIND FIRST tt-64-dec WHERE
        SUBSTRING(STRING(tt-64-dec.DEC),1,3) EQ substring(STRING(ip-dec),1,3) NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt-64-dec  THEN
        op-error = YES.
    ELSE  op-dec = tt-64-dec.DEC .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-est-no D-Dialog 
PROCEDURE valid-est-no :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DEFINE BUFFER b-eb  FOR eb.
    DEFINE BUFFER b-est FOR est.
    DO WITH FRAME {&FRAME-NAME}:
        IF trim(cSourceEst:SCREEN-VALUE) NE "0" AND NOT CAN-FIND(FIRST b-eb
            WHERE b-eb.company  EQ cocode
            AND b-eb.est-no    EQ cSourceEst:SCREEN-VALUE)  THEN 
        DO:
            MESSAGE "Invalid Estimate, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cSourceEst .
            oplOutError = YES .
        END.
        IF lMiscEstimateSource AND iMiscEstimateSource EQ 1 AND CAN-FIND(FIRST b-est
            WHERE b-est.company  EQ cocode
            AND b-est.est-no    EQ cSourceEst:SCREEN-VALUE
            AND ((b-est.estimateTypeID EQ "SetSubAssembly" AND ipcEstType EQ "SetSubAssembly")
            OR (b-est.estimateTypeID EQ "NewSetEstimate" AND ipcEstType EQ "Wood"))) THEN 
        DO:
            MESSAGE "Estimate is already Set SubAssembly estimate , try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO cSourceEst .
            oplOutError = YES .
        END.
        
        FIND FIRST b-eb NO-LOCK
            WHERE b-eb.company  EQ cocode
            AND b-eb.sourceEstimate   EQ cSourceEst:SCREEN-VALUE NO-ERROR.
        
        IF AVAILABLE b-eb AND CAN-FIND(FIRST b-est
            WHERE b-est.company  EQ cocode
            AND b-est.est-no   EQ b-eb.est-no
            AND ((b-est.estimateTypeID EQ "SetSubAssembly" AND ipcEstType EQ "SetSubAssembly")
            OR (b-est.estimateTypeID EQ "NewSetEstimate" AND ipcEstType EQ "Wood")))  THEN 
        DO:
            MESSAGE "Estimate #" trim(b-eb.est-no) " already exists for this source estimate,  Continue?" 
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
            IF NOT ll-ans THEN 
            do:  
                APPLY "entry" TO cSourceEst .
                oplOutError = YES .
            END.
        END.
        
        IF trim(cSourceEst:SCREEN-VALUE) NE "0" AND  cMiscEstimateSource EQ "Quote" THEN
        DO:
            IF NOT CAN-FIND(FIRST quotehd
                WHERE quotehd.company  EQ cocode
                AND quotehd.est-no    EQ cSourceEst:SCREEN-VALUE)  THEN 
            DO:
                MESSAGE "N-K-1 MiscEstimateSource is set to get prices from the quote, so without a quote you may not proceed." VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO cSourceEst .
                oplOutError = YES .
            END.        
        END.
        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fgitem Dialog-Frame 
PROCEDURE valid-fgitem :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
     
        IF cFGItem:SCREEN-VALUE  NE "" AND NOT lCreateNewFG THEN 
        DO:
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company  EQ cocode
                AND itemfg.i-no    EQ cFGItem:SCREEN-VALUE  NO-ERROR.
            IF NOT AVAILABLE itemfg  THEN 
            DO:
                MESSAGE "This item does not exist, would you like to add it?" 
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE ll-ans AS LOG.
                IF ll-ans THEN
                    ASSIGN lCreateNewFG = TRUE .
                IF NOT ll-ans THEN 
                DO:
                    APPLY "entry" TO cFGItem .
                    oplOutError = YES .
                END.
            END.
        END.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-Form-Blank Dialog-Frame 
PROCEDURE valid-Form-Blank :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DEFINE BUFFER bf-ttInputEst FOR ttInputEst .
    
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST bf-ttInputEst  NO-LOCK
            WHERE bf-ttInputEst.cCompany  EQ cocode
            AND bf-ttInputEst.iFormNo    EQ INTEGER(iForm:SCREEN-VALUE)
            AND bf-ttInputEst.iBlankNo   EQ integer(iBlank:SCREEN-VALUE)
            AND RECID(bf-ttInputEst) NE ip-recid NO-ERROR.
        IF AVAILABLE bf-ttInputEst THEN 
        DO:
            MESSAGE "Form and blank already Entered ..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO iForm .
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-no Dialog-Frame 
PROCEDURE valid-part-no :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        IF cCustPart:SCREEN-VALUE  EQ "" THEN 
        DO:
            MESSAGE "Customer Part # required..." VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO cCustPart .
            oplOutError = YES .
        END.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-procat Dialog-Frame 
PROCEDURE valid-procat :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        fg-cat:SCREEN-VALUE  = CAPS(fg-cat:SCREEN-VALUE).

        IF NOT CAN-FIND(FIRST fgcat
            WHERE fgcat.company EQ cocode
            AND fgcat.procat  EQ fg-cat:SCREEN-VALUE) THEN 
        DO:
            MESSAGE "Invalid FG Category, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fg-cat .
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-QtyPerSet Dialog-Frame 
PROCEDURE valid-QtyPerSet :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        IF integer(dQtyPerSet:SCREEN-VALUE)  LE 0 THEN 
        DO:
            MESSAGE "Qty Per Set must be greater then 0..." VIEW-AS ALERT-BOX INFORMATION.
            APPLY "entry" TO dQtyPerSet .
            oplOutError = YES .
        END.
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-style Dialog-Frame 
PROCEDURE valid-style :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOutError AS LOGICAL NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST style
            WHERE style.company  EQ cocode
            AND style.style    EQ style-cod:SCREEN-VALUE
            AND style.industry EQ "2")  THEN 
        DO:
            MESSAGE "Invalid Style Code, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO style-cod .
            oplOutError = YES .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


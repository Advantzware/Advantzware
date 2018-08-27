&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: po\d-poordl.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-recid2 AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO .   /* add,update,view */
DEFINE OUTPUT PARAMETER ip-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN 
    cocode = g_company.
ASSIGN 
    locode = g_loc.

DEFINE VARIABLE li AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE tt-est-prep LIKE est-prep 
    FIELD row-id AS ROWID 
    INDEX row-id row-id.

{est/d-selblk.i NEW}
{sys/inc/ceprepprice.i}

DEFINE VARIABLE lv-item-recid   AS RECID   NO-UNDO.
DEFINE VARIABLE ll-order-warned AS LOGICAL NO-UNDO.
DEFINE VARIABLE ll-new-record   AS LOGICAL NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES est-prep est

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame est-prep.s-num est-prep.b-num ~
est-prep.code est-prep.qty est-prep.dscr est-prep.simon est-prep.cost ~
est-prep.mkup est-prep.spare-dec-1 est-prep.ml est-prep.amtz 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame est-prep.s-num ~
est-prep.b-num est-prep.code est-prep.qty est-prep.dscr est-prep.simon ~
est-prep.cost est-prep.mkup est-prep.spare-dec-1 est-prep.ml est-prep.amtz 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame est-prep
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame est-prep
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH est-prep ~
      WHERE ASI.est-prep.company eq cocode  SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH est-prep ~
      WHERE ASI.est-prep.company eq cocode  SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame est-prep est
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame est-prep
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame est


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS est-prep.s-num est-prep.b-num est-prep.code ~
est-prep.qty est-prep.dscr est-prep.simon est-prep.cost est-prep.mkup ~
est-prep.spare-dec-1 est-prep.ml est-prep.amtz 
&Scoped-define ENABLED-TABLES est-prep
&Scoped-define FIRST-ENABLED-TABLE est-prep
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 
&Scoped-Define DISPLAYED-FIELDS est-prep.s-num est-prep.b-num est-prep.code ~
est-prep.qty est-prep.dscr est-prep.simon est-prep.cost est-prep.mkup ~
est-prep.spare-dec-1 est-prep.ml est-prep.amtz 
&Scoped-define DISPLAYED-TABLES est-prep
&Scoped-define FIRST-DISPLAYED-TABLE est-prep


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
    LABEL "Cancel" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
    LABEL "&Done" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
    LABEL "&Save" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE RECTANGLE RECT-21
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 127 BY 3.57.

DEFINE RECTANGLE RECT-38
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 127 BY 10.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
    est-prep, 
    est SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    est-prep.s-num AT ROW 2.43 COL 28.4 COLON-ALIGNED
    LABEL "Sheet #" FORMAT ">>>"
    VIEW-AS FILL-IN 
    SIZE 16.4 BY 1
    est-prep.b-num AT ROW 2.43 COL 80.6 COLON-ALIGNED
    LABEL "Blank#" FORMAT ">>>"
    VIEW-AS FILL-IN 
    SIZE 16.4 BY 1
    est-prep.code AT ROW 3.67 COL 28.4 COLON-ALIGNED
    LABEL "Code" FORMAT "x(20)"
    VIEW-AS FILL-IN 
    SIZE 31.6 BY 1
    est-prep.qty AT ROW 3.67 COL 80.6 COLON-ALIGNED
    LABEL "Qty" FORMAT "->>,>>9.9"
    VIEW-AS FILL-IN 
    SIZE 16.4 BY 1
    est-prep.dscr AT ROW 4.91 COL 28.4 COLON-ALIGNED
    LABEL "Desc." FORMAT "x(20)"
    VIEW-AS FILL-IN 
    SIZE 31.6 BY 1
    est-prep.simon AT ROW 4.91 COL 80.6 COLON-ALIGNED
    LABEL "SIMON" FORMAT "x"
    VIEW-AS FILL-IN 
    SIZE 16.4 BY 1
    est-prep.cost AT ROW 6.14 COL 28.4 COLON-ALIGNED
    LABEL "Cost" FORMAT "->>,>>9.99"
    VIEW-AS FILL-IN 
    SIZE 16.4 BY 1
    est-prep.mkup AT ROW 6.14 COL 80.6 COLON-ALIGNED
    LABEL "Markup" FORMAT "->>9.9999"
    VIEW-AS FILL-IN 
    SIZE 16.4 BY 1
    est-prep.spare-dec-1 AT ROW 7.38 COL 28.4 COLON-ALIGNED
    LABEL "Price" FORMAT "->>,>>9.99"
    VIEW-AS FILL-IN 
    SIZE 16.4 BY 1
    est-prep.ml AT ROW 7.38 COL 80.6 COLON-ALIGNED
    LABEL "M/L" FORMAT "M/L"
    VIEW-AS FILL-IN 
    SIZE 16.4 BY 1
    est-prep.amtz AT ROW 8.67 COL 28.4 COLON-ALIGNED
    LABEL "Amort" FORMAT ">>9.99"
    VIEW-AS FILL-IN 
    SIZE 16.4 BY 1
    Btn_OK AT ROW 13.29 COL 37
    Btn_Done AT ROW 13.24 COL 57
    Btn_Cancel AT ROW 13.24 COL 77.2
    RECT-21 AT ROW 11.71 COL 1
    RECT-38 AT ROW 1 COL 1
    SPACE(1.39) SKIP(4.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FONT 6
    TITLE "Estimate Preparation Update".


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

/* SETTINGS FOR FILL-IN est-prep.amtz IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-prep.b-num IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-prep.code IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-prep.cost IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-prep.dscr IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-prep.mkup IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-prep.ml IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-prep.qty IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-prep.s-num IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-prep.simon IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN est-prep.spare-dec-1 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "asi.est-prep,asi.est "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.est-prep.company eq cocode "
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Estimate Prep Item Update */
    ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Estimate Prep Item Update */
    DO:
        DISABLE TRIGGERS FOR LOAD OF est-prep .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST est-prep EXCLUSIVE-LOCK
                WHERE RECID(est-prep) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE est-prep THEN DELETE est-prep .
        END.

        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-prep.b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-prep.b-num Dialog-Frame
ON ENTRY OF est-prep.b-num IN FRAME Dialog-Frame /* Blank# */
    DO:
        /*single item estimate*/
        IF est.est-type EQ 1 OR est.est-type EQ 5 THEN
        DO:
            APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-prep.b-num Dialog-Frame
ON LEAVE OF est-prep.b-num IN FRAME Dialog-Frame /* Blank# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-b-num NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
    DO:
        DISABLE TRIGGERS FOR LOAD OF est-prep .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST est-prep EXCLUSIVE-LOCK
                WHERE RECID(est-prep) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE est-prep THEN DELETE est-prep .
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
        DEFINE VARIABLE v-qty    AS DECIMAL NO-UNDO.
        DEFINE VARIABLE ll       AS LOGICAL NO-UNDO.
        DEFINE VARIABLE op-error AS LOGICAL NO-UNDO.
  
 
        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
        RUN valid-s-num NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-b-num NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.  

        RUN valid-code(0) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
        RUN valid-simon NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
        IF ceprepprice-chr EQ "Profit" AND
            DEC(est-prep.mkup:SCREEN-VALUE ) GE 100 THEN
        DO:
            MESSAGE "Invalid Markup."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "entry" TO est-prep.mkup.
            RETURN NO-APPLY.
        END.

        DO TRANSACTION:
            FIND CURRENT est-prep EXCLUSIVE-LOCK NO-ERROR.

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
            END.
        END.

        FIND FIRST prep
            WHERE prep.company EQ est.company
            AND prep.loc     EQ est.loc
            AND prep.code    EQ est-prep.code
            NO-LOCK NO-ERROR.
        IF AVAILABLE prep THEN est-prep.mat-type = prep.mat-type.

        /* check order whether plate's existing. if exist, create reftalbe for link*/
        IF est-prep.mat-type = "P" AND est.ord-no <> 0 AND est-prep.simon   EQ "S"
            AND est-prep.amtz    EQ 100 
            THEN  RUN create-reft4plate.

        ip-rowid = ROWID(est-prep).

        APPLY "go" TO FRAME {&FRAME-NAME}.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-prep.code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-prep.code Dialog-Frame
ON HELP OF est-prep.code IN FRAME Dialog-Frame /* Code */
    DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.
    
       
        RUN windows/l-prep.w (est.company, SELF:screen-value  , OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value  NE entry(1,char-val) THEN 
        DO:
            SELF:screen-value  = ENTRY(1,char-val).
            APPLY "value-changed" TO SELF . 
        END.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME est-prep.simon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-prep.simon Dialog-Frame
ON HELP OF est-prep.simon IN FRAME Dialog-Frame /* simon */
    DO:
        DEFINE VARIABLE char-val AS cha NO-UNDO.
    
       
        RUN windows/l-codedscr.w ("S,I,M,O,N","Separate,Integrate,Maintenance,Other, No charge", OUTPUT char-val).
        IF char-val <> "" AND SELF:screen-value  NE entry(1,char-val) THEN 
        DO:
            SELF:screen-value  = ENTRY(1,char-val).
            APPLY "value-changed" TO SELF . 
        END.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-prep.s-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-prep.s-num Dialog-Frame
ON HELP OF est-prep.s-num IN FRAME Dialog-Frame /* Code */
    DO:
        DEFINE VARIABLE rowidval AS ROWID NO-UNDO.
        DEFINE BUFFER bff-eb FOR eb . 
       
        RUN windows/l-esteb.w (est.company,locode, IF AVAILABLE est THEN est.est-no ELSE ""  , INPUT-OUTPUT rowidval).
        IF rowidval <> ? THEN 
        DO:
            FIND FIRST bff-eb NO-LOCK WHERE ROWID(bff-eb) EQ rowidval  NO-ERROR .
            IF AVAILABLE bff-eb THEN
                est-prep.s-num:screen-value  = STRING(bff-eb.FORM-no) .
        END.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-prep.b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-prep.b-num Dialog-Frame
ON HELP OF est-prep.b-num IN FRAME Dialog-Frame /* Code */
    DO:
        DEFINE VARIABLE rowidval AS ROWID NO-UNDO.
        DEFINE BUFFER bff-eb FOR eb . 
       
        RUN windows/l-esteb.w (est.company,locode, IF AVAILABLE est THEN est.est-no ELSE ""  , INPUT-OUTPUT rowidval).
        IF rowidval <> ? THEN 
        DO:
            FIND FIRST bff-eb NO-LOCK WHERE ROWID(bff-eb) EQ rowidval  NO-ERROR .
            IF AVAILABLE bff-eb THEN
                est-prep.b-num:screen-value  = STRING(bff-eb.blank-no) .
        END.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-prep.code Dialog-Frame
ON LEAVE OF est-prep.code IN FRAME Dialog-Frame /* Code */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-code(1) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-prep.code Dialog-Frame
ON VALUE-CHANGED OF est-prep.code IN FRAME Dialog-Frame /* Code */
    DO:
        DEFINE VARIABLE v-qty AS DECIMAL NO-UNDO.

        FIND FIRST prep
            WHERE prep.company EQ est.company
            AND prep.code    EQ est-prep.code:SCREEN-VALUE 
            NO-LOCK NO-ERROR.
  
        IF AVAILABLE prep THEN 
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN /*focus:screen-value  = caps(prep.code)*/
                est-prep.dscr:screen-value        = prep.dscr
                est-prep.cost:screen-value        = STRING(prep.cost)
                est-prep.ml:screen-value          = STRING(prep.ml)
                est-prep.simon:screen-value       = STRING(prep.simon)
                est-prep.mkup:screen-value        = STRING(prep.mkup)
                est-prep.spare-dec-1:screen-value = STRING(prep.spare-dec-1)
                est-prep.amtz:screen-value        = STRING(prep.amtz)
                .
            v-qty = dec(est-prep.qty:screen-value).
            RUN est/GetPrepQty.p(INPUT ROWID(est),
                INPUT prep.mat-type,
                INPUT int(est-prep.s-num:SCREEN-VALUE ),
                OUTPUT v-qty).

            est-prep.qty:screen-value = STRING(v-qty).
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-prep.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-prep.cost Dialog-Frame
ON LEAVE OF est-prep.cost IN FRAME Dialog-Frame /* Cost */
    DO:
        RUN UpdatePrice.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-prep.mkup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-prep.mkup Dialog-Frame
ON LEAVE OF est-prep.mkup IN FRAME Dialog-Frame /* Markup */
    DO:
        IF LASTKEY NE -1 AND ceprepprice-chr EQ "Profit" AND
            DEC(est-prep.mkup:SCREEN-VALUE ) GE 100 THEN
        DO:
            MESSAGE "Invalid Markup."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "entry" TO est-prep.mkup.
            RETURN NO-APPLY.
        END.
        RUN UpdatePrice.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-prep.s-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-prep.s-num Dialog-Frame
ON ENTRY OF est-prep.s-num IN FRAME Dialog-Frame /* Sheet # */
    DO:
        /*single item est*/
        IF est.est-type EQ 1 OR est.est-type EQ 5 THEN
        DO:
            IF AVAILABLE est-prep THEN
                est-prep.s-num:SCREEN-VALUE = "1".

            APPLY "tab" TO SELF /*{&self-name}*/ .
            RETURN NO-APPLY.
  
        END.
        ELSE                                            
            IF est.form-qty <= 1 THEN 
            DO WITH FRAME {&FRAME-NAME}  :
                IF adm-adding-record AND AVAILABLE est-prep THEN
                    est-prep.s-num:SCREEN-VALUE  = "1".

                APPLY "tab" TO SELF /*{&self-name}*/ .     
                RETURN NO-APPLY.
            END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-prep.s-num Dialog-Frame
ON LEAVE OF est-prep.s-num IN FRAME Dialog-Frame /* Sheet # */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-num NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-prep.simon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-prep.simon Dialog-Frame
ON LEAVE OF est-prep.simon IN FRAME Dialog-Frame /* SIMON */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-simon NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-prep.spare-dec-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-prep.spare-dec-1 Dialog-Frame
ON VALUE-CHANGED OF est-prep.spare-dec-1 IN FRAME Dialog-Frame /* Price */
    DO:
        RUN UpdateMarkup.
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

    FIND FIRST est NO-LOCK 
        WHERE est.company EQ cocode 
        AND RECID(est) EQ ip-recid2 NO-ERROR .

    
    IF ip-type EQ "copy" THEN lv-item-recid = ip-recid.


    IF ip-recid EQ ? THEN 
    DO:
        RUN create-item.
       
    END.
    ELSE FIND est-prep NO-LOCK WHERE RECID(est-prep) EQ ip-recid NO-ERROR.

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

    DO WITH FRAME {&FRAME-NAME}:
        IF ip-type EQ "update" THEN DISABLE est-prep.s-num est-prep.b-num.

        IF ip-type EQ "add"  OR ip-type EQ "copy" THEN 
        DO:
            APPLY "entry" TO est-prep.s-num  .
        END.
    /*est-prep.s-no:HIDDEN IN FRAME {&FRAME-NAME}  = TRUE .*/
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
    DEFINE BUFFER bf-prep FOR est-prep.
    DEFINE VARIABLE li-next AS INTEGER NO-UNDO.

    FOR EACH bf-prep
        WHERE bf-prep.company EQ est.company 
        AND bf-prep.est-no  EQ est.est-no                      
        USE-INDEX est-qty NO-LOCK
        BY bf-prep.line DESCENDING:
        LEAVE.
    END.

    li-next = (IF AVAILABLE bf-prep THEN bf-prep.line ELSE 0) + 1.

    
    DO WITH FRAME {&FRAME-NAME}:

      
        CREATE est-prep.
        ASSIGN 
            lv-item-recid = RECID(est-prep).
        ll-new-record = YES.

        ASSIGN
            est-prep.e-num   = est.e-num
            est-prep.company = est.company
            est-prep.est-no  = est.est-no
            est-prep.line    = li-next
            est-prep.s-num   = 1
            est-prep.b-num   = 0
            est-prep.qty     = 1.
        
        FIND CURRENT est-prep NO-LOCK NO-ERROR.
    END. /* avail oe-relh */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-reft4plate Dialog-Frame 
PROCEDURE create-reft4plate :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-prep-cnt AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-returnc  AS cha     NO-UNDO.
    DEFINE VARIABLE lv-form#    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-line#    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-eqty     AS INTEGER NO-UNDO.

    FIND FIRST oe-ordm WHERE oe-ordm.company = est.company
        AND oe-ordm.ord-no = est.ord-no
        AND oe-ordm.charge = est-prep.CODE NO-ERROR.
    IF AVAILABLE oe-ordm AND 
        (oe-ordm.miscType <> 1 OR
        oe-ordm.estPrepEqty <> est-prep.eqty OR
        oe-ordm.estPrepLine <> est-prep.line)      
     
        THEN 
    DO:
        ASSIGN 
            oe-ordm.miscType    = 1
            oe-ordm.estPrepEqty = est-prep.eqty
            oe-ordm.estPrepLine = est-prep.line
            oe-ordm.est-no      = est-prep.est-no.  
        RELEASE oe-ordm.
    END.
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
    IF AVAILABLE est-prep  THEN 
    DO:
        
        DISPLAY est-prep.s-num est-prep.b-num 
            est-prep.code est-prep.qty est-prep.dscr est-prep.simon est-prep.cost 
            est-prep.mkup est-prep.spare-dec-1 est-prep.ml est-prep.amtz 
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
    IF AVAILABLE est-prep THEN 
        DISPLAY est-prep.s-num est-prep.b-num est-prep.code est-prep.qty est-prep.dscr 
            est-prep.simon est-prep.cost est-prep.mkup est-prep.spare-dec-1 
            est-prep.ml est-prep.amtz 
            WITH FRAME Dialog-Frame.
    ENABLE est-prep.s-num est-prep.b-num est-prep.code est-prep.qty est-prep.dscr 
        est-prep.simon est-prep.cost est-prep.mkup est-prep.spare-dec-1 
        est-prep.ml est-prep.amtz Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateMarkup Dialog-Frame 
PROCEDURE UpdateMarkup :
    /*------------------------------------------------------------------------------
      Purpose:    Calculates Markup
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dCost  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMkup  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPrice AS DECIMAL NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            dCost  = DEC(est-prep.cost:SCREEN-VALUE )
            dPrice = DEC(est-prep.spare-dec-1:SCREEN-VALUE ).
   
        IF ceprepprice-chr EQ "Profit" THEN
            dMkup = (1 - dCost / dPrice) * 100. 
        ELSE
            dMkup = (dPrice / dCost - 1) * 100.
    
        IF dMkup LT 1000 AND dMkup GT -1000  THEN
            est-prep.mkup:SCREEN-VALUE  = STRING(dMkup).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdatePrice Dialog-Frame 
PROCEDURE UpdatePrice :
    /*------------------------------------------------------------------------------
      Purpose:    Calculates Price and displays it
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dCost  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMkup  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPrice AS DECIMAL NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN 
            dCost = DEC(est-prep.cost:SCREEN-VALUE )
            dMkup = DEC(est-prep.mkup:SCREEN-VALUE ).
        IF ceprepprice-chr EQ "Profit" THEN
            dPrice = dCost / (1 - (dMkup / 100)).
        ELSE
            dPrice = dCost * (1 + (dMkup / 100)).
        est-prep.spare-dec-1:SCREEN-VALUE  = STRING(dPrice).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-b-num Dialog-Frame 
PROCEDURE valid-b-num :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-eb FOR eb.


    DO WITH FRAME {&FRAME-NAME}:
        IF INT(est-prep.b-num:SCREEN-VALUE ) NE 0 AND
            NOT CAN-FIND(FIRST b-eb OF est
            WHERE b-eb.form-no EQ INT(est-prep.s-num:SCREEN-VALUE )
            AND b-eb.blank-no EQ INT(est-prep.b-num:SCREEN-VALUE ))
            THEN 
        DO:
            MESSAGE "Invalid " + TRIM(STRING(est-prep.b-num:LABEL )) +
                ", please re-enter..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO est-prep.b-num.
            RETURN ERROR.
        END.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-code Dialog-Frame 
PROCEDURE valid-code :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-count AS INTEGER NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST prep
            WHERE prep.company EQ est.company
            AND prep.code    EQ est-prep.code:SCREEN-VALUE )
            THEN 
        DO:
            MESSAGE "Code not found - Reenter valid code" VIEW-AS ALERT-BOX.
            APPLY "entry" TO est-prep.code.
            RETURN ERROR.
        END.
        IF ip-count EQ 1 THEN 
        DO:
            IF NOT CAN-FIND(FIRST prep
                WHERE prep.company EQ est.company
                AND prep.loc     EQ est.loc
                AND prep.code    EQ est-prep.code:SCREEN-VALUE )
                THEN 
            DO:
                MESSAGE "Code is at a different location than the estimate" VIEW-AS ALERT-BOX WARNING.
                APPLY "entry" TO est-prep.code.
            END.
        END.
        /* validate # inks and coat for Plate*/
        IF CAN-FIND(FIRST prep WHERE prep.company EQ est.company
            AND prep.loc     EQ est.loc
            AND prep.code    EQ est-prep.code:SCREEN-VALUE 
            AND prep.mat-type = "P")
            THEN 
        DO:
            DEFINE BUFFER b-eb FOR eb.
            IF NOT CAN-FIND(FIRST b-eb OF est
                WHERE b-eb.form-no EQ INT(est-prep.s-num:SCREEN-VALUE )
                AND b-eb.blank-no EQ INT(est-prep.b-num:SCREEN-VALUE )
                AND b-eb.i-col + b-eb.i-coat > 0)
                THEN 
            DO:
                IF NOT CAN-FIND(FIRST b-eb OF est
                    WHERE b-eb.form-no EQ INT(est-prep.s-num:SCREEN-VALUE )
                    AND b-eb.i-col + b-eb.i-coat > 0)
                    THEN 
                DO:
                    MESSAGE "No Inks or Coats are defineded for this estimate's form.  "
                        " Enter Inks or Coats first before entering plates. "
                        VIEW-AS ALERT-BOX ERROR.
                    APPLY "entry" TO est-prep.CODE.                    
                    RETURN ERROR.
                END.                                 
            END.           
        END.    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-num Dialog-Frame 
PROCEDURE valid-s-num :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-ef FOR ef.


    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST b-ef OF est
            WHERE b-ef.form-no EQ INT(est-prep.s-num:SCREEN-VALUE ))
            THEN 
        DO:
            MESSAGE "Invalid " + TRIM(STRING(est-prep.s-num:LABEL )) +
                ", please re-enter..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO est-prep.s-num.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-simon Dialog-Frame 
PROCEDURE valid-simon :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
                              
    DO WITH FRAME {&FRAME-NAME}:
        IF INDEX("SIMON",est-prep.simon:SCREEN-VALUE ) LE 0
            THEN 
        DO:
            MESSAGE "Simon code must be 'S', 'I', 'M', 'O', or 'N'..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO est-prep.simon.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: rm\d-trans.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO .   /* add,update,view */
DEFINE OUTPUT PARAMETER op-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN 
    cocode = g_company.
ASSIGN 
    locode = g_loc.

DEFINE VARIABLE char-val                 AS cha           NO-UNDO.
DEFINE VARIABLE ext-cost                 AS DECIMAL       NO-UNDO.
DEFINE VARIABLE lv-recid                 AS RECID         NO-UNDO.
DEFINE VARIABLE ls-prev-po               AS cha           NO-UNDO.
DEFINE VARIABLE hd-post                  AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hd-post-child            AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE ll-help-run              AS LOG           NO-UNDO.  /* set on browse help, reset row-entry */
DEFINE VARIABLE lv-fgrecpt-val           AS INTEGER       NO-UNDO.
DEFINE VARIABLE trans-time               AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lv-new-tag-number-chosen AS LOG           NO-UNDO.


DEFINE VARIABLE lv-item-recid            AS RECID         NO-UNDO.
DEFINE VARIABLE ll-order-warned          AS LOGICAL       NO-UNDO.
DEFINE VARIABLE ll-new-record            AS LOGICAL       NO-UNDO.
DEFINE BUFFER bf-rell FOR oe-rell.
DEFINE BUFFER bf-ordl FOR oe-ordl.
DEFINE VARIABLE hInventoryProcs AS HANDLE  NO-UNDO.
DEFINE VARIABLE lActiveBin      AS LOGICAL NO-UNDO.
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
&Scoped-define INTERNAL-TABLES fg-rctd

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame fg-rctd.r-no fg-rctd.rct-date ~
fg-rctd.i-no fg-rctd.i-name fg-rctd.job-no fg-rctd.job-no2 fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.created-by fg-rctd.tag fg-rctd.cust-no ~
fg-rctd.cases fg-rctd.qty-case fg-rctd.partial fg-rctd.loc2 ~
fg-rctd.loc-bin2 fg-rctd.tag2 fg-rctd.updated-by 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame fg-rctd.rct-date ~
fg-rctd.i-no fg-rctd.i-name fg-rctd.job-no fg-rctd.job-no2 fg-rctd.loc ~
fg-rctd.loc-bin fg-rctd.tag fg-rctd.cust-no fg-rctd.cases fg-rctd.qty-case ~
fg-rctd.partial fg-rctd.loc2 fg-rctd.loc-bin2 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame fg-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame fg-rctd
&Scoped-define TABLES-IN-QUERY-Dialog-Frame fg-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame fg-rctd


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS fg-rctd.rct-date fg-rctd.i-no fg-rctd.i-name ~
fg-rctd.job-no fg-rctd.job-no2 fg-rctd.loc fg-rctd.loc-bin fg-rctd.tag ~
fg-rctd.cust-no fg-rctd.cases fg-rctd.qty-case fg-rctd.partial fg-rctd.loc2 ~
fg-rctd.loc-bin2 
&Scoped-define ENABLED-TABLES fg-rctd
&Scoped-define FIRST-ENABLED-TABLE fg-rctd
&Scoped-Define ENABLED-OBJECTS btnCalendar-1 Btn_OK Btn_Done Btn_Cancel ~
RECT-21 RECT-38 
&Scoped-Define DISPLAYED-FIELDS fg-rctd.r-no fg-rctd.rct-date fg-rctd.i-no ~
fg-rctd.i-name fg-rctd.job-no fg-rctd.job-no2 fg-rctd.loc fg-rctd.loc-bin ~
fg-rctd.created-by fg-rctd.tag fg-rctd.cust-no fg-rctd.cases ~
fg-rctd.qty-case fg-rctd.partial fg-rctd.loc2 fg-rctd.loc-bin2 fg-rctd.tag2 ~
fg-rctd.updated-by 
&Scoped-define DISPLAYED-TABLES fg-rctd
&Scoped-define FIRST-DISPLAYED-TABLE fg-rctd
&Scoped-Define DISPLAYED-OBJECTS fi_tr-time 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

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

DEFINE VARIABLE fi_tr-time AS CHARACTER FORMAT "X(15)":U 
     LABEL "Tr Time" 
     VIEW-AS FILL-IN 
     SIZE 17.2 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 19 BY 2.38
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 133.8 BY 11.71
     BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      fg-rctd SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     fg-rctd.r-no AT ROW 1.24 COL 29.8 COLON-ALIGNED
          LABEL "Seq#" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.rct-date AT ROW 2.43 COL 29.8 COLON-ALIGNED
          LABEL "Transfer Date" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 17.2 BY 1
          BGCOLOR 15 FONT 1
     btnCalendar-1 AT ROW 2.43 COL 49
     fi_tr-time AT ROW 3.62 COL 29.8 COLON-ALIGNED
     fg-rctd.i-no AT ROW 4.76 COL 29.8 COLON-ALIGNED
          LABEL "Item No" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 31.2 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.i-name AT ROW 5.91 COL 29.8 COLON-ALIGNED
          LABEL "Name/Desc" FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 31.2 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.job-no AT ROW 7.19 COL 29.8 COLON-ALIGNED
          LABEL "Job#" FORMAT "x(6)"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
    fg-rctd.job-no2 AT ROW 7.19 COL 49.2 COLON-ALIGNED NO-LABELS FORMAT "99"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.loc AT ROW 8.48 COL 29.8 COLON-ALIGNED
          LABEL "From Whs" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.loc-bin AT ROW 9.76 COL 29.8 COLON-ALIGNED
          LABEL "From Bin" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 18.6 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.created-by AT ROW 11.1 COL 29.8 COLON-ALIGNED
          LABEL "Created By" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 18.6 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.tag AT ROW 1.24 COL 85.4 COLON-ALIGNED
          LABEL "From Tag" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 28.6 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.cust-no AT ROW 2.43 COL 85.4 COLON-ALIGNED
          LABEL "Customer#" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.cases AT ROW 3.62 COL 85.4 COLON-ALIGNED
          LABEL "Units" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.qty-case AT ROW 4.76 COL 85.4 COLON-ALIGNED
          LABEL "Qty/Unit" FORMAT ">>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.partial AT ROW 5.91 COL 85.4 COLON-ALIGNED
          LABEL "Partial" FORMAT ">>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.loc2 AT ROW 7.19 COL 85.4 COLON-ALIGNED
          LABEL "To Whse" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.loc-bin2 AT ROW 8.48 COL 85.4 COLON-ALIGNED
          LABEL "To Bin" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.tag2 AT ROW 9.76 COL 85.4 COLON-ALIGNED
          LABEL "To Tag" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 28.6 BY 1
          BGCOLOR 15 FONT 1
     fg-rctd.updated-by AT ROW 11.1 COL 85.4 COLON-ALIGNED
          LABEL "Last Updated By" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FONT 1
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 13.24 COL 116
     Btn_Done AT ROW 13.52 COL 117
     Btn_Cancel AT ROW 13.24 COL 125
     RECT-21 AT ROW 13 COL 115
     RECT-38 AT ROW 1 COL 1
    SPACE(0.99) SKIP(3.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "Finished Goods Warehouse Transaction Transfer Update".


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

/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME Dialog-Frame
   3                                                                    */
/* SETTINGS FOR FILL-IN fg-rctd.cases IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.created-by IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN fg-rctd.cust-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fi_tr-time IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fg-rctd.i-name IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.i-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.job-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.job-no2 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.loc IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.loc-bin IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.loc-bin2 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.loc2 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.partial IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.qty-case IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.r-no IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN fg-rctd.rct-date IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.tag IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN fg-rctd.tag2 IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN fg-rctd.updated-by IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.fg-rctd "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.fg-rctd.company eq cocode "
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Warehouse Transaction Transfers Update */
DO:
        DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.

        IF NOT AVAILABLE fg-rctd THEN FIND fg-rctd WHERE RECID(fg-rctd) = lv-recid NO-LOCK NO-ERROR. 
 
        DEFINE VARIABLE ll-tag# AS LOG NO-UNDO.
        ll-help-run = YES.
        CASE FOCUS:NAME :
            WHEN "i-no" THEN 
                DO:
                    RUN windows/l-itemfg.w (fg-rctd.company, "", FOCUS:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO :
                        ASSIGN 
                            FOCUS:SCREEN-VALUE          = ENTRY(1,char-val)
                            fg-rctd.i-name:screen-value = ENTRY(2,char-val)
                            .
                    END.
                    RETURN NO-APPLY.   
                END.

            WHEN "job-no" THEN 
                DO:
                    RUN fgbin-help.
                END.

            WHEN "job-no2" THEN 
                DO:
                    RUN fgbin-help.
                END.

            WHEN "loc" THEN 
                DO:
                    RUN fgbin-help.
                END.

            WHEN "loc-bin" THEN 
                DO:
                    RUN fgbin-help.
                END.

            WHEN "tag" THEN 
                DO:
                    RUN fgbin-help.
                END.

            WHEN "cust-no" THEN 
                DO:
                    RUN fgbin-help.
                END.

            WHEN "loc2" THEN 
                DO:
                    RUN windows/l-loc.w (fg-rctd.company,FOCUS:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO :
                        ASSIGN 
                            FOCUS:SCREEN-VALUE = ENTRY(1,char-val).             
                    END.
                    RETURN NO-APPLY.   
                END.
            WHEN "loc-bin2" THEN 
                DO:
                    RUN windows/l-fgbin.w (fg-rctd.company,fg-rctd.loc2:screen-value,FOCUS:SCREEN-VALUE, OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO :
                        ASSIGN 
                            FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
                    END.
                    RETURN NO-APPLY.   
                END.

        END CASE.



    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Warehouse Transaction Transfers Update */
ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Warehouse Transaction Transfers Update */
DO:
        DISABLE TRIGGERS FOR LOAD OF fg-rctd .
    
        IF AVAILABLE fg-rctd THEN
            op-rowid = ROWID(fg-rctd) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST fg-rctd EXCLUSIVE-LOCK
                WHERE RECID(fg-rctd) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE fg-rctd THEN DELETE fg-rctd .
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
        DISABLE TRIGGERS FOR LOAD OF fg-rctd .
    
        IF AVAILABLE fg-rctd THEN
            op-rowid = ROWID(fg-rctd) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST fg-rctd EXCLUSIVE-LOCK
                WHERE RECID(fg-rctd) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE fg-rctd THEN DELETE fg-rctd .
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
        IF AVAILABLE fg-rctd THEN
            ASSIGN op-rowid = ROWID(fg-rctd) .
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
        DEFINE VARIABLE lv-tag2 LIKE fg-rctd.tag2 NO-UNDO.
        DEFINE VARIABLE ld      AS DECIMAL NO-UNDO.
        DEFINE VARIABLE lValidateResult AS LOGICAL NO-UNDO.

        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
   
        RUN valid-i-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-job-loc-bin-tag NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-loc2 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-loc-bin2 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-tag NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        
        fg-rctd.tag2:SCREEN-VALUE = fg-rctd.tag:SCREEN-VALUE.
        RUN valid-tag2 (OUTPUT lValidateResult) NO-ERROR.
        IF NOT lValidateResult THEN 
            RETURN NO-APPLY.
  
        DO TRANSACTION:
            lv-tag2 = fg-rctd.tag2:SCREEN-VALUE .
            FIND CURRENT fg-rctd EXCLUSIVE-LOCK NO-ERROR.

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
            END.
        END.

        ASSIGN
            fg-rctd.t-qty      = (fg-rctd.cases * fg-rctd.qty-case) + fg-rctd.partial
            fg-rctd.tag2       = lv-tag2
            fg-rctd.trans-time = TIME
            .

        FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ fg-rctd.i-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE itemfg THEN
            ASSIGN
                fg-rctd.pur-uom  = itemfg.prod-uom
                fg-rctd.cost-uom = itemfg.prod-uom
                fg-rctd.std-cost = itemfg.std-tot-cost.

        FIND FIRST fg-bin 
            WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ fg-rctd.i-no
            AND fg-bin.job-no  EQ fg-rctd.job-no
            AND fg-bin.job-no2 EQ fg-rctd.job-no2
            AND fg-bin.loc     EQ fg-rctd.loc
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin
            AND fg-bin.tag     EQ fg-rctd.tag
            AND fg-bin.cust-no EQ fg-rctd.cust-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE fg-bin THEN
            ASSIGN
                fg-rctd.pur-uom      = fg-bin.pur-uom
                fg-rctd.cost-uom     = fg-bin.pur-uom
                fg-rctd.std-cost     = fg-bin.std-tot-cost
                fg-rctd.units-pallet = fg-bin.units-pallet
                fg-rctd.cases-unit   = fg-bin.cases-unit
                fg-rctd.tot-wt       = fg-bin.tot-wt .

        ld = fg-rctd.std-cost.

        IF fg-rctd.pur-uom NE "EA" THEN
            RUN sys/ref/convcuom.p(fg-rctd.pur-uom, "EA", 0, 0, 0, 0,
                ld, OUTPUT ld).

        fg-rctd.ext-cost = fg-rctd.t-qty * ld.

        IF AVAILABLE fg-bin                AND
            fg-bin.qty GT fg-rctd.t-qty AND
            fg-bin.tag NE ""            AND
            fg-bin.tag EQ fg-rctd.tag2  AND lv-new-tag-number-chosen THEN      
            RUN fg/mkloadtg.p (ROWID(fg-rctd), 0 /*fg-rctd.cases*/, INPUT-OUTPUT fg-rctd.tag2).
  
        lv-new-tag-number-chosen = ?.


        FIND CURRENT fg-rctd NO-LOCK NO-ERROR .
        op-rowid = ROWID(fg-rctd).

        APPLY "go" TO FRAME {&FRAME-NAME}.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cases Dialog-Frame
ON LEAVE OF fg-rctd.cases IN FRAME Dialog-Frame /* Units */
DO:
        DEFINE BUFFER b-loadtag FOR loadtag.

        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-job-loc-bin-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            FIND FIRST b-loadtag WHERE
                b-loadtag.company = cocode AND
                b-loadtag.item-type = NO AND
                b-loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE 
                NO-LOCK NO-ERROR.

            IF AVAILABLE b-loadtag THEN
            DO:
            /* Task 10081201 - This is a transfer, so units may be less */        
            /*         IF INT(fg-rctd.cases:SCREEN-VALUE ) < b-loadtag.tot-cases THEN */
            /*         DO:                                                                                    */
            /*            MESSAGE "Units is Less Than Loadtag O/H Cases."                                     */
            /*                VIEW-AS ALERT-BOX ERROR BUTTONS OK.                                             */
            /*            RETURN NO-APPLY.                                                                    */
            /*         END.                                                                                   */

            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.cust-no Dialog-Frame
ON LEAVE OF fg-rctd.cust-no IN FRAME Dialog-Frame /* Customer# */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-job-loc-bin-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-name Dialog-Frame
ON ENTRY OF fg-rctd.i-name IN FRAME Dialog-Frame /* Name/Desc */
DO:
        APPLY "tab" TO {&self-name} .
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Dialog-Frame
ON LEAVE OF fg-rctd.i-no IN FRAME Dialog-Frame /* Item No */
DO:
        IF LASTKEY NE -1 THEN 
        DO:      
            lv-new-tag-number-chosen = ?.

            RUN valid-i-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.i-no Dialog-Frame
ON VALUE-CHANGED OF fg-rctd.i-no IN FRAME Dialog-Frame /* Item No */
DO:
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    BEGINS {&self-name}:SCREEN-VALUE 
       NO-ERROR.
        IF AVAILABLE itemfg THEN
            fg-rctd.i-name:SCREEN-VALUE  = itemfg.i-name.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no Dialog-Frame
ON LEAVE OF fg-rctd.job-no IN FRAME Dialog-Frame /* Job# */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-job-loc-bin-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.job-no2 Dialog-Frame
ON LEAVE OF fg-rctd.job-no2 IN FRAME Dialog-Frame
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-job-loc-bin-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc Dialog-Frame
ON LEAVE OF fg-rctd.loc IN FRAME Dialog-Frame /* From!Whs */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-job-loc-bin-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin Dialog-Frame
ON LEAVE OF fg-rctd.loc-bin IN FRAME Dialog-Frame /* From!Bin */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-job-loc-bin-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc-bin2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc-bin2 Dialog-Frame
ON LEAVE OF fg-rctd.loc-bin2 IN FRAME Dialog-Frame /* To!Bin */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-loc-bin2 NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.loc2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.loc2 Dialog-Frame
ON LEAVE OF fg-rctd.loc2 IN FRAME Dialog-Frame /* To!Whse */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            IF fg-rctd.loc2:SCREEN-VALUE  EQ ""  THEN
                fg-rctd.loc2:SCREEN-VALUE  = fg-rctd.loc2:SCREEN-VALUE  .
            RUN valid-loc2 NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.partial Dialog-Frame
ON LEAVE OF fg-rctd.partial IN FRAME Dialog-Frame /* Partial */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-job-loc-bin-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.qty-case Dialog-Frame
ON ENTRY OF fg-rctd.qty-case IN FRAME Dialog-Frame /* Qty/Unit */
DO:
        APPLY "tab" TO {&self-name} .
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Dialog-Frame
ON LEAVE OF fg-rctd.tag IN FRAME Dialog-Frame /* From!Tag */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            RUN valid-job-loc-bin-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            fg-rctd.tag2:SCREEN-VALUE = fg-rctd.tag:SCREEN-VALUE.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag Dialog-Frame
ON VALUE-CHANGED OF fg-rctd.tag IN FRAME Dialog-Frame /* From!Tag */
DO:
        RUN new-tag.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rctd.tag2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rctd.tag2 Dialog-Frame
ON ENTRY OF fg-rctd.tag2 IN FRAME Dialog-Frame /* To!Tag */
DO:
        APPLY "tab" TO {&self-name} .
        RETURN NO-APPLY.
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
        
    RUN Inventory/InventoryProcs.p PERSISTENT SET hInventoryProcs.
    
    IF ip-type EQ "copy" THEN lv-item-recid = ip-recid.

    IF ip-recid EQ ? THEN 
    DO:
        RUN create-item.
    END.
    ELSE FIND fg-rctd NO-LOCK WHERE RECID(fg-rctd) EQ ip-recid NO-ERROR.

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
    DEFINE VARIABLE lv-rno LIKE fg-rctd.r-no NO-UNDO.
    DEFINE BUFFER b-fg-rctd FOR fg-rctd.

    DO WITH FRAME {&FRAME-NAME}:
        lv-rno = 0.
        FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAILABLE b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

        FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.
        
        CREATE fg-rctd.

        DO WHILE TRUE:
            lv-rno = lv-rno + 1.
            FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
            IF AVAILABLE fg-rcpth THEN NEXT.
            FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
            IF AVAILABLE b-fg-rctd THEN NEXT.
            LEAVE.
        END.

        
        ASSIGN 
            fg-rctd.company   = cocode
            fg-rctd.r-no      = lv-rno
            fg-rctd.rita-code = "T".


        ASSIGN
            fg-rctd.rct-date     = TODAY
            fg-rctd.trans-time   = TIME
            fg-rctd.s-num        = 0
            fg-rctd.units-pallet = 1
            fg-rctd.cases-unit   = 1.

        DISPLAY fg-rctd.rct-date . 
        ASSIGN 
            lv-item-recid = RECID(fg-rctd).
        ll-new-record = YES.

        FIND CURRENT fg-rctd NO-LOCK NO-ERROR.
    END. /* avail oe-relh */ 

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
    IF AVAILABLE fg-rctd  THEN 
    DO:
        ASSIGN 
            fi_tr-time = STRING(fg-rctd.trans-time,'HH:MM') .

        DISPLAY  fg-rctd.r-no fg-rctd.rct-date 
            fg-rctd.i-no fg-rctd.i-name 
            fg-rctd.job-no fg-rctd.job-no2 fg-rctd.loc fg-rctd.loc-bin fg-rctd.tag 
            fg-rctd.cust-no fg-rctd.cases fg-rctd.qty-case fg-rctd.partial fg-rctd.loc2 
            fg-rctd.loc-bin2 fg-rctd.tag2 fg-rctd.created-by fg-rctd.updated-by  fi_tr-time
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
  DISPLAY fi_tr-time 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE fg-rctd THEN 
    DISPLAY fg-rctd.r-no fg-rctd.rct-date fg-rctd.i-no fg-rctd.i-name 
          fg-rctd.job-no fg-rctd.job-no2 fg-rctd.loc fg-rctd.loc-bin 
          fg-rctd.created-by fg-rctd.tag fg-rctd.cust-no fg-rctd.cases 
          fg-rctd.qty-case fg-rctd.partial fg-rctd.loc2 fg-rctd.loc-bin2 
          fg-rctd.tag2 fg-rctd.updated-by 
      WITH FRAME Dialog-Frame.
  ENABLE fg-rctd.rct-date btnCalendar-1 fg-rctd.i-no fg-rctd.i-name 
         fg-rctd.job-no fg-rctd.job-no2 fg-rctd.loc fg-rctd.loc-bin fg-rctd.tag 
         fg-rctd.cust-no fg-rctd.cases fg-rctd.qty-case fg-rctd.partial 
         fg-rctd.loc2 fg-rctd.loc-bin2 Btn_OK Btn_Done Btn_Cancel RECT-21 
         RECT-38 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgbin-help Dialog-Frame 
PROCEDURE fgbin-help :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        fg-rctd.job-no:SCREEN-VALUE  =
            FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE ))) +
            TRIM(fg-rctd.job-no:SCREEN-VALUE ).

        RUN windows/l-fgibn4.w (fg-rctd.company, fg-rctd.i-no:screen-value , fg-rctd.job-no:screen-value , INT(fg-rctd.job-no2:screen-value ), fg-rctd.loc:screen-value , fg-rctd.loc-bin:screen-value , fg-rctd.tag:screen-value , OUTPUT lv-rowid).

        FIND fg-bin WHERE ROWID(fg-bin) EQ lv-rowid NO-LOCK NO-ERROR.

        IF AVAILABLE fg-bin AND (fg-rctd.job-no:SCREEN-VALUE        NE fg-bin.job-no  OR
            INT(fg-rctd.job-no2:SCREEN-VALUE ) NE fg-bin.job-no2 OR
            fg-rctd.loc:SCREEN-VALUE           NE fg-bin.loc     OR
            fg-rctd.loc-bin:SCREEN-VALUE       NE fg-bin.loc-bin OR
            fg-rctd.tag:SCREEN-VALUE           NE fg-bin.tag     OR
            fg-rctd.cust-no:SCREEN-VALUE       NE fg-bin.cust-no)
            THEN 
        DO:
            ASSIGN
                fg-rctd.job-no:SCREEN-VALUE  = fg-bin.job-no
                fg-rctd.job-no2:SCREEN-VALUE = STRING(fg-bin.job-no2)
                fg-rctd.loc:SCREEN-VALUE     = fg-bin.loc
                fg-rctd.loc-bin:SCREEN-VALUE = fg-bin.loc-bin
                fg-rctd.tag:SCREEN-VALUE     = fg-bin.tag
                fg-rctd.cust-no:SCREEN-VALUE = fg-bin.cust-no.
       
            IF fg-bin.loc  = "MAIN" THEN
                IF fg-rctd.loc2:SCREEN-VALUE  EQ ""  THEN
                    fg-rctd.loc2:SCREEN-VALUE       = CAPS(fg-bin.loc) .
      
            RUN new-bin.
        END.
    END.

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
    DELETE OBJECT hInventoryProcs.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-bin Dialog-Frame 
PROCEDURE new-bin :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        fg-rctd.job-no:SCREEN-VALUE  =
            FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no:SCREEN-VALUE ))) +
            TRIM(fg-rctd.job-no:SCREEN-VALUE ).

        FIND FIRST fg-bin 
            WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ fg-rctd.i-no:SCREEN-VALUE 
            AND fg-bin.job-no  EQ fg-rctd.job-no:SCREEN-VALUE 
            AND fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE )
            AND fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE 
            AND fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE 
            AND fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE 
            AND fg-bin.cust-no EQ fg-rctd.cust-no:SCREEN-VALUE 
            NO-LOCK NO-ERROR.
    
        IF AVAILABLE fg-bin THEN
            ASSIGN
                fg-rctd.qty-case:SCREEN-VALUE = STRING(fg-bin.case-count)
                fg-rctd.job-no:SCREEN-VALUE   = fg-bin.job-no
                fg-rctd.job-no2:SCREEN-VALUE  = STRING(fg-bin.job-no2)
                fg-rctd.loc:SCREEN-VALUE      = CAPS(fg-bin.loc)
                fg-rctd.loc-bin:SCREEN-VALUE  = CAPS(fg-bin.loc-bin)
                fg-rctd.tag:SCREEN-VALUE      = CAPS(fg-bin.tag)
                fg-rctd.cust-no:SCREEN-VALUE  = CAPS(fg-bin.cust-no).
        IF AVAILABLE fg-bin THEN
            fg-rctd.cases:SCREEN-VALUE  = STRING( TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)).

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-tag Dialog-Frame 
PROCEDURE new-tag :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DO WITH FRAME {&FRAME-NAME}:

        IF fg-rctd.tag:SCREEN-VALUE  NE "" THEN 
        DO:
            FIND FIRST fg-bin
                WHERE fg-bin.company  EQ cocode
                AND fg-bin.tag      EQ trim(fg-rctd.tag:SCREEN-VALUE )
                AND fg-bin.i-no     EQ trim(fg-rctd.i-no:SCREEN-VALUE )
                USE-INDEX tag NO-LOCK NO-ERROR.
        
            IF AVAILABLE fg-bin THEN 
            DO:
                ASSIGN
                    fg-rctd.job-no:SCREEN-VALUE  = fg-bin.job-no
                    fg-rctd.job-no2:SCREEN-VALUE = STRING(fg-bin.job-no2)
                    fg-rctd.loc:SCREEN-VALUE     = fg-bin.loc
                    fg-rctd.loc-bin:SCREEN-VALUE = fg-bin.loc-bin
                    fg-rctd.tag:SCREEN-VALUE     = fg-bin.tag
                    fg-rctd.cust-no:SCREEN-VALUE = fg-bin.cust-no .
        
                IF fg-bin.loc  = "MAIN" THEN
                    IF fg-rctd.loc2:SCREEN-VALUE  EQ ""  THEN
                        fg-rctd.loc2:SCREEN-VALUE       = CAPS(fg-bin.loc) .

                RUN new-bin.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no Dialog-Frame 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ fg-rctd.i-no:SCREEN-VALUE 
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE itemfg THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
            RETURN ERROR.
        END.
  
        IF AVAILABLE itemfg AND itemfg.pur-uom EQ "" THEN 
        DO:
            MESSAGE "The finished goods item must have a valid Puchase Quantity UOM..." VIEW-AS ALERT-BOX.
            RETURN ERROR.     
        END.    
    
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-loc-bin-tag Dialog-Frame 
PROCEDURE valid-job-loc-bin-tag :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-fields AS CHARACTER INIT "job-no,job-no2,loc,loc-bin,tag,cust-no" NO-UNDO.
    DEFINE VARIABLE li-field# AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li-fieldc AS CHARACTER NO-UNDO.
  

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            li-fieldc                   = TRIM(fg-rctd.job-no:SCREEN-VALUE )
            li-fieldc                   = FILL(" ",6 - LENGTH(li-fieldc)) + li-fieldc
            fg-rctd.job-no:SCREEN-VALUE = li-fieldc

            li-field#                   = LOOKUP(FOCUS:NAME ,lv-fields).

        IF li-field# EQ 0 THEN li-field# = 9999.

        IF li-field# LT 3                                          AND
            fg-rctd.loc:SCREEN-VALUE  NE "" THEN li-field# = 3.

        IF li-field# LT 4                                              AND
            fg-rctd.loc-bin:SCREEN-VALUE  NE "" THEN li-field# = 4.
        
        IF li-field# LT 5                                          AND
            fg-rctd.tag:SCREEN-VALUE  NE "" THEN li-field# = 5.
        
        IF li-field# LT 6                                              AND
            fg-rctd.cust-no:SCREEN-VALUE  NE "" THEN li-field# = 6.

        FOR EACH fg-bin NO-LOCK
            WHERE fg-bin.company  EQ cocode
            AND fg-bin.active   EQ TRUE
            AND fg-bin.i-no     EQ fg-rctd.i-no:SCREEN-VALUE 
            AND fg-bin.job-no   EQ fg-rctd.job-no:SCREEN-VALUE 
            AND (fg-bin.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE ) OR
            li-field#      LT 2)
            AND (fg-bin.loc     EQ fg-rctd.loc:SCREEN-VALUE           OR
            li-field#      LT 3)
            AND (fg-bin.loc-bin EQ fg-rctd.loc-bin:SCREEN-VALUE       OR
            li-field#      LT 4)
            AND (fg-bin.tag     EQ fg-rctd.tag:SCREEN-VALUE           OR
            li-field#      LT 5)
            AND (fg-bin.cust-no EQ fg-rctd.cust-no:SCREEN-VALUE           OR
            li-field#      LT 6)
            USE-INDEX co-ino
            BY fg-bin.qty DESCENDING:
            LEAVE.
        END.

        IF AVAILABLE fg-bin AND
            fg-bin.qty GE (DEC(fg-rctd.cases:SCREEN-VALUE ) *
            DEC(fg-rctd.qty-case:SCREEN-VALUE )) +
            DEC(fg-rctd.partial:SCREEN-VALUE )
            THEN 
        DO:
            ASSIGN
                fg-rctd.qty-case:SCREEN-VALUE = STRING(fg-bin.case-count)
                fg-rctd.loc:SCREEN-VALUE      = CAPS(fg-bin.loc)
                fg-rctd.loc-bin:SCREEN-VALUE  = CAPS(fg-bin.loc-bin)
                fg-rctd.tag:SCREEN-VALUE      = CAPS(fg-bin.tag)
                fg-rctd.cust-no:SCREEN-VALUE  = CAPS(fg-bin.cust-no)
                fg-rctd.tag2:SCREEN-VALUE     = IF adm-new-record THEN CAPS(fg-bin.tag) ELSE CAPS(fg-rctd.tag2:SCREEN-VALUE )
                fg-rctd.cust-no:SCREEN-VALUE  = CAPS(fg-bin.cust-no).
       
            IF fg-bin.loc  = "MAIN" THEN 
                IF fg-rctd.loc2:SCREEN-VALUE  EQ ""  THEN
                    fg-rctd.loc2:SCREEN-VALUE       = CAPS(fg-bin.loc) .
       
            IF fg-rctd.tag:SCREEN-VALUE  NE "" AND lv-new-tag-number-chosen = ? 
                AND fg-rctd.tag:SCREEN-VALUE  EQ fg-rctd.tag2:SCREEN-VALUE   THEN 
            DO:
                IF fg-rctd.cases:SCREEN-VALUE  NE  STRING(TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count, 0)) 
                    AND int(fg-rctd.cases:SCREEN-VALUE ) NE 0 THEN 
                DO:
                    MESSAGE "Units not matching units in Tag will need to create a new tag #"
                        SKIP(1)
                        "Click OK to continue and create a new tag number on save or cancel"
                        VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL
                        TITLE "" UPDATE choice AS LOGICAL.
                    IF choice THEN
                        lv-new-tag-number-chosen = TRUE.
                    ELSE
                        lv-new-tag-number-chosen = FALSE.
                    IF NOT lv-new-tag-number-chosen THEN
                        fg-rctd.cases:SCREEN-VALUE     = STRING(TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count, 0)).
                END. /* ask if creating a new tag number */

            END.

        END.
        ELSE 
        DO:
            IF AVAILABLE fg-bin THEN 
            DO:
                MESSAGE "Insufficient qty in bin..." VIEW-AS ALERT-BOX.
                APPLY "entry" TO fg-rctd.cases .
            END.

            ELSE 
            DO:
                IF FOCUS:LABEL EQ ? THEN
                    MESSAGE "Job2 not valid in bin ..." VIEW-AS ALERT-BOX.
                ELSE
                    MESSAGE FOCUS:LABEL + " not valid in bin ..." VIEW-AS ALERT-BOX.
        
                APPLY "entry" TO FOCUS .
        
            END.

            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin2 Dialog-Frame 
PROCEDURE valid-loc-bin2 :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF lv-msg EQ "" THEN
            IF fg-rctd.loc-bin2:SCREEN-VALUE  EQ "" THEN
                lv-msg = "To Bin may not be spaces".

        IF lv-msg EQ "" THEN
            IF fg-rctd.loc:SCREEN-VALUE       EQ
                fg-rctd.loc2:SCREEN-VALUE      AND
                fg-rctd.loc-bin:SCREEN-VALUE   EQ
                fg-rctd.loc-bin2:SCREEN-VALUE  THEN
                lv-msg = "To Whse/Bin may not be the same as From Whse/Bin".

        IF lv-msg EQ "" THEN 
        DO:
            RUN ValidateBin IN hInventoryProcs (cocode, 
                fg-rctd.loc2:SCREEN-VALUE,
                fg-rctd.loc-bin2:SCREEN-VALUE, 
                OUTPUT lActiveBin ).
            IF NOT lActiveBin THEN lv-msg = "Invalid entry, try help...".
        END.

        IF lv-msg NE "" THEN 
        DO:
            MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO fg-rctd.loc-bin2 .
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc2 Dialog-Frame 
PROCEDURE valid-loc2 :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF lv-msg EQ "" THEN
            IF fg-rctd.loc2:SCREEN-VALUE  EQ "" THEN
                lv-msg = "To Bin may not be spaces".

        IF lv-msg EQ "" THEN 
        DO:
            RUN ValidateLoc IN hInventoryProcs (cocode, fg-rctd.loc2:SCREEN-VALUE, OUTPUT lActiveBin).
            IF NOT lActiveBin THEN 
                 lv-msg = "Invalid entry, try help".
        END.

        IF lv-msg NE "" THEN 
        DO:
            MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO fg-rctd.loc2 .
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty Dialog-Frame 
PROCEDURE valid-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
/* (fg-rctd.cases * fg-rctd.qty-case) + fg-rctd.partial. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag Dialog-Frame 
PROCEDURE valid-tag :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-fg-rctd FOR fg-rctd.

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST bf-fg-rctd NO-LOCK
            WHERE bf-fg-rctd.company = cocode 
            AND bf-fg-rctd.rita-code = "T" 
            AND bf-fg-rctd.tag = fg-rctd.tag:SCREEN-VALUE 
            AND RECID(bf-fg-rctd) <> RECID(fg-rctd)  NO-ERROR.
        IF AVAILABLE bf-fg-rctd AND fg-rctd.tag:SCREEN-VALUE NE "" THEN 
        DO:
            MESSAGE "This Tag number has already been used." SKIP
                "Please enter a unique Tag number." 
                VIEW-AS ALERT-BOX INFO.
                APPLY "entry" TO fg-rctd.tag .
                RETURN ERROR.
        END.
    END.


    IF lv-fgrecpt-val = 1 THEN 
    DO:
        FIND FIRST loadtag WHERE loadtag.company = cocode
            AND loadtag.item-type = NO
            AND loadtag.tag-no = fg-rctd.tag:SCREEN-VALUE  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE loadtag /*or fg-rctd.tag:SCREEN-VALUE  = ""*/ THEN 
        DO:
            MESSAGE "Invalid Tag#. Try help or Scan valid tag#..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fg-rctd.tag .
            RETURN ERROR.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag2 Dialog-Frame 
PROCEDURE valid-tag2 :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplValidTag2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE lValidTag2 AS LOGICAL NO-UNDO.

    lValidTag2 = TRUE.
    DO WITH FRAME {&FRAME-NAME}:
        IF fg-rctd.tag:SCREEN-VALUE GT "" 
           AND fg-rctd.tag2:SCREEN-VALUE EQ "" THEN         
        DO:
            MESSAGE 'The "To" tag number is blank.  Please re-enter the "From" tag number' SKIP                
                VIEW-AS ALERT-BOX ERROR.
            APPLY 'entry' TO fg-rctd.tag .
            lValidTag2 = FALSE.            
        END.
           
        oplValidTag2 = lValidTag2.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



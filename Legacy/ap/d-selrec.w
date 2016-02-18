&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: ap\d-selrec.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEF INPUT PARAM ip-ap-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-po-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-qty-inv LIKE ap-invl.qty NO-UNDO.

/* Local Variable Definitions --- */
DEF VAR li AS INT NO-UNDO.

DEF TEMP-TABLE tt-rec FIELD selekt AS LOG FORMAT "yes/no" LABEL "Selected"
                      FIELD rec-id AS RECID
                      FIELD trans-date LIKE fg-rcpth.trans-date LABEL "Receipt Date"
                      FIELD r-no AS INT
                      FIELD qty-rec AS DEC FORM "->>>,>>>,>>9.9<<"  LABEL "Qty Received"
                      FIELD qty-inv AS DEC FORM "->>>,>>>,>>9.9<<"  LABEL "Qty Invoiced"
                      .

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DO TRANSACTION:
  {sys/inc/appaper.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-rec

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 tt-rec.trans-date tt-rec.qty-rec tt-rec.selekt tt-rec.qty-inv   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 tt-rec.selekt tt-rec.qty-inv   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 tt-rec
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 tt-rec
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH tt-rec BY tt-rec.trans-date BY tt-rec.r-no
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH tt-rec BY tt-rec.trans-date BY tt-rec.r-no.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 tt-rec
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 tt-rec


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 Btn_OK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      tt-rec SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 Dialog-Frame _FREEFORM
  QUERY BROWSE-2 DISPLAY
      tt-rec.trans-date tt-rec.qty-rec tt-rec.selekt tt-rec.qty-inv
     ENABLE tt-rec.selekt tt-rec.qty-inv
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 51 BY 12.38
         BGCOLOR 8  EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-2 AT ROW 1.24 COL 2
     Btn_OK AT ROW 6.71 COL 54
     SPACE(1.99) SKIP(5.95)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Select Receipts for PO/Item:".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB BROWSE-2 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-rec BY tt-rec.trans-date BY tt-rec.r-no.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Select Receipts for PO/Item: */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  op-qty-inv = 0.

  IF li EQ 0 THEN
    MESSAGE "This will select all " +
            STRING(po-ordl.item-type,"RM/FG") + " Receipts marked YES. " SKIP
            "Do you wish to continue?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.

  ELSE ll-ans = YES.

  IF ll-ans THEN
    IF po-ordl.item-type THEN
    FOR EACH tt-rec:
      IF tt-rec.rec-id EQ ? THEN op-qty-inv = tt-rec.qty-inv.

      ELSE
      FOR FIRST rm-rcpth WHERE RECID(rm-rcpth) EQ tt-rec.rec-id NO-LOCK,
          EACH rm-rdtlh
            WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
              AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code:
          
        IF tt-rec.selekt THEN
          ASSIGN
           rm-rdtlh.receiver-no = (STRING(ap-inv.i-no,"9999999999") +
                                   STRING(tt-rec.qty-inv,"-9999999999.99999"))
           op-qty-inv = op-qty-inv + tt-rec.qty-inv.

        ELSE rm-rdtlh.receiver-no = "".
      END.
    END.

    ELSE
    FOR EACH tt-rec,
        FIRST fg-rcpth WHERE RECID(fg-rcpth) EQ tt-rec.rec-id NO-LOCK,
        EACH fg-rdtlh
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
          
      IF tt-rec.selekt THEN
        ASSIGN
         fg-rdtlh.receiver-no = (STRING(ap-inv.i-no,"9999999999") +
                                 STRING(tt-rec.qty-inv,"-9999999999.99999"))
         op-qty-inv = op-qty-inv + tt-rec.qty-inv.

      ELSE fg-rdtlh.receiver-no = "".
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


ON 'mouse-select-click':U OF tt-rec.selekt OR
   'return':U OF tt-rec.selekt
DO:
  tt-rec.selekt:SCREEN-VALUE IN BROWSE {&browse-name} =
      STRING(NOT (tt-rec.selekt:SCREEN-VALUE IN BROWSE {&browse-name} EQ "yes"),"yes/no").
END.

ON 'leave':U OF tt-rec.selekt IN BROWSE {&browse-name}
DO:
    ASSIGN tt-rec.selekt = tt-rec.selekt:SCREEN-VALUE IN BROWSE {&browse-name} = "Yes".
    RETURN.
END.

/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  li = 0.
/*
  FIND ap-invl WHERE RECID(ap-invl) EQ ip-ap-recid NO-LOCK NO-ERROR.

  FIND FIRST ap-inv WHERE ap-inv.i-no EQ ap-invl.i-no NO-LOCK NO-ERROR.

  FIND FIRST po-ordl
      WHERE po-ordl.company   EQ cocode
        AND po-ordl.po-no     EQ ap-invl.po-no
        AND po-ordl.line      EQ {ap/invlline.i -1}
      NO-LOCK NO-ERROR.
*/
  FIND FIRST ap-inv WHERE ROWID(ap-inv) EQ ip-ap-rowid NO-LOCK NO-ERROR.

  FIND FIRST po-ordl WHERE ROWID(po-ordl) EQ ip-po-rowid NO-LOCK NO-ERROR.

  IF AVAIL ap-inv AND AVAIL po-ordl THEN DO:
    FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + " " +
                                TRIM(STRING(po-ordl.po-no,">>>>>>>>")) + "/" +
                                TRIM(po-ordl.i-no).

    FIND FIRST po-ord WHERE
         po-ord.company EQ po-ordl.company AND
         po-ord.po-no   EQ po-ordl.po-no
         NO-LOCK NO-ERROR.

    RUN build-table.
  END.

  IF li GT 1 THEN DO WITH FRAME {&FRAME-NAME}:
    li = 0.

    RUN enable_UI.
  
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  END.
  ELSE IF li > 0 THEN  DO:
      FIND FIRST tt-rec NO-ERROR.
      op-qty-inv = IF AVAIL tt-rec THEN  tt-rec.qty-inv ELSE 0.
  END.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table Dialog-Frame 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld AS DEC EXTENT 2 NO-UNDO.
  DEF VAR v-wid AS DEC NO-UNDO.
  DEF VAR v-len AS DEC NO-UNDO.
  DEF VAR v-dep AS DEC NO-UNDO.
  DEF VAR v-bwt AS DEC NO-UNDO.
  DEF VAR v-qty AS DEC NO-UNDO.
  DEF VAR lv-uom LIKE po-ordl.pr-qty-uom NO-UNDO.

  DEF BUFFER b-ap-invl FOR ap-invl.


  FOR EACH tt-rec:
    DELETE tt-rec.
  END.

  IF po-ordl.item-type THEN DO:
    ASSIGN
     v-bwt = 0
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     v-dep = 0.

    FIND FIRST item
        WHERE item.company EQ cocode
          AND item.i-no    EQ po-ordl.i-no
        NO-LOCK NO-ERROR.

    IF AVAIL item THEN DO:
      v-dep = item.s-dep.          
      {po/pol-dims.i}
    END. 

    IF AVAIL item           AND
       item.i-code EQ "R"   AND
       INDEX("MOXY789@",ITEM.mat-type) GT 0 AND
       item.stocked EQ NO   THEN DO:

      v-qty = po-ordl.cons-qty.

      IF po-ordl.cons-uom NE po-ordl.pr-qty-uom THEN
        RUN sys/ref/convquom.p (po-ordl.cons-uom, po-ordl.pr-qty-uom,
                                v-bwt, v-len, v-wid, v-dep,
                                v-qty, OUTPUT v-qty).
                                   
      v-qty = v-qty - po-ordl.t-inv-qty.

      IF v-qty GT 0 THEN DO:
        CREATE tt-rec.
        ASSIGN
         tt-rec.rec-id     = ?
         tt-rec.selekt     = NO
         tt-rec.trans-date = po-ord.po-date
         tt-rec.r-no       = 0
         tt-rec.qty-rec    = v-qty
         tt-rec.qty-inv    = v-qty.
      END.
    END.
  
    ELSE
    FOR EACH rm-rcpth
        WHERE rm-rcpth.company   EQ cocode
          AND rm-rcpth.i-no      EQ po-ordl.i-no
          AND rm-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
          AND rm-rcpth.job-no    EQ po-ordl.job-no
          AND rm-rcpth.job-no2   EQ po-ordl.job-no2
          AND rm-rcpth.rita-code EQ "R"
        USE-INDEX item-po NO-LOCK,

        EACH rm-rdtlh
        WHERE rm-rdtlh.r-no                      EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code                 EQ rm-rcpth.rita-code
          AND rm-rdtlh.s-num                     EQ po-ordl.s-num
          AND (rm-rdtlh.receiver-no              EQ "" OR
               NOT CAN-FIND(FIRST b-ap-invl WHERE b-ap-invl.i-no   EQ INT(SUBSTR(rm-rdtlh.receiver-no,1,10))
                                              AND b-ap-invl.line   EQ (po-ordl.po-no * 1000) + po-ordl.line
                                             /* AND ROWID(b-ap-invl) NE ROWID(ap-invl)*/ ))
        NO-LOCK:

     lv-uom = po-ordl.pr-qty-uom.

     IF lv-uom EQ "ROLL" THEN
       ASSIGN
        v-len  = 12
        lv-uom = "LF".

     IF po-ordl.item-type AND appaper-chr NE "PO UOM" AND
        CAN-FIND(FIRST item
                 WHERE item.company EQ cocode
                   AND item.i-no    EQ po-ordl.i-no
                   AND item.mat-type EQ "P")          THEN
       lv-uom = appaper-chr.

      v-qty = rm-rdtlh.qty.

      IF rm-rcpth.pur-uom NE po-ordl.pr-qty-uom THEN
        RUN sys/ref/convquom.p (rm-rcpth.pur-uom, lv-uom,
                                v-bwt, v-len, v-wid, v-dep,
                                v-qty, OUTPUT v-qty).

      CREATE tt-rec.
      ASSIGN
       tt-rec.rec-id     = RECID(rm-rcpth)
       tt-rec.selekt     = SUBSTR(rm-rdtlh.receiver-no,1,10) EQ
                           STRING(ap-inv.i-no,"9999999999")
       tt-rec.trans-date = rm-rcpth.trans-date
       tt-rec.qty-rec    = v-qty
       tt-rec.qty-inv    = IF tt-rec.selekt THEN DEC(SUBSTR(rm-rdtlh.receiver-no,11,17))
                                            ELSE v-qty.
    END.
  END.
        
  ELSE
  FOR EACH fg-rcpth
      WHERE fg-rcpth.company   EQ cocode
        AND fg-rcpth.i-no      EQ po-ordl.i-no
        AND fg-rcpth.po-no     EQ TRIM(STRING(po-ordl.po-no,">>>>>>>>>>"))
        AND fg-rcpth.rita-code EQ "R"
      USE-INDEX item-po NO-LOCK,

      EACH fg-rdtlh
      WHERE fg-rdtlh.r-no                      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code                 EQ fg-rcpth.rita-code
        AND (fg-rdtlh.receiver-no              EQ "" OR
             NOT CAN-FIND(FIRST b-ap-invl WHERE b-ap-invl.i-no   EQ INT(SUBSTR(fg-rdtlh.receiver-no,1,10))
                                            AND b-ap-invl.line   EQ (po-ordl.po-no * 1000) + po-ordl.line
                                           /* AND ROWID(b-ap-invl) NE ROWID(ap-invl)*/ ))
      NO-LOCK:
 
    CREATE tt-rec.
    ASSIGN
     tt-rec.rec-id     = RECID(fg-rcpth)
     tt-rec.selekt     = SUBSTR(fg-rdtlh.receiver-no,1,10) EQ
                         STRING(ap-inv.i-no,"9999999999")
     tt-rec.trans-date = fg-rcpth.trans-date
     tt-rec.r-no       = fg-rcpth.r-no
     tt-rec.qty-rec    = fg-rdtlh.qty
     tt-rec.qty-inv    = IF tt-rec.selekt THEN DEC(SUBSTR(fg-rdtlh.receiver-no,11,17))
                                          ELSE fg-rdtlh.qty.
  END.

  FOR EACH tt-rec WHERE tt-rec.rec-id EQ ?:
    li = li + 1.
  END.

  FOR EACH tt-rec WHERE tt-rec.rec-id NE ? BREAK BY tt-rec.rec-id:
    ASSIGN
     ld[1] = ld[1] + tt-rec.qty-rec
     ld[2] = ld[2] + tt-rec.qty-inv.

    IF LAST-OF(tt-rec.rec-id) THEN
      ASSIGN
       li             = li + 1
       tt-rec.qty-rec = ld[1]
       tt-rec.qty-inv = ld[2]
       ld             = 0.

    ELSE DELETE tt-rec.
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
  ENABLE BROWSE-2 Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


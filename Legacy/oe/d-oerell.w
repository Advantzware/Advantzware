&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
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

ASSIGN cocode = g_company.
ASSIGN locode = g_loc.

{sa/sa-sls01.i}

DEFINE VARIABLE lv-do-all-items    AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-item-imported   AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-import-rejected AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-ship-from       AS LOGICAL INITIAL YES NO-UNDO.
DEFINE VARIABLE ll-ask-import      AS LOGICAL NO-UNDO.
/* DEFINE VARIABLE v-copy-rowid       AS ROWID NO-UNDO. */
DEFINE VARIABLE lv-rowid           AS ROWID NO-UNDO.
DEFINE VARIABLE cv-s-codes         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cv-s-dscrs         AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cOrd-ok AS CHARACTER INITIAL ["R,I,S,P,A,N,U"].

DO TRANSACTION:
  {sys/inc/relmerge.i}
END.

RUN sys/ref/s-codes.p (OUTPUT cv-s-codes, OUTPUT cv-s-dscrs).

DEFINE VARIABLE lv-item-recid   AS RECID         NO-UNDO.
DEFINE VARIABLE ll-order-warned AS LOGICAL       NO-UNDO.
DEFINE VARIABLE ll-new-record   AS LOGICAL       NO-UNDO.
DEFINE BUFFER bf-rell FOR oe-rell.
DEFINE BUFFER bf-ordl FOR oe-ordl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-rell oe-ordl

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame oe-rell.ord-no oe-rell.i-no ~
oe-rell.po-no oe-rell.tag oe-rell.loc oe-rell.loc-bin oe-rell.job-no ~
oe-rell.job-no2 oe-rell.cust-no oe-rell.qty oe-rell.cases oe-rell.qty-case ~
oe-rell.partial oe-rell.rel-no oe-rell.b-ord-no oe-rell.s-code ~
oe-rell.link-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame oe-rell.ord-no ~
oe-rell.i-no oe-rell.po-no oe-rell.tag oe-rell.loc oe-rell.loc-bin ~
oe-rell.job-no oe-rell.job-no2 oe-rell.cust-no oe-rell.qty oe-rell.cases ~
oe-rell.qty-case oe-rell.partial oe-rell.rel-no oe-rell.b-ord-no ~
oe-rell.s-code oe-rell.link-no 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame oe-rell
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame oe-rell
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH oe-rell ~
      WHERE oe-rell.company eq cocode  SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH oe-rell ~
      WHERE oe-rell.company eq cocode  SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame oe-rell oe-ordl
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame oe-rell
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame oe-ordl


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-rell.ord-no oe-rell.i-no oe-rell.po-no ~
oe-rell.tag oe-rell.loc oe-rell.loc-bin oe-rell.job-no oe-rell.job-no2 ~
oe-rell.cust-no oe-rell.qty oe-rell.cases oe-rell.qty-case oe-rell.partial ~
oe-rell.rel-no oe-rell.b-ord-no oe-rell.s-code oe-rell.link-no 
&Scoped-define ENABLED-TABLES oe-rell
&Scoped-define FIRST-ENABLED-TABLE oe-rell
&Scoped-Define ENABLED-OBJECTS fi_part-no Btn_OK Btn_Done Btn_Cancel ~
RECT-21 RECT-38 
&Scoped-Define DISPLAYED-FIELDS oe-rell.ord-no oe-rell.i-no oe-rell.po-no ~
oe-rell.tag oe-rell.loc oe-rell.loc-bin oe-rell.job-no oe-rell.job-no2 ~
oe-rell.cust-no oe-rell.qty oe-rell.cases oe-rell.qty-case oe-rell.partial ~
oe-rell.rel-no oe-rell.b-ord-no oe-rell.s-code oe-rell.link-no 
&Scoped-define DISPLAYED-TABLES oe-rell
&Scoped-define FIRST-DISPLAYED-TABLE oe-rell
&Scoped-Define DISPLAYED-OBJECTS fi_part-no 

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

DEFINE VARIABLE fi_part-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cust Part#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 133.8 BY 3.57.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 133.8 BY 10.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      oe-rell, 
      oe-ordl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     oe-rell.ord-no AT ROW 1.24 COL 29.8 COLON-ALIGNED FORMAT ">>>>>>"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     oe-rell.i-no AT ROW 2.43 COL 29.8 COLON-ALIGNED HELP
          ""
          LABEL "FG Item#"
          VIEW-AS FILL-IN 
          SIZE 33.6 BY 1
     oe-rell.po-no AT ROW 3.62 COL 29.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 33.6 BY 1
     oe-rell.tag AT ROW 4.76 COL 29.8 COLON-ALIGNED
          LABEL "Tag" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 33.6 BY 1
     oe-rell.loc AT ROW 5.91 COL 29.8 COLON-ALIGNED
          LABEL "Whse"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     oe-rell.loc-bin AT ROW 7.19 COL 29.8 COLON-ALIGNED
          LABEL "Bin Loc."
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     oe-rell.job-no AT ROW 8.48 COL 29.8 COLON-ALIGNED
          LABEL "Job Number"
          VIEW-AS FILL-IN 
          SIZE 18.6 BY 1
     oe-rell.job-no2 AT ROW 8.48 COL 48.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     oe-rell.cust-no AT ROW 9.62 COL 29.8 COLON-ALIGNED
          LABEL "Customer"
          VIEW-AS FILL-IN 
          SIZE 23.6 BY 1
     oe-rell.qty AT ROW 2.43 COL 85.4 COLON-ALIGNED
          LABEL "Qty" FORMAT "->>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-rell.cases AT ROW 3.62 COL 85.4 COLON-ALIGNED
          LABEL "Units"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-rell.qty-case AT ROW 4.76 COL 85.4 COLON-ALIGNED
          LABEL "Qty/Unit"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-rell.partial AT ROW 5.91 COL 85.4 COLON-ALIGNED
          LABEL "Partial"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     oe-rell.rel-no AT ROW 7.19 COL 85.4 COLON-ALIGNED
          LABEL "Rel#"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     oe-rell.b-ord-no AT ROW 7.19 COL 94.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     oe-rell.s-code AT ROW 7.19 COL 109 COLON-ALIGNED
          LABEL "Type"
          VIEW-AS COMBO-BOX INNER-LINES 4 
          LIST-ITEM-PAIRS "B-Both","B",
                     "S-Ship","S",
                     "I-Invoice","I",
                     "T-Transfer","T"
          DROP-DOWN-LIST
     oe-rell.link-no AT ROW 9.62 COL 85.4 COLON-ALIGNED
          LABEL "Rel. Seq. #" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     fi_part-no AT ROW 8.48 COL 85.4 COLON-ALIGNED
     Btn_OK AT ROW 13.29 COL 37
     Btn_Done AT ROW 13.24 COL 57
     Btn_Cancel AT ROW 13.24 COL 77.2
     RECT-21 AT ROW 11.71 COL 1
     RECT-38 AT ROW 1 COL 1
     SPACE(0.99) SKIP(4.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Order Release Item Update".


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

/* SETTINGS FOR FILL-IN oe-rell.b-ord-no IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-rell.cases IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-rell.cust-no IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-rell.i-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN oe-rell.job-no IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-rell.job-no2 IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-rell.link-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-rell.loc IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-rell.loc-bin IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-rell.ord-no IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN oe-rell.partial IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-rell.qty IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-rell.qty-case IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-rell.rel-no IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-rell.s-code IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-rell.tag IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.oe-rell,asi.oe-ordl "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.oe-rell.company eq cocode "
     _Where[2]         = "ASI.oe-ordl.company eq oe-rell.company and ASI.oe-ordl.ord-no eq oe-rell.ord-no
  and oe-ordl.i-no eq oe-rell.i-no and oe-ordl.line eq oe-rell.line OUTER-JOIN NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Order Release Item Update */
DO:
    DEFINE VARIABLE char-val  AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE hlp-recid AS RECID         NO-UNDO.
    DEFINE VARIABLE lw-focus  AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE ll AS LOGICAL INITIAL YES  NO-UNDO.
    DEFINE VARIABLE op-rowid AS ROWID          NO-UNDO.

    lw-focus = FOCUS.

    CASE lw-focus:NAME :
         WHEN "ord-no" THEN DO:
           MESSAGE "Press YES to select by Order or NO by FG Item"
               VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL
               UPDATE ll.
           IF ll EQ YES THEN DO:
              RUN windows/l-ordlcs.w (g_company, oe-relh.cust-no,oe-relh.ship-id,lw-focus:SCREEN-VALUE, OUTPUT char-val, OUTPUT hlp-recid).     
              IF hlp-recid NE ? THEN DO:
                /*run display-orders (hlp-recid, char-val).*/
                 RUN import-order-items-look (hlp-recid, char-val, OUTPUT op-rowid).
                ll-ask-import = NO.
              END.
           END.
           ELSE
           IF ll EQ NO THEN DO:

             lv-rowid = IF AVAILABLE oe-rell THEN ROWID(oe-rell) ELSE ?.
             RUN oe/l-binsel.w (ROWID(oe-relh), OUTPUT lv-rowid).
             ip-rowid  = lv-rowid .
             IF lv-item-recid NE ? THEN DO:
                 FIND oe-rell EXCLUSIVE-LOCK WHERE RECID(oe-rell) EQ lv-item-recid  NO-ERROR.
                 IF AVAILABLE oe-rell THEN DELETE oe-rell .
             END.
             APPLY "go" TO FRAME {&FRAME-NAME}.

           END.
         END.
         WHEN "po-no" THEN DO:
              RUN windows/l-custpo.w (g_company, INTEGER(oe-rell.ord-no:screen-value ),lw-focus:SCREEN-VALUE, OUTPUT char-val, OUTPUT hlp-recid).     
              IF char-val NE "" THEN ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val)
                                            oe-rell.i-no:SCREEN-VALUE = ENTRY(2,char-val).
         END.
        WHEN "i-no" THEN DO:
              RUN windows/l-orditm.w (g_company, INTEGER(oe-rell.ord-no:SCREEN-VALUE),lw-focus:SCREEN-VALUE, OUTPUT char-val, OUTPUT hlp-recid).     
              IF hlp-recid NE ? THEN RUN display-orditm (hlp-recid).
        END.        
        WHEN "loc" THEN DO:
            RUN windows/l-loc.w (g_company,lw-focus:SCREEN-VALUE, OUTPUT char-val).
            IF char-val NE "" THEN DO :
               ASSIGN lw-focus:SCREEN-VALUE  = ENTRY(1,char-val).
                      /*oe-rell.loc-bin:screen-value in browse {&browse-name} = entry(2,char-val)*/

            END.  
       END.
       WHEN "loc-bin" THEN DO:
            RUN windows/l-fgbin.w (g_company,oe-rell.loc:SCREEN-VALUE, lw-focus:SCREEN-VALUE, OUTPUT char-val).
            IF char-val NE "" THEN DO :
               ASSIGN lw-focus:SCREEN-VALUE  = ENTRY(1,char-val).
                      /*rm-rctd.qty:screen-value = entry(3,char-val)
                        rm-rctd.tag:screen-value = entry(4,char-val)*/

            END.   
      END.
      WHEN "s-code" THEN DO:
            RUN windows/l-cddscr.w ("Release Types", cv-s-codes, cv-s-dscrs, lw-focus:SCREEN-VALUE, OUTPUT char-val).
            IF char-val NE "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
      END.

    END CASE.

    RETURN NO-APPLY.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Order Release Item Update */
ANYWHERE
DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Order Release Item Update */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    DISABLE TRIGGERS FOR LOAD OF oe-rell .

    IF lv-item-recid NE ? THEN DO:

       FIND oe-rell EXCLUSIVE-LOCK
            WHERE RECID(oe-rell) EQ lv-item-recid  NO-ERROR.
       IF AVAILABLE oe-rell THEN DELETE oe-rell .
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
  DEFINE VARIABLE ll        AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE op-error  AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE old-po-no LIKE oe-rell.po-no NO-UNDO.
  DEFINE BUFFER b-oe-rell   FOR oe-rell.

  IF ip-type EQ "view" THEN DO: 
     APPLY "go" TO FRAME {&FRAME-NAME}.
     RETURN.
  END.
   /* Code placed here will execute PRIOR to standard behavior. */
  old-po-no = IF AVAILABLE oe-rell THEN oe-rell.po-no ELSE "".


  RUN valid-ord-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-i-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-job-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc-bin NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-s-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  DO TRANSACTION:
      FIND CURRENT oe-rell EXCLUSIVE-LOCK NO-ERROR.

      DO WITH FRAME {&FRAME-NAME}:
          ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
      END.
  END.

  IF oe-rell.line EQ 0 THEN DO:
    FIND FIRST bf-ordl NO-LOCK
        WHERE bf-ordl.company EQ oe-rell.company
          AND bf-ordl.ord-no  EQ oe-rell.ord-no
          AND bf-ordl.i-no    EQ oe-rell.i-no
          NO-ERROR.
    IF AVAILABLE bf-ordl THEN oe-rell.line = bf-ordl.line.
  END.

  FIND FIRST oe-rel
      WHERE oe-rel.company EQ oe-rell.company
        AND oe-rel.ord-no  EQ oe-rell.ord-no
        AND oe-rel.ship-id EQ oe-relh.ship-id
        AND oe-rel.i-no    EQ oe-rell.i-no
        AND oe-rel.line    EQ oe-rell.line
        AND oe-rel.po-no   EQ old-po-no
        AND oe-rel.link-no EQ 0
        AND NOT CAN-FIND(FIRST b-oe-rell
                         WHERE b-oe-rell.company  EQ oe-rel.company
                           AND b-oe-rell.ord-no   EQ oe-rel.ord-no
                           AND b-oe-rell.i-no     EQ oe-rel.i-no
                           AND b-oe-rell.line     EQ oe-rel.line
                           AND b-oe-rell.rel-no   EQ oe-rel.rel-no
                           AND b-oe-rell.b-ord-no EQ oe-rel.b-ord-no
                           AND b-oe-rell.po-no    EQ oe-rel.po-no
                           AND ROWID(b-oe-rell)   NE ROWID(oe-rell))
      NO-ERROR.
  IF AVAILABLE oe-rel THEN 
      ASSIGN oe-rel.qty = oe-rell.qty
       oe-rel.rel-no = oe-rell.rel-no.

  IF oe-rell.po-no NE old-po-no THEN
  FOR EACH oe-rel
      WHERE oe-rel.company  EQ oe-rell.company
        AND oe-rel.ord-no   EQ oe-rell.ord-no
        AND oe-rel.ship-id  EQ oe-relh.ship-id
        AND oe-rel.i-no     EQ oe-rell.i-no
        AND oe-rel.line     EQ oe-rell.line
        AND oe-rel.rel-no   EQ oe-rell.rel-no
        AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
        AND oe-rel.po-no    EQ old-po-no
        AND oe-rel.link-no  EQ 0:
    oe-rel.po-no = oe-rell.po-no.
  END.

  RELEASE oe-rel.

  ip-rowid = ROWID(oe-rell).

APPLY "go" TO FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.cases Dialog-Frame
ON VALUE-CHANGED OF oe-rell.cases IN FRAME Dialog-Frame /* Units */
DO:
  RUN calc-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.cust-no Dialog-Frame
ON LEAVE OF oe-rell.cust-no IN FRAME Dialog-Frame /* Customer */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.i-no Dialog-Frame
ON LEAVE OF oe-rell.i-no IN FRAME Dialog-Frame /* FG Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF oe-rell.i-no:SCREEN-VALUE NE "" THEN DO:
        RUN valid-i-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.job-no Dialog-Frame
ON LEAVE OF oe-rell.job-no IN FRAME Dialog-Frame /* Job Number */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.job-no2 Dialog-Frame
ON LEAVE OF oe-rell.job-no2 IN FRAME Dialog-Frame /* job-no2 */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-job-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.loc Dialog-Frame
ON LEAVE OF oe-rell.loc IN FRAME Dialog-Frame /* Whse */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.loc-bin Dialog-Frame
ON LEAVE OF oe-rell.loc-bin IN FRAME Dialog-Frame /* Bin Loc. */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.ord-no Dialog-Frame
ON ENTRY OF oe-rell.ord-no IN FRAME Dialog-Frame /* Order# */
DO:

  IF ip-type EQ "update"  OR ip-type EQ "view" THEN DO:
    APPLY "tab" TO {&self-name} .
    RETURN NO-APPLY.
  END.
  ll-ask-import = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.ord-no Dialog-Frame
ON LEAVE OF oe-rell.ord-no IN FRAME Dialog-Frame /* Order# */
DO:
  DEFINE VARIABLE op-rowid AS ROWID NO-UNDO.

  IF LASTKEY NE -1 THEN DO:
    RUN valid-ord-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF ip-type EQ "Add" THEN DO:
      ASSIGN
       oe-rell.loc:SCREEN-VALUE      = g_loc
       oe-rell.s-code:SCREEN-VALUE   = IF lv-ship-from THEN "B" ELSE "I".

      IF ll-ask-import AND NOT lv-do-all-items THEN DO:
         RUN check-exist-ord (ROWID(oe-rell)).
         IF RETURN-VALUE EQ "" THEN DO:
             MESSAGE "Import all items? " VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                      UPDATE ll-ans AS LOGICAL.
             IF ll-ans THEN DO:
                ASSIGN lv-item-imported = YES
                       lv-import-rejected = NO.

                RUN import-order-items (RECID(oe-ord), "order", OUTPUT op-rowid).

                ip-rowid = op-rowid .

                IF lv-item-recid NE ? THEN DO:
                    FIND oe-rell EXCLUSIVE-LOCK WHERE RECID(oe-rell) EQ lv-item-recid  NO-ERROR.
                    IF AVAILABLE oe-rell THEN DELETE oe-rell .
                END.

                 APPLY "go" TO FRAME {&FRAME-NAME}.

             END.
             ELSE lv-import-rejected = YES.
         END.

      END.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.partial Dialog-Frame
ON VALUE-CHANGED OF oe-rell.partial IN FRAME Dialog-Frame /* Partial */
DO:
  RUN calc-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.po-no Dialog-Frame
ON LEAVE OF oe-rell.po-no IN FRAME Dialog-Frame /* Customer PO */
DO:
  /* if lastkey = -1 then return.
   IF oe-rell.po-no:MODIFIED IN BROWSE {&browse-name} THEN DO:
      find first oe-rel
          where oe-rel.company = g_company
            AND oe-rel.ord-no = int(oe-rell.ord-no:screen-value in browse {&browse-name} )
            and oe-rel.po-no = self:SCREEN-VALUE
          no-lock no-error.
     if NOT avail oe-rel then do:
        message "Invalid PO#. Try Help. " view-as alert-box error.
        return no-apply.
     end.               
     assign oe-rell.po-no:screen-value = oe-rel.po-no
            oe-rell.i-no:screen-value = oe-rel.i-no
            .         
   END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.qty Dialog-Frame
ON LEAVE OF oe-rell.qty IN FRAME Dialog-Frame /* Qty */
DO:
  IF {&self-name}:MODIFIED THEN RUN calc-cases.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.qty-case Dialog-Frame
ON VALUE-CHANGED OF oe-rell.qty-case IN FRAME Dialog-Frame /* Qty/Unit */
DO:
  RUN calc-qty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-rell.s-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-rell.s-code Dialog-Frame
ON LEAVE OF oe-rell.s-code IN FRAME Dialog-Frame /* S/I */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-s-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
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

    FIND oe-relh NO-LOCK
        WHERE oe-relh.company EQ cocode
          AND RECID(oe-relh)  EQ ip-recid2 NO-ERROR .

    IF ip-type EQ "copy" THEN lv-item-recid = ip-recid.


    IF ip-recid EQ ? THEN DO:
        RUN create-item.
        FIND po-ordl NO-LOCK WHERE RECID(oe-rell) EQ lv-item-recid NO-ERROR.
    END.
    ELSE FIND oe-rell NO-LOCK WHERE RECID(oe-rell) EQ ip-recid NO-ERROR.

    IF ip-type NE "view" THEN 
    DO: 
        RUN enable_UI.
        RUN display-item.

        ASSIGN ll-order-warned                     = NO.
            btn_done:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE 
    DO:
        RUN display-item.
        ASSIGN btn_done:HIDDEN IN FRAME {&FRAME-NAME} = NO.
            btn_done:SENSITIVE                        = YES.
            btn_ok:HIDDEN                             = YES.
            btn_cancel:HIDDEN                         = YES.
    END.

    DO WITH FRAME {&FRAME-NAME}:
        IF ip-type EQ "update" THEN DISABLE oe-rell.ord-no oe-rell.rel-no oe-rell.b-ord-no.

        IF ip-type EQ "add"  OR ip-type EQ "copy" THEN DO:
            APPLY "entry" TO oe-rell.ord-no  .
        END.
        oe-rell.cust-no:HIDDEN IN FRAME {&FRAME-NAME}  = TRUE .
    END.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-cases Dialog-Frame 
PROCEDURE calc-cases :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-qty      LIKE oe-rell.qty      NO-UNDO.
  DEFINE VARIABLE lv-cases    LIKE oe-rell.cases    NO-UNDO.
  DEFINE VARIABLE lv-qty-case LIKE oe-rell.qty-case NO-UNDO.
  DEFINE VARIABLE lv-partial  LIKE oe-rell.partial  NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN lv-qty      = DEC(oe-rell.qty:SCREEN-VALUE ).
     lv-qty-case = DEC(oe-rell.qty-case:SCREEN-VALUE ).
     lv-partial  = DEC(oe-rell.partial:SCREEN-VALUE ).

    IF lv-qty-case EQ 0 THEN lv-qty-case = lv-qty.

    ASSIGN lv-cases   = TRUNC((lv-qty - lv-partial) / lv-qty-case,0).
     lv-partial = lv-qty - (lv-cases * lv-qty-case).

     oe-rell.cases:SCREEN-VALUE     = STRING(lv-cases).
     oe-rell.qty-case:SCREEN-VALUE  = STRING(lv-qty-case).
     oe-rell.partial:SCREEN-VALUE   = STRING(lv-partial).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-qty Dialog-Frame 
PROCEDURE calc-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    oe-rell.qty:SCREEN-VALUE  =
        STRING(INT(oe-rell.cases:SCREEN-VALUE ) *
               INT(oe-rell.qty-case:SCREEN-VALUE ) +
               INT(oe-rell.partial:SCREEN-VALUE ),
               oe-rell.qty:FORMAT ).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-exist-ord Dialog-Frame 
PROCEDURE check-exist-ord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:     return "" to import  or any string  not to import items
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
  IF CAN-FIND(FIRST bf-rell
               WHERE bf-rell.company EQ oe-relh.company
                 AND bf-rell.r-no EQ oe-relh.r-no
                 AND INTEGER(oe-rell.ord-no:SCREEN-VALUE  ) EQ bf-rell.ord-no
                 AND ROWID(bf-rell) NE ip-rowid) THEN
     RETURN "Order Exist".

  RETURN "".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item Dialog-Frame 
PROCEDURE create-item :
/*------------------------------------------------------------------------------
      Purpose:     
      PARAMs:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-oe-rell FOR oe-rell.

    FIND FIRST oe-relh NO-LOCK
        WHERE oe-relh.company EQ cocode
        AND RECID(oe-relh) EQ ip-recid2 
        NO-ERROR.

    IF AVAILABLE oe-relh THEN 
    DO WITH FRAME {&FRAME-NAME}:

        /*FIND LAST oe-rell NO-LOCK WHERE
            oe-rell.company EQ oe-relh.company AND
            oe-rell.r-no EQ oe-relh.r-no 
            NO-ERROR.

        z = IF AVAILABLE oe-rell THEN oe-rell.line + 1 ELSE 1.*/

        CREATE oe-rell.
        ASSIGN lv-item-recid = RECID(oe-rell).
            ll-new-record = YES.

        ASSIGN oe-rell.company = oe-relh.company.
               oe-rell.r-no = oe-relh.r-no.

        FIND FIRST bf-oe-rell NO-LOCK
            WHERE bf-oe-rell.r-no   EQ oe-relh.r-no
              AND bf-oe-rell.s-code NE ""
              AND ROWID(bf-oe-rell) NE ROWID(oe-rell)
            NO-ERROR.
        IF AVAILABLE bf-oe-rell THEN oe-rell.s-code = bf-oe-rell.s-code.


        FIND CURRENT oe-rell NO-LOCK NO-ERROR.
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
    IF AVAILABLE oe-rell  THEN 
    DO:
        FIND FIRST oe-ordl NO-LOCK
             WHERE oe-ordl.company EQ oe-rell.company
               AND oe-ordl.ord-no EQ oe-rell.ord-no 
               AND oe-ordl.i-no EQ oe-rell.i-no 
               AND oe-ordl.line EQ oe-rell.line NO-ERROR .
        IF AVAILABLE oe-ordl THEN
            ASSIGN fi_part-no = oe-ordl.part-no .

        DISPLAY  oe-rell.ord-no oe-rell.i-no 
            oe-rell.po-no oe-rell.qty oe-rell.tag oe-rell.loc oe-rell.loc-bin 
            oe-rell.job-no oe-rell.job-no2 oe-rell.cust-no oe-rell.cases 
            oe-rell.qty-case oe-rell.partial oe-rell.rel-no oe-rell.b-ord-no 
            oe-rell.s-code oe-rell.link-no fi_part-no 
            WITH FRAME Dialog-Frame.
    END.


    IF ip-type NE "view" THEN DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.
    END.

    VIEW FRAME {&FRAME-NAME}. 
    APPLY "entry" TO FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-orditm Dialog-Frame 
PROCEDURE display-orditm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.


  FIND bf-ordl WHERE RECID(bf-ordl) = ip-recid NO-LOCK NO-ERROR.
  IF NOT AVAILABLE bf-ordl THEN RETURN.

  FIND FIRST oe-ord OF bf-ordl NO-LOCK.

  FIND FIRST oe-rel NO-LOCK
       WHERE oe-rel.company EQ bf-ordl.company
         AND oe-rel.ord-no EQ bf-ordl.ord-no
         AND oe-rel.ship-id EQ oe-relh.ship-id
         AND oe-rel.rel-date EQ oe-relh.rel-date
         AND oe-rel.i-no EQ bf-ordl.i-no
         AND oe-rel.link-no EQ 0
      NO-ERROR.  
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN oe-rell.i-no:screen-value  = STRING(bf-ordl.i-no).
         oe-rell.qty:SCREEN-VALUE = IF AVAILABLE oe-rel THEN STRING(oe-rel.qty) ELSE STRING(bf-ordl.qty).
         oe-rell.qty-case:SCREEN-VALUE = STRING(bf-ordl.cas-cnt).
         oe-rell.job-no:SCREEN-VALUE = STRING(bf-ordl.job-no).
         oe-rell.job-no2:SCREEN-VALUE = STRING(bf-ordl.job-no2).
         oe-rell.po-no:SCREEN-VALUE = IF AVAILABLE oe-rel THEN oe-rel.po-no
                                      ELSE
                                      IF bf-ordl.po-no NE "" THEN bf-ordl.po-no
                                      ELSE oe-ord.po-no.

  FIND fg-bin NO-LOCK 
      WHERE fg-bin.company EQ bf-ordl.company
      AND fg-bin.i-no EQ bf-ordl.i-no
      AND fg-bin.loc EQ  oe-rell.loc
      AND fg-bin.qty EQ INTEGER(oe-rell.qty:SCREEN-VALUE)
      NO-ERROR.
  IF AVAILABLE fg-bin THEN  DO:
         ASSIGN oe-rell.tag:SCREEN-VALUE = fg-bin.tag.
                oe-rell.loc-bin:SCREEN-VALUE = fg-bin.loc-bin.
         IF fg-bin.case-count GT 0 THEN oe-rell.qty-case:SCREEN-VALUE = STRING(fg-bin.case-count).         
  END.

  RUN calc-cases.

  /** Find last actual release for this order number and add 1 to
     the get the next release. **/
  /* === rel-no logic moved to line (oe-rell) ========*/
  DEFINE BUFFER bf-rell FOR oe-rell .
  DEFINE VARIABLE li-nxt-rel-no AS INTEGER NO-UNDO.
  FOR EACH bf-rell WHERE bf-rell.company EQ g_company
      AND bf-rell.ord-no  EQ INT(oe-rell.ord-no:SCREEN-VALUE)
      AND ROWID(bf-rell) NE ROWID(oe-rell) NO-LOCK 
      BY bf-rell.rel-no DESCENDING:
      li-nxt-rel-no =  bf-rell.rel-no.
      LEAVE.  
  END.
  li-nxt-rel-no = li-nxt-rel-no + 1.
  oe-rell.rel-no:SCREEN-VALUE = STRING(li-nxt-rel-no).
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
  DISPLAY fi_part-no 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE oe-rell THEN 
    DISPLAY oe-rell.ord-no oe-rell.i-no oe-rell.po-no oe-rell.tag oe-rell.loc 
          oe-rell.loc-bin oe-rell.job-no oe-rell.job-no2 oe-rell.cust-no 
          oe-rell.qty oe-rell.cases oe-rell.qty-case oe-rell.partial 
          oe-rell.rel-no oe-rell.b-ord-no oe-rell.s-code oe-rell.link-no 
      WITH FRAME Dialog-Frame.
  ENABLE oe-rell.ord-no oe-rell.i-no oe-rell.po-no oe-rell.tag oe-rell.loc 
         oe-rell.loc-bin oe-rell.job-no oe-rell.job-no2 oe-rell.cust-no 
         oe-rell.qty oe-rell.cases oe-rell.qty-case oe-rell.partial 
         oe-rell.rel-no oe-rell.b-ord-no oe-rell.s-code oe-rell.link-no 
         fi_part-no Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import-order-items Dialog-Frame 
PROCEDURE import-order-items :
/*------------------------------------------------------------------------------
  Purpose:    oe/allrel.p  oe/oe-rellu.p  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
  DEFINE INPUT PARAMETER ip-char   AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER op-rowid AS ROWID     NO-UNDO.

  DEFINE BUFFER b-oe-rell FOR oe-rell.
  DEFINE BUFFER b-oe-rell-new FOR oe-rell.

  FIND bf-ordl NO-LOCK
      WHERE RECID(bf-ordl) EQ ip-recid NO-ERROR.
  IF AVAILABLE bf-ordl THEN
    FIND FIRST oe-ord OF bf-ordl NO-LOCK NO-ERROR.
  ELSE
    FIND oe-ord NO-LOCK
        WHERE RECID(oe-ord) EQ ip-recid NO-ERROR.

  FIND FIRST b-oe-rell NO-LOCK
      WHERE b-oe-rell.r-no   EQ oe-relh.r-no
        AND b-oe-rell.s-code NE ""
        AND ROWID(b-oe-rell) NE ROWID(oe-rell)
      NO-ERROR.

  IF AVAILABLE oe-ord THEN
  FOR EACH bf-ordl OF oe-ord NO-LOCK:
    IF RECID(bf-ordl) EQ ip-recid OR ip-char EQ "order" THEN DO:
      FIND FIRST oe-rel
          WHERE oe-rel.company EQ oe-ord.company
          AND oe-rel.ord-no EQ oe-ord.ord-no
          AND oe-rel.ship-id EQ oe-relh.ship-id
          AND oe-rel.rel-date EQ oe-relh.rel-date
          AND oe-rel.i-no EQ bf-ordl.i-no
          AND oe-rel.link-no EQ 0
          NO-ERROR.

      CREATE b-oe-rell-new.
      ASSIGN b-oe-rell-new.company = oe-relh.company.
             b-oe-rell-new.loc = /* g_loc - 10021210 */ IF AVAILABLE oe-rel THEN oe-rel.spare-char-1
                                ELSE g_loc.
             b-oe-rell-new.r-no = oe-relh.r-no.
             b-oe-rell-new.po-no = IF AVAILABLE oe-rel THEN oe-rel.po-no
                                   ELSE
                                   IF bf-ordl.po-no NE "" THEN bf-ordl.po-no
                                   ELSE oe-ord.po-no .
             b-oe-rell-new.line = bf-ordl.line.
             b-oe-rell-new.qty = IF AVAILABLE oe-rel THEN oe-rel.qty ELSE bf-ordl.qty.
             b-oe-rell-new.i-no = bf-ordl.i-no.
             b-oe-rell-new.link-no = IF AVAILABLE oe-rel THEN oe-rel.r-no ELSE 0.
             b-oe-rell-new.s-code = IF AVAILABLE b-oe-rell THEN b-oe-rell.s-code ELSE "B".
             b-oe-rell-new.qty-case = bf-ordl.cas-cnt.
             b-oe-rell-new.ord-no = oe-ord.ord-no.
             b-oe-rell-new.job-no = bf-ordl.job-no.
             b-oe-rell-new.job-no2 = bf-ordl.job-no2.

      /*{oe/rel-no.i}*/
      DEFINE BUFFER bf-rell FOR oe-rell .
      DEFINE VARIABLE il-nxt-rel-no AS INTEGER NO-UNDO.
      FOR EACH bf-rell NO-LOCK
           WHERE bf-rell.company EQ g_company
             AND bf-rell.ord-no  EQ b-oe-rell-new.ord-no 
          BY bf-rell.rel-no DESCENDING:
          il-nxt-rel-no =  bf-rell.rel-no.
          LEAVE.  
       END.

      ASSIGN
       il-nxt-rel-no = il-nxt-rel-no + 1
       b-oe-rell-new.rel-no = il-nxt-rel-no.

      IF AVAILABLE oe-rel THEN oe-rel.rel-no = b-oe-rell-new.rel-no. 

      FIND FIRST itemfg NO-LOCK
          WHERE itemfg.company EQ b-oe-rell-new.company
            AND itemfg.i-no    EQ b-oe-rell-new.i-no
          NO-ERROR.

      ASSIGN
      b-oe-rell-new.qty-case = IF AVAILABLE bf-ordl AND
                               bf-ordl.cas-cnt GT 0 THEN bf-ordl.cas-cnt
                               ELSE
                               IF AVAILABLE itemfg           AND
                                  itemfg.case-count GT 0 THEN itemfg.case-count
                               ELSE 1.


       b-oe-rell-new.cases   = TRUNC((b-oe-rell-new.qty - b-oe-rell-new.partial) /
                               b-oe-rell-new.qty-case,0).
       b-oe-rell-new.partial = b-oe-rell-new.qty - (b-oe-rell-new.cases * b-oe-rell-new.qty-case).

      IF RECID(bf-ordl) EQ ip-recid THEN op-rowid = ROWID(b-oe-rell-new).
    END.                 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import-order-items-look Dialog-Frame 
PROCEDURE import-order-items-look :
/*------------------------------------------------------------------------------
  Purpose:    oe/allrel.p  oe/oe-rellu.p  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
  DEFINE INPUT PARAMETER ip-char   AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER op-rowid AS ROWID     NO-UNDO.

  DEFINE BUFFER b-oe-rell FOR oe-rell.
  DEFINE BUFFER b-oe-rell-new FOR oe-rell.

  FIND bf-ordl NO-LOCK WHERE RECID(bf-ordl) EQ ip-recid NO-ERROR.
  IF AVAILABLE bf-ordl THEN
    FIND FIRST oe-ord OF bf-ordl NO-LOCK NO-ERROR.
  ELSE
    FIND oe-ord NO-LOCK WHERE RECID(oe-ord) EQ ip-recid NO-ERROR.

  FIND FIRST b-oe-rell NO-LOCK
      WHERE b-oe-rell.r-no   EQ oe-relh.r-no
        AND b-oe-rell.s-code NE ""
        AND ROWID(b-oe-rell) NE ROWID(oe-rell)
      NO-ERROR.
 DO WITH FRAME {&FRAME-NAME}:
  IF AVAILABLE oe-ord THEN
  FOR EACH bf-ordl OF oe-ord NO-LOCK:
    IF RECID(bf-ordl) EQ ip-recid OR ip-char EQ "order" THEN DO:
      FIND FIRST oe-rel
          WHERE oe-rel.company EQ oe-ord.company
          AND oe-rel.ord-no EQ oe-ord.ord-no
          AND oe-rel.ship-id EQ oe-relh.ship-id
          AND oe-rel.rel-date EQ oe-relh.rel-date
          AND oe-rel.i-no EQ bf-ordl.i-no
          AND oe-rel.link-no EQ 0
          NO-ERROR.

     /* create b-oe-rell-new.*/
      ASSIGN oe-rell.loc:SCREEN-VALUE = /* g_loc - 10021210 */ IF AVAILABLE oe-rel THEN oe-rel.spare-char-1
                                ELSE g_loc .
             oe-rell.po-no:SCREEN-VALUE = IF AVAILABLE oe-rel THEN oe-rel.po-no
                                   ELSE
                                   IF bf-ordl.po-no NE "" THEN bf-ordl.po-no
                                   ELSE oe-ord.po-no  .
             /*oe-rell.line = bf-ordl.line*/
             oe-rell.qty:SCREEN-VALUE = IF AVAILABLE oe-rel THEN STRING(oe-rel.qty) ELSE STRING(bf-ordl.qty).
             oe-rell.i-no:SCREEN-VALUE = bf-ordl.i-no .
             oe-rell.link-no:SCREEN-VALUE = IF AVAILABLE oe-rel THEN STRING(oe-rel.r-no) ELSE "0".
             oe-rell.s-code:SCREEN-VALUE = IF AVAILABLE b-oe-rell THEN b-oe-rell.s-code ELSE "B" .
             oe-rell.qty-case:SCREEN-VALUE = STRING(bf-ordl.cas-cnt) .
             oe-rell.ord-no:SCREEN-VALUE = STRING(oe-ord.ord-no) .
             oe-rell.job-no:SCREEN-VALUE = bf-ordl.job-no .
             oe-rell.job-no2:SCREEN-VALUE = STRING(bf-ordl.job-no2) .



      FIND FIRST itemfg NO-LOCK
          WHERE itemfg.company EQ oe-rell.company
            AND itemfg.i-no    EQ oe-rell.i-no:SCREEN-VALUE
          NO-ERROR.

      ASSIGN
      oe-rell.qty-case:SCREEN-VALUE = IF AVAILABLE bf-ordl AND
                               bf-ordl.cas-cnt GT 0 THEN STRING(bf-ordl.cas-cnt)
                               ELSE
                               IF AVAILABLE itemfg           AND
                                  itemfg.case-count GT 0 THEN STRING(itemfg.case-count)
                               ELSE "1" .


       oe-rell.cases:SCREEN-VALUE   = STRING((INT(oe-rell.qty:SCREEN-VALUE) - INT(oe-rell.partial:SCREEN-VALUE)) /
                              INT(oe-rell.qty-case:SCREEN-VALUE)) .
       oe-rell.partial:SCREEN-VALUE = STRING(INT(oe-rell.qty:SCREEN-VALUE) - (INT(oe-rell.cases:SCREEN-VALUE) * INT(oe-rell.qty-case:SCREEN-VALUE))).



    END.                 
  END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no Dialog-Frame 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF oe-rell.cust-no:SCREEN-VALUE /*IN BROWSE {&browse-name}*/ NE "" AND
       (oe-rell.cust-no:SCREEN-VALUE /*IN BROWSE {&browse-name}*/ NE oe-bolh.cust-no OR
        NOT CAN-FIND(FIRST fg-bin 
                     WHERE fg-bin.company EQ cocode
                       AND fg-bin.i-no    EQ oe-rell.i-no:SCREEN-VALUE 
                       AND fg-bin.job-no  EQ oe-rell.job-no:SCREEN-VALUE 
                       AND fg-bin.job-no2 EQ INT(oe-rell.job-no2:SCREEN-VALUE )
                       AND fg-bin.loc     EQ oe-rell.loc:SCREEN-VALUE 
                       AND fg-bin.loc-bin EQ oe-rell.loc-bin:SCREEN-VALUE 
                       AND fg-bin.tag     EQ oe-rell.tag:SCREEN-VALUE 
                       AND fg-bin.cust-no EQ oe-rell.cust-no:SCREEN-VALUE ))
        THEN DO:
      MESSAGE "Invalid Customer#, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-rell.ord-no .
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
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

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST bf-ordl NO-LOCK
        WHERE bf-ordl.company EQ cocode
          AND bf-ordl.ord-no  EQ INTEGER(oe-rell.ord-no:SCREEN-VALUE )
          AND bf-ordl.i-no    EQ oe-rell.i-no:SCREEN-VALUE 
         NO-ERROR.
    IF NOT AVAILABLE bf-ordl THEN DO:
      MESSAGE TRIM(oe-rell.i-no:LABEL ) +
              " not on Order, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-rell.i-no .
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no Dialog-Frame 
PROCEDURE valid-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-job-no LIKE bf-ordl.job-no NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN lv-job-no = TRIM(oe-rell.job-no:SCREEN-VALUE )
     lv-job-no = FILL(" ",6 - LENGTH(lv-job-no)) + lv-job-no
     oe-rell.job-no:SCREEN-VALUE  = lv-job-no.

    IF relmerge-int EQ 0 THEN DO:
      FIND FIRST bf-ordl
          WHERE bf-ordl.company  EQ cocode
            AND bf-ordl.ord-no   EQ INTEGER(oe-rell.ord-no:SCREEN-VALUE )
            AND bf-ordl.i-no     EQ oe-rell.i-no:SCREEN-VALUE 
            AND (TRIM(bf-ordl.job-no) EQ "" OR
                 (bf-ordl.job-no EQ lv-job-no AND
                  (bf-ordl.job-no2 EQ INTEGER(oe-rell.job-no2:SCREEN-VALUE ) OR
                   FOCUS:NAME EQ "job-no")))
          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bf-ordl THEN DO:
        MESSAGE TRIM(oe-rell.job-no:LABEL ) +
              " not for Order/FG, try again..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO oe-rell.job-no .
        RETURN ERROR.
      END.
    END.
  END.

  {methods/lValidateError.i NO}
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

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
      FIND FIRST loc NO-LOCK
           WHERE loc.company = cocode
             AND loc.loc EQ oe-rell.loc:SCREEN-VALUE NO-ERROR.
      IF NOT AVAIL loc THEN DO:
          MESSAGE "Invalid Whse, try help..."
              VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO oe-rell.loc .
          RETURN ERROR.
      END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin Dialog-Frame 
PROCEDURE valid-loc-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
 DO WITH FRAME {&FRAME-NAME}:
      FIND FIRST fg-bin NO-LOCK
          WHERE fg-bin.company EQ cocode 
            AND fg-bin.loc EQ oe-rell.loc:SCREEN-VALUE 
            AND fg-bin.i-no EQ '' 
            AND fg-bin.loc-bin EQ oe-rell.loc-bin:SCREEN-VALUE NO-ERROR. 
      IF NOT AVAIL fg-bin THEN DO:
          MESSAGE "Invalid Bin Loc, try help..."
              VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO oe-rell.loc-bin .
          RETURN ERROR.
      END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ord-no Dialog-Frame 
PROCEDURE valid-ord-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-ord-no LIKE oe-rell.ord-no NO-UNDO.
  DEFINE VARIABLE cv-msg    AS CHARACTER        NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     cv-msg    = ""
     lv-ord-no = INT(oe-rell.ord-no:SCREEN-VALUE ).

    IF oe-rell.loc:SCREEN-VALUE  EQ "" THEN
      oe-rell.loc:SCREEN-VALUE     = g_loc.

    IF oe-rell.s-code:SCREEN-VALUE  EQ "" THEN
      oe-rell.s-code:SCREEN-VALUE  =
          IF lv-ship-from THEN "B" ELSE "I".

    IF lv-ord-no NE 0 THEN
    FIND FIRST oe-ord NO-LOCK
         WHERE oe-ord.company EQ g_company
           AND oe-ord.ord-no  EQ lv-ord-no
           NO-ERROR.

    IF cv-msg EQ "" THEN
      IF NOT AVAILABLE oe-ord THEN cv-msg = "invalid".

    IF AVAILABLE oe-ord AND oe-ord.cust-no NE oe-relh.cust-no THEN DO:
         MESSAGE "Sorry, order must match current customer. Try help..." 
                VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO oe-rell.ord-no .
         RETURN ERROR.
    END.

    IF cv-msg EQ "" THEN
      IF oe-ord.stat EQ "H" THEN cv-msg = "on hold".

    IF cv-msg EQ "" THEN
      IF LOOKUP(oe-ord.stat,cOrd-ok) LE 0 THEN cv-msg = "not available for release".

    IF cv-msg NE "" THEN DO:
      MESSAGE TRIM(oe-rell.ord-no:LABEL ) + " is " +
              TRIM(cv-msg) + ", try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-rell.ord-no .
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-code Dialog-Frame 
PROCEDURE valid-s-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER b-oe-rell FOR oe-rell.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF LOOKUP(oe-rell.s-code:SCREEN-VALUE ,cv-s-codes) LE 0 OR
       CAN-FIND(FIRST b-oe-rell
                WHERE b-oe-rell.r-no   EQ oe-relh.r-no
                  AND b-oe-rell.s-code NE oe-rell.s-code:SCREEN-VALUE 
                  AND ROWID(b-oe-rell) NE ROWID(oe-rell))
    THEN DO:
      MESSAGE "Invalid " + TRIM(oe-rell.s-code:LABEL)  +
              ", try help..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO oe-rell.s-code .
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


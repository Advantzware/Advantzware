&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est\d-copyblank.w
  
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
DEF INPUT PARAM ip-eb-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-ef-rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF BUFFER b-eb FOR eb.
DEF BUFFER b2-eb FOR eb.
DEF BUFFER b-ef FOR ef.
DEF BUFFER xeb FOR eb.
DEF BUFFER xef FOR ef.
DEF BUFFER xest FOR est.
DEF BUFFER bx-eb FOR eb.
DEF BUFFER bx-ef FOR ef.

def new shared var cocode as cha no-undo.
def new shared var locode as cha no-undo.

def TEMP-TABLE w-box-h NO-UNDO like box-design-hdr.
def TEMP-TABLE w-box-l NO-UNDO like box-design-line.

{custom/globdefs.i}

cocode = g_company.
       
DO TRANSACTION:
   {sys/inc/cestyle.i F}
END.

{cec/descalc.i new}

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
&Scoped-Define ENABLED-OBJECTS RECT-26 v-copy-sheet-from v-copy-blank-from ~
v-copy-sheet-to v-copy-blank-to tb_die tb_cad Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS v-copy-sheet-from v-copy-blank-from ~
v-copy-sheet-to v-copy-blank-to tb_die tb_cad 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE scr-box-text AS CHARACTER FORMAT "X(256)":U INITIAL "Recalc Box Design?" 
      VIEW-AS TEXT 
     SIZE 28 BY .91
     BGCOLOR 13  NO-UNDO.

DEFINE VARIABLE v-copy-blank-from AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE v-copy-blank-to AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 10  NO-UNDO.

DEFINE VARIABLE v-copy-sheet-from AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE v-copy-sheet-to AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 10  NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 7.38.

DEFINE VARIABLE tb_box-design AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY 1
     BGCOLOR 13  NO-UNDO.

DEFINE VARIABLE tb_cad AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY 1
     BGCOLOR 13  NO-UNDO.

DEFINE VARIABLE tb_die AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY 1
     BGCOLOR 13  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     v-copy-sheet-from AT ROW 5.05 COL 38.2 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     v-copy-blank-from AT ROW 5.05 COL 54 COLON-ALIGNED NO-LABEL
     v-copy-sheet-to AT ROW 6.24 COL 38 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     v-copy-blank-to AT ROW 6.24 COL 54 COLON-ALIGNED NO-LABEL
     tb_die AT ROW 7.67 COL 47
     tb_cad AT ROW 8.62 COL 47
     tb_box-design AT ROW 9.57 COL 47 WIDGET-ID 10
     Btn_OK AT ROW 12.19 COL 16
     Btn_Cancel AT ROW 12.19 COL 46
     scr-box-text AT ROW 9.67 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     "Copy From Sheet:" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 5.05 COL 19
          BGCOLOR 14 
     "Copy Die#?" VIEW-AS TEXT
          SIZE 28 BY 1 AT ROW 7.67 COL 19
          BGCOLOR 13 
     "Currently displayed blank info will be copied to the selected range" VIEW-AS TEXT
          SIZE 75 BY 1.19 AT ROW 1.95 COL 3
          FONT 6
     "Blank:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 6.24 COL 48 WIDGET-ID 18
          BGCOLOR 10 
     "Blank:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 5.05 COL 48 WIDGET-ID 16
          BGCOLOR 14 
     "Copy Cad#?" VIEW-AS TEXT
          SIZE 28 BY 1 AT ROW 8.67 COL 19 WIDGET-ID 2
          BGCOLOR 13 
     "Through Sheet:" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 6.24 COL 19
          BGCOLOR 10 
     RECT-26 AT ROW 4.33 COL 11.4
     SPACE(8.79) SKIP(2.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Estimate Layout Copy"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN scr-box-text IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       scr-box-text:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_box-design IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tb_box-design:HIDDEN IN FRAME D-Dialog           = TRUE.

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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Estimate Layout Copy */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
   DEF VAR lv-box-des AS CHAR NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.
   DEF VAR v-style-same AS LOG NO-UNDO.
   DEF VAR li AS INT NO-UNDO.
   DEF VAR v-log AS LOG NO-UNDO.
   
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN v-copy-sheet-from v-copy-blank-from
             v-copy-sheet-to v-copy-blank-to
             tb_cad tb_die tb_box-design.
   END.

   IF v-copy-blank-to EQ 0 OR
      NOT CAN-FIND(FIRST bx-eb WHERE
      bx-eb.company = b-eb.company AND
      bx-eb.est-no = b-eb.est-no AND
      bx-eb.eqty = b-eb.eqty AND
      bx-eb.blank-no = v-copy-blank-to) THEN
      DO:
         MESSAGE "Invalid Blank#. Try Help. " VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO v-copy-blank-to.
         RETURN NO-APPLY.
      END.
 
   MESSAGE "Are you sure you want to Copy Blank Info? " VIEW-AS ALERT-BOX WARNING
         BUTTON YES-NO UPDATE ll-ans AS LOG.

   IF ll-ans THEN DO:
       
      SESSION:SET-WAIT-STATE("GENERAL").

      FOR EACH bx-ef WHERE
          bx-ef.company EQ b-ef.company AND
          bx-ef.est-no EQ b-ef.est-no AND
          bx-ef.eqty EQ b-ef.eqty AND
          bx-ef.form-no >= v-copy-sheet-from AND
          bx-ef.form-no <= v-copy-sheet-to AND
          bx-ef.form-no NE 0,
          EACH bx-eb WHERE
               bx-eb.company = b-eb.company AND
               bx-eb.est-no = b-eb.est-no AND
               bx-eb.eqty EQ b-eb.eqty AND
               bx-eb.form-no EQ bx-ef.form-no AND
               bx-eb.blank-no NE 0:

          IF v-copy-sheet-from = bx-eb.form-no AND
             v-copy-blank-from = bx-eb.blank-no THEN
             NEXT.

          ASSIGN
             v-log = NO.

          RUN greater-proc(INPUT v-copy-sheet-from,
                           INPUT v-copy-blank-from,
                           INPUT bx-eb.form-no,
                           INPUT bx-eb.blank-no,
                           OUTPUT v-log).

          IF NOT v-log THEN
             NEXT.

          RUN less-proc(INPUT v-copy-sheet-to,
                        INPUT v-copy-blank-to,
                        INPUT bx-eb.form-no,
                        INPUT bx-eb.blank-no,
                        OUTPUT v-log).

          IF NOT v-log THEN
             NEXT.

          IF tb_die THEN
             bx-eb.die-no = b-eb.die-no.

          IF tb_cad THEN
             bx-eb.cad-no = b-eb.cad-no.

          ASSIGN
              lv-box-des = "S"
              v-style-same = bx-eb.style = b-eb.style
              bx-eb.spc-no = b-eb.spc-no
              bx-eb.upc-no = b-eb.upc-no
              bx-eb.style = b-eb.style
              bx-eb.len = b-eb.len
              bx-eb.wid = b-eb.wid
              bx-eb.dep = b-eb.dep
              bx-eb.adhesive = b-eb.adhesive
              bx-eb.dust = b-eb.dust
              bx-eb.fpanel = b-eb.fpanel
              bx-eb.lock = b-eb.lock
              bx-eb.gluelap = b-eb.gluelap
              bx-eb.k-len = b-eb.k-len
              bx-eb.k-wid = b-eb.k-wid
              bx-eb.tuck = b-eb.tuck
              bx-eb.lin-in = b-eb.lin-in
              bx-eb.t-wid = b-eb.t-wid
              bx-eb.t-len = b-eb.t-len
              bx-eb.t-sqin = b-eb.t-sqin
              bx-ef.cad-image = b-ef.cad-image.

          DO li = 1 TO 20:
             ASSIGN
                bx-eb.k-wid-array[li] = b-eb.k-wid-array[li]
                bx-eb.k-wid-scr-type[li] = b-eb.k-wid-scr-type[li]
                bx-eb.k-len-array[li] = b-eb.k-len-array[li]
                bx-eb.k-len-scr-type[li] = b-eb.k-len-scr-type[li].
          END.

          RUN est/u2kinc1.p (RECID(bx-eb)).
          RUN est/u2kinc2.p (RECID(bx-eb)).

          IF NOT v-style-same AND cestyle-log THEN
          DO:
             IF tb_box-design THEN
                lv-box-des = "B".
             ELSE
                lv-box-des = "N".
          END.

          RUN build-box (lv-box-des).
      END. /* for each */

      SESSION:SET-WAIT-STATE("").

      MESSAGE "Blank Information Copied."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-copy-blank-from
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-copy-blank-from D-Dialog
ON HELP OF v-copy-blank-from IN FRAME D-Dialog
DO:
    DEF VAR char-val AS cha NO-UNDO.
    RUN windows/l-estbk.w (ROWID(b-eb),OUTPUT char-val).
    IF char-val <> "" THEN DO:
       FOCUS:SCREEN-VALUE = char-val.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-copy-blank-from D-Dialog
ON LEAVE OF v-copy-blank-from IN FRAME D-Dialog
DO:
   IF LASTKEY <> -1 AND
      (INT(v-copy-blank-from:SCREEN-VALUE) EQ 0 OR
       NOT CAN-FIND(FIRST eb WHERE
           eb.company = b-eb.company AND
           eb.est-no = b-eb.est-no AND
           eb.eqty = b-eb.eqty AND
           eb.blank-no = INT(v-copy-blank-from:SCREEN-VALUE))) THEN
       DO:
          MESSAGE "Invalid Blank#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-copy-blank-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-copy-blank-to D-Dialog
ON HELP OF v-copy-blank-to IN FRAME D-Dialog
DO:
    DEF VAR char-val AS cha NO-UNDO.
    RUN windows/l-estfrm.w (RECID(b-eb),OUTPUT char-val).
    IF char-val <> "" THEN DO:
       FOCUS:SCREEN-VALUE = char-val.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-copy-blank-to D-Dialog
ON LEAVE OF v-copy-blank-to IN FRAME D-Dialog
DO:
   IF LASTKEY <> -1 AND
      (INT(v-copy-blank-to:SCREEN-VALUE) EQ 0 OR
       NOT CAN-FIND(FIRST eb WHERE
           eb.company = b-eb.company AND
           eb.est-no = b-eb.est-no AND
           eb.eqty = b-eb.eqty AND
           eb.blank-no = INT(v-copy-blank-to:SCREEN-VALUE))) THEN
       DO:
          MESSAGE "Invalid Blank#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-copy-sheet-from
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-copy-sheet-from D-Dialog
ON HELP OF v-copy-sheet-from IN FRAME D-Dialog
DO:
    DEF VAR char-val AS cha NO-UNDO.
    RUN windows/l-estfrm.w (RECID(b-ef),OUTPUT char-val).
    IF char-val <> "" THEN DO:
       FOCUS:SCREEN-VALUE = char-val.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-copy-sheet-from D-Dialog
ON LEAVE OF v-copy-sheet-from IN FRAME D-Dialog
DO:
   IF LASTKEY <> -1 AND
      (INT(v-copy-sheet-from:SCREEN-VALUE)) EQ 0 OR
       NOT CAN-FIND(FIRST ef WHERE
           ef.company = b-ef.company AND
           ef.est-no = b-ef.est-no AND
           ef.eqty = b-ef.eqty AND
           ef.form-no = INT(v-copy-sheet-from:SCREEN-VALUE)) THEN
           DO:
              MESSAGE "Invalid Form#. Try Help. " VIEW-AS ALERT-BOX ERROR.
              RETURN NO-APPLY.
           END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-copy-sheet-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-copy-sheet-to D-Dialog
ON HELP OF v-copy-sheet-to IN FRAME D-Dialog
DO:
    DEF VAR char-val AS cha NO-UNDO.
    RUN windows/l-estfrm.w (RECID(b-ef),OUTPUT char-val).
    IF char-val <> "" THEN DO:
       FOCUS:SCREEN-VALUE = char-val.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-copy-sheet-to D-Dialog
ON LEAVE OF v-copy-sheet-to IN FRAME D-Dialog
DO:
  IF LASTKEY <> -1 AND
      (INT(v-copy-sheet-from:SCREEN-VALUE)) EQ 0 OR
       NOT CAN-FIND(FIRST ef WHERE ef.company = b-ef.company AND
           ef.est-no = b-ef.est-no AND
           ef.eqty = b-ef.eqty AND
           ef.form-no = INT(v-copy-sheet-to:SCREEN-VALUE)) THEN
       DO:
          MESSAGE "Invalid Form#. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

FIND FIRST b-eb WHERE ROWID(b-eb) EQ ip-eb-rowid NO-LOCK NO-ERROR.
FIND FIRST b-ef WHERE ROWID(b-ef) EQ ip-ef-rowid NO-LOCK NO-ERROR.

IF cestyle-log THEN
DO:
   ASSIGN
      scr-box-text:HIDDEN = NO
      tb_box-design:HIDDEN = NO
      tb_box-design:SENSITIVE = YES.

   DISPLAY scr-box-text WITH FRAME {&FRAME-NAME}.
END.

IF AVAIL b-eb AND AVAIL b-ef THEN DO:

   ASSIGN
      v-copy-blank-from = b-eb.blank-no
      v-copy-sheet-from = b-eb.form-no
      v-copy-sheet-to   = v-copy-sheet-from.
  
   FOR EACH b2-eb FIELDS(blank-no) WHERE
       b2-eb.company EQ b-eb.company AND
       b2-eb.est-no  EQ b-eb.est-no AND
       b2-eb.eqty EQ b-eb.eqty AND
       b2-eb.blank-no NE 0 AND
       b2-eb.form-no EQ b-eb.form-no
       NO-LOCK
       BY b2-eb.blank-no DESC:
     
       v-copy-blank-to = b2-eb.blank-no.
       LEAVE.
   END.
  
   {src/adm/template/dialogmn.i}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-box D-Dialog 
PROCEDURE build-box :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-box-des AS CHAR NO-UNDO.
   
   find xeb where recid(xeb) = recid(bx-eb) no-lock.

   find first xest where
        xest.company = xeb.company and
        xest.est-no = xeb.est-no
        no-lock.

   find first xef where
        xef.company = xeb.company AND
        xef.est-no   eq xeb.est-no AND
        xef.form-no eq xeb.form-no
        no-lock.

   RUN build-box1(INPUT ip-box-des).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-box1 D-Dialog 
PROCEDURE build-box1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAMETER v-rebuild AS CHAR NO-UNDO.
   
   def buffer xbox-design-hdr  for box-design-hdr.
   def buffer xbox-design-line for box-design-line.

   EMPTY TEMP-TABLE w-box-h.
   EMPTY TEMP-TABLE w-box-l.

   for each box-design-hdr where
       box-design-hdr.design-no = 0 and
       box-design-hdr.company = xeb.company AND
       box-design-hdr.est-no = xeb.est-no AND
       box-design-hdr.form-no   eq xeb.form-no AND
       box-design-hdr.blank-no  eq xeb.blank-no
       no-lock:

       create w-box-h.
       buffer-copy box-design-hdr to w-box-h.

       FOR EACH box-design-line OF box-design-hdr NO-LOCK:
           CREATE w-box-l.
           BUFFER-COPY box-design-line TO w-box-l.
       END.
   end.

   IF v-rebuild NE "N" THEN
   DO:
      {cec/est-6del.i}
   END.

   find first style where
        style.company eq xeb.company AND
        style.style   eq xeb.style
        no-lock no-error.

   if avail style then
      find first xbox-design-hdr where
           xbox-design-hdr.design-no eq style.design-no AND
           xbox-design-hdr.company   eq xeb.company  and
           xbox-design-hdr.est-no    eq ""
           NO-LOCK no-error.

   if avail xbox-design-hdr then do:

      IF v-rebuild NE "N" THEN
      DO:
         run cec/descalc.p (recid(xest), recid(xeb)).
        
         create box-design-hdr.
         assign  box-design-hdr.design-no   = 0
                 box-design-hdr.company = xeb.company
                 box-design-hdr.est-no      = xeb.est-no
                 box-design-hdr.form-no     = xeb.form-no
                 box-design-hdr.blank-no    = xeb.blank-no
                 box-design-hdr.description = if avail xbox-design-hdr then
                                                xbox-design-hdr.description else ""
                 box-design-hdr.lscore      = v-lscore-c
                 box-design-hdr.lcum-score  = v-lcum-score-c
                 box-design-hdr.wscore = xbox-design-hdr.wscore
                 box-design-hdr.wcum-score = xbox-design-hdr.wcum-score
                 box-design-hdr.box-text = xbox-design-hdr.box-text
                 box-design-hdr.box-image = xbox-design-hdr.box-image
                 box-design-hdr.box-3d-image = xbox-design-hdr.box-3d-image.
              
         for each xbox-design-line of xbox-design-hdr no-lock:
            create box-design-line.
            assign box-design-line.design-no  = box-design-hdr.design-no
                   box-design-line.company = box-design-hdr.company
                   box-design-line.est-no      = box-design-hdr.est-no
                   box-design-line.form-no    = box-design-hdr.form-no
                   box-design-line.blank-no   = box-design-hdr.blank-no
                   box-design-line.line-no    = xbox-design-line.line-no
                   box-design-line.line-text  = xbox-design-line.line-text.
        
            find first w-box-design-line
                 where w-box-design-line.line-no eq box-design-line.line-no   no-error.
        
            if avail w-box-design-line then
               assign  box-design-line.wscore     = w-box-design-line.wscore-c
                       box-design-line.wcum-score = w-box-design-line.wcum-score-c.
         end.
      END. /*if v-rebuild ne "N"*/
     
      if v-rebuild ne "B" AND v-rebuild NE "N" then do:
         FIND FIRST w-box-h NO-ERROR.
     
         IF AVAIL w-box-h THEN
         DO:
            if v-rebuild eq "S" then
               ASSIGN box-design-hdr.description = w-box-h.description
                      box-design-hdr.box-image = w-box-h.box-image
                      box-design-hdr.box-3d-image = w-box-h.box-3d-image.
            ELSE
               assign box-design-hdr.lscore      = w-box-h.lscore
                      box-design-hdr.lcum-score  = w-box-h.lcum-score
                      box-design-hdr.wscore      = w-box-h.wscore
                      box-design-hdr.wcum-score  = w-box-h.wcum-score.
           
            for each w-box-l of box-design-hdr,
                first box-design-line of w-box-l:
            
                if v-rebuild eq "S" then
                   assign box-design-line.line-no    = w-box-l.line-no
                          box-design-line.line-text  = w-box-l.line-text.
                ELSE
                   assign box-design-line.wscore     = w-box-l.wscore
                          box-design-line.wcum-score = w-box-l.wcum-score.
            end.
         END. 
      end.
   END.
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
  DISPLAY v-copy-sheet-from v-copy-blank-from v-copy-sheet-to v-copy-blank-to 
          tb_die tb_cad 
      WITH FRAME D-Dialog.
  ENABLE RECT-26 v-copy-sheet-from v-copy-blank-from v-copy-sheet-to 
         v-copy-blank-to tb_die tb_cad Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE greater-proc D-Dialog 
PROCEDURE greater-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-copy-sheet-from AS INT NO-UNDO.
   DEFINE INPUT PARAMETER ip-copy-blank-from AS INT NO-UNDO.
   DEFINE INPUT PARAMETER ip-eb-form-no AS INT NO-UNDO.
   DEFINE INPUT PARAMETER ip-eb-blank-no AS INT NO-UNDO.
   DEFINE OUTPUT PARAMETER op-greater-equal-to AS LOG NO-UNDO.

   IF ip-eb-form-no GT ip-copy-sheet-from OR
      (ip-eb-form-no EQ ip-copy-sheet-from AND
       ip-eb-blank-no GE ip-copy-blank-from) THEN
      op-greater-equal-to = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE less-proc D-Dialog 
PROCEDURE less-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-copy-sheet-to AS INT NO-UNDO.
   DEFINE INPUT PARAMETER ip-copy-blank-to AS INT NO-UNDO.
   DEFINE INPUT PARAMETER ip-eb-form-no AS INT NO-UNDO.
   DEFINE INPUT PARAMETER ip-eb-blank-no AS INT NO-UNDO.
   DEFINE OUTPUT PARAMETER op-less-equal-to AS LOG NO-UNDO.

   IF ip-eb-form-no LT ip-copy-sheet-to OR
      (ip-eb-form-no EQ ip-copy-sheet-to AND
       ip-eb-blank-no LE ip-copy-blank-to) THEN
      op-less-equal-to = YES.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


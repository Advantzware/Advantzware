&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
DEF INPUT PARAM ip-eb-recid AS RECID NO-UNDO.
/* Local Variable Definitions ---                                       */

DEF BUFFER bf-xf FOR ef.
DEF BUFFER b-tef FOR ef.
DEF BUFFER b-fef FOR ef.
DEF BUFFER b-feb FOR eb.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS v-copy-from v-copy-to tb_num-up tb_wax-lbl ~
tb_adder Btn_OK Btn_Cancel RECT-26 
&Scoped-Define DISPLAYED-OBJECTS v-copy-from v-copy-to tb_num-up fi_wax-lab ~
tb_wax-lbl fi_adder tb_adder 

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

DEFINE VARIABLE fi_adder AS CHARACTER FORMAT "X(256)":U INITIAL "Copy Adders?" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 13  NO-UNDO.

DEFINE VARIABLE fi_wax-lab AS CHARACTER FORMAT "X(256)":U INITIAL "Copy Wax/Label?" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 13  NO-UNDO.

DEFINE VARIABLE v-copy-from AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE v-copy-to AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
     BGCOLOR 10  NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 47 BY 6.91.

DEFINE VARIABLE tb_adder AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY 1
     BGCOLOR 13  NO-UNDO.

DEFINE VARIABLE tb_num-up AS LOGICAL INITIAL yes 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY 1
     BGCOLOR 13  NO-UNDO.

DEFINE VARIABLE tb_wax-lbl AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY 1
     BGCOLOR 13  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     v-copy-from AT ROW 5.05 COL 45 COLON-ALIGNED NO-LABEL
     v-copy-to AT ROW 6.24 COL 45 COLON-ALIGNED NO-LABEL
     tb_num-up AT ROW 7.67 COL 47
     fi_wax-lab AT ROW 8.62 COL 23 COLON-ALIGNED NO-LABEL
     tb_wax-lbl AT ROW 8.62 COL 47
     fi_adder AT ROW 9.57 COL 23 COLON-ALIGNED NO-LABEL
     tb_adder AT ROW 9.57 COL 47
     Btn_OK AT ROW 12.19 COL 16
     Btn_Cancel AT ROW 12.19 COL 46
     RECT-26 AT ROW 4.33 COL 15
     "Current displayed form will be copied to the selected range below" VIEW-AS TEXT
          SIZE 75 BY 1.19 AT ROW 1.95 COL 3
          FONT 6
     "Copy To  Form:" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 5.05 COL 26
          BGCOLOR 14 
     "Copy Number Up?" VIEW-AS TEXT
          SIZE 22 BY 1 AT ROW 7.67 COL 25
          BGCOLOR 13 
     "Through Form" VIEW-AS TEXT
          SIZE 21 BY 1 AT ROW 6.24 COL 26
          BGCOLOR 10 
     SPACE(33.19) SKIP(6.94)
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
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_adder IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_wax-lab IN FRAME D-Dialog
   NO-ENABLE                                                            */
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
     DEF BUFFER bx-eb FOR eb.
     DEF BUFFER xest-flm FOR est-flm.

     DEF VAR li AS INT NO-UNDO.


     DO WITH FRAME {&FRAME-NAME}:
       ASSIGN {&displayed-objects}.
     END.

     FIND FIRST b-fef WHERE b-fef.company = bf-xf.company
                      AND b-fef.est-no = bf-xf.est-no
                      AND b-fef.form-no = INT(v-copy-from:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF NOT AVAIL b-fef THEN DO:
        MESSAGE "Invalid Form#. Try Help. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO v-copy-from.
        RETURN NO-APPLY.
     END.

     FIND FIRST b-tef WHERE b-tef.company = bf-xf.company
                      AND b-tef.est-no = bf-xf.est-no
                      AND b-tef.form-no = INT(v-copy-to:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF NOT AVAIL b-tef THEN DO:
        MESSAGE "Invalid Form#. Try Help. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO v-copy-to.
        RETURN NO-APPLY.
     END.

     ASSIGN v-copy-from v-copy-to tb_num-up.
/*
     if v-copy-from ne v-copy-to and avail b-fef and avail b-tef then do:
*/     
        MESSAGE "Are you sure you want to Copy? " VIEW-AS ALERT-BOX WARNING
              BUTTON YES-NO UPDATE ll-ans AS LOG.
        IF ll-ans THEN DO:
            FIND b-fef WHERE RECID(b-fef) = RECID(bf-xf) NO-LOCK NO-ERROR.
            FIND b-feb WHERE RECID(b-feb) = ip-eb-recid NO-LOCK .
            FOR EACH b-tef WHERE b-tef.company = bf-xf.company
                             AND b-tef.est-no = bf-xf.est-no
                             AND b-tef.form-no >= v-copy-from
                             AND b-tef.form-no <= v-copy-to 
                             AND ROWID(b-tef) NE ROWID(b-fef):
                ASSIGN b-tef.m-code   = b-fef.m-code
                       b-tef.m-dscr   = b-fef.m-dscr
                       b-tef.lsh-wid  = b-fef.lsh-wid
                       b-tef.lsh-len  = b-fef.lsh-len
                       b-tef.lam-dscr = b-fef.lam-dscr
                       b-tef.xgrain   = b-fef.xgrain
                       b-tef.board    = b-fef.board
                       b-tef.brd-dscr = b-fef.brd-dscr
                       b-tef.i-code   = b-fef.i-code
                       b-tef.cal      = b-fef.cal
                       b-tef.weight   = b-fef.weight
                       b-tef.nc       = b-fef.nc
                       b-tef.roll     = b-fef.roll
                       b-tef.roll-w   = b-fef.roll-w
                       b-tef.cost-uom = b-fef.cost-uom
                       b-tef.cost-msh = b-fef.cost-msh
                       b-tef.fr-uom   = b-fef.fr-uom
                       b-tef.fr-msh   = b-fef.fr-msh
                       b-tef.gsh-wid  = b-fef.gsh-wid
                       b-tef.gsh-len  = b-fef.gsh-len
                       b-tef.n-out    = b-fef.n-out                    
                       b-tef.n-cuts   = b-fef.n-cuts
                       b-tef.trim-pen = b-fef.trim-pen
                       b-tef.nsh-len  = b-fef.nsh-len
                       b-tef.nsh-wid  = b-fef.nsh-wid
                       b-tef.trim-l   = b-fef.trim-l
                       b-tef.trim-w   = b-fef.trim-w
                       b-tef.die-in   = b-fef.die-in 
                       b-tef.medium   = b-fef.medium
                       b-tef.lam-code = b-fef.lam-code
                       b-tef.flute    = b-fef.flute
                       b-tef.adh-code = b-fef.adh-code
                       b-tef.adh-sqin = b-fef.adh-sqin
                      /* new in gui */
                      b-tef.n-out-l    = b-fef.n-out-l
                      .
                IF tb_num-up THEN
                FOR EACH bx-eb WHERE bx-eb.company = b-tef.company
                                 AND bx-eb.est-no = b-tef.est-no
                                 AND bx-eb.form-no = b-tef.form-no:
                    FIND FIRST b-feb
                        WHERE b-feb.company  EQ b-fef.company
                          AND b-feb.est-no   EQ b-fef.est-no
                          AND b-feb.form-no  EQ b-fef.form-no
                          AND b-feb.blank-no EQ bx-eb.blank-no
                        NO-LOCK NO-ERROR.
                    IF AVAIL b-feb THEN
                      ASSIGN bx-eb.num-wid = b-feb.num-wid 
                             bx-eb.num-len = b-feb.num-len
                             bx-eb.num-up  = b-feb.num-up.

                END.

                IF tb_adder THEN
                DO li = 1 TO EXTENT(b-tef.adder):
                  b-tef.adder[li] = b-fef.adder[li].
                END.

                IF tb_wax-lbl THEN DO:
                  FOR EACH xest-flm
                      WHERE xest-flm.company EQ b-tef.company
                        AND xest-flm.est-no  EQ b-tef.est-no
                        AND xest-flm.snum    EQ b-tef.form-no
                        AND (xest-flm.bnum EQ 0 OR
                             CAN-FIND(FIRST eb
                                      WHERE eb.company  EQ b-fef.company
                                        AND eb.est-no   EQ b-fef.est-no
                                        AND eb.form-no  EQ b-fef.form-no
                                        AND eb.blank-no EQ xest-flm.bnum)):
                    DELETE xest-flm.
                  END.

                  FOR EACH est-flm NO-LOCK
                      WHERE est-flm.company EQ b-fef.company
                        AND est-flm.est-no  EQ b-fef.est-no
                        AND est-flm.snum    EQ b-fef.form-no
                        AND (est-flm.bnum   EQ 0 OR
                             CAN-FIND(FIRST eb
                                      WHERE eb.company  EQ b-tef.company
                                        AND eb.est-no   EQ b-tef.est-no
                                        AND eb.form-no  EQ b-tef.form-no
                                        AND eb.blank-no EQ est-flm.bnum)):

                    FIND LAST xest-flm
                        WHERE xest-flm.company EQ est.company
                          AND xest-flm.est-no  EQ est.est-no
                        USE-INDEX est-qty NO-LOCK NO-ERROR.
                    li = IF AVAIL xest-flm THEN xest-flm.line ELSE 0.

                    CREATE xest-flm.    
                    BUFFER-COPY est-flm EXCEPT rec_key TO xest-flm
                    ASSIGN
                     xest-flm.line = li + 1
                     xest-flm.snum = b-tef.form-no.
                  END.
                END.
            END. /* for each */
        END.
     /*END.            */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-copy-from
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-copy-from D-Dialog
ON HELP OF v-copy-from IN FRAME D-Dialog
DO:
    DEF VAR char-val AS cha NO-UNDO.
    RUN windows/l-estfrm.w (RECID(bf-xf),OUTPUT char-val).
    IF char-val <> "" THEN DO:
       FOCUS:SCREEN-VALUE = char-val.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-copy-from D-Dialog
ON LEAVE OF v-copy-from IN FRAME D-Dialog
DO:
    IF LASTKEY <> -1 THEN DO:
        FIND FIRST b-fef WHERE b-fef.company = bf-xf.company
                      AND b-fef.est-no = bf-xf.est-no
                      AND b-fef.eqty = bf-xf.eqty
                      AND b-fef.form-no = INT(v-copy-from:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF NOT AVAIL b-fef THEN DO:
        MESSAGE "Invalid Form#. Try Help. " VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
     END.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME v-copy-to
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-copy-to D-Dialog
ON HELP OF v-copy-to IN FRAME D-Dialog
DO:
    DEF VAR char-val AS cha NO-UNDO.
    RUN windows/l-estfrm.w (RECID(bf-xf),OUTPUT char-val).
    IF char-val <> "" THEN DO:
       FOCUS:SCREEN-VALUE = char-val.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL v-copy-to D-Dialog
ON LEAVE OF v-copy-to IN FRAME D-Dialog
DO:
  IF LASTKEY <> -1 THEN DO:
        FIND FIRST b-tef WHERE b-tef.company = bf-xf.company
                      AND b-tef.est-no = bf-xf.est-no
                      AND b-tef.eqty = bf-xf.eqty
                      AND b-tef.form-no = INT(v-copy-to:SCREEN-VALUE) NO-LOCK NO-ERROR.
     IF NOT AVAIL b-tef THEN DO:
        MESSAGE "Invalid Form#. Try Help. " VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
     END.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
DEF VAR ll-tandem AS LOG NO-UNDO.


FIND FIRST bf-xf WHERE recid(bf-xf) EQ ip-recid NO-LOCK NO-ERROR.

IF AVAIL bf-xf THEN DO:
  FIND FIRST est OF bf-xf NO-LOCK NO-ERROR.

  IF AVAIL est AND (est.est-type EQ 4 OR est.est-type EQ 8) THEN
    RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-tandem).

  ASSIGN
   v-copy-from = bf-xf.form-no
   tb_num-up   = NOT AVAIL est OR ll-tandem OR
                 (est.est-type NE 4 AND est.est-type EQ 8).

  FOR EACH b-tef
      WHERE b-tef.company EQ bf-xf.company
        AND b-tef.est-no  EQ bf-xf.est-no
      NO-LOCK
      BY b-tef.form-no DESC:
    v-copy-to = b-tef.form-no.
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
  DISPLAY v-copy-from v-copy-to tb_num-up fi_wax-lab tb_wax-lbl fi_adder 
          tb_adder 
      WITH FRAME D-Dialog.
  ENABLE v-copy-from v-copy-to tb_num-up tb_wax-lbl tb_adder Btn_OK Btn_Cancel 
         RECT-26 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    IF bf-xf.est-type LE 4 THEN
      ASSIGN
       fi_adder:HIDDEN = YES
       tb_adder:HIDDEN = YES
       fi_wax-lab:SCREEN-VALUE = "Copy Leaf/Film?".
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


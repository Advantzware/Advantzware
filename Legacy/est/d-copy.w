&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: est/d-copy.w
  
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
DEF INPUT PARAM ip-add-what AS CHAR NO-UNDO.
DEF INPUT PARAM ip-recid-ef AS RECID NO-UNDO.
DEF INPUT PARAM ip-recid-eb AS RECID NO-UNDO.
DEF PARAM BUFFER io-ef FOR ef.
DEF PARAM BUFFER io-eb FOR eb.
DEF OUTPUT PARAM op-error AS LOG INIT YES NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
{sys/inc/varasgn.i}

DEF VAR v-prgmname LIKE prgrms.prgmname NO-UNDO.
DEF VAR period_pos AS INTEGER NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
   INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
   INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
v-prgmname = USERID("NOSWEAT") + "..".
ELSE
ASSIGN
  period_pos = INDEX(PROGRAM-NAME(1),".")
  v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
  v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).

DO TRANSACTION:
  {ce/cecopy.i}
END.

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
&Scoped-Define ENABLED-OBJECTS RECT-26 tb_die tb_image tb_cad tb_layout ~
tb_inks tb_case tb_pallet tb_freight tb_design Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_die tb_image tb_cad tb_layout tb_inks ~
tb_case tb_pallet tb_freight tb_design 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 tb_die tb_image tb_cad tb_layout tb_inks tb_case ~
tb_pallet tb_freight tb_design 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-GO 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 10.71.

DEFINE VARIABLE tb_cad AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_case AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_design AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY .95 NO-UNDO.

DEFINE VARIABLE tb_die AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_freight AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_image AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_inks AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_layout AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE tb_pallet AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 4 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     tb_die AT ROW 2.19 COL 23
     tb_image AT ROW 3.19 COL 23
     tb_cad AT ROW 4.19 COL 23
     tb_layout AT ROW 5.19 COL 23
     tb_inks AT ROW 6.19 COL 23
     tb_case AT ROW 7.19 COL 23
     tb_pallet AT ROW 8.19 COL 23
     tb_freight AT ROW 9.19 COL 23
     tb_design AT ROW 10.19 COL 23
     Btn_OK AT ROW 11.95 COL 2
     Btn_Cancel AT ROW 11.95 COL 18
     "Box Design?" VIEW-AS TEXT
          SIZE 15 BY 1 AT ROW 10.19 COL 7
     "Die#?" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 2.19 COL 14
     "Image?" VIEW-AS TEXT
          SIZE 9 BY 1 AT ROW 3.19 COL 13
     "Inks?" VIEW-AS TEXT
          SIZE 7 BY 1 AT ROW 6.19 COL 15
     "Copy..." VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 1.24 COL 3
     "Pallet?" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 8.19 COL 13
     "CAD#?" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 4.19 COL 13
     "Layout?" VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 5.19 COL 12
     "Case?" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 7.19 COL 13
     "Freight?" VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 9.19 COL 12
     RECT-26 AT ROW 1 COL 1
     SPACE(0.00) SKIP(1.71)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Form/Blank Copy"
         DEFAULT-BUTTON Btn_OK.


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

/* SETTINGS FOR TOGGLE-BOX tb_cad IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_case IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_design IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_die IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_freight IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_image IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_inks IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_layout IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX tb_pallet IN FRAME D-Dialog
   1                                                                    */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Form/Blank Copy */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */

  APPLY "CHOOSE" TO Btn_Cancel IN FRAME {&FRAME-NAME}.

  /*APPLY "END-ERROR":U TO SELF.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  RUN do-copy(INPUT NO).

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&displayed-objects}.
  END.

  RUN do-copy(INPUT YES).

  RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
DEF VAR ll AS LOG NO-UNDO.


FIND eb WHERE RECID(eb) EQ ip-recid-eb NO-LOCK NO-ERROR.

IF AVAIL eb AND AVAIL io-eb THEN DO:
  IF cecopy-int EQ 3                        OR
     (cecopy-int EQ 1 AND eb.est-type LE 4) OR
     (cecopy-int EQ 2 AND eb.est-type GE 5) THEN DO:

    ASSIGN
     tb_die     = YES
     tb_image   = YES
     tb_cad     = YES
     tb_layout  = YES
     tb_inks    = YES
     tb_case    = YES
     tb_pallet  = YES
     tb_freight = YES
     tb_design  = YES.

    RUN do-copy(INPUT YES).
  END.

  ELSE DO:
    {src/adm/template/dialogmn.i}
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE do-copy D-Dialog 
PROCEDURE do-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-ok AS LOG NO-UNDO.
  
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR li AS INT NO-UNDO.

  DEF BUFFER est-misc FOR reftable.
  DEF BUFFER b-est-flm FOR est-flm.
  DEF BUFFER b-box-h FOR box-design-hdr.
  DEF BUFFER b-box-l FOR box-design-line.
  DEF BUFFER unit-ref FOR reftable.
  DEF BUFFER depth-ref FOR reftable.
  DEF VAR v-str AS CHAR NO-UNDO.
  DEF VAR v-side-count AS INT NO-UNDO.

  {est/layout.i}


  FIND CURRENT io-eb NO-ERROR.

  IF AVAIL io-eb THEN DO:
    BUFFER-COPY eb EXCEPT form-no blank-no die-no cad-no
                          i-col i-pass i-coat i-coat-p i-coldscr
                          i-ps i-code i-dscr i-%
                          i-ps2 i-code2 i-dscr2 i-%2
                          eb.cas-no eb.cas-len eb.cas-cost eb.cas-wid
                          eb.cas-cnt eb.cas-dep eb.cas-pal eb.layer-pad
                          eb.lp-len eb.lp-wid eb.lp-up eb.divider
                          eb.div-len eb.div-wid eb.div-up eb.cas-wt
                          eb.tr-no eb.tr-len eb.tr-cost eb.tr-wid eb.tr-cnt
                          eb.tr-dep eb.tr-cas eb.stacks eb.stack-code
                          eb.chg-method eb.weight-m eb.carrier eb.carr-dscr
                          eb.dest-code eb.fr-out-c eb.fr-out-m
                          eb.spare-char-3 eb.spare-char-4 eb.spare-int-3
                          TO io-eb.

    IF ip-ok THEN
    DO:
       IF tb_die THEN io-eb.die-no = eb.die-no.
      
       IF tb_cad THEN io-eb.cad-no = eb.cad-no.
      
       IF tb_inks THEN DO:
         ASSIGN
          io-eb.i-col     = eb.i-col
          io-eb.i-pass    = eb.i-pass
          io-eb.i-coat    = eb.i-coat
          io-eb.i-coat-p  = eb.i-coat-p
          io-eb.i-coldscr = eb.i-coldscr.
      
         DO li = 1 TO EXTENT(eb.i-ps):
           ASSIGN
            io-eb.i-ps[li]   = eb.i-ps[li]
            io-eb.i-code[li] = eb.i-code[li]
            io-eb.i-dscr[li] = eb.i-dscr[li]
            io-eb.i-%[li]    = eb.i-%[li].
         END.
         DO li = 1 TO EXTENT(eb.i-ps2):
           ASSIGN
            io-eb.i-ps2[li]   = eb.i-ps2[li]
            io-eb.i-code2[li] = eb.i-code2[li]
            io-eb.i-dscr2[li] = eb.i-dscr2[li]
            io-eb.i-%2[li]    = eb.i-%2[li].
         END.
         
               {ce/updunit#.i io-eb}               
         
       END.
      
       IF tb_case THEN
       DO:
         ASSIGN
          io-eb.cas-no    = eb.cas-no
          io-eb.cas-len   = eb.cas-len
          io-eb.cas-cost  = eb.cas-cost
          io-eb.cas-wid   = eb.cas-wid
          io-eb.cas-cnt   = eb.cas-cnt
          io-eb.cas-dep   = eb.cas-dep
          io-eb.cas-pal   = eb.cas-pal
          io-eb.layer-pad = eb.layer-pad
          io-eb.lp-len    = eb.lp-len
          io-eb.lp-wid    = eb.lp-wid
          io-eb.lp-up     = eb.lp-up
          io-eb.spare-char-3 = eb.spare-char-3
          io-eb.spare-int-3 = eb.spare-int-3
          io-eb.divider   = eb.divider
          io-eb.div-len   = eb.div-len
          io-eb.div-wid   = eb.div-wid
          io-eb.div-up    = eb.div-up
          io-eb.spare-char-4 = eb.spare-char-4
          io-eb.cas-wt    = eb.cas-wt.

         IF eb.est-type LT 5 THEN
         DO:
            FIND FIRST reftable WHERE
                 reftable.reftable EQ "cedepth" AND
                 reftable.company  EQ eb.company AND
                 reftable.loc      EQ eb.est-no AND
                 reftable.code     EQ STRING(eb.form-no,"9999999999") AND
                 reftable.code2    EQ STRING(eb.blank-no,"9999999999")
                 NO-LOCK NO-ERROR.

            IF AVAIL reftable THEN
            DO:
               FIND FIRST depth-ref WHERE
                    depth-ref.reftable EQ "cedepth" AND
                    depth-ref.company EQ eb.company AND
                    depth-ref.loc EQ io-eb.est-no AND
                    depth-ref.CODE = STRING(io-eb.form-no,"9999999999") AND
                    depth-ref.code2 = STRING(io-eb.blank-no,"9999999999")
                    NO-ERROR.

               IF NOT AVAIL depth-ref THEN
                  CREATE depth-ref.

               BUFFER-COPY reftable EXCEPT rec_key loc CODE code2 TO depth-ref
                  ASSIGN
                     depth-ref.loc = io-eb.est-no
                     depth-ref.CODE = STRING(io-eb.form-no,"9999999999")
                     depth-ref.code2 = STRING(io-eb.blank-no,"9999999999").
               RELEASE depth-ref.
            END.
         END.
       END.
      
       IF tb_pallet THEN
         ASSIGN
          io-eb.tr-no      = eb.tr-no
          io-eb.tr-len     = eb.tr-len
          io-eb.tr-cost    = eb.tr-cost
          io-eb.tr-wid     = eb.tr-wid
          io-eb.tr-cnt     = eb.tr-cnt
          io-eb.tr-dep     = eb.tr-dep
          io-eb.tr-cas     = eb.tr-cas
          io-eb.stacks     = eb.stacks
          io-eb.stack-code = eb.stack-code.
      
       IF tb_freight THEN
         ASSIGN
          io-eb.chg-method = eb.chg-method
          io-eb.weight-m   = eb.weight-m
          io-eb.carrier    = eb.carrier
          io-eb.carr-dscr  = eb.carr-dscr
          io-eb.dest-code  = eb.dest-code
          io-eb.fr-out-c   = eb.fr-out-c
          io-eb.fr-out-m   = eb.fr-out-m.
      
       IF tb_design THEN
       FOR EACH box-design-hdr NO-LOCK
           WHERE box-design-hdr.design-no EQ 0
             AND box-design-hdr.company   EQ eb.company
             AND box-design-hdr.est-no    EQ eb.est-no
             AND box-design-hdr.form-no   EQ eb.form-no
             AND box-design-hdr.blank-no  EQ eb.blank-no:
         CREATE b-box-h.
         BUFFER-COPY box-design-hdr EXCEPT rec_key form-no blank-no TO b-box-h
         ASSIGN
          b-box-h.form-no  = io-eb.form-no
          b-box-h.blank-no = io-eb.blank-no.
      
         FOR EACH box-design-line NO-LOCK
             WHERE box-design-line.design-no EQ 0
               AND box-design-line.company   EQ eb.company
               AND box-design-line.est-no    EQ eb.est-no
               AND box-design-line.form-no   EQ eb.form-no
               AND box-design-line.blank-no  EQ eb.blank-no:
           CREATE b-box-l.
           BUFFER-COPY box-design-line EXCEPT rec_key form-no blank-no TO b-box-l
           ASSIGN
            b-box-l.form-no  = io-eb.form-no
            b-box-l.blank-no = io-eb.blank-no.
         END.
       END.
    END.
    
    IF ip-add-what EQ "form" AND AVAIL io-ef THEN DO:
      FIND ef WHERE RECID(ef) EQ ip-recid-ef NO-LOCK.
      FIND CURRENT io-ef.

      BUFFER-COPY ef EXCEPT rec_key form-no xgrain board brd-dscr i-code flute test cost-uom cost-msh weight fr-uom fr-msh nc gsh-wid gsh-len gsh-dep nsh-wid nsh-len nsh-dep trim-w trim-l trim-d n-out n-out-l n-out-d n-cuts die-in adder leaf leaf-dscr leaf-snum leaf-bnum leaf-w leaf-l cad-image TO io-ef
      ASSIGN
       io-ef.leaf = "".

      IF ip-ok THEN
      DO:
         IF tb_layout THEN BUFFER-COPY ef USING xgrain board brd-dscr i-code flute test cost-uom cost-msh weight fr-uom fr-msh nc gsh-wid gsh-len gsh-dep nsh-wid nsh-len nsh-dep trim-w trim-l trim-d n-out n-out-l n-out-d n-cuts die-in adder leaf leaf-dscr leaf-snum leaf-bnum leaf-w leaf-l TO io-ef.
         IF tb_image THEN io-ef.cad-image = ef.cad-image.
      END.

      FOR EACH reftable
          WHERE reftable.reftable EQ "EST-MISC"
            AND reftable.company  EQ ef.company
            AND reftable.loc      EQ ef.loc
            AND reftable.code     EQ TRIM(ef.est-no) + STRING(ef.form-no,"/99"):
        CREATE est-misc.
        BUFFER-COPY reftable EXCEPT rec_key TO est-misc
        ASSIGN
         est-misc.code = TRIM(ef.est-no) + STRING(ef.form-no,"/99").
      END.
    END.

    FOR EACH est-flm NO-LOCK
        WHERE est-flm.company EQ eb.company
          AND est-flm.est-no  EQ eb.est-no
          AND est-flm.snum    EQ eb.form-no
          AND est-flm.bnum    EQ eb.blank-no:

      FIND LAST b-est-flm
          WHERE b-est-flm.company EQ io-eb.company
            AND b-est-flm.est-no  EQ io-eb.est-no
          USE-INDEX est-qty NO-LOCK NO-ERROR.
      li = IF AVAIL b-est-flm THEN b-est-flm.line ELSE 0.

      CREATE b-est-flm.    
      BUFFER-COPY est-flm EXCEPT rec_key TO b-est-flm
      ASSIGN
       b-est-flm.e-num  = io-eb.e-num
       b-est-flm.est-no = io-eb.est-no
       b-est-flm.eqty   = io-eb.eqty
       b-est-flm.line   = li + 1
       b-est-flm.snum   = io-eb.form-no
       b-est-flm.bnum   = io-eb.blank-no.
    END.

    op-error = NO.
  END.

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
  DISPLAY tb_die tb_image tb_cad tb_layout tb_inks tb_case tb_pallet tb_freight 
          tb_design 
      WITH FRAME D-Dialog.
  ENABLE RECT-26 tb_die tb_image tb_cad tb_layout tb_inks tb_case tb_pallet 
         tb_freight tb_design Btn_OK Btn_Cancel 
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
    {custom/usrprint.i}
    IF ip-add-what NE "form" THEN DISABLE tb_image tb_layout.
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


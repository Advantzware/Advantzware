&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF INPUT PARAMETER ip-company AS cha NO-UNDO.
DEF INPUT PARAMETER ip-est-no AS cha NO-UNDO.
DEF INPUT PARAMETER ip-eqty AS INT NO-UNDO.

DEF BUFFER bf-flm FOR est-flm.

DEF VAR ll-new-record AS LOG NO-UNDO.
DEF VAR ll-copy-record AS LOG NO-UNDO.
DEF VAR lv-new-recid AS RECID NO-UNDO.  /* new est-flm */
DEF VAR lv-flm-recid AS RECID NO-UNDO.  /* current est-flm */
DEF VAR ll-update-record AS LOG NO-UNDO.
DEF VAR lv-ind AS cha NO-UNDO.

DEF TEMP-TABLE tt-est-flm LIKE est-flm.

{est/d-selblk.i NEW}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES est-flm

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 est-flm.i-no est-flm.dscr ~
est-flm.snum est-flm.bnum est-flm.len est-flm.wid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 est-flm.i-no est-flm.dscr ~
est-flm.snum est-flm.bnum est-flm.len est-flm.wid 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 est-flm
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 est-flm
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH est-flm ~
      WHERE est-flm.company = ip-company and ~
est-flm.est-no = ip-est-no /*and ~
est-flm.eqty = ip-eqty*/ NO-LOCK ~
    BY est-flm.snum ~
       BY est-flm.bnum ~
        BY est-flm.line INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH est-flm ~
      WHERE est-flm.company = ip-company and ~
est-flm.est-no = ip-est-no /*and ~
est-flm.eqty = ip-eqty*/ NO-LOCK ~
    BY est-flm.snum ~
       BY est-flm.bnum ~
        BY est-flm.line INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 est-flm
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 est-flm


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 btn-add btn-update btn-delete ~
btn-copy Btn-Cancel Btn-OK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-add 
     LABEL "&Add" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn-Cancel 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btn-copy 
     LABEL "&Copy" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-delete 
     LABEL "&Delete" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn-OK AUTO-GO 
     LABEL "C&lose" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btn-update 
     LABEL "&Update" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      est-flm SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      est-flm.i-no FORMAT "x(25)":U
      est-flm.dscr FORMAT "x(30)":U
      est-flm.snum FORMAT ">>>":U
      est-flm.bnum FORMAT ">>>":U
      est-flm.len FORMAT ">9.9999":U
      est-flm.wid FORMAT ">9.9999":U WIDTH 7.8
  ENABLE
      est-flm.i-no
      est-flm.dscr
      est-flm.snum
      est-flm.bnum
      est-flm.len
      est-flm.wid
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 102 BY 9.05
         BGCOLOR 8  EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     btn-add AT ROW 10.52 COL 3
     btn-update AT ROW 10.52 COL 19
     btn-delete AT ROW 10.52 COL 35
     btn-copy AT ROW 10.52 COL 51
     Btn-Cancel AT ROW 10.52 COL 67
     Btn-OK AT ROW 10.52 COL 86
     SPACE(2.79) SKIP(0.19)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Window Film / Foil"
         DEFAULT-BUTTON Btn-OK CANCEL-BUTTON Btn-Cancel.


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
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "ASI.est-flm"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", FIRST"
     _OrdList          = "ASI.est-flm.snum|yes,ASI.est-flm.bnum|yes,ASI.est-flm.line|yes"
     _Where[1]         = "ASI.est-flm.company = ip-company and
est-flm.est-no = ip-est-no /*and
est-flm.eqty = ip-eqty*/"
     _FldNameList[1]   > ASI.est-flm.i-no
"est-flm.i-no" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > ASI.est-flm.dscr
"est-flm.dscr" ? "x(30)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > ASI.est-flm.snum
"est-flm.snum" ? ">>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > ASI.est-flm.bnum
"est-flm.bnum" ? ">>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > ASI.est-flm.len
"est-flm.len" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > ASI.est-flm.wid
"est-flm.wid" ? ? "decimal" ? ? ? ? ? ? yes ? no no "7.8" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Window Film / Foil */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON ROW-ENTRY OF BROWSE-1 IN FRAME Dialog-Frame
DO:
    IF NOT ll-update-record  AND NOT ll-new-record
    THEN DO: 

        IF NUM-RESULTS("{&BROWSE-NAME}":U) = ? OR  /* query not opened */
         NUM-RESULTS("{&BROWSE-NAME}":U) = 0 /* query's empty */
         /*OR BROWSE br-flm:NUM-SELECTED-ROWS < 1 */ THEN DO: END.
         ELSE DO:
            BROWSE {&BROWSE-name}:SELECT-FOCUSED-ROW().
            REPOSITION {&browse-name} TO ROWID ROWID(est-flm).
            RETURN NO-APPLY.
         END.
    END.

    ELSE
    IF ll-new-record THEN DO:
       ASSIGN
        est-flm.snum:SCREEN-VALUE IN BROWSE {&browse-name} = "1"
        est-flm.bnum:SCREEN-VALUE IN BROWSE {&browse-name} = "1".

       RUN new-blank-no (2).

       FIND FIRST tt-est-flm NO-ERROR.
       IF AVAIL tt-est-flm THEN DO:
         ASSIGN
          est-flm.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = tt-est-flm.i-no
          est-flm.dscr:SCREEN-VALUE IN BROWSE {&browse-name} = tt-est-flm.dscr
          est-flm.snum:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(tt-est-flm.snum)
          est-flm.bnum:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(tt-est-flm.bnum)
          est-flm.wid:SCREEN-VALUE  IN BROWSE {&browse-name} = STRING(tt-est-flm.wid)
          est-flm.len:SCREEN-VALUE  IN BROWSE {&browse-name} = STRING(tt-est-flm.len).
         DELETE tt-est-flm.
       END.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-flm.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.i-no BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON HELP OF est-flm.i-no IN BROWSE BROWSE-1 /* Item No */
DO:
  DEF VAR char-val AS cha NO-UNDO.
  
  RUN windows/l-item.w (ip-company,lv-ind,"F,W",FOCUS:SCREEN-VALUE,OUTPUT char-val).
  IF char-val NE "" AND FOCUS:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO:
    FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
    RUN new-i-no.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.i-no BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON VALUE-CHANGED OF est-flm.i-no IN BROWSE BROWSE-1 /* Item No */
DO:
  RUN new-i-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-flm.dscr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.dscr BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF est-flm.dscr IN BROWSE BROWSE-1 /* Description */
DO:
    IF NOT ll-update-record  AND NOT ll-new-record
      THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-flm.snum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.snum BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF est-flm.snum IN BROWSE BROWSE-1 /* Sheet # */
DO:
    IF NOT ll-update-record  AND NOT ll-new-record
      THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.snum BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON HELP OF est-flm.snum IN BROWSE BROWSE-1 /* Sheet # */
DO:
    DEF VAR char-val AS cha NO-UNDO.

    /*RUN windows/l-form.w (ip-company,ip-est-no,ip-eqty, OUTPUT char-val).*/
    RUN windows/l-blank.w (ip-company,ip-est-no,ip-eqty, OUTPUT char-val).
    IF char-val <> "" THEN DO:
           ASSIGN FOCUS:SCREEN-VALUE = entry(1,char-val)
                  est-flm.bnum:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(2,char-val).
    END.
    
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.snum BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF est-flm.snum IN BROWSE BROWSE-1 /* Sheet # */
DO:
 
 IF LASTKEY = -1 THEN RETURN.
    
   IF NOT CAN-FIND(FIRST ef WHERE ef.company = ip-company
                    AND ef.est-no = ip-est-no
                    AND ef.form-no = INT(est-flm.snum:SCREEN-VALUE IN BROWSE {&browse-name} )                    
                   )

   THEN DO:
       MESSAGE "Form does not exist on this estimate." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.snum BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON VALUE-CHANGED OF est-flm.snum IN BROWSE BROWSE-1 /* Sheet # */
DO:
  RUN new-blank-no (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-flm.bnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.bnum BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF est-flm.bnum IN BROWSE BROWSE-1 /* Blank # */
DO:
    IF NOT ll-update-record  AND NOT ll-new-record
      THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.bnum BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON HELP OF est-flm.bnum IN BROWSE BROWSE-1 /* Blank # */
DO:
    DEF VAR char-val AS cha NO-UNDO.                /*INT(est-flm.snum:SCREEN-VALUE IN BROWSE {&browse-name})*/
    RUN windows/l-blank.w (ip-company,ip-est-no,ip-eqty, OUTPUT char-val).
    IF char-val <> "" THEN DO:
            ASSIGN FOCUS:SCREEN-VALUE = entry(2,char-val)
                   est-flm.snum:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(1,char-val).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.bnum BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF est-flm.bnum IN BROWSE BROWSE-1 /* Blank # */
DO:
   IF LASTKEY = -1 THEN RETURN.
    
   IF NOT CAN-FIND(FIRST eb WHERE eb.company = ip-company
                    AND eb.est-no = ip-est-no
                    AND eb.form-no = INT(est-flm.snum:SCREEN-VALUE IN BROWSE {&browse-name} )
                    AND (eb.blank-no = INT(est-flm.bnum:SCREEN-VALUE) OR INT(est-flm.bnum:SCREEN-VALUE) EQ 0)
                   )

   THEN DO:
       MESSAGE "Blank does not exist on this estimate." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.bnum BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON VALUE-CHANGED OF est-flm.bnum IN BROWSE BROWSE-1 /* Blank # */
DO:
  RUN new-blank-no (2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-flm.len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.len BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF est-flm.len IN BROWSE BROWSE-1 /* Length */
DO:
    IF NOT ll-update-record  AND NOT ll-new-record
      THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.len BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF est-flm.len IN BROWSE BROWSE-1 /* Length */
DO:
    IF LASTKEY = -1 THEN RETURN.

    IF INT(SELF:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME est-flm.wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.wid BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF est-flm.wid IN BROWSE BROWSE-1 /* Width */
DO:
    IF NOT ll-update-record  AND NOT ll-new-record
      THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL est-flm.wid BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF est-flm.wid IN BROWSE BROWSE-1 /* Width */
DO:
    IF LASTKEY <> -1 THEN
    DO WITH FRAME {&FRAME-NAME} :  
      APPLY "entry" TO btn-update.
      APPLY "choose" TO btn-update .
      RETURN NO-APPLY.
    END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-add Dialog-Frame
ON CHOOSE OF btn-add IN FRAME Dialog-Frame /* Add */
DO:
  ASSIGN ll-new-record = NO
         ll-update-record = NO
         ll-copy-record = NO.
  RUN add-a-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Cancel Dialog-Frame
ON CHOOSE OF Btn-Cancel IN FRAME Dialog-Frame /* Cancel */
DO:
    IF ll-new-record THEN DO:
       /*APPLY "choose" TO btn-delete. */

       BROWSE {&browse-name}:delete-current-row().
    END.
    ELSE
    IF AVAIL est-flm THEN DO:
        DISPLAY est-flm.i-no est-flm.dscr
               est-flm.snum est-flm.bnum
               est-flm.len est-flm.wid WITH BROWSE {&browse-name}.
        RUN make-readonly.
    END.

    btn-update:LABEL = "&Update".
    ENABLE btn-add btn-update btn-delete btn-copy btn-ok WITH FRAME {&FRAME-NAME}.
    DISABLE btn-cancel WITH FRAME {&FRAME-NAME}.

    ASSIGN ll-new-record = NO
           ll-update-record = NO
           ll-copy-record = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-copy Dialog-Frame
ON CHOOSE OF btn-copy IN FRAME Dialog-Frame /* Copy */
DO:
  IF AVAIL est-flm THEN DO WITH FRAME {&FRAME-NAME}:
    ASSIGN ll-new-record = NO
           ll-update-record = NO
           ll-copy-record = YES.
    RUN add-a-record.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-delete Dialog-Frame
ON CHOOSE OF btn-delete IN FRAME Dialog-Frame /* Delete */
DO:
    message "Delete Currently Selected Record?" view-as alert-box question
          button yes-no update ll-ans as log.
    if not ll-ans then return .

    FIND CURRENT est-flm NO-ERROR.
    IF AVAIL est-flm THEN DELETE est-flm.
    BROWSE {&browse-name}:delete-current-row().

    RUN make-readonly.
    ENABLE btn-add btn-update btn-delete btn-copy btn-ok WITH FRAME {&FRAME-NAME}.
    DISABLE btn-cancel.

    ASSIGN ll-new-record = NO
           ll-update-record = NO
           ll-copy-record = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-update Dialog-Frame
ON CHOOSE OF btn-update IN FRAME Dialog-Frame /* Update */
DO:
    DEF VAR li-line AS INT NO-UNDO.
    DEF VAR lv-rowid AS ROWID NO-UNDO.


    IF SELF:LABEL = "&Save" THEN DO:
        /* validate*/    

    IF NOT CAN-FIND(FIRST item
                    WHERE item.company  EQ ip-company
                      AND item.i-no     EQ est-flm.i-no:SCREEN-VALUE in BROWSE {&browse-name}
                      AND item.industry EQ lv-ind
                      AND LOOKUP(item.mat-type,"F,W") GT 0)
    THEN DO:
       MESSAGE "Invalid material, try help..." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO est-flm.i-no.
       RETURN NO-APPLY.
    END.

    IF NOT CAN-FIND(FIRST ef WHERE ef.company = ip-company
                    AND ef.est-no = ip-est-no
                    AND ef.form-no = INT(est-flm.snum:SCREEN-VALUE IN BROWSE {&browse-name} ) )
    THEN DO:
       MESSAGE "Form does not exist on this estimate." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO est-flm.snum.
       RETURN NO-APPLY.
    END.
    IF NOT CAN-FIND(FIRST eb WHERE eb.company = ip-company
                    AND eb.est-no = ip-est-no
                    AND eb.form-no = INT(est-flm.snum:SCREEN-VALUE IN BROWSE {&browse-name} )
                    AND (eb.blank-no = INT(est-flm.bnum:SCREEN-VALUE) OR INT(est-flm.bnum:SCREEN-VALUE) EQ 0) 
                   )
    THEN DO:
        MESSAGE "Blank does not exist on this estimate." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO est-flm.bnum.
        RETURN NO-APPLY.
    END.

    IF INT(est-flm.len:SCREEN-VALUE IN BROWSE {&browse-name}) = 0 THEN DO:
       MESSAGE "Length must be entered." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO est-flm.len.
       RETURN NO-APPLY.
    END.
    IF INT(est-flm.wid:SCREEN-VALUE) = 0 THEN DO:
          MESSAGE "Width must be entered." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO est-flm.wid.
          RETURN NO-APPLY.
    END.
    /* end of validation*/


    IF ll-new-record THEN DO:
       FIND LAST bf-flm WHERE bf-flm.company = ip-company
                     AND bf-flm.est-no = ip-est-no
                     USE-INDEX est-qty NO-LOCK NO-ERROR.
       li-line = IF AVAIL bf-flm THEN bf-flm.LINE + 1 ELSE 1.

       CREATE est-flm.
       ASSIGN est-flm.company = ip-company
              est-flm.est-no = ip-est-no
              est-flm.eqty = ip-eqty
              est-flm.LINE = li-line
              .
        lv-new-recid = RECID(est-flm).      
    END.
    ELSE FIND CURRENT est-flm.

      ASSIGN BROWSE {&browse-name}
              est-flm.i-no
              est-flm.dscr
              est-flm.snum
              est-flm.bnum
              est-flm.len
              est-flm.wid.
      est-flm.i-no = CAPS(est-flm.i-no).
      FIND CURRENT est-flm NO-LOCK.

      ASSIGN
       ll-update-record = NO
       lv-rowid         = ROWID(est-flm).

      {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}

      REPOSITION {&browse-name} TO ROWID lv-rowid NO-ERROR.

      IF CAN-FIND(FIRST tt-est-flm) THEN
        BROWSE {&browse-name}:INSERT-ROW("after").
      ELSE DO:
        btn-update:LABEL = "&Update".
        ENABLE btn-add btn-update btn-delete btn-copy btn-ok WITH FRAME {&FRAME-NAME}.
        DISABLE btn-cancel WITH FRAME {&FRAME-NAME}.
      END.
    END.

    ELSE DO:
        btn-update:LABEL = "&Save".
        /*ENABLE ALL WITH FRAME {&FRAME-NAME}.*/
        RUN make-enable.
        ll-update-record = YES.

        DISABLE btn-add btn-delete btn-copy btn-OK WITH FRAME {&FRAME-NAME}.
        ENABLE btn-cancel WITH FRAME {&FRAME-NAME}.
        lv-flm-recid = RECID(est-flm).
        APPLY "entry" TO est-flm.i-no.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND FIRST est
      WHERE est.company EQ ip-company
        AND est.est-no  EQ ip-est-no
      NO-LOCK NO-ERROR.

  IF AVAIL est THEN DO WITH FRAME {&FRAME-NAME}:
    IF est.est-type LE 4 THEN lv-ind = "1".
    ELSE
      ASSIGN
       lv-ind = "2"
       FRAME {&FRAME-NAME}:TITLE = "Wax / Label".

    RUN enable_UI.
    RUN make-readonly.
    DISABLE btn-cancel.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  END.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-a-record Dialog-Frame 
PROCEDURE add-a-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR li-line AS INT NO-UNDO.

DEF BUFFER b-est-flm FOR est-flm.


DO WITH FRAME {&FRAME-NAME}:
  FOR EACH tt-est-flm:
    DELETE tt-est-flm.
  END.

  IF ll-copy-record AND AVAIL est-flm THEN DO:
    FIND FIRST eb
        WHERE eb.company   EQ est.company
          AND eb.est-no    EQ est.est-no
          AND eb.form-no   EQ INT(est-flm.snum:SCREEN-VALUE IN BROWSE {&browse-name})
          AND (eb.blank-no EQ INT(est-flm.bnum:SCREEN-VALUE IN BROWSE {&browse-name}) OR
               INT(est-flm.bnum:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0)
        NO-LOCK NO-ERROR.

    IF AVAIL eb THEN DO:
      RUN est/d-selblk.w (ROWID(eb), "Copy " + TRIM(FRAME {&FRAME-NAME}:TITLE)).

      FOR EACH tt-select WHERE tt-selected,
          FIRST eb WHERE ROWID(eb) EQ tt-rowid
          BREAK BY eb.form-no
                BY eb.blank-no:

        FIND FIRST b-est-flm
            WHERE b-est-flm.company EQ est-flm.company
              AND b-est-flm.est-no  EQ est-flm.est-no
              AND b-est-flm.i-no    EQ est-flm.i-no
              AND b-est-flm.snum    EQ eb.form-no
              AND b-est-flm.bnum    EQ eb.blank-no
            NO-ERROR.

        IF AVAIL b-est-flm THEN
          ASSIGN
           b-est-flm.wid = est-flm.wid
           b-est-flm.len = est-flm.len.

        ELSE DO:
          FIND LAST b-est-flm
              WHERE b-est-flm.company EQ ip-company
                AND b-est-flm.est-no  EQ ip-est-no
              USE-INDEX est-qty NO-LOCK NO-ERROR.
          li-line = IF AVAIL b-est-flm THEN b-est-flm.LINE + 1 ELSE 1.

          CREATE b-est-flm.

          BUFFER-COPY est-flm EXCEPT rec_key TO b-est-flm
          ASSIGN
           b-est-flm.line = li-line
           b-est-flm.snum = eb.form-no
           b-est-flm.bnum = eb.blank-no.
        END.
      END.
    END.

    ELSE DO:
      CREATE tt-est-flm.
      BUFFER-COPY est-flm EXCEPT rec_key TO tt-est-flm.
    END.
  END.

  btn-update:LABEL = "&Save".

  IF AVAIL tt-est-flm OR NOT ll-copy-record THEN DO:
    RUN make-enable.
    DISABLE btn-add btn-delete btn-copy btn-OK.
    ENABLE btn-cancel.
    BROWSE {&browse-name}:INSERT-ROW("after").
    ll-new-record = YES.
  END.

  ELSE APPLY "choose" TO btn-update.
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
  ENABLE BROWSE-1 btn-add btn-update btn-delete btn-copy Btn-Cancel Btn-OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE make-enable Dialog-Frame 
PROCEDURE make-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*ASSIGN est-flm.i-no:SENSITIVE IN BROWSE {&browse-name} = YES
       est-flm.dscr:SENSITIVE = YES
       est-flm.dscr:SENSITIVE = YES
       est-flm.dscr:SENSITIVE = YES.
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE make-readonly Dialog-Frame 
PROCEDURE make-readonly :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*ASSIGN est-flm.i-no:SENSITIVE IN BROWSE {&browse-name} = NO
         est-flm.dscr:SENSITIVE = NO
         est-flm.dscr:SENSITIVE = NO
         est-flm.dscr:SENSITIVE = NO.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-blank-no Dialog-Frame 
PROCEDURE new-blank-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.


  IF NOT ll-copy-record THEN
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST eb
        WHERE eb.company   EQ est.company
          AND eb.est-no    EQ est.est-no
          AND eb.form-no   EQ INT(est-flm.snum:SCREEN-VALUE IN BROWSE {&browse-name})
          AND (eb.blank-no EQ INT(est-flm.bnum:SCREEN-VALUE IN BROWSE {&browse-name}) OR
               INT(est-flm.bnum:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0           OR
               ip-int EQ 1)
        NO-LOCK NO-ERROR.
    IF AVAIL eb THEN DO:
      IF ip-int EQ 1 THEN
        est-flm.bnum:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(eb.blank-no).
      ASSIGN
       est-flm.len:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(eb.t-len)
       est-flm.wid:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(eb.t-wid).
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-i-no Dialog-Frame 
PROCEDURE new-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
    RELEASE item.
    FIND item
        WHERE item.company  EQ ip-company
          AND item.i-no     EQ est-flm.i-no:SCREEN-VALUE in BROWSE {&browse-name}
          AND item.industry EQ lv-ind
          AND LOOKUP(item.mat-type,"F,W") GT 0
        NO-LOCK NO-ERROR.
    IF AVAIL item THEN est-flm.dscr:SCREEN-VALUE in BROWSE {&browse-name} = item.i-name.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


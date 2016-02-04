&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dialog-Frame 
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER ip-note-rec-key LIKE NOSWEAT.notes.rec_key      NO-UNDO.
DEF INPUT PARAMETER ip-note-type    LIKE  NOSWEAT.notes.note_type   NO-UNDO.
DEF INPUT PARAMETER ip-note-source  LIKE NOSWEAT.notes.note_source  NO-UNDO.
DEF INPUT PARAMETER ip-note-group   LIKE NOSWEAT.notes.note_group   NO-UNDO.
DEF INPUT PARAMETER ip-note-form-no LIKE NOSWEAT.notes.note_form_no  NO-UNDO.
DEF INPUT PARAMETER ip-note-code    LIKE NOSWEAT.notes.note_code     NO-UNDO.
/* Any field values to place in the description */
DEF INPUT PARAMETER ip-ref-fields   AS   CHAR                        NO-UNDO.
DEF OUTPUT PARAMETER op-code-chosen AS   CHAR                        NO-UNDO.
DEF OUTPUT PARAMETER op-added-rowid AS   ROWID                       NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE saveNoteCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE ip-rec_key AS CHARACTER NO-UNDO.
DEFINE VARIABLE ll-note-required AS LOG NO-UNDO.
DEFINE VARIABLE v-rtn-char AS CHAR NO-UNDO.
DEFINE VARIABLE v-rec-found AS LOG NO-UNDO.

{custom/globdefs.i}
{custom/gcompany.i}
{custom/gloc.i}
DEF BUFFER bf-item-spec FOR item-spec.
def var cocode as cha no-undo.
def var locode as cha no-undo.

assign cocode = gcompany
       locode = gloc.

DEF TEMP-TABLE tt-notes LIKE notes.

 RUN sys/ref/nk1look.p (INPUT cocode, INPUT "OEDATECHANGE", INPUT "L",
                       INPUT no, INPUT no, INPUT "", INPUT "", 
                       Output v-rtn-char, output v-rec-found).  
IF v-rec-found THEN
  ll-note-required = LOGICAL(v-rtn-char).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES notes

/* Definitions for DIALOG-BOX dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-dialog-Frame notes.note_code notes.note_text 
&Scoped-define ENABLED-FIELDS-IN-QUERY-dialog-Frame notes.note_code ~
notes.note_text 
&Scoped-define ENABLED-TABLES-IN-QUERY-dialog-Frame notes
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-dialog-Frame notes
&Scoped-define QUERY-STRING-dialog-Frame FOR EACH notes ~
      WHERE notes.rec_key = ip-note-rec-key ~
  and false SHARE-LOCK
&Scoped-define OPEN-QUERY-dialog-Frame OPEN QUERY dialog-Frame FOR EACH notes ~
      WHERE notes.rec_key = ip-note-rec-key ~
  and false SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-dialog-Frame notes
&Scoped-define FIRST-TABLE-IN-QUERY-dialog-Frame notes


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS notes.note_code notes.note_text 
&Scoped-define ENABLED-TABLES notes
&Scoped-define FIRST-ENABLED-TABLE notes
&Scoped-Define ENABLED-OBJECTS cbTitle btAddNote btOk 
&Scoped-Define DISPLAYED-FIELDS notes.note_code notes.note_text 
&Scoped-define DISPLAYED-TABLES notes
&Scoped-define FIRST-DISPLAYED-TABLE notes
&Scoped-Define DISPLAYED-OBJECTS spec-desc cbTitle 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAddNote 
     LABEL "+" 
     SIZE 6 BY 1.14
     FONT 6.

DEFINE BUTTON btCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btOk 
     LABEL "OK" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE cbTitle AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEM-PAIRS "1","1"
     DROP-DOWN-LIST
     SIZE 71 BY 1 NO-UNDO.

DEFINE VARIABLE spec-desc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY dialog-Frame FOR 
      notes SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME dialog-Frame
     notes.note_date AT ROW 1.24 COL 14 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     notes.note_time AT ROW 1.24 COL 46 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     notes.user_id AT ROW 1.24 COL 73 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 16.4 BY 1
          BGCOLOR 7 FGCOLOR 15 FONT 4
     notes.viewed AT ROW 1.24 COL 94 WIDGET-ID 16
          VIEW-AS TOGGLE-BOX
          SIZE 11.8 BY 1
     notes.note_code AT ROW 2.43 COL 16 COLON-ALIGNED WIDGET-ID 4
          LABEL "Spec" FORMAT "X(3)"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     spec-desc AT ROW 2.43 COL 25 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     notes.note_title AT ROW 3.62 COL 16 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
          FONT 4
     cbTitle AT ROW 3.62 COL 16 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     btAddNote AT ROW 3.62 COL 90 WIDGET-ID 24
     notes.note_text AT ROW 4.81 COL 3 NO-LABEL WIDGET-ID 6
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 101.6 BY 9.29
     btOk AT ROW 15.29 COL 45 WIDGET-ID 18
     btCancel AT ROW 15.29 COL 76 WIDGET-ID 20
     SPACE(14.80) SKIP(0.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Add Promise Date Change  Note".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME dialog-Frame:SCROLLABLE       = FALSE
       FRAME dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btCancel IN FRAME dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       btCancel:HIDDEN IN FRAME dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN notes.note_code IN FRAME dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       notes.note_code:HIDDEN IN FRAME dialog-Frame           = TRUE
       notes.note_code:READ-ONLY IN FRAME dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN notes.note_date IN FRAME dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       notes.note_date:HIDDEN IN FRAME dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN notes.note_time IN FRAME dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       notes.note_time:HIDDEN IN FRAME dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN notes.note_title IN FRAME dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       notes.note_title:HIDDEN IN FRAME dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN spec-desc IN FRAME dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       spec-desc:HIDDEN IN FRAME dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN notes.user_id IN FRAME dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       notes.user_id:HIDDEN IN FRAME dialog-Frame           = TRUE.

/* SETTINGS FOR TOGGLE-BOX notes.viewed IN FRAME dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       notes.viewed:HIDDEN IN FRAME dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX dialog-Frame
/* Query rebuild information for DIALOG-BOX dialog-Frame
     _TblList          = "NOSWEAT.notes"
     _Options          = "SHARE-LOCK"
     _Where[1]         = "NOSWEAT.notes.rec_key = ip-note-rec-key
  and false"
     _Query            is OPENED
*/  /* DIALOG-BOX dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dialog-Frame dialog-Frame
ON ESCAPE OF FRAME dialog-Frame /* Add Promise Date Change  Note */
DO:
  APPLY 'window-close' TO FRAME dialog-frame.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dialog-Frame dialog-Frame
ON WINDOW-CLOSE OF FRAME dialog-Frame /* Add Promise Date Change  Note */
DO:
  ASSIGN cbTitle.


  IF cbTitle EQ "" THEN DO:
      MESSAGE "A note title is required to save this note. " SKIP
              "Please select one and click 'OK' " skip 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  FIND FIRST rejct-cd 
      WHERE rejct-cd.TYPE = ip-note-type
        AND rejct-cd.CODE = cbTitle NO-LOCK NO-ERROR.
  IF NOT AVAIL rejct-cd THEN DO:
      MESSAGE "You must select Reason for Date Change via Down Down Arrow or Press Cancel"
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO cbTitle.
      RETURN NO-APPLY.
  END.
  /* This means they didn't press OK, so if they entered a valid reason */
  /* then save it.                                                      */
  RUN save-record.

  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAddNote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAddNote dialog-Frame
ON CHOOSE OF btAddNote IN FRAME dialog-Frame /* + */
DO:

    DEF VAR lNewCode     AS CHAR NO-UNDO.
    DEF VAR lNewDesc     AS CHAR NO-UNDO.
    DEF VAR ip-parms     AS CHAR NO-UNDO.
    DEF VAR op-values    AS CHAR NO-UNDO.
    DEF VAR i            AS INT  NO-UNDO.
    DEF VAR iCurRow      AS INT NO-UNDO.
    DEF VAR iCurCol      AS INT NO-UNDO.
    DEF VAR h_d-prompt   AS HANDLE NO-UNDO.
    
    REPEAT:
        ASSIGN lNewCode = ""
               lNewDesc = ""
               op-values = "".
        ip-parms = 
               "type=fill-in,name=fi6,row=4,col=3,enable=false,FORMAT=X(5),scrval=Code: " 
            + "|type=fill-in,name=fi2,row=4,col=16,enable=false,FORMAT=X(5),scrval=Desc: " 
            + "|type=fill-in,name=fi3,row=4,col=11,enable=true,FORMAT=X(2),width=5"
            + "|type=fill-in,name=fi4,row=4,col=23,enable=true,FORMAT=X(35),width=50"
             + "|type=win,name=win1,row=20,col=33,label=Add New Date Change Reason".
        iCurRow = FRAME {&FRAME-NAME}:ROW.
        iCurCol = FRAME {&FRAME-NAME}:COL.
        RUN custom/d-prompt.w PERSISTENT SET h_d-prompt (INPUT "Normal",
                               INPUT ip-parms,
                               INPUT "" /* validation procedure */,
                               OUTPUT op-values).
        RUN set-position IN h_d-prompt (INPUT 10, INPUT 10).
        DO i = 1 TO NUM-ENTRIES(op-values) BY 2.
            IF ENTRY(i, op-values) EQ "fi3" THEN
              lNewCode = ENTRY(i + 1, op-values).
            IF ENTRY(i, op-values) EQ "fi4" THEN
              lNewDesc = ENTRY(i + 1, op-values).    
        END.
        /* They entered nothing */
        IF lNewCode EQ "" THEN
            RETURN.
        FIND FIRST rejct-cd WHERE rejct-cd.CODE EQ lNewCode NO-LOCK NO-ERROR .
        IF AVAIL(rejct-cd) THEN DO:
            MESSAGE "This code already exists, please try another..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            NEXT.
        END.
        ELSE DO:
          CREATE rejct-cd.
          ASSIGN rejct-cd.TYPE = "R"
                 rejct-cd.CODE = lNewCode
                 rejct-cd.dscr = lNewDesc.
            cbTitle:ADD-LAST(lNewCode + " " + lNewDesc, lNewCode).
            cbTitle:SCREEN-VALUE = lNewCode.
        END.
        LEAVE.
    END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCancel dialog-Frame
ON CHOOSE OF btCancel IN FRAME dialog-Frame /* Cancel */
DO:
  ASSIGN cbTitle.
  
  IF cbTitle = "" THEN DO:
      MESSAGE "A note title is required to save this note. " SKIP
              "Please select one and click 'OK' " skip 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  FIND FIRST rejct-cd 
      WHERE rejct-cd.TYPE = ip-note-type
        AND rejct-cd.CODE = cbTitle NO-LOCK NO-ERROR.
  IF NOT AVAIL rejct-cd THEN DO:
      MESSAGE "You must select Reason for Date Change via Down Down Arrow or Press Cancel"
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO cbTitle.
      RETURN NO-APPLY.
  END.
           
  RUN save-record.

  APPLY 'go' TO  FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOk dialog-Frame
ON CHOOSE OF btOk IN FRAME dialog-Frame /* OK */
DO:

  ASSIGN cbTitle.
  
  IF cbTitle:SCREEN-VALUE = "" THEN DO:
      MESSAGE "A note title is required to save this note. " SKIP
              "Please select one and click 'OK' " skip 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  FIND FIRST rejct-cd 
      WHERE rejct-cd.TYPE = ip-note-type
        AND rejct-cd.CODE = cbTitle NO-LOCK NO-ERROR.
  IF NOT AVAIL rejct-cd THEN DO:
      MESSAGE "You must select Reason for Date Change via Down Down Arrow or Press Cancel"
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO cbTitle.
      RETURN NO-APPLY.
  END.
           
  RUN save-record.

  APPLY 'go' TO  FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbTitle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbTitle dialog-Frame
ON VALUE-CHANGED OF cbTitle IN FRAME dialog-Frame
DO:
  ASSIGN cbTitle.
  FIND FIRST rejct-cd WHERE rejct-cd.CODE = cbTitle NO-LOCK NO-ERROR.
  IF AVAIL(rejct-cd) AND notes.note_text:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN
     notes.note_text:SCREEN-VALUE IN FRAME {&FRAME-NAME} = rejct-cd.dscr.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME notes.note_code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_code dialog-Frame
ON ENTRY OF notes.note_code IN FRAME dialog-Frame /* Spec */
DO:
/*   saveNoteCode = {&self-name}:SCREEN-VALUE. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_code dialog-Frame
ON HELP OF notes.note_code IN FRAME dialog-Frame /* Spec */
DO:
/*   DEF VAR char-val AS CHAR NO-UNDO.                                             */
/*                                                                                 */
/*                                                                                 */
/*   RUN cec/l-itspec.w (g_company, {&self-name}:SCREEN-VALUE, OUTPUT char-val).   */
/*   IF char-val NE "" AND {&self-name}:SCREEN-VALUE NE ENTRY(1,char-val) THEN DO: */
/*     {&self-name}:SCREEN-VALUE = ENTRY(1,char-val).                              */
/*     RUN new-note_code.                                                          */
/*   END.                                                                          */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_code dialog-Frame
ON LEAVE OF notes.note_code IN FRAME dialog-Frame /* Spec */
DO:
/*   IF LASTKEY NE -1 THEN DO:                     */
/*     RUN valid-note_code (FOCUS) NO-ERROR.       */
/*     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */
/*   END.                                          */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL notes.note_code dialog-Frame
ON VALUE-CHANGED OF notes.note_code IN FRAME dialog-Frame /* Spec */
DO:
/*   RUN new-note_code. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dialog-Frame 


/* ***************************  Main Block  *************************** */
  
IF ip-note-rec-key = ? THEN DO:
    RUN DISABLE_ui.
    APPLY 'close' TO CURRENT-WINDOW.
END.

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
IF ip-note-code BEGINS "R" THEN
  RUN asi-initialize.
ELSE
  IF ip-note-code BEGINS "P" THEN DO:
  
    /* Set because "P" uses the same codes as "R" */
    ip-note-type = "R".

    RUN promise-date-init.
  END.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asi-initialize dialog-Frame 
PROCEDURE asi-initialize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND oe-rel WHERE oe-rel.rec_key = ip-note-rec-key NO-LOCK NO-ERROR.
IF NOT AVAIL oe-rel THEN 
    FIND oe-rell WHERE oe-rell.rec_key = ip-note-rec-key NO-LOCK NO-ERROR.
IF AVAIL oe-rell THEN DO:
      FIND FIRST oe-rel
      WHERE oe-rel.company  EQ oe-rell.company
        AND oe-rel.link-no  EQ oe-rell.r-no
        AND oe-rel.ord-no   EQ oe-rell.ord-no
        AND oe-rel.rel-no   EQ oe-rell.rel-no
        AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
        AND oe-rel.i-no     EQ oe-rell.i-no
        AND oe-rel.line     EQ oe-rell.line
        AND oe-rel.po-no    EQ oe-rell.po-no        
     USE-INDEX link-ord NO-LOCK NO-ERROR.

     IF NOT AVAIL oe-rel THEN
      FIND FIRST oe-rel
      WHERE oe-rel.company  EQ oe-rell.company
        AND oe-rel.ord-no   EQ oe-rell.ord-no
        AND oe-rel.i-no     EQ oe-rell.i-no
        AND oe-rel.line     EQ oe-rell.line
        AND oe-rel.rel-no   EQ oe-rell.rel-no
        AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
        AND oe-rel.po-no    EQ oe-rell.po-no
        AND INDEX("SIL", oe-rel.stat) EQ 0
      USE-INDEX ord-item NO-LOCK NO-ERROR.
     IF AVAIL oe-rel THEN
         ip-note-rec-key = oe-rel.rec_key.
     ELSE
         ip-note-rec-key = ?.
END.

cbTitle:DELETE("1") IN FRAME {&FRAME-NAME}.
FOR EACH rejct-cd 
    WHERE rejct-cd.TYPE    EQ ip-note-type
    NO-LOCK WITH FRAME {&FRAME-NAME}.
    cbTitle:ADD-LAST(rejct-cd.CODE + " " + rejct-cd.dscr, rejct-cd.CODE).        
END.
ASSIGN notes.note_code:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "RDC"
       notes.note_text:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI dialog-Frame  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-dialog-Frame}
  GET FIRST dialog-Frame.
  DISPLAY spec-desc cbTitle 
      WITH FRAME dialog-Frame.
  IF AVAILABLE notes THEN 
    DISPLAY notes.note_code notes.note_text 
      WITH FRAME dialog-Frame.
  ENABLE notes.note_code cbTitle btAddNote notes.note_text btOk 
      WITH FRAME dialog-Frame.
  VIEW FRAME dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE promise-date-init dialog-Frame 
PROCEDURE promise-date-init :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND oe-ordl WHERE oe-ordl.rec_key = ip-note-rec-key NO-LOCK NO-ERROR.

 IF AVAIL oe-ordl THEN
     ip-note-rec-key = oe-ordl.rec_key.
 ELSE
     ip-note-rec-key = ?.


cbTitle:DELETE("1") IN FRAME {&FRAME-NAME} NO-ERROR.
FOR EACH rejct-cd 
    WHERE rejct-cd.TYPE    EQ ip-note-type
    NO-LOCK WITH FRAME {&FRAME-NAME}.
    cbTitle:ADD-LAST(rejct-cd.CODE + " " + rejct-cd.dscr, rejct-cd.CODE).        
END.
ASSIGN notes.note_code:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "PDC"
       notes.note_text:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE save-record dialog-Frame 
PROCEDURE save-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST rejct-cd 
      WHERE rejct-cd.TYPE = ip-note-type
        AND rejct-cd.CODE = cbTitle NO-LOCK NO-ERROR.
  IF NOT AVAIL rejct-cd THEN DO:
      MESSAGE "You must select Reason for Date Change via Down Down Arrow or Press Cancel"
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO cbTitle IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
  END.
           
  op-code-chosen = rejct-cd.CODE.
  CREATE nosweat.notes.
  ASSIGN NOSWEAT.notes.note_text 
         NOSWEAT.notes.note_title = cbTitle.
  ASSIGN 
      NOSWEAT.notes.viewed      = NO
      NOSWEAT.notes.user_id     = USERID("NOSWEAT")
      NOSWEAT.notes.rec_key     = ip-note-rec-key
      NOSWEAT.notes.note_type   = IF ip-note-code EQ "PDC" THEN "P" ELSE ip-note-type   
      NOSWEAT.notes.note_time   = TIME    
      NOSWEAT.notes.note_source = ip-note-source
      NOSWEAT.notes.note_group  = ip-note-group
      NOSWEAT.notes.note_form_no = ip-note-form-no
      NOSWEAT.notes.note_date    = TODAY
      NOSWEAT.notes.note_code    .
  op-added-rowid = ROWID(nosweat.notes).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


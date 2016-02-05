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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF INPUT PARAM ip-est-recid AS RECID NO-UNDO.
DEF INPUT PARAM ip-eb-recid AS RECID NO-UNDO.
DEF BUFFER bf-est FOR est.
DEF BUFFER bf-eb FOR eb.

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
&Scoped-define INTERNAL-TABLES eb ef

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 eb.form-no eb.blank-no eb.part-no ~
eb.bl-qty eb.yld-qty eb.yrprice eb.num-wid eb.num-len eb.num-up 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 eb.form-no eb.blank-no ~
eb.part-no eb.bl-qty eb.yld-qty eb.yrprice eb.num-wid eb.num-len eb.num-up 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 eb
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 eb
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH eb ~
      WHERE eb.company = bf-est.company and ~
eb.est-no = bf-est.est-no NO-LOCK, ~
      EACH ef OF eb NO-LOCK ~
    BY eb.form-no ~
       BY eb.blank-no INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 eb ef
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 eb
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 ef


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 Btn_OK Btn_Cancel 

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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      eb, 
      ef SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      eb.form-no COLUMN-LABEL "Form#" FORMAT ">>>":U
      eb.blank-no COLUMN-LABEL "Blank#" FORMAT ">>>":U
      eb.part-no COLUMN-LABEL "Cust Part#" FORMAT "x(15)":U WIDTH 20
      eb.bl-qty COLUMN-LABEL "Request!Qty" FORMAT ">>,>>>,>>>":U
      eb.yld-qty COLUMN-LABEL "Yield!Qty" FORMAT ">>,>>>,>>>":U
      eb.yrprice COLUMN-LABEL "Price" FORMAT "Yield/Request":U
            WIDTH 6
      eb.num-wid COLUMN-LABEL "# on!Width" FORMAT ">9":U WIDTH 6
      eb.num-len COLUMN-LABEL "# on!Length" FORMAT ">9":U WIDTH 6
      eb.num-up FORMAT ">>9":U WIDTH 11.2
  ENABLE
      eb.form-no
      eb.blank-no
      eb.part-no
      eb.bl-qty
      eb.yld-qty
      eb.yrprice
      eb.num-wid
      eb.num-len
      eb.num-up
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 98 BY 10
         BGCOLOR 8  ROW-HEIGHT-CHARS .71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     Btn_OK AT ROW 11.95 COL 24
     Btn_Cancel AT ROW 11.95 COL 62
     SPACE(22.39) SKIP(0.90)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Estimate Combo Goto"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
     _TblList          = "ASI.eb,ASI.ef OF ASI.eb"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "ASI.eb.form-no|yes,ASI.eb.blank-no|yes"
     _Where[1]         = "eb.company = bf-est.company and
eb.est-no = bf-est.est-no"
     _FldNameList[1]   > ASI.eb.form-no
"eb.form-no" "Form#" ">>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > ASI.eb.blank-no
"eb.blank-no" "Blank#" ">>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > ASI.eb.part-no
"eb.part-no" "Cust Part#" ? "character" ? ? ? ? ? ? yes ? no no "20" yes no no "U" "" ""
     _FldNameList[4]   > ASI.eb.bl-qty
"eb.bl-qty" "Request!Qty" ">>,>>>,>>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > ASI.eb.yld-qty
"eb.yld-qty" "Yield!Qty" ">>,>>>,>>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > ASI.eb.yrprice
"eb.yrprice" "Price" "Yield/Request" "logical" ? ? ? ? ? ? yes ? no no "6" yes no no "U" "" ""
     _FldNameList[7]   > ASI.eb.num-wid
"eb.num-wid" "# on!Width" ? "integer" ? ? ? ? ? ? yes ? no no "6" yes no no "U" "" ""
     _FldNameList[8]   > ASI.eb.num-len
"eb.num-len" "# on!Length" ? "integer" ? ? ? ? ? ? yes ? no no "6" yes no no "U" "" ""
     _FldNameList[9]   > ASI.eb.num-up
"eb.num-up" ? ? "integer" ? ? ? ? ? ? yes ? no no "11.2" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Estimate Combo Goto */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON ROW-DISPLAY OF BROWSE-1 IN FRAME Dialog-Frame
DO:
    IF eb.bl-qty = 0 THEN eb.bl-qty:SCREEN-VALUE IN BROWSE {&browse-name} = string(bf-est.est-qty[1]).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.bl-qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.bl-qty BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF eb.bl-qty IN BROWSE BROWSE-1 /* Request!Qty */
DO:
   eb.yld-qty:SCREEN-VALUE IN BROWSE {&browse-name} = eb.bl-qty:SCREEN-VALUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.num-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-wid BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON VALUE-CHANGED OF eb.num-wid IN BROWSE BROWSE-1 /* # on!Width */
DO:
  RUN calc-#up.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.num-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-len BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON VALUE-CHANGED OF eb.num-len IN BROWSE BROWSE-1 /* # on!Length */
DO:
  RUN calc-#up.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.num-up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.num-up BROWSE-1 _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF eb.num-up IN BROWSE BROWSE-1 /* # UP */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   /*
    v-qty = max(input eb.yld-qty,input eb.bl-qty) / input eb.num-up.
          {sys/inc/roundup.i v-qty}
/*
          if input eb.num-up eq 0                  or
             (v-new and not eb.num-up entered and
                        not eb.yld-qty entered)    then do:
            find first eb
                where eb.e-num   eq xest.e-num
                  and eb.form-no eq xef.form-no
                  and recid(eb)  ne recid(eb)
                no-lock no-error.
            if avail eb then v-qty = eb.yld-qty / eb.num-up.
            
            {sys/inc/roundup.i v-qty}
            
            if v-qty eq ? or v-qty le 0 then v-qty = input eb.bl-qty.
            
            display trunc(input eb.bl-qty / v-qty,0) +
                    int(input eb.bl-qty modulo v-qty gt 0) @ eb.num-up.
          end.
*/          
          display v-qty * input eb.num-up @ eb.yld-qty.
          
          if eb.die-in ne 0 then
            eb.die-in = (eb.die-in / eb.num-up) * input eb.num-up.
            
          assign eb.num-up.
          if eb.die-in eq ? then eb.die-in = 0.
     ===========*/ 
    DEF VAR v-qty AS DEC NO-UNDO.
    DEF BUFFER bf-eb FOR eb.
    DEF BUFFER xef FOR ef.


    FIND CURRENT eb .
    v-qty = eb.yld-qty / eb.num-up.
    FIND xef WHERE xef.company = eb.company
               AND xef.est-no = eb.est-no
               AND xef.form-no = eb.form-no .
    assign
       eb.yld-qty = v-qty * eb.num-up
       xef.die-in  = eb.die-in.
  
    for each bf-eb
          where bf-eb.company = eb.company
            AND bf-eb.est-no   eq eb.est-no
            and bf-eb.form-no eq eb.form-no
            and recid(bf-eb)  ne recid(eb)
            by bf-eb.blank-no:
        
        ASSIGN xef.die-in = xef.die-in - bf-eb.die-in
               bf-eb.die-in  = bf-eb.die-in / bf-eb.num-up
               bf-eb.num-up  = trunc(bf-eb.bl-qty / v-qty,0) +
                               int(bf-eb.bl-qty modulo v-qty gt 0)
               bf-eb.die-in  = bf-eb.die-in * bf-eb.num-up
               bf-eb.yld-qty = v-qty * bf-eb.num-up
               xef.die-in = xef.die-in + bf-eb.die-in.
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
  FIND bf-est WHERE RECID(bf-est) = ip-est-recid NO-LOCK NO-ERROR.
  FIND bf-eb WHERE RECID(bf-eb) = ip-eb-recid .
  RUN enable_UI.
  REPOSITION BROWSE-1 TO RECID ip-eb-recid.  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-#up Dialog-Frame 
PROCEDURE calc-#up :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    eb.num-up:SCREEN-VALUE IN BROWSE {&browse-name} = 
        STRING(DEC(eb.num-wid:SCREEN-VALUE IN BROWSE {&browse-name}) *
               DEC(eb.num-len:SCREEN-VALUE IN BROWSE {&browse-name})).
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
  ENABLE BROWSE-1 Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


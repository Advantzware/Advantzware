&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 

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

/* Local Variable Definitions ---                                       */
&SCOPED-DEFINE NEW 
DEF VAR h_w-estfc AS HANDLE NO-UNDO.
{custom/globdefs.i }
{methods/defines/hndldefs.i } 
def var lv-eb-tmpid as recid no-undo.
DEF VAR k_frac as dec init 6.25 no-undo.
DEF VAR v-count AS INT NO-UNDO.
def new shared var cocode as cha no-undo.
def new shared var locode as cha no-undo.

assign cocode = g_company
       locode = g_loc.

{sys/inc/f16to32.i}

IF v-cecscrn-dec THEN
DO:
   DEF TEMP-TABLE tt-64-dec NO-UNDO
       FIELD DEC AS DEC DECIMALS 6.

   DO v-count = 0 TO 63:
       CREATE tt-64-dec.
       tt-64-dec.DEC = v-count / 64.0.
       RELEASE tt-64-dec.
   END.
END.

&SCOPED-DEFINE find-eb                                  ~
    FIND eb NO-LOCK                                     ~
        WHERE eb.company    EQ g_company                ~
          AND eb.loc        EQ g_loc                    ~
          AND eb.est-no     BEGINS ls-est-no            ~
          AND eb.cust-no    BEGINS ls-cust-no           ~
          AND eb.part-no    BEGINS ls-part-no           ~
          AND eb.part-dscr1 BEGINS ls-i-name            ~
          AND eb.stock-no   BEGINS ls-i-no              ~
          AND eb.die-no     BEGINS ls-die-no            ~
          AND eb.cad-no     BEGINS ls-cad-no            ~
          AND (eb.len       EQ ld-len OR ld-len EQ 0)   ~
          AND (eb.wid       EQ ld-wid OR ld-wid EQ 0)   ~
          AND (eb.dep       EQ ld-dep OR ld-dep EQ 0)   ~
          AND ((eb.form-no = 0 AND chk-box-1)  OR NOT chk-box-1 )

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES est

/* Definitions for FRAME F-Main                                         */
&Scoped-define QUERY-STRING-F-Main FOR EACH est SHARE-LOCK
&Scoped-define OPEN-QUERY-F-Main OPEN QUERY F-Main FOR EACH est SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F-Main est
&Scoped-define FIRST-TABLE-IN-QUERY-F-Main est


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ls-est-no ls-cust-no ls-part-no ls-i-name ~
ls-i-no ls-die-no ls-cad-no ld-len ld-wid ld-dep  BUTTON-3 chk-box-1 RECT-1 
&Scoped-Define DISPLAYED-OBJECTS ls-est-no ls-cust-no ls-part-no ls-i-name ~
ls-i-no ls-die-no ls-cad-no ld-len ld-wid ld-dep 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 ls-est-no ls-cust-no ls-part-no ls-i-name ls-i-no ~
ls-die-no ls-cad-no ld-len ld-wid ld-dep 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-3 
     LABEL "&Get Estimate" 
     SIZE 25 BY 1.62.

DEFINE VARIABLE chk-box-1 AS LOGICAL INITIAL NO
     LABEL "&Set Item Only" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY 1.62.

DEFINE VARIABLE ld-len AS DECIMAL FORMAT ">>9.999":U INITIAL 0 
     LABEL "Length" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE ld-wid AS DECIMAL FORMAT ">>9.999":U INITIAL 0 
     LABEL "Width" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE ld-dep AS DECIMAL FORMAT ">>9.999":U INITIAL 0 
     LABEL "Depth" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.


DEFINE VARIABLE ls-cad-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "CAD Number" 
     VIEW-AS FILL-IN 
     SIZE 28.8 BY 1 NO-UNDO.

DEFINE VARIABLE ls-cust-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE ls-die-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Die Number" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE ls-est-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Estimate" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ls-i-name AS CHARACTER FORMAT "X(30)":U 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE ls-i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE ls-part-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cust Part" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 80 BY 10.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F-Main FOR 
      est SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ls-est-no AT ROW 1.71 COL 20 COLON-ALIGNED
     ls-cust-no AT ROW 2.91 COL 20 COLON-ALIGNED
     ls-part-no AT ROW 4.1 COL 20 COLON-ALIGNED
     ls-i-name AT ROW 5.29 COL 20 COLON-ALIGNED
     ls-i-no AT ROW 6.48 COL 20 COLON-ALIGNED
     ls-die-no AT ROW 7.67 COL 20 COLON-ALIGNED
     ls-cad-no AT ROW 8.86 COL 20 COLON-ALIGNED
     ld-len AT ROW 10.05 COL 20 COLON-ALIGNED
     ld-wid AT ROW 10.05 COL 42 COLON-ALIGNED
     ld-dep AT ROW 10.05 COL 63 COLON-ALIGNED
     BUTTON-3 AT ROW 12.43 COL 45
     chk-box-1 AT ROW 12.43 COL 15
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 13.91
         FONT 6
         DEFAULT-BUTTON BUTTON-3.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Estimate Query"
         HEIGHT             = 13.91
         WIDTH              = 80
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 80
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR FILL-IN ld-len IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN ls-cad-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN ls-cust-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN ls-die-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN ls-est-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN ls-i-name IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN ls-i-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN ls-part-no IN FRAME F-Main
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _TblList          = "ASI.est"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Estimate Query */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Estimate Query */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main W-Win
ON HELP OF FRAME F-Main
DO:
   def var lv-handle as widget-handle no-undo.
   def var ls-cur-val as cha no-undo.
   def var ls-int-val as INT no-undo.
   DEF VAR char-val AS cha NO-UNDO.
   DEF VAR lw-focus AS HANDLE NO-UNDO.


   lw-focus = FOCUS.

   case lw-focus:name :
      when "ls-est-no" then do:
              run windows/l-est.w (g_company,g_loc,lw-focus:screen-value, output char-val).
              if char-val <> "" then do:                 
                 FIND FIRST eb WHERE string(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
                 IF AVAIL eb THEN ASSIGN lw-focus:SCREEN-VALUE = eb.est-no
                                         lv-eb-tmpid = RECID(eb)
                                  ls-cust-no:screen-value in frame {&frame-name} =  eb.cust-no
                                  ls-est-no:SCREEN-VALUE = eb.est-no
                                  ls-part-no:SCREEN-VALUE = eb.part-no
                                  ls-i-name:SCREEN-VALUE = eb.part-dscr1
                                  ls-i-no:SCREEN-VALUE = eb.stock-no
                                  ls-die-no:SCREEN-VALUE = eb.die-no
                                  ls-cad-no:SCREEN-VALUE = eb.cad-no
                                  ld-len:SCREEN-VALUE = STRING(eb.len)
                                  ld-wid:SCREEN-VALUE = STRING(eb.wid)
                                  ld-dep:SCREEN-VALUE = STRING(eb.dep) .  
              end.                
      end.   
      when "ls-part-no" then do: 
           run est/l-ebrfqP.w (g_company, g_loc, lw-focus:screen-value, output lv-eb-tmpid) .
           FIND FIRST eb WHERE RECID(eb) = lv-eb-tmpid NO-LOCK NO-ERROR.
                 IF AVAIL eb THEN ASSIGN lw-focus:SCREEN-VALUE = eb.part-no
                                  ls-cust-no:screen-value in frame {&frame-name} =  eb.cust-no
                                  ls-est-no:SCREEN-VALUE = eb.est-no
                                  ls-part-no:SCREEN-VALUE = eb.part-no
                                  ls-i-name:SCREEN-VALUE = eb.part-dscr1
                                  ls-i-no:SCREEN-VALUE = eb.stock-no
                                  ls-die-no:SCREEN-VALUE = eb.die-no
                                  ls-cad-no:SCREEN-VALUE = eb.cad-no
                                  ld-len:SCREEN-VALUE = STRING(eb.len) 
                                  ld-wid:SCREEN-VALUE = STRING(eb.wid)
                                  ld-dep:SCREEN-VALUE = STRING(eb.dep) . 
           return no-apply.          
      end.
      when "ls-i-no" then do:
        /* run windows/l-itemfg.w  (gcompany, output char-val). */
           run est/l-ebrfq.w (g_company, g_loc,lw-focus:screen-value, output lv-eb-tmpid) .           
           FIND FIRST eb WHERE RECID(eb) = lv-eb-tmpid NO-LOCK NO-ERROR.
                 IF AVAIL eb THEN 
                     ASSIGN lw-focus:SCREEN-VALUE = eb.stock-no
                         ls-cust-no:screen-value in frame {&frame-name} =  eb.cust-no
                         ls-est-no:SCREEN-VALUE = eb.est-no
                         ls-part-no:SCREEN-VALUE = eb.part-no
                         ls-i-name:SCREEN-VALUE = eb.part-dscr1
                         ls-i-no:SCREEN-VALUE = eb.stock-no
                         ls-die-no:SCREEN-VALUE = eb.die-no
                         ls-cad-no:SCREEN-VALUE = eb.cad-no
                         ld-len:SCREEN-VALUE = STRING(eb.len)
                         ld-wid:SCREEN-VALUE = STRING(eb.wid)
                         ld-dep:SCREEN-VALUE = STRING(eb.dep) . 
           return no-apply.
      end.
      when "ls-cust-no" OR WHEN "ls-i-name" OR WHEN "ls-die-no" OR WHEN "ls-cad-no" OR WHEN "ld-len" OR WHEN "ld-wid" OR WHEN "ld-dep"
      then do:
          ASSIGN
            ls-cur-val = lw-focus:SCREEN-VALUE
            ls-int-val = IF lw-focus:NAME EQ "ld-dep"    THEN 10 ELSE 
                         IF lw-focus:NAME EQ "ld-wid"    THEN 9 ELSE
                         IF lw-focus:NAME EQ "ld-len"    THEN 8 ELSE
                         IF lw-focus:NAME EQ "ls-cad-no" THEN 7 ELSE
                         IF lw-focus:NAME EQ "ls-die-no" THEN 6 ELSE
                         IF lw-focus:NAME EQ "ls-i-name" THEN 5 ELSE 2.

           run est/l-ebcst.w (g_company,g_loc,ls-cur-val,ls-int-val,chk-box-1,ld-len,ld-wid,ld-dep, output lv-eb-tmpid).
           if lv-eb-tmpid <> ? then do:
              FIND FIRST eb WHERE RECID(eb) = lv-eb-tmpid NO-LOCK NO-ERROR.
              IF AVAIL eb THEN 
                  ASSIGN ls-cust-no:screen-value in frame {&frame-name} =  eb.cust-no
                         ls-est-no:SCREEN-VALUE = eb.est-no
                         ls-part-no:SCREEN-VALUE = eb.part-no
                         ls-i-name:SCREEN-VALUE = eb.part-dscr1
                         ls-i-no:SCREEN-VALUE = eb.stock-no
                         ls-die-no:SCREEN-VALUE = eb.die-no
                         ls-cad-no:SCREEN-VALUE = eb.cad-no
                         ld-len:SCREEN-VALUE = string(eb.len)
                         ld-wid:SCREEN-VALUE = string(eb.wid)
                         ld-dep:SCREEN-VALUE = string(eb.dep) .
           end.
           return no-apply.
       end.  /* cust-no*/

  end case.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Get Estimate */
DO:
  ls-est-no:SCREEN-VALUE = FILL(" ",8 - LENGTH(TRIM(ls-est-no:SCREEN-VALUE))) +
                           TRIM(ls-est-no:SCREEN-VALUE).

  ASSIGN {&List-1}.

  RELEASE eb.
  RELEASE est.

  FIND FIRST eb WHERE RECID(eb) EQ lv-eb-tmpid NO-LOCK NO-ERROR.

  IF NOT AVAIL eb AND ls-est-no NE "" THEN DO:
    {&find-eb} USE-INDEX est-no NO-ERROR.
    IF AMBIG eb THEN DO:
      APPLY "entry" TO ls-est-no.
      APPLY "help" TO FRAME {&FRAME-NAME}.
    END.
  END.

  ELSE
  IF NOT AVAIL eb AND ls-cust-no NE "" THEN DO:
    {&find-eb} USE-INDEX cust NO-ERROR.
    IF AMBIG eb THEN DO:
      APPLY "entry" TO ls-cust-no.
      APPLY "help" TO FRAME {&FRAME-NAME}.
    END.
  END.

  ELSE
  IF NOT AVAIL eb AND ls-part-no NE "" THEN DO:
    {&find-eb} USE-INDEX part NO-ERROR.
    IF AMBIG eb THEN DO:
      APPLY "entry" TO ls-part-no.
      APPLY "help" TO FRAME {&FRAME-NAME}.
    END.
  END.

  ELSE
  IF NOT AVAIL eb AND ls-i-name NE "" THEN DO:
    {&find-eb} USE-INDEX pdscr NO-ERROR.
    IF AMBIG eb THEN DO:
      APPLY "entry" TO ls-i-name.
      APPLY "help" TO FRAME {&FRAME-NAME}.
    END.
  END.

  ELSE
  IF NOT AVAIL eb AND ls-i-no NE "" THEN DO:
    {&find-eb} USE-INDEX stock NO-ERROR.
    IF AMBIG eb THEN DO:
      APPLY "entry" TO ls-i-no.
      APPLY "help" TO FRAME {&FRAME-NAME}.
    END.
  END.

  ELSE
  IF NOT AVAIL eb AND ls-die-no NE "" THEN DO:
    {&find-eb} USE-INDEX die NO-ERROR.
    IF AMBIG eb THEN DO:
      APPLY "entry" TO ls-die-no.
      APPLY "help" TO FRAME {&FRAME-NAME}.
    END.
  END.

  ELSE
  IF NOT AVAIL eb AND ls-cad-no NE "" THEN DO:
    {&find-eb} USE-INDEX cad NO-ERROR.
    IF AMBIG eb THEN DO:
      APPLY "entry" TO ls-cad-no.
      APPLY "help" TO FRAME {&FRAME-NAME}.
    END.
  END.

  ELSE
  IF NOT AVAIL eb AND ld-len NE 0 THEN DO:
      {&find-eb} NO-ERROR.
    IF AMBIG eb THEN DO:
      APPLY "entry" TO ld-len.
      APPLY "help" TO FRAME {&FRAME-NAME}.
    END.
  END.

  ELSE
  IF NOT AVAIL eb AND ld-wid NE 0 THEN DO:
    {&find-eb} NO-ERROR.
    IF AMBIG eb THEN DO:
      APPLY "entry" TO ld-wid.
      APPLY "help" TO FRAME {&FRAME-NAME}.
    END.
  END.

  ELSE
  IF NOT AVAIL eb AND ld-dep NE 0 THEN DO:
    {&find-eb} NO-ERROR.
    IF AMBIG eb THEN DO:
      APPLY "entry" TO ld-dep.
      APPLY "help" TO FRAME {&FRAME-NAME}.
    END.
  END.

  IF AVAIL eb THEN
  FIND FIRST est
      WHERE est.company EQ eb.company
        AND est.est-no  EQ eb.est-no
      NO-LOCK NO-ERROR.

  IF AVAIL est THEN RUN open-estimate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-len W-Win
ON LEAVE OF ld-len IN FRAME F-Main /* Length */
DO:
   ASSIGN ls-est-no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-wid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-wid W-Win
ON LEAVE OF ld-wid IN FRAME F-Main /* Length */
DO:
   ASSIGN ls-est-no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-dep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-dep W-Win
ON LEAVE OF ld-dep IN FRAME F-Main /* Length */
DO:
   ASSIGN ls-est-no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-cad-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-cad-no W-Win
ON LEAVE OF ls-cad-no IN FRAME F-Main /* CAD Number */
DO:
   ASSIGN ls-est-no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-cust-no W-Win
ON LEAVE OF ls-cust-no IN FRAME F-Main /* Customer# */
DO:
   ASSIGN ls-est-no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-die-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-die-no W-Win
ON LEAVE OF ls-die-no IN FRAME F-Main /* Die Number */
DO:
   ASSIGN ls-est-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-i-name W-Win
ON LEAVE OF ls-i-name IN FRAME F-Main /* Item Name */
DO:
   ASSIGN ls-est-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-i-no W-Win
ON LEAVE OF ls-i-no IN FRAME F-Main /* FG Item# */
DO:
   ASSIGN ls-est-no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-part-no W-Win
ON LEAVE OF ls-part-no IN FRAME F-Main /* Cust Part */
DO:
   ASSIGN ls-est-no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME chk-box-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL chk-box-1 W-Win
ON LEAVE OF chk-box-1 IN FRAME F-Main 
DO:
   ASSIGN ls-est-no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME chk-box-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL chk-box-1 W-Win
ON VALUE-CHANGED OF chk-box-1 IN FRAME F-Main /* Include 0 Order Balance Items? */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
   {sys/inc/f3helpw.i}
/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

   IF v-cecscrn-char EQ "Decimal" THEN
     ASSIGN
        ld-len:FORMAT = ">>9.999999"
        ld-len:WIDTH = 13.2
        ld-wid:FORMAT = ">>9.999999"
        ld-wid:WIDTH = 13.2
        ld-dep:FORMAT = ">>9.999999"
        ld-dep:WIDTH = 14.2 .


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY ls-est-no ls-cust-no ls-part-no ls-i-name ls-i-no ls-die-no ls-cad-no 
          ld-len ld-wid ld-dep 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE ls-est-no ls-cust-no ls-part-no ls-i-name ls-i-no ls-die-no ls-cad-no 
         ld-len ld-wid ld-dep BUTTON-3 chk-box-1 RECT-1 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.

   RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY "entry" TO ls-est-no IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-estimate W-Win 
PROCEDURE open-estimate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF est.est-type >= 5 THEN DO:
      RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/w-estcf.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-estfc ).
       /* Position in AB:  ( 5.19 , 5.60 ) */
       /* Size in UIB:  ( 2.05 , 11.60 ) */
  END.
  ELSE IF est.est-type <= 4  THEN DO:
      RUN init-object IN THIS-PROCEDURE (
             INPUT  'est/w-estf.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'Layout = ':U ,
             OUTPUT h_w-estfc ).
  END.
             /* Links to SmartWindow h_w-ordest. */
    RUN add-link IN adm-broker-hdl ( this-procedure , 'Record':U , h_w-estfc ).
    RUN dispatch IN h_w-estfc ('initialize').
    /*RUN dispatch IN h_w-estfc ('apply-entry'). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "est"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
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


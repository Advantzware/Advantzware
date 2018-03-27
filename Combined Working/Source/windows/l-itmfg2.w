&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def input parameter ip-company like itemfg.company no-undo.
def input parameter ip-cust-no like itemfg.cust-no no-undo.
def input parameter ip-cur-val as cha no-undo.
DEF INPUT PARAMETER ip-field AS INT NO-UNDO.
def output parameter op-char-val as cha no-undo. /* string i-code + i-name */

{sys/inc/VAR.i NEW SHARED}

def var lv-type-dscr as cha no-undo.
def var lv-first-time as log init yes no-undo.

DEF VAR lActive AS LOG NO-UNDO.
DEF VAR v-check-page AS LOG INIT NO NO-UNDO .

&scoped-define SORTBY-1 BY itemfg.i-no
&scoped-define SORTBY-2 BY itemfg.i-name
&scoped-define SORTBY-3 BY itemfg.part-dscr1
&scoped-define SORTBY-4 BY TRIM(itemfg.est-no)
&scoped-define SORTBY-5 BY itemfg.style
&scoped-define SORTBY-6 BY itemfg.procat
&scoped-define fld-name-1 itemfg.i-no
&scoped-define fld-name-2 itemfg.i-name
&scoped-define fld-name-3 itemfg.part-dscr1
&scoped-define fld-name-4 TRIM(itemfg.est-no)
&scoped-define fld-name-5 itemfg.style
&scoped-define fld-name-6 itemfg.procat
&scoped-define IAMWHAT LOOKUP
&SCOPED-DEFINE useMatches

ASSIGN cocode = ip-company .

IF  PROGRAM-NAME(2) MATCHES "*/b-itemfg.w*"   THEN
    v-check-page = YES .

DO TRANSACTION:
     {sys/ref/CustList.i NEW}
    {sys/inc/custlistform.i ""IF1"" }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES itemfg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 itemfg.i-no itemfg.i-name ~
itemfg.part-dscr1 itemfg.est-no itemfg.style itemfg.procat 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH itemfg WHERE ~{&KEY-PHRASE} ~
      AND itemfg.company = ip-company  ~
      AND ((v-check-page AND ( (lookup(itemfg.cust-no,custcount) <> 0 AND itemfg.cust-no <> "") OR custcount = "")) OR NOT v-check-page) NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH itemfg WHERE ~{&KEY-PHRASE} ~
      AND itemfg.company = ip-company   ~
      AND ((v-check-page AND ( (lookup(itemfg.cust-no,custcount) <> 0 AND itemfg.cust-no <> "") OR custcount = "")) OR NOT v-check-page) NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 itemfg


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 BROWSE-1 rd-sort bt-clear bt-cancel ~
bt-ok lv-search 
&Scoped-Define DISPLAYED-OBJECTS rd-sort lv-search 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 11 BY 1.14.

DEFINE BUTTON bt-clear 
     LABEL "C&lear Find" 
     SIZE 12 BY 1.14.

DEFINE BUTTON bt-ok 
     LABEL "&OK" 
     SIZE 11 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 131 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item No", 1,
"Name", 2,
"Description", 3,
"Estimate", 4,
"Style", 5,
"Category", 6
     SIZE 125.4 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 131 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      itemfg.i-no FORMAT "x(15)":U WIDTH 21.6
      itemfg.i-name FORMAT "x(30)":U WIDTH 41.8
      itemfg.part-dscr1 COLUMN-LABEL "Description" FORMAT "x(30)":U
            WIDTH 32.6
      itemfg.est-no COLUMN-LABEL "Estimate" FORMAT "x(10)":U
      itemfg.style COLUMN-LABEL "Style" FORMAT "x(6)":U
      itemfg.procat FORMAT "x(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 131.4 BY 13.33
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1.4
     rd-sort AT ROW 14.67 COL 6.6 NO-LABEL
     bt-clear AT ROW 16 COL 1.4
     bt-cancel AT ROW 16 COL 107.6
     bt-ok AT ROW 16 COL 121.6
     lv-search AT ROW 17.19 COL 1.4 NO-LABEL
     "By:" VIEW-AS TEXT
          SIZE 3.4 BY .62 AT ROW 14.76 COL 2.6
     RECT-1 AT ROW 14.38 COL 1.4
     SPACE(0.59) SKIP(2.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Finished Goods Information".


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
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 RECT-1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN lv-search IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "ASI.itemfg"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.itemfg.company = ip-company
"
     _FldNameList[1]   > ASI.itemfg.i-no
"itemfg.i-no" ? ? "character" ? ? ? ? ? ? no ? no no "21.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.itemfg.i-name
"itemfg.i-name" ? ? "character" ? ? ? ? ? ? no ? no no "41.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.itemfg.part-dscr1
"itemfg.part-dscr1" "Description" ? "character" ? ? ? ? ? ? no ? no no "32.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.itemfg.est-no
"itemfg.est-no" "Estimate" "x(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.itemfg.style
"itemfg.style" "Style" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = ASI.itemfg.procat
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Finished Goods Information */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON ANY-PRINTABLE OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   if lv-first-time then assign lv-search:screen-value = ""
                                lv-first-time = no.
                                
   lv-search:screen-value = lv-search:screen-value + keylabel(lastkey).
   
   apply "leave" to lv-search. 
   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:
   op-char-val = itemfg.i-no:screen-value in browse {&browse-name} + "," +
                 itemfg.i-name:screen-value in browse {&browse-name} + "," + 
                 itemfg.est-no:screen-value in browse {&browse-name} + "," +
                 itemfg.style:screen-value in browse {&browse-name} + "," +
                 itemfg.procat:screen-value in browse {&browse-name}.
    apply "window-close" to frame {&frame-name}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-clear Dialog-Frame
ON CHOOSE OF bt-clear IN FRAME Dialog-Frame /* Clear Find */
DO:
    assign lv-search:screen-value = "".
           lv-search = "".

           RUN open-query.

/*     case rd-sort:      */
/*         {srtord2.i 1}  */
/*         {srtord2.i 2}  */
/*         {srtord2.i 3}  */
/*         {srtord2.i 4}  */
/*         {srtord2.i 5}  */
/*         {srtord2.i 6}  */
/*     end.               */

    apply "entry" to {&browse-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
   op-char-val = itemfg.i-no:screen-value in browse {&browse-name} + "," +
                 itemfg.i-name:screen-value in browse {&browse-name} + "," + 
                 itemfg.est-no:screen-value in browse {&browse-name} + "," +
                 itemfg.style:screen-value in browse {&browse-name} + "," +
                 itemfg.procat:screen-value in browse {&browse-name}.

   apply "window-close" to frame {&frame-name}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search Dialog-Frame
ON LEAVE OF lv-search IN FRAME Dialog-Frame
OR RETURN OF lv-search
DO:
    
    assign rd-sort 
           lv-search.

    RUN open-query.
 
    apply "entry" to {&browse-name}.

    IF keyfunction(lastkey) = "Return" THEN RETURN NO-APPLY. 
 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort Dialog-Frame
ON VALUE-CHANGED OF rd-sort IN FRAME Dialog-Frame
DO:
    /* redefined for lookup */
    &scoped-define IAMWHAT LOOKUP   
         
    assign rd-sort.

    RUN open-query.

/*     case rd-sort:      */
/*         {srtord2.i 1}  */
/*         {srtord2.i 2}  */
/*         {srtord2.i 3}  */
/*         {srtord2.i 4}  */
/*         {srtord2.i 5}  */
/*         {srtord2.i 6}  */
/*     end.               */
    apply "entry" to {&browse-name}.
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

/*   &scoped-define key-phrase {&fld-name-1} >= ip-cur-val  */
/*   &scoped-define sortby-phrase {&sortby-1}               */

    RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'IF1',
                            INPUT YES,
                            OUTPUT lActive).
{sys/inc/chblankcust.i ""IF1""}
    IF ou-cust-int = 0 THEN
        custcount = "" .
  
  RUN enable_UI.
  CASE ip-field:
     WHEN 2 THEN
        ASSIGN rd-sort = 2.
     WHEN 3 THEN
        ASSIGN rd-sort = 4.
     WHEN 4 THEN
        ASSIGN rd-sort = 5.
     WHEN 5 THEN
        ASSIGN rd-sort = 6.
  END CASE.
  ASSIGN
     lv-search = ip-cur-val.
  DISPLAY rd-sort lv-search WITH FRAME {&FRAME-NAME}.
  APPLY "Return" TO lv-search.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY rd-sort lv-search 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 BROWSE-1 rd-sort bt-clear bt-cancel bt-ok lv-search 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open-query Dialog-Frame 
PROCEDURE open-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

CASE rd-sort:
    WHEN 1 THEN DO:
          IF INDEX(lv-search,'*') NE 0 THEN
              OPEN QUERY BROWSE-1 
            FOR EACH itemfg WHERE itemfg.i-no MATCHES (lv-search) AND
             itemfg.company = ip-company AND 
             ((v-check-page AND ( (lookup(itemfg.cust-no,custcount) <>  0 AND itemfg.cust-no <> "") OR custcount = "")) OR NOT v-check-page) NO-LOCK BY itemfg.i-no.
        ELSE
                OPEN QUERY BROWSE-1 
            FOR EACH itemfg WHERE itemfg.i-no GE (lv-search) AND
             itemfg.company = ip-company AND 
             ((v-check-page AND ( (lookup(itemfg.cust-no,custcount) <> 0 AND itemfg.cust-no <> "") OR custcount = "")) OR NOT v-check-page)  NO-LOCK BY itemfg.i-no.
            END.
    WHEN 2 THEN DO:
        IF INDEX(lv-search,'*') NE 0 THEN
        OPEN QUERY BROWSE-1 
            FOR EACH itemfg WHERE itemfg.i-name MATCHES (lv-search) AND
             itemfg.company = ip-company AND 
             ((v-check-page AND ( (lookup(itemfg.cust-no,custcount) <> 0 AND itemfg.cust-no <> "") OR custcount = "")) OR NOT v-check-page)   NO-LOCK BY itemfg.i-name.
        ELSE
            OPEN QUERY BROWSE-1 
                FOR EACH itemfg WHERE itemfg.i-name BEGINS (lv-search) AND
                 itemfg.company = ip-company AND 
                ((v-check-page AND ( (lookup(itemfg.cust-no,custcount) <> 0 AND itemfg.cust-no <> "") OR custcount = "")) OR NOT v-check-page)   NO-LOCK BY itemfg.i-name.

    END.

    WHEN 3 THEN DO:

        IF INDEX(lv-search,'*') NE 0 THEN
        OPEN QUERY BROWSE-1 
            FOR EACH itemfg WHERE itemfg.part-dscr1 MATCHES (lv-search) AND
             itemfg.company = ip-company AND 
             ((v-check-page AND ( (lookup(itemfg.cust-no,custcount) <> 0 AND itemfg.cust-no <> "") OR custcount = "")) OR NOT v-check-page)  NO-LOCK BY itemfg.part-dscr1.
        ELSE
            OPEN QUERY BROWSE-1 
                FOR EACH itemfg WHERE itemfg.part-dscr1 BEGINS (lv-search) AND
                 itemfg.company = ip-company AND
                ((v-check-page AND ( (lookup(itemfg.cust-no,custcount) <> 0 AND itemfg.cust-no <> "") OR custcount = "")) OR NOT v-check-page)   NO-LOCK BY itemfg.part-dscr1.
    END.

    /* est-no */
    WHEN 4 THEN DO:
        lv-search = FILL(" ",8 - LENGTH(TRIM(lv-search))) + TRIM(lv-search).

        OPEN QUERY BROWSE-1 
            FOR EACH itemfg WHERE (itemfg.est-no BEGINS (lv-search) OR (itemfg.est-no MATCHES lv-search AND trim(lv-search) BEGINS "*")) AND
             itemfg.company = ip-company AND
            ((v-check-page AND ( (lookup(itemfg.cust-no,custcount) <> 0 AND itemfg.cust-no <> "") OR custcount = "")) OR NOT v-check-page)   NO-LOCK BY itemfg.est-no.
        lv-search = TRIM(lv-search) .
    END.


    /* style */
    WHEN 5 THEN DO:

        OPEN QUERY BROWSE-1 
            FOR EACH itemfg WHERE (itemfg.style BEGINS (lv-search) OR (itemfg.style MATCHES lv-search AND trim(lv-search) BEGINS "*")) AND
             itemfg.company = ip-company AND
             ((v-check-page AND ( (lookup(itemfg.cust-no,custcount) <> 0 AND itemfg.cust-no <> "") OR custcount = "")) OR NOT v-check-page)   NO-LOCK BY itemfg.style.

    END.


    /* procat */
    WHEN 6 THEN DO:

        OPEN QUERY BROWSE-1 
            FOR EACH itemfg WHERE (itemfg.procat BEGINS (lv-search) OR (itemfg.procat MATCHES lv-search AND trim(lv-search) BEGINS "*"))  AND
             itemfg.company = ip-company AND
            ((v-check-page AND ( (lookup(itemfg.cust-no,custcount) <> 0 AND itemfg.cust-no <> "") OR custcount = "")) OR NOT v-check-page)  NO-LOCK BY itemfg.procat.

    END.

END CASE.


  IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
     DO:
        MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
        VIEW-AS ALERT-BOX.
    /*    lv-search:screen-value = "".  */
         APPLY "ENTRY" TO {&BROWSE-NAME}.
     end.    

    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


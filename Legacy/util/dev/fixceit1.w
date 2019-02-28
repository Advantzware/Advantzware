&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR ll-valid AS LOG NO-UNDO.

DEF BUFFER b-eb FOR eb.

&SCOPED-DEFINE find-eb                                    ~
    FIND FIRST b-eb NO-LOCK                               ~
        WHERE b-eb.company    EQ cocode                   ~
          AND b-eb.est-no     BEGINS ls-est-no            ~
          AND b-eb.cust-no    BEGINS ls-cust-no           ~
          AND b-eb.part-no    BEGINS ls-part-no           ~
          AND b-eb.part-dscr1 BEGINS ls-i-name            ~
          AND b-eb.stock-no   BEGINS ls-i-no              ~
          AND b-eb.die-no     BEGINS ls-die-no            ~
          AND b-eb.cad-no     BEGINS ls-cad-no            ~
          AND (b-eb.len       EQ ld-len OR ld-len EQ 0)

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES eb

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table eb.form-no eb.blank-no eb.part-no ~
eb.stock-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table eb.stock-no 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table eb
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table eb
&Scoped-define QUERY-STRING-br_table FOR EACH eb WHERE ~{&KEY-PHRASE} ~
      AND eb.company eq cocode and ~
eb.est-no  eq ls-est-no NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH eb WHERE ~{&KEY-PHRASE} ~
      AND eb.company eq cocode and ~
eb.est-no  eq ls-est-no NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table eb
&Scoped-define FIRST-TABLE-IN-QUERY-br_table eb


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ls-est-no ls-cust-no ls-part-no ls-i-name ~
ls-i-no ls-die-no ls-cad-no ld-len BUTTON-3 br_table 
&Scoped-Define DISPLAYED-OBJECTS ls-est-no ls-cust-no ls-part-no ls-i-name ~
ls-i-no ls-die-no ls-cad-no ld-len 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 ls-est-no ls-cust-no ls-part-no ls-i-name ls-i-no ~
ls-die-no ls-cad-no ld-len 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|asi.eb.company
Carrier||y|asi.eb.Carrier
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,Carrier"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-3 
     LABEL "&Get Estimate" 
     SIZE 29 BY 1.62.

DEFINE VARIABLE ld-len AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Length" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE ls-cad-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "CAD Number" 
     VIEW-AS FILL-IN 
     SIZE 28.8 BY 1 NO-UNDO.

DEFINE VARIABLE ls-cust-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer#" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ls-die-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "Die Number" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE ls-est-no AS CHARACTER FORMAT "X(8)":U 
     LABEL "Estimate#" 
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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      eb SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      eb.form-no COLUMN-LABEL "S#" FORMAT ">9":U WIDTH 4
      eb.blank-no COLUMN-LABEL "B#" FORMAT ">9":U WIDTH 4
      eb.part-no FORMAT "x(15)":U WIDTH 23.2
      eb.stock-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U WIDTH 22
  ENABLE
      eb.stock-no
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 76 BY 6.91
         BGCOLOR 8 FONT 6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ls-est-no AT ROW 1.24 COL 17 COLON-ALIGNED
     ls-cust-no AT ROW 2.19 COL 17 COLON-ALIGNED
     ls-part-no AT ROW 3.14 COL 17 COLON-ALIGNED
     ls-i-name AT ROW 4.1 COL 17 COLON-ALIGNED
     ls-i-no AT ROW 5.05 COL 17 COLON-ALIGNED
     ls-die-no AT ROW 6 COL 17 COLON-ALIGNED
     ls-cad-no AT ROW 6.95 COL 17 COLON-ALIGNED
     ld-len AT ROW 7.91 COL 17 COLON-ALIGNED
     BUTTON-3 AT ROW 8.38 COL 41
     br_table AT ROW 10.52 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6
         DEFAULT-BUTTON BUTTON-3.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 16.57
         WIDTH              = 78.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table BUTTON-3 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

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
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "asi.eb"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "eb.company eq cocode and
eb.est-no  eq ls-est-no"
     _FldNameList[1]   > asi.eb.form-no
"eb.form-no" "S#" ? "integer" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" ""
     _FldNameList[2]   > asi.eb.blank-no
"eb.blank-no" "B#" ? "integer" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" ""
     _FldNameList[3]   > asi.eb.part-no
"eb.part-no" ? ? "character" ? ? ? ? ? ? no ? no no "23.2" yes no no "U" "" ""
     _FldNameList[4]   > asi.eb.stock-no
"eb.stock-no" "FG Item#" ? "character" ? ? ? ? ? ? yes ? no no "22" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main B-table-Win
ON HELP OF FRAME F-Main
DO:
   def var lv-handle as widget-handle no-undo.
   def var ls-cur-val as cha no-undo.
   def var ls-int-val as INT no-undo.
   DEF VAR lv-eb-tmpid AS RECID NO-UNDO.
   
   DEF VAR char-val AS cha NO-UNDO.
  
   case focus:name :
      when "ls-est-no" then do:
              run windows/l-est.w (g_company,g_loc,focus:screen-value, output char-val).
              if char-val <> "" then do:                 
                 FIND FIRST eb WHERE string(RECID(eb)) = (char-val) NO-LOCK NO-ERROR.
                 IF AVAIL eb THEN ASSIGN FOCUS:SCREEN-VALUE = eb.est-no
                                  ls-cust-no:screen-value in frame {&frame-name} =  eb.cust-no
                                  ls-est-no:SCREEN-VALUE = eb.est-no
                                  ls-part-no:SCREEN-VALUE = eb.part-no
                                  ls-i-name:SCREEN-VALUE = eb.part-dscr1
                                  ls-i-no:SCREEN-VALUE = eb.stock-no
                                  ls-die-no:SCREEN-VALUE = eb.die-no
                                  ls-cad-no:SCREEN-VALUE = eb.cad-no
                                  ld-len:SCREEN-VALUE = STRING(eb.len).
              end.                
      end.   
      when "ls-part-no" then do: 
           run est/l-ebrfqP.w (g_company, g_loc, focus:screen-value, output lv-eb-tmpid) .
           FIND FIRST eb WHERE RECID(eb) = lv-eb-tmpid NO-LOCK NO-ERROR.
                 IF AVAIL eb THEN ASSIGN FOCUS:SCREEN-VALUE = eb.part-no
                                  ls-cust-no:screen-value in frame {&frame-name} =  eb.cust-no
                                  ls-est-no:SCREEN-VALUE = eb.est-no
                                  ls-part-no:SCREEN-VALUE = eb.part-no
                                  ls-i-name:SCREEN-VALUE = eb.part-dscr1
                                  ls-i-no:SCREEN-VALUE = eb.stock-no
                                  ls-die-no:SCREEN-VALUE = eb.die-no
                                  ls-cad-no:SCREEN-VALUE = eb.cad-no
                                  ld-len:SCREEN-VALUE = STRING(eb.len) .
           return no-apply.          
      end.
      when "ls-i-no" then do:
        /* run windows/l-itemfg.w  (gcompany, output char-val). */
           run est/l-ebrfq.w (g_company, g_loc,focus:screen-value, output lv-eb-tmpid) .           
           FIND FIRST eb WHERE RECID(eb) = lv-eb-tmpid NO-LOCK NO-ERROR.
                 IF AVAIL eb THEN 
                     ASSIGN FOCUS:SCREEN-VALUE = eb.stock-no
                         ls-cust-no:screen-value in frame {&frame-name} =  eb.cust-no
                         ls-est-no:SCREEN-VALUE = eb.est-no
                         ls-part-no:SCREEN-VALUE = eb.part-no
                         ls-i-name:SCREEN-VALUE = eb.part-dscr1
                         ls-i-no:SCREEN-VALUE = eb.stock-no
                         ls-die-no:SCREEN-VALUE = eb.die-no
                         ls-cad-no:SCREEN-VALUE = eb.cad-no
                         ld-len:SCREEN-VALUE = STRING(eb.len).
           return no-apply.
      end.
      when "ls-cust-no" OR WHEN "ls-i-name" OR WHEN "ls-die-no" OR WHEN "ls-cad-no" OR WHEN "ld-len"
      then do:
           ASSIGN
            ls-cur-val = focus:SCREEN-VALUE
            ls-int-val = IF FOCUS:NAME EQ "ld-len"    THEN 8 ELSE
                         IF FOCUS:NAME EQ "ls-cad-no" THEN 7 ELSE
                         IF FOCUS:NAME EQ "ls-die-no" THEN 6 ELSE
                         IF FOCUS:NAME EQ "ls-i-name" THEN 5 ELSE 2.
           run est/l-ebcst.w (g_company,g_loc,ls-cur-val,ls-int-val, output lv-eb-tmpid).
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
                         ld-len:SCREEN-VALUE = string(eb.len).
           end.
           return no-apply.
       end.  /* cust-no*/
         
  end case.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  ll-valid = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME eb.stock-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stock-no br_table _BROWSE-COLUMN B-table-Win
ON HELP OF eb.stock-no IN BROWSE br_table /* FG Item# */
DO:          
  def var char-val as cha no-undo.
  def var look-recid as recid no-undo.


  run windows/l-itemfa.w (g_company, eb.cust-no, focus:screen-value, output char-val, output look-recid).
  if char-val NE "" then focus:screen-value = entry(1,char-val).
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stock-no br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF eb.stock-no IN BROWSE br_table /* FG Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-stock-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL eb.stock-no br_table _BROWSE-COLUMN B-table-Win
ON VALUE-CHANGED OF eb.stock-no IN BROWSE br_table /* FG Item# */
DO:
  ll-valid = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 B-table-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Get Estimate */
DO:
  ls-est-no:SCREEN-VALUE = FILL(" ",8 - LENGTH(TRIM(ls-est-no:SCREEN-VALUE))) +
                           TRIM(ls-est-no:SCREEN-VALUE).

  ASSIGN {&List-1}.

  IF ls-est-no NE "" THEN {&find-eb} USE-INDEX est-no NO-ERROR.

  ELSE
  IF ls-cust-no NE "" THEN {&find-eb} USE-INDEX cust NO-ERROR.

  ELSE
  IF ls-part-no NE "" THEN {&find-eb} USE-INDEX part NO-ERROR.

  ELSE
  IF ls-i-name NE "" THEN {&find-eb} USE-INDEX pdscr NO-ERROR.

  ELSE
  IF ls-i-no NE "" THEN {&find-eb} USE-INDEX stock NO-ERROR.

  ELSE
  IF ls-die-no NE "" THEN {&find-eb} USE-INDEX die NO-ERROR.

  ELSE
  IF ls-cad-no NE "" THEN {&find-eb} USE-INDEX cad NO-ERROR.

  ELSE
  IF ld-len NE 0 THEN {&find-eb} NO-ERROR.

  IF AVAIL b-eb THEN DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ls-est-no:SCREEN-VALUE  = b-eb.est-no
     ls-cust-no:SCREEN-VALUE = ""
     ls-part-no:SCREEN-VALUE = ""
     ls-i-name:SCREEN-VALUE  = ""
     ls-i-no:SCREEN-VALUE    = ""
     ls-die-no:SCREEN-VALUE  = ""
     ls-cad-no:SCREEN-VALUE  = ""
     ld-len:SCREEN-VALUE     = "".

    ASSIGN {&List-1}.

    APPLY "entry" TO ls-est-no.

    RUN dispatch ("open-query").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ld-len
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ld-len B-table-Win
ON LEAVE OF ld-len IN FRAME F-Main /* Length */
DO:
   ASSIGN ls-est-no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-cad-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-cad-no B-table-Win
ON LEAVE OF ls-cad-no IN FRAME F-Main /* CAD Number */
DO:
   ASSIGN ls-est-no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-cust-no B-table-Win
ON LEAVE OF ls-cust-no IN FRAME F-Main /* Customer# */
DO:
  ASSIGN ls-est-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-die-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-die-no B-table-Win
ON LEAVE OF ls-die-no IN FRAME F-Main /* Die Number */
DO:
   ASSIGN ls-est-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-est-no B-table-Win
ON LEAVE OF ls-est-no IN FRAME F-Main /* Estimate# */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    {&self-name}:SCREEN-VALUE = 
        FILL(" ",8 - LENGTH({&self-name}:SCREEN-VALUE)) + {&self-name}:SCREEN-VALUE.

    ASSIGN {&self-name}.

    FIND FIRST eb
        WHERE eb.company EQ cocode
          AND eb.est-no  EQ ls-est-no
        NO-LOCK NO-ERROR.

    IF NOT AVAIL eb THEN DO:
      MESSAGE "Estimate does not exist..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO {&self-name}.
      RETURN NO-APPLY.
    END.

    ls-cust-no:SCREEN-VALUE = eb.cust-no.

    FIND FIRST cust
        WHERE cust.company EQ eb.company
          AND cust.cust-no EQ eb.cust-no
        NO-LOCK NO-ERROR.

    lv-name:SCREEN-VALUE = IF AVAIL cust THEN cust.name ELSE "Not on file...".

    RUN dispatch ("open-query").
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-i-name B-table-Win
ON LEAVE OF ls-i-name IN FRAME F-Main /* Item Name */
DO:
   ASSIGN ls-est-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-i-no B-table-Win
ON LEAVE OF ls-i-no IN FRAME F-Main /* FG Item# */
DO:
   ASSIGN ls-est-no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-part-no B-table-Win
ON LEAVE OF ls-part-no IN FRAME F-Main /* Cust Part */
DO:
   ASSIGN ls-est-no.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win 
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR old-i-no LIKE eb.stock-no NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-stock-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  old-i-no = eb.stock-no.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  SESSION:SET-WAIT-STATE ("general").

  IF old-i-no NE eb.stock-no THEN DO:
    IF eb.stock-no NE "" THEN DO TRANSACTION:
      FOR EACH oe-ordl
          WHERE oe-ordl.company EQ eb.company
            AND oe-ordl.i-no    EQ old-i-no
            AND oe-ordl.est-no  EQ eb.est-no:
        oe-ordl.i-no = eb.stock-no.
      END.

      FOR EACH job-hdr
          WHERE job-hdr.company EQ eb.company
            AND job-hdr.i-no    EQ old-i-no
            AND job-hdr.est-no  EQ est.est-no:
        job-hdr.i-no = eb.stock-no.
      END.

      FOR EACH job
          WHERE job.company EQ eb.company
            AND job.est-no  EQ eb.est-no
          NO-LOCK,
          EACH reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job-hdr.job,"999999999")
            AND reftable.code2    EQ old-i-no
            AND reftable.val[12]  EQ eb.form-no
            AND reftable.val[13]  EQ eb.blank-no:
        reftable.code2 = eb.stock-no.
      END.
    END.

    RELEASE itemfg.
    IF old-i-no NE "" THEN
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ old-i-no
        NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN RUN fg/fg-reset.p (RECID(itemfg)).

    RELEASE itemfg.
    IF eb.stock-no NE "" THEN
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ eb.stock-no
        NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN RUN fg/fg-reset.p (RECID(itemfg)).

    SESSION:SET-WAIT-STATE ("").
  END.

  RUN dispatch ("display-fields").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "eb" "company"}
  {src/adm/template/sndkycas.i "Carrier" "eb" "Carrier"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "eb"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-stock-no B-table-Win 
PROCEDURE valid-stock-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eb FOR eb.


  DO WITH FRAME {&FRAME-NAME}:
    IF eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND
       NOT CAN-FIND(FIRST itemfg
                    WHERE itemfg.company EQ cocode
                      AND itemfg.i-no    EQ eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE TRIM(eb.stock-no:LABEL IN BROWSE {&browse-name}) +
              " does not exist FG File..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO eb.stock-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.

    IF NOT ll-valid                                            AND
       eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" AND
       NOT CAN-FIND(FIRST b-eb
                    WHERE b-eb.company  EQ eb.company
                      AND b-eb.stock-no EQ eb.stock-no
                      AND b-eb.est-no   EQ eb.est-no
                      AND ROWID(b-eb)   NE ROWID(eb))          AND
       (CAN-FIND(FIRST oe-ordl
                 WHERE oe-ordl.company EQ eb.company
                   AND oe-ordl.i-no    EQ eb.stock-no
                   AND oe-ordl.est-no  EQ eb.est-no) OR
        CAN-FIND(FIRST job-hdr
                 WHERE job-hdr.company EQ eb.company
                   AND job-hdr.i-no    EQ eb.stock-no
                   AND job-hdr.est-no  EQ eb.est-no))          THEN DO:
      MESSAGE TRIM(eb.stock-no:LABEL IN BROWSE {&browse-name})  +
              " should not be blank when an order/job exists,"  +
              " continue?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll-valid.
      IF NOT ll-valid THEN DO:
        APPLY "entry" TO eb.stock-no IN BROWSE {&browse-name}.
        RETURN ERROR.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


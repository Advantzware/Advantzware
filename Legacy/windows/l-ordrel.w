&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File:       Lookup for the loadtags never received, only created

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
DEFINE INPUT PARAMETER ip-company LIKE itemfg.company NO-UNDO.
DEFINE INPUT PARAMETER ip-order AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-item AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-char-val AS CHARACTER NO-UNDO. /* string i-code + i-name */
DEFINE OUTPUT PARAMETER op-rec-val AS RECID NO-UNDO.

DEFINE VARIABLE lv-type-dscr AS cha NO-UNDO.
DEFINE VARIABLE ll-casetag   AS LOG NO-UNDO.

DEFINE TEMP-TABLE tt-rel-order LIKE oe-rel
    FIELD release#     AS INTEGER 
    FIELD ord-rel-date AS DATE  .

&scoped-define fld-name-1 string(tt-rel-order.release#)
&scoped-define fld-name-2 tt-rel-order.po-no
&scoped-define fld-name-3 STRING(tt-rel-order.ord-rel-date)
&scoped-define SORTBY-1 BY tt-rel-order.release#
&scoped-define SORTBY-2 BY tt-rel-order.po-no
&scoped-define SORTBY-3 BY tt-rel-order.ord-rel-date

/*&scoped-define datatype-1 integer*/

&global-define IAMWHAT LOOKUP

DEFINE VARIABLE lv-first-time AS LOG INIT YES NO-UNDO.

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED }
{sys/inc/varasgn.i}



DEFINE VARIABLE v-prgmname LIKE prgrms.prgmname NO-UNDO.
DEFINE VARIABLE period_pos AS INTEGER NO-UNDO.

IF INDEX(PROGRAM-NAME(1),".uib") NE 0 OR
    INDEX(PROGRAM-NAME(1),".ab")  NE 0 OR
    INDEX(PROGRAM-NAME(1),".ped") NE 0 THEN
    v-prgmname = USERID("NOSWEAT") + "..".
ELSE
    ASSIGN
        period_pos = INDEX(PROGRAM-NAME(1),".")
        v-prgmname = SUBSTR(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"/",period_pos - 9) + 1)
        v-prgmname = SUBSTR(v-prgmname,1,INDEX(v-prgmname,".")).


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-rel-order 

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tt-rel-order.ord-no tt-rel-order.release# ~
tt-rel-order.s-code tt-rel-order.ord-rel-date tt-rel-order.spare-char-1 tt-rel-order.ship-id tt-rel-order.po-no ~
tt-rel-order.tot-qty tt-rel-order.qty 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH tt-rel-order WHERE ~{&KEY-PHRASE} ~
      AND tt-rel-order.company = ip-company NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH tt-rel-order WHERE ~{&KEY-PHRASE} ~
      AND tt-rel-order.company = ip-company  NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tt-rel-order 
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tt-rel-order

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 RECT-1 rd-sort bt-clear lv-search ~
bt-ok bt-cancel 
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
    SIZE 12.4 BY 1.14.

DEFINE BUTTON bt-ok 
    LABEL "&OK" 
    SIZE 10 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
    LABEL "Search" 
    VIEW-AS FILL-IN 
    SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort   AS INTEGER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "Release", 1,
    "Cust Po", 2,
    "Date", 3
    SIZE 56 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
    SIZE 137 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
    tt-rel-order
    SCROLLING.
&ANALYZE-RESUME
    
/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
    QUERY BROWSE-1 NO-LOCK DISPLAY
    tt-rel-order.ord-no COLUMN-LABEL "Order" FORMAT ">>>>>>9":U
    tt-rel-order.release# COLUMN-LABEL "Release" 
    tt-rel-order.s-code COLUMN-LABEL "S/I"  FORMAT "x(10)":U
    VIEW-AS COMBO-BOX INNER-LINES 4 
    LIST-ITEM-PAIRS "B-Both","B",
    "S-Ship","S",
    "I-Invoice","I",
    "T-Transfer","T"
    DROP-DOWN-LIST
    tt-rel-order.ord-rel-date COLUMN-LABEL "Rel Date" FORMAT "99/99/9999":U
    tt-rel-order.spare-char-1 COLUMN-LABEL "From" FORMAT "x(8)":U
    tt-rel-order.ship-id COLUMN-LABEL "To" FORMAT "x(10)":U
    tt-rel-order.po-no COLUMN-LABEL "Cust PO#" FORMAT "x(15)":U
    tt-rel-order.tot-qty COLUMN-LABEL "Schedueld Qty" FORMAT "->>>>>9":U
    tt-rel-order.qty COLUMN-LABEL "Actual Qty" FORMAT "->>>>>>9":U
      
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 137 BY 11.19
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    BROWSE-1 AT ROW 1 COL 1
    rd-sort AT ROW 12.67 COL 14 NO-LABELS
    bt-clear AT ROW 14.1 COL 2
    lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
    bt-ok AT ROW 14.1 COL 69
    bt-cancel AT ROW 14.1 COL 81
    "Sort By:" VIEW-AS TEXT
    SIZE 8 BY .62 AT ROW 12.91 COL 4
    RECT-1 AT ROW 12.43 COL 1
    SPACE(0.19) SKIP(1.38)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    TITLE "Order Release Information".


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
                                                                        */
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "ASI.tt-rel-order"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST OUTER"
     _Where[1]         = "ASI.tt-rel-order.company = ip-company 


"
     _FldNameList[1]   > ASI.tt-rel-order.ord-no
"ord-no" ? ">>>>>>>>" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   = ASI.tt-rel-order.release#
     _FldNameList[3]   = ASI.tt-rel-order.s-code
     _FldNameList[4]   > ASI.tt-rel-order.ord-rel-date
"ord-rel-date" "Rel Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > ASI.tt-rel-order.spare-char-1
"spare-char-1" "" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   = ASI.tt-rel-order.ship-id
     _FldNameList[7]   > ASI.tt-rel-order.po-no
"po-no" "Cust Po" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   = ASI.tt-rel-order.tot-qty
     _FldNameList[9]   > ASI.tt-rel-order.qty
"qty" "Actual Qty" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Loagtag Information */
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
        IF lv-first-time THEN ASSIGN lv-search:screen-value = ""
                lv-first-time          = NO.
                                
        lv-search:screen-value = lv-search:screen-value + keylabel(LASTKEY).
        APPLY "leave" TO lv-search.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
    DO:
        op-char-val = STRING(tt-rel-order.release#) .
        op-rec-val = RECID(tt-rel-order).
        APPLY "window-close" TO FRAME {&frame-name}. 
      
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-clear Dialog-Frame
ON CHOOSE OF bt-clear IN FRAME Dialog-Frame /* Clear Find */
    DO:
        ASSIGN 
            lv-search:screen-value = "".
        lv-search = "".
        /*RUN pBuildSearch .*/
        CASE rd-sort:
            {srtord2.i 1}
            {srtord2.i 2}
            {srtord2.i 3}
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
    DO:
        op-char-val = STRING(tt-rel-order.release#).
        op-rec-val = RECID(tt-rel-order).
        APPLY "window-close" TO FRAME {&frame-name}. 
      
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search Dialog-Frame
ON LEAVE OF lv-search IN FRAME Dialog-Frame /* Search */
    OR RETURN OF lv-search
    DO:
        ASSIGN rd-sort 
            lv-search.
     /*RUN pBuildSearch . */
    &scoped-define IAMWHAT Search
    &scoped-define where-statement BEGINS lv-search 
        CASE rd-sort:
            {srtord2.i 1}
            {srtord2.i 2}
            {srtord2.i 3}
        END.      
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-sort Dialog-Frame
ON VALUE-CHANGED OF rd-sort IN FRAME Dialog-Frame
    DO:
        ASSIGN rd-sort
            lv-search.
    &scoped-define IAMWHAT LOOKUP  
    
        CASE rd-sort:
            {srtord2.i 1}
            {srtord2.i 2}
            {srtord2.i 3}
        END.    
        /* APPLY "choose" TO bt-clear.
         APPLY "entry" TO BROWSE {&browse-name}.
         RETURN NO-APPLY.                       */
        lv-search = "".
        lv-search:SCREEN-VALUE = "".
        RUN new-rd-sort.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  
  
    /*&scoped-define key-phrase {&fld-name-2} >= ip-cur-val
    lv-search:screen-value in frame {&frame-name} = ip-cur-val.*/
   
    DO WITH FRAME {&FRAME-NAME}:
        {custom/usrprint.i}
        lv-search:SCREEN-VALUE = "".
    END.
    RUN pBuildSearch .

    RUN enable_UI.
    RUN new-rd-sort.

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
    ENABLE BROWSE-1 RECT-1 rd-sort bt-clear lv-search bt-ok bt-cancel 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-rd-sort Dialog-Frame 
PROCEDURE new-rd-sort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* redefined for lookup */
  &scoped-define IAMWHAT LOOKUP   
         
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN rd-sort.
        CASE rd-sort:
            {srtord.i 1}
            {srtord.i 2}
            {srtord.i 3}
        END.    
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
    APPLY 'entry' TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildSearch Dialog-Frame 
PROCEDURE pBuildSearch :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount  AS INTEGER   NO-UNDO .
    DEFINE VARIABLE cTagNo  AS CHARACTER NO-UNDO .
    DEFINE VARIABLE iItemNo AS CHARACTER NO-UNDO .
    DEFINE VARIABLE iJobNo  AS CHARACTER NO-UNDO .
    DEFINE VARIABLE lv-stat AS cha       NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
    
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company EQ ip-company
            AND oe-rel.ord-no  EQ int(ip-order)
            AND oe-rel.i-no    EQ ip-item
            USE-INDEX ord-item
      
            BREAK BY oe-rel.rel-no
            BY oe-rel.b-ord-no
            BY oe-rel.po-no

            TRANSACTION:

            IF LAST-OF(oe-rel.po-no) OR oe-rel.rel-no EQ 0 THEN 
            DO:
           
                FIND FIRST oe-rell
                    WHERE oe-rell.company  EQ oe-rel.company
                    AND oe-rell.r-no     EQ oe-rel.link-no
                    AND oe-rell.ord-no   EQ oe-rel.ord-no
                    AND oe-rell.rel-no   EQ oe-rel.rel-no
                    AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                    AND oe-rell.i-no     EQ oe-rel.i-no
                    AND oe-rell.line     EQ oe-rel.line
                    AND oe-rell.po-no    EQ oe-rel.po-no
                    AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
                    USE-INDEX r-no NO-LOCK NO-ERROR.
                IF NOT AVAILABLE oe-rell THEN
                    FIND FIRST oe-rell
                        WHERE oe-rell.company  EQ oe-rel.company
                        AND oe-rell.link-no  EQ oe-rel.r-no
                        AND oe-rell.ord-no   EQ oe-rel.ord-no
                        AND oe-rell.rel-no   EQ oe-rel.rel-no
                        AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                        AND oe-rell.i-no     EQ oe-rel.i-no
                        AND oe-rell.line     EQ oe-rel.line
                        AND oe-rell.po-no    EQ oe-rel.po-no
                        AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
                        NO-LOCK NO-ERROR.
                /* Needed because line was sometimes different between the two */
                IF NOT AVAILABLE oe-rell THEN
                    FIND FIRST oe-rell
                        WHERE oe-rell.company  EQ oe-rel.company
                        AND oe-rell.ord-no   EQ oe-rel.ord-no              
                        AND oe-rell.i-no     EQ oe-rel.i-no
                        AND oe-rell.link-no  EQ oe-rel.r-no
                        AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
                        NO-LOCK NO-ERROR.

                IF AVAILABLE oe-rell THEN
                    FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.

                IF AVAILABLE oe-relh THEN 
                DO: 
                    CREATE tt-rel-order .
                    BUFFER-COPY oe-rel TO tt-rel-order .  
                    ASSIGN
                        tt-rel-order.release#     = IF AVAILABLE oe-relh THEN oe-relh.release# ELSE 0
                        tt-rel-order.ord-rel-date = IF AVAILABLE oe-relh THEN oe-relh.rel-date
                          ELSE oe-rel.rel-date .
                END.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




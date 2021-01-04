&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: rm\d-trans.w

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
SESSION:DEBUG-ALERT = FALSE.

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO .   /* add,update,view */
DEFINE OUTPUT PARAMETER op-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN 
    cocode = g_company.
ASSIGN 
    locode = g_loc.

DEFINE NEW SHARED TEMP-TABLE tt-selected 
    FIELD tt-rowid AS ROWID.

DO TRANSACTION:
    {sys/inc/rmrecpt.i}
END.
DEFINE VARIABLE lv-item-recid   AS RECID   NO-UNDO.
DEFINE VARIABLE ll-order-warned AS LOGICAL NO-UNDO.
DEFINE VARIABLE ll-new-record   AS LOGICAL NO-UNDO.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rm-rctd

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame rm-rctd.r-no rm-rctd.rct-date ~
rm-rctd.i-no rm-rctd.i-name rm-rctd.tag rm-rctd.loc rm-rctd.loc-bin ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.loc2 rm-rctd.loc-bin2 rm-rctd.tag2 ~
rm-rctd.user-id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame rm-rctd.rct-date ~
rm-rctd.i-no rm-rctd.i-name rm-rctd.tag rm-rctd.loc rm-rctd.loc-bin ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.loc2 rm-rctd.loc-bin2 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame rm-rctd
&Scoped-define TABLES-IN-QUERY-Dialog-Frame rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame rm-rctd


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS rm-rctd.rct-date rm-rctd.i-no rm-rctd.i-name ~
rm-rctd.tag rm-rctd.loc rm-rctd.loc-bin rm-rctd.qty rm-rctd.pur-uom ~
rm-rctd.loc2 rm-rctd.loc-bin2 
&Scoped-define ENABLED-TABLES rm-rctd
&Scoped-define FIRST-ENABLED-TABLE rm-rctd
&Scoped-Define ENABLED-OBJECTS btnCalendar-1 Btn_OK Btn_Done Btn_Cancel ~
RECT-21 RECT-38 
&Scoped-Define DISPLAYED-FIELDS rm-rctd.r-no rm-rctd.rct-date rm-rctd.i-no ~
rm-rctd.i-name rm-rctd.tag rm-rctd.loc rm-rctd.loc-bin rm-rctd.qty ~
rm-rctd.pur-uom rm-rctd.loc2 rm-rctd.loc-bin2 rm-rctd.tag2 rm-rctd.user-id 
&Scoped-define DISPLAYED-TABLES rm-rctd
&Scoped-define FIRST-DISPLAYED-TABLE rm-rctd


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 btnCalendar-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
    IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
    LABEL "" 
    SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON Btn_Cancel 
    IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
    LABEL "Cancel" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
    LABEL "&Done" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
    IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U NO-FOCUS FLAT-BUTTON
    LABEL "&Save" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE RECTANGLE RECT-21
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 19 BY 2.38
    BGCOLOR 15 .

DEFINE RECTANGLE RECT-38
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 133.8 BY 9.71
    BGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
    rm-rctd SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    rm-rctd.r-no AT ROW 1.24 COL 29.8 COLON-ALIGNED
    LABEL "Seq#" FORMAT ">>>>>>>9"
    VIEW-AS FILL-IN 
    SIZE 19.2 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.rct-date AT ROW 2.43 COL 29.8 COLON-ALIGNED
    LABEL "Transfer Date" FORMAT "99/99/9999"
    VIEW-AS FILL-IN 
    SIZE 19.2 BY 1
    BGCOLOR 15 FONT 1
    btnCalendar-1 AT ROW 2.43 COL 51.4
    rm-rctd.i-no AT ROW 3.62 COL 29.8 COLON-ALIGNED
    LABEL "Item No" FORMAT "x(10)"
    VIEW-AS FILL-IN 
    SIZE 33.6 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.i-name AT ROW 4.76 COL 29.8 COLON-ALIGNED
    LABEL "Name" FORMAT "x(30)"
    VIEW-AS FILL-IN 
    SIZE 33.6 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.tag AT ROW 5.91 COL 29.8 COLON-ALIGNED
    LABEL "From Tag" FORMAT "x(20)"
    VIEW-AS FILL-IN 
    SIZE 33.6 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.loc AT ROW 7.19 COL 29.8 COLON-ALIGNED
    LABEL "From Whs" FORMAT "x(5)"
    VIEW-AS FILL-IN 
    SIZE 19 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.loc-bin AT ROW 8.48 COL 29.8 COLON-ALIGNED
    LABEL "From Bin" FORMAT "x(8)"
    VIEW-AS FILL-IN 
    SIZE 18.6 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.qty AT ROW 2.43 COL 85.4 COLON-ALIGNED
    LABEL "Qty" FORMAT "->>>>>>9.9<<<<<"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.pur-uom AT ROW 3.62 COL 85.4 COLON-ALIGNED
    LABEL "Uom" FORMAT "x(3)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.loc2 AT ROW 4.76 COL 85.4 COLON-ALIGNED FORMAT "x(5)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.loc-bin2 AT ROW 5.91 COL 85.4 COLON-ALIGNED
    LABEL "To Bin" FORMAT "x(8)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.tag2 AT ROW 7.19 COL 85.4 COLON-ALIGNED
    LABEL "To Tag" FORMAT "x(20)"
    VIEW-AS FILL-IN 
    SIZE 33 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.user-id AT ROW 8.48 COL 85.4 COLON-ALIGNED
    LABEL "User ID" FORMAT "x(8)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    Btn_OK AT ROW 11.24 COL 116
    Btn_Done AT ROW 11.52 COL 117
    Btn_Cancel AT ROW 11.24 COL 125
    RECT-21 AT ROW 11 COL 115
    RECT-38 AT ROW 1 COL 1
    SPACE(0.99) SKIP(3.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    FGCOLOR 1 FONT 6
    TITLE "Warehouse Transaction Transfers Update".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
/*{methods/template/viewer.i} */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME Dialog-Frame
   3                                                                    */
/* SETTINGS FOR FILL-IN rm-rctd.i-name IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.i-no IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.loc IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.loc-bin IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.loc-bin2 IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.loc2 IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rm-rctd.pur-uom IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.qty IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.r-no IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN rm-rctd.rct-date IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.tag IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.tag2 IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN rm-rctd.user-id IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "ASI.rm-rctd "
     _Options          = "SHARE-LOCK"
     _Where[1]         = "ASI.rm-rctd.company eq cocode "
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON HELP OF FRAME Dialog-Frame /* Warehouse Transaction Transfers Update */
    DO:
        DEFINE VARIABLE ll-tag#   AS LOG   NO-UNDO.
        DEFINE VARIABLE char-val  AS cha   NO-UNDO.
        DEFINE VARIABLE recid-val AS RECID NO-UNDO.
        DEFINE VARIABLE rowid-val AS ROWID NO-UNDO.
        
        CASE FOCUS:NAME :
            WHEN "i-no" THEN 
                DO:
                    RUN windows/l-itmre.w (rm-rctd.company, "", "", "R", FOCUS:SCREEN-VALUE , OUTPUT char-val, OUTPUT recid-val).
                    IF char-val <> "" THEN 
                    DO :
                        FOCUS:SCREEN-VALUE  = ENTRY(1,char-val).
                        APPLY "value-changed" TO FOCUS .
                    END.   
                END.

            WHEN "loc"     THEN 
                DO:
                    RUN rmbin-help. 
                    APPLY "ENTRY" TO rm-rctd.loc .  
                END.
            WHEN "loc-bin" THEN 
                DO:
                    RUN rmbin-help. 
                    APPLY "ENTRY" TO rm-rctd.loc-bin .  
                END.
            WHEN "tag"     THEN 
                DO:
                    RUN rmbin-help. 
                    APPLY "ENTRY" TO rm-rctd.tag .  
                END.

            WHEN "loc2" THEN 
                DO:
                    RUN rm/l-loc.w (rm-rctd.company, FOCUS:SCREEN-VALUE , OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO :
                        ASSIGN 
                            FOCUS:SCREEN-VALUE = ENTRY(1,char-val).             
                    END.
                END.
            WHEN "loc-bin2" THEN 
                DO:
                    RUN rm/l-locbin.w (rm-rctd.company, rm-rctd.loc2:screen-value , OUTPUT char-val).
                    IF char-val <> "" THEN 
                    DO :
                        ASSIGN 
                            FOCUS:SCREEN-VALUE = ENTRY(1,char-val).             
                    END.
                END.

        END CASE.

        RETURN NO-APPLY.


    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON RETURN OF FRAME Dialog-Frame /* Warehouse Transaction Transfers Update */
    ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Warehouse Transaction Transfers Update */
    DO:
        DISABLE TRIGGERS FOR LOAD OF rm-rctd .
    
        IF AVAILABLE rm-rctd THEN
            op-rowid = ROWID(rm-rctd) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST rm-rctd EXCLUSIVE-LOCK
                WHERE RECID(rm-rctd) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE rm-rctd THEN DELETE rm-rctd .
            op-rowid = ? .
        END.
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.

    /*APPLY "END-ERROR":U TO SELF.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 Dialog-Frame
ON CHOOSE OF btnCalendar-1 IN FRAME Dialog-Frame
    DO:
        {methods/btnCalendar.i rm-rctd.rct-date }
        APPLY "entry" TO rm-rctd.rct-date .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Cancel */
    DO:
        DISABLE TRIGGERS FOR LOAD OF rm-rctd .
    
        IF AVAILABLE rm-rctd THEN
            op-rowid = ROWID(rm-rctd) .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND FIRST rm-rctd EXCLUSIVE-LOCK
                WHERE RECID(rm-rctd) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE rm-rctd THEN DELETE rm-rctd .
            op-rowid = ? .
        END.
        APPLY 'GO':U TO FRAME {&FRAME-NAME}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done Dialog-Frame
ON CHOOSE OF Btn_Done IN FRAME Dialog-Frame /* Done */
    DO:
        IF AVAILABLE rm-rctd THEN
            ASSIGN op-rowid = ROWID(rm-rctd) .
  &IF DEFINED (adm-panel) NE 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
        APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Save */
    DO:
        DEFINE VARIABLE lv-tag2 LIKE rm-rctd.tag2 NO-UNDO.

        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
   
        RUN valid-i-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-tag NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-loc-bin-tag NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-loc2 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-loc-bin2 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  
        DO TRANSACTION:
            lv-tag2 = rm-rctd.tag2:SCREEN-VALUE .
            FIND CURRENT rm-rctd EXCLUSIVE-LOCK NO-ERROR.

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
            END.
        END.

        ASSIGN 
            rm-rctd.tag2      = lv-tag2
             .
        FIND CURRENT rm-rctd NO-LOCK NO-ERROR .
        op-rowid = ROWID(rm-rctd).

        APPLY "go" TO FRAME {&FRAME-NAME}.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-name Dialog-Frame
ON ENTRY OF rm-rctd.i-name IN FRAME Dialog-Frame /* Name */
    DO:
        APPLY "tab" TO {&self-name} .
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Dialog-Frame
ON LEAVE OF rm-rctd.i-no IN FRAME Dialog-Frame /* Item No */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-i-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no Dialog-Frame
ON VALUE-CHANGED OF rm-rctd.i-no IN FRAME Dialog-Frame /* Item No */
    DO:
        /*   FIND item                                                                    */
        /*       WHERE item.company EQ cocode                                             */
        /*         AND item.i-no    EQ {&self-name}:SCREEN-VALUE  */
        /*       NO-LOCK NO-ERROR.                                                        */
        /*   IF AVAIL item THEN                                                           */
        /*     ASSIGN                                                                     */
        /*      {&self-name}:SCREEN-VALUE    = item.i-no          */
        /*      rm-rctd.i-name:SCREEN-VALUE  = item.i-name.       */

        DEFINE VARIABLE li AS INTEGER NO-UNDO.

        FIND item
            WHERE item.company EQ cocode
            AND item.i-no    EQ {&self-name}:SCREEN-VALUE 
            /*         AND item.i-code  EQ "R" */
            NO-LOCK NO-ERROR.
        IF AVAILABLE item THEN 
        DO:
            RUN display-item-info (RECID(item)).
            DO li = 1 TO LENGTH({&self-name}:SCREEN-VALUE ) + 1:
                APPLY "cursor-right" TO {&self-name} .
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc Dialog-Frame
ON LEAVE OF rm-rctd.loc IN FRAME Dialog-Frame /* From!Whs */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-loc-bin-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc Dialog-Frame
ON VALUE-CHANGED OF rm-rctd.loc IN FRAME Dialog-Frame /* From!Whs */
    DO:
        RUN new-bin.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin Dialog-Frame
ON LEAVE OF rm-rctd.loc-bin IN FRAME Dialog-Frame /* From!Bin */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-loc-bin-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin Dialog-Frame
ON VALUE-CHANGED OF rm-rctd.loc-bin IN FRAME Dialog-Frame /* From!Bin */
    DO:
        RUN new-bin.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc-bin2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin2 Dialog-Frame
ON LEAVE OF rm-rctd.loc-bin2 IN FRAME Dialog-Frame /* To!Bin */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-loc-bin2 NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc2 Dialog-Frame
ON LEAVE OF rm-rctd.loc2 IN FRAME Dialog-Frame /* To !Whs */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-loc2 NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Dialog-Frame
ON ENTRY OF rm-rctd.qty IN FRAME Dialog-Frame /* Qty */
    DO:
        IF rm-rctd.tag:SCREEN-VALUE  <> "" THEN 
        DO:
            APPLY "tab" TO {&self-name} .
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty Dialog-Frame
ON LEAVE OF rm-rctd.qty IN FRAME Dialog-Frame /* Qty */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-loc-bin-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.rct-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.rct-date Dialog-Frame
ON HELP OF rm-rctd.rct-date IN FRAME Dialog-Frame /* Transfer!Date */
    DO:
        {methods/calpopup.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Dialog-Frame
ON LEAVE OF rm-rctd.tag IN FRAME Dialog-Frame /* From!Tag */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        /*
        RUN valid-loc-bin-tag NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        */
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag Dialog-Frame
ON VALUE-CHANGED OF rm-rctd.tag IN FRAME Dialog-Frame /* From!Tag */
    DO:
        RUN new-bin.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpd.i} 
SESSION:DATA-ENTRY-RETURN = YES.       

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    
    IF ip-type EQ "copy" THEN lv-item-recid = ip-recid.


    IF ip-recid EQ ? THEN 
    DO:
        RUN create-item.
    END.
    ELSE FIND rm-rctd NO-LOCK WHERE RECID(rm-rctd) EQ ip-recid NO-ERROR.

    IF ip-type NE "view" THEN 
    DO: 
        RUN enable_UI.
        RUN display-item.

        ASSIGN 
            ll-order-warned = NO.
        btn_done:HIDDEN IN FRAME {&FRAME-NAME} = YES.
    END.
    ELSE 
    DO:
        RUN display-item.
        ASSIGN 
            btn_done:HIDDEN IN FRAME {&FRAME-NAME} = NO.
        btn_done:SENSITIVE                        = YES.
        btn_ok:HIDDEN                             = YES.
        btn_cancel:HIDDEN                         = YES.
    END.

    DO WITH FRAME {&FRAME-NAME}:
    /*IF ip-type EQ "update" THEN DISABLE oe-rell.ord-no oe-rell.rel-no oe-rell.b-ord-no.
    DISABLE fi_part-no .
    IF ip-type EQ "add"  OR ip-type EQ "copy" THEN DO:
        APPLY "entry" TO oe-rell.ord-no  .
    END.
    oe-rell.cust-no:HIDDEN IN FRAME {&FRAME-NAME}  = TRUE .*/
    END.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item Dialog-Frame 
PROCEDURE create-item :
    /*------------------------------------------------------------------------------
          Purpose:     
          PARAMs:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rno LIKE rm-rctd.r-no NO-UNDO.
    DEFINE BUFFER b-rm-rctd FOR rm-rctd.

    DO WITH FRAME {&FRAME-NAME}:
        RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT lv-rno) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        
        CREATE rm-rctd.
        
        ASSIGN 
            rm-rctd.company   = cocode
            rm-rctd.r-no      = lv-rno
            rm-rctd.rita-code = "T"
            rm-rctd.s-num     = 0
            rm-rctd.rct-date  = TODAY
            .
        DISPLAY rm-rctd.rct-date . 
        ASSIGN 
            lv-item-recid = RECID(rm-rctd).
        ll-new-record = YES.

        FIND CURRENT rm-rctd NO-LOCK NO-ERROR.
    END. /* avail oe-relh */ 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item Dialog-Frame 
PROCEDURE display-item :
    /*------------------------------------------------------------------------------
          Purpose:     
          PARAMs:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    IF AVAILABLE rm-rctd  THEN 
    DO:
        DISPLAY  rm-rctd.r-no rm-rctd.rct-date 
            rm-rctd.i-no rm-rctd.i-name rm-rctd.tag rm-rctd.loc rm-rctd.loc-bin 
            rm-rctd.qty rm-rctd.pur-uom rm-rctd.loc2 rm-rctd.loc-bin2 rm-rctd.tag2 
            rm-rctd.user-id   
            WITH FRAME Dialog-Frame.
    END.


    IF ip-type NE "view" THEN 
    DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME Dialog-Frame.
    END.

    VIEW FRAME {&FRAME-NAME}. 
    APPLY "entry" TO FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item-info Dialog-Frame 
PROCEDURE display-item-info :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-item-recid AS RECID.
    DO WITH FRAME {&FRAME-NAME}:
        FIND ITEM WHERE RECID(ITEM) = ip-item-recid NO-LOCK NO-ERROR.
        IF AVAILABLE ITEM THEN
            ASSIGN rm-rctd.i-no:SCREEN-VALUE    = ITEM.i-no
                rm-rctd.i-name:SCREEN-VALUE  = ITEM.i-name
                rm-rctd.pur-uom:SCREEN-VALUE = ITEM.cons-uom
                /* rm-rctd.loc:SCREEN-VALUE = ITEM.loc
                 rm-rctd.loc-bin:SCREEN-VALUE = ITEM.loc-bin */
                .
    END.
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
    IF AVAILABLE rm-rctd THEN 
        DISPLAY rm-rctd.r-no rm-rctd.rct-date rm-rctd.i-no rm-rctd.i-name rm-rctd.tag 
            rm-rctd.loc rm-rctd.loc-bin rm-rctd.qty rm-rctd.pur-uom rm-rctd.loc2 
            rm-rctd.loc-bin2 rm-rctd.tag2 rm-rctd.user-id 
            WITH FRAME Dialog-Frame.
    ENABLE rm-rctd.rct-date btnCalendar-1 rm-rctd.i-no rm-rctd.i-name rm-rctd.tag 
        rm-rctd.loc rm-rctd.loc-bin rm-rctd.qty rm-rctd.pur-uom rm-rctd.loc2 
        rm-rctd.loc-bin2 Btn_OK Btn_Done Btn_Cancel RECT-21 RECT-38 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-bin Dialog-Frame 
PROCEDURE new-bin :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF rm-rctd.tag:SCREEN-VALUE  <> ""  THEN 
        DO:
            FIND FIRST rm-bin 
                WHERE rm-bin.company EQ cocode
                AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE 
                AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE 
                AND rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE 
                AND rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE rm-bin THEN
                FIND FIRST rm-bin 
                    WHERE rm-bin.company EQ cocode
                    AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE 
                    AND rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE 
                    NO-LOCK NO-ERROR.

            IF AVAILABLE rm-bin THEN 
            DO:
                /*       IF rm-rctd.tag:SCREEN-VALUE  NE "" THEN */
                rm-rctd.qty:SCREEN-VALUE  = STRING(rm-bin.qty).
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rmbin-help Dialog-Frame 
PROCEDURE rmbin-help :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rowid   AS ROWID     NO-UNDO.
    DEFINE VARIABLE save-rowid AS ROWID     NO-UNDO.
    DEFINE VARIABLE save-focus AS CHARACTER NO-UNDO.
    DEFINE VARIABLE char-hdl   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ll-error   AS LOG       NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        RUN windows/l-rmibn2.w (rm-rctd.company, rm-rctd.i-no:SCREEN-VALUE , rm-rctd.loc:SCREEN-VALUE , rm-rctd.loc-bin:SCREEN-VALUE , rm-rctd.tag:SCREEN-VALUE ,0, OUTPUT lv-rowid).

        FOR EACH tt-selected WHERE tt-rowid EQ lv-rowid,
            FIRST rm-bin WHERE ROWID(rm-bin) EQ tt-rowid:

            IF rm-rctd.loc:SCREEN-VALUE      NE rm-bin.loc     OR
                rm-rctd.loc-bin:SCREEN-VALUE  NE rm-bin.loc-bin OR
                rm-rctd.tag:SCREEN-VALUE      NE rm-bin.tag     THEN 
            DO:
                ASSIGN
                    rm-rctd.loc:SCREEN-VALUE     = rm-bin.loc
                    rm-rctd.loc-bin:SCREEN-VALUE = rm-bin.loc-bin
                    rm-rctd.tag:SCREEN-VALUE     = rm-bin.tag.

                RUN new-bin.
            END.

            DELETE tt-selected.

            LEAVE.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no Dialog-Frame 
PROCEDURE valid-i-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST item
            WHERE item.company EQ cocode
            AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE 
            AND ITEM.i-no    NE ""
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE item THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin-tag Dialog-Frame 
PROCEDURE valid-loc-bin-tag :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-fields AS CHARACTER INIT "loc,loc-bin,tag" NO-UNDO.
    DEFINE VARIABLE li-field# AS INTEGER   NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        li-field# = LOOKUP(FOCUS:NAME ,lv-fields).

        IF li-field# EQ 0 THEN li-field# = 9999.

        FIND FIRST rm-bin
            WHERE rm-bin.company  EQ cocode
            AND rm-bin.i-no     EQ rm-rctd.i-no:SCREEN-VALUE 
            AND (rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE           OR
            (li-field#     LT 1 AND
            rm-rctd.loc:SCREEN-VALUE  EQ ""))
            AND (rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE       OR
            (li-field#     LT 2 AND
            rm-rctd.loc-bin:SCREEN-VALUE  EQ ""))
            AND (rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE           OR
            (li-field#     LT 3 AND
            rm-rctd.tag:SCREEN-VALUE  EQ ""))
            USE-INDEX loc-bin NO-LOCK NO-ERROR.

        IF AVAILABLE rm-bin                                                             AND
            (rm-bin.qty GE DEC(rm-rctd.qty:SCREEN-VALUE ) OR
            li-field# LT 3)                                                         THEN
            ASSIGN
                rm-rctd.loc:SCREEN-VALUE     = CAPS(rm-bin.loc)
                rm-rctd.loc-bin:SCREEN-VALUE = CAPS(rm-bin.loc-bin)
                rm-rctd.tag:SCREEN-VALUE     = CAPS(rm-bin.tag)
                rm-rctd.tag2:SCREEN-VALUE    = CAPS(rm-bin.tag).

        ELSE 
        DO:
            IF AVAILABLE rm-bin THEN 
            DO:
                MESSAGE "Insufficient qty in bin..." VIEW-AS ALERT-BOX.
                IF li-field# LE 3 THEN
                    APPLY "entry" TO FOCUS .
                ELSE
                    APPLY "entry" TO rm-rctd.qty .
            END.

            ELSE 
            DO:
                MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
                IF li-field# LE 3 THEN
                    APPLY "entry" TO FOCUS .
                ELSE
                    APPLY "entry" TO rm-rctd.loc-bin .
            END.

            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin-tag-old Dialog-Frame 
PROCEDURE valid-loc-bin-tag-old :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-int AS INTEGER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST rm-bin 
            WHERE rm-bin.company  EQ cocode
            AND rm-bin.i-no     EQ rm-rctd.i-no:SCREEN-VALUE 
            AND (rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE      OR ip-int LT 1)
            AND (rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE  OR ip-int LT 2)
            AND (rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE      OR ip-int LT 3))
            THEN 
        DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
            IF ip-int EQ 3 THEN
                APPLY "entry" TO rm-rctd.tag .
            ELSE
                IF ip-int EQ 2 THEN
                    APPLY "entry" TO rm-rctd.loc-bin .
                ELSE
                    APPLY "entry" TO rm-rctd.loc .
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin2 Dialog-Frame 
PROCEDURE valid-loc-bin2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF lv-msg EQ "" THEN
            IF rm-rctd.loc-bin2:SCREEN-VALUE  EQ "" THEN
                lv-msg = "To Bin may not be spaces".

        IF lv-msg EQ "" THEN
            IF rm-rctd.loc:SCREEN-VALUE       EQ
                rm-rctd.loc2:SCREEN-VALUE      AND
                rm-rctd.loc-bin:SCREEN-VALUE   EQ
                rm-rctd.loc-bin2:SCREEN-VALUE  THEN
                lv-msg = "To Whse/Bin may not be the same as From Whse/Bin".

        IF lv-msg EQ "" THEN 
        DO:
            FIND FIRST rm-bin
                WHERE rm-bin.company EQ cocode
                AND rm-bin.i-no    EQ ""
                AND rm-bin.loc     EQ rm-rctd.loc2:SCREEN-VALUE 
                AND rm-bin.loc-bin EQ rm-rctd.loc-bin2:SCREEN-VALUE 
                USE-INDEX loc-bin NO-LOCK NO-ERROR.

            IF NOT AVAILABLE rm-bin THEN lv-msg = "Invalid entry, try help...".
        END.

        IF lv-msg NE "" THEN 
        DO:
            MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO rm-rctd.loc-bin2 .
            RETURN ERROR.
        END.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc2 Dialog-Frame 
PROCEDURE valid-loc2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF lv-msg EQ "" THEN
            IF rm-rctd.loc2:SCREEN-VALUE  EQ "" THEN
                lv-msg = "To Warehouse may not be spaces".

        IF lv-msg EQ "" THEN 
        DO:
            FIND FIRST loc
                WHERE loc.company EQ cocode
                AND loc.loc     EQ rm-rctd.loc2:SCREEN-VALUE 
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE loc THEN lv-msg = "Invalid entry, try help".
        END.

        IF lv-msg NE "" THEN 
        DO:
            MESSAGE TRIM(lv-msg) + "..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO rm-rctd.loc2 .
            RETURN ERROR.
        END.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag Dialog-Frame 
PROCEDURE valid-tag :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-rm-rctd FOR rm-rctd . 

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST bf-rm-rctd NO-LOCK
            WHERE bf-rm-rctd.company = cocode 
            AND bf-rm-rctd.rita-code = "T" 
            AND bf-rm-rctd.tag NE ""
            AND bf-rm-rctd.tag = rm-rctd.tag:SCREEN-VALUE
            AND RECID(bf-rm-rctd) <> RECID(rm-rctd)  NO-ERROR.
        IF AVAILABLE bf-rm-rctd THEN 
        DO:
            MESSAGE "This Tag Number Has Already Been Used." SKIP
                "Please Enter A Unique Tag Number." 
                VIEW-AS ALERT-BOX ERROR.
            APPLY 'entry' TO rm-rctd.tag.
            RETURN ERROR.
        END.
    END.
 
    IF rmrecpt-int EQ 1 THEN 
    DO:
        FIND FIRST loadtag WHERE loadtag.company = g_company
            AND loadtag.item-type = YES
            AND loadtag.tag-no = rm-rctd.tag:SCREEN-VALUE  NO-LOCK NO-ERROR.
        IF NOT AVAILABLE loadtag THEN 
        DO:
            MESSAGE "Invalid Tag#. Try help or Scan valid tag#..." VIEW-AS ALERT-BOX ERROR.
            rm-rctd.tag:SCREEN-VALUE  = ''.
            APPLY "entry" TO rm-rctd.tag .
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


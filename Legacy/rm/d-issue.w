&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi       PROGRESS
*/
&Scoped-define WINDOW-NAME M-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS M-Win 
/*------------------------------------------------------------------------

  File: rm/d-issue.w

  Description: 

  Input Parameters: <none>

  Output Parameters: <none>

  Author: 

  Created: 11.13.2018

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.



/* ***************************  Definitions  ************************** */

/*Gets rid of stack trace window when pressing F1*/
/*SESSION:DEBUG-ALERT = FALSE.*/

/* PARAMs Definitions ---                                           */
DEFINE INPUT PARAMETER ip-recid  AS RECID     NO-UNDO.
DEFINE INPUT PARAMETER ip-type   AS CHARACTER NO-UNDO .   /* add,update,view */
DEFINE OUTPUT PARAMETER ip-rowid AS ROWID     NO-UNDO.

{custom/globdefs.i}

{sys/inc/var.i new shared}

ASSIGN 
    cocode = g_company.
ASSIGN 
    locode = g_loc.

{sa/sa-sls01.i}

DEFINE            VARIABLE lv-do-all-items    AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE lv-item-imported   AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE lv-import-rejected AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE lv-ship-from       AS LOGICAL   INITIAL YES NO-UNDO.
DEFINE            VARIABLE ll-ask-import      AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE lv-rowid           AS ROWID     NO-UNDO.
DEFINE            VARIABLE cv-s-codes         AS CHARACTER NO-UNDO.
DEFINE            VARIABLE cv-s-dscrs         AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cOrd-ok            AS CHARACTER INITIAL ["R,I,S,P,A,N,U"].


RUN sys/ref/s-codes.p (OUTPUT cv-s-codes, OUTPUT cv-s-dscrs).

DEFINE VARIABLE lv-item-recid     AS RECID     NO-UNDO.
DEFINE VARIABLE ll-order-warned   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ll-new-record     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lAllowRmAdd       AS LOG       NO-UNDO.
DEFINE VARIABLE lcReturn          AS CHARACTER NO-UNDO.
DEFINE VARIABLE llRecFound        AS LOG       NO-UNDO.
DEFINE VARIABLE v-bin             AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-rmtags-log      AS LOG       NO-UNDO.
DEFINE VARIABLE lv-i-no           LIKE po-ordl.i-no NO-UNDO.
DEFINE VARIABLE lv-line           LIKE po-ordl.line NO-UNDO.
DEFINE VARIABLE char-val          AS CHARACTER NO-UNDO.
DEFINE VARIABLE vlc-success       AS CHARACTER INIT "" NO-UNDO.
DEFINE VARIABLE ll-is-copy-record AS LOG       NO-UNDO.
DEFINE VARIABLE lv-uom-list       AS cha       INIT ["EA,TON,MSF,MSH,LB,LF,DIA"] NO-UNDO.
DEFINE VARIABLE lv-po-wid         AS DECIMAL   FORMAT ">>>9.9999" NO-UNDO.
DEFINE VARIABLE lv-po-len         AS DECIMAL   FORMAT ">>,>>9.9999" NO-UNDO.
DEFINE VARIABLE v-len             LIKE lv-po-len NO-UNDO.
DEFINE VARIABLE v-wid             LIKE lv-po-len NO-UNDO.
DEFINE VARIABLE ext-cost          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lv-rmissue        AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-get-tandem-rec  AS LOG       NO-UNDO.
DEFINE VARIABLE jobreopn-log      AS LOG       NO-UNDO.
DEFINE VARIABLE lCheckDateField   AS LOGICAL   NO-UNDO .
DEFINE VARIABLE v-rtn-char        AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-rec-found       AS LOG       NO-UNDO.
DEFINE VARIABLE ll-help-run       AS LOG       NO-UNDO.  /* set on browse help, reset row-entry */

DEFINE BUFFER xitem FOR ITEM.
DEFINE NEW SHARED TEMP-TABLE item-chg NO-UNDO
    FIELD i-no   LIKE job-mat.i-no
    FIELD rec-id AS RECID.
DEFINE NEW SHARED TEMP-TABLE tt-selected 
    FIELD tt-rowid AS ROWID.
DEFINE NEW SHARED VARIABLE fil_id AS RECID NO-UNDO.

DO TRANSACTION:
    {sys/inc/rmrecpt.i}
END.
RUN sys/ref/nk1look.p (cocode, "RMAllowAdd", "L", NO, NO, "", "", 
    OUTPUT lcReturn, OUTPUT llRecFound).

lAllowRmAdd = LOGICAL(lcReturn) NO-ERROR.  

RUN sys/ref/nk1look.p (cocode, "RMWHSBIN", "C", NO, NO, "", "", 
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    v-bin = v-rtn-char NO-ERROR.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name EQ "RMTAGS"
    NO-LOCK NO-ERROR.

IF NOT AVAILABLE sys-ctrl THEN
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "RMTAGS"
        sys-ctrl.descrip  = "Number of RM Loadtags to Print & Create Wip Tags"
        sys-ctrl.char-fld = ""
        sys-ctrl.int-fld  = 1
        sys-ctrl.log-fld  = FALSE. /* true create wip/false do not */
END.

ASSIGN 
    v-rmtags-log = sys-ctrl.log-fld.

RELEASE sys-ctrl.

DEFINE VARIABLE lv-job-no              LIKE rm-rctd.job-no NO-UNDO.
DEFINE VARIABLE look-recid             AS RECID   NO-UNDO.

DEFINE VARIABLE v-number-rows-selected AS INTEGER NO-UNDO.

{windows/l-jobmt3.i NEW}

DEFINE TEMP-TABLE tt-frm NO-UNDO 
    FIELD frm    LIKE job-mat.frm
    FIELD mrp    LIKE job-mat.qty
    FIELD qty    LIKE job-mat.qty
    FIELD qtypct AS DECIMAL
    INDEX frm frm.      

DEFINE TEMP-TABLE tt-tag NO-UNDO 
    FIELD tag-no   LIKE rm-rctd.tag
    FIELD qty      LIKE rm-rctd.qty
    FIELD tt-rowid AS ROWID
    INDEX tt-rowid tt-rowid
    INDEX tag-no   tag-no.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rm-rctd

/* Definitions for FRAME F-Main                                         */
&Scoped-define FIELDS-IN-QUERY-F-Main rm-rctd.r-no rm-rctd.rct-date ~
rm-rctd.po-no rm-rctd.job-no rm-rctd.job-no2 rm-rctd.i-no rm-rctd.i-name ~
rm-rctd.s-num rm-rctd.b-num rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom rm-rctd.user-id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F-Main rm-rctd.rct-date ~
rm-rctd.po-no rm-rctd.job-no rm-rctd.job-no2 rm-rctd.i-no rm-rctd.i-name ~
rm-rctd.s-num rm-rctd.b-num rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag ~
rm-rctd.qty rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom rm-rctd.user-id 
&Scoped-define ENABLED-TABLES-IN-QUERY-F-Main rm-rctd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F-Main rm-rctd
&Scoped-define TABLES-IN-QUERY-F-Main rm-rctd
&Scoped-define FIRST-TABLE-IN-QUERY-F-Main rm-rctd


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS rm-rctd.rct-date rm-rctd.po-no rm-rctd.job-no ~
rm-rctd.job-no2 rm-rctd.i-no rm-rctd.i-name rm-rctd.s-num rm-rctd.b-num ~
rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag rm-rctd.qty rm-rctd.pur-uom 
&Scoped-define ENABLED-TABLES rm-rctd
&Scoped-define FIRST-ENABLED-TABLE rm-rctd
&Scoped-Define ENABLED-OBJECTS fi_ext-amount fi_diswid fi_dislen Btn_Done ~
Btn_Cancel Btn_OK RECT-21 RECT-38 
&Scoped-Define DISPLAYED-FIELDS rm-rctd.r-no rm-rctd.rct-date rm-rctd.po-no ~
rm-rctd.job-no rm-rctd.job-no2 rm-rctd.i-no rm-rctd.i-name rm-rctd.s-num ~
rm-rctd.b-num rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag rm-rctd.qty ~
rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom rm-rctd.user-id 
&Scoped-define DISPLAYED-TABLES rm-rctd
&Scoped-define FIRST-DISPLAYED-TABLE rm-rctd
&Scoped-Define DISPLAYED-OBJECTS fi_ext-amount fi_diswid fi_dislen 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE M-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel 
    IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
    LABEL "Cancel" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
    IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
    LABEL "&Done" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
    IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
    LABEL "&Save" 
    SIZE 8 BY 1.91
    BGCOLOR 8 .

DEFINE VARIABLE fi_dislen     AS DECIMAL FORMAT "->>>,>>>,>>9.99<<<<":U INITIAL 0 
    LABEL "Length" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_diswid     AS DECIMAL FORMAT "->>>,>>>,>>9.99<<<<":U INITIAL 0 
    LABEL "Width" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE fi_ext-amount AS DECIMAL FORMAT "->>>,>>>,>>9.99<<<<":U INITIAL 0 
    LABEL "Ext.Amount" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 
    BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-21
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  ROUNDED 
    SIZE 19 BY 2.38
    BGCOLOR 15.

DEFINE RECTANGLE RECT-38
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL  ROUNDED 
    SIZE 133.8 BY 10.71
    BGCOLOR 15.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F-Main FOR 
    rm-rctd SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    rm-rctd.r-no AT ROW 1.24 COL 29.8 COLON-ALIGNED
    LABEL "Seq#" FORMAT ">>>>>>>9"
    VIEW-AS FILL-IN 
    SIZE 14.2 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.rct-date AT ROW 2.43 COL 29.8 COLON-ALIGNED
    LABEL "Issue Date" FORMAT "99/99/9999"
    VIEW-AS FILL-IN 
    SIZE 30.2 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.po-no AT ROW 2.43 COL 85.4 COLON-ALIGNED
    LABEL "PO#" FORMAT "x(9)"
    VIEW-AS FILL-IN 
    SIZE 33.6 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.job-no AT ROW 3.62 COL 29.8 COLON-ALIGNED HELP
    ""
    LABEL "Job" FORMAT "x(6)"
    VIEW-AS FILL-IN 
    SIZE 23.2 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.job-no2 AT ROW 3.62 COL 53.4 COLON-ALIGNED NO-LABELS FORMAT "99"
    VIEW-AS FILL-IN 
    SIZE 5.8 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.i-no AT ROW 3.62 COL 85.4 COLON-ALIGNED
    LABEL "Item" FORMAT "x(15)"
    VIEW-AS FILL-IN 
    SIZE 33.6 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.i-name AT ROW 4.81 COL 29.8 COLON-ALIGNED
    VIEW-AS FILL-IN 
    SIZE 43 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.s-num AT ROW 4.81 COL 85.8 COLON-ALIGNED
    LABEL "S" FORMAT ">9"
    VIEW-AS FILL-IN 
    SIZE 6.6 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.b-num AT ROW 4.81 COL 98.4 COLON-ALIGNED
    LABEL "B" FORMAT ">9"
    VIEW-AS FILL-IN 
    SIZE 8 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.loc AT ROW 6.1 COL 29.8 COLON-ALIGNED FORMAT "x(5)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.loc-bin AT ROW 6.1 COL 54.4 COLON-ALIGNED
    LABEL "Bin" FORMAT "x(8)"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.tag AT ROW 6.1 COL 85.4 COLON-ALIGNED
    LABEL "Tag#" FORMAT "x(20)"
    VIEW-AS FILL-IN 
    SIZE 33.6 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.qty AT ROW 7.33 COL 29.8 COLON-ALIGNED
    LABEL "Qty" FORMAT "->>>>>>9.9<<<<<"
    VIEW-AS FILL-IN 
    SIZE 17 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.pur-uom AT ROW 7.33 COL 58 COLON-ALIGNED
    LABEL "PUOM" FORMAT "x(4)"
    VIEW-AS FILL-IN 
    SIZE 5 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.cost AT ROW 7.33 COL 85.4 COLON-ALIGNED
    LABEL "Cost" FORMAT "->>>,>>>,>>9.99<<<<"
    VIEW-AS FILL-IN 
    SIZE 20 BY 1
    BGCOLOR 15 FONT 1
    rm-rctd.cost-uom AT ROW 7.33 COL 115.4 COLON-ALIGNED
    LABEL "CUOM" FORMAT "x(4)"
    VIEW-AS FILL-IN 
    SIZE 8 BY 1
    BGCOLOR 15 FONT 1
    fi_ext-amount AT ROW 8.67 COL 29.8 COLON-ALIGNED
    fi_diswid AT ROW 8.67 COL 85.4 COLON-ALIGNED
    fi_dislen AT ROW 9.91 COL 29.8 COLON-ALIGNED
    rm-rctd.user-id AT ROW 9.91 COL 85.4 COLON-ALIGNED
    LABEL "UserId" FORMAT "x(10)"
    VIEW-AS FILL-IN 
    SIZE 19.6 BY 1
    BGCOLOR 15 FONT 1
    Btn_Done AT ROW 12.24 COL 120
    Btn_Cancel AT ROW 12.24 COL 125
    Btn_OK AT ROW 12.29 COL 116
    RECT-21 AT ROW 12.00 COL 115
    RECT-38 AT ROW 1.1 COL 1.3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 135.2 BY 14.00
    FGCOLOR 1 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW M-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "WAREHOUSE TRANSACTION ISSUES"
        HEIGHT             = 14.00
        WIDTH              = 135.2
        MAX-HEIGHT         = 33.29
        MAX-WIDTH          = 273.2
        VIRTUAL-HEIGHT     = 33.29
        VIRTUAL-WIDTH      = 273.2
        RESIZE             = YES
        SCROLL-BARS        = NO
        STATUS-AREA        = YES
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = YES
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN rm-rctd.b-num IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.cost IN FRAME F-Main
   EXP-LABEL EXP-FORMAT  no-enable                                       */
/* SETTINGS FOR FILL-IN rm-rctd.cost-uom IN FRAME F-Main
   EXP-LABEL EXP-FORMAT  no-enable                                            */
/* SETTINGS FOR FILL-IN rm-rctd.i-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.job-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN rm-rctd.job-no2 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.loc IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN rm-rctd.loc-bin IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.po-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.pur-uom IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.qty IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.r-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN rm-rctd.rct-date IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.s-num IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.tag IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN rm-rctd.user-id IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(M-Win)
    THEN M-Win:HIDDEN = NO.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _TblList          = "asitest168.rm-rctd "
     _Where[1]         = "ASI.rm-rctd.company eq cocode "
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 /* ************************  Control Triggers  ************************ */


&Scoped-define SELF-NAME M-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL M-Win M-Win
ON END-ERROR OF M-Win /* Payment Selection by Vendor */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL M-Win M-Win
ON WINDOW-CLOSE OF M-Win /* Payment Selection by Vendor */
    DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
  
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL M-Win M-Win
ON HELP OF FRAME F-Main
    DO: 
        DEFINE VARIABLE ll-tag#    AS LOG   NO-UNDO.
        DEFINE VARIABLE help-recid AS RECID NO-UNDO.
        DEFINE VARIABLE lv-search  AS cha   NO-UNDO.

        /* gdm - 08070907 */
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN 
                lv-job-no              = FILL(" ", 6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE ))) +
               TRIM(rm-rctd.job-no:SCREEN-VALUE)
                char-val               = ""
                v-number-rows-selected = 0
                look-recid             = 0.

            /* gdm - 08070907 end */

            ll-help-run = YES.
            CASE FOCUS:NAME:
                WHEN "i-no" THEN 
                    DO:
                        RUN rm/g-joblk.w (OUTPUT lv-search).  /* search job or item */
                        IF lv-search = "job" THEN 
                        DO:
                            RUN windows/l-jobmat.w (rm-rctd.company,rm-rctd.job-no:SCREEN-VALUE,
                                rm-rctd.job-no2:SCREEN-VALUE,rm-rctd.i-no:SCREEN-VALUE, OUTPUT char-val, OUTPUT help-recid).
                            IF help-recid <> ? THEN RUN DISPLAY-jobmat (help-recid).
                
                        END.
                        ELSE 
                        DO:
                            /* company,industry,mat-type,i-code,i-no, output, output */
                            RUN windows/l-itmRE.w (rm-rctd.company,"","","R",FOCUS:SCREEN-VALUE, OUTPUT char-val,OUTPUT help-recid).
                            IF char-val <> "" AND ENTRY(1,char-val) NE FOCUS:SCREEN-VALUE THEN 
                            DO :
                                FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
                                RUN new-i-no.
                            END.
                        END.  
                    END.

                WHEN "po-no" THEN 
                    DO:
                        RUN windows/l-poords.w (rm-rctd.company, FOCUS:SCREEN-VALUE, 0, OUTPUT char-val).
                        IF char-val <> "" THEN 
                        DO:
                            ASSIGN
                                FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                lv-i-no            = ENTRY(2,char-val)
                                lv-line            = int(ENTRY(6,char-val)).
                        END.
                    END.

                WHEN "job-no" OR 
                WHEN "job-no2" THEN 
                    DO:
                        RUN windows/l-jobno.w (rm-rctd.company,FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT help-recid).
                        IF char-val <> "" THEN 
                        DO :
                            ASSIGN 
                                rm-rctd.job-no:screen-value  = ENTRY(1,char-val)
                                rm-rctd.job-no2:screen-value = ENTRY(2,char-val).
                        END.  
                    END.

                WHEN "loc"     OR
                WHEN "loc-bin" OR
                WHEN "tag"     THEN RUN rmbin-help (FOCUS:HANDLE).
                WHEN "s-num"   THEN 
                    DO:
                        /* gdm - 08070907 */
                        RUN get-job-mat.

                    END.
                /* gdm - 08070907 end */
                WHEN "b-num"   THEN RUN s-b-help.
            END CASE.

            help-recid = ?.

            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME rm-rctd.b-num
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.b-num M-Win
ON LEAVE OF rm-rctd.b-num IN FRAME F-Main /* B */
    DO:
        IF LASTKEY = -1 THEN RETURN.
        RUN validate-jobmat (NO) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel M-Win
ON CHOOSE OF Btn_Cancel IN FRAME F-Main /* Cancel */
    DO:
        DISABLE TRIGGERS FOR LOAD OF rm-rctd .

        IF lv-item-recid NE ? THEN 
        DO:
            FIND rm-rctd EXCLUSIVE-LOCK
                WHERE RECID(rm-rctd) EQ lv-item-recid  NO-ERROR.
            IF AVAILABLE rm-rctd THEN DELETE rm-rctd .
        END.
        IF AVAILABLE rm-rctd THEN
            ip-rowid = ROWID(rm-rctd).
        lCheckDateField = NO .
    
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done M-Win
ON CHOOSE OF Btn_Done IN FRAME F-Main /* Done */
    DO:
  &IF DEFINED (adm-panel) NE 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
        APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK M-Win
ON CHOOSE OF Btn_OK IN FRAME F-Main /* Save */
    DO:

        DEFINE VARIABLE v-qty     AS DECIMAL NO-UNDO.
        DEFINE VARIABLE ll        AS LOGICAL NO-UNDO.
        DEFINE VARIABLE op-error  AS LOGICAL NO-UNDO.
        DEFINE VARIABLE old-po-no LIKE oe-rell.po-no NO-UNDO.
        DEFINE BUFFER b-oe-rell FOR oe-rell.

        IF ip-type EQ "view" THEN 
        DO: 
            APPLY "go" TO FRAME {&FRAME-NAME}.
            RETURN.
        END.
        /* Code placed here will execute PRIOR to standard behavior. */
        RUN valid-po-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        DO WITH FRAME {&FRAME-NAME}:
            IF INT(rm-rctd.po-no:SCREEN-VALUE ) NE 0 THEN 
            DO:
                FIND po-ordl
                    WHERE po-ordl.company EQ rm-rctd.company
                    AND po-ordl.po-no   EQ INT(rm-rctd.po-no:SCREEN-VALUE )
                    NO-LOCK NO-ERROR.

                IF AVAILABLE po-ordl THEN
                    ASSIGN
                        lv-i-no = po-ordl.i-no
                        lv-line = po-ordl.line.

                IF lv-i-no EQ "" OR lv-line EQ 0 THEN 
                DO:
                    RUN windows/l-poords.w (rm-rctd.company, rm-rctd.po-no, INT(rm-rctd.po-no), OUTPUT char-val).

                    IF char-val NE "" THEN
                        ASSIGN
                            lv-i-no = ENTRY(2,char-val)
                            lv-line = INT(ENTRY(6,char-val)).
                END.

                IF lv-i-no EQ "" OR lv-line EQ 0 THEN 
                DO:
                    MESSAGE "Must select PO Line to Issue to..." VIEW-AS ALERT-BOX ERROR.
                    APPLY "entry" TO rm-rctd.po-no .
                    RETURN NO-APPLY.
                END.
            END.
        END.

        RUN valid-job-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN valid-job-no2 NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        
        RUN valid-all NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        SESSION:SET-WAIT-STATE ("general").
        DO TRANSACTION:
            FIND CURRENT rm-rctd EXCLUSIVE-LOCK NO-ERROR.

            DO WITH FRAME {&FRAME-NAME}:
                ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
            END.
        END.

        IF v-rmtags-log AND
            rm-rctd.tag NE "" AND
            NOT CAN-FIND(FIRST wiptag WHERE
            wiptag.company EQ cocode AND
            wiptag.rm-tag-no EQ rm-rctd.tag) THEN
        DO:
            MESSAGE "Launch WIP Tag Creation for Item " rm-rctd.i-no "Tag #" rm-rctd.tag "?" VIEW-AS ALERT-BOX QUESTION
                BUTTON YES-NO UPDATE ll-wip AS LOG.
            /* btr */
        
            IF ll-wip THEN
                RUN jcrep/wipldtg.w  (INPUT rm-rctd.tag,
                    INPUT rm-rctd.job-no,
                    INPUT rm-rctd.job-no2,
                    INPUT rm-rctd.i-no,
                    INPUT rm-rctd.s-num,
                    INPUT rm-rctd.b-num,
                    INPUT rm-rctd.qty,
                    INPUT rm-rctd.pur-uom,
                    OUTPUT vlc-success).
        END.
  
        ASSIGN 
            ll-is-copy-record = NO .

        RUN multi-issues (ROWID(rm-rctd)) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        SESSION:SET-WAIT-STATE ("").

        ip-rowid = ROWID(rm-rctd).

        ASSIGN 
            lCheckDateField = NO .
        APPLY "close" TO THIS-PROCEDURE.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost M-Win
ON LEAVE OF rm-rctd.cost IN FRAME F-Main /* Cost */
    DO:
        RUN get-matrix (NO).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.cost-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.cost-uom M-Win
ON LEAVE OF rm-rctd.cost-uom IN FRAME F-Main /* CUOM */
    DO:
        IF LASTKEY = -1 THEN RETURN .

        IF INDEX(lv-uom-list,SELF:SCREEN-VALUE) <= 0 THEN 
        DO:
            MESSAGE "Invalid UOM." VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
        RUN get-matrix (NO).
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-name M-Win
ON ENTRY OF rm-rctd.i-name IN FRAME F-Main /* Name/Desc */
    DO:
        APPLY "tab" TO {&self-name} IN FRAME F-Main.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-name M-Win
ON LEAVE OF rm-rctd.i-name IN FRAME F-Main /* Name/Desc */
    DO:
        APPLY "entry" TO rm-rctd.s-num .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no M-Win
ON LEAVE OF rm-rctd.i-no IN FRAME F-Main /* Item */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-i-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            RUN validate-jobmat (YES) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.i-no M-Win
ON VALUE-CHANGED OF rm-rctd.i-no IN FRAME F-Main /* Item */
    DO:
        RUN new-i-no.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no M-Win
ON LEAVE OF rm-rctd.job-no IN FRAME F-Main /* Job */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-job-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no M-Win
ON VALUE-CHANGED OF rm-rctd.job-no IN FRAME F-Main /* Job */
    DO:
        RUN new-job-no.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 M-Win
ON LEAVE OF rm-rctd.job-no2 IN FRAME F-Main
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-job-no2 NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.job-no2 M-Win
ON VALUE-CHANGED OF rm-rctd.job-no2 IN FRAME F-Main
    DO:
        RUN new-job-no.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc M-Win
ON LEAVE OF rm-rctd.loc IN FRAME F-Main /* Warehouse */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-loc (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc M-Win
ON VALUE-CHANGED OF rm-rctd.loc IN FRAME F-Main /* Warehouse */
    DO:
        RUN new-bin.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin M-Win
ON LEAVE OF rm-rctd.loc-bin IN FRAME F-Main /* Bin */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-loc-bin (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.loc-bin M-Win
ON VALUE-CHANGED OF rm-rctd.loc-bin IN FRAME F-Main /* Bin */
    DO:
        RUN new-bin.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.po-no M-Win
ON LEAVE OF rm-rctd.po-no IN FRAME F-Main /* PO# */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-po-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.pur-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.pur-uom M-Win
ON LEAVE OF rm-rctd.pur-uom IN FRAME F-Main /* PUOM */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-uom NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
            RUN get-matrix (NO).
            RUN valid-qty2 NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.qty M-Win
ON LEAVE OF rm-rctd.qty IN FRAME F-Main /* Qty */
    DO:
        IF LASTKEY NE -1 THEN 
        DO:
            /* Task No- 03151112     */
            /*RUN valid-qty NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.*/

            IF {&self-name}:MODIFIED THEN 
            DO:
       
                RUN valid-loc-bin-tag (99) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN 
                DO:
                    APPLY "entry" TO {&self-name} .
                    RETURN NO-APPLY.
                END.

                IF DEC(rm-rctd.qty:SCREEN-VALUE ) LT 0 AND
                    DEC(rm-rctd.cost:SCREEN-VALUE ) EQ 0 AND
                    rm-rctd.tag:SCREEN-VALUE  NE "" THEN
                    FOR EACH rm-rcpth FIELDS(company r-no rita-code pur-uom) WHERE
                        rm-rcpth.company EQ cocode AND
                        rm-rcpth.i-no EQ rm-rctd.i-no:SCREEN-VALUE  AND
                        rm-rcpth.rita-code EQ "I"
                        NO-LOCK,
                        EACH rm-rdtlh FIELDS(trans-date trans-time cost) WHERE
                        rm-rdtlh.company   EQ rm-rcpth.company AND
                        rm-rdtlh.r-no      EQ rm-rcpth.r-no AND
                        rm-rdtlh.rita-code EQ rm-rcpth.rita-code AND
                        rm-rdtlh.loc       EQ rm-rctd.loc:SCREEN-VALUE  AND
                        rm-rdtlh.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE  AND
                        rm-rdtlh.tag       EQ rm-rctd.tag:SCREEN-VALUE  AND
                        rm-rdtlh.qty       GT 0
                        NO-LOCK
                        BREAK BY rm-rcpth.trans-date DESCENDING
                        BY rm-rdtlh.trans-time DESCENDING:

                        ASSIGN
                            rm-rctd.cost:SCREEN-VALUE     = STRING(rm-rdtlh.cost)
                            rm-rctd.cost-uom:SCREEN-VALUE = rm-rcpth.pur-uom.
                        LEAVE.
                    END.
            END.
        END.    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.rct-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.rct-date M-Win
ON LEAVE OF rm-rctd.rct-date IN FRAME F-Main /* Issue Date */
    DO:
        IF NOT lCheckDateField THEN 
        DO:
            {custom/currentDatePrompt.i SELF:SCREEN-VALUE}
            lCheckDateField = YES .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rm-rctd.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag M-Win
ON LEAVE OF rm-rctd.tag IN FRAME F-Main /* Tag# */
    DO:   
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-tag NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
     
            RUN valid-loc-bin-tag (3) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rm-rctd.tag M-Win
ON VALUE-CHANGED OF rm-rctd.tag IN FRAME F-Main /* Tag# */
    DO:
        RUN new-bin.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK M-Win 


{sys/inc/f3helpd.i} 

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    FIND rm-rctd NO-LOCK
        WHERE rm-rctd.company EQ cocode
        AND RECID(rm-rctd)  EQ ip-recid NO-ERROR .

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
        btn_done:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
        btn_ok:HIDDEN IN FRAME {&FRAME-NAME}      = YES.
        btn_cancel:HIDDEN  IN FRAME {&FRAME-NAME} = YES.
    END.

    DO WITH FRAME {&FRAME-NAME}:
        IF ip-type EQ "update" THEN DISABLE rm-rctd.user-id .

        IF ip-type EQ "add"  OR ip-type EQ "copy" THEN 
        DO:
            APPLY "entry" TO rm-rctd.rct-date  .
        END.
    END.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-multi-line M-Win 
PROCEDURE check-multi-line :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-job-mat   FOR job-mat.
    DEFINE BUFFER b-w-rm-rctd FOR w-rm-rctd.

    DEFINE VARIABLE v-cnt    AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll-multi AS LOG     INIT YES NO-UNDO.

    ASSIGN 
        ll-multi = AVAILABLE w-rm-rctd.

    IF ll-multi THEN
        FIND FIRST job-mat WHERE
            ROWID(job-mat) EQ w-rm-rctd.job-mat-rowid
            NO-LOCK NO-ERROR.

    ASSIGN 
        ll-multi = AVAILABLE job-mat AND
                     CAN-FIND(FIRST b-job-mat
                               WHERE b-job-mat.company EQ job-mat.company
                                 AND b-job-mat.job     EQ job-mat.job
                                 AND b-job-mat.job-no  EQ job-mat.job-no
                                 AND b-job-mat.job-no2 EQ job-mat.job-no2
                                 AND b-job-mat.rm-i-no EQ job-mat.rm-i-no
                                 AND ROWID(b-job-mat)  NE ROWID(job-mat)
                              ).
    DO WITH FRAME {&FRAME-NAME}:
        IF ll-multi THEN 
            ASSIGN rm-rctd.job-no:SCREEN-VALUE  = w-rm-rctd.job-no
                rm-rctd.job-no2:SCREEN-VALUE = STRING(w-rm-rctd.job-no2)
                rm-rctd.i-no:SCREEN-VALUE    = w-rm-rctd.i-no
                rm-rctd.s-num:SCREEN-VALUE   = "?"
                rm-rctd.b-num:SCREEN-VALUE   = STRING(w-rm-rctd.b-num).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item M-Win 
PROCEDURE create-item :
    /*------------------------------------------------------------------------------
          Purpose:     
          PARAMs:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-rm-rctd FOR rm-rctd.
    DEFINE VARIABLE li AS INTEGER INIT 0 NO-UNDO.
    
    
    DO WITH FRAME {&FRAME-NAME}:
        /* Code placed here will execute PRIOR to standard behavior. */
        RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT li) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            MESSAGE "Could not obtain next sequence #, please contact ASI: " RETURN-VALUE
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.


       
        CREATE rm-rctd.
        ASSIGN 
            lv-item-recid = RECID(rm-rctd).
        ll-new-record = YES.

        ASSIGN 
            rm-rctd.company   = cocode
            rm-rctd.r-no      = li 
            rm-rctd.rita-code = "I".
        
        IF v-bin NE "user entered" THEN
            ASSIGN rm-rctd.loc = SUBSTR(v-bin,1,5).
        ASSIGN
            rm-rctd.s-num    = 1
            rm-rctd.b-num    = 0
            rm-rctd.rct-date = TODAY
            rm-rctd.user-id  = USERID(LDBNAME(1)).
        
        FIND CURRENT rm-rctd NO-LOCK NO-ERROR.
    END. /* avail oe-relh */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI M-Win  _DEFAULT-DISABLE
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
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(M-Win)
        THEN DELETE WIDGET M-Win.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item M-Win 
PROCEDURE display-item :
    /*------------------------------------------------------------------------------
          Purpose:     
          PARAMs:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    IF AVAILABLE rm-rctd  THEN 
    DO:
        RUN get-matrix (YES).
        ASSIGN 
            fi_ext-amount = ext-cost
            fi_diswid     = v-wid
            fi_dislen     = v-len .

        DISPLAY  rm-rctd.r-no rm-rctd.rct-date 
            rm-rctd.po-no rm-rctd.job-no rm-rctd.job-no2 rm-rctd.i-no rm-rctd.i-name 
            rm-rctd.s-num rm-rctd.b-num rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag 
            rm-rctd.qty rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom rm-rctd.USER-ID
            fi_ext-amount fi_diswid fi_dislen
            WITH FRAME F-Main.
    END.


    IF ip-type NE "view" THEN 
    DO:
        ENABLE  Btn_Cancel Btn_OK WITH FRAME F-Main.
    END.

    VIEW FRAME {&FRAME-NAME}. 
    APPLY "entry" TO FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-jobmat M-Win 
PROCEDURE display-jobmat :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        FIND job-mat WHERE RECID(job-mat) EQ ip-recid NO-LOCK NO-ERROR.
        IF AVAILABLE job-mat THEN 
        DO:
            rm-rctd.i-no:SCREEN-VALUE  = job-mat.i-no.

            RUN new-i-no.

            FIND job-mat WHERE RECID(job-mat) EQ ip-recid NO-LOCK.

            ASSIGN
                rm-rctd.s-num:SCREEN-VALUE = STRING(job-mat.frm)
                rm-rctd.b-num:SCREEN-VALUE = STRING(job-mat.blank-no).
        END.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI M-Win  _DEFAULT-ENABLE
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
    DISPLAY fi_ext-amount fi_diswid fi_dislen 
        WITH FRAME F-Main IN WINDOW M-Win.
    IF AVAILABLE rm-rctd THEN 
        DISPLAY rm-rctd.r-no rm-rctd.rct-date rm-rctd.po-no rm-rctd.job-no 
            rm-rctd.job-no2 rm-rctd.i-no rm-rctd.i-name rm-rctd.s-num 
            rm-rctd.b-num rm-rctd.loc rm-rctd.loc-bin rm-rctd.tag rm-rctd.qty 
            rm-rctd.pur-uom rm-rctd.cost rm-rctd.cost-uom rm-rctd.user-id 
            WITH FRAME F-Main IN WINDOW M-Win.
    ENABLE rm-rctd.rct-date rm-rctd.po-no rm-rctd.job-no rm-rctd.job-no2 
        rm-rctd.i-no rm-rctd.i-name rm-rctd.s-num rm-rctd.b-num rm-rctd.loc 
        rm-rctd.loc-bin rm-rctd.tag rm-rctd.qty rm-rctd.pur-uom  
        Btn_Done Btn_Cancel Btn_OK RECT-21 RECT-38 
        WITH FRAME F-Main IN WINDOW M-Win.
    {&OPEN-BROWSERS-IN-QUERY-F-Main}
    VIEW M-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-job-mat M-Win 
PROCEDURE get-job-mat :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE w-rm-rctd.
    DO WITH FRAME {&FRAME-NAME}:
        IF lv-job-no NE "" OR 
            rm-rctd.i-no:SCREEN-VALUE NE "" THEN
            RUN windows/l-jobmt3.w (cocode, lv-job-no,
                INT(rm-rctd.job-no2:SCREEN-VALUE),
                rm-rctd.i-no:SCREEN-VALUE,
                OUTPUT char-val,OUTPUT look-recid,
                OUTPUT v-number-rows-selected).

        FIND FIRST w-rm-rctd NO-ERROR.

        IF NOT AVAILABLE w-rm-rctd THEN 
        DO:
            APPLY "ENTRY" TO rm-rctd.s-num.
            RETURN NO-APPLY.
        END.
    END.
    IF v-number-rows-selected > 1 THEN
        RUN check-multi-line.
    ELSE 
    DO:         
        ASSIGN 
            fil_id = look-recid.
        RUN s-b-help. 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix M-Win 
PROCEDURE get-matrix :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-first-disp AS LOG NO-UNDO.

    DEFINE VARIABLE v-dep       LIKE po-ordl.s-len NO-UNDO. 
    DEFINE VARIABLE v-bwt       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE lv-out-qty  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-out-cost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-qty-uom  AS cha     NO-UNDO.
    DEFINE VARIABLE lv-cost-uom AS cha     NO-UNDO.
    DEFINE VARIABLE v-job-up    LIKE job-hdr.n-on NO-UNDO.
    DEFINE VARIABLE v-out       LIKE ef.n-out INIT 1 NO-UNDO.
    DEFINE VARIABLE lv-uom      LIKE rm-rctd.pur-uom NO-UNDO.
    DEFINE VARIABLE ld-lf-used  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ld          AS DECIMAL NO-UNDO FORMAT ">,>>9.9<<<".
    DEFINE VARIABLE ll-disp     AS LOG     INIT NO NO-UNDO.

    DEFINE BUFFER b-rm-rctd FOR rm-rctd.
    DO WITH FRAME {&FRAME-NAME}:  
        IF ip-first-disp  AND AVAILABLE rm-rctd AND rm-rctd.i-no <> "" THEN 
        DO: /* for row-display */
            FIND item  WHERE item.company EQ cocode                           /* no screen-value used */
                AND item.i-no  EQ rm-rctd.i-no /*:screen-value */
                USE-INDEX i-no NO-LOCK NO-ERROR.
            IF AVAILABLE item THEN v-dep = item.s-dep.

            RELEASE po-ordl.
      
            IF INT(rm-rctd.po-no) NE 0 THEN
                FIND FIRST po-ordl NO-LOCK
                    WHERE po-ordl.company   EQ rm-rctd.company
                    AND po-ordl.po-no     EQ INT(rm-rctd.po-no)
                    AND (po-ordl.i-no     EQ lv-i-no OR lv-i-no EQ "")
                    AND (po-ordl.line     EQ lv-line OR lv-line EQ 0)
                    AND po-ordl.item-type EQ YES
                    NO-ERROR.

            IF AVAILABLE po-ordl THEN 
            DO:
                ASSIGN
                    v-len   = po-ordl.s-len
                    v-wid   = po-ordl.s-wid
                    v-bwt   = 0
                    lv-i-no = po-ordl.i-no
                    lv-line = po-ordl.line.
                {rm/pol-dims.i}
            END.

            ELSE 
            DO:
                FIND FIRST job WHERE job.company EQ cocode
                    AND job.job-no  EQ rm-rctd.job-no
                    AND job.job-no2 EQ rm-rctd.job-no2
                    NO-LOCK NO-ERROR.
                IF AVAILABLE job THEN 
                DO :
                    FIND FIRST job-mat WHERE job-mat.company EQ cocode
                        AND job-mat.job     EQ job.job
                        AND job-mat.i-no    EQ rm-rctd.i-no
                        AND job-mat.frm     EQ rm-rctd.s-num
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE job-mat THEN 
                    DO:
                        ASSIGN 
                            v-len = job-mat.len
                            v-wid = job-mat.wid
                            v-bwt = job-mat.basis-w.
                    END.
                END.
            END.
            IF v-len EQ 0 THEN v-len = IF AVAILABLE item THEN item.s-len ELSE 0.
            IF v-wid EQ 0 THEN v-wid = IF AVAILABLE item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAILABLE item THEN item.s-wid ELSE 0.
            IF v-bwt EQ 0 THEN v-bwt = IF AVAILABLE item THEN item.basis-w ELSE 0.
            ASSIGN 
                lv-qty-uom  = rm-rctd.pur-uom
                lv-cost-uom = rm-rctd.cost-uom.
  
            /* convert qty    pr-qty-uom or po-ordl.pr-uom cons-uom*/
            /* run rm/convquom.p(rm-rctd.pur-uom,
                               po-ordl.cons-uom,
                                    v-bwt,
                                    v-len,
                                    input v-wid,
                                    input v-dep,
                                    input rm-rctd.qty,
                                    output lv-out-qty).
             
             /* convert cost pr-uom*/
             run rm/convcuom.p(rm-rctd.cost-uom, po-ordl.cons-uom,
                               v-bwt, v-len, v-wid, v-dep,
                                          rm-rctd.cost, output lv-out-cost).
             */
        
            RUN custom/convquom.p(cocode,
                rm-rctd.pur-uom,
                lv-qty-uom,
                v-bwt,
                v-len,
                v-wid,
                v-dep,
                rm-rctd.qty,
                OUTPUT lv-out-qty).
  
            /* convert cost pr-uom*/
            RUN custom/convcuom.p(cocode,
                rm-rctd.cost-uom,
                lv-qty-uom,
                v-bwt,
                v-len,
                v-wid,
                v-dep,
                rm-rctd.cost, OUTPUT lv-out-cost).

            ASSIGN
                ext-cost = lv-out-qty * lv-out-cost
                ll-disp  = YES.
  
            /*    ASSIGN                                                                     */
            /*      rm-rctd.cost:SCREEN-VALUE  = STRING(lv-out-cost) */
            /*      rm-rctd.cost-uom:SCREEN-VALUE  = lv-cost-uom     */
            /*      ext-cost:SCREEN-VALUE  = STRING(ext-cost)        */
            /*      rm-rctd.qty:SCREEN-VALUE  = STRING(lv-out-qty)   */
            /*      rm-rctd.pur-uom:SCREEN-VALUE  = lv-qty-uom       */
            /*      NO-ERROR.                                                                */

            ASSIGN
                rm-rctd.qty:MODIFIED     = NO
                rm-rctd.pur-uom:MODIFIED = NO.

        /*disp ext-cost with browse {&BROWSE-NAME}. it's displayed automatically */
        /* message "after calc:" po-ordl.cons-uom rm-rctd.cost-uom lv-out-cost ext-cost.
         */

        END. /* ip-first */
        /* ======================================================================= */
        ELSE 
            IF AVAILABLE rm-rctd AND rm-rctd.i-no:SCREEN-VALUE  <> "" THEN 
            DO: /* in update mode - use screen-value */
                ASSIGN
                    lv-uom     = rm-rctd.pur-uom:SCREEN-VALUE 
                    lv-out-qty = DEC(rm-rctd.qty:SCREEN-VALUE ).

                FIND item  WHERE item.company EQ cocode
                    AND item.i-no  EQ rm-rctd.i-no:screen-value 
                    USE-INDEX i-no NO-LOCK NO-ERROR.
                IF AVAILABLE item THEN v-dep = item.s-dep.    
  
                RELEASE po-ordl.

                IF INT(rm-rctd.po-no:SCREEN-VALUE ) NE 0 THEN
                    FIND FIRST po-ordl NO-LOCK
                        WHERE po-ordl.company EQ cocode
                        AND po-ordl.po-no   EQ INT(rm-rctd.po-no:SCREEN-VALUE )
                        AND (po-ordl.i-no   EQ lv-i-no OR lv-i-no EQ "")
                        AND (po-ordl.line   EQ lv-line OR lv-line EQ 0)
                        NO-ERROR.

                IF AVAILABLE po-ordl THEN 
                DO:
                    ASSIGN
                        v-len   = po-ordl.s-len
                        v-wid   = po-ordl.s-wid
                        v-bwt   = 0
                        lv-i-no = po-ordl.i-no
                        lv-line = po-ordl.line.
                    {rm/pol-dims.i}
                END.

                ELSE 
                DO:
                    FIND FIRST job WHERE job.company EQ cocode
                        AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE 
                        AND job.job-no2 EQ integer(rm-rctd.job-no2:SCREEN-VALUE )
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE job THEN 
                    DO :
                        v-job-up = 0.
                        FOR EACH job-hdr FIELDS(n-on)
                            WHERE job-hdr.company EQ cocode
                            AND job-hdr.job     EQ job.job
                            AND job-hdr.job-no  EQ job.job-no
                            AND job-hdr.job-no2 EQ job.job-no2
                            AND job-hdr.frm     EQ int(rm-rctd.s-num:screen-value)
                            NO-LOCK:
                            v-job-up = v-job-up + job-hdr.n-on.  
                        END.
             
                        FIND FIRST job-mat WHERE job-mat.company EQ cocode
                            AND job-mat.job     EQ job.job
                            AND job-mat.i-no    EQ rm-rctd.i-no:SCREEN-VALUE 
                            AND job-mat.frm     EQ int(rm-rctd.s-num:SCREEN-VALUE )
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE job-mat THEN 
                        DO:
                            IF lv-rmissue EQ "Net" THEN v-out = job-mat.n-up / v-job-up.
                            ASSIGN 
                                v-len = job-mat.len
                                v-wid = job-mat.wid
                                v-bwt = job-mat.basis-w.
                        END.
                    END.
                END.

                IF v-len EQ 0 THEN v-len = IF AVAILABLE item THEN item.s-len ELSE 0.
                IF v-wid EQ 0 THEN v-wid = IF AVAILABLE item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAILABLE item THEN item.s-wid ELSE 0.
                IF v-bwt EQ 0 THEN v-bwt = IF AVAILABLE item THEN item.basis-w ELSE 0.

                ASSIGN 
                    lv-qty-uom  = item.cons-uom
                    lv-cost-uom = ITEM.cons-uom .

                IF lv-uom EQ "DIA" THEN 
                DO:
                    ld-lf-used = 0.

                    FOR EACH rm-rcpth
                        WHERE rm-rcpth.company   EQ cocode
                        AND rm-rcpth.i-no      EQ rm-rctd.i-no:SCREEN-VALUE 
                        AND rm-rcpth.rita-code EQ "R"
                        NO-LOCK,
                        EACH rm-rdtlh FIELDS(qty)
                        WHERE rm-rdtlh.company   EQ rm-rcpth.company
                        AND rm-rdtlh.r-no      EQ rm-rcpth.r-no
                        AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                        AND rm-rdtlh.loc       EQ rm-rctd.loc:SCREEN-VALUE 
                        AND rm-rdtlh.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE 
                        AND rm-rdtlh.tag       EQ rm-rctd.tag:SCREEN-VALUE 
                        NO-LOCK:

                        ld = rm-rdtlh.qty.

                        IF rm-rcpth.pur-uom NE "LF" THEN
                            RUN rm/convquom.p(rm-rcpth.pur-uom, "LF",
                                v-bwt, v-len, v-wid, v-dep,
                                ld, OUTPUT ld).

                        ld-lf-used = ld-lf-used + ld.
                    END.

                    FOR EACH b-rm-rctd FIELDS(qty pur-uom)
                        WHERE b-rm-rctd.company   EQ cocode
                        AND b-rm-rctd.i-no      EQ rm-rctd.i-no:SCREEN-VALUE 
                        AND b-rm-rctd.rita-code EQ "R"
                        AND b-rm-rctd.loc       EQ rm-rctd.loc:SCREEN-VALUE 
                        AND b-rm-rctd.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE 
                        AND b-rm-rctd.tag       EQ rm-rctd.tag:SCREEN-VALUE 
                        AND ROWID(b-rm-rctd)    NE ROWID(rm-rctd)
                        NO-LOCK:

                        ld = b-rm-rctd.qty.

                        IF b-rm-rctd.pur-uom NE "LF" THEN
                            RUN rm/convquom.p(b-rm-rctd.pur-uom, "LF",
                                v-bwt, v-len, v-wid, v-dep,
                                ld, OUTPUT ld).

                        ld-lf-used = ld-lf-used + ld.
                    END.

                    FOR EACH rm-rcpth
                        WHERE rm-rcpth.company   EQ cocode
                        AND rm-rcpth.i-no      EQ rm-rctd.i-no:SCREEN-VALUE 
                        AND rm-rcpth.rita-code EQ "I"
                        NO-LOCK,
                        EACH rm-rdtlh FIELDS(qty)
                        WHERE rm-rdtlh.company   EQ rm-rcpth.company
                        AND rm-rdtlh.r-no      EQ rm-rcpth.r-no
                        AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                        AND rm-rdtlh.loc       EQ rm-rctd.loc:SCREEN-VALUE 
                        AND rm-rdtlh.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE 
                        AND rm-rdtlh.tag       EQ rm-rctd.tag:SCREEN-VALUE 
                        NO-LOCK:

                        ld = rm-rdtlh.qty.

                        IF rm-rcpth.pur-uom NE "LF" THEN
                            RUN rm/convquom.p(rm-rcpth.pur-uom, "LF",
                                v-bwt, v-len, v-wid, v-dep,
                                ld, OUTPUT ld).

                        ld-lf-used = ld-lf-used - ld.
                    END.

                    FOR EACH b-rm-rctd FIELDS(qty pur-uom)
                        WHERE b-rm-rctd.company   EQ cocode
                        AND b-rm-rctd.i-no      EQ rm-rctd.i-no:SCREEN-VALUE 
                        AND b-rm-rctd.rita-code EQ "I"
                        AND b-rm-rctd.loc       EQ rm-rctd.loc:SCREEN-VALUE 
                        AND b-rm-rctd.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE 
                        AND b-rm-rctd.tag       EQ rm-rctd.tag:SCREEN-VALUE 
                        AND ROWID(b-rm-rctd)    NE ROWID(rm-rctd)
                        NO-LOCK:

                        ld = b-rm-rctd.qty.

                        IF b-rm-rctd.pur-uom NE "LF" THEN
                            RUN rm/convquom.p(b-rm-rctd.pur-uom, "LF",
                                v-bwt, v-len, v-wid, v-dep,
                                ld, OUTPUT ld).

                        ld-lf-used = ld-lf-used - ld.
                    END.

                    ld = item.ect / 10000.

                    IF ld LE 0 THEN 
                    DO TRANSACTION:
                        MESSAGE "Please enter Core Diameter:" UPDATE ld.
                        FIND CURRENT item EXCLUSIVE NO-ERROR.
                        IF AVAILABLE item THEN item.ect = ld * 10000.
                        FIND CURRENT item NO-LOCK NO-ERROR.
                    END.

                    ASSIGN
                        lv-out-qty = ld-lf-used -
                  (((lv-out-qty * lv-out-qty) - (ld * ld)) *
                   .0655 / item.cal)
                        lv-uom     = "LF".
                END. /* IF lv-uom EQ "DIA" */

                /* convert qty */
                IF lv-uom NE lv-qty-uom THEN
                    RUN rm/convquom.p(lv-uom, lv-qty-uom, v-bwt, v-len, v-wid, v-dep,
                        lv-out-qty / v-out, OUTPUT lv-out-qty).
  
                /* convert cost */
                IF rm-rctd.cost-uom:screen-value  EQ lv-cost-uom THEN
                    lv-out-cost = dec(rm-rctd.cost:screen-value ).
                ELSE
                    RUN rm/convcuom.p(rm-rctd.cost-uom:screen-value ,
                        lv-cost-uom, v-bwt, v-len, v-wid, v-dep,
                        rm-rctd.cost:screen-value ,
                        OUTPUT lv-out-cost).

                ASSIGN
                    ext-cost = lv-out-qty * lv-out-cost
                    ll-disp  = YES.

                ASSIGN
                    rm-rctd.cost:SCREEN-VALUE     = STRING(lv-out-cost)
                    rm-rctd.cost-uom:SCREEN-VALUE = lv-cost-uom
                    rm-rctd.qty:SCREEN-VALUE      = STRING(lv-out-qty)
                    rm-rctd.pur-uom:SCREEN-VALUE  = lv-qty-uom
                    fi_ext-amount:SCREEN-VALUE    = STRING(ext-cost)
     NO-ERROR.
            END.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix-tandem-rec M-Win 
PROCEDURE get-matrix-tandem-rec :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-dep       LIKE po-ordl.s-len NO-UNDO. 
    DEFINE VARIABLE v-bwt       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE lv-out-qty  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-out-cost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-qty-uom  AS cha     NO-UNDO.
    DEFINE VARIABLE lv-cost-uom AS cha     NO-UNDO.
    DEFINE VARIABLE v-job-up    LIKE job-hdr.n-on NO-UNDO.
    DEFINE VARIABLE v-out       LIKE ef.n-out INIT 1 NO-UNDO.
    DEFINE VARIABLE lv-uom      LIKE rm-rctd.pur-uom NO-UNDO.
    DEFINE VARIABLE ld-lf-used  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ld          AS DECIMAL NO-UNDO FORMAT ">,>>9.9<<<".
    DEFINE VARIABLE ll-disp     AS LOG     INIT NO NO-UNDO.

    DEFINE BUFFER b-rm-rctd FOR rm-rctd.
    DO WITH FRAME {&FRAME-NAME}:  
        IF AVAILABLE rm-rctd AND rm-rctd.i-no:SCREEN-VALUE  <> "" THEN 
        DO:
            ASSIGN
                lv-uom     = rm-rctd.pur-uom:SCREEN-VALUE 
                lv-out-qty = DEC(rm-rctd.qty:SCREEN-VALUE ).
    
            FIND item  WHERE item.company EQ cocode
                AND item.i-no  EQ rm-rctd.i-no:screen-value 
                USE-INDEX i-no NO-LOCK NO-ERROR.
            IF AVAILABLE item THEN v-dep = item.s-dep.    
     
            RELEASE po-ordl.
    
            IF INT(rm-rctd.po-no:SCREEN-VALUE ) NE 0 THEN
                FIND FIRST po-ordl NO-LOCK
                    WHERE po-ordl.company EQ cocode
                    AND po-ordl.po-no   EQ INT(rm-rctd.po-no:SCREEN-VALUE )
                    AND (po-ordl.i-no   EQ lv-i-no OR lv-i-no EQ "")
                    AND (po-ordl.line   EQ lv-line OR lv-line EQ 0)
                    NO-ERROR.
    
            IF AVAILABLE po-ordl THEN 
            DO:
                ASSIGN
                    v-len   = po-ordl.s-len
                    v-wid   = po-ordl.s-wid
                    v-bwt   = 0
                    lv-i-no = po-ordl.i-no
                    lv-line = po-ordl.line.
                {rm/pol-dims.i}
            END.
    
            ELSE 
            DO:
                FIND FIRST job WHERE job.company EQ cocode
                    AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE 
                    AND job.job-no2 EQ integer(rm-rctd.job-no2:SCREEN-VALUE )
                    NO-LOCK NO-ERROR.
                IF AVAILABLE job THEN 
                DO :
                    v-job-up = 0.
                    FOR EACH job-hdr FIELDS(n-on)
                        WHERE job-hdr.company EQ cocode
                        AND job-hdr.job     EQ job.job
                        AND job-hdr.job-no  EQ job.job-no
                        AND job-hdr.job-no2 EQ job.job-no2
                        AND job-hdr.frm     EQ int(rm-rctd.s-num:screen-value)
                        NO-LOCK:
                        v-job-up = v-job-up + job-hdr.n-on.  
                    END.
             
                    FIND FIRST job-mat WHERE job-mat.company EQ cocode
                        AND job-mat.job     EQ job.job
                        AND job-mat.i-no    EQ rm-rctd.i-no:SCREEN-VALUE 
                        AND job-mat.frm     EQ int(rm-rctd.s-num:SCREEN-VALUE )
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE job-mat THEN 
                    DO:
                        IF lv-rmissue EQ "Net" THEN v-out = job-mat.n-up / v-job-up.
                        ASSIGN 
                            v-len = job-mat.len
                            v-wid = job-mat.wid
                            v-bwt = job-mat.basis-w.
                    END.
                END.
            END.
    
            IF v-len EQ 0 THEN v-len = IF AVAILABLE item THEN item.s-len ELSE 0.
            IF v-wid EQ 0 THEN v-wid = IF AVAILABLE item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAILABLE item THEN item.s-wid ELSE 0.
            IF v-bwt EQ 0 THEN v-bwt = IF AVAILABLE item THEN item.basis-w ELSE 0.
    
            ASSIGN 
                lv-qty-uom  = item.cons-uom
                lv-cost-uom = ITEM.cons-uom .
    
            IF lv-uom EQ "DIA" THEN 
            DO:
                ld-lf-used = 0.

                FOR EACH rm-rcpth
                    WHERE rm-rcpth.company   EQ cocode
                    AND rm-rcpth.i-no      EQ rm-rctd.i-no:SCREEN-VALUE 
                    AND rm-rcpth.rita-code EQ "R"
                    NO-LOCK,
                    EACH rm-rdtlh FIELDS(qty)
                    WHERE rm-rdtlh.company   EQ rm-rcpth.company
                    AND rm-rdtlh.r-no      EQ rm-rcpth.r-no
                    AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                    AND rm-rdtlh.loc       EQ rm-rctd.loc:SCREEN-VALUE 
                    AND rm-rdtlh.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE 
                    AND rm-rdtlh.tag       EQ rm-rctd.tag:SCREEN-VALUE 
                    NO-LOCK:

                    ld = rm-rdtlh.qty.

                    IF rm-rcpth.pur-uom NE "LF" THEN
                        RUN rm/convquom.p(rm-rcpth.pur-uom, "LF",
                            v-bwt, v-len, v-wid, v-dep,
                            ld, OUTPUT ld).

                    ld-lf-used = ld-lf-used + ld.
                END.

                FOR EACH b-rm-rctd FIELDS(qty pur-uom)
                    WHERE b-rm-rctd.company   EQ cocode
                    AND b-rm-rctd.i-no      EQ rm-rctd.i-no:SCREEN-VALUE 
                    AND b-rm-rctd.rita-code EQ "R"
                    AND b-rm-rctd.loc       EQ rm-rctd.loc:SCREEN-VALUE 
                    AND b-rm-rctd.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE 
                    AND b-rm-rctd.tag       EQ rm-rctd.tag:SCREEN-VALUE 
                    AND ROWID(b-rm-rctd)    NE ROWID(rm-rctd)
                    NO-LOCK:

                    ld = b-rm-rctd.qty.

                    IF b-rm-rctd.pur-uom NE "LF" THEN
                        RUN rm/convquom.p(b-rm-rctd.pur-uom, "LF",
                            v-bwt, v-len, v-wid, v-dep,
                            ld, OUTPUT ld).

                    ld-lf-used = ld-lf-used + ld.
                END.

                FOR EACH rm-rcpth
                    WHERE rm-rcpth.company   EQ cocode
                    AND rm-rcpth.i-no      EQ rm-rctd.i-no:SCREEN-VALUE 
                    AND rm-rcpth.rita-code EQ "I"
                    NO-LOCK,
                    EACH rm-rdtlh FIELDS(qty)
                    WHERE rm-rdtlh.company   EQ rm-rcpth.company
                    AND rm-rdtlh.r-no      EQ rm-rcpth.r-no
                    AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                    AND rm-rdtlh.loc       EQ rm-rctd.loc:SCREEN-VALUE 
                    AND rm-rdtlh.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE 
                    AND rm-rdtlh.tag       EQ rm-rctd.tag:SCREEN-VALUE 
                    NO-LOCK:

                    ld = rm-rdtlh.qty.

                    IF rm-rcpth.pur-uom NE "LF" THEN
                        RUN rm/convquom.p(rm-rcpth.pur-uom, "LF",
                            v-bwt, v-len, v-wid, v-dep,
                            ld, OUTPUT ld).

                    ld-lf-used = ld-lf-used - ld.
                END.

                FOR EACH b-rm-rctd FIELDS(qty pur-uom)
                    WHERE b-rm-rctd.company   EQ cocode
                    AND b-rm-rctd.i-no      EQ rm-rctd.i-no:SCREEN-VALUE 
                    AND b-rm-rctd.rita-code EQ "I"
                    AND b-rm-rctd.loc       EQ rm-rctd.loc:SCREEN-VALUE 
                    AND b-rm-rctd.loc-bin   EQ rm-rctd.loc-bin:SCREEN-VALUE 
                    AND b-rm-rctd.tag       EQ rm-rctd.tag:SCREEN-VALUE 
                    AND ROWID(b-rm-rctd)    NE ROWID(rm-rctd)
                    NO-LOCK:

                    ld = b-rm-rctd.qty.

                    IF b-rm-rctd.pur-uom NE "LF" THEN
                        RUN rm/convquom.p(b-rm-rctd.pur-uom, "LF",
                            v-bwt, v-len, v-wid, v-dep,
                            ld, OUTPUT ld).

                    ld-lf-used = ld-lf-used - ld.
                END.

                ld = item.ect / 10000.

                IF ld LE 0 THEN 
                DO TRANSACTION:
                    MESSAGE "Please enter Core Diameter:" UPDATE ld.
                    FIND CURRENT item EXCLUSIVE NO-ERROR.
                    IF AVAILABLE item THEN item.ect = ld * 10000.
                    FIND CURRENT item NO-LOCK NO-ERROR.
                END.

                ASSIGN
                    lv-out-qty = ld-lf-used -
                      (((lv-out-qty * lv-out-qty) - (ld * ld)) *
                       .0655 / item.cal)
                    lv-uom     = "LF".
            END. /* IF lv-uom EQ "DIA" */
    

            /* convert qty */
            IF lv-uom NE lv-qty-uom THEN
                RUN rm/convquom.p(lv-uom, lv-qty-uom, v-bwt, v-len, v-wid, v-dep,
                    lv-out-qty / v-out, OUTPUT lv-out-qty).
     
            /* convert cost */
            IF rm-rctd.cost-uom:screen-value  EQ lv-cost-uom THEN
                lv-out-cost = dec(rm-rctd.cost:screen-value ).
            ELSE
                RUN rm/convcuom.p(rm-rctd.cost-uom:screen-value ,
                    lv-cost-uom, v-bwt, v-len, v-wid, v-dep,
                    rm-rctd.cost:screen-value ,
                    OUTPUT lv-out-cost).
    
            ASSIGN
                ext-cost = lv-out-qty * lv-out-cost
                ll-disp  = YES.
        END.

        IF ll-disp THEN
            ASSIGN
                rm-rctd.cost:SCREEN-VALUE     = STRING(lv-out-cost)
                rm-rctd.cost-uom:SCREEN-VALUE = lv-cost-uom
                rm-rctd.qty:SCREEN-VALUE      = STRING(lv-out-qty)
                rm-rctd.pur-uom:SCREEN-VALUE  = lv-qty-uom
                fi_ext-amount:SCREEN-VALUE    = STRING(ext-cost)
   NO-ERROR.

    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-qty-matrix M-Win 
PROCEDURE get-qty-matrix :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DEFINE VARIABLE ld    LIKE job-mat.qty NO-UNDO.
    DEFINE VARIABLE v-qty LIKE job-mat.qty NO-UNDO. 

    RELEASE job.

    EMPTY TEMP-TABLE tt-frm.

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST job WHERE
            job.company EQ cocode AND
            job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE  AND
            job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE )
            NO-LOCK NO-ERROR.

        IF AVAILABLE job THEN 
        DO:
            ld = 0.
    
            FOR EACH w-rm-rctd WHERE
                w-rm-rctd.i-no EQ rm-rctd.i-no:SCREEN-VALUE :

                FIND FIRST tt-frm WHERE
                    tt-frm.frm = w-rm-rctd.s-num
                    NO-ERROR.

                IF NOT AVAILABLE tt-frm THEN
                DO:
                    CREATE tt-frm.
                    ASSIGN 
                        tt-frm.frm = w-rm-rctd.s-num.
                    RELEASE tt-frm.
                END.
            END.

            FOR EACH tt-frm,
                EACH job-mat FIELDS(frm qty qty-uom rm-i-no job-no job-no2) WHERE
                job-mat.company EQ job.company AND
                job-mat.job     EQ job.job AND
                job-mat.job-no  EQ job.job-no AND
                job-mat.job-no2 EQ job.job-no2 AND
                job-mat.rm-i-no EQ rm-rctd.i-no:SCREEN-VALUE  AND
                job-mat.frm EQ tt-frm.frm
                NO-LOCK:
    
                RUN tandem-rec-uom-conv(INPUT job-mat.rm-i-no,
                    INPUT job-mat.qty-uom,
                    INPUT job-mat.qty,
                    INPUT job-mat.job-no,
                    INPUT job-mat.job-no2,
                    INPUT job-mat.frm,
                    OUTPUT v-qty).

                ASSIGN
                    tt-frm.mrp = tt-frm.mrp + v-qty
                    ld         = ld + v-qty.
            END.
    
            IF ld NE 0 THEN
                FOR EACH tt-frm:
                    tt-frm.qtypct = (tt-frm.mrp / ld).
                END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-tandem-rec M-Win 
PROCEDURE get-tandem-rec :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-cons-uom AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE ld AS DECIMAL NO-UNDO.

    v-get-tandem-rec = YES.

    RUN get-qty-matrix.

    EMPTY TEMP-TABLE tt-tag.

    FOR EACH tt-selected,
        FIRST rm-bin FIELDS(qty tag) WHERE
        ROWID(rm-bin) EQ tt-selected.tt-rowid
        NO-LOCK:

        CREATE tt-tag.
        ASSIGN
            tt-tag.tag      = rm-bin.tag
            tt-tag.qty      = rm-bin.qty
            tt-tag.tt-rowid = ROWID(rm-bin)
            ld              = ld + rm-bin.qty.
    END.

    FOR EACH tt-frm:
        tt-frm.qty = ROUND(ld * tt-frm.qtypct,2).

        IF ip-cons-uom EQ "EA" THEN
        DO:
            {sys/inc/roundup.i tt-frm.qty}
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lookup-job-mat M-Win 
PROCEDURE lookup-job-mat :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-for-item-only AS LOG NO-UNDO.

    DEFINE VARIABLE count-mat AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll-lookup AS LOG     NO-UNDO.


    IF ip-for-item-only EQ ? THEN
        ASSIGN
            ip-for-item-only = YES
            ll-lookup        = YES.

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST item 
            WHERE item.company EQ cocode
            AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE 
            NO-LOCK NO-ERROR.

        IF AVAILABLE item AND rm-rctd.i-no:SCREEN-VALUE  NE "" THEN 
        DO:
            EMPTY TEMP-TABLE item-chg.
      
            count-mat = 0.

            FOR EACH job-mat
                WHERE job-mat.company    EQ cocode
                AND job-mat.job        EQ job.job 
                AND job-mat.job-no     EQ INPUT rm-rctd.job-no
                AND job-mat.job-no2    EQ INPUT rm-rctd.job-no2 
                AND (ip-for-item-only OR
                (job-mat.frm      EQ INT(rm-rctd.s-num:SCREEN-VALUE) AND
                job-mat.blank-no EQ INT(rm-rctd.b-num:SCREEN-VALUE)))
                USE-INDEX seq-idx NO-LOCK,

                FIRST xitem FIELDS(i-no)
                WHERE xitem.company  EQ cocode
                AND xitem.i-no     EQ job-mat.rm-i-no
                AND xitem.mat-type EQ item.mat-type
                NO-LOCK
          
                BREAK BY job-mat.frm
                BY job-mat.blank-no:

                IF FIRST-OF(job-mat.blank-no) OR NOT ll-lookup THEN 
                DO:
                    count-mat = count-mat + 1.
                    CREATE item-chg.
                    ASSIGN
                        item-chg.i-no   = xitem.i-no
                        item-chg.rec-id = RECID(job-mat)
                        fil_id          = RECID(item-chg).
                END.
            END.

            IF ll-lookup THEN fil_id = ?.
      
            IF count-mat NE 1 OR ll-lookup THEN RUN rm/g-itmchg.w.
      
            FIND FIRST item-chg WHERE RECID(item-chg) EQ fil_id NO-LOCK NO-ERROR.
            IF AVAILABLE item-chg THEN fil_id = item-chg.rec-id.
        END.
    END.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE multi-issues M-Win 
PROCEDURE multi-issues :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.  

    DEFINE BUFFER b-rm-rctd  FOR rm-rctd.
    DEFINE BUFFER bf-rm-rctd FOR rm-rctd.
    DEFINE VARIABLE v-continue AS LOG     INIT YES NO-UNDO.
    DEFINE VARIABLE li         AS INTEGER INIT 0 NO-UNDO.
    DO WITH FRAME {&frame-name}:

        IF v-get-tandem-rec THEN
        DO:
            DO WHILE v-continue:
           
                FIND FIRST tt-frm WHERE
                    tt-frm.qty GT 0
                    NO-ERROR.
             
                IF AVAILABLE tt-frm THEN
                DO:
                    FOR FIRST tt-tag USE-INDEX tag-no,
                        FIRST tt-selected WHERE tt-selected.tt-rowid EQ tt-tag.tt-rowid,
                        FIRST rm-bin FIELDS(loc loc-bin tag cost) WHERE
                        ROWID(rm-bin) EQ tt-tag.tt-rowid
                        NO-LOCK:
             
                        /*RUN dispatch ('copy-record').*/
                        RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT li) NO-ERROR.
                     
                        CREATE bf-rm-rctd.
                        ASSIGN
                            bf-rm-rctd.company   = cocode 
                            bf-rm-rctd.r-no      = li 
                            bf-rm-rctd.rita-code = "I"
                            bf-rm-rctd.user-id   = USERID(LDBNAME(1)) .

                        BUFFER-COPY rm-rctd EXCEPT rec_key company r-no rita-code user-id TO bf-rm-rctd.

                        FIND rm-rctd NO-LOCK
                            WHERE rm-rctd.company EQ cocode
                            AND RECID(rm-rctd)  EQ RECID(bf-rm-rctd) NO-ERROR .
                     
                        RUN display-item. 
                        /* {&BROWSE-NAME}:SELECT-FOCUSED-ROW().*/

                        ASSIGN
                            rm-rctd.loc:SCREEN-VALUE     = rm-bin.loc
                            rm-rctd.loc-bin:SCREEN-VALUE = rm-bin.loc-bin
                            rm-rctd.tag:SCREEN-VALUE     = rm-bin.tag
                            rm-rctd.cost:SCREEN-VALUE    = STRING(rm-bin.cost).
              
                        IF tt-tag.qty LE tt-frm.qty THEN
                        DO:
                            ASSIGN
                                rm-rctd.qty:SCREEN-VALUE   = STRING(tt-tag.qty)
                                rm-rctd.s-num:SCREEN-VALUE = STRING(tt-frm.frm)
                                tt-frm.qty                 = tt-frm.qty - tt-tag.qty.
             
                            DELETE tt-tag.
                            DELETE tt-selected.

                            IF tt-frm.qty EQ 0 THEN
                                DELETE tt-frm.
                        END.
                        ELSE /*tt-tag.qty GT tt-frm.qty*/
                        DO:
                            ASSIGN
                                rm-rctd.qty:SCREEN-VALUE   = STRING(tt-frm.qty)
                                rm-rctd.s-num:SCREEN-VALUE = STRING(tt-frm.frm)
                                tt-tag.qty                 = tt-tag.qty - tt-frm.qty.

                            DELETE tt-frm.
                        END.

                        RUN get-matrix-tandem-rec.
       
                        RUN valid-all NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

                        FIND CURRENT rm-rctd EXCLUSIVE-LOCK NO-ERROR.
                     
                        DO WITH FRAME {&FRAME-NAME}:
                            ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
                        END.
                     
                        /*RUN dispatch ('assign-record').*/
                     
                        IF v-rmtags-log AND
                            rm-rctd.tag NE "" AND
                            NOT CAN-FIND(FIRST wiptag WHERE
                            wiptag.company EQ cocode AND
                            wiptag.rm-tag-no EQ rm-rctd.tag) THEN
                        DO:
                            MESSAGE "Launch WIP Tag Creation for Item " rm-rctd.i-no "Tag #" rm-rctd.tag "?" VIEW-AS ALERT-BOX QUESTION
                                BUTTON YES-NO UPDATE ll-wip AS LOG.
                       
                            /* btr */
                            IF ll-wip THEN
                                RUN jcrep/wipldtg.w  (INPUT rm-rctd.tag,
                                    INPUT rm-rctd.job-no,
                                    INPUT rm-rctd.job-no2,
                                    INPUT rm-rctd.i-no,
                                    INPUT rm-rctd.s-num,
                                    INPUT rm-rctd.b-num,
                                    INPUT rm-rctd.qty,
                                    INPUT rm-rctd.pur-uom,
                                    OUTPUT vlc-success).
                        END.

                        IF NOT CAN-FIND(FIRST tt-frm) OR
                            NOT CAN-FIND(FIRST tt-tag) THEN
                            v-continue = NO.
                    END.
                END.
            END.
        END.
        ELSE
        DO:
            FOR EACH tt-selected,
                FIRST rm-bin WHERE ROWID(rm-bin) EQ tt-selected.tt-rowid:
       
                IF NOT CAN-FIND(FIRST b-rm-rctd
                    WHERE b-rm-rctd.company  EQ rm-rctd.company
                    AND b-rm-rctd.rct-date EQ rm-rctd.rct-date
                    AND b-rm-rctd.job-no   EQ rm-rctd.job-no
                    AND b-rm-rctd.job-no2  EQ rm-rctd.job-no2
                    AND b-rm-rctd.i-no     EQ rm-rctd.i-no
                    AND b-rm-rctd.loc      EQ rm-bin.loc
                    AND b-rm-rctd.loc-bin  EQ rm-bin.loc-bin
                    AND b-rm-rctd.tag      EQ rm-bin.tag) THEN 
                DO:
       
                    /*RUN dispatch ('copy-record').*/
                    RUN sys/ref/asiseq.p (INPUT cocode, INPUT "rm_rcpt_seq", OUTPUT li) NO-ERROR.
                     
                    CREATE bf-rm-rctd.
                    ASSIGN
                        bf-rm-rctd.company   = cocode 
                        bf-rm-rctd.r-no      = li 
                        bf-rm-rctd.rita-code = "I"
                        bf-rm-rctd.user-id   = USERID(LDBNAME(1)) .

                    BUFFER-COPY rm-rctd EXCEPT rec_key company r-no rita-code user-id TO bf-rm-rctd.

                    FIND rm-rctd NO-LOCK
                        WHERE rm-rctd.company EQ cocode
                        AND RECID(rm-rctd)  EQ RECID(bf-rm-rctd) NO-ERROR .
                     
                    RUN display-item. 
       
                    /*{&BROWSE-NAME}:SELECT-FOCUSED-ROW().*/
       
                    ASSIGN
                        rm-rctd.loc:SCREEN-VALUE     = rm-bin.loc
                        rm-rctd.loc-bin:SCREEN-VALUE = rm-bin.loc-bin
                        rm-rctd.tag:SCREEN-VALUE     = rm-bin.tag
                        rm-rctd.qty:SCREEN-VALUE     = ""
                        rm-rctd.cost:SCREEN-VALUE    = "".
       
                    RUN new-bin.
       
                    RUN get-matrix (NO).        
       
                    RUN valid-all NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
       
                    FIND CURRENT rm-rctd EXCLUSIVE-LOCK NO-ERROR.
                     
                    DO WITH FRAME {&FRAME-NAME}:
                        ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}} .
                    END.
                    /*RUN dispatch ('assign-record').*/
       
                    IF v-rmtags-log AND
                        rm-rctd.tag NE "" AND
                        NOT CAN-FIND(FIRST wiptag WHERE
                        wiptag.company EQ cocode AND
                        wiptag.rm-tag-no EQ rm-rctd.tag) THEN
                    DO:
                        MESSAGE "Launch WIP Tag Creation for Item " rm-rctd.i-no "Tag #" rm-rctd.tag "?" VIEW-AS ALERT-BOX QUESTION
                            BUTTON YES-NO UPDATE ll-wip-2 AS LOG.
              
                        IF ll-wip-2 AND rm-rctd.spare-char-1 <> "WIP-Issued" THEN
                            RUN jcrep/wipldtg.w  (INPUT rm-rctd.tag,
                                INPUT rm-rctd.job-no,
                                INPUT rm-rctd.job-no2,
                                INPUT rm-rctd.i-no,
                                INPUT rm-rctd.s-num,
                                INPUT rm-rctd.b-num,
                                INPUT rm-rctd.qty,
                                INPUT rm-rctd.pur-uom,
                                OUTPUT vlc-success).
                    END.
       
               
                END.
       
                DELETE tt-selected.
            END.
        END.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-bin M-Win 
PROCEDURE new-bin :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST rm-bin 
            WHERE rm-bin.company EQ cocode
            AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE 
            AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE 
            AND rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE 
            AND rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE 
            NO-LOCK NO-ERROR.
        IF AVAILABLE rm-bin THEN 
        DO:
            rm-rctd.cost:SCREEN-VALUE  = STRING(rm-bin.cost).
            IF rm-rctd.tag:SCREEN-VALUE  NE "" THEN
                rm-rctd.qty:SCREEN-VALUE  = STRING(rm-bin.qty).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-i-no M-Win 
PROCEDURE new-i-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-item FOR item.


    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST item 
            WHERE item.company EQ cocode
            AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE 
            AND item.i-code  EQ "R"
            NO-LOCK NO-ERROR.

        IF AVAILABLE item THEN 
        DO:
            ASSIGN
                rm-rctd.i-name:SCREEN-VALUE   = item.i-name
                rm-rctd.pur-uom:SCREEN-VALUE  = item.cons-uom
                rm-rctd.cost-uom:SCREEN-VALUE = item.cons-uom.

            FOR EACH rm-bin 
                WHERE rm-bin.company EQ cocode
                AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE 
                NO-LOCK
                BY rm-bin.qty DESCENDING BY rm-bin.loc BY rm-bin.loc-bin BY rm-bin.tag:
                IF v-bin NE "user entered" THEN
                    ASSIGN
                        rm-rctd.loc:SCREEN-VALUE     = rm-bin.loc
                        rm-rctd.loc-bin:SCREEN-VALUE = rm-bin.loc-bin.
                rm-rctd.tag:SCREEN-VALUE      = rm-bin.tag.

                RUN new-bin.

                LEAVE.
            END.

            FOR EACH job
                WHERE job.company EQ cocode
                AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE 
                AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE )
                NO-LOCK,
                EACH job-mat
                WHERE job-mat.company EQ job.company
                AND job-mat.job     EQ job.job
                AND job-mat.job-no  EQ job.job-no
                AND job-mat.job-no2 EQ job.job-no2
                NO-LOCK,
                FIRST b-item
                WHERE b-item.company  EQ job-mat.company
                AND b-item.i-no     EQ job-mat.i-no
                AND b-item.mat-type EQ item.mat-type
                NO-LOCK
                BREAK BY job-mat.frm      DESCENDING
                BY job-mat.blank-no DESCENDING:

                IF job-mat.i-no EQ rm-rctd.i-no:SCREEN-VALUE  OR
                    LAST(job-mat.frm)                                                  THEN 
                DO:
                    ASSIGN
                        rm-rctd.s-num:SCREEN-VALUE = STRING(job-mat.frm)
                        rm-rctd.b-num:SCREEN-VALUE = STRING(job-mat.blank-no).
                    LEAVE.
                END.
            END.
        END.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-job-no M-Win 
PROCEDURE new-job-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-job-no LIKE rm-rctd.job-no NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            rm-rctd.s-num:READ-ONLY = NO
            rm-rctd.b-num:READ-ONLY = NO.

        IF rm-rctd.job-no:SCREEN-VALUE  NE "" THEN 
        DO:
            ASSIGN
                lv-job-no = rm-rctd.job-no:SCREEN-VALUE 
                lv-job-no = FILL(" ",6 - LENGTH(TRIM(lv-job-no))) + TRIM(lv-job-no).

            RELEASE job-hdr.

            FIND FIRST job
                WHERE job.company EQ cocode
                AND job.job-no  EQ lv-job-no
                AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE )
                USE-INDEX job-no NO-LOCK NO-ERROR.

            IF AVAILABLE job THEN
                FIND FIRST job-hdr
                    WHERE job-hdr.company EQ job.company
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job.job-no2     EQ job.job-no2
                    NO-LOCK NO-ERROR.

            IF AVAILABLE job-hdr THEN 
            DO:
                FIND FIRST est
                    WHERE est.company EQ job-hdr.company
                    AND est.est-no  EQ job-hdr.est-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE est AND (est.est-type EQ 1 OR est.est-type EQ 5) THEN 
                    ASSIGN
                        rm-rctd.s-num:SCREEN-VALUE = "1" /*string(job-hdr.frm)      */
                        rm-rctd.b-num:SCREEN-VALUE = "1" /* string(job-hdr.blank-no)         */
                        rm-rctd.s-num:READ-ONLY    = YES
                        rm-rctd.b-num:READ-ONLY    = YES.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rmbin-help M-Win 
PROCEDURE rmbin-help :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-focus-hdl AS HANDLE NO-UNDO.
    DEFINE VARIABLE lv-rowid   AS ROWID     NO-UNDO.
    DEFINE VARIABLE save-rowid AS ROWID     NO-UNDO.
    DEFINE VARIABLE save-focus AS CHARACTER NO-UNDO.
    DEFINE VARIABLE char-hdl   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ll-error   AS LOG       NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
             
        RUN windows/l-rmibn2.w (rm-rctd.company, rm-rctd.i-no:SCREEN-VALUE , rm-rctd.loc:SCREEN-VALUE , rm-rctd.loc-bin:screen-value , rm-rctd.tag:SCREEN-VALUE , OUTPUT lv-rowid).
    
        IF rm-rctd.job-no:SCREEN-VALUE  NE "" AND
            rm-rctd.s-num:SCREEN-VALUE  EQ "?" AND
            rm-rctd.i-no:SCREEN-VALUE  NE "" THEN
        DO:
            FIND FIRST ITEM WHERE
                ITEM.company EQ cocode AND
                ITEM.i-no EQ rm-rctd.i-no:SCREEN-VALUE 
                NO-LOCK NO-ERROR.

            IF NOT AVAILABLE ITEM THEN
                LEAVE.

            RUN get-tandem-rec(INPUT ITEM.cons-uom).

            FIND FIRST tt-frm WHERE
                tt-frm.qty GT 0
                NO-ERROR.

            IF AVAILABLE tt-frm THEN
            DO:
                FOR FIRST tt-tag USE-INDEX tag-no,
                    FIRST tt-selected WHERE tt-selected.tt-rowid EQ tt-tag.tt-rowid,
                    FIRST rm-bin FIELDS(loc loc-bin tag cost ) WHERE
                    ROWID(rm-bin) EQ tt-tag.tt-rowid
                    NO-LOCK:
                    IF v-bin NE "user entered" THEN
                        ASSIGN
                            rm-rctd.loc:SCREEN-VALUE     = rm-bin.loc
                            rm-rctd.loc-bin:SCREEN-VALUE = rm-bin.loc-bin.
                    ASSIGN
                        rm-rctd.tag:SCREEN-VALUE  = rm-bin.tag
                        rm-rctd.cost:SCREEN-VALUE = STRING(rm-bin.cost).
       
                    IF tt-tag.qty LE tt-frm.qty THEN
                    DO:
                        ASSIGN
                            rm-rctd.qty:SCREEN-VALUE   = STRING(tt-tag.qty)
                            rm-rctd.s-num:SCREEN-VALUE = STRING(tt-frm.frm)
                            tt-frm.qty                 = tt-frm.qty - tt-tag.qty.

                        DELETE tt-tag.
                        DELETE tt-selected.

                        IF tt-frm.qty EQ 0 THEN
                            DELETE tt-frm.
                    END.
                    ELSE
                    DO:
                        ASSIGN
                            rm-rctd.qty:SCREEN-VALUE   = STRING(tt-frm.qty)
                            rm-rctd.s-num:SCREEN-VALUE = STRING(tt-frm.frm)
                            tt-tag.qty                 = tt-tag.qty - tt-frm.qty.

                        DELETE tt-frm.
                    END.
                END.
            END.
        END.
    
        ELSE
            FOR FIRST tt-selected WHERE tt-selected.tt-rowid EQ lv-rowid,
                FIRST rm-bin WHERE ROWID(rm-bin) EQ tt-selected.tt-rowid
                NO-LOCK:
       
                IF rm-rctd.loc:SCREEN-VALUE      NE rm-bin.loc     OR
                    rm-rctd.loc-bin:SCREEN-VALUE  NE rm-bin.loc-bin OR
                    rm-rctd.tag:SCREEN-VALUE      NE rm-bin.tag     THEN 
                DO:
                    IF v-bin NE "user entered" THEN
                        ASSIGN
                            rm-rctd.loc:SCREEN-VALUE     = rm-bin.loc
                            rm-rctd.loc-bin:SCREEN-VALUE = rm-bin.loc-bin.
                    rm-rctd.tag:SCREEN-VALUE      = rm-bin.tag.
       
                    RUN new-bin.
                END.
       
                DELETE tt-selected.
            END.
    
        FIND FIRST tt-selected NO-LOCK NO-ERROR.
        /* multiple records selected from Tag F1 lookup*/
        IF AVAILABLE tt-selected THEN APPLY "row-leave" .
        ELSE APPLY "ENTRY" TO ip-focus-hdl.
    
        v-get-tandem-rec = NO.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE s-b-help M-Win 
PROCEDURE s-b-help :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:

        IF fil_id EQ ? 
            THEN 
            RUN lookup-job-mat (?). /*Mismatched param error, OUTPUT fil_id)  YSK Task# 06060508*/

        RELEASE job-mat.


        IF fil_id NE ? THEN
            FIND job-mat WHERE RECID(job-mat) EQ fil_id NO-LOCK NO-ERROR.
        IF AVAILABLE job-mat THEN
            ASSIGN
                rm-rctd.s-num:SCREEN-VALUE = STRING(job-mat.frm) 
                rm-rctd.b-num:SCREEN-VALUE = STRING(job-mat.blank-no).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tag-method M-Win 
PROCEDURE tag-method :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-tag# AS LOG NO-UNDO.
 
  
    {rm/tag#.i}
    op-tag# = v-tag#.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tandem-rec-uom-conv M-Win 
PROCEDURE tandem-rec-uom-conv :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-i-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-pur-uom AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-qty AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ip-job-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-job-no2 AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ip-s-num AS INTEGER NO-UNDO.

    DEFINE OUTPUT PARAMETER lv-out-qty AS DECIMAL NO-UNDO.

    DEFINE VARIABLE v-dep    LIKE po-ordl.s-len NO-UNDO. 
    DEFINE VARIABLE v-job-up LIKE job-hdr.n-on NO-UNDO.
    DEFINE VARIABLE v-out    LIKE ef.n-out INIT 1 NO-UNDO.
    DEFINE VARIABLE v-bwt    LIKE po-ordl.s-len NO-UNDO.

    IF ip-i-no EQ "" THEN
        LEAVE.

    ASSIGN
        lv-out-qty = ip-qty.
   
    FIND FIRST item WHERE
        item.company EQ cocode AND
        item.i-no EQ ip-i-no
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE ITEM OR
        ip-pur-uom EQ item.cons-uom THEN
        LEAVE.

    v-dep = item.s-dep.

    FIND FIRST job WHERE
        job.company EQ cocode AND
        job.job-no  EQ ip-job-no AND
        job.job-no2 EQ ip-job-no2
        NO-LOCK NO-ERROR.
   
    IF AVAILABLE job THEN 
    DO:
        v-job-up = 0.
        FOR EACH job-hdr FIELDS(n-on)
            WHERE job-hdr.company EQ cocode
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.frm     EQ ip-s-num
            NO-LOCK:
            v-job-up = v-job-up + job-hdr.n-on.  
        END.
      
        FIND FIRST job-mat WHERE
            job-mat.company EQ cocode AND
            job-mat.job     EQ job.job AND
            job-mat.i-no    EQ ip-i-no AND
            job-mat.frm     EQ ip-s-num
            NO-LOCK NO-ERROR.

        IF AVAILABLE job-mat THEN 
        DO:
            IF lv-rmissue EQ "Net" THEN
                v-out = job-mat.n-up / v-job-up.
            ASSIGN 
                v-len = job-mat.len
                v-wid = job-mat.wid
                v-bwt = job-mat.basis-w.
        END.
    END.
   
    IF v-len EQ 0 THEN v-len = IF AVAILABLE item THEN item.s-len ELSE 0.
    IF v-wid EQ 0 THEN v-wid = IF AVAILABLE item AND item.r-wid NE 0 THEN item.r-wid ELSE IF AVAILABLE item THEN item.s-wid ELSE 0.
    IF v-bwt EQ 0 THEN v-bwt = IF AVAILABLE item THEN item.basis-w ELSE 0.
   
    RUN rm/convquom.p(ip-pur-uom, item.cons-uom, v-bwt, v-len, v-wid, v-dep,
        lv-out-qty / v-out, OUTPUT lv-out-qty).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-all M-Win 
PROCEDURE valid-all :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
   
    RUN valid-loc-bin-tag (99) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  
    /* Task No- 03151112     */
    /*RUN valid-qty NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.*/  /* Task No- 03151112     */

    RUN valid-uom NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.

    RUN valid-qty2 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.

    RUN validate-jobmat (NO) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no M-Win 
PROCEDURE valid-i-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-msg AS CHARACTER NO-UNDO.

    DO WHILE TRUE WITH FRAME {&FRAME-NAME}:
        v-msg = "".

        IF v-msg EQ "" THEN
            IF rm-rctd.i-no:SCREEN-VALUE  EQ "" THEN
                v-msg = TRIM(rm-rctd.i-no:LABEL ) +
                    " may not be spaces".

        IF v-msg EQ "" THEN 
        DO:
            IF /*(rm-rctd.job-no:SCREEN-VALUE  EQ "" AND NOT CAN-FIND(FIRST item 
           WHERE item.company EQ cocode
             AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE 
             AND item.i-code  EQ "R")) OR
           (rm-rctd.i-no:SCREEN-VALUE  NE "" AND  */
                NOT CAN-FIND(FIRST item 
                WHERE item.company EQ cocode
                AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE )/*)*/ THEN 
            DO:
                IF lAllowRmAdd EQ YES THEN 
                DO:
                    MESSAGE "Item is not on file. Do you want to add it? "
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
                    IF ll-ans THEN 
                    DO:
                        RUN create-item.
                        NEXT.
                    END.
                    ELSE 
                        v-msg = "Invalid entry, try help".
                END.
                ELSE 
                DO:
                    v-msg = "Invalid entry, try help".
                END.
            END.
        END.

        IF v-msg EQ "" THEN
            IF rm-rctd.job-no:SCREEN-VALUE  EQ ""    AND
                INT(rm-rctd.po-no:SCREEN-VALUE ) EQ 0 AND
                AVAILABLE ITEM AND item.mat-type NE "P"                                          THEN
                v-msg = "If PO# and Job# are blank then RM Type must be 'P'aper".

        IF v-msg EQ "" THEN
            IF INT(rm-rctd.po-no:SCREEN-VALUE ) NE 0 AND
                lv-i-no NE "" AND lv-line NE 0 AND
                NOT CAN-FIND(FIRST po-ordl
                WHERE po-ordl.company EQ rm-rctd.company
                AND po-ordl.po-no   EQ INT(rm-rctd.po-no:SCREEN-VALUE )
                AND po-ordl.i-no    EQ lv-i-no
                AND po-ordl.line    EQ lv-line
                AND po-ordl.s-wid   LE (IF AVAILABLE ITEM AND item.r-wid NE 0 THEN item.r-wid
            ELSE item.s-wid)) THEN
                v-msg = "RM width must be greater than PO RM you are issuing to...".

        IF v-msg NE "" THEN 
        DO:
            MESSAGE TRIM(v-msg) + "..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO rm-rctd.i-no .
            RETURN ERROR.
        END.

        LEAVE.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no M-Win 
PROCEDURE valid-job-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-job-no LIKE rm-rctd.job-no NO-UNDO.
    DEFINE VARIABLE lv-po-no  LIKE po-ord.po-no NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF rm-rctd.job-no:SCREEN-VALUE  NE "" THEN 
        DO:
            ASSIGN
                lv-job-no                   = rm-rctd.job-no:SCREEN-VALUE 
                rm-rctd.job-no:SCREEN-VALUE = FILL(" ",6 - LENGTH(TRIM(lv-job-no))) + TRIM(lv-job-no)
                lv-po-no                    = INT(rm-rctd.po-no:SCREEN-VALUE ).

            IF NOT CAN-FIND(FIRST job
                WHERE job.company EQ cocode
                AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE 
                USE-INDEX job-no) OR lv-po-no NE 0
                THEN 
            DO:
                IF lv-po-no NE 0 THEN
                    MESSAGE "You may not enter both " +
                        TRIM(rm-rctd.job-no:LABEL ) + " and " +
                        TRIM(rm-rctd.po-no:LABEL ) + "..."
                        VIEW-AS ALERT-BOX ERROR.
                ELSE
                    MESSAGE "Invalid " +
                        TRIM(rm-rctd.job-no:LABEL ) +
                        ", try help..."
                        VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO rm-rctd.job-no .
                RETURN ERROR.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-job-no2 M-Win 
PROCEDURE valid-job-no2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ll     AS LOG NO-UNDO.
    DEFINE VARIABLE lv-ans AS LOG NO-UNDO.
        
    DO WITH FRAME {&FRAME-NAME}:
        IF rm-rctd.job-no:SCREEN-VALUE  NE "" THEN 
        DO:
            FIND FIRST job
                WHERE job.company EQ cocode
                AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE 
                AND job.job-no2 EQ int(rm-rctd.job-no2:SCREEN-VALUE)
                USE-INDEX job-no NO-LOCK NO-ERROR.
            IF NOT AVAILABLE job THEN 
            DO:
                MESSAGE "Invalid " +
                    TRIM(rm-rctd.job-no:LABEL ) +
                    ", try help..."
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO rm-rctd.job-no .
                RETURN ERROR.
            END.

            ll = NO.
            /* If not asking to open, and job is closed, just return */
            IF jobreopn-log = NO  THEN 
                RUN jc/chk-stat.p(RECID(job), 1, YES /* with message */, OUTPUT ll).
            ELSE 
            DO:
                /* If job is closed and we are asking, run the open procedure */
                /* and re-check until it's good */
                job-open-check:
                DO WHILE ll EQ NO:
                    RUN jc/chk-stat.p(RECID(job), 1, NO /* no message */, OUTPUT ll).
                    IF NOT ll THEN 
                    DO:
                        lv-ans = NO.
                        IF jobreopn-log EQ YES  THEN
                            MESSAGE 
                                "Job is CLOSED, would you like to reopen?"
                                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL UPDATE lv-ans.
                        IF lv-ans THEN 
                        DO:            
                            RUN jc/jc-reopn.p (ROWID(job)).
                            RUN jc/chk-stat.p(RECID(job), 1, NO /* no message */, OUTPUT ll).
                        END.
                        ELSE /* They decided not to open it, so exit loop */
                            LEAVE job-open-check.
                    END.
                    ELSE
                        LEAVE job-open-check.
                END.
            END.

            IF NOT ll THEN 
            DO:    
                APPLY "entry" TO rm-rctd.job-no .
                RETURN ERROR.
            END.

        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc M-Win 
PROCEDURE valid-loc :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-focus AS WIDGET-HANDLE NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST loc
            WHERE loc.company EQ cocode
            AND loc.loc     EQ ip-focus:SCREEN-VALUE)
            THEN 
        DO:
            MESSAGE "Invalid Warehouse, try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO ip-focus.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin M-Win 
PROCEDURE valid-loc-bin :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-focus AS WIDGET-HANDLE NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF NOT CAN-FIND(FIRST rm-bin
            WHERE rm-bin.company EQ cocode
            AND rm-bin.i-no    EQ ""
            AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE 
            AND rm-bin.loc-bin EQ ip-focus:SCREEN-VALUE)
            THEN 
        DO:
            MESSAGE "Invalid Bin, try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO ip-focus.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin-tag M-Win 
PROCEDURE valid-loc-bin-tag :
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
            RELEASE rm-rdtlh.

            IF DEC(rm-rctd.qty:SCREEN-VALUE ) LE 0 OR ip-int NE 99 THEN
                IF rm-rctd.tag:SCREEN-VALUE  NE "" AND ip-int GE 3 THEN
                    FOR EACH rm-rdtlh
                        WHERE rm-rdtlh.company  EQ cocode
                        AND rm-rdtlh.loc      EQ rm-rctd.loc:SCREEN-VALUE 
                        AND rm-rdtlh.tag      EQ rm-rctd.tag:SCREEN-VALUE 
                        AND rm-rdtlh.loc-bin  EQ rm-rctd.loc-bin:SCREEN-VALUE 
                        USE-INDEX tag NO-LOCK,
                        FIRST rm-rcpth
                        WHERE rm-rcpth.r-no      EQ rm-rdtlh.r-no
                        AND rm-rcpth.rita-code EQ rm-rdtlh.rita-code
                        AND rm-rcpth.i-no      EQ rm-rctd.i-no:SCREEN-VALUE 
                        USE-INDEX r-no NO-LOCK:
                        LEAVE.
                    END.
                ELSE
                    FOR EACH rm-rcpth
                        WHERE rm-rcpth.company EQ cocode
                        AND rm-rcpth.i-no    EQ rm-rctd.i-no:SCREEN-VALUE 
                        USE-INDEX i-no NO-LOCK,
                        EACH rm-rdtlh
                        WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
                        AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
                        AND (rm-rdtlh.loc      EQ rm-rctd.loc:SCREEN-VALUE      OR ip-int LT 1)
                        AND (rm-rdtlh.loc-bin  EQ rm-rctd.loc-bin:SCREEN-VALUE  OR ip-int LT 2)
                        NO-LOCK:
                        LEAVE.
                    END.
        /* Task No- 03151112     */
        /*IF NOT AVAIL rm-rdtlh THEN DO:
          MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX.
          IF ip-int EQ 3 THEN
            APPLY "entry" TO rm-rctd.tag .
          ELSE
          IF ip-int EQ 2 THEN
            APPLY "entry" TO rm-rctd.loc-bin .
          ELSE
            APPLY "entry" TO rm-rctd.loc .
          RETURN ERROR.
        END.*/  /* Task No- 03151112     */
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no M-Win 
PROCEDURE valid-po-no :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-msg AS CHARACTER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        v-msg = "".

        IF INT(rm-rctd.po-no:SCREEN-VALUE ) NE 0 THEN 
        DO:
            IF v-msg EQ "" THEN
                IF rm-rctd.job-no:SCREEN-VALUE  NE "" THEN
                    v-msg = "You may not enter both " +
                        TRIM(rm-rctd.job-no:LABEL ) + " and " +
                        TRIM(rm-rctd.po-no:LABEL ).

            FIND FIRST po-ordl
                WHERE po-ordl.company   EQ rm-rctd.company
                AND po-ordl.po-no     EQ INT(rm-rctd.po-no:SCREEN-VALUE )
                AND po-ordl.item-type EQ YES
                NO-LOCK NO-ERROR.

            IF v-msg EQ "" THEN
                IF NOT AVAILABLE po-ordl THEN v-msg = "is invalid, try help".

            IF v-msg EQ "" THEN
                IF NOT CAN-FIND(FIRST po-ord WHERE
                    po-ord.company EQ po-ordl.company AND
                    po-ord.po-no   EQ po-ordl.po-no AND
                    po-ord.type EQ "S") THEN
                    v-msg = "must be 'S'heets from Roll type PO".
        END.

        IF v-msg NE "" THEN 
        DO:
            MESSAGE TRIM(rm-rctd.po-no:LABEL ) +
                " " + v-msg + "..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO rm-rctd.po-no .
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty M-Win 
PROCEDURE valid-qty :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF DEC(rm-rctd.qty:SCREEN-VALUE ) EQ 0 THEN 
        DO:
            MESSAGE "Issued qty may not be 0..." VIEW-AS ALERT-BOX.
            APPLY "entry" TO rm-rctd.qty .
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty2 M-Win 
PROCEDURE valid-qty2 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-rm-rctd FOR rm-rctd.
    DEFINE VARIABLE dTotalI AS DECIMAL NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST rm-bin NO-LOCK
            WHERE rm-bin.company EQ cocode
            AND rm-bin.i-no    EQ rm-rctd.i-no:SCREEN-VALUE 
            AND rm-bin.loc     EQ rm-rctd.loc:SCREEN-VALUE 
            AND rm-bin.loc-bin EQ rm-rctd.loc-bin:SCREEN-VALUE 
            AND rm-bin.tag     EQ rm-rctd.tag:SCREEN-VALUE 
            NO-ERROR.
        IF NOT AVAILABLE rm-bin AND integer(rm-rctd.tag:SCREEN-VALUE ) GE 0  THEN 
        DO:
            /* ticket 22650 - Tag does not have to exist for a negative issue */
            MESSAGE "Tag # does not exist in the Bin File..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO rm-rctd.tag .
            RETURN ERROR.
        END.

        IF NOT ll-is-copy-record THEN
            FOR EACH bf-rm-rctd 
                WHERE bf-rm-rctd.company EQ cocode
                AND bf-rm-rctd.rita-code EQ "I"                    
                AND bf-rm-rctd.i-no      EQ rm-bin.i-no
                AND bf-rm-rctd.loc       EQ rm-bin.loc
                AND bf-rm-rctd.loc-bin   EQ rm-bin.loc-bin     
                AND bf-rm-rctd.tag       EQ rm-bin.tag  
                AND NOT (AVAIL(rm-rctd) AND ROWID(rm-rctd) EQ ROWID(bf-rm-rctd))
                NO-LOCK USE-INDEX rita-code:
                dTotalI = dTotalI + bf-rm-rctd.qty.    
            END.
        ELSE 
        DO:
            FOR EACH bf-rm-rctd 
                WHERE bf-rm-rctd.company EQ cocode
                AND bf-rm-rctd.rita-code EQ "I"                    
                AND bf-rm-rctd.i-no      EQ rm-bin.i-no
                AND bf-rm-rctd.loc       EQ rm-bin.loc
                AND bf-rm-rctd.loc-bin   EQ rm-bin.loc-bin     
                AND bf-rm-rctd.tag       EQ rm-bin.tag  
                NO-LOCK USE-INDEX rita-code:
                dTotalI = dTotalI + bf-rm-rctd.qty.    
            END.

        END.

    
    
        IF DEC(rm-rctd.qty:SCREEN-VALUE ) > 0 AND
            rm-bin.qty LT DEC(rm-rctd.qty:SCREEN-VALUE ) THEN 
        DO:
            MESSAGE "Issue Quantity exceeds Quantity on Hand for this Warehouse/Bin/Tag Location..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO rm-rctd.loc-bin .
            RETURN ERROR.
        END.
        ELSE 
        DO:
            FIND ITEM WHERE ITEM.company EQ rm-bin.company
                AND ITEM.i-no EQ rm-bin.i-no
                NO-LOCK NO-ERROR.

            IF DEC(rm-rctd.qty:SCREEN-VALUE ) > 0 AND
                rm-bin.qty LT (DEC(rm-rctd.qty:SCREEN-VALUE ) + dTotalI) 
                AND AVAIL(item) AND item.stocked THEN 
            DO:
                MESSAGE "Issue Quantity exceeds Quantity on Hand + Unposted Issues for this Warehouse/Bin/Tag Location..."          
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO rm-rctd.loc-bin .
                RETURN ERROR.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag M-Win 
PROCEDURE valid-tag :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
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
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom M-Win 
PROCEDURE valid-uom :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        rm-rctd.pur-uom:SCREEN-VALUE  =
            CAPS(rm-rctd.pur-uom:SCREEN-VALUE ).

        FIND FIRST ITEM
            WHERE item.company EQ cocode
            AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE 
            NO-LOCK NO-ERROR.

        IF AVAILABLE item THEN RUN sys/ref/uom-rm.p (INPUT item.mat-type, OUTPUT lv-uom-list).

        lv-uom-list = lv-uom-list + ",DIA".

        IF INDEX(lv-uom-list,rm-rctd.pur-uom:SCREEN-VALUE ) LE 0 THEN 
        DO:
            MESSAGE TRIM(rm-rctd.pur-uom:LABEL ) +
                " must be " + TRIM(lv-uom-list) + "..."
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END. 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-jobmat M-Win 
PROCEDURE validate-jobmat :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-for-item-only AS LOG NO-UNDO.

    DEFINE VARIABLE v-job-up AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-frm    AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-blk    AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-out    AS INTEGER NO-UNDO.
    DEFINE VARIABLE choice   AS LOG     NO-UNDO.
    DEFINE VARIABLE v-cost   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-bwt    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-len    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-wid    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-dep    AS DECIMAL NO-UNDO.

    DEFINE BUFFER xjob-mat FOR job-mat.

    /* gdm - 08070907 */
    DEFINE VARIABLE v-reccnt AS INTEGER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:    
        IF rm-rctd.s-num:SCREEN-VALUE  EQ "?"
            THEN ASSIGN ip-for-item-only = YES.
    
        /* gdm - 08070907 end */
    

        FIND FIRST item 
            WHERE item.company EQ cocode
            AND item.i-no    EQ rm-rctd.i-no:SCREEN-VALUE 
            NO-LOCK NO-ERROR.
        /*
        IF (rm-rctd.b-num:MODIFIED   OR int(rm-rctd.b-num:SCREEN-VALUE) = 0 )
           OR rm-rctd.s-num:MODIFIED 
           OR rm-rctd.i-no:MODIFIED
        THEN DO:
        */
        RELEASE job.
        RELEASE job-mat.

        IF rm-rctd.job-no:SCREEN-VALUE  NE "" THEN
            FIND FIRST job
                WHERE job.company EQ cocode
                AND job.job-no  EQ rm-rctd.job-no:SCREEN-VALUE 
                AND job.job-no2 EQ INT(rm-rctd.job-no2:SCREEN-VALUE )
                NO-LOCK NO-ERROR.
        IF AVAILABLE job THEN
            FIND FIRST job-mat WHERE job-mat.company = job.company
                AND job-mat.job = job.job
                AND job-mat.job-no = job.job-no
                AND job-mat.job-no2 = job.job-no2
                AND job-mat.i-no = rm-rctd.i-no:SCREEN-VALUE
                AND (ip-for-item-only OR
                (job-mat.frm = INT(rm-rctd.s-num:SCREEN-VALUE) AND
                job-mat.blank-no = INT(rm-rctd.b-num:SCREEN-VALUE)))
                USE-INDEX seq-idx NO-LOCK NO-ERROR.    
        IF NOT AVAILABLE job-mat AND rm-rctd.job-no:SCREEN-VALUE  NE "" THEN 
        DO:
        
            /* gdm - */
            ASSIGN 
                lv-job-no              = FILL(" ", 6 - LENGTH(TRIM(rm-rctd.job-no:SCREEN-VALUE ))) +
                  TRIM(rm-rctd.job-no:SCREEN-VALUE)
                char-val               = ""
                v-number-rows-selected = 0
                look-recid             = 0.
                  
            IF rm-rctd.job-no:SCREEN-VALUE NE "" OR 
                rm-rctd.i-no:SCREEN-VALUE NE "" THEN
            DO:
         

                EMPTY TEMP-TABLE w-rm-rctd.

                RUN windows/l-jobmt3.w (cocode, lv-job-no,
                    INT(rm-rctd.job-no2:SCREEN-VALUE),
                    rm-rctd.i-no:SCREEN-VALUE,
                    OUTPUT char-val,OUTPUT look-recid,
                    OUTPUT v-number-rows-selected).
         
            END.

            ASSIGN 
                char-val = IF char-val BEGINS "," THEN TRIM(SUBSTR(char-val,2))
                                               ELSE TRIM(char-val).                                                    

            IF v-number-rows-selected > 1 AND AVAILABLE w-rm-rctd
                THEN RUN check-multi-line.
            ELSE 
                IF v-number-rows-selected > 1 THEN
                    ASSIGN rm-rctd.s-num:SCREEN-VALUE = "?"
                        rm-rctd.b-num:SCREEN-VALUE = "0".
                ELSE 
                DO:         
                    ASSIGN 
                        fil_id = look-recid.
                    RUN s-b-help. 
                END.

            /* gdm - */
    
            MESSAGE "Update item on Job file? " VIEW-AS ALERT-BOX QUESTION
                BUTTON YES-NO UPDATE ll-ans AS LOG.
            IF ll-ans THEN 
            DO:

                FIND FIRST job WHERE job.company = cocode
                    AND job.job-no =  rm-rctd.job-no:SCREEN-VALUE
                    AND job.job-no2 = int(rm-rctd.job-no2:SCREEN-VALUE)
                    NO-LOCK NO-ERROR.
                /*             v-job-up = 0.                                    */
                /*             for each job-hdr fields(n-on)                                */
                /*                 where job-hdr.company eq cocode              */
                /*                   and job-hdr.job     eq job.job             */
                /*                   and job-hdr.job-no  eq job.job-no          */
                /*                   and job-hdr.job-no2 eq job.job-no2         */
                /*                   and job-hdr.frm     eq input rm-rctd.s-num */
                /*                 no-lock:                                     */
                /*                 v-job-up = v-job-up + job-hdr.n-on.          */
                /*             end.                                             */            

                DO v-reccnt = 1 TO NUM-ENTRIES(char-val):

                    ASSIGN 
                        fil_id = INT(ENTRY(v-reccnt,char-val)).

                    IF fil_id = ? THEN RUN lookup-job-mat (ip-for-item-only).
          
                    FIND job-mat WHERE RECID(job-mat) EQ fil_id NO-ERROR.

                    IF AVAILABLE job-mat THEN 
                    DO:
                        v-job-up = 0.
                        FOR EACH job-hdr FIELDS(n-on)
                            WHERE job-hdr.company EQ cocode
                            AND job-hdr.job     EQ job.job
                            AND job-hdr.job-no  EQ job.job-no
                            AND job-hdr.job-no2 EQ job.job-no2
                            AND job-hdr.frm     EQ job-mat.frm NO-LOCK:
                            v-job-up = v-job-up + job-hdr.n-on.  
                        END.

                        IF INDEX("1234BPR",item.mat-type) GT 0 THEN 
                        DO ON ENDKEY UNDO, RETRY:
                            ASSIGN
                                v-frm = job-mat.frm
                                v-blk = job-mat.blank-no
                                v-out = job-mat.n-up / v-job-up.
                            RUN rm/g-iss2.w ( v-frm, v-blk , INPUT-OUTPUT v-out ). 
                        /*
                          display v-frm v-blk with frame s-b.
                          update v-out with frame s-b.
                        */
                        END.  

                        IF item.i-code EQ "R" THEN 
                        DO:
                            IF (item.r-wid NE 0 AND  item.r-wid LT job-mat.wid) OR
                                (item.r-wid EQ 0 AND (item.s-wid LT job-mat.wid OR
                                item.s-len LT job-mat.len))
                                THEN 
                            DO ON ENDKEY UNDO, RETRY:
                                choice = NO.

                                IF ITEM.r-wid <> 0 THEN
                                    RUN rm/g-iss21.w (job-mat.len, job-mat.len, item.r-wid,job-mat.wid, job-mat.frm,
                                        OUTPUT choice)  .
                                ELSE RUN rm/g-iss21.w (item.s-len, job-mat.len, item.s-wid,job-mat.wid, job-mat.frm,
                                        OUTPUT choice)  .
                                /* display item.s-len
                                         job-mat.len when item.r-wid ne 0 @ item.s-len
                                       job-mat.len
                                       item.s-wid
                                         item.r-wid  when item.r-wid ne 0 @ item.s-wid
                                       job-mat.wid
                                       job-mat.frm
                                   with frame tsmall.
                                update choice with frame tsmall. 
                                */
                
                                IF NOT choice THEN 
                                DO: 
                                    RELEASE job-mat.
                                    APPLY "entry" TO rm-rctd.i-no.
                                    RETURN ERROR.
                                END.
                            END.
                        END.
                    END. /* avail job-mat */
         
                    FIND FIRST xitem WHERE xitem.company EQ cocode
                        AND xitem.i-no    EQ job-mat.rm-i-no
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE xitem THEN RELEASE job-mat.

                    IF AVAILABLE job-mat THEN 
                    DO:
                        CREATE xjob-mat.
                        BUFFER-COPY job-mat TO xjob-mat.
                
                        FIND job-mat WHERE RECID(job-mat) EQ recid(xjob-mat).
          
                        IF job-mat.sc-uom EQ job-mat.qty-uom THEN
                            v-cost = job-mat.std-cost.
                        ELSE
                            RUN sys/ref/convcuom.p(job-mat.sc-uom,
                                job-mat.qty-uom,
                                job-mat.basis-w,
                                job-mat.len,
                                job-mat.wid,
                                item.s-dep,
                                job-mat.std-cost,
                                OUTPUT v-cost).
                    
                        ASSIGN
                            v-cost                     = v-cost * job-mat.qty
                            rm-rctd.s-num:SCREEN-VALUE = IF v-number-rows-selected EQ 1 
                                                  THEN STRING(job-mat.frm)
                                                  ELSE rm-rctd.s-num:SCREEN-VALUE
                            rm-rctd.b-num:SCREEN-VALUE = STRING(job-mat.blank-no)
                            job-mat.j-no               = 1                 
                            job-mat.rm-i-no            = item.i-no
                            job-mat.i-no               = item.i-no
                            job-mat.sc-uom             = item.cons-uom                 
                            job-mat.wid                = IF item.r-wid NE 0 THEN
                                     item.r-wid ELSE item.s-wid
                            job-mat.len                = IF item.r-wid NE 0 THEN
                                     job-mat.len ELSE item.s-len
                            job-mat.basis-w            = item.basis-w
                            job-mat.qty                = job-mat.qty * IF job-mat.n-up EQ 0 THEN 1 ELSE job-mat.n-up
                            job-mat.n-up               = v-job-up * v-out                 
                            job-mat.qty                = job-mat.qty / IF job-mat.n-up EQ 0 THEN 1 ELSE job-mat.n-up.
                     
                        {sys/inc/roundup.i job-mat.qty}
                
                        v-cost = v-cost / job-mat.qty.
                
                        IF job-mat.qty-uom EQ job-mat.sc-uom THEN
                            job-mat.std-cost = v-cost.
                        ELSE  
                            RUN sys/ref/convcuom.p(job-mat.qty-uom,
                                job-mat.sc-uom,
                                job-mat.basis-w,
                                job-mat.len,
                                job-mat.wid,
                                item.s-dep,
                                v-cost,
                                OUTPUT job-mat.std-cost).                                                                         

                        ASSIGN
                            v-bwt = job-mat.basis-w
                            v-len = job-mat.len
                            v-wid = job-mat.wid
                            v-dep = item.s-dep.
                    END. /* avail job-mat */
                END. /* DO counter for recids */
            END.  /* ll-ans = yes */
            ELSE 
            DO: 
                APPLY "entry" TO rm-rctd.i-no.
                RETURN ERROR.  /* not update item */
            END.
        END. /* not avail job-mat */

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


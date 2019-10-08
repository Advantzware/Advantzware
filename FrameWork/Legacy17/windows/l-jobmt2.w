&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: windows\l-jobmt1.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER ip-company   LIKE itemfg.company NO-UNDO.
DEF INPUT PARAMETER ip-job-no    LIKE job-mat.job-no NO-UNDO.
DEF INPUT PARAMETER ip-job-no2   LIKE job-mat.job-no2 NO-UNDO.
DEF INPUT PARAMETER ip-cur-val   AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER op-char-val AS CHAR NO-UNDO. /* string i-code + i-name */
DEF OUTPUT PARAMETER op-rec-val  AS RECID NO-UNDO.
DEF OUTPUT PARAMETER op-number-rows-selected AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR lv-type-dscr AS CHAR NO-UNDO.
DEF VAR lv-mat-type LIKE item.mat-type NO-UNDO.
DEF VAR lv-rm-i-no LIKE item.i-no NO-UNDO.

&scoped-define SORTBY-1 BY job-mat.rm-i-no
&scoped-define fld-name-1  job-mat.rm-i-no

&scoped-define IAMWHAT LOOKUP

{windows/l-jobmt1.i}

{sa/sa-sls01.i}

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
&Scoped-define INTERNAL-TABLES job-mat item report

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 job-mat.job-no job-mat.job-no2 ~
job-mat.rm-i-no item.i-name job-mat.frm job-mat.blank-no item.i-code 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH job-mat WHERE ~{&KEY-PHRASE} ~
      AND job-mat.company = job.company ~
and job-mat.job = job.job ~
and job-mat.job-no = job.job-no ~
and job-mat.job-no2 = job.job-no2 NO-LOCK, ~
      FIRST item WHERE item.company = job-mat.company ~
  AND item.i-no = job-mat.rm-i-no NO-LOCK, ~
      FIRST report WHERE report.rec_key = string(rowid(ASI.job-mat)) ~
      AND report.term-id eq v-term + USERID("nosweat") NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH job-mat WHERE ~{&KEY-PHRASE} ~
      AND job-mat.company = job.company ~
and job-mat.job = job.job ~
and job-mat.job-no = job.job-no ~
and job-mat.job-no2 = job.job-no2 NO-LOCK, ~
      FIRST item WHERE item.company = job-mat.company ~
  AND item.i-no = job-mat.rm-i-no NO-LOCK, ~
      FIRST report WHERE report.rec_key = string(rowid(ASI.job-mat)) ~
      AND report.term-id eq v-term + USERID("nosweat") NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 job-mat item report
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 job-mat
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 item
&Scoped-define THIRD-TABLE-IN-QUERY-BROWSE-1 report


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 RECT-1 rd-sort Btn_Select bt-clear ~
lv-search bt-ok bt-cancel Btn_Deselect 
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

DEFINE BUTTON Btn_Deselect 
     LABEL "Unselect All" 
     SIZE 13 BY 1.14.

DEFINE BUTTON Btn_Select 
     LABEL "Select All" 
     SIZE 13 BY 1.14.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE rd-sort AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Item#", 1
     SIZE 74 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      job-mat, 
      item, 
      report SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 Dialog-Frame _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      job-mat.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U WIDTH 10
      job-mat.job-no2 COLUMN-LABEL "" FORMAT ">9":U WIDTH 3
      job-mat.rm-i-no COLUMN-LABEL "RM Item#" FORMAT "x(10)":U
            WIDTH 20
      item.i-name FORMAT "x(30)":U
      job-mat.frm COLUMN-LABEL "Sheet#" FORMAT ">>9":U
      job-mat.blank-no COLUMN-LABEL "Blank#" FORMAT ">9":U
      item.i-code COLUMN-LABEL "Real/Estimated" FORMAT "X":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 106 BY 11.19
         BGCOLOR 8 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-1 AT ROW 1 COL 1
     rd-sort AT ROW 12.67 COL 14 NO-LABEL
     Btn_Select AT ROW 12.67 COL 93
     bt-clear AT ROW 14.1 COL 2
     lv-search AT ROW 14.1 COL 21 COLON-ALIGNED
     bt-ok AT ROW 14.1 COL 69
     bt-cancel AT ROW 14.1 COL 81
     Btn_Deselect AT ROW 14.1 COL 93
     "Sort By:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.91 COL 4
     RECT-1 AT ROW 12.43 COL 1
     SPACE(14.99) SKIP(1.46)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Job Item Information".


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
/* BROWSE-TAB BROWSE-1 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "ASI.job-mat,ASI.item WHERE ASI.job-mat  ...,ASI.report WHERE ASI.job-mat ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST, FIRST"
     _Where[1]         = "ASI.job-mat.company = job.company
and job-mat.job = job.job
and job-mat.job-no = job.job-no
and job-mat.job-no2 = job.job-no2"
     _JoinCode[2]      = "ASI.item.company = ASI.job-mat.company
  AND ASI.item.i-no = ASI.job-mat.rm-i-no"
     _JoinCode[3]      = "ASI.report.rec_key = string(rowid(ASI.job-mat))"
     _Where[3]         = "ASI.report.term-id eq v-term + USERID(""nosweat"")"
     _FldNameList[1]   > ASI.job-mat.job-no
"job-mat.job-no" "Job#" ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.job-mat.job-no2
"job-mat.job-no2" "" ? "integer" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.job-mat.rm-i-no
"job-mat.rm-i-no" "RM Item#" ? "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = ASI.item.i-name
     _FldNameList[5]   > ASI.job-mat.frm
"job-mat.frm" "Sheet#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.job-mat.blank-no
"job-mat.blank-no" "Blank#" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.item.i-code
"item.i-code" "Real/Estimated" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Job Item Information */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 Dialog-Frame
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME Dialog-Frame
DO:              
  apply "choose" to bt-ok.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-clear Dialog-Frame
ON CHOOSE OF bt-clear IN FRAME Dialog-Frame /* Clear Find */
DO:
    assign lv-search:screen-value = "".
           lv-search = "".
    case rd-sort:
        {srtord.i 1}
        {srtord.i 2}
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR li AS INT NO-UNDO.
  DEF VAR v-corr AS LOG NO-UNDO.
  DEF VAR v-wid AS DEC NO-UNDO.
  DEF VAR v-len AS DEC NO-UNDO.
  DEF VAR v-valid AS LOG INIT YES NO-UNDO.
  DEF VAR v-lscore AS DEC EXTENT 30 NO-UNDO.
  DEF VAR v-wscore AS DEC EXTENT 30 NO-UNDO.
  DEF VAR i AS INT NO-UNDO.

  IF {&browse-name}:NUM-SELECTED-ROWS GT 1 THEN
     DO:
        FIND FIRST job-hdr WHERE
             job-hdr.company EQ ip-company AND
             job-hdr.job-no EQ ip-job-no AND
             job-hdr.job-no2 EQ ip-job-no2
             NO-LOCK NO-ERROR.
       
        IF AVAIL job-hdr THEN
        DO:
            FIND FIRST est WHERE
                 est.company EQ job-hdr.company AND
                 est.est-no EQ job-hdr.est-no
                 NO-LOCK NO-ERROR.
       
            IF AVAIL est AND est.est-type GT 4 THEN
               v-corr = YES.
        END.

        DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
           {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.

           IF AVAIL job-mat THEN
           DO:
              IF li = 1 THEN
              DO:
                 ASSIGN
                    v-wid = job-mat.wid
                    v-len = job-mat.len.

                 IF v-corr THEN
                 DO:
                    FOR EACH eb WHERE
                        eb.company EQ job-mat.company AND
                        eb.est-no EQ est.est-no AND
                        eb.form-no EQ job-mat.frm
                        NO-LOCK,
                        FIRST ef WHERE
                              ef.company EQ job-mat.company AND
                              ef.est-no EQ est.est-no AND
                              ef.form-no EQ eb.form-no AND
                              ef.board = job-mat.rm-i-no
                              USE-INDEX est-qty
                              NO-LOCK:
                   
                        LEAVE.
                    END.
                   
                    IF AVAIL eb THEN
                    DO:
                       DO i = 1 TO 30:
                          v-wscore[i] = eb.k-wid-array2[i].
                       END.
                   
                       DO i = 1 TO 30:
                          v-lscore[i] = eb.k-len-array2[i].
                       END.
                   
                       RELEASE eb.
                    END.
                 END.
              END.

              ELSE
              DO:
                 IF job-mat.wid NE v-wid OR
                    job-mat.len NE v-len THEN
                 DO:
                    v-valid = NO.
                    LEAVE.
                 END.

                 IF v-valid AND v-corr THEN
                 DO:
                    FOR EACH eb WHERE
                        eb.company EQ job-mat.company AND
                        eb.est-no EQ est.est-no AND
                        eb.form-no EQ job-mat.frm
                        NO-LOCK,
                        FIRST ef WHERE
                              ef.company EQ job-mat.company AND
                              ef.est-no EQ est.est-no AND
                              ef.form-no EQ eb.form-no AND
                              ef.board = job-mat.rm-i-no
                              USE-INDEX est-qty
                              NO-LOCK:
                        LEAVE.
                    END.

                    IF AVAIL eb THEN
                    DO:
                       DO i = 1 TO 30:

                          IF v-wscore[i] NE eb.k-wid-array2[i] THEN
                          DO:
                              v-valid = NO.
                              RELEASE eb.
                              LEAVE.
                          END.
                       END.
                   
                       IF v-valid THEN
                          DO i = 1 TO 30:

                             IF v-lscore[i] NE eb.k-len-array2[i] THEN
                             DO:
                                v-valid = NO.
                                RELEASE eb.
                                LEAVE.
                             END.
                       END.
                    END.
                 END.
              END.
           END.
        END.

        IF NOT v-valid THEN
        DO:
            MESSAGE "Only Matching Sheet Sizes Can Be Combined."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "SELECT":U TO Btn_Deselect IN FRAME {&FRAME-NAME}.
            LEAVE.
        END.
  END.

  DO li = 1 TO {&browse-name}:NUM-SELECTED-ROWS:
    {&browse-name}:FETCH-SELECTED-ROW (li) NO-ERROR.

    IF AVAIL job-mat THEN DO:

      IF li EQ 1 THEN
        ASSIGN
         op-char-val = job-mat.rm-i-no:SCREEN-VALUE IN BROWSE {&browse-name}
         op-rec-val  = RECID(job-mat)
         op-number-rows-selected = {&browse-name}:NUM-SELECTED-ROWS.

      IF lv-mat-type NE "" THEN DO:
        CREATE w-po-ordl.
        ASSIGN
         w-po-ordl.i-no          = lv-rm-i-no
         w-po-ordl.job-no        = job-mat.job-no
         w-po-ordl.job-no2       = job-mat.job-no2
         w-po-ordl.s-num         = job-mat.frm
         w-po-ordl.b-num         = job-mat.blank-no
         w-po-ordl.job-mat-rowid = ROWID(job-mat).
      END.
    END.
  END.

  APPLY "window-close" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deselect Dialog-Frame
ON CHOOSE OF Btn_Deselect IN FRAME Dialog-Frame /* Unselect All */
DO:
  {&browse-name}:DESELECT-ROWS ().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Select Dialog-Frame
ON CHOOSE OF Btn_Select IN FRAME Dialog-Frame /* Select All */
DO:
  {&browse-name}:SELECT-ALL ().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search Dialog-Frame
ON LEAVE OF lv-search IN FRAME Dialog-Frame /* Search */
or return of lv-search
DO:
    assign rd-sort 
           lv-search.
    &scoped-define IAMWHAT Search
    &scoped-define where-statement begins lv-search
    case rd-sort:
        {srtord2.i 1}
     
    end.      
    
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
    case rd-sort:
        {srtord2.i 1}
    
    end.    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
DEF VAR ll-job-mat AS LOG NO-UNDO.
DEF VAR ll-rec-created AS LOG NO-UNDO.
DEF VAR lv-frm LIKE job-mat.frm NO-UNDO.

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  lv-rm-i-no = ip-cur-val.

  /* When running out of jc/b-jobmat.w get params from passed rec_key of job-mat */
  IF ip-job-no EQ "" AND ip-job-no2 EQ 0 AND ip-cur-val EQ "" THEN DO:
    FIND job-mat WHERE ROWID(job-mat) EQ TO-ROWID(ip-company) NO-LOCK NO-ERROR.
    IF AVAIL job-mat THEN DO:
      ASSIGN
       ip-company = job-mat.company
       ip-job-no  = job-mat.job-no
       ip-job-no2 = job-mat.job-no2
       lv-rm-i-no = job-mat.rm-i-no
       ll-job-mat = YES
       lv-frm     = job-mat.frm.

      FOR EACH w-po-ordl:
        DELETE w-po-ordl.
      END.
    END.

    RELEASE job-mat.
  END.

  IF NOT CAN-FIND(FIRST w-po-ordl) THEN DO:
    IF lv-rm-i-no NE "" THEN
    FIND FIRST item
        WHERE item.company EQ ip-company
          AND item.i-no    EQ lv-rm-i-no
        NO-LOCK NO-ERROR.
    IF AVAIL item THEN lv-mat-type = item.mat-type.

    FIND FIRST job
        WHERE job.company  EQ ip-company
          AND job.job-no   EQ ip-job-no
          AND (job.job-no2 EQ ip-job-no2 OR ip-job-no2 EQ ?)
        NO-LOCK NO-ERROR.

   IF AVAIL job THEN DO:
      FOR EACH job-mat
          WHERE job-mat.company   EQ job.company
            AND job-mat.job       EQ job.job
            AND job-mat.job-no    EQ job.job-no
            AND job-mat.job-no2   EQ job.job-no2
            AND job-mat.rec_key   NE ip-company
            AND CAN-FIND(FIRST item
                         WHERE item.company   EQ job-mat.company
                           AND item.i-no      EQ job-mat.rm-i-no
                           AND (item.mat-type EQ lv-mat-type OR
                                lv-mat-type   EQ ""))
            AND (NOT ll-job-mat                     OR
                 (job-mat.rm-i-no NE lv-rm-i-no AND
                  job-mat.frm     NE lv-frm))
          USE-INDEX job NO-LOCK:

        /*IF ll-job-mat OR
           NOT CAN-FIND(FIRST po-ordl
                        WHERE po-ordl.company EQ job-mat.company
                          AND po-ordl.job-no  EQ job-mat.job-no
                          AND po-ordl.job-no2 EQ job-mat.job-no2
                          AND po-ordl.s-num   EQ job-mat.frm
                          AND po-ordl.b-num   EQ job-mat.blank-no
                          AND po-ordl.i-no    EQ job-mat.rm-i-no
                        USE-INDEX job-no) THEN  DO: */
          CREATE xreport.
          ASSIGN
           xreport.term-id = v-term + USERID("nosweat")
           xreport.rec_key = STRING(ROWID(job-mat))
           ll-rec-created  = YES.
        /*END.*/
      END.

      IF NOT ll-job-mat OR ll-rec-created THEN DO:
        RUN enable_UI.

        IF ll-job-mat THEN ip-cur-val = "".

        {custom/lookpos.i &lookup-file = "job-mat" &lookup-field = "rm-i-no" }

        APPLY "value-changed" TO rd-sort IN FRAME {&FRAME-NAME}.
    
        WAIT-FOR GO OF FRAME {&FRAME-NAME}.
      END.
    END.
  END.
END.

RUN disable_UI.

FOR EACH xreport WHERE xreport.term-id EQ v-term + USERID("nosweat"):
  DELETE xreport.
END.

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
  ENABLE BROWSE-1 RECT-1 rd-sort Btn_Select bt-clear lv-search bt-ok bt-cancel 
         Btn_Deselect 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


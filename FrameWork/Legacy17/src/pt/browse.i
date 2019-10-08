/* pt/browse.i    Main Browse driver   Jim Van Ryn  10/23/1997 

lisakay matchen 1/4/00 call 13228 add 13'th field for find
lisakay matchen 12/14/01 call # 16120 fix zoom functionname.
lisakay matchen 9/16/02 call # 17115 fix init val going to zoom
lisakay matchen 7/9/03 call # 18164 set component when setting item-no
 */

&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{pt/shared}                                     
DEFINE VAR v-rec-cnt as int init 0 no-undo.
DEFINE VAR v-default-action as log INIT YES no-undo.
DEFINE VAR log1#             AS LOG NO-UNDO .
DEFINE VAR iCurrentIndex  AS INT INIT 0 NO-UNDO .
DEFINE VAR column#       AS INT NO-UNDO.
DEFINE VAR use-field#     AS CHAR NO-UNDO.
DEFINE VAR where-index AS CHAR NO-UNDO.
DEFINE VAR choice as CHAR NO-UNDO.
DEFINE VAR l-matches AS L NO-UNDO.
DEFINE VAR l-cancel AS L NO-UNDO.
DEFINE VAR c-self AS C NO-UNDO.
DEFINE VAR r-rowid AS ROWID NO-UNDO.
DEFINE VAR w-init-val AS CHAR NO-UNDO.
/* help handlers */
define variable h-help as handle no-undo.
define variable h-helps as handle extent 100 no-undo.
define variable i as int no-undo init 0.
define variable h-save as handle no-undo.
define var v-dest as char format "x(60)" no-undo.
DEFINE VARIABLE trashLogical AS LOGICAL NO-UNDO.
def var tmpFunctName as char no-undo.
DEFINE TEMP-TABLE dn NO-UNDO
    FIELD cField1 AS CHARACTER
    FIELD cField2 AS CHARACTER .

DEFINE TEMP-TABLE undn NO-UNDO
    FIELD rField1 AS CHARACTER
    FIELD rField2 AS CHARACTER .

DEFINE TEMP-TABLE fndBestIndex NO-UNDO
     FIELD tIdxName AS CHAR 
     FIELD tWeight AS INT
     INDEX iBest IS PRIMARY tWeight ASCENDING. 
DEFINE TEMP-TABLE pci NO-UNDO
    FIELD pciParent AS CHAR
    FIELD pciChild AS CHAR
    FIELD pciIndex AS CHAR
    INDEX ipci IS PRIMARY pciParent ASCENDING.                        

FUNCTION parentchildIndex RETURNS LOGICAL (INPUT cf1 as Character, INPUT cf2 AS CHARACTER, INPUT cf3 AS CHARACTER) FORWARD.

FUNCTION addDN RETURNS LOGICAL (INPUT cf1 as Character, INPUT cf2 AS CHARACTER) FORWARD.
FUNCTION getNormalField RETURNS CHARACTER (INPUT cf1 as Character, INPUT cf2 AS CHARACTER) FORWARD.

FUNCTION rmDN RETURNS LOGICAL (INPUT cf1 as Character, INPUT cf2 AS CHARACTER) FORWARD.
FUNCTION fixcondition RETURNS LOGICAL (INPUT cf1 as Character, INPUT cf2 AS CHARACTER) FORWARD.

{pt/browse2.i}

&IF  "{&K1}":U  ne  "":U &THEN DEFINE VARIABLE xx{&K1} LIKE {&FILE}.{&K1}
    {&KF1} VIEW-AS FILL-IN SIZE {&KS1} BY 1 NO-UNDO. &ENDIF
&IF  "{&K2}":U  ne  "":U &THEN DEFINE VARIABLE xx{&K2} LIKE {&FILE}.{&K2}
    {&KF2} VIEW-AS FILL-IN SIZE {&KS2} BY 1 NO-UNDO. &ENDIF
&IF  "{&K3}":U  ne  "":U &THEN DEFINE VARIABLE xx{&K3} LIKE {&FILE}.{&K3}
    {&KF3} VIEW-AS FILL-IN SIZE {&KS3} BY 1 NO-UNDO. &ENDIF
&IF  "{&K4}":U  ne  "":U &THEN DEFINE VARIABLE xx{&K4} LIKE {&FILE}.{&K4}
    {&KF4} VIEW-AS FILL-IN SIZE {&KS4} BY 1 NO-UNDO. &ENDIF
&IF  "{&K5}":U  ne  "":U &THEN DEFINE VARIABLE xx{&K5} LIKE {&FILE}.{&K5}
    {&KF5} VIEW-AS FILL-IN SIZE {&KS5} BY 1 NO-UNDO. &ENDIF
&IF  "{&K6}":U  ne  "":U &THEN DEFINE VARIABLE xx{&K6} LIKE {&FILE}.{&K6}
    {&KF6} VIEW-AS FILL-IN SIZE {&KS6} BY 1 NO-UNDO. &ENDIF
&IF  "{&K7}":U  ne  "":U &THEN DEFINE VARIABLE xx{&K7} LIKE {&FILE}.{&K7}
    {&KF7} VIEW-AS FILL-IN SIZE {&KS7} BY 1 NO-UNDO. &ENDIF
&IF  "{&K8}":U  ne  "":U &THEN DEFINE VARIABLE xx{&K8} LIKE {&FILE}.{&K8}
    {&KF8} VIEW-AS FILL-IN SIZE {&KS8} BY 1 NO-UNDO. &ENDIF
&IF  "{&K9}":U  ne  "":U &THEN DEFINE VARIABLE xx{&K9} LIKE {&FILE}.{&K9}
    {&KF9} VIEW-AS FILL-IN SIZE {&KS9} BY 1 NO-UNDO. &ENDIF
&IF  "{&K10}":U  ne  "":U &THEN DEFINE VARIABLE xx{&K10} LIKE {&FILE}.{&K10}
    {&KF10} VIEW-AS FILL-IN SIZE {&KS10} BY 1 NO-UNDO. &ENDIF
&IF  "{&K11}":U  ne  "":U &THEN DEFINE VARIABLE xx{&K11} LIKE {&FILE}.{&K11}
    {&KF11} VIEW-AS FILL-IN SIZE {&KS11} BY 1 NO-UNDO. &ENDIF
&IF  "{&K12}":U  ne  "":U &THEN DEFINE VARIABLE xx{&K12} LIKE {&FILE}.{&K12}
    {&KF12} VIEW-AS FILL-IN SIZE {&KS12} BY 1 NO-UNDO. &ENDIF
&IF  "{&K13}":U  ne  "":U &THEN DEFINE VARIABLE xx{&K13} LIKE {&FILE}.{&K13}
    {&KF13} VIEW-AS FILL-IN SIZE {&KS13} BY 1 NO-UNDO. &ENDIF

{&UOMDEFS}

column# =  if {pt/getinit column} > ""
                        then integer({pt/getinit  column})
                   else 0.
column# = column# + 2 .
{pt/setinit column column#}

/* ********************  Preprocessor Definitions  ******************** */
/* All preprocessors are defined in the calling  browse */

/* _UIB-PREPROCESSOR-BLOCK-END */


/* Define a variable to store the name of the active layout.            */
DEFINE VAR CURRENT-WINDOW-layout AS CHAR INITIAL "Master Layout":U NO-UNDO.

/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cance&l" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1.13 &ENDIF
     .

DEFINE BUTTON Btn_find 
     LABEL "F&ind" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1.13 &ENDIF.

DEFINE BUTTON Btn_full 
     LABEL "F&ull" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1.13 &ENDIF.

DEFINE BUTTON Btn_help 
     LABEL "&Help" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1.13 &ENDIF.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "O&K" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1.13 &ENDIF
     BGCOLOR 8 .

DEFINE BUTTON Btn_zoom 
     LABEL "Z&oom" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
      &ELSE SIZE 10 BY 1.13 &ENDIF.

/* Query definitions                                                    */
DEFINE QUERY BROWSE-1 FOR {&FILE} FIELDS ( {&DEFINE-QUERY} )
    {&UOM-FILE}
    SCROLLING.
    

/* Browse definitions                                                   */
   DEFINE BROWSE BROWSE-1 QUERY BROWSE-1 DISPLAY {&FIELDS-IN-BROWSE}
    WITH NO-ROW-MARKERS SEPARATORS 
    &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 77 BY 15
              &ELSE SIZE 150 BY 16 &ENDIF

         BGCOLOR 16 FGCOLOR 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-1
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 1 COL 2
          &ELSE AT ROW 1 COL 2 &ENDIF
     Btn_find
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 3
          &ELSE AT ROW 17.21 COL 2 &ENDIF
     Btn_zoom
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 15
          &ELSE AT ROW 17.21 COL 14 &ENDIF
     Btn_full
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 28
          &ELSE AT ROW 17.21 COL 26 &ENDIF
     Btn_Cancel
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 41
          &ELSE AT ROW 17.21 COL 38 &ENDIF
     Btn_OK
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 54
          &ELSE AT ROW 17.21 COL 50 &ENDIF
     Btn_help
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 16 COL 65
          &ELSE AT ROW 17.21 COL 68 &ENDIF
          
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "{&TITLE} Browse" 
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel
         ROW 1 COL 1 + column#.


ASSIGN browse-1:NUM-LOCKED-COLUMNS IN FRAME {&FRAME-NAME} = {&LOCKED-COLUMNS} .

/* ***************  Runtime Attributes and UIB Settings  ************** */

ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.


/* ************************  Control Triggers  ************************ */

ON WINDOW-CLOSE OF FRAME D-Dialog /* XXX Browse */
DO: 
  APPLY "END-ERROR":U TO SELF.
END.

ON CHOOSE OF Btn_full IN FRAME {&FRAME-NAME} DO:
  RUN full-frame.
END.

ON CHOOSE OF Btn_find IN FRAME {&FRAME-NAME} DO:
  RUN find-frame.
END.

ON END OF BROWSE-1 DO:
   DEF VAR lend AS LOG.
   Message "This function may take some time. Do you want to continue"
     view-as alert-box question update lend.
   IF NOT lend THEN RETURN NO-APPLY.
END.

ON ANY-PRINTABLE OF BROWSE-1 IN FRAME {&FRAME-NAME} DO:
  APPLY "CHOOSE" TO btn_find.
  RETURN NO-APPLY.
END.
  
  
ON  GO OF BROWSE-1 
    OR DEFAULT-ACTION OF BROWSE-1
    OR  CHOOSE OF btn_ok
    OR  GO     OF btn_ok 
    IN FRAME {&FRAME-NAME}    
DO:  
    IF v-default-action THEN 
    DO:
        RUN local-select-proc IN THIS-PROCEDURE.
        IF AVAIL {&FILE} THEN ASSIGN result = STRING(rowid({&FILE})).  
        APPLY "END-ERROR" TO FRAME {&FRAME-NAME}.   
    END.  
END.

 /*
ON CHOOSE OF btn_cancel IN FRAME {&FRAME-NAME} DO:
    v-default-action = yes.
END. 
  */

ON CHOOSE OF btn_zoom IN FRAME {&FRAME-NAME} DO:
  
  DEF VAR last-selection AS CHAR NO-UNDO.
  DEF VAR result-value# AS CHAR NO-UNDO.
  DEF VAR kase as INT NO-UNDO.

  h-help = current-window:first-child.
  do while h-help ne ?:
    if h-help:visible = true then assign
    i = i + 1
    h-helps[i] = h-help:handle.
    h-help = h-help:next-sibling.
 end.


  DO ON ERROR UNDO, LEAVE ON END-KEY UNDO, LEAVE:
      ASSIGN log1# = BROWSE-1:FETCH-SELECTED-ROW(1) IN FRAME {&FRAME-NAME} NO-ERROR.
      r-rowid = ROWID ({&FILE}).
      FIND {&FILE} WHERE ROWID({&FILE}) = r-rowid NO-LOCK.
      /* 17115 */
      assign init-val = "".
      &IF "{&K1}":U NE "":U &THEN {pt/setinit {&K1} {&FILE}.{&K1}} &ENDIF
      &IF "{&K2}":U NE "":U &THEN {pt/setinit {&K2} {&FILE}.{&K2}} &ENDIF
      &IF "{&K3}":U NE "":U &THEN {pt/setinit {&K3} {&FILE}.{&K3}} &ENDIF
      &IF "{&K4}":U NE "":U &THEN {pt/setinit {&K4} {&FILE}.{&K4}} &ENDIF
      &IF "{&K5}":U NE "":U &THEN {pt/setinit {&K5} {&FILE}.{&K5}} &ENDIF
      &IF "{&K6}":U NE "":U &THEN {pt/setinit {&K6} {&FILE}.{&K6}} &ENDIF
      &IF "{&K7}":U NE "":U &THEN {pt/setinit {&K7} {&FILE}.{&K7}} &ENDIF
      &IF "{&K8}":U NE "":U &THEN {pt/setinit {&K8} {&FILE}.{&K8}} &ENDIF
      &IF "{&K9}":U NE "":U &THEN {pt/setinit {&K9} {&FILE}.{&K9}} &ENDIF
      &IF "{&K10}":U NE "":U &THEN {pt/setinit {&K10} {&FILE}.{&K10}} &ENDIF
      &IF "{&K11}":U NE "":U &THEN {pt/setinit {&K11} {&FILE}.{&K11}} &ENDIF
      &IF "{&K12}":U NE "":U &THEN {pt/setinit {&K12} {&FILE}.{&K12}} &ENDIF
      &IF "{&K13}":U NE "":U &THEN {pt/setinit {&K13} {&FILE}.{&K13}} &ENDIF
      &IF "{&K1}":U eq "ITEM-NO":U &THEN {pt/setinit Component {&FILE}.{&K1}} &ENDIF
      &IF "{&K2}":U eq "ITEM-NO":U &THEN {pt/setinit Component {&FILE}.{&K2}} &ENDIF
      &IF "{&K3}":U eq "ITEM-NO":U &THEN {pt/setinit Component {&FILE}.{&K3}} &ENDIF
      &IF "{&K4}":U eq "ITEM-NO":U &THEN {pt/setinit Component {&FILE}.{&K4}} &ENDIF
      &IF "{&K5}":U eq "ITEM-NO":U &THEN {pt/setinit Component {&FILE}.{&K5}} &ENDIF
      &IF "{&K6}":U eq "ITEM-NO":U &THEN {pt/setinit Component {&FILE}.{&K6}} &ENDIF
      &IF "{&K7}":U eq "ITEM-NO":U &THEN {pt/setinit Component {&FILE}.{&K7}} &ENDIF
      &IF "{&K8}":U eq "ITEM-NO":U &THEN {pt/setinit Component {&FILE}.{&K8}} &ENDIF
      &IF "{&K9}":U eq "ITEM-NO":U &THEN {pt/setinit Component {&FILE}.{&K9}} &ENDIF
      &IF "{&K10}":U eq "ITEM-NO":U &THEN {pt/setinit Component {&FILE}.{&K10}} &ENDIF
      &IF "{&K11}":U eq "ITEM-NO":U &THEN {pt/setinit Component {&FILE}.{&K11}} &ENDIF
      &IF "{&K12}":U eq "ITEM-NO":U &THEN {pt/setinit Component {&FILE}.{&K12}} &ENDIF
      &IF "{&K13}":U eq "ITEM-NO":U &THEN {pt/setinit Component {&FILE}.{&K13}} &ENDIF

          tmpFunctName = functionname.

      RUN VALUE("pt/zoom.p":U) (functionname,yes,0,0,
                          output kase,
                          output result-value#,
                          input-output last-selection).
 
          functionname = tmpFunctName.

    END.

    IF result-value# = 'error':U THEN
        MESSAGE 'Zoom is not available. Run `"Zoom Maintenance~" '
            VIEW-AS ALERT-BOX .
    ELSE DO ON ERROR UNDO, LEAVE ON END-KEY UNDO, LEAVE:
      IF result-value# > "" and search(result-value#) ne ?
        then DO:
          FRAME {&FRAME-NAME}:HIDDEN = TRUE.
          RUN VALUE(result-value#) NO-ERROR.
          FRAME {&FRAME-NAME}:HIDDEN = FALSE.
        END.
    END.

    do i = i to 1 by -1:
      h-helps[i]:visible = true no-error.
     end.
    &IF  "{&WINDOW-SYSTEM}":U  eq "TTY":U &THEN
      /* apply "entry"  to h-save . JRB- THIS APPLY ENTRY CAUSE ERROR 3190 ON RETURN OF ZOOM FROM ANOTHER BROWSE */
      /* DO NOT KNOW IT'S ORIGIONAL REASON FOR BEING HERE - THE CORRECT BROWSE SEEMS TO GET FOCUS BELOW*/
    &ENDIF

   APPLY 'ENTRY':U TO BROWSE-1 IN FRAME {&FRAME-NAME}.
END.

ON CHOOSE OF btn_help IN FRAME {&FRAME-NAME}
         OR GO OF btn_help IN FRAME {&FRAME-NAME}
DO:
 .
       RUN pt/ubrhelp.p ( "main":U ).
END.


/* ***************************  Main Block  *************************** */

PAUSE 0 BEFORE-HIDE.
{pt/saveinit.i w}    /* save init-val */

init-val[1] = "". /* if set by bad code */
 

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  DEF VAR c-return AS C NO-UNDO.
  RUN local-init-local-vars.
  IF return-value = 'empty':U THEN
    LEAVE MAIN-BLOCK.
  RUN enable-UI.
  c-return = RETURN-VALUE.  RUN local-enable-UI IN THIS-PROCEDURE.
  IF c-return NE "FIND" THEN APPLY 'ENTRY':U TO BROWSE-1.
  ELSE APPLY "CHOOSE" TO btn_find.
  
  

  WAIT-FOR WINDOW-CLOSE OF FRAME {&FRAME-NAME}.



END.   /* main-block */

{pt/saveinit.i  " " w}            /* restore init-val */


/* **********************  Internal Procedures  *********************** */

PROCEDURE enable-UI :
   
  ENABLE BROWSE-1 btn_find btn_zoom btn_full btn_cancel btn_ok btn_help
      WITH FRAME {&FRAME-NAME}.
  log1# = BROWSE-1:SELECT-ROW(1) IN FRAME {&FRAME-NAME} NO-ERROR.
  VIEW FRAME {&FRAME-NAME}.
END PROCEDURE.

PROCEDURE disable-ui:
    DISABLE btn_zoom btn_ok
        WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

PROCEDURE init-local-vars :
 
    use-field# = use-field.
    RUN local-select-key IN THIS-PROCEDURE (use-field).

    &IF "{&K1}":U NE "":U &THEN 
      ASSIGN trashLogical = fixcondition(STRING("{&FILE}"+ "." + "{&K1}"),{pt/getinit file-name}).
      ASSIGN w-init-val = {pt/getinit "{&K1}"}.
      IF w-init-val = "" AND INDEX("{&K1}","-entity") NE 0 THEN w-init-val = {pt/getinit "entity-code"}.
      IF w-init-val = "" THEN ASSIGN w-init-val = 
         init-val[lookup(getNormalField(STRING("{&FILE}"+ "." + "{&K1}"),{pt/getinit file-name}),condition) + 1].
      IF w-init-val NE "" THEN DO:
        &IF "{&KT1}" EQ "CH" &THEN xx{&K1} = w-init-val.
        &ELSEIF "{&KT1}" EQ "IN" &THEN xx{&K1} = INTEGER(w-init-val).
        &ELSEIF "{&KT1}" EQ "DE" &THEN xx{&K1} = DECIMAL(w-init-val).
        &ELSEIF "{&KT1}" EQ "DA" &THEN xx{&K1} = DATE(w-init-val).
        &ELSEIF "{&KT1}" EQ "LO" &THEN xx{&K1} = IF w-init-val = "TRUE" THEN TRUE ELSE FALSE.  &ENDIF
      /*IF iCurrentIndex = 0 THEN run local-select-key IN THIS-PROCEDURE ("{&K1}").*/
      END.
    &ENDIF

    &IF "{&K2}":U NE "":U &THEN 
      ASSIGN trashLogical = fixcondition(STRING("{&FILE}"+ "." + "{&K2}"),{pt/getinit file-name}).
      ASSIGN w-init-val = {pt/getinit "{&K2}"}.
    IF w-init-val = "" AND INDEX("{&K2}","-entity") NE 0
        THEN w-init-val = {pt/getinit "entity-code"}.
    IF w-init-val = "" THEN ASSIGN w-init-val = 
       init-val[lookup(getNormalField(STRING("{&FILE}"+ "." + "{&K2}"),{pt/getinit file-name}),condition) + 1].
    IF w-init-val NE "" THEN DO:
      &IF "{&KT2}" EQ "CH" &THEN xx{&K2} = w-init-val.
      &ELSEIF "{&KT2}" EQ "IN" &THEN xx{&K2} = INTEGER(w-init-val).
      &ELSEIF "{&KT2}" EQ "DE" &THEN xx{&K2} = DECIMAL(w-init-val).
      &ELSEIF "{&KT2}" EQ "DA" &THEN xx{&K2} = DATE(w-init-val).
      &ELSEIF "{&KT2}" EQ "LO" &THEN xx{&K2} = IF w-init-val = "TRUE" THEN TRUE ELSE FALSE.  &ENDIF
    /*IF iCurrentIndex = 0 THEN run local-select-key IN THIS-PROCEDURE ("{&K2}").*/
    END.
    &ENDIF

    &IF "{&K3}":U NE "":U &THEN 
      ASSIGN trashLogical = fixcondition(STRING("{&FILE}"+ "." + "{&K3}"),{pt/getinit file-name}).
      ASSIGN w-init-val = {pt/getinit "{&K3}"}.
    IF w-init-val = "" AND INDEX("{&K3}","-entity") NE 0
        THEN w-init-val = {pt/getinit "entity-code"}.
    IF w-init-val = "" THEN ASSIGN w-init-val = 
       init-val[lookup(getNormalField(STRING("{&FILE}"+ "." + "{&K3}"),{pt/getinit file-name}),condition) + 1].
    IF w-init-val NE "" THEN DO:
      &IF "{&KT3}" EQ "CH" &THEN xx{&K3} = w-init-val.
      &ELSEIF "{&KT3}" EQ "IN" &THEN xx{&K3} = INTEGER(w-init-val).
      &ELSEIF "{&KT3}" EQ "DE" &THEN xx{&K3} = DECIMAL(w-init-val).
      &ELSEIF "{&KT3}" EQ "DA" &THEN xx{&K3} = DATE(w-init-val).
      &ELSEIF "{&KT3}" EQ "LO" &THEN xx{&K3} = IF w-init-val = "TRUE" THEN TRUE ELSE FALSE.  &ENDIF
    /*IF iCurrentIndex = 0 THEN run local-select-key IN THIS-PROCEDURE ("{&K3}").*/
    END.
    &ENDIF

    &IF "{&K4}":U NE "":U &THEN 
      ASSIGN trashLogical = fixcondition(STRING("{&FILE}"+ "." + "{&K4}"),{pt/getinit file-name}).
      ASSIGN w-init-val = {pt/getinit "{&K4}"}.
    IF w-init-val = "" AND INDEX("{&K4}","-entity") NE 0
        THEN w-init-val = {pt/getinit "entity-code"}.
    IF w-init-val = "" THEN ASSIGN w-init-val = 
       init-val[lookup(getNormalField(STRING("{&FILE}"+ "." + "{&K4}"),{pt/getinit file-name}),condition) + 1].
    IF w-init-val NE "" THEN DO:
      &IF "{&KT4}" EQ "CH" &THEN xx{&K4} = w-init-val.
      &ELSEIF "{&KT4}" EQ "IN" &THEN xx{&K4} = INTEGER(w-init-val).
      &ELSEIF "{&KT4}" EQ "DE" &THEN xx{&K4} = DECIMAL(w-init-val).
      &ELSEIF "{&KT4}" EQ "DA" &THEN xx{&K4} = DATE(w-init-val).
      &ELSEIF "{&KT4}" EQ "LO" &THEN xx{&K4} = IF w-init-val = "TRUE" THEN TRUE ELSE FALSE.  &ENDIF
    /*IF iCurrentIndex = 0 THEN run local-select-key IN THIS-PROCEDURE ("{&K4}"). */
    END.
    &ENDIF
    
    &IF "{&K5}":U NE "":U &THEN 
      ASSIGN trashLogical = fixcondition(STRING("{&FILE}"+ "." + "{&K5}"),{pt/getinit file-name}).
      ASSIGN w-init-val = {pt/getinit "{&K5}"}.
    IF w-init-val = "" AND INDEX("{&K5}","-entity") NE 0
        THEN w-init-val = {pt/getinit "entity-code"}.
    IF w-init-val = "" THEN ASSIGN w-init-val = 
       init-val[lookup(getNormalField(STRING("{&FILE}"+ "." + "{&K5}"),{pt/getinit file-name}),condition) + 1].
    IF w-init-val NE "" THEN DO:
      &IF "{&KT5}" EQ "CH" &THEN xx{&K5} = w-init-val.
      &ELSEIF "{&KT5}" EQ "IN" &THEN xx{&K5} = INTEGER(w-init-val).
      &ELSEIF "{&KT5}" EQ "DE" &THEN xx{&K5} = DECIMAL(w-init-val).
      &ELSEIF "{&KT5}" EQ "DA" &THEN xx{&K5} = DATE(w-init-val).
      &ELSEIF "{&KT5}" EQ "LO" &THEN xx{&K5} = IF w-init-val = "TRUE" THEN TRUE ELSE FALSE.  &ENDIF
    /*IF iCurrentIndex = 0 THEN run local-select-key IN THIS-PROCEDURE ("{&K5}").*/
    END.
    &ENDIF

    &IF "{&K6}":U NE "":U &THEN 
      ASSIGN trashLogical = fixcondition(STRING("{&FILE}"+ "." + "{&K6}"),{pt/getinit file-name}).
      ASSIGN w-init-val = {pt/getinit "{&K6}"}.
    IF w-init-val = "" AND INDEX("{&K6}","-entity") NE 0
        THEN w-init-val = {pt/getinit "entity-code"}.
    IF w-init-val = "" THEN ASSIGN w-init-val = 
       init-val[lookup(getNormalField(STRING("{&FILE}"+ "." + "{&K6}"),{pt/getinit file-name}),condition) + 1].
    IF w-init-val NE "" THEN DO:
      &IF "{&KT6}" EQ "CH" &THEN xx{&K6} = w-init-val.
      &ELSEIF "{&KT6}" EQ "IN" &THEN xx{&K6} = INTEGER(w-init-val).
      &ELSEIF "{&KT6}" EQ "DE" &THEN xx{&K6} = DECIMAL(w-init-val).
      &ELSEIF "{&KT6}" EQ "DA" &THEN xx{&K6} = DATE(w-init-val).
      &ELSEIF "{&KT6}" EQ "LO" &THEN xx{&K6} = IF w-init-val = "TRUE" THEN TRUE ELSE FALSE.  &ENDIF
    /*IF iCurrentIndex = 0 THEN run local-select-key IN THIS-PROCEDURE ("{&K6}").*/
    END.
    &ENDIF

    &IF "{&K7}":U NE "":U &THEN 
      ASSIGN trashLogical = fixcondition(STRING("{&FILE}"+ "." + "{&K7}"),{pt/getinit file-name}).
      ASSIGN w-init-val = {pt/getinit "{&K7}"}.
    IF w-init-val = "" AND INDEX("{&K7}","-entity") NE 0
        THEN w-init-val = {pt/getinit "entity-code"}.
    IF w-init-val = "" THEN ASSIGN w-init-val = 
       init-val[lookup(getNormalField(STRING("{&FILE}"+ "." + "{&K7}"),{pt/getinit file-name}),condition) + 1].
    IF w-init-val NE "" THEN DO:
      &IF "{&KT7}" EQ "CH" &THEN xx{&K7} = w-init-val.
      &ELSEIF "{&KT7}" EQ "IN" &THEN xx{&K7} = INTEGER(w-init-val).
      &ELSEIF "{&KT7}" EQ "DE" &THEN xx{&K7} = DECIMAL(w-init-val).
      &ELSEIF "{&KT7}" EQ "DA" &THEN xx{&K7} = DATE(w-init-val).
      &ELSEIF "{&KT7}" EQ "LO" &THEN xx{&K7} = IF w-init-val = "TRUE" THEN TRUE ELSE FALSE.  &ENDIF
    /*IF iCurrentIndex = 0 THEN run local-select-key IN THIS-PROCEDURE ("{&K7}").*/
    END.
    &ENDIF

    &IF "{&K8}":U NE "":U &THEN 
      ASSIGN trashLogical = fixcondition(STRING("{&FILE}"+ "." + "{&K8}"),{pt/getinit file-name}).
      ASSIGN w-init-val = {pt/getinit "{&K8}"}.
    IF w-init-val = "" AND INDEX("{&K8}","-entity") NE 0
        THEN w-init-val = {pt/getinit "entity-code"}.
    IF w-init-val = "" THEN ASSIGN w-init-val = 
       init-val[lookup(getNormalField(STRING("{&FILE}"+ "." + "{&K8}"),{pt/getinit file-name}),condition) + 1].
    IF w-init-val NE "" THEN DO:
      &IF "{&KT8}" EQ "CH" &THEN xx{&K8} = w-init-val.
      &ELSEIF "{&KT8}" EQ "IN" &THEN xx{&K8} = INTEGER(w-init-val).
      &ELSEIF "{&KT8}" EQ "DE" &THEN xx{&K8} = DECIMAL(w-init-val).
      &ELSEIF "{&KT8}" EQ "DA" &THEN xx{&K8} = DATE(w-init-val).
      &ELSEIF "{&KT8}" EQ "LO" &THEN xx{&K8} = IF w-init-val = "TRUE" THEN TRUE ELSE FALSE.  &ENDIF
    /*IF iCurrentIndex = 0 THEN run local-select-key IN THIS-PROCEDURE ("{&K8}").*/
    END.
    &ENDIF

    &IF "{&K9}":U NE "":U &THEN 
      ASSIGN trashLogical = fixcondition(STRING("{&FILE}"+ "." + "{&K9}"),{pt/getinit file-name}).
      ASSIGN w-init-val = {pt/getinit "{&K9}"}.
    IF w-init-val = "" AND INDEX("{&K9}","-entity") NE 0
        THEN w-init-val = {pt/getinit "entity-code"}.
    IF w-init-val = "" THEN ASSIGN w-init-val = 
       init-val[lookup(getNormalField(STRING("{&FILE}"+ "." + "{&K9}"),{pt/getinit file-name}),condition) + 1].
    IF w-init-val NE "" THEN DO:
      &IF "{&KT9}" EQ "CH" &THEN xx{&K9} = w-init-val.
      &ELSEIF "{&KT9}" EQ "IN" &THEN xx{&K9}= INTEGER(w-init-val).
      &ELSEIF "{&KT9}" EQ "DE" &THEN xx{&K9} = DECIMAL(w-init-val).
      &ELSEIF "{&KT9}" EQ "DA" &THEN xx{&K9} = DATE(w-init-val).
      &ELSEIF "{&KT9}" EQ "LO" &THEN xx{&K9} = IF w-init-val = "TRUE" THEN TRUE ELSE FALSE.  &ENDIF
    /*IF iCurrentIndex = 0 THEN run local-select-key IN THIS-PROCEDURE ("{&K9}").*/
    END.
    &ENDIF

    &IF "{&K10}":U NE "":U &THEN 
      ASSIGN trashLogical = fixcondition(STRING("{&FILE}"+ "." + "{&K10}"),{pt/getinit file-name}).
      ASSIGN w-init-val = {pt/getinit "{&K10}"}.
    IF w-init-val = "" AND INDEX("{&K10}","-entity") NE 0
        THEN w-init-val = {pt/getinit "entity-code"}.
    IF w-init-val = "" THEN ASSIGN w-init-val = 
       init-val[lookup(getNormalField(STRING("{&FILE}"+ "." + "{&K10}"),{pt/getinit file-name}),condition) + 1].
    IF w-init-val NE "" THEN DO:
      &IF "{&KT10}" EQ "CH" &THEN xx{&K10} = w-init-val.
      &ELSEIF "{&KT10}" EQ "IN" &THEN xx{&K10} = INTEGER(w-init-val).
      &ELSEIF "{&KT10}" EQ "DE" &THEN xx{&K10} = DECIMAL(w-init-val).
      &ELSEIF "{&KT10}" EQ "DA" &THEN xx{&K10} = DATE(w-init-val).
      &ELSEIF "{&KT10}" EQ "LO" &THEN xx{&K10} = IF w-init-val = "TRUE" THEN TRUE ELSE FALSE.  &ENDIF
      /*IF iCurrentIndex = 0 THEN run local-select-key IN THIS-PROCEDURE ("{&K10}").*/
    END.
    &ENDIF

    &IF "{&K11}":U NE "":U &THEN 
      ASSIGN trashLogical = fixcondition(STRING("{&FILE}"+ "." + "{&K11}"),{pt/getinit file-name}).
      ASSIGN w-init-val = {pt/getinit "{&K11}"}.
    IF w-init-val = "" AND INDEX("{&K11}","-entity") NE 0
        THEN w-init-val = {pt/getinit "entity-code"}.
    IF w-init-val = "" THEN ASSIGN w-init-val = 
       init-val[lookup(getNormalField(STRING("{&FILE}"+ "." + "{&K11}"),{pt/getinit file-name}),condition) + 1].
    IF w-init-val NE "" THEN DO:
      &IF "{&KT11}" EQ "CH" &THEN xx{&K11} = w-init-val.
      &ELSEIF "{&KT11}" EQ "IN" &THEN xx{&K11} = INTEGER(w-init-val).
      &ELSEIF "{&KT11}" EQ "DE" &THEN xx{&K11} = DECIMAL(w-init-val).
      &ELSEIF "{&KT11}" EQ "DA" &THEN xx{&K11} = DATE(w-init-val).
      &ELSEIF "{&KT11}" EQ "LO" &THEN xx{&K11} = IF w-init-val = "TRUE" THEN TRUE ELSE FALSE.  &ENDIF
      /*IF iCurrentIndex = 0 THEN run local-select-key IN THIS-PROCEDURE ("{&K11}").*/
    END.
    &ENDIF

    &IF "{&K12}":U NE "":U &THEN 
      ASSIGN trashLogical = fixcondition(STRING("{&FILE}"+ "." + "{&K12}"),{pt/getinit file-name}).
      ASSIGN w-init-val = {pt/getinit "{&K12}"}.
    IF w-init-val = "" AND INDEX("{&K12}","-entity") NE 0
        THEN w-init-val = {pt/getinit "entity-code"}.
    IF w-init-val = "" THEN ASSIGN w-init-val = 
       init-val[lookup(getNormalField(STRING("{&FILE}"+ "." + "{&K12}"),{pt/getinit file-name}),condition) + 1].
    IF w-init-val NE "" THEN DO:
      &IF "{&KT12}" EQ "CH" &THEN xx{&K12} = w-init-val.
      &ELSEIF "{&KT12}" EQ "IN" &THEN xx{&K12} = INTEGER(w-init-val).
      &ELSEIF "{&KT12}" EQ "DE" &THEN xx{&K12} = DECIMAL(w-init-val).
      &ELSEIF "{&KT12}" EQ "DA" &THEN xx{&K12} = DATE(w-init-val).
      &ELSEIF "{&KT12}" EQ "LO" &THEN xx{&K12} = IF w-init-val = "TRUE" THEN TRUE ELSE FALSE.  &ENDIF
      /*IF iCurrentIndex = 0 THEN run local-select-key IN THIS-PROCEDURE ("{&K12}").*/
    END.
    &ENDIF
   &IF "{&K13}":U NE "":U &THEN 
      ASSIGN trashLogical = fixcondition(STRING("{&FILE}"+ "." + "{&K13}"),{pt/getinit file-name}).
      ASSIGN w-init-val = {pt/getinit "{&K13}"}.
    IF w-init-val = "" AND INDEX("{&K13}","-entity") NE 0
        THEN w-init-val = {pt/getinit "entity-code"}.
    IF w-init-val = "" THEN ASSIGN w-init-val = 
       init-val[lookup(getNormalField(STRING("{&FILE}"+ "." + "{&K13}"),{pt/getinit file-name}),condition) + 1].
    IF w-init-val NE "" THEN DO:
      &IF "{&KT13}" EQ "CH" &THEN xx{&K13} = w-init-val.
      &ELSEIF "{&KT13}" EQ "IN" &THEN xx{&K13} = INTEGER(w-init-val).
      &ELSEIF "{&KT13}" EQ "DE" &THEN xx{&K13} = DECIMAL(w-init-val).
      &ELSEIF "{&KT13}" EQ "DA" &THEN xx{&K13} = DATE(w-init-val).
      &ELSEIF "{&KT13}" EQ "LO" &THEN xx{&K13} = IF w-init-val = "TRUE" THEN TRUE ELSE FALSE.  &ENDIF
      /*IF iCurrentIndex = 0 THEN run local-select-key IN THIS-PROCEDURE ("{&K13}").*/
    END.
    &ENDIF

   /* JRB */
/*    run fndIdx ({pt/getinit file-name}). */
    IF iCurrentIndex = 0 THEN run fndIdx ({pt/getinit file-name}).
   IF iCurrentIndex = 0 THEN iCurrentIndex = 1.
   RUN local-open-query IN THIS-PROCEDURE.

   IF NOT AVAILABLE {&FILE} THEN DO:
      FIND FIRST {&FILE} NO-LOCK NO-ERROR.
      IF NOT AVAIL {&FILE} THEN DO:
        MESSAGE "File: {&FILE} {&TITLE} is empty"
            VIEW-AS ALERT-BOX INFORMATION.
        RETURN 'empty'.
      END.
      ELSE DO:
        MESSAGE "File: {&FILE} {&TITLE} does not have records for these parameters"
                VIEW-AS ALERT-BOX.
        RETURN 'find':U.
      END.
    END.

    {pt/setinit file-name '{&FILE}':U}  /* for file related stuff */

END PROCEDURE.

PROCEDURE open-query:

  assign  BROWSE-1:max-data-guess IN FRAME {&FRAME-NAME} = 10000.  
    log1# = SESSION:SET-WAIT-STATE("GENERAL":U) .
   CLOSE QUERY browse-1 .

   CASE iCurrentIndex:

   &IF "{&INDEX1}":U NE "":U &THEN
   
   WHEN 1 THEN
    IF l-matches THEN
        OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
            WHERE {&WHERE-CLAUSE-MATCHES} {&where-clause1}
            TRUE USE-INDEX {&INDEX1} {&UOM-OPEN}.
    ELSE OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
        WHERE {&WHERE-CLAUSE} {&WHERE-CLAUSE1}
        TRUE USE-INDEX {&INDEX1} {&UOM-OPEN}.
   &ENDIF
 
   &IF "{&INDEX2}":U NE "":U &THEN
   WHEN 2 THEN
    IF l-matches THEN
        OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
            WHERE {&WHERE-CLAUSE-MATCHES} {&where-clause1}
            TRUE USE-INDEX {&INDEX2}{&UOM-OPEN}.
    ELSE OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
        WHERE {&WHERE-CLAUSE} {&WHERE-CLAUSE1}
        TRUE USE-INDEX {&INDEX2} {&UOM-OPEN}.
   &ENDIF

   &IF "{&INDEX3}":U NE "":U &THEN
   WHEN 3 THEN
    IF l-matches THEN
        OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
            WHERE {&WHERE-CLAUSE-MATCHES} {&where-clause1}
            TRUE USE-INDEX {&INDEX3} {&UOM-OPEN}.
    ELSE OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
        WHERE {&WHERE-CLAUSE} {&where-clause1}
        TRUE USE-INDEX {&INDEX3} {&UOM-OPEN}.
   &ENDIF

   &IF "{&INDEX4}":U NE "":U &THEN
   WHEN 4 THEN
    IF l-matches THEN
        OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
            WHERE {&WHERE-CLAUSE-MATCHES} {&where-clause1}
            TRUE USE-INDEX {&INDEX4} {&UOM-OPEN}.
    ELSE OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
        WHERE {&WHERE-CLAUSE} {&where-clause1}
        TRUE USE-INDEX {&INDEX4} {&UOM-OPEN}.
   &ENDIF

   &IF "{&INDEX5}":U NE "":U &THEN
   WHEN 5 THEN
    IF l-matches THEN
        OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
            WHERE {&WHERE-CLAUSE-MATCHES} {&where-clause1}
            TRUE USE-INDEX {&INDEX5} {&UOM-OPEN}.
    ELSE OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
        WHERE {&WHERE-CLAUSE} {&where-clause1}
        TRUE USE-INDEX {&INDEX5} {&UOM-OPEN}.
   &ENDIF

   &IF "{&INDEX6}":U NE "":U &THEN
   WHEN 6 THEN
    IF l-matches THEN
        OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
            WHERE {&WHERE-CLAUSE-MATCHES} {&where-clause1}
            TRUE USE-INDEX {&INDEX6} {&UOM-OPEN}.
    ELSE OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
        WHERE {&WHERE-CLAUSE} {&where-clause1}
        TRUE USE-INDEX {&INDEX6} {&UOM-OPEN}.
   &ENDIF

   &IF "{&INDEX7}":U NE "":U &THEN
   WHEN 7 THEN
    IF l-matches THEN
        OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
            WHERE {&WHERE-CLAUSE-MATCHES} {&where-clause1}
            TRUE USE-INDEX {&INDEX7}
            {&UOM-OPEN}.
    ELSE OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
        WHERE {&WHERE-CLAUSE} {&where-clause1}
        TRUE USE-INDEX {&INDEX7} {&UOM-OPEN}.
   &ENDIF

   &IF "{&INDEX8}":U NE "":U &THEN
   WHEN 8 THEN
    IF l-matches THEN
        OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
            WHERE {&WHERE-CLAUSE-MATCHES} {&where-clause1}
            TRUE USE-INDEX {&INDEX8} {&UOM-OPEN}.
    ELSE OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
        WHERE {&WHERE-CLAUSE} {&where-clause1}
        TRUE USE-INDEX {&INDEX8} {&UOM-OPEN}.
   &ENDIF

   &IF "{&INDEX9}":U NE "":U &THEN
   WHEN 9 THEN
    IF l-matches THEN
        OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
            WHERE {&WHERE-CLAUSE-MATCHES} {&where-clause1}
            TRUE USE-INDEX {&INDEX9} {&UOM-OPEN}.
    ELSE OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
        WHERE {&WHERE-CLAUSE} {&where-clause1}
        TRUE USE-INDEX {&INDEX9} {&UOM-OPEN}.
   &ENDIF

   &IF "{&INDEX10}":U NE "":U &THEN
   WHEN 10 THEN
    IF l-matches THEN
        OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
            WHERE {&WHERE-CLAUSE-MATCHES} {&where-clause1}
            TRUE USE-INDEX {&INDEX10} {&UOM-OPEN}.
    ELSE OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
        WHERE {&WHERE-CLAUSE} {&where-clause1}
        TRUE USE-INDEX {&INDEX10} {&UOM-OPEN}.
   &ENDIF

   &IF "{&INDEX11}":U NE "":U &THEN
   WHEN 11 THEN
    IF l-matches THEN
        OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
            WHERE {&WHERE-CLAUSE-MATCHES} {&where-clause1}
            TRUE USE-INDEX {&INDEX11} {&UOM-OPEN}.
    ELSE OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
        WHERE {&WHERE-CLAUSE} {&where-clause1}
        TRUE USE-INDEX {&INDEX11} {&UOM-OPEN}.
   &ENDIF

   &IF "{&INDEX12}":U NE "":U &THEN
   WHEN 12 THEN
    IF l-matches THEN
        OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
            WHERE {&WHERE-CLAUSE-MATCHES} {&where-clause1}
            TRUE USE-INDEX {&INDEX12} {&UOM-OPEN}.
    ELSE OPEN QUERY BROWSE-1 FOR EACH {&FILE} NO-LOCK
        WHERE {&WHERE-CLAUSE} {&where-clause1}
        TRUE USE-INDEX {&INDEX12} {&UOM-OPEN}.
   &ENDIF

   END CASE.
   log1# = SESSION:SET-WAIT-STATE("":U) .
 
END PROCEDURE.

Procedure fndIdx:
  DEFINE INPUT PARAMETER cParentFile AS CHAR NO-UNDO.
  define variable vParent as character no-undo.
  define variable vChild as character no-undo.
  DEFINE VARIABLE iParce AS INTEGER NO-UNDO.
  DEFINE VARIABLE vIndexWeight AS INTEGER NO-UNDO.
  DEFINE VARIABLE idxString AS CHAR NO-UNDO.
  DEFINE VARIABLE vIndexFieldList AS CHAR NO-UNDO.

  assign vParent = cParentFile
         vChild  = "{&FILE}"
         idxString = &IF "{&INDEX1}" NE "":U &THEN "{&INDEX1}" &ELSE "" &ENDIF + "," + 
                     &IF "{&INDEX2}" NE "":U &THEN "{&INDEX2}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX3}" NE "":U &THEN "{&INDEX3}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX4}" NE "":U &THEN "{&INDEX4}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX5}" NE "":U &THEN "{&INDEX5}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX6}" NE "":U &THEN "{&INDEX6}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX7}" NE "":U &THEN "{&INDEX7}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX8}" NE "":U &THEN "{&INDEX8}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX9}" NE "":U &THEN "{&INDEX9}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX10}" NE "":U &THEN "{&INDEX10}" &ELSE "" &ENDIF.

  FIND FIRST pci WHERE pci.pciParent = vParent AND pci.pciChild = vChild NO-LOCK NO-ERROR.
  IF AVAILABLE pci THEN DO:
     ASSIGN iCurrentIndex = lookup(pci.pciIndex,idxString).
     RETURN.
  END.

  FOR EACH fndBestIndex:
    DELETE fndBestIndex.
  END.

  FIND ptdb9._file WHERE ptdb9._file._file-name = vChild no-lock no-error.
  IF NOT AVAILABLE ptdb9._file THEN RETURN.
  FOR EACH ptdb9._index OF ptdb9._file no-lock:
    ASSIGN vIndexFieldList = ""
           vIndexWeight = 0.
    FOR EACH ptdb9._index-field OF ptdb9._index no-lock:
      FIND ptdb9._field OF ptdb9._Index-field NO-LOCK NO-ERROR.
      IF AVAILABLE ptdb9._field THEN 
          ASSIGN vIndexFieldList = 
            IF vIndexFieldList = "" THEN getNormalField(STRING(vChild + "." + ptdb9._field._field-name),vParent) 
            ELSE vIndexFieldList + "," + getNormalField(STRING(vChild + "." + ptdb9._field._field-name),vParent).
    END.
    IF NUM-ENTRIES(vIndexFieldList) > 0 THEN DO iParce = 1 TO NUM-ENTRIES(vIndexFieldList):
        IF init-val[lookup(entry(iParce,vIndexFieldList),condition) + 1] <> "" 
           then assign vIndexWeight = vIndexWeight + (1000000 / (iParce * 10)).
    END.
    CREATE fndBestIndex.
    ASSIGN tIdxName = ptdb9._index._index-name
           tWeight = vIndexWeight.
  END.
  
  FIND LAST fndBestIndex WHERE fndBestIndex.tWeight > 0 no-error.
  if available fndBestIndex then assign iCurrentIndex = lookup(fndBestIndex.tIdxName,idxString).
  else assign iCurrentIndex = 1.
  if iCurrentIndex = 0 then assign iCurrentIndex = 1.

  FOR EACH fndBestIndex:
    DELETE fndBestIndex.
  END.
end.

PROCEDURE select-key:
 
  DEFINE INPUT PARAMETER pField AS CHAR NO-UNDO.
  DEFINE VAR i AS INT NO-UNDO.
  DEFINE VAR cTemp AS CHAR NO-UNDO.
    IF pField NE "" THEN DO i = 1 to NUM-ENTRIES('{&SEL-IDX}'):
    cTemp = ENTRY(i,'{&SEL-IDX}').
    IF INDEX(cTemp,pField) NE 0 THEN DO: 
      iCurrentIndex = i.
      LEAVE.
    END.
  END.
END.

PROCEDURE select-proc :
 
    DEF VAR old-value# AS CHAR NO-UNDO.
    old-value# = fr-value .
    
    ASSIGN log1# = BROWSE-1:FETCH-SELECTED-ROW(1) IN FRAME {&FRAME-NAME} NO-ERROR.
      r-rowid = ROWID({&FILE}).
    FIND {&FILE} WHERE ROWID({&FILE}) = r-rowid NO-LOCK.

    
    CASE use-field#:
        &IF "{&K1}":U NE "":U &THEN WHEN '{&K1}':U THEN fr-value = STRING({&FILE}.{&K1}). &ENDIF
        &IF "{&K2}":U NE "":U &THEN WHEN '{&K2}':U THEN fr-value = STRING({&FILE}.{&K2}). &ENDIF
        &IF "{&K3}":U NE "":U &THEN WHEN '{&K3}':U THEN fr-value = STRING({&FILE}.{&K3}). &ENDIF
        &IF "{&K4}":U NE "":U &THEN WHEN '{&K4}':U THEN fr-value = STRING({&FILE}.{&K4}). &ENDIF
        &IF "{&K5}":U NE "":U &THEN WHEN '{&K5}':U THEN fr-value = STRING({&FILE}.{&K5}). &ENDIF
        &IF "{&K6}":U NE "":U &THEN WHEN '{&K6}':U THEN fr-value = STRING({&FILE}.{&K6}). &ENDIF
        &IF "{&K7}":U NE "":U &THEN WHEN '{&K7}':U THEN fr-value = STRING({&FILE}.{&K7}). &ENDIF
        &IF "{&K8}":U NE "":U &THEN WHEN '{&K8}':U THEN fr-value = STRING({&FILE}.{&K8}). &ENDIF
        &IF "{&K9}":U NE "":U &THEN WHEN '{&K9}':U THEN fr-value = STRING({&FILE}.{&K9}). &ENDIF
        &IF "{&K10}":U NE "":U &THEN WHEN '{&K10}':U THEN fr-value = STRING({&FILE}.{&K10}). &ENDIF
        &IF "{&K11}":U NE "":U &THEN WHEN '{&K11}':U THEN fr-value = STRING({&FILE}.{&K11}). &ENDIF
        &IF "{&K12}":U NE "":U &THEN WHEN '{&K12}':U THEN fr-value = STRING({&FILE}.{&K12}). &ENDIF
        &IF "{&K13}":U NE "":U &THEN WHEN '{&K13}':U THEN fr-value = STRING({&FILE}.{&K13}). &ENDIF
    END CASE.

     IF old-value# NE fr-value AND VALID-HANDLE(xfocus#)
         AND xfocus#:type = "fill-in":U
         THEN xfocus#:SCREEN-VALUE = fr-value.

END PROCEDURE.

PROCEDURE display-full :
 
  ASSIGN log1# = BROWSE-1:FETCH-SELECTED-ROW(1) IN FRAME {&FRAME-NAME} NO-ERROR.
      r-rowid = ROWID({&FILE}).
  FIND {&FILE} WHERE ROWID({&FILE}) = r-rowid NO-LOCK.
  DISPLAY {&FIELDS-IN-FULL-QUERY-BROWSE-1}
    WITH FRAME F-Full.
end procedure.



PROCEDURE find-frame.
 
DEFINE BUTTON Btn_fCancel AUTO-END-KEY 
     LABEL "Cancel" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 12 BY 1.13 &ENDIF
     BGCOLOR 8.

DEFINE BUTTON Btn_fHelp 
     LABEL "&Help" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 12 BY 1.13 &ENDIF
     BGCOLOR 8.

DEFINE BUTTON Btn_fOK AUTO-GO 
     LABEL "OK" 
     DEFAULT
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 12 BY 1.13 &ENDIF
     BGCOLOR 8.

DEFINE BUTTON Btn_fsug  
     LABEL "Idx Suggest" 
     DEFAULT
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 12 BY 1.13 &ENDIF
     BGCOLOR 8.
     
DEFINE VARIABLE SELECT-INDEX AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     LIST-ITEMS {&SEL-IDX}
    ""
    &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 29 BY 12
    &ELSE SIZE 40 by 11 &ENDIF.


DEFINE FRAME D-Find
    &IF "{&K1}" NE "" &THEN xx{&K1} {&KL1} AT ROW {&KR1} COL {&MAX-LABEL} COLON-ALIGN &ENDIF
    &IF "{&K2}" NE "" &THEN xx{&K2} {&KL2} AT ROW {&KR2} COL {&MAX-LABEL} COLON-ALIGN &ENDIF
    &IF "{&K3}" NE "" &THEN xx{&K3} {&KL3} AT ROW {&KR3} COL {&MAX-LABEL} COLON-ALIGN &ENDIF
    &IF "{&K4}" NE "" &THEN xx{&K4} {&KL4} AT ROW {&KR4} COL {&MAX-LABEL} COLON-ALIGN &ENDIF
    &IF "{&K5}" NE "" &THEN xx{&K5} {&KL5} AT ROW {&KR5} COL {&MAX-LABEL} COLON-ALIGN &ENDIF
    &IF "{&K6}" NE "" &THEN xx{&K6} {&KL6} AT ROW {&KR6} COL {&MAX-LABEL} COLON-ALIGN &ENDIF
    &IF "{&K7}" NE "" &THEN xx{&K7} {&KL7} AT ROW {&KR7} COL {&MAX-LABEL} COLON-ALIGN &ENDIF
    &IF "{&K8}" NE "" &THEN xx{&K8} {&KL8} AT ROW {&KR8} COL {&MAX-LABEL} COLON-ALIGN &ENDIF
    &IF "{&K9}" NE "" &THEN xx{&K9} {&KL9} AT ROW {&KR9} COL {&MAX-LABEL} COLON-ALIGN &ENDIF
    &IF "{&K10}" NE "" &THEN xx{&K10} {&KL10} AT ROW {&KR10} COL {&MAX-LABEL} COLON-ALIGN &ENDIF
    &IF "{&K11}" NE "" &THEN xx{&K11} {&KL11} AT ROW {&KR11} COL {&MAX-LABEL} COLON-ALIGN &ENDIF
    &IF "{&K12}" NE "" &THEN xx{&K12} {&KL12} AT ROW {&KR12} COL {&MAX-LABEL} COLON-ALIGN &ENDIF
    &IF "{&K13}" NE "" &THEN xx{&K13} {&KL13} AT ROW {&KR13} COL {&MAX-LABEL} COLON-ALIGN &ENDIF

     SELECT-INDEX
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 2 COL 50
          &ELSE AT ROW 1 COL 54 &ENDIF NO-LABEL
     Btn_fOK
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 1
          &ELSE AT ROW 12.5 COL 44 &ENDIF
     Btn_fCancel
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 15
          &ELSE AT ROW 12.5 COL 57 &ENDIF
     Btn_fsug
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 30
          &ELSE AT ROW 12.5 COL 70 &ENDIF
     Btn_fHelp
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 45
          &ELSE AT ROW 12.5 COL 83 &ENDIF
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         /* V6FRAME */
         TITLE "{&TITLE} Find"
         DEFAULT-BUTTON Btn_fOK CANCEL-BUTTON Btn_fCancel.

ON GO OF FRAME D-find DO:
  APPLY "CHOOSE" TO btn_fOK IN FRAME D-find.
END.
                 
ON ENTRY OF
    &IF "{&K1}":U NE "":U &THEN xx{&K1}, &ENDIF
    &IF "{&K2}":U NE "":U &THEN xx{&K2}, &ENDIF
    &IF "{&K3}":U NE "":U &THEN xx{&K3}, &ENDIF
    &IF "{&K4}":U NE "":U &THEN xx{&K4}, &ENDIF
    &IF "{&K5}":U NE "":U &THEN xx{&K5}, &ENDIF
    &IF "{&K6}":U NE "":U &THEN xx{&K6}, &ENDIF
    &IF "{&K7}":U NE "":U &THEN xx{&K7}, &ENDIF
    &IF "{&K8}":U NE "":U &THEN xx{&K8}, &ENDIF
    &IF "{&K9}":U NE "":U &THEN xx{&K9}, &ENDIF
    &IF "{&K10}":U NE "":U &THEN xx{&K10}, &ENDIF
    &IF "{&K11}":U NE "":U &THEN xx{&K11}, &ENDIF
    &IF "{&K12}":U NE "":U &THEN xx{&K12}, &ENDIF
   &IF "{&K13}":U NE "":U &THEN xx{&K13}, &ENDIF

    btn_fhelp /* dummy to make last comma work */
    IN FRAME D-find DO:
  IF SELF:TYPE NE "BUTTON" THEN c-self = SELF:SCREEN-VALUE.
END.

/*
ON LEAVE OF
    &IF "{&K1}":U NE "":U &THEN xx{&K1}, &ENDIF
    &IF "{&K2}":U NE "":U &THEN xx{&K2}, &ENDIF
    &IF "{&K3}":U NE "":U &THEN xx{&K3}, &ENDIF
    &IF "{&K4}":U NE "":U &THEN xx{&K4}, &ENDIF
    &IF "{&K5}":U NE "":U &THEN xx{&K5}, &ENDIF
    &IF "{&K6}":U NE "":U &THEN xx{&K6}, &ENDIF
    &IF "{&K7}":U NE "":U &THEN xx{&K7}, &ENDIF
    &IF "{&K8}":U NE "":U &THEN xx{&K8}, &ENDIF
    &IF "{&K9}":U NE "":U &THEN xx{&K9}, &ENDIF
    &IF "{&K10}":U NE "":U &THEN xx{&K10}, &ENDIF
    &IF "{&K11}":U NE "":U &THEN xx{&K11}, &ENDIF
    &IF "{&K12}":U NE "":U &THEN xx{&K12}, &ENDIF
    &IF "{&K13}":U NE "":U &THEN xx{&K13}, &ENDIF
    btn_fhelp /* dummy to make last comma work */
    IN FRAME d-find DO:
  IF SELF:TYPE NE "BUTTON"
      AND SELF:SCREEN-VALUE IN FRAME d-find NE c-self
      AND LAST-EVENT:WIDGET-ENTER NE select-index:HANDLE IN FRAME d-find
      AND SELF:SCREEN-VALUE NE ""
      THEN DO:
    RUN local-select-key IN THIS-PROCEDURE (SUBSTR(SELF:NAME,3)).
    select-index:SCREEN-VALUE = ENTRY(iCurrentIndex,select-index:LIST-ITEMS) .
  END.
END.
*/
ON CHOOSE OF Btn_fsug IN FRAME d-find DO:

  define variable vChild as character no-undo.
  DEFINE VARIABLE iParce AS INTEGER NO-UNDO.
  DEFINE VARIABLE vIndexWeight AS INTEGER NO-UNDO.
  DEFINE VARIABLE idxString AS CHAR NO-UNDO.
  DEFINE VARIABLE vIndexFieldList AS CHAR NO-UNDO.
  DEFINE VARIABLE vfindList AS CHAR NO-UNDO.
  DEFINE VARIABLE vfindValue AS CHAR NO-UNDO.

  assign vChild    = "{&FILE}"
         idxString = &IF "{&INDEX1}" NE "":U &THEN "{&INDEX1}" &ELSE "" &ENDIF + "," + 
                     &IF "{&INDEX2}" NE "":U &THEN "{&INDEX2}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX3}" NE "":U &THEN "{&INDEX3}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX4}" NE "":U &THEN "{&INDEX4}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX5}" NE "":U &THEN "{&INDEX5}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX6}" NE "":U &THEN "{&INDEX6}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX7}" NE "":U &THEN "{&INDEX7}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX8}" NE "":U &THEN "{&INDEX8}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX9}" NE "":U &THEN "{&INDEX9}" &ELSE "" &ENDIF + "," +
                     &IF "{&INDEX10}" NE "":U &THEN "{&INDEX10}" &ELSE "" &ENDIF
         vfindList = &IF "{&K1}":U NE "":U &THEN "{&K1}":U &ENDIF
                     &IF "{&K2}":U NE "":U &THEN + "," + "{&K2}":U &ENDIF
                     &IF "{&K3}":U NE "":U &THEN + "," + "{&K3}":U &ENDIF
                     &IF "{&K4}":U NE "":U &THEN + "," + "{&K4}":U &ENDIF
                     &IF "{&K5}":U NE "":U &THEN + "," + "{&K5}":U &ENDIF
                     &IF "{&K6}":U NE "":U &THEN + "," + "{&K6}":U &ENDIF
                     &IF "{&K7}":U NE "":U &THEN + "," + "{&K7}":U &ENDIF
                     &IF "{&K8}":U NE "":U &THEN + "," + "{&K8}":U &ENDIF
                     &IF "{&K9}":U NE "":U &THEN + "," + "{&K9}":U &ENDIF
                     &IF "{&K10}":U NE "":U &THEN + "," + "{&K10}":U &ENDIF
                     &IF "{&K11}":U NE "":U &THEN + "," + "{&K11}":U &ENDIF
                     &IF "{&K12}":U NE "":U &THEN + "," + "{&K12}":U &ENDIF
                     &IF "{&K13}":U NE "":U &THEN + "," + "{&K13}":U &ENDIF.

        vfindValue = &IF "{&K1}":U NE "":U &THEN xx{&K1}:SCREEN-VALUE IN FRAME D-Find &ENDIF
                     &IF "{&K2}":U NE "":U &THEN + "," + xx{&K2}:SCREEN-VALUE IN FRAME D-Find &ENDIF
                     &IF "{&K3}":U NE "":U &THEN + "," + xx{&K3}:SCREEN-VALUE IN FRAME D-Find &ENDIF
                     &IF "{&K4}":U NE "":U &THEN + "," + xx{&K4}:SCREEN-VALUE IN FRAME D-Find &ENDIF
                     &IF "{&K5}":U NE "":U &THEN + "," + xx{&K5}:SCREEN-VALUE IN FRAME D-Find &ENDIF
                     &IF "{&K6}":U NE "":U &THEN + "," + xx{&K6}:SCREEN-VALUE IN FRAME D-Find &ENDIF
                     &IF "{&K7}":U NE "":U &THEN + "," + xx{&K7}:SCREEN-VALUE IN FRAME D-Find &ENDIF
                     &IF "{&K8}":U NE "":U &THEN + "," + xx{&K8}:SCREEN-VALUE IN FRAME D-Find &ENDIF
                     &IF "{&K9}":U NE "":U &THEN + "," + xx{&K9}:SCREEN-VALUE IN FRAME D-Find &ENDIF
                     &IF "{&K10}":U NE "":U &THEN + "," + xx{&K10}:SCREEN-VALUE IN FRAME D-Find &ENDIF
                     &IF "{&K11}":U NE "":U &THEN + "," + xx{&K11}:SCREEN-VALUE IN FRAME D-Find &ENDIF
                     &IF "{&K12}":U NE "":U &THEN + "," + xx{&K12}:SCREEN-VALUE IN FRAME D-Find &ENDIF
                     &IF "{&K13}":U NE "":U &THEN + "," + xx{&K13}:SCREEN-VALUE IN FRAME D-Find &ENDIF.


  FOR EACH fndBestIndex:
    DELETE fndBestIndex.
  END.

  FIND ptdb9._file WHERE ptdb9._file._file-name = vChild no-lock no-error.
  IF NOT AVAILABLE ptdb9._file THEN RETURN.
  FOR EACH ptdb9._index OF ptdb9._file no-lock:
    ASSIGN vIndexFieldList = ""
           vIndexWeight = 0.
    FOR EACH ptdb9._index-field OF ptdb9._index no-lock:
      FIND ptdb9._field OF ptdb9._Index-field NO-LOCK NO-ERROR.
      IF AVAILABLE ptdb9._field THEN 
          ASSIGN vIndexFieldList = 
            IF vIndexFieldList = "" THEN ptdb9._field._field-name 
            ELSE vIndexFieldList + "," + ptdb9._field._field-name.
    END.
    IF NUM-ENTRIES(vIndexFieldList) > 0 THEN DO iParce = 1 TO NUM-ENTRIES(vIndexFieldList):
        IF lookup(entry(iParce,vIndexFieldList),vfindList) > 0 THEN DO:
          IF ENTRY(lookup(entry(iParce,vIndexFieldList),vfindList),vfindValue) <> "" 
             THEN ASSIGN vIndexWeight = vIndexWeight + (1000000 / (iParce * 10)).
        END.
    END.
    CREATE fndBestIndex.
    ASSIGN tIdxName = ptdb9._index._index-name
           tWeight = vIndexWeight.
  END.

  FIND LAST fndBestIndex WHERE fndBestIndex.tWeight > 0 no-error.
  if available fndBestIndex then assign iCurrentIndex = lookup(fndBestIndex.tIdxName,idxString).
  else assign iCurrentIndex = 1.
  if iCurrentIndex = 0 then assign iCurrentIndex = 1.

  FOR EACH fndBestIndex:
    DELETE fndBestIndex.
  END.
  select-index:SCREEN-VALUE = ENTRY(iCurrentIndex,select-index:LIST-ITEMS) .


END.
     
ON CHOOSE OF btn_fok IN FRAME d-find DO.
   ASSIGN
        &IF "{&K1}":U NE "":U &THEN xx{&K1} &ENDIF
        &IF "{&K2}":U NE "":U &THEN xx{&K2} &ENDIF
        &IF "{&K3}":U NE "":U &THEN xx{&K3} &ENDIF
        &IF "{&K4}":U NE "":U &THEN xx{&K4} &ENDIF
        &IF "{&K5}":U NE "":U &THEN xx{&K5} &ENDIF
        &IF "{&K6}":U NE "":U &THEN xx{&K6} &ENDIF
        &IF "{&K7}":U NE "":U &THEN xx{&K7} &ENDIF
        &IF "{&K8}":U NE "":U &THEN xx{&K8} &ENDIF
        &IF "{&K9}":U NE "":U &THEN xx{&K9} &ENDIF
        &IF "{&K10}":U NE "":U &THEN xx{&K10} &ENDIF
        &IF "{&K11}":U NE "":U &THEN xx{&K11} &ENDIF
        &IF "{&K12}":U NE "":U &THEN xx{&K12} &ENDIF
       &IF "{&K13}":U NE "":U &THEN xx{&K13} &ENDIF
        .
    l-matches = FALSE.
    IF
        &IF "{&KT1}":U EQ "CH":U &THEN (INDEX(xx{&K1},"*") NE 0) OR &ENDIF
        &IF "{&KT2}":U EQ "CH":U &THEN (INDEX(xx{&K2},"*") NE 0) OR &ENDIF
        &IF "{&KT3}":U EQ "CH":U &THEN (INDEX(xx{&K3},"*") NE 0) OR &ENDIF
        &IF "{&KT4}":U EQ "CH":U &THEN (INDEX(xx{&K4},"*") NE 0) OR &ENDIF
        &IF "{&KT5}":U EQ "CH":U &THEN (INDEX(xx{&K5},"*") NE 0) OR &ENDIF
        &IF "{&KT6}":U EQ "CH":U &THEN (INDEX(xx{&K6},"*") NE 0) OR &ENDIF
        &IF "{&KT7}":U EQ "CH":U &THEN (INDEX(xx{&K7},"*") NE 0) OR &ENDIF
        &IF "{&KT8}":U EQ "CH":U &THEN (INDEX(xx{&K8},"*") NE 0) OR &ENDIF
        &IF "{&KT9}":U EQ "CH":U &THEN (INDEX(xx{&K9},"*") NE 0) OR &ENDIF
        &IF "{&KT10}":U EQ "CH":U &THEN (INDEX(xx{&K10},"*") NE 0) OR &ENDIF
        &IF "{&KT11}":U EQ "CH":U &THEN (INDEX(xx{&K11},"*") NE 0) OR &ENDIF
        &IF "{&KT12}":U EQ "CH":U &THEN (INDEX(xx{&K12},"*") NE 0) OR &ENDIF
        &IF "{&KT13}":U EQ "CH":U &THEN (INDEX(xx{&K13},"*") NE 0) OR &ENDIF
        1 = 2 THEN l-matches = TRUE.
    IF l-matches THEN DO:
    
      log1# = FALSE.
      MESSAGE "You have entered find parameters" skip
          "that will cause a sequential search." skip (1)
          "Do you wish to continue:"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
          TITLE "" UPDATE log1#.
      IF log1# = FALSE THEN RETURN NO-APPLY.
    END.

    RUN local-open-query IN THIS-PROCEDURE.
    IF NOT AVAIL {&FILE} THEN DO:
      message "Record not found. Try again." view-as alert-box error.
      RETURN NO-APPLY.
    END.
    ELSE APPLY "END-ERROR" TO FRAME D-Find.
END.

ON VALUE-CHANGED OF SELECT-INDEX IN FRAME D-Find DO:
 
  iCurrentIndex = LOOKUP(select-index:SCREEN-VALUE, select-index:LIST-ITEMS).
END.

ON CHOOSE OF Btn_fHelp IN FRAME D-Find
         OR GO OF btn_fhelp IN FRAME D-Find
DO:
       RUN pt/ubrhelp.p ( "find":U ).
END.


DISPLAY
        &IF "{&K1}":U NE "":U &THEN xx{&K1} &ENDIF
        &IF "{&K2}":U NE "":U &THEN xx{&K2} &ENDIF
        &IF "{&K3}":U NE "":U &THEN xx{&K3} &ENDIF
        &IF "{&K4}":U NE "":U &THEN xx{&K4} &ENDIF
        &IF "{&K5}":U NE "":U &THEN xx{&K5} &ENDIF
        &IF "{&K6}":U NE "":U &THEN xx{&K6} &ENDIF
        &IF "{&K7}":U NE "":U &THEN xx{&K7} &ENDIF
        &IF "{&K8}":U NE "":U &THEN xx{&K8} &ENDIF
        &IF "{&K9}":U NE "":U &THEN xx{&K9} &ENDIF
        &IF "{&K10}":U NE "":U &THEN xx{&K10} &ENDIF
        &IF "{&K11}":U NE "":U &THEN xx{&K11} &ENDIF
        &IF "{&K12}":U NE "":U &THEN xx{&K12} &ENDIF
       &IF "{&K13}":U NE "":U &THEN xx{&K13} &ENDIF
        WITH FRAME d-Find.
   
select-index:SCREEN-VALUE = ENTRY(iCurrentIndex,select-index:LIST-ITEMS) .
ENABLE ALL WITH FRAME D-FIND.
VIEW FRAME D-FIND.

WAIT-FOR WINDOW-CLOSE OF FRAME D-FIND.


END PROCEDURE.

PROCEDURE no-default-action:

    v-default-action = NO.
 
END PROCEDURE.

PROCEDURE full-frame.
 
DEFINE BUTTON Btn_FCancel AUTO-END-KEY 
     LABEL "Cancel" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1.13 &ENDIF
     DEFAULT.

DEFINE BUTTON Btn_Fnext 
     LABEL "Next" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1.13 &ENDIF.

DEFINE BUTTON Btn_Fprev 
     LABEL "Previous" 
     &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 10 BY 1
     &ELSE SIZE 10 BY 1.13 &ENDIF.


DEFINE FRAME D-Full
     Btn_Fprev
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 16
          &ELSE AT ROW 20.2 COL 16 &ENDIF
     Btn_FCancel
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 41
          &ELSE AT ROW 20.2 COL 41 &ENDIF
     Btn_Fnext
          &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN AT ROW 18 COL 66
          &ELSE AT ROW 20.2 COL 66 &ENDIF
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN SIZE 80 BY 20 &ELSE SIZE 129 BY 23 &ENDIF
         TITLE "{&TITLE} Full Record"
         DEFAULT-BUTTON Btn_FCancel CANCEL-BUTTON Btn_FCancel.

DEFINE FRAME F-Full
    {&FIELDS-IN-FULL-QUERY-BROWSE-1}
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 3 COLUMNS 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 NO-BOX
         &IF '{&WINDOW-SYSTEM}' = 'TTY':U &THEN WIDTH 78 &ELSE WIDTH 128 &ENDIF
         /* V6FRAME */.

FRAME F-Full:FRAME = FRAME D-Full:HANDLE.   
FRAME F-Full:SCROLLABLE = TRUE.
FRAME F-Full:VIRTUAL-HEIGHT = 300. 
FRAME F-Full:HEIGHT = &IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN 19 &ELSE 16 &ENDIF.

ON CHOOSE OF Btn_FNext IN FRAME d-Full DO:
  log1# = browse-1:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  IF AVAIL {&FILE} THEN DO:
    r-rowid = ROWID({&FILE}).
    FIND {&FILE} WHERE ROWID({&FILE}) = r-rowid NO-LOCK.
    DISPLAY {&FIELDS-IN-FULL-QUERY-BROWSE-1} WITH FRAME F-Full.
  END.
END.

ON CHOOSE OF Btn_FPrev IN FRAME d-Full DO:
  log1# = browse-1:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  IF AVAIL {&FILE} THEN DO:
    r-rowid = ROWID({&FILE}).
    FIND {&FILE} WHERE ROWID({&FILE}) = r-rowid NO-LOCK.
    DISPLAY {&FIELDS-IN-FULL-QUERY-BROWSE-1} WITH FRAME F-Full.
  END.
END.

VIEW FRAME d-Full.

ENABLE ALL WITH FRAME d-Full.

&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN 
ENABLE ALL WITH FRAME f-Full NO-VALIDATE. 

ON TAB OF Btn_FNext IN FRAME d-Full DO:
  APPLY "ENTRY" TO btn_fprev IN FRAME D-Full.
  RETURN NO-APPLY.
END.

ON TAB OF FRAME F-Full ANYWHERE DO:
  APPLY "ENTRY" TO btn_fcancel IN FRAME D-Full.
  RETURN NO-APPLY.
END.
&ENDIF

ASSIGN log1# = BROWSE-1:FETCH-SELECTED-ROW(1) IN FRAME {&FRAME-NAME} NO-ERROR.
r-rowid = ROWID({&FILE}).
FIND {&FILE} WHERE ROWID({&FILE}) = r-rowid NO-LOCK NO-ERROR.
DISPLAY {&FIELDS-IN-FULL-QUERY-BROWSE-1} WITH FRAME F-Full.

APPLY "ENTRY" TO Btn_FCancel IN FRAME D-Full.

WAIT-FOR WINDOW-CLOSE OF FRAME d-full.


END PROCEDURE.

FUNCTION addDN RETURNS LOGICAL
  (INPUT cf1 as Character, INPUT cf2 AS CHARACTER) :
  
  FIND dn WHERE dn.cField1 = cf1 AND
                dn.cField2 = cf2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE dn THEN DO:
    CREATE dn.
    ASSIGN dn.cField1 = cf1
           dn.cField2 = cf2.
    RETURN TRUE.   /* Function return value. */
  END.
  ELSE RETURN FALSE.
  
END FUNCTION.

FUNCTION parentchildIndex RETURNS LOGICAL
  (INPUT cf1 as Character, INPUT cf2 AS CHARACTER, INPUT cf3 AS CHARACTER) :
  
  FIND pci WHERE pci.pciParent = cf1 AND
                 pci.pciChild  = cf2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE pci THEN DO:
    CREATE pci.
    ASSIGN pci.pciParent = cf1 
           pci.pciChild  = cf2
           pci.pciIndex  = cf3.
    RETURN TRUE.   /* Function return value. */
  END.
  ELSE RETURN FALSE.
  
END FUNCTION.

FUNCTION getNormalField RETURNS CHARACTER 
  (INPUT cf1 as Character, INPUT cf2 AS CHARACTER):
   FIND FIRST dn WHERE dn.cField1 = cf1 AND
                       dn.cField2 BEGINS STRING(cf2 + ".") NO-LOCK NO-ERROR.
   IF AVAILABLE dn THEN RETURN ENTRY(2,dn.cField2,".").
   FIND FIRST dn WHERE dn.cField2 = cf1 AND
                       dn.cField1 BEGINS STRING(cf2 + ".") NO-LOCK NO-ERROR.
   IF AVAILABLE dn THEN RETURN ENTRY(2,dn.cField1,".").
   RETURN ENTRY(2,cf1,".").
END FUNCTION.

FUNCTION rmDN RETURNS LOGICAL
  (INPUT cf1 as Character, INPUT cf2 AS CHARACTER) :
  
  FIND undn WHERE undn.rField1 = cf1 AND
                  undn.rField2 = cf2 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE undn THEN DO:
    CREATE undn.
    ASSIGN undn.rField1 = cf1
           undn.rField2 = cf2.
    RETURN TRUE.   /* Function return value. */
  END.
  ELSE RETURN FALSE.
  
END FUNCTION.

FUNCTION fixcondition RETURNS LOGICAL 
     (INPUT cf1 as Character, INPUT cf2 AS CHARACTER):
   FIND FIRST undn WHERE undn.rField1 = cf1 AND
                         undn.rField2 BEGINS STRING(cf2 + ".") NO-LOCK NO-ERROR.
   IF AVAILABLE undn THEN DO:
      IF LOOKUP(ENTRY(2,cf1,"."),condition) > 0 THEN
         ASSIGN init-val[LOOKUP(ENTRY(2,cf1,"."),condition) + 1] = "".
   END.
   ELSE DO:
     FIND FIRST undn WHERE undn.rField2 = cf1 AND
                           undn.rField1 BEGINS STRING(cf2 + ".") NO-LOCK NO-ERROR.
     IF AVAILABLE undn THEN DO:
        IF LOOKUP(ENTRY(2,cf1,"."),condition) > 0 THEN
           ASSIGN init-val[LOOKUP(ENTRY(2,cf1,"."),condition) + 1] = "".
     END.
   END.
   RETURN TRUE.

END FUNCTION.



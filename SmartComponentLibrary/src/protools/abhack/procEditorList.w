&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win
/*------------------------------------------------------------------------

  File: procEditorList by Sébastien Lacroix Nov 2006

  Description: popup selection list in a static frame to make appear
   in a procedure editor window.

  Input Parameters: handle of the target procedure editor widget itself
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

/* Parameters Definitions --- */
DEFINE INPUT PARAMETER iphEditor AS HANDLE      NO-UNDO.

/* Pseudo Parameters Definitions --- (Set by a first call to LoadXXX) */
DEFINE VARIABLE ipcInitBufferList   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ipcInitBufferPrefix AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iphBuffer           AS HANDLE     NO-UNDO.
DEFINE VARIABLE ipcOptn             AS CHARACTER  NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE gcActionOpenFile               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcObjectTypeFilter             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcRunningMode                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ghCaller                       AS HANDLE      NO-UNDO. /* we will use it for various purposes like getPrevWord */.
/* Dummy code: comment trick to let ahback suggest internal procedures of abhackwin.w in the ghCaller handle
RUN protools/abhack/ABHackWin.w PERSISTENT set ghCaller  */

DEFINE VARIABLE ghEditorWin                    AS HANDLE    NO-UNDO.
DEFINE VARIABLE giAutoCompMinSize              AS INTEGER   NO-UNDO.
DEFINE VARIABLE glABLAttributesAndMethods      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glBellowCarret                 AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glConvertDbTablesToLC          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glEnableUIRun                  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glExitWhenEmpty                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glExternalProc                 AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glgoAfterDynFuncOnChoose       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glGotAHypeStrike               AS LOGICAL   NO-UNDO. /* 15-NOV-2007 sla: set when we push completion on space */.
DEFINE VARIABLE glHypeStrikeMode               AS LOGICAL   NO-UNDO. /* takes value of abhackwin.w's' glHypeStrikeMode */.
DEFINE VARIABLE glInsertInMultipleLines        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glInsertMultipleLinesWithEqual AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glInsertYourOwnParamIfMethod   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glInsertYourOwnParamIfFunc     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glInsertYourOwnParamIfProc     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glinsSpaceAfterPrevRunOnChoose AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glLeaveCurlyBracket            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glSortedByName                 AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gltrailingParenthMeansFunction AS LOGICAL   NO-UNDO.


DEFINE TEMP-TABLE ttItem NO-UNDO LABEL "ttItem (to feed the list on a given order)"
 FIELD iOrder AS INTEGER
 FIELD cName  AS CHARACTER
 INDEX iOrder iOrder
 INDEX cName cName.

DEFINE TEMP-TABLE ttAttr NO-UNDO LABEL "ttAttr (for completion of attributes and methods on TAB Key)"
 FIELD cAttr       AS CHARACTER
 FIELD cObjTypes   AS CHARACTER   /* list of Object/widget types for a given attribute/method */
 FIELD cSortOption AS CHARACTER
 INDEX cAttr cAttr
 INDEX cSortOptionItem IS PRIMARY cSortOption cAttr
 INDEX cObjTypes IS WORD-INDEX cObjTypes.


PROCEDURE GetCaretPos EXTERNAL "user32.dll":
    DEFINE INPUT  PARAMETER HWND AS MEMPTR.
    DEFINE RETURN PARAMETER lOK  AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fEditorPopupList

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS seList
&Scoped-Define DISPLAYED-OBJECTS seList

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dataType C-Win
FUNCTION dataType RETURNS CHARACTER
  (hField AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD targetEditorHandle C-Win
FUNCTION targetEditorHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU PMSeList
       MENU-ITEM mAddTableName  LABEL "Field Multiple Selection: Add <tableName.>"
              DISABLED TOGGLE-BOX
       MENU-ITEM m_Filter_on_type LABEL "Filter on type"
              DISABLED TOGGLE-BOX
       MENU-ITEM m_Insert_items_in_multiple_li LABEL "Insert items in multiple lines"
              DISABLED
       MENU-ITEM m_InsertInMultiWithEqual LABEL "Insert items in multiple lines with '='"
              DISABLED.


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrameEntryToEditor AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrameEntryToEditor AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE seList AS CHARACTER
     CONTEXT-HELP-ID 0
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL
     SIZE 29 BY 9.05
     FGCOLOR 4  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fEditorPopupList
     seList AT ROW 1 COL 1 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 29.2 BY 9.1
         FONT 4.


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
/* SUPPRESS Window definition (used by the UIB)
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "simple frame no title"
         HEIGHT             = 9.1
         WIDTH              = 28.4
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME fEditorPopupList
   FRAME-NAME                                                           */
ASSIGN
       seList:POPUP-MENU IN FRAME fEditorPopupList       = MENU PMSeList:HANDLE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME




/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME fEditorPopupList:HANDLE
       ROW             = 1.71
       COLUMN          = 2
       HEIGHT          = 1.43
       WIDTH           = 6
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrameEntryToEditor ASSIGN
       FRAME           = FRAME fEditorPopupList:HANDLE
       ROW             = 1.71
       COLUMN          = 10
       HEIGHT          = 1.43
       WIDTH           = 6
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimerMoveToTop */
/* CtrlFrameEntryToEditor OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimerEntryEditor */
      CtrlFrame:MOVE-AFTER(seList:HANDLE IN FRAME fEditorPopupList).
      CtrlFrameEntryToEditor:MOVE-AFTER(CtrlFrame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* simple frame no title */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* simple frame no title */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimerMoveToTop.Tick .
/*------------------------------------------------------------------------------
  Purpose:     This trick is necessary to put the frame back to top after a few
               milliseconds
               Indeed, in some cases, when the frame is just realized, it
               goes to bottom when the flow of the program goes back to ade
               procedures, or when the focus goes back to the editor

  Parameters:  None required for OCX.
  Notes:
------------------------------------------------------------------------------*/

chCtrlFrame:PSTimerMoveToTop:enabled = NO.
FRAME fEditorPopupList:MOVE-TO-TOP().
iphEditor:FRAME:MOVE-TO-BOTTOM().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrameEntryToEditor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrameEntryToEditor C-Win OCX.Tick
PROCEDURE CtrlFrameEntryToEditor.PSTimerEntryEditor.Tick .
/*------------------------------------------------------------------------------
  Purpose:     This trick is necessary to put the frame back to top after a few
               milliseconds
               Indeed, in some cases, when the frame is just realized, it
               goes to bottom when the flow of the program goes back to ade
               procedures, or when the focus goes back to the editor

  /* 25-MAY-2007 sla: changed the interval from 150 to 20 as suggested by Dries Feys */

  Parameters:  None required for OCX.
  Notes:
------------------------------------------------------------------------------*/
chCtrlFrameEntryToEditor:PSTimerEntryEditor:enabled = NO.
APPLY 'ENTRY' TO iphEditor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Filter_on_type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Filter_on_type C-Win
ON VALUE-CHANGED OF MENU-ITEM m_Filter_on_type /* Filter on type */
DO:
    /* 09-JAN-2007 sla: now update ipcInitBufferList so the filtering can be kept
     disabled while typing to narrow down the list */
    RUN loadListFromAttr (OUTPUT ipcInitBufferList).
    seList:LIST-ITEMS IN FRAME {&FRAME-NAME} = ipcInitBufferList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_InsertInMultiWithEqual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_InsertInMultiWithEqual C-Win
ON CHOOSE OF MENU-ITEM m_InsertInMultiWithEqual /* Insert items in multiple lines with '=' */
DO:
    glInsertMultipleLinesWithEqual = YES.
    APPLY 'choose' TO MENU-ITEM m_Insert_items_in_multiple_li IN MENU PMSeList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Insert_items_in_multiple_li
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Insert_items_in_multiple_li C-Win
ON CHOOSE OF MENU-ITEM m_Insert_items_in_multiple_li /* Insert items in multiple lines */
DO:
    glInsertInMultipleLines = YES.
    APPLY "RETURN" TO seList IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME seList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL seList C-Win
ON END-ERROR OF seList IN FRAME fEditorPopupList
DO:
    APPLY 'ENTRY' TO iphEditor.
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL seList C-Win
ON LEFT-MOUSE-DBLCLICK OF seList IN FRAME fEditorPopupList
DO:
  APPLY 'RETURN' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL seList C-Win
ON RETURN OF seList IN FRAME fEditorPopupList
DO:
DEFINE VARIABLE cCaseSensistiveValue       AS CHARACTER   CASE-SENSITIVE NO-UNDO.
DEFINE VARIABLE cItem                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cParamToInsert             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrevWord                  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cProcFileNameWithExtention AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTableName                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCursorChar                AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNextBreak                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE iVSlickWordsToDelete       AS INTEGER     NO-UNDO. /* '_' are taken as word separators, which is not the case of A4GBL */.
DEFINE VARIABLE lByInTheLine               AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lCloseThisProcedure        AS LOGICAL     INITIAL YES NO-UNDO.
DEFINE VARIABLE lClosingParentheseDirective AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lPutCursorDirective        AS LOGICAL     NO-UNDO.

cItem = seList:SCREEN-VALUE.
/* 15-FEB-2007 sla: now supporting "(datatype)" in field list
so I have to remove them... */
IF gcRunningMode = "fieldList" THEN DO:
    DEFINE VARIABLE iItem AS INTEGER     NO-UNDO.
    DO iItem = 1 TO NUM-ENTRIES(cItem, seList:DELIMITER):
        ENTRY(iItem, cItem, seList:DELIMITER) = ENTRY(1, ENTRY(iItem, cItem, seList:DELIMITER), " ").
    END.
END.

/* 08-JAN-2007 sla: New Open selected file action */
IF gcActionOpenFile > "" THEN DO:
    RUN openFile IN ghCaller (gcActionOpenFile).
    APPLY 'CLOSE' TO THIS-PROCEDURE.
    RETURN.
END.

/* 26-AUG-2007 sla: The point of this line was to avoid the insertion of .<FileExtension> */
IF gcRunningMode = "ProcedureMode" THEN ASSIGN
 cProcFileNameWithExtention = cItem
 cItem                      = ENTRY(1, cItem, ".").


IF INDEX(cItem, "%\(") > 0 THEN ASSIGN
 lClosingParentheseDirective = YES
 cItem = REPLACE(cItem, "%\(", "(").

iCursorChar = iphEditor:CURSOR-CHAR.

IF NOT VALID-HANDLE(ghCaller) THEN DO:
    APPLY 'CLOSE' TO THIS-PROCEDURE.
    RETURN.
END.

/* The point of the following code is to remove temp stuff that was typed to narrow down the list */
RUN getEditorLine IN ghCaller (iphEditor, OUTPUT cLine).
lByInTheLine = INDEX(" " + cLine, " BY ") > 0 AND INDEX(" " + cLine, " BY ") < iCursorChar. /* 16-JUN-2008 sla: support insertion of multiple BY when inserting multiple fields */

/* "allowEmptyWord"  => so the resulting word can be empty */
RUN extractWordN  IN ghCaller (0, cLine, iCursorChar, "allowEmptyWord", OUTPUT cPrevWord).

/* 15-DEC-2006 sla: if the carret has been move away after a space, then it means the
 developper is actually not interested in the item, so kill this and apply the RETURN KEY
  19-DEC-2006 sla: refined it for the case after the WHERE keyword when in tableList mode  */
IF   LAST-EVENT:FUNCTION = "RETURN"
 AND (      iphEditor:CURSOR-CHAR > 1
        AND SUBSTRING(cLine, iphEditor:CURSOR-CHAR - 1, 1) = " "
        AND cPrevWord = "WHERE"
        AND gcRunningMode = "TableList"
      OR    (TRIM(cItem) = "" OR cItem = ?)
        AND SUBSTRING(cline, 1, iCursorChar) MATCHES "*:") /* interesting when list popuped up again with table-names that begins with a table we just choosed, we move cursor to end and we want to insert a CR */
 THEN DO:
    PUBLISH "DelayWindowEvent" (iphEditor, "CarriageReturn").
    APPLY "ENTRY" TO iphEditor. /* very important, otherwise the focus might end up at stange places... */
    APPLY 'CLOSE' TO THIS-PROCEDURE.
    RETURN.
END.


IF gcRunningMode = "fieldList"
 AND MENU-ITEM MAddTableName:CHECKED IN MENU PMSeList THEN DO:
    IF cPrevWord = "" THEN RUN extractWordN  IN ghCaller (0, cLine, iCursorChar, "", OUTPUT cTableName).
    ELSE RUN extractWordN  IN ghCaller (-1, cLine, iCursorChar, "", OUTPUT cTableName).
    cItem = REPLACE(cItem, seList:DELIMITER, seList:DELIMITER + cTableName + ".").
END.
IF CAN-DO(ipcOptn, "multipleSelection") THEN DO:
    IF glInsertInMultipleLines THEN DO:
        DEFINE VARIABLE cMultiLineSpace AS CHARACTER   NO-UNDO.
        iNextBreak = 0.
        iNextBreak = MAXIMUM(R-INDEX(cLine, " ", iCursorChar), iNextBreak).
        iNextBreak = MAXIMUM(R-INDEX(cLine, ",", iCursorChar), iNextBreak).
        iNextBreak = MAXIMUM(R-INDEX(cLine, "(", iCursorChar), iNextBreak).
        IF iNextBreak > 0 THEN cMultiLineSpace = FILL(" ", iNextBreak).

        IF glInsertMultipleLinesWithEqual
         THEN cItem = REPLACE(cItem , seList:DELIMITER, " =~n" + cMultiLineSpace) + " = ".
        ELSE DO:
            IF lByInTheLine THEN DO: /* 16-JUN-2008 sla: support of instertion of multiple 'BY' */
                cMultiLineSpace = SUBSTRING(cMultiLineSpace, 4) + "BY ".
                cItem = REPLACE(cItem , seList:DELIMITER, "~n" + cMultiLineSpace).
            END.
            ELSE cItem = REPLACE(cItem , seList:DELIMITER, "~n" + cMultiLineSpace).
        END.
    END.
    ELSE DO:
        IF lByInTheLine /* 16-JUN-2008 sla: support of instertion of multiple 'BY' */
         THEN cItem = REPLACE(cItem , seList:DELIMITER, " BY ").
        ELSE cItem = REPLACE(cItem , seList:DELIMITER, " ").
    END.
END.


IF cPrevWord > ""
  /* 24-AUG-2007 sla: added the following condition to not remove the previous word before " NEW <classVar>" or " CAST(%\c, <classVar>)" */
 AND NOT (gcRunningMode = "alias" AND cItem BEGINS " ")
 THEN DO:
    /* 08-JAN-2007 sla: implementation of closing directive, do not remove stuff on right of carret */
    IF lClosingParentheseDirective THEN iphEditor:INSERT-STRING(" ").

    iNextBreak = 32000.
    cLine = cLine + " YOYO". /* Make sure there is a word after the pointer */
    IF INDEX(cLine, " ", iCursorChar) > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(cLine, " ", iCursorChar)).
    IF INDEX(cLine, ",", iCursorChar) > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(cLine, ",", iCursorChar)).
    IF INDEX(cLine, "'", iCursorChar) > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(cLine, "'", iCursorChar)).
    IF INDEX(cLine, '"', iCursorChar) > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(cLine, '"', iCursorChar)).
    IF INDEX(cLine, ".", iCursorChar) > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(cLine, ".", iCursorChar)).
    IF INDEX(cLine, ":", iCursorChar) > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(cLine, ":", iCursorChar)).
    IF INDEX(cLine, ")", iCursorChar) > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(cLine, ")", iCursorChar)).
    IF INDEX(cLine, "(", iCursorChar) > 0 THEN iNextBreak = MINIMUM(iNextBreak, INDEX(cLine, "(", iCursorChar)).

    /* First move the cursor at the end of the current word */
    DEFINE VARIABLE iStopDelAt AS INTEGER    NO-UNDO.

    /* 08-JAN-2007 sla: implementation of closing directive, do not remove stuff on right of carret */
    IF NOT (lClosingParentheseDirective OR gcRunningMode = "preprocessor") THEN iphEditor:CURSOR-CHAR = iNextBreak.

    /* 14-DEC-2006 sla: Problems when the word to delete begins with a digit or any CHARACTER not considered as part of word by vslick
    Solution is to determine the cursor position where we want to stop the deletion, then delete then number of words
    we guessed, then complete the deletion if required, until we reach this position
    This is going to simplify the code regarding exceptions when a word begins with '_' or other guys */
    iStopDelAt = iNextBreak - LENGTH(cPrevWord).

    /* 28-NOV-2006 sla: special case for meta schema table that begin with '_' */
    iVSlickWordsToDelete = NUM-ENTRIES(LEFT-TRIM(cPrevWord, "_"), "_"). /* VSlick takes '_' as word breaks */
    /* 08-DEC-2006 sla: handle '*' like '_' so we could narrow the list with the powwer of matches */
    iVSlickWordsToDelete = iVSlickWordsToDelete + NUM-ENTRIES(RIGHT-TRIM(cPrevWord, "*"), "*") - 1.

    /* 10-JAN-2007 sla: support OS file popup for .i include file without removing leading '{' */
    IF   glLeaveCurlyBracket
     AND cPrevWord BEGINS "~{"
      THEN ASSIGN
       iVSlickWordsToDelete = 0 /* in this case we will delete char by char */
       iStopDelAt = iStopDelAt + 1.

   /* 13-JAN-2008 sla: support of preprocessor completion => don't remove leading '{&' */
   IF gcRunningMode = "preprocessor"
    AND cPrevWord BEGINS "~{&"
     THEN ASSIGN
       iVSlickWordsToDelete = 0 /* in this case we will delete char by char */
       iStopDelAt = iStopDelAt + 2.


    DO iVSlickWordsToDelete = iVSlickWordsToDelete TO 1 BY -1:
        /* 27-JAN-2007 sla: Was reported one case where the delete-prev-word command would consider '_' as part of word ...
          anywhay this extra test on CURSOR-CHAR will not hurt */
        IF iphEditor:CURSOR-CHAR <= iStopDelAt THEN LEAVE.

        iphEditor:SOURCE-COMMAND('delete-prev-word','').
    END.

    /* 14-DEC-2006 sla: now complete the deletion as explained above */
    DO WHILE iphEditor:CURSOR-CHAR > iStopDelAt:
        iphEditor:SOURCE-COMMAND('p4gl-maybe-case-backspace','').
    END.

    /* old approach that pollutes the undo stack
    iLenghtToDelete = LENGTH(cPrevWord).
    DO iLenghtToDelete = iLenghtToDelete TO 1 BY -1:
        iphEditor:SOURCE-COMMAND('CURSOR-LEFT','').
        iphEditor:SOURCE-COMMAND('DELETE-CHAR','').
    END. */
END.

/* 03-APR-2007 sla: disable next value changed to avoid probably unwanted rebuilt popup
 except for aliases which may put the cursor after a word to suggest buffers after */
IF NOT CAN-DO("ProcedureMode,alias", gcRunningMode) THEN RUN disableNextValueChanged IN ghCaller (iphEditor, "ItemChoosenInPop").

/* now insert the corresponding guy */
IF gcRunningMode = "alias" THEN DO:
    DEFINE VARIABLE iAliasLine    AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iCarretPos    AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iCursorLine   AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iLineBeginsAt AS INTEGER  NO-UNDO.
    iCursorLine = iphEditor:CURSOR-LINE.
    iLineBeginsAt = LENGTH(cLine) - LENGTH(LEFT-TRIM(cLine)). /*  to keep track of the number of spaces to insert at beginning of each new line */

    IF cItem BEGINS "<LargeText>" THEN RUN getAliasLargeText IN ghCaller (INPUT cItem, OUTPUT cItem).

    /* 15-NOV-2007 sla: little refine for pushing completion on space */
    IF glGotAHypeStrike THEN DO:
        cItem = REPLACE(cItem, " %\c", "%\c").
        IF INDEX(cItem, "%\c") = 0
         AND cItem MATCHES "* "
         THEN cItem = SUBSTRING(cItem, 1, LENGTH(cItem) - 1).
    END.

    /* 18-DEC-2006 sla: Improvement for multiple line aliases regarding the %\c directive */
    cItem = REPLACE(cItem, '~~n', "~n" + FILL(' ', iLineBeginsAt)).
    DO iAliasLine = 1 TO NUM-ENTRIES(cItem, "~n"):
        IF INDEX(ENTRY(iAliasLine, cItem, "~n"), "%\c") = 0 THEN NEXT.
        ASSIGN
         iCursorLine = iphEditor:CURSOR-LINE + iAliasLine - 1
         iCarretPos  = INDEX(ENTRY(iAliasLine, cItem, "~n"), "%\c").
        lPutCursorDirective = YES.
        IF iAliasLine = 1 THEN iCarretPos = iCarretPos + iphEditor:CURSOR-CHAR - 1.
    END.

    cItem = REPLACE(cItem, "%\c", "").

    iphEditor:INSERT-STRING(cItem).
    IF iCarretPos > 0 THEN ASSIGN
     iphEditor:CURSOR-LINE = iCursorLine
     iphEditor:CURSOR-CHAR = iCarretPos.
END.  /* IF gcRunningMode = "alias" */
ELSE DO:
    /* 27-NOV-2006 sla: some people use caps in the schema all the time, which is ugly
     let's convert to lower case if the all word is entirely CAPS, otherwise keep as is */
    cCaseSensistiveValue = cItem.

    IF   gcRunningMode = "preprocessor"
     AND INDEX(cLine, "}", iCursorChar) = 0
     AND INDEX(cLine, "~{") > 0 /* 19-JAN-2008 sla: added this condition so we do not add a '}' hen inserting a preprocessor name after the &UNDEFINE directive */
     THEN cItem = cItem + "}".

    IF CAN-DO("TableList,FieldList", gcRunningMode)
     AND glConvertDbTablesToLC /* 03-MAR-2007 sla: convert to LC only if the glConvertDbTablesToLC option is set */
     AND CAPS(cItem) = cCaseSensistiveValue /* allow mix of LC and CAPS, detect only usage of 100% CAPS */
     THEN iphEditor:INSERT-STRING(LC(cItem)).
    ELSE iphEditor:INSERT-STRING(cItem).
END.

APPLY 'ENTRY' TO iphEditor.

/* sla 16-NOV-2007 impementing a new way to do that */
/* add default 'NO-LOCK' when appropriate  */
/*IF gcRunningMode = "TableList" THEN RUN addNoLockIfAppropriate IN ghCaller (iphEditor).*/

/* 17-FEB-2007 sla: new ttItem directive to be replaced a buffer */
IF gcRunningMode = "TableList" THEN RUN replaceBufferDirective IN ghCaller (iphEditor, cItem) NO-ERROR.

/* we try to display a tooltip on open Parenthese */
IF cItem MATCHES "*(*" THEN DO:
    /* 29-NOV-2006 sla: for 4GL methods, we have no %\c directive */
    IF NOT lPutCursorDirective AND cItem MATCHES "*()"
     THEN iphEditor:SOURCE-COMMAND("cursor-left", "").

    /* special handling with inserted ABL methods  */
    IF cItem MATCHES "*)" THEN RUN leftParenthesePressed IN ghCaller (iphEditor). /* To fire API Tooltip */

    /* 17-FEB-2007 sla: do not insert closing guy when already present like for "FUNCTION  RETURNS ( ):" */
    ELSE IF NOT cItem MATCHES "*(*)*"
           THEN RUN insertClosingGuy IN ghCaller (iphEditor, "("). /* will try to insert ')' at the best place, and will also fire the API Tooltip */
END.

/* 14-SEP-2007 sla: for an external procedure, request parameters */
IF glExternalProc AND NUM-ENTRIES(cProcFileNameWithExtention, ".") > 1 THEN DO:
    DEFINE VARIABLE cExtProcName AS CHARACTER   NO-UNDO.

    /* re-ask the current line now that the ext procname has been inserted */
    RUN getEditorLine IN ghCaller (iphEditor, OUTPUT cLine).

    /* the point of the 2 following steps is to catch the proc name *with* it relative path */
    RUN extractWordN IN ghCaller (0
                                 ,cLine
                                 ,iCursorChar
                                 ,"periodPartOfWords,/PartOfWords,:PartOfWords"
                                 ,OUTPUT cExtProcName).
    RUN requestExtProcParam IN ghCaller (cExtProcName, OUTPUT cParamToInsert) NO-ERROR.

    IF cParamToInsert > "" THEN iphEditor:CURSOR-CHAR = LENGTH(RIGHT-TRIM(cLine)) + 1.
    RUN disableNextValueChanged IN ghCaller (iphEditor, "Attempt to insert Proc params"). /* the main point is to not let a abhackwin/valueChanged clear the monitor editor */
END.


/* 30-OCT-2007 sla: cleaned up the code regarding the PROCEDURE and func/method cases */
/* insert parameters for procedure when applicable */
/* 04-JUN-2008 sla: insert parameters for procedure when applicable */
IF  glInsertYourOwnParamIfProc
 OR glInsertYourOwnParamIfMethod /* 06-NOV-2007 sla: forgot that one on 30-OCT-2007 */
 OR glInsertYourOwnParamIfFunc
 THEN DO:
    FIND FIRST ttAttr WHERE ttAttr.cAttr = cItem NO-ERROR.
    IF AVAILABLE ttAttr
     AND ttAttr.cObjTypes > "" /* if no param, then nothing to insert */
     THEN RUN insertParams IN ghCaller (iphEditor
                                       ,cItem + IF glInsertYourOwnParamIfFunc THEN "()" ELSE ""  /* required to find out it is a UDF, but will remove it afterward */
                                       ,ttAttr.cObjTypes)
                                       NO-ERROR. /* 15-AUG-2007 sla: this new call retrieves from the local temp-table ttAttr in the cObjTypes */
END.


/* insert parameters for functions or methods when applicable */
ELSE IF gcRunningMode = "ProcedureMode"
 OR gltrailingParenthMeansFunction AND cItem MATCHES "*()"
 THEN DO:
   IF cParamToInsert = ""  THEN DO:
       FIND FIRST ttAttr WHERE ttAttr.cAttr = cItem NO-ERROR.
        IF AVAILABLE ttAttr THEN cParamToInsert = ttAttr.cObjTypes.
   END.

   RUN insertParams IN ghCaller (iphEditor   /* 15-AUG-2007 sla: this call is used to let abhack retrieve the parameters */
                                    ,cItem
                                    ,cParamToInsert) /* 14-SEP-2007 sla: if cParamToInsert is empty, then insertParams will retrieve the parameters by itself */
                                    NO-ERROR.
 END.

/* 17-NOV-2007 sla: new insSpaceAfterPrevRunOnChoose to make the cursor go backward automatically after RUN IN someProcHandle */
IF glinsSpaceAfterPrevRunOnChoose THEN DO:
    iCursorChar = INDEX(cLine, "RUN ") + 3.
    IF iCursorChar > 0 THEN DO:
        iphEditor:CURSOR-CHAR = iCursorChar.
        iphEditor:INSERT-STRING(" ").
        RUN DelayWindowEvent IN ghCaller (iphEditor, "valueChanged").
    END.
END.


/* 04-JUN-2008 sla: go back after DYNAMIC-FUNCTION() on a library handle has been chosen */
IF glgoAfterDynFuncOnChoose THEN DO:
    iCursorChar = INDEX(cLine, "DYNAMIC-FUNCTION('") + 18.
    IF iCursorChar > 0 THEN DO:
        iphEditor:CURSOR-CHAR = iCursorChar.
        RUN DelayWindowEvent IN ghCaller (iphEditor, "valueChanged").
    END.
END.


IF lCloseThisProcedure THEN APPLY 'CLOSE' TO THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL seList C-Win
ON VALUE-CHANGED OF seList IN FRAME fEditorPopupList
DO:
DEFINE VARIABLE cFieldName AS CHARACTER   NO-UNDO.

IF gcRunningMode = "fieldList" THEN DO:
    cFieldName = ENTRY(1, SELF:SCREEN-VALUE, SELF:DELIMITER ).
    cFieldName = ENTRY(1, cFieldName, " "). /* remove (dataType) */
    RUN displaytextSelectedInfo IN ghCaller
       (iphEditor
       ,iphBuffer:NAME + "." + cFieldName).
END.
IF gcRunningMode = "TableList" THEN DO:
    RUN displaytextSelectedInfo IN ghCaller
       (iphEditor
       ,SELF:SCREEN-VALUE).
END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win


/* ***************************  Main Block  *************************** */


/* disable default, as we want the frame to appear in the window of the editor */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
/*ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} */
ghEditorWin = iphEditor:WINDOW.
THIS-PROCEDURE:CURRENT-WINDOW = ghEditorWin.

ghCaller = SOURCE-PROCEDURE:HANDLE. /* We will use it as Library */

giAutoCompMinSize   = DYNAMIC-FUNCTION('getgiAutoCompMinSize' IN ghCaller).
glHypeStrikeMode = DYNAMIC-FUNCTION('getglHypeStrikeMode' IN ghCaller).

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
    DEFINE VARIABLE httToDelete AS HANDLE     NO-UNDO.

    /* cleanup dyn buffer for field list mode */
    IF VALID-HANDLE(iphBuffer) THEN DO:
        IF iphBuffer:DBNAME = "PROGRESST" THEN DO:
            httToDelete = iphBuffer:TABLE-HANDLE.
            DELETE OBJECT httToDelete.
        END.
        /* maybe it has just been deleted with the deletion of httToDelete */
        /* IF VALID-HANDLE(iphBuffer) THEN DELETE OBJECT iphBuffer. */
        IF VALID-HANDLE(iphBuffer) THEN DELETE OBJECT iphBuffer.
    END.
    RUN disable_UI.
END.

/* Kill other running instances in the same editor */
PUBLISH "KillEditorList" FROM ghCaller (iphEditor).

/* This event will help us to keep one single instance in the editor  */
SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "KillEditorList" IN ghCaller.
SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "getEditorListRunningMode" IN ghCaller.
SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "delayMoveToTop" IN ghCaller.
SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "completionTableAndApplyDot" IN ghCaller.
SUBSCRIBE PROCEDURE ghCaller TO "DelayWindowEvent" IN THIS-PROCEDURE.

/*======================= Override a few triggers =========================*/

/* 15-NOV-2007 sla: another trick for hypes */
IF glHypeStrikeMode THEN ON ' ', ',', ':', '(', ')', '[', ']', '.', '=', '<', '>', '+' OF seList DO:
    DEFINE VARIABLE cKeyToInsert AS CHARACTER   NO-UNDO.
    glGotAHypeStrike = YES.
    cKeyToInsert = LAST-EVENT:LABEL.

    APPLY "RETURN" TO seList IN FRAME fEditorPopupList.

    IF cKeyToInsert = "=" THEN cKeyToInsert = " =".
    IF cKeyToInsert = "<" THEN cKeyToInsert = " <".
    IF cKeyToInsert = ">" THEN cKeyToInsert = " >".

    IF cKeyToInsert = ":" THEN RUN delayWindowEvent IN ghCaller (iphEditor, "colon"). /* the point is to fire an attribute/property/method popup list */
    ELSE DO:
        iphEditor:INSERT-STRING(cKeyToInsert).
        RUN valueChanged IN ghCaller (iphEditor).
    END.
END.

/* 15-NOV-2007 sla: improvement, finally, try to use the Space to push a completion */
IF glHypeStrikeMode THEN ON ' ', ',', ':', '(', ')', '[', ']', '.', '=', '<', '>', '+' OF iphEditor DO:
    DEFINE VARIABLE cEditorPrevWord AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cKeyToInsert    AS CHARACTER   NO-UNDO.

    ASSIGN
     glGotAHypeStrike = YES
     cKeyToInsert     = LAST-EVENT:LABEL.

    /* 17-NOV-2007 sla: for hypes, kill the popup on space when the current word is 'IN' */
    IF cKeyToInsert = " " THEN DO:
        RUN getEditorPrevWord IN ghCaller (iphEditor
                                          ,"allowEmptyWord"
                                          ,OUTPUT cEditorPrevWord).

        /* IF gcRunningMode = "ProcedureMode"   */
        /*  AND cEditorPrevWord = "IN" THEN DO: */
        IF KEYWORD-ALL(cEditorPrevWord) <> ? THEN DO: /* this will cover the 'IN' case and all API's that begins with an ABL keywords like CREATE while having a selected poup item like 'createTTBuffer' */
            RUN editorValueChanged.  /* this will autocapitalize 'IN' when appropriate */
            RETURN.
        END.
    END.

    /* 30-NOV-2007 sla: for hypes, kill the popup if we strike '(' right after an AND or OR ... operator (current word is blank) */
    IF cKeyToInsert = "(" THEN DO:
        RUN getEditorPrevWord IN ghCaller (iphEditor
                                          ,"allowEmptyWord"
                                          ,OUTPUT cEditorPrevWord).

        IF cEditorPrevWord = "" THEN DO:
            iphEditor:INSERT-STRING("(").
            RUN delayMoveToTop.
            RETURN NO-APPLY.
        END.
    END.


    IF    cKeyToInsert = "."
      AND gcRunningMode = "TableList" THEN RUN completionTableAndApplyDot (iphEditor).
    ELSE APPLY "RETURN" TO seList IN FRAME fEditorPopupList.

    IF cKeyToInsert = "=" THEN iphEditor:INSERT-STRING(" ").
    ELSE IF cKeyToInsert = "<" THEN iphEditor:INSERT-STRING(" ").
    ELSE IF cKeyToInsert = ">" THEN iphEditor:INSERT-STRING(" ").
    ELSE IF cKeyToInsert = ":" THEN RUN delayWindowEvent IN ghCaller (iphEditor, "colonPressedOnly"). /* the point is to fire an attribute/property/method popup list */
    ELSE RUN valueChanged IN ghCaller (iphEditor). /* 16-NOV-2007 sla:  */
END.
ELSE
/* 22-DEC-2006 sla: Improvement to let vlick auto-captitalize a word
This is used especially for custom aliases where we can afford to let ABHack
rebuild the popup list at each key stoke */
/* 06-JAN-2007 sla: Actually, the EditorListShouldNotSpyEditor should even
 let abhack win handle the backspace event */
 IF CAN-DO(SOURCE-PROCEDURE:ADM-DATA, "EditorListShouldNotSpyEditor") THEN
  ON ' ' /*, 'DELETE-CHARACTER'  no way to handle this one correctly*/
   /*, 'RETURN' handled later separately */
   OF iphEditor RUN editorValueChanged IN THIS-PROCEDURE.

/* 15-NOV-2007 sla: beware about the unknown value in ADM-DATA */
IF CAN-DO(SOURCE-PROCEDURE:ADM-DATA, "EditorListShouldNotSpyEditor") <> YES THEN DO:
     /*  IF CAN-DO(SOURCE-PROCEDURE:ADM-DATA, "EditorListShouldNotSpyEditor")  */
    /* sadly, they have not implemented the value-changed event for the source-code editor, so I cannot track mouse-paste */
    ON 'ANY-PRINTABLE', 'BACKSPACE'  /*, 'DELETE-CHARACTER'  no way to handle this one correctly*/
    /* 16-MAR-2007 sla: attempt to let vslick auto-caps be effective finally, this one is not interesting  , '(' */
     /*, 'RETURN' handled later separately */
      OF iphEditor RUN editorValueChanged IN THIS-PROCEDURE.
    /* note also there is no way to trap the ESC key in such an editor */
    /* 07-DEC-2006 sla: Adding this point so ABHack */
    SUBSCRIBE PROCEDURE THIS-PROCEDURE TO "IsPopupHandlingValueChanged" IN ghCaller.
END.



/* The pointof these tricks are to avoid weird size state or errors
 when resizing or closing the editor window while this procedure is active */
ON "WINDOW-RESIZED" OF ghEditorWin DO:
    PUBLISH "DelayWindowEvent" (ghEditorWin, "WINDOW-RESIZED").
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

ON "WINDOW-CLOSE" OF ghEditorWin DO:
    PUBLISH "DelayWindowEvent" (ghEditorWin, "WINDOW-CLOSE").
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.

/* 24-NOV-2006 sla: The point of the following code is to avoid an error on CTRL-Z or other copy/paste events
  Sadly the only way to avoid these errors is to kill the popup guys because the
  ADE intensively relies on the FIRST-CHILD attribute, and the moved-to-top() popup
  guys become the first child of the editor FRAME with a simple move-to-top()
  The only way to avoid that would be to use only dynamic frame and widgets
  for the popup guys, then I would not need to use the VIEW FRAME IN WINDOW heditor:WINDOW Statement
  04-DEC-2006 sla: the same for shift-F2 check syntax */
DEFINE VARIABLE hMenu AS HANDLE     NO-UNDO.
DEFINE VARIABLE hSubMenu AS HANDLE     NO-UNDO.
hMenu = ghEditorWin:MENU-BAR.
hSubMenu     = hMenu:FIRST-CHILD NO-ERROR. /* 14-DEC-2006 sla: added NO-ERROR for case of window without menu, like the POV Code Viewer */
DO WHILE hSubMenu <> ?:
    /* 04-DEC-2006 sla: nowtry to handle all submenus this way */
    IF VALID-HANDLE(hSubMenu)
     AND hSubMenu:TYPE = "SUB-MENU" THEN
     ON "MENU-DROP" OF hSubMenu DO:
        PUBLISH "DelayWindowEvent" (hSubMenu, "MENU-DROP").
        APPLY 'CLOSE' TO THIS-PROCEDURE.
    END.
    hSubMenu = hSubMenu:NEXT-SIBLING.
END.

/* /* 04-DEC-2006 sla: obsolete now, because this trick is not limited to the &edit sub-menu */
DO WHILE hEditMenu <> ?:
    IF REPLACE(hEditMenu:LABEL ,"&" , "") = "Edit" THEN LEAVE.
    hEditMenu = hEditMenu:NEXT-SIBLING.
END.
IF VALID-HANDLE(hEditMenu) THEN
 ON "MENU-DROP" OF hEditMenu DO:
    PUBLISH "DelayWindowEvent" (hEditMenu, "MENU-DROP").
    APPLY 'CLOSE' TO THIS-PROCEDURE.
END.
*/

ON 'CURSOR-UP', 'CURSOR-DOWN' OF iphEditor RUN editorUpDown IN THIS-PROCEDURE.

ON 'RETURN' OF iphEditor DO:
    /* It is necessary to remove the Carriage Return that has just been inserted by the RETURN Key */

      /* makes problems when we hit the RETURN key right at the begining
         => it deletes the CR, but also the word before (buffer name or its last part)
          iphEditor:SOURCE-COMMAND('delete-prev-word', '').*/

       /* does not handle leading space just inserted !
        iphEditor:SOURCE-COMMAND('p4gl-maybe-case-backspace', '').  */

       /* removes trailing spaces...
          iphEditor:SOURCE-COMMAND('cursor-up', '').
          iphEditor:SOURCE-COMMAND('end-line', '').
          iphEditor:SOURCE-COMMAND('linewrap-delete-char', ''). */

/*  This solution works, but pollutes the undo stack
    /* Solution: delete these nasty spaces one by one until beginning of the line */
    DO WHILE iphEditor:CURSOR-CHAR > 1:
        iphEditor:SOURCE-COMMAND('p4gl-maybe-case-backspace', '').
    END.
    /* then delete the carriage return at last */
    iphEditor:SOURCE-COMMAND('p4gl-maybe-case-backspace', '').
  */

    /* simple and smart :)) */
    iphEditor:SOURCE-COMMAND('undo', '').

    APPLY "RETURN" TO seList IN FRAME fEditorPopupList.
END.


/* 14-MAR-2007 sla: I cannot define this particular trigger with the normal way
 Its point is to fire the completion when pressing '.' in a table/buffer list
 and make a field list pop up */
ON '.' OF seList DO:
    IF gcRunningMode = "TableList"  THEN DO:
        APPLY "RETURN" TO seList IN FRAME {&FRAME-NAME}.
        RUN delayWindowEvent IN ghCaller (iphEditor, "dot"). /* the point is to fire the field popup list */
    END.
END.


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

MENU-ITEM MAddTableName:CHECKED IN MENU PMSeList = YES.

seList:DELIMITER = CHR(1).


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  /*IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.*/
  IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    MESSAGE "Procedure" THIS-PROCEDURE:FILE-NAME "is designed to run persistenly only!" SKIP
     "About to apply close to it!"
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'CLOSE' TO THIS-PROCEDURE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE completionTableAndApplyDot C-Win
PROCEDURE completionTableAndApplyDot :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

DEFINE VARIABLE cLine     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrevWord AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDbName AS CHARACTER   NO-UNDO.

IF phEditor <> iphEditor THEN RETURN.

IF gcRunningMode = "TableList" THEN DO:

    /* 08-DEC-2008 sla: better support of dbname.buffer */
    IF INDEX(seList:SCREEN-VALUE IN FRAME {&FRAME-NAME}, ".") > 0 THEN DO:
        cDbName = ENTRY(1, seList:SCREEN-VALUE, ".").
        IF CONNECTED(cDbName) THEN DO:
            RUN getEditorLine IN ghCaller (phEditor, OUTPUT cLine).
            RUN extractWordN IN ghCaller (0, cLine, iphEditor:CURSOR-CHAR, "allowEmptyWord,periodPartOfWords,doNotPassCursor", OUTPUT cPrevWord) NO-ERROR.

            IF cPrevWord > "" AND INDEX(cPrevWord, ".") = 0 THEN DO:
                phEditor:INSERT-STRING(SUBSTRING(cDbName, LENGTH(cPrevWord) + 1 )).
                APPLY "close" TO THIS-PROCEDURE.
                RETURN.
            END.
        END. /* IF CONNECT(cDbName) THEN */
    END. /* emd 08-DEC-2008 sla: better support of dbname.buffer */

    APPLY "RETURN" TO seList IN FRAME {&FRAME-NAME}.
    RUN delayWindowEvent IN ghCaller (iphEditor, "dotPressedOnly"). /* the point is to fire the field popup list */
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the
               OCXs in the interface.
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "procEditorList.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
    chCtrlFrameEntryToEditor = CtrlFrameEntryToEditor:COM-HANDLE
    UIB_S = chCtrlFrameEntryToEditor:LoadControls( OCXFile, "CtrlFrameEntryToEditor":U)
    CtrlFrameEntryToEditor:NAME = "CtrlFrameEntryToEditor":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "procEditorList.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delayMoveToTop C-Win
PROCEDURE delayMoveToTop :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

chCtrlFrame:PSTimerMoveToTop:enabled = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  HIDE FRAME fEditorPopupList.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE editorUpDown C-Win
PROCEDURE editorUpDown :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cScreenValue AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cKey         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLookup      AS INTEGER     NO-UNDO.


cKey = LAST-EVENT:LABEL.

/* if only one item, then it means the user is not interested the list but just
 wants to move into the editor */
IF seList:NUM-ITEMS IN FRAME {&FRAME-NAME} = 1 THEN DO:
    APPLY 'CLOSE' TO THIS-PROCEDURE.
    CASE cKey :
        WHEN "CURSOR-UP"   THEN iphEditor:SOURCE-COMMAND('cursor-up', '').
        WHEN "CURSOR-DOWN" THEN iphEditor:SOURCE-COMMAND('cursor-down', '').
    END CASE.
    RETURN.
END.

APPLY 'ENTRY' TO seList IN FRAME fEditorPopupList.
DO WITH FRAME fEditorPopupList:
    /* 24-NOV-2006 sla: manage the mutliple case */
    cScreenValue = ENTRY(NUM-ENTRIES(seList:SCREEN-VALUE, seList:DELIMITER), seList:SCREEN-VALUE, seList:DELIMITER).
    iLookup = seList:LOOKUP(cScreenValue).
    seList:SCREEN-VALUE = ? NO-ERROR.
    CASE cKey:
        WHEN "CURSOR-UP"   THEN cScreenValue = seList:ENTRY(iLookup - 1) NO-ERROR.
        WHEN "CURSOR-DOWN" THEN cScreenValue = seList:ENTRY(iLookup + 1) NO-ERROR.
    END CASE.
    IF cScreenValue <> "" AND cScreenValue <> ?
     THEN seList:SCREEN-VALUE = cScreenValue.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE editorValueChanged C-Win
PROCEDURE editorValueChanged :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cAddToWord           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cKey                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cList                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPrevWord            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cScreenValue         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTableByFLA          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cToMatch             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLookup              AS INTEGER     NO-UNDO.
DEFINE VARIABLE lLowerCaseCompletion AS LOGICAL     NO-UNDO.
DEFINE VARIABLE ltryFLA              AS LOGICAL     NO-UNDO.

cKey = LAST-EVENT:FUNCTION.

CASE cKey:
    WHEN "(" THEN DO:
         /* 16-MAR-2007 a bit later sla: actually, the only way to fire this code is to DEFINE the
         TRIGGER IN the definition block, but this is not worth it */

        /* 16-MAR-2007 sla: see comments for the SPACE " " case bellow  */
        RUN getEditorLine IN ghCaller (iphEditor, OUTPUT cLine).
        /* anticipate what the line is going to be */
        cLine = SUBSTRING(cLine, 1, iphEditor:CURSOR-CHAR).
        RUN extractWordN IN ghCaller (0, cLine, iphEditor:CURSOR-CHAR, "", OUTPUT cPrevWord) NO-ERROR.
        IF KEYWORD-ALL(cPrevWord) <> ? THEN iphEditor:SOURCE-COMMAND('upcase-selection', '').

        RUN leftParenthesePressed IN ghCaller (iphEditor).
        FRAME {&FRAME-NAME}:MOVE-TO-TOP().
        RETURN.
    END.

    WHEN " " THEN DO:
        /* 16-MAR-2007 sla: try to solve a few cases where the auto-capitalization of A4GBL keywords
         is broken because a udf that begins with 'assign' or 'update' is present in a popup so the
         printable keys were not 100% handled by VSLick but applied from here...
         The solution is to test if the previous keyword is a reserved keyword, and send a convert
         to caps command if yes */
        lLowerCaseCompletion = DYNAMIC-FUNCTION('getglLowerCaseCompletion' IN ghCaller) NO-ERROR. /* 24-MAR-2007 sla: if ABHack exited leaving a completion list, this call can raise an error */
        IF NOT lLowerCaseCompletion THEN DO:
            RUN getEditorLine IN ghCaller (iphEditor, OUTPUT cLine).
            /* anticipate what the line is going to be */
            cLine = SUBSTRING(cLine, 1, iphEditor:CURSOR-CHAR).
            RUN extractWordN IN ghCaller (0, cLine, iphEditor:CURSOR-CHAR, "allowEmptyWord", OUTPUT cPrevWord) NO-ERROR. /* 21-MAR-2007 sla: added allowEmptyWord so we do not catch a word too far on the left */
            IF   cPrevWord > ""
             AND cPrevWord <> "="  /* 24-AUG-2007 sla: adding this condition to avoid a go backward, indded no need to convert '=' to upper case */
             AND KEYWORD-ALL(cPrevWord) = cPrevWord
             THEN DO:
                iphEditor:CURSOR-CHAR = iphEditor:CURSOR-CHAR - 1. /* otherwise we go to the end of the line !  Stangely, the cursor will go back the correct place afterwards */
                iphEditor:SOURCE-COMMAND('upcase-selection', '').
            END.
        END.

        APPLY 'CLOSE' TO THIS-PROCEDURE.
        RETURN.
    END.

    WHEN "BACKSPACE" THEN DO:
/*      iphEditor:SOURCE-COMMAND('CURSOR-LEFT', '').
        iphEditor:SOURCE-COMMAND('DELETE-CHAR', '').*/
        iphEditor:SOURCE-COMMAND('p4gl-maybe-case-backspace', '').
        cAddToWord = "doNotAdd".
    END.
/*  we n longer catch this trigger, anyway, we care only on the part on the left of the cursor
    WHEN "DELETE-CHARACTER" THEN DO:
        /* it seems there is no way to delete the char now, let's ask him to delete it later
        anyway, for now, we consider the part of the word before the cursor */
        cAddToWord = "doNotAdd".
    END.*/
END CASE.

IF cAddToWord = "doNotAdd" THEN cAddToWord = "".
ELSE cAddToWord = cKey.

RUN getEditorLine IN ghCaller (iphEditor, OUTPUT cLine).

/* anticipate what the line is going to be */
cLine = SUBSTRING(cLine, 1, iphEditor:CURSOR-CHAR - 1) + cAddToWord + SUBSTRING(cLine, iphEditor:CURSOR-CHAR).

RUN extractWordN IN ghCaller (0, cLine, iphEditor:CURSOR-CHAR, "allowEmptyWord,trace", OUTPUT cToMatch) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    APPLY 'CLOSE' TO THIS-PROCEDURE.
    RETURN.
END.

IF gcRunningMode = "preprocessor" THEN ASSIGN
 cToMatch = REPLACE(cToMatch, "~{&", "")
 cToMatch = REPLACE(cToMatch, "~}", "").

/* 13-SEP-2007 sla: improvement on backspace, if the current word is shorter that giAutoCompMinSize, then kill yourself */
IF   cKey = "BACKSPACE"
 AND (   LENGTH(cToMatch) = 0
      OR    LENGTH(cToMatch) < giAutoCompMinSize
        AND glABLAttributesAndMethods = NO /* 16-NOV-2007 sla: little improvement */
        AND gcRunningMode <> "fieldList" /* 21-NOV-2007 sla: for field list, keep the popup on backspace */
        AND gcRunningMode <> "ProcedureMode") /* 21-NOV-2007 sla: for ProcedureMode list, keep the popup on backspace */
 THEN DO:
    APPLY 'CLOSE' TO THIS-PROCEDURE.
    RETURN.
END.


CASE gcRunningMode:
    WHEN "FieldList" THEN DO:
        RUN extractWordN IN ghCaller (0, cLine, iphEditor:CURSOR-CHAR, "periodPartOfWords", OUTPUT cPrevWord) NO-ERROR.
        IF  ERROR-STATUS:ERROR
         OR NOT cPrevWord MATCHES "*~~~.*" /* the dot has been removed  => kill yourself */
         THEN DO:
            APPLY 'CLOSE' TO THIS-PROCEDURE.
            RETURN.
        END.
        RUN feedFieldList IN TARGET-PROCEDURE ("*" + cToMatch + "*").
        /* when we type in the editor, then we are not interested in multiple selection
          => remove current selection as we will manage to select the first most appropriate later  */
        seList:SCREEN-VALUE = "".
    END.
    WHEN "TableList" OR WHEN "FreeList" OR WHEN "alias" OR WHEN "ProcedureMode" OR WHEN "Preprocessor" THEN DO:
        /* 07-DEC-2006 sla: alwyas try to get FLA in tableList mode */
        IF gcRunningMode = "TableList" THEN DO:
            ltryFLA = DYNAMIC-FUNCTION('getglCompleteTableOnFLA' IN ghCaller) NO-ERROR.
            /* 20-MAR-2007 sla: repaired getBufferByFLA that was broken when support of qualified db was improved */
            IF ltryFLA THEN RUN getBufferByFLA IN ghCaller (cToMatch, "", OUTPUT cTableByFLA) NO-ERROR.

            /* 08-DEC-2008 sla: rebuild a table list with used buffer at top when the developper has typed a current word
              of same size as giAutoCompMinSize */
            IF LENGTH(cToMatch) = giAutoCompMinSize THEN DO:
                APPLY 'CLOSE' TO THIS-PROCEDURE.
                RUN delayWindowEvent IN ghCaller (iphEditor, "valueChanged"). /* the point is to fire the field popup list */
                RETURN.
            END.
        END.

        IF   NOT cToMatch BEGINS ipcInitBufferPrefix
         AND cTableByFLA = ""
         THEN DO:
            APPLY 'CLOSE' TO THIS-PROCEDURE.
            RETURN.
        END.
        SUBSTRING(cToMatch, 1, LENGTH(ipcInitBufferPrefix)) = "".

        RUN feedTableList IN TARGET-PROCEDURE ("*" + cToMatch + "*"
                                                 + IF cTableByFLA = "" THEN ""
                                                    ELSE "AddThisTableToListIfNotIn=" + cTableByFLA).
        /* 07-DEC-2006 sla: This one is terrible ! We may implement a little option to tell it to go to the other side of the cursor if possible */
        IF cTableByFLA > "" THEN RUN showTooltip IN ghCaller
          (iphEditor, "Found table by FLA  "               /* this trick will not let a ? go through  */
          ,"single,visibleTime=500,TryAboveCarret=" + IF glBellowCarret THEN "YES" ELSE "NO").
    END.
END CASE.

IF cTableByFLA > "" THEN seList:SCREEN-VALUE IN FRAME fEditorPopupList = cTableByFLA NO-ERROR.

IF cTableByFLA = ""
 OR seList:SCREEN-VALUE <> cTableByFLA /* it failed for some reasons */
 THEN DO:
    IF glSortedByName THEN FIND FIRST ttItem WHERE ttItem.cName BEGINS cToMatch USE-INDEX cName NO-ERROR.
    ELSE FIND FIRST ttItem WHERE ttItem.cName BEGINS cToMatch USE-INDEX iOrder NO-ERROR.
    IF AVAIL ttItem THEN seList:SCREEN-VALUE IN FRAME fEditorPopupList = ttItem.cName.
END.

/* 26-JAN-2007 sla: Select something by default */
IF seList:SCREEN-VALUE = ? AND seList:NUM-ITEMS > 0 THEN seList:SCREEN-VALUE = seList:ENTRY(1).

/* 15-NOV-2006 sla: it seems that editor can loose the focus in a Progress
 session that had been running for a long time.  Solution is to implement another
 timer to force an entry to the editor */
chCtrlFrameEntryToEditor:PSTimerEntryEditor:enabled = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY seList
      WITH FRAME fEditorPopupList.
  ENABLE seList
      WITH FRAME fEditorPopupList.
  {&OPEN-BROWSERS-IN-QUERY-fEditorPopupList}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE feedFieldList C-Win
PROCEDURE feedFieldList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcMatches AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cBufferList         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDataType           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFieldName          AS CHARACTER   CASE-SENSITIVE NO-UNDO.
DEFINE VARIABLE cScreenValue        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hField              AS HANDLE      NO-UNDO.
DEFINE VARIABLE iField              AS INTEGER     NO-UNDO.
DEFINE VARIABLE iFieldDataTypeWidth AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxWidth           AS INTEGER     NO-UNDO.

ASSIGN
 cScreenValue      = seList:SCREEN-VALUE IN FRAME fEditorPopupList
 seList:LIST-ITEMS = "".

EMPTY TEMP-TABLE ttItem.
/* Prepare list of items, keep track of the largest one */
DO iField = 1 TO iphBuffer:NUM-FIELDS:
    hField = iphBuffer:BUFFER-FIELD(iField).
    IF NOT hField:NAME MATCHES pcMatches THEN NEXT.

    ASSIGN
     cDataType  = dataType(hField)
     cFieldName = hField:NAME.

    /* 21-AUG-2007 sla: it will look nicer when converted in the list  */
    IF glConvertDbTablesToLC AND CAPS(cFieldName) = cFieldName THEN cFieldName = LC(cFieldName).

    CREATE ttItem.
    ASSIGN
     ttItem.iOrder = iField
     ttItem.cName  = cFieldName + "  (" + cDataType + ")"
     iMaxWidth     = MAXIMUM(iMaxWidth, FONT-TABLE:GET-TEXT-WIDTH-PIXELS(ttItem.cName, seList:FONT)).
END.
iMaxWidth = MAXIMUM(40, MINIMUM(240, iMaxWidth + 24)). /* 24 to cope the width of vertical scrollbar of the list */

/* try to align the datatype to the right... */
FOR EACH ttItem BY ttItem.iOrder:
    MoveDataTypeToRight:
    DO WHILE TRUE:
        iFieldDataTypeWidth = FONT-TABLE:GET-TEXT-WIDTH-PIXELS(ttItem.cName , seList:FONT).
        IF iFieldDataTypeWidth = iMaxWidth - 25 THEN LEAVE.
        IF iFieldDataTypeWidth > iMaxWidth - 25 THEN DO:
            /* get back by one space so it fits in it ;) */
            ttItem.cName = REPLACE(ttItem.cName, " (", "(").
            LEAVE MoveDataTypeToRight.
        END.
        ttItem.cName = REPLACE(ttItem.cName, "(", " (").
    END.
END.
/* feed the list with wanted sorting */
ASSIGN cBufferList = "".

/* 22/05/2007 DFE -- Set list-items once is much faster than add each item individually. */
IF glSortedByName THEN FOR EACH ttItem BY ttItem.cname:
  ASSIGN cBufferList = cBufferlist + ttItem.cName + seList:DELIMITER.
    /* seList:ADD-LAST(ttItem.cName). */
END.
ELSE FOR EACH ttItem BY ttItem.iOrder:
  ASSIGN cBufferList = cBufferlist + ttItem.cName + seList:DELIMITER.
    /* seList:ADD-LAST(ttItem.cName). */
END.
IF cBufferList <> "" THEN
  ASSIGN seList:LIST-ITEMS = RIGHT-TRIM(cBufferList,seList:DELIMITER) .

/* try to reselection previously selected item if any */
IF cScreenValue <> ? AND seList:SCREEN-VALUE <> "" AND seList:SCREEN-VALUE <> ?
 THEN seList:SCREEN-VALUE = cScreenValue NO-ERROR.

/* now manage the vertical size and position the guys */
RUN resizeAndLocateList IN TARGET-PROCEDURE (iMaxWidth).

/* if none select, then select first one */
IF  seList:SCREEN-VALUE = ""
 OR seList:SCREEN-VALUE = ?
 THEN seList:SCREEN-VALUE = seList:ENTRY(1).

IF seList:NUM-ITEMS = 0
 AND glExitWhenEmpty
 THEN APPLY 'CLOSE' TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE feedTableList C-Win
PROCEDURE feedTableList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcMatches AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cExtraTable  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cScreenValue AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListItems  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTable       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iMaxWidth    AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTable       AS INTEGER    NO-UNDO.
DEFINE VARIABLE iTableMax    AS INTEGER    NO-UNDO.

&SCOPED-DEFINE extraTableTag AddThisTableToListIfNotIn
IF INDEX(pcMatches, "{&extraTableTag}=") > 0 THEN DO:
    cExtraTable = SUBSTRING(pcMatches, INDEX(pcMatches, "{&extraTableTag}=") + LENGTH("{&extraTableTag}=")).
    pcMatches = SUBSTRING(pcMatches, 1, INDEX(pcMatches, "{&extraTableTag}=") - 1).
END.

ASSIGN
 cScreenValue      = seList:SCREEN-VALUE IN FRAME fEditorPopupList
 seList:LIST-ITEMS = "".

EMPTY TEMP-TABLE ttItem. /* yes, I use a TT called ttItem to manage tables... and so what ? */
/* Prepare list of items, keep track of the largest one */
cListItems = ipcInitBufferList.
IF cExtraTable > "" AND LOOKUP(cExtraTable, cListItems, CHR(1)) = 0
 THEN cListItems = cListItems + CHR(1) + cExtraTable.

iTableMax = NUM-ENTRIES(cListItems, CHR(1)).
DO iTable = 1 TO iTableMax:
    cTable = ENTRY(iTable, cListItems, CHR(1)).
    IF cTable MATCHES pcMatches
     OR CAN-DO(cExtraTable, cTable) /* 14-DEC-2006 sla: Let FLA Table get in */
     THEN DO:
        CREATE ttItem.
        ASSIGN
         ttItem.iOrder = iTable
         ttItem.cName  = cTable
         iMaxWidth      = MAXIMUM(iMaxWidth, FONT-TABLE:GET-TEXT-WIDTH-PIXELS(ttItem.cName, seList:FONT)).
     END.
END.
iMaxWidth = MAXIMUM(40, MINIMUM(180, iMaxWidth + 24)). /* 24 to cope the width of vertical scrollbar of the list */

/* feed the list with wanted sorting */
cListItems = "".
/* 22/05/2007 DFE -- Set list-items once is much faster than add each item individually. */
IF glSortedByName THEN FOR EACH ttItem BY ttItem.cname:
    cListItems = cListItems + ttItem.cName + seList:DELIMITER.
    /* seList:ADD-LAST(ttItem.cName). */
END.
ELSE FOR EACH ttItem BY ttItem.iOrder:
    cListItems = cListItems + ttItem.cName + seList:DELIMITER.
    /* seList:ADD-LAST(ttItem.cName). */
END.
IF cListItems <> "" THEN seList:LIST-ITEMS = RIGHT-TRIM(cListItems, seList:DELIMITER).

/* try to reselection previously selected item if any */
IF   cScreenValue <> ?
 AND seList:SCREEN-VALUE <> ""
 AND seList:SCREEN-VALUE <> ?
 THEN seList:SCREEN-VALUE = cScreenValue NO-ERROR.

/* now manage the vertical size and position the guys */
RUN resizeAndLocateList IN TARGET-PROCEDURE (iMaxWidth).

/* if none select, then select first one */
IF  seList:SCREEN-VALUE = ""
 OR seList:SCREEN-VALUE = ?
 THEN seList:SCREEN-VALUE = seList:ENTRY(1).

IF gcRunningMode = "TableList" THEN seList:BGCOLOR IN FRAME fEditorPopupList = 14. /* yellow to show we are in table list mode */
IF CAN-DO("FreeList,alias,ProcedureMode", gcRunningMode)  THEN seList:BGCOLOR IN FRAME fEditorPopupList = 8. /* yellow to show we are in table list mode */

IF seList:NUM-ITEMS = 0
 AND glExitWhenEmpty
 THEN APPLY 'CLOSE' TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getEditorListRunningMode C-Win
PROCEDURE getEditorListRunningMode :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT         PARAMETER phEditor       AS HANDLE      NO-UNDO.
DEFINE INPUT-OUTPUT  PARAMETER iopRunningMode AS CHARACTER   NO-UNDO.

IF phEditor <> iphEditor THEN RETURN.
iopRunningMode = gcRunningMode.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IsPopupHandlingValueChanged C-Win
PROCEDURE IsPopupHandlingValueChanged :
/*------------------------------------------------------------------------------
  Purpose:     /* 07-DEC-2006 sla: Adding this functionality so ABHack does not
                fire another popup if this one is already spying the editor */
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor              AS HANDLE     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER oplReturnIfTrue AS LOGICAL    NO-UNDO.

IF oplReturnIfTrue THEN RETURN. /* weird, another popup in this editor said yes (should not happen) */

oplReturnIfTrue = phEditor = iphEditor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE killEditorList C-Win
PROCEDURE killEditorList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER hEditor AS HANDLE     NO-UNDO.

IF hEditor <> iphEditor THEN RETURN.  /* let's allow multiple instances to run, but in different editor windows */

APPLY 'ENTRY' TO iphEditor.

APPLY 'CLOSE' TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadFieldList C-Win
PROCEDURE loadFieldList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phBuffer AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pcOptn   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cscrollToFieldLike AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cField             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE eOptn              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iOptn              AS INTEGER     NO-UNDO.

DEFINE BUFFER ttitem FOR ttitem.

ASSIGN
 iphBuffer     = phBuffer
 ipcOptn       = pcOptn
 gcRunningMode = "fieldList".

MENU-ITEM mAddTableName:SENSITIVE IN MENU PMSeList = YES.
MENU-ITEM m_Insert_items_in_multiple_li:SENSITIVE IN MENU PMSeList = YES.
MENU-ITEM m_InsertInMultiWithEqual:SENSITIVE IN MENU PMSeList = YES.

DO iOptn = 1 TO NUM-ENTRIES(ipcOptn):
    eOptn = ENTRY(iOptn, ipcOptn).

    IF eOptn = "multipleSelection" THEN seList:MULTIPLE IN FRAME {&FRAME-NAME} = YES.

    IF eOptn = "sortedBy=name" THEN glSortedByName = YES.
    IF eOptn = "exitWhenEmpty" THEN glExitWhenEmpty = YES.
    IF eOptn = "glConvertDbTablesToLC=YES" THEN glConvertDbTablesToLC = YES.

    IF eOptn BEGINS "scrollToFieldLike=" THEN cscrollToFieldLike = ENTRY(2, eOptn, "=").
END.

RUN feedFieldList IN TARGET-PROCEDURE ("*").

/* 25-FEB-2007 sla: new feature */
IF cscrollToFieldLike > "" THEN DO:
    cField = cscrollToFieldLike.

    FIND FIRST ttitem WHERE ENTRY(1,ttitem.cName, " ") = cField NO-ERROR.
    IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE ENTRY(1,ttitem.cName, " ") MATCHES "*" + cField NO-ERROR.
    IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE ENTRY(1,ttitem.cName, " ") MATCHES cField + "*" NO-ERROR.
    IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE ENTRY(1,ttitem.cName, " ") MATCHES "*" + cField + "*" NO-ERROR.

    /* try again without first char that may potentialy be a standard 'c' for CHARACTER 'i' for INTEGER' */
    IF NOT AVAILABLE ttitem THEN DO:
        cField = SUBSTRING(cField, 2).
        IF cField > "" THEN DO:
            FIND FIRST ttitem WHERE ENTRY(1,ttitem.cName, " ") = cField NO-ERROR.
            IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE ENTRY(1,ttitem.cName, " ") MATCHES "*" + cField NO-ERROR.
            IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE ENTRY(1,ttitem.cName, " ") MATCHES cField + "*" NO-ERROR.
            IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE ENTRY(1,ttitem.cName, " ") MATCHES "*" + cField + "*" NO-ERROR.
        END.
    END.

    /* and do that a 2nd time, as it does not hurt */
    IF NOT AVAILABLE ttitem THEN DO:
        cField = SUBSTRING(cField, 2).
        IF cField > "" THEN DO:
            FIND FIRST ttitem WHERE ENTRY(1,ttitem.cName, " ") = cField NO-ERROR.
            IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE ENTRY(1,ttitem.cName, " ") MATCHES "*" + cField NO-ERROR.
            IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE ENTRY(1,ttitem.cName, " ") MATCHES cField + "*" NO-ERROR.
            IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE ENTRY(1,ttitem.cName, " ") MATCHES "*" + cField + "*" NO-ERROR.
        END.
    END.

    /* 26-FEB-2007 sla: Suggestion by otr  => do the same thing the other way round */
    IF NOT AVAILABLE ttitem THEN DO:
        cField = cscrollToFieldLike.
        IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE cField MATCHES "*" + ENTRY(1,ttitem.cName, " ")  NO-ERROR.
        IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE cField MATCHES ENTRY(1,ttitem.cName, " ") + "*" NO-ERROR.
        IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE cField MATCHES "*" + ENTRY(1,ttitem.cName, " ") + "*" NO-ERROR.
    END.

    IF NOT AVAILABLE ttitem THEN DO:
        IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE cField MATCHES "*" + SUBSTRING(ENTRY(1,ttitem.cName, " "), 2)  NO-ERROR.
        IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE cField MATCHES SUBSTRING(ENTRY(1,ttitem.cName, " "),2) + "*" NO-ERROR.
        IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE cField MATCHES "*" + SUBSTRING(ENTRY(1,ttitem.cName, " "),2) + "*" NO-ERROR.
    END.

    IF NOT AVAILABLE ttitem THEN DO:
        IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE cField MATCHES "*" + SUBSTRING(ENTRY(1,ttitem.cName, " "),3)  NO-ERROR.
        IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE cField MATCHES SUBSTRING(ENTRY(1,ttitem.cName, " "),3) + "*" NO-ERROR.
        IF NOT AVAILABLE ttitem THEN FIND FIRST ttitem WHERE cField MATCHES "*" + SUBSTRING(ENTRY(1,ttitem.cName, " "),3) + "*" NO-ERROR.
    END.


    IF AVAILABLE ttitem THEN ASSIGN
     seList:SCREEN-VALUE = ? /* get rid off first selected item if any */
     seList:SCREEN-VALUE = ttitem.cName.
END.

APPLY 'VALUE-CHANGED' TO seList.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadListFromAttr C-Win
PROCEDURE loadListFromAttr :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opcList AS CHARACTER   NO-UNDO.

IF gcObjectTypeFilter = ""
 OR NOT MENU-ITEM m_Filter_on_type:CHECKED IN MENU PMSeList
 THEN FOR EACH ttAttr:
    opcList = opcList + CHR(1) + ttAttr.cAttr.
END.

ELSE FOR EACH ttAttr WHERE ttAttr.cObjTypes CONTAINS gcObjectTypeFilter
 BY ttAttr.cAttr:
    opcList = opcList + CHR(1) + ttAttr.cAttr.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadListFromTT C-Win
PROCEDURE loadListFromTT :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER TABLE FOR ttAttr.
DEFINE INPUT PARAMETER pcOptn AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hFilterMenuItem AS HANDLE   NO-UNDO.
DEFINE VARIABLE iOptn           AS INTEGER  NO-UNDO.


IF CAN-DO(pcOptn, "aliasMode") THEN gcRunningMode = "alias".
IF CAN-DO(pcOptn, "ABLAttributesAndMethods") THEN glABLAttributesAndMethods = YES.
IF CAN-DO(pcOptn, "insertSpaceAfterPrevRunOnChoose") THEN glinsSpaceAfterPrevRunOnChoose = YES.
IF CAN-DO(pcOptn, "goAfterDynFuncOnChoose") THEN glgoAfterDynFuncOnChoose = YES.
IF CAN-DO(pcOptn, "preprocessorMode") THEN gcRunningMode = "preprocessor".

ipcOptn = pcOptn.
glLeaveCurlyBracket = CAN-DO(pcOptn, "LeaveCurlyBracket").

DO iOptn = 1 TO NUM-ENTRIES(ipcOptn):
    IF ENTRY(iOptn, ipcOptn) BEGINS "objectType="
     THEN ASSIGN
      gcObjectTypeFilter = ENTRY(2, ENTRY(iOptn, ipcOptn), "=")
      hFilterMenuItem    = MENU-ITEM m_Filter_on_type:HANDLE IN MENU PMSeList
      hFilterMenuItem:SENSITIVE = YES
      hFilterMenuItem:CHECKED   = YES
      hFilterMenuItem:LABEL     = hFilterMenuItem:LABEL + " " + gcObjectTypeFilter.
END.

RUN loadListFromAttr (OUTPUT ipcInitBufferList).


/* for attribute list when striking ':' sometimes, we just hit return after a for
 each block, and we do not want an attribute to come */
IF NOT CAN-DO(ipcOptn, "addFirstEmptyItem")
 THEN ipcInitBufferList = SUBSTR(ipcInitBufferList, 2).


IF CAN-DO(ipcOptn, "ProcedureMode") THEN gcRunningMode = "ProcedureMode".
gltrailingParenthMeansFunction = CAN-DO(ipcOptn, "trailingParenthesesMeansFunction").
glInsertYourOwnParamIfMethod = CAN-DO(ipcOptn, "InsertYourOwnParamIfMethod").
glInsertYourOwnParamIfProc = CAN-DO(ipcOptn, "InsertYourOwnParamIfProc").
glInsertYourOwnParamIfFunc = CAN-DO(ipcOptn, "InsertYourOwnParamIfFunc").


glExternalProc = CAN-DO(ipcOptn, "ExternalProc").

ipcInitBufferPrefix = "".
IF gcRunningMode = "" THEN gcRunningMode = "FreeList". /* keep 'alias' if already set in */
IF gcRunningMode = "alias" THEN SUBSCRIBE TO "shouldAliasEditorListBeKilled" IN ghCaller.

glExitWhenEmpty = LOOKUP("exitWhenEmpty", pcOptn) > 0.

RUN feedTableList IN TARGET-PROCEDURE ("*").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadTableList C-Win
PROCEDURE loadTableList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcBufferList AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pcOptn       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iEntry AS INTEGER    NO-UNDO.
DEFINE VARIABLE eEntry AS CHARACTER  NO-UNDO.

ipcOptn     = pcOptn.

pcBufferList = REPLACE(pcBufferList, ",", CHR(1)).

DO iEntry = 1 TO NUM-ENTRIES(pcOptn):
    eEntry = ENTRY(iEntry, pcOptn).
    IF eEntry BEGINS "prefix=" THEN ipcInitBufferPrefix = ENTRY(2, eEntry, "=").
    IF eEntry = "exitWhenEmpty" THEN glExitWhenEmpty = YES.

    IF eEntry = "openFileAction" THEN gcActionOpenFile = ENTRY(iEntry + 1, pcOptn).
END.

ASSIGN
 ipcInitBufferList = pcBufferList
 gcRunningMode       = "TableList".

RUN feedTableList IN TARGET-PROCEDURE ("*").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeAndLocateList C-Win
PROCEDURE resizeAndLocateList :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER piMaxWidth AS INTEGER    NO-UNDO.

DEFINE VARIABLE hFrame           AS HANDLE   NO-UNDO.
DEFINE VARIABLE iHeight          AS INTEGER  NO-UNDO.
DEFINE VARIABLE iMaxVisibleItems AS INTEGER  NO-UNDO.
DEFINE VARIABLE iOK              AS INTEGER  NO-UNDO.
DEFINE VARIABLE iSpaceAbove      AS INTEGER  NO-UNDO.
DEFINE VARIABLE iSpaceBellow     AS INTEGER  NO-UNDO.
DEFINE VARIABLE iTextHeight      AS INTEGER  NO-UNDO.
DEFINE VARIABLE iX               AS INTEGER  NO-UNDO.
DEFINE VARIABLE iY               AS INTEGER  NO-UNDO.
DEFINE VARIABLE mPoint           AS MEMPTR   NO-UNDO.

/* now its time to realize the guys */
VIEW FRAME fEditorPopupList IN WINDOW iphEditor:WINDOW.
hFrame = FRAME fEditorPopupList:HANDLE. /* more convenient */
hFrame:VISIBLE = NO.


/* Get position of the caret (text cursor) */
SET-SIZE(mPoint) = 8.
RUN GetCaretPos(mPoint, OUTPUT iOK).
iX = GET-LONG(mPoint, 1).
iY = GET-LONG(mPoint, 5).
SET-SIZE(mPoint) = 0.

/* Work out height of a line, and available space above and bellow */
iTextHeight  = FONT-TABLE:GET-TEXT-HEIGHT-PIXELS(seList:FONT IN FRAME fEditorPopupList).
iSpaceAbove  = iY - iphEditor:Y - iTextHeight - 7.
iSpaceBellow = iphEditor:HEIGHT-PIXELS - iY - iTextHeight - 7.


/* One may increase the maximum of 17 visible iteration, as desired (why 17?  well, because it's better than 16 ;)*/
iMaxVisibleItems = MINIMUM(17, MAXIMUM(1, seList:NUM-ITEMS)).

/* Try to make it fit bellow if possible */
IF iSpaceBellow / iTextHeight >= iMaxVisibleItems THEN glBellowCarret = YES.
/* otherwize put it above if possible */
ELSE IF iSpaceAbove / iTextHeight > iMaxVisibleItems THEN glBellowCarret = NO.
/* Otherwize put it in hightest place */
ELSE DO:
    IF iSpaceBellow > iSpaceAbove THEN ASSIGN
     glBellowCarret = YES
     iMaxVisibleItems = MAXIMUM(1, TRUNCATE(iSpaceBellow / iTextHeight, 0)).
    ELSE ASSIGN
     glBellowCarret = NO
     iMaxVisibleItems = MAXIMUM(1, TRUNCATE(iSpaceAbove / iTextHeight, 0)).
END.

iHeight = iTextHeight * iMaxVisibleItems + 4.

/* Now resize the selection list and its frame */
IF iHeight > hFrame:HEIGHT-PIXELS THEN ASSIGN
 hFrame:HEIGHT-PIXELS         = iHeight
 hFrame:VIRTUAL-HEIGHT-PIXELS = iHeight
 seList:HEIGHT-PIXELS         = iHeight.
IF iHeight < hFrame:HEIGHT-PIXELS THEN ASSIGN
 seList:HEIGHT-PIXELS         = iHeight
 hFrame:HEIGHT-PIXELS         = iHeight
 hFrame:VIRTUAL-HEIGHT-PIXELS = iHeight.


IF glBellowCarret THEN hFrame:Y = iY + iTextHeight + iphEditor:Y + 2.
ELSE hFrame:Y = iphEditor:Y + iY - iHeight.

/* resize the list and its frame horizontally now to fit with the items */
IF piMaxWidth > hFrame:WIDTH-PIXELS THEN ASSIGN
 hFrame:WIDTH-PIXELS          = piMaxWidth
 hFrame:VIRTUAL-WIDTH-PIXELS  = piMaxWidth
 seList:WIDTH-PIXELS          = piMaxWidth.
IF piMaxWidth < hFrame:WIDTH-PIXELS THEN ASSIGN
 seList:WIDTH-PIXELS          = piMaxWidth
 hFrame:WIDTH-PIXELS          = piMaxWidth
 hFrame:VIRTUAL-WIDTH-PIXELS  = piMaxWidth.


/* at last, position this guys above or bellow the caret, as determined above */
IF hFrame:WINDOW:VIRTUAL-WIDTH-PIXELS < iX + iphEditor:X + hFrame:WIDTH-PIXELS
 THEN hFrame:WINDOW:VIRTUAL-WIDTH-PIXELS = iX + iphEditor:X + hFrame:WIDTH-PIXELS.
hFrame:X = iX + iphEditor:X.
/*
IF hFrame:X <> iX + iphEditor:X /* happens when the frame could not fit in the editor */
 THEN hFrame:X = iphEditor:X + iphEditor:WIDTH-PIXELS - hFrame:WIDTH-PIXELS + 1.
*/
/* if too far on the right, then provoque scroll to the right ;) */
IF hFrame:X + hFrame:WIDTH-PIXELS > iphEditor:X + iphEditor:WIDTH-PIXELS THEN DO:
/*IF hFrame:X <> iX + iphEditor:X THEN DO:*/
    DEFINE VARIABLE iHowMuchToScroll AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iAttempt         AS INTEGER     NO-UNDO.
    DO iAttempt = 2 TO seList:WIDTH-CHAR + 2 BY 3: /* let's give it a limit, by 3 will make it faster */
        /* go by iAttempt characters on the right to provoque a right scroll */
        DO iHowMuchToScroll = 1 TO iAttempt:
            iphEditor:source-command('CURSOR-RIGHT', '').
        END.
        /* then put the cursor where it was */
        DO iHowMuchToScroll = 1 TO iAttempt:
            iphEditor:source-command('CURSOR-LEFT', '').
        END.
        /* move the guys to new location of the cursor */
        SET-SIZE(mPoint) = 8.
        RUN GetCaretPos(mPoint, OUTPUT iOK).
        iX = GET-LONG(mPoint, 1).
        SET-SIZE(mPoint) = 0.

        hFrame:X = iX + iphEditor:X NO-ERROR.

        IF hFrame:X <> iX + + iphEditor:X THEN NEXT.
        IF hFrame:X + hFrame:WIDTH-PIXELS <= iphEditor:X + iphEditor:WIDTH-PIXELS THEN LEAVE.
    END.
END.

hFrame:VISIBLE = YES.

/* 15-NOV-2006 sla: get rid off scrollbars */
ASSIGN
 hFrame:VIRTUAL-WIDTH-PIXELS  = hFrame:WIDTH-PIXELS
 hFrame:VIRTUAL-HEIGHT-PIXELS = hFrame:HEIGHT-PIXELS.


IF NOT glEnableUIRun THEN DO:
    RUN enable_UI.
    glEnableUIRun = YES.
END.

PUBLISH "delayMoveToTop" FROM ghCaller.

/*chCtrlFrame:PSTimerMoveToTop:enabled = YES.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShallICallEditorValueChanged C-Win
PROCEDURE ShallICallEditorValueChanged :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor              AS HANDLE     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iophRunInIt     AS HANDLE     NO-UNDO.

IF iophRunInIt <> ? THEN RETURN. /* weird, another popup in this editor said yes (should not happen) */

IF phEditor = iphEditor THEN iophRunInIt = THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE shouldAliasEditorListBeKilled C-Win
PROCEDURE shouldAliasEditorListBeKilled :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER phEditor           AS HANDLE      NO-UNDO.
DEFINE INPUT PARAMETER pcWordBeforeCarret AS CHARACTER   NO-UNDO.

IF  phEditor <> iphEditor
 OR gcRunningMode <> "alias" THEN RETURN.

/* 16-NOV-2007 sla: changed it to be more convenient with the Hypes' Strikes */
/* IF NOT CAN-FIND(FIRST ttattr WHERE ttattr.cAttr BEGINS pcWordBeforeCarret ) THEN APPLY 'CLOSE' TO THIS-PROCEDURE. */
IF NOT CAN-FIND(FIRST ttattr WHERE ttattr.cAttr = pcWordBeforeCarret ) THEN APPLY 'CLOSE' TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dataType C-Win
FUNCTION dataType RETURNS CHARACTER
  (hField AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  return abreviated datatype string
    Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cDatatype AS CHARACTER   NO-UNDO.

cDatatype = hField:DATA-TYPE.

CASE cDatatype:
    WHEN "CHARACTER" THEN RETURN "char".
    WHEN "INTEGER" THEN RETURN "int".
    WHEN "LOGICAL" THEN RETURN "log".
    WHEN "DECIMAL" THEN RETURN "dec".
    OTHERWISE RETURN LC(cDatatype).
END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION targetEditorHandle C-Win
FUNCTION targetEditorHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:
    Notes:
------------------------------------------------------------------------------*/

  RETURN iphEditor.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* brwsrtrg.i 
11/01/01   YSK  changed to have better performance*/

{methods/template/brwsord.i 1}
{methods/template/brwsord.i 2}
{methods/template/brwsord.i 3}
{methods/template/brwsord.i 4}
{methods/template/brwsord.i 5}
{methods/template/brwsord.i 6}
{methods/template/brwsord.i 7}
{methods/template/brwsord.i 8}
{methods/template/brwsord.i 9}
{methods/template/brwsord.i 10}
{methods/template/brwsord.i 11}
{methods/template/brwsord.i 12}
{methods/template/brwsord.i 13}

&Scoped-define SELF-NAME auto_find
ON LEAVE OF {&SELF-NAME} IN FRAME {&FRAME-NAME} /* Auto Find */
DO:
  ASSIGN
    {&SELF-NAME}
    find-auto = yes.
  APPLY "ANY-PRINTABLE" TO {&BROWSE-NAME}.
  find-auto = no.
END.

ON RETURN OF {&SELF-NAME} IN FRAME {&FRAME-NAME} /* Auto Find */
DO:
  APPLY "LEAVE" TO {&SELF-NAME}.
END.

&Scoped-define SELF-NAME browse-order
ON VALUE-CHANGED OF {&SELF-NAME} IN FRAME {&FRAME-NAME}
DO:
  &IF "{&IAMWHAT}" = "SEARCH" &THEN
  APPLY "CHOOSE" TO Btn_Clear_Search.
  &ENDIF
  /*APPLY "CHOOSE" TO Btn_Clear_Find.       ** not run any-printable of browse  but reset auto_find value only
                                               run open-query 3 times  */
  ASSIGN                   
    auto_find = ""
    auto_find:SCREEN-VALUE = "".
/*  {methods/wait.i}    */            /* ysk set hour-glass */
  RUN Change-Order ({&SELF-NAME}:SCREEN-VALUE).
  APPLY "ENTRY" TO {&BROWSE-NAME}.
END.

ON ANY-PRINTABLE OF {&BROWSE-NAME} IN FRAME {&FRAME-NAME}
DO:
  IF NUM-RESULTS("{&BROWSE-NAME}") = 0 OR NUM-RESULTS("{&BROWSE-NAME}") = ? THEN
  RETURN NO-APPLY.
  {methods/wait.i}
  current-rowid = ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
  IF LASTKEY NE 8 AND NOT find-auto THEN
  auto_find = auto_find + KEYLABEL(LASTKEY).
  RUN Find-Record (browse-order:SCREEN-VALUE).
  IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
  DO:
    MESSAGE "Record not found beginning with '" + auto_find + "' !!!"
        VIEW-AS ALERT-BOX.
    auto_find = SUBSTR(auto_find,1,LENGTH(auto_find) - 1).
    RUN Find-Record (browse-order:SCREEN-VALUE).
  END.
  IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
  DO:
    auto_find = "".
    RUN Find-Record (browse-order:SCREEN-VALUE).
  END.
  ASSIGN
    auto_find:SCREEN-VALUE = auto_find
    save-rowid = ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
  
  IF current-rowid NE save-rowid THEN
  REPOSITION {&BROWSE-NAME} TO ROWID save-rowid.

  IF auto_find = "" THEN
  DISABLE Btn_Clear_Find WITH FRAME {&FRAME-NAME}.
  ELSE
  ENABLE Btn_Clear_Find WITH FRAME {&FRAME-NAME}.
  IF current-rowid NE save-rowid THEN
  APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
  APPLY "ENTRY" TO {&BROWSE-NAME}.
  {methods/nowait.i}
END.

ON BACKSPACE OF {&BROWSE-NAME} IN FRAME {&FRAME-NAME}
DO:
  auto_find = SUBSTR(auto_find,1,LENGTH(auto_find) - 1).
  APPLY "ANY-PRINTABLE" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.
END.

&IF "{&IAMWHAT}" NE "SEARCH" AND
    "{&IAMWHAT}" NE "LOOKUP" &THEN
ON DEFAULT-ACTION OF {&BROWSE-NAME} IN FRAME {&FRAME-NAME}
DO:
  ASSIGN
    auto_find:SCREEN-VALUE = ""
    auto_find.
  {methods/template/selectpg.i}
END.
&ENDIF

ON RETURN OF {&BROWSE-NAME} IN FRAME {&FRAME-NAME}
DO:
  APPLY "DEFAULT-ACTION" TO {&BROWSE-NAME}.
END.

ON RIGHT-MOUSE-CLICK OF {&BROWSE-NAME} IN FRAME {&FRAME-NAME}
DO:
  IF {methods/chkdevid.i} THEN
  RUN Get_Procedure IN Persistent-Handle ("ruler.",OUTPUT run-proc,yes).
END.

&Scoped-define SELF-NAME Btn_Clear_Find
ON CHOOSE OF {&SELF-NAME} IN FRAME {&FRAME-NAME} /* Clear Find */
DO:
  APPLY LASTKEY.
  ASSIGN
    auto_find = ""
    auto_find:SCREEN-VALUE = "".
  APPLY "ANY-PRINTABLE" TO {&BROWSE-NAME}.
END.

&UNDEFINE SELF-NAME

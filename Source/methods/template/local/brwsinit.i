/* brwsinit.i */

&IF "{&FORMAT-1}" NE "" &THEN
&Scoped-define FORMAT-1 X(256)
&ENDIF

auto_find:FORMAT IN FRAME {&FRAME-NAME} = "{&FORMAT-1}".

IF CAN-DO("notes","{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}") THEN
RETURN.

&IF INDEX("{&NORECKEY}","{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}") = 0 &THEN
{methods/template/local/setvalue.i}

DEFINE VARIABLE op-rec_key AS CHARACTER NO-UNDO.

{methods/run_link.i "CONTAINER-SOURCE" "Get-g_rec_key" "(OUTPUT op-rec_key)"}
IF op-rec_key = "" THEN
RETURN.

FIND {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}
    WHERE {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.rec_key = op-rec_key
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN
RETURN.

{methods/run_link.i "CONTAINER-SOURCE" "Reset-g_rec_key"}
save-rowid = ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
REPOSITION {&BROWSE-NAME} TO ROWID save-rowid.
&ENDIF


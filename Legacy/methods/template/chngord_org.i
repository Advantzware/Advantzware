/* chngord.i */

&IF "{&FLDNAME{1}}" NE "" &THEN
&IF "{&FORMAT-{1}}" NE "" &THEN
&Scoped-define FORMAT-{1} X(256)
&ENDIF

&IF "{&IAMWHAT}" = "SEARCH" &THEN
{methods/template/srchrecd.i {1}}
&ELSE
&Scoped-define SORTBY-PHRASE {&IDXNAME{1}} {&SORTBY-{1}}
&IF "{&IAMWHAT}" NE "LOOKUP" &THEN
&Scoped-define KEY-PHRASE ~
{&FLDNAME{1}} {&SETFIRST{&DATATYP{1}}} AND {&FLDNAME{1}} {&SETLAST{&DATATYP{1}}}
&ELSE
&Scoped-define KEY-PHRASE {&WHERE-STATEMENT}
&ENDIF
WHEN {1} THEN
DO WITH FRAME {&FRAME-NAME}:
  auto_find:FORMAT = "{&FORMAT-{1}}".
  {&OPEN-QUERY-{&BROWSE-NAME}}
END.
&Scoped-define KEY-PHRASE TRUE
&Undefine SORTBY-PHRASE
&ENDIF
&ENDIF

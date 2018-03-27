/* srchrecd.i */

&IF "{&WHERE-STATEMENT}" = "" &THEN
&Scoped-define WHERE-STATEMENT TRUE
&ENDIF
&IF "{&FLDNAME{1}}" NE "" &THEN
  &Scoped-define SORTBY-PHRASE {&SORTBY-{1}}
  &Scoped-define KEY-PHRASE {&WHERE-STATEMENT} AND {&WORDFLD} CONTAINS word_search AND {&FLDNAME{1}} BEGINS auto_find
  &Scoped-define OPENQUERY-1 {&OPEN-QUERY-{&BROWSE-NAME}}
  &Scoped-define KEY-PHRASE {&WHERE-STATEMENT} AND {&WORDFLD} CONTAINS word_search
  &Scoped-define OPENQUERY-2 {&OPEN-QUERY-{&BROWSE-NAME}}
  &Scoped-define KEY-PHRASE {&WHERE-STATEMENT} AND {&FLDNAME{1}} BEGINS auto_find
  &Scoped-define OPENQUERY-3 {&OPEN-QUERY-{&BROWSE-NAME}}
  &Scoped-define KEY-PHRASE {&WHERE-STATEMENT}
  &Scoped-define OPENQUERY-4 {&OPEN-QUERY-{&BROWSE-NAME}}

/* rstark */
  &Scoped-define KEY-PHRASE {&WHERE-STATEMENT} AND {&WORDFLD} CONTAINS word_search AND {&FLDNAME{1}} MATCHES auto_find
  &Scoped-define OPENQUERY-5 {&OPEN-QUERY-{&BROWSE-NAME}}
  &Scoped-define KEY-PHRASE {&WHERE-STATEMENT} AND {&FLDNAME{1}} MATCHES auto_find
  &Scoped-define OPENQUERY-6 {&OPEN-QUERY-{&BROWSE-NAME}}
/* rstark */

  &Scoped-define KEY-PHRASE TRUE
  &Undefine SORTBY-PHRASE
WHEN {1} THEN
DO WITH FRAME {&FRAME-NAME}:
  auto_find:FORMAT = "{&FORMAT-{1}}".

/* rstark */
  IF word_search NE '' AND INDEX(auto_find,'*') NE 0 THEN DO:
      {&OPENQUERY-5}
  END.
  
  ELSE
  IF INDEX(auto_find,'*') NE 0 THEN
  {&openquery-6}
  ELSE
/* rstark */

  IF word_search NE '' AND auto_find NE '' THEN
  {&OPENQUERY-1}
  ELSE
  IF word_search NE '' THEN
  {&OPENQUERY-2}
  ELSE
  IF auto_find NE '' THEN
  {&OPENQUERY-3}
  ELSE
  {&OPENQUERY-4}
END.
&ENDIF

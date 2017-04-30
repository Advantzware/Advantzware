/* chngord.i 
11/01/01  YSK  changed for table joined browser */

&IF "{&FLDNAME{1}}" NE "" &THEN
&IF "{&FORMAT-{1}}" NE "" &THEN
&SCOPED-DEFINE FORMAT-{1} X(256)
&ENDIF

&IF "{&IAMWHAT}" = "SEARCH" &THEN
{methods/template/srchrecd.i {1}}
&ELSE
&SCOPED-DEFINE SORTBY-PHRASE {&IDXNAME{1}} {&SORTBY-{1}}
&IF "{&IAMWHAT}" NE "LOOKUP" &THEN
&SCOPED-DEFINE KEY-PHRASE ~
{&FLDNAME{1}} {&SETFIRST{&DATATYP{1}}} AND {&FLDNAME{1}} {&SETLAST{&DATATYP{1}}}
&ELSE
&SCOPED-DEFINE KEY-PHRASE {&WHERE-STATEMENT}
&ENDIF
WHEN {1} THEN
DO WITH FRAME {&FRAME-NAME}:
  auto_find:FORMAT = "{&FORMAT-{1}}".
  /* ========== YSK 11/01/01  to browse query with join table ex. browsers/rfqpart.w */
  &if DEFINED (item-key-phrase) <> 0  &then 
       &SCOPED-DEFINE key-phrase true
       &IF "{&DATATYP{1}}" = "" &THEN
         &SCOPED-DEFINE item-key-phrase {&WHERE-STATEMENT} AND {&FLDNAME{1}} BEGINS auto_find
       &ELSE
         &SCOPED-DEFINE item-key-phrase {&WHERE-STATEMENT} AND {&FLDNAME{1}} GE {&DATATYP{1}}(auto_find,"{&FORMAT-{1}}")
       &ENDIF
  &endif
  /* ======= end of mods =============== */
  /*MESSAGE "chngord.i {&item-key-phrase}" SKIP
      "{&iamwhat} , {&where-statement}" skip
    "  {&OPEN-QUERY-{&BROWSE-NAME}}"
      VIEW-AS ALERT-BOX.*/

  &if DEFINED(browse2) &then 
     {{&browse2}}
  &else {&OPEN-QUERY-{&BROWSE-NAME}}
  &endif
  run dispatch ('row-changed').  /* ysk added */ 
END.
&SCOPED-DEFINE KEY-PHRASE TRUE
&Undefine SORTBY-PHRASE
&ENDIF
&ENDIF

/* srtord.i    resort include */        

&IF "{&IAMWHAT}" = "LOOKUP" &THEN
&scoped-define sortby-phrase {&sortby-{1}}
&SCOPED-DEFINE index-phrase {&idxname{1}}
&ElseIF  "{&IAMWHAT}" = "Search" &THEN
  &scoped-define where-statement >= {&datatype-{1}}(lv-search)
  &scoped-define key-phrase {&fld-name-{1}} {&Where-statement}
&endif

WHEN {1} THEN DO:
{&open-query-{&browse-name}}
END.

/* srtord.i    resort include */        

/*&IF "{&IAMWHAT}" EQ "LOOKUP" &THEN*/
  &scoped-define sortby-phrase {&sortby-{1}}
  &SCOPED-DEFINE index-phrase {&idxname{1}}
/*&endif*/

&IF "{&IAMWHAT}" EQ "Search" &THEN
  &IF "{&ASCDSC-{1}}" BEGINS "DESC" &THEN
    &scoped-define where-statement <= {&datatype-{1}}(lv-search)
  &ELSE
    &scoped-define where-statement >= {&datatype-{1}}(lv-search)
  &endif
  &scoped-define key-phrase {&fld-name-{1}} {&Where-statement}
&endif  
  
WHEN {1} THEN DO:
  {&open-query-{&browse-name}}
END.

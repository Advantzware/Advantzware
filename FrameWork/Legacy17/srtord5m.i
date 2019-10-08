/* srtord5m.i    resort include  same as srtord.i but use begins instead of >= and error */        

/*&IF "{&IAMWHAT}" EQ "LOOKUP" &THEN*/
  &SCOPED-DEFINE sortby-phrase {&sortby-{1}}
  &SCOPED-DEFINE index-phrase {&idxname{1}}
/*&ENDIF*/

&IF "{&IAMWHAT}" EQ "Search" &THEN
  &IF "{&ASCDSC-{1}}" BEGINS "DESC" &THEN
    &SCOPED-DEFINE how GE 
  &ELSE
    &SCOPED-DEFINE how LE
  &ENDIF
  
  &IF "{&DATATYPE-{1}}" EQ "STRING" OR "{&DATATYPE-{1}}" EQ "" &THEN
    &SCOPED-DEFINE how BEGINS
  &ENDIF
  
  &SCOPED-DEFINE where-statement 
  
  &SCOPED-DEFINE key-phrase {&fld-name-{1}} {&how} {&DATATYPE-{1}}(lv-search)
&ENDIF

WHEN {1} THEN DO:
  {&open-query-{&browse-name}}

  IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
     DO:
        MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
        VIEW-AS ALERT-BOX.
    /*    lv-search:screen-value = "".  */
         APPLY "ENTRY" TO {&BROWSE-NAME}.
     end.    
END.

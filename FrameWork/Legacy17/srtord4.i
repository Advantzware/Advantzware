
&IF TRIM("{&idxname{1}}") NE "" &THEN
  &SCOPED-DEFINE index-phrase USE-INDEX {&idxname{1}}
&ELSE
  &SCOPED-DEFINE index-phrase
&ENDIF
  
&IF "{&ASCDSC-{1}}" BEGINS "DESC" &THEN
  &SCOPED-DEFINE how LE 
&ELSE
  &SCOPED-DEFINE how GE
&ENDIF
                                            
&IF TRIM("{&DATATYPE-{1}}") EQ "" &THEN
  &SCOPED-DEFINE how BEGINS
&ENDIF

/*&SCOPED-DEFINE key-phrase {&FLD-NAME-{1}} {&how} {&DATATYPE-{1}}(lv-search)*/
&IF DEFINED(useMatches) NE 0  &THEN
           &SCOPED-DEFINE KEY-PHRASE  ({&fld-name-{1}} {&how} {&DATATYPE-{1}}(lv-search) OR ({&fld-name-{1}} MATCHES '*' + lv-search + '*' AND lv-search BEGINS '*' ))   /*task 10171311   */
        &ELSE
           &SCOPED-DEFINE KEY-PHRASE  {&fld-name-{1}} {&how} {&DATATYPE-{1}}(lv-search)
        &ENDIF

&SCOPED-DEFINE OPEN-QUERY-BROWSE-1 OPEN QUERY {&BROWSE-NAME} {&BROWSER-A} {&INDEX-PHRASE} {&BROWSER-B}.

WHEN {1} THEN DO:
  IF TRIM(lv-search) NE "" THEN
    IF "{&FLD-NAME-{1}}" MATCHES "*.est-no" THEN
      lv-search = FILL(" ",8 - LENGTH(TRIM(lv-search))) + TRIM(lv-search).
    ELSE
    IF "{&FLD-NAME-{1}}" MATCHES "*.job-no" THEN
      lv-search = FILL(" ",6 - LENGTH(TRIM(lv-search))) + TRIM(lv-search).

  {&open-query-{&browse-name}}

  IF ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = ? THEN
     DO:
        MESSAGE "Record not found beginning with '" + lv-search + "' !!!"
        VIEW-AS ALERT-BOX.
    /*    lv-search:screen-value = "".  */
         APPLY "ENTRY" TO {&BROWSE-NAME}.
     end.    
END.



IF fi_job-no NE "" THEN fi_job-no = FILL(" ",6 - LENGTH(TRIM(fi_job-no)))
                                  + TRIM(fi_job-no).

RELEASE po-ord.
IF fi_po-no NE 0 THEN
FIND po-ord NO-LOCK
    WHERE po-ord.company EQ cocode
      AND po-ord.po-no   EQ fi_po-no
    NO-ERROR.
    
IF AVAIL po-ord THEN DO:
  &SCOPED-DEFINE open-query                         ~
      OPEN QUERY {&browse-name}                     ~
          {&for-each1}                              ~
            AND rm-rcpth.vend-no EQ po-ord.vend-no  ~
            AND rm-rcpth.po-no   EQ TRIM(STRING(po-ord.po-no,">>>>>>>>>>")) ~
              USE-INDEX vend NO-LOCK,               ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.    

ELSE
IF TRIM(fi_rm-i-no) EQ "" AND
  TRIM(fi_tag#) NE "" THEN DO:

  fi_rm-i-no:SCREEN-VALUE = SUBSTR(fi_tag#,1,15).
  
  FIND FIRST loadtag NO-LOCK
      WHERE loadtag.company   EQ cocode
        AND loadtag.item-type EQ NO
        AND loadtag.tag-no    EQ fi_tag#:SCREEN-VALUE
    NO-ERROR.
  IF AVAIL loadtag THEN
    fi_rm-i-no:SCREEN-VALUE = loadtag.i-no.
  ELSE
  FOR EACH rm-rdtlh-1 NO-LOCK
      WHERE rm-rdtlh-1.company EQ cocode
        AND rm-rdtlh-1.tag     BEGINS fi_tag#:SCREEN-VALUE,
      FIRST rm-rcpth-1 FIELDS(i-no) WHERE rm-rcpth-1.r-no EQ rm-rdtlh-1.r-no
      BY rm-rcpth-1.trans-date
      BY rm-rcpth-1.rec_key:
    fi_rm-i-no:SCREEN-VALUE = rm-rcpth-1.i-no.
    LEAVE.
  END.
END.

fi_tag# = fi_tag# + '*'.

IF fi_job-no NE "" THEN DO:
  &SCOPED-DEFINE open-query          ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               ~
              USE-INDEX job NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

ELSE DO:
  &SCOPED-DEFINE open-query           ~
      OPEN QUERY {&browse-name}       ~
          {&for-each1}                ~
              USE-INDEX i-no NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

fi_tag# = SUBSTR(fi_tag#,1,LENGTH(fi_tag#) - 1).

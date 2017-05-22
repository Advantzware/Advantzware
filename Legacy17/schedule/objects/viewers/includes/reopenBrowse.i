/* reopenBrowse.i - used in procedure reopenBrowse in viewersInclude.i */

&SCOPED-DEFINE dateRangePhrase {&startDatePhrase} ~
 BY {&useTtbl}.{1}Field{2} ~{&sorting}

PROCEDURE By{1}Field{2}Ascending:
  &SCOPED-DEFINE sorting 
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

PROCEDURE By{1}Field{2}Descending:
  &SCOPED-DEFINE sorting DESCENDING
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

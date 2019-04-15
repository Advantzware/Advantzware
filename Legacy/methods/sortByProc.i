/* sortByProc.i - rstark - 12.7.2018 */

/* place this include at end of Main Block section                     */
/* scoped-define needed if browse name not {&BROWSE-NAME}              */
/* syntax: &Scoped-define sdBrowseName <Browse Name>                   */
/* syntax: {methods/sortByProc.i "<Procedure Name>" "<Sort By Field>"} */

/* {1} = Procedure Name */
/* {2} = Sort By Field  */

&IF DEFINED(sdBrowseName) EQ 0 &THEN
&Scoped-define sdBrowseName {&BROWSE-NAME}
&ENDIF

PROCEDURE {1}:
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY {2} {&MAX-ROWS}
    {&OPEN-QUERY-{&sdBrowseName}}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY {2} DESCENDING {&MAX-ROWS}
    {&OPEN-QUERY-{&sdBrowseName}}
END PROCEDURE.

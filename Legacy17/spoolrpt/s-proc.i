/* s-proc.i - rstark - 11.7.2013 */

RUN get-values.
RUN run-report.
RUN run-print.

PROCEDURE get-values:
  DEF VAR li AS INT NO-UNDO. 

  FIND FIRST user-print NO-LOCK
       WHERE user-print.company EQ g_company
         AND user-print.program-id EQ v-prgmname  
         AND user-print.batch-seq EQ ip-batch-seq
         AND user-print.batch NE ""
       NO-ERROR.
   
  ASSIGN
    parm-fld-list = ''
    parm-lbl-list = ''
    parm-val-list = ''
    v-prt-name = IF AVAIL user-print THEN user-print.printer-name ELSE ''
    v-copies = IF AVAIL user-print THEN int(user-print.frequency) ELSE 1
    .
  
  IF AVAIL user-print THEN
  DO li = 1 TO EXTENT(user-print.field-name):
    IF INDEX(parm-var-list,user-print.field-name[li]) GT 0 THEN
    ASSIGN
      parm-val-list = parm-val-list + user-print.field-value[li] + ','
      parm-lbl-list = parm-lbl-list
                    + (IF user-print.field-label[li] EQ ? THEN user-print.field-name[li]
                       ELSE user-print.field-label[li])
                    + ','
      parm-fld-list = parm-fld-list + user-print.field-name[li] + ','
      .

    CASE user-print.field-name[li]:
      {spoolrpt/{&s-name}.i}
      {batch/rptvalue.i user-print.field-name[li] "lv-ornt" lv-ornt}
      {batch/rptvalue.i user-print.field-name[li] "lines-per-page" lines-per-page "Integer"}
      {batch/rptvalue.i user-print.field-name[li] "lv-font-no" lv-font-no "Integer"}
      {batch/rptvalue.i user-print.field-name[li] "lv-font-name" lv-font-name}
      {batch/rptvalu2.i user-print.field-name[li] "td-show-parm" td-show-parm "Logical"}
      {batch/rptvalu2.i user-print.field-name[li] "tb_excel" tb_excel "Logical"}
      {batch/rptvalue.i user-print.field-name[li] "fi_file" fi_file}
    END CASE.
  END. /* do */
END PROCEDURE.

PROCEDURE run-print:
  RUN custom/prntbat.p (list-name,INT(lv-font-no),lv-ornt,v-copies,v-prt-name).
END PROCEDURE.

PROCEDURE show-param:
  DEF VAR lv-label AS cha NO-UNDO.
  DEF VAR i AS INT NO-UNDO.

  PUT SPACE(28) '< Selection Parameters >' SKIP(1).
  DO i = 1 TO NUM-ENTRIES(parm-fld-list,','):
    IF (ENTRY(i,parm-fld-list) NE '' OR
        ENTRY(i,parm-lbl-list) NE '') AND
        INDEX(parm-var-list,ENTRY(i,parm-fld-list)) GT 0 THEN DO:
      lv-label = FILL(' ',34 - LENGTH(TRIM(ENTRY(i,parm-lbl-list))))
               + TRIM(ENTRY(i,parm-lbl-list)) + ':'.
      PUT lv-label FORMAT 'x(35)' AT 5 SPACE(1)
          TRIM(ENTRY(i,parm-val-list)) FORMAT 'x(40)' SKIP.
    END. /* if */
 END. /* do */
 PUT FILL('-',80) FORMAT 'x(80)' SKIP.
END PROCEDURE.

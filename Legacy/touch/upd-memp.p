 /*addon/touch/upd-memp.p  */
DEF INPUT PARAM ip-machtran-rowid AS ROWID NO-UNDO.
DEF BUFFER bf-machtran FOR machtran .
DEF BUFFER buf-machemp FOR machemp.

{custom/emprate.i}

FIND bf-machtran WHERE ROWID(bf-machtran) = ip-machtran-rowid NO-LOCK NO-ERROR.
IF NOT AVAIL bf-machtran THEN RETURN.

FOR EACH buf-machemp WHERE buf-machemp.TABLE_rec_key = bf-machtran.rec_key:
    ASSIGN
          buf-machemp.start_date = bf-machtran.start_date
          buf-machemp.start_time = bf-machtran.start_time
          buf-machemp.end_date = bf-machtran.end_date
          buf-machemp.end_time = bf-machtran.end_time.
     /*     buf-machemp.shift = bf-machtran.shift
          buf-machemp.ratetype = 'Standard'
          buf-machemp.rate_usage = employee.rate_usage.
        RUN Employee-Rate(ip-company_code,buf-machemp.employee,buf-machemp.shift,ip-machine_code,
                          buf-machemp.rate_usage,buf-machemp.ratetype,OUTPUT buf-machemp.rate).
     */                          
        {custom/calctime.i &file="buf-machemp"}
END. /* each emplogin */

/* overdate.i  copied from login-logout in touchfrm.w for right date calculation
   YSK 09/20/01
*/
    DEF VAR v-cur-shift AS LOG NO-UNDO.
    DEF VAR v-last-end-date AS DATE NO-UNDO.

    machinecode = emplogin.machine.

    /*when logging out for lunch, don't dock time*/
    IF maxbreak-int GT 0 THEN
    DO:
       FIND FIRST b-emplogin WHERE
            b-emplogin.company = company_code AND
            b-emplogin.employee = employee_code AND
            b-emplogin.machine <> "CLOCK" AND
            b-emplogin.END_date NE ? AND
            b-emplogin.rec_key NE emplogin.rec_key
            USE-INDEX pi-emplogin
            NO-LOCK NO-ERROR.
    
       IF AVAIL b-emplogin THEN
       DO:
          IF emplogin.start_date = b-emplogin.end_date THEN
             li-diff-time = emplogin.START_time - b-emplogin.end_time .
          ELSE
             li-diff-time = (86400 - b-emplogin.end_time)
                          + (emplogin.start_date - b-emplogin.end_date - 1) * 86400
                          +  emplogin.START_time.
         
          if NOT (li-diff-time < 0 OR li-diff-time EQ ?) AND
             li-diff-time GT maxbreak-int then
             stoptime = li-time-no-dock.
          ELSE
             stoptime = li-time.

          RELEASE b-emplogin.
       END.
       ELSE
         stoptime = li-time.
    END.
    ELSE
       stoptime = li-time. 
  
    RUN Get-Shift(company_code,emplogin.machine,stoptime,"END",OUTPUT shiftvar).

    /* shift change, close out current */
    RUN Shift-Data(company_code,emplogin.machine,emplogin.shift,
                   OUTPUT starttime,OUTPUT endtime).
    
    IF tsdocksec-log AND
       employee.dock-time GT 0 AND
       endtime NE li-time-no-dock THEN
       ASSIGN
          v-add-one-sec = TRUE
          endtime = endtime + 1.
    ELSE
       v-add-one-sec = FALSE.

    ASSIGN lv-end-date = if endtime < starttime then lv-end-date + 1 else lv-end-date
           emplogin.end_date =  lv-end-date
           v-last-end-date = emplogin.end_date
           emplogin.end_time = endtime
           machinecode = emplogin.machine.
    {custom/calctime.i &file="emplogin"}
    
    RUN Missing-Shift(company_code,machinecode,emplogin.shift,shiftvar,
                      OUTPUT missingshift).

    FIND FIRST machtran WHERE
         machtran.company = company_code AND
         machtran.machine = machinecode AND
         machtran.END_date EQ ? AND
         machtran.end_time = 0 AND
         machtran.total_time = 0
         NO-LOCK NO-ERROR.

    IF AVAILABLE machtran THEN
    DO:
       FIND FIRST machemp WHERE
            machemp.table_rec_key = machtran.rec_key AND
            machemp.employee = employee_code AND
            machemp.END_date EQ ? AND
            machemp.end_time = 0 AND
            machemp.total_time = 0
            EXCLUSIVE-LOCK NO-ERROR.

       IF AVAILABLE machemp THEN
       DO:
          ASSIGN
             machemp.end_date = machemp.start_date
             machemp.end_time = 86399.

          {custom/calctime.i &file="machemp"}
          FIND CURRENT machemp NO-LOCK.

          CREATE bf-machemp.
          BUFFER-COPY machemp EXCEPT machemp.rec_key TO bf-machemp
              ASSIGN bf-machemp.start_date = IF missingshift NE '' THEN lv-end-date
                                             ELSE machemp.end_date + 1
                     bf-machemp.start_time = 0
                     bf-machemp.ratetype = 'Standard'
                     bf-machemp.rate_usage = employee.rate_usage
                     bf-machemp.end_time = endtime
                     bf-machemp.END_date = bf-machemp.start_date.

          {custom/calctime.i &file="bf-machemp"}
          RUN Employee-Rate(company_code,bf-machemp.employee,bf-machemp.shift,machinecode,
                            bf-machemp.rate_usage,bf-machemp.ratetype,OUTPUT bf-machemp.rate).
          RELEASE bf-machemp.
       END.

       
    END. /*AVAILABLE machtran*/

    IF missingshift NE '' THEN /* skipped a shift */
    DO:
       /* create record for skipped shift */
       RUN Shift-Data(company_code,machinecode,missingshift,
                      OUTPUT starttime,OUTPUT endtime).
      
       IF v-add-one-sec THEN
          endtime = endtime + 1.
      
       CREATE emplogin.
       ASSIGN
         emplogin.company = company_code
         emplogin.employee = employee_code
         emplogin.machine = machinecode
         emplogin.start_date = lv-end-date
         emplogin.start_time = starttime
         emplogin.end_date = if endtime < starttime then lv-end-date + 1 else lv-end-date
         emplogin.end_time = endtime
         emplogin.shift = missingshift
         v-last-end-date = emplogin.end_date.
       {custom/calctime.i &file="emplogin"}

       FIND FIRST machtran WHERE
            machtran.company = company_code AND
            machtran.machine = machinecode AND
            machtran.END_date EQ ? AND
            machtran.end_time = 0 AND
            machtran.total_time = 0
            NO-LOCK NO-ERROR.

       IF AVAIL machtran AND
          NOT CAN-FIND(FIRST machemp WHERE
              machemp.table_rec_key = machtran.rec_key AND
              machemp.employee = employee_code AND
              machemp.start_date = lv-end-date AND
              machemp.start_time = starttime) THEN
              DO:
                 CREATE machemp.
                 ASSIGN
                   machemp.table_rec_key = machtran.rec_key
                   machemp.employee = employee_code
                   machemp.start_date = lv-end-date
                   machemp.start_time = starttime
                   machemp.end_date = if endtime < starttime then lv-end-date + 1 else lv-end-date
                   machemp.end_time = endtime
                   machemp.shift = missingshift
                   machemp.ratetype = 'Standard'
                   machemp.rate_usage = employee.rate_usage.
                 RUN Employee-Rate(company_code,employee_code,machemp.shift,machinecode,
                                   machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
                 {custom/calctime.i &file="machemp"}
                 RELEASE machemp.
              END.
    END.
    /* create record for current shift */
    RUN Get-Shift(company_code,emplogin.machine,stoptime,"END",OUTPUT shiftvar).

    IF shiftvar <> emplogin.shift THEN  do:

       v-cur-shift = YES.

       RUN Shift-Data(company_code,machinecode,shiftvar,
                   OUTPUT starttime,OUTPUT endtime).

       IF NOT(v-add-one-sec AND starttime EQ stoptime) THEN
       DO:
          CREATE emplogin.
          ASSIGN emplogin.company = company_code
                 emplogin.employee = employee_code
                 emplogin.machine = machinecode
                 emplogin.start_date = /*if missingshift <> "" and starttime < stoptime
                                       then lv-end-date + 1 else lv-end-date */
                                       /*IF stoptime < starttime THEN lv-end-date + 1 ELSE lv-end-date*/
                                       v-last-end-date
                 emplogin.start_time = starttime
                 emplogin.end_date = v-today /*if endtime >= starttime then lv-end-date + 1 else lv-end-date*/
                 emplogin.end_time = stoptime
                 emplogin.shift = shiftvar.
          {custom/calctime.i &file="emplogin"}

          FIND FIRST machtran WHERE
               machtran.company = company_code AND
               machtran.machine = machinecode AND
               machtran.END_date EQ ? AND
               machtran.end_time = 0 AND
               machtran.total_time = 0
               NO-LOCK NO-ERROR.
          
          IF AVAIL machtran THEN
          DO:
             CREATE machemp.
             ASSIGN
               machemp.table_rec_key = machtran.rec_key
               machemp.employee = employee_code
               machemp.start_date = /*if missingshift <> "" and starttime < stoptime
                                      then lv-end-date + 1 else lv-end-date*/
                                    /*IF stoptime < starttime THEN lv-end-date + 1 ELSE lv-end-date*/
                                    v-last-end-date
               machemp.start_time = starttime
               machemp.end_date = v-today
               machemp.end_time = stoptime
               machemp.shift = shiftvar
               machemp.ratetype = 'Standard'
               machemp.rate_usage = employee.rate_usage.
             RUN Employee-Rate(company_code,employee_code,machemp.shift,machinecode,
                               machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
             {custom/calctime.i &file="machemp"}

             RELEASE machemp.
          END.
       END.
    end.

    /*Handles situation where login 11:55 PM, logout 12:00 AM next day,
      all in one shift*/
    IF missingshift EQ '' AND v-cur-shift EQ NO THEN
    DO:
       ASSIGN
          emplogin.end_time = 0
          emplogin.END_date = v-today.

       {custom/calctime.i &file="emplogin"}

       IF NOT CAN-FIND(FIRST emplogin WHERE 
          emplogin.company EQ company_code AND
          emplogin.employee EQ employee_code AND
          emplogin.start_date EQ v-today AND
          emplogin.start_time EQ 0 AND
          emplogin.machine EQ machinecode) THEN
          DO:
             CREATE emplogin.
             ASSIGN emplogin.company = company_code
                    emplogin.employee = employee_code
                    emplogin.machine = machinecode
                    emplogin.start_date = v-today 
                    emplogin.start_time = 0
                    emplogin.end_date = v-today
                    emplogin.end_time = stoptime
                    emplogin.shift = shiftvar.
             {custom/calctime.i &file="emplogin"}
          END.
    END.
      
    /* get active machine if it exists and logout of machine */
    FIND FIRST machtran
         WHERE machtran.company = company_code
           AND machtran.machine = machinecode
           AND machtran.END_date EQ ?
           AND machtran.end_time = 0
           AND machtran.total_time = 0 NO-LOCK NO-ERROR.
    IF AVAILABLE machtran THEN
    DO:
       FIND FIRST machemp WHERE
            machemp.table_rec_key = machtran.rec_key AND
            machemp.employee = employee_code AND
            machemp.END_date EQ ? AND
            machemp.end_time = 0 AND
            machemp.total_time = 0
            EXCLUSIVE-LOCK NO-ERROR.
      
       IF AVAILABLE machemp THEN
       DO:
          machemp.end_date = lv-end-date.

          RUN Get-Shift(company_code,machinecode,stoptime,"END",OUTPUT shiftvar).
         
          IF shiftvar <> emplogin.shift THEN
          DO:
             /* shift change, close out current */
             FIND employee WHERE employee.company = company_code
                             AND employee.employee = employee_code NO-LOCK NO-ERROR.
             RUN Shift-Data(company_code,machinecode,machemp.shift,
                            OUTPUT starttime,OUTPUT endtime).
             
             IF tsdocksec-log AND
                employee.dock-time GT 0 AND
                endtime NE li-time-no-dock THEN
                ASSIGN
                   v-add-one-sec = TRUE
                   endtime = endtime + 1.
             ELSE
                v-add-one-sec = FALSE.
             
             machemp.end_time = endtime.
             
             {custom/calctime.i &file="machemp"}
             RUN Missing-Shift(company_code,machinecode,machemp.shift,shiftvar,
                               OUTPUT missingshift).

             IF missingshift NE '' THEN /* skipped a shift */
             DO: /* create record for skipped shift */
                RUN Shift-Data(company_code,machinecode,missingshift,
                               OUTPUT starttime,OUTPUT endtime).
               
                IF NOT CAN-FIND(FIRST machemp WHERE
                   machemp.table_rec_key = machtran.rec_key AND
                   machemp.employee = employee_code AND
                   machemp.start_date = lv-end-date AND
                   machemp.start_time = starttime) THEN
                   DO:
                      IF v-add-one-sec THEN
                         endtime = endtime + 1.
               
                      CREATE machemp.
                      ASSIGN
                        machemp.table_rec_key = machtran.rec_key
                        machemp.employee = employee_code
                        machemp.start_date = lv-end-date
                        machemp.start_time = starttime
                        machemp.end_date = if endtime < starttime then lv-end-date + 1 else lv-end-date
                        machemp.end_time = endtime
                        machemp.shift = missingshift
                        machemp.ratetype = 'Standard'
                        machemp.rate_usage = employee.rate_usage.
                      RUN Employee-Rate(company_code,employee_code,machemp.shift,machinecode,
                                        machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
                      {custom/calctime.i &file="machemp"}
                   END.
             END.
             /* create record for current shift */
             
             RUN Shift-Data (company_code,machine_code,shiftvar,
                           OUTPUT starttime,OUTPUT endtime).
             
             v-tmp-start-date = if missingshift <> "" and starttime < stoptime
                                then lv-end-date + 1  else lv-end-date.
             
             IF NOT(v-add-one-sec AND starttime EQ stoptime AND
                v-today EQ v-tmp-start-date) AND
                NOT CAN-FIND(FIRST machemp WHERE
                machemp.table_rec_key = machtran.rec_key AND
                machemp.employee = employee_code AND
                machemp.start_date = v-tmp-start-date AND
                machemp.start_time = starttime) THEN
                DO:
                   CREATE machemp.
                   ASSIGN
                   machemp.table_rec_key = machtran.rec_key
                   machemp.employee = employee_code
                   machemp.start_date = v-tmp-start-date
                   machemp.start_time = starttime
                   machemp.end_date = v-today /*if endtime >= starttime then lv-end-date + 1 else lv-end-date */
                   machemp.end_time = stoptime
                   machemp.shift = shiftvar
                   machemp.ratetype = 'Standard'
                   machemp.rate_usage = employee.rate_usage.
                   RUN Employee-Rate(company_code,employee_code,machemp.shift,machinecode,
                                     machemp.rate_usage,machemp.ratetype,OUTPUT machemp.rate).
                   {custom/calctime.i &file="machemp"}
                END.
             /*end.*/
          
             {custom/calctime.i &file="machemp"}
          END. /*shiftvar <> emplogin.shift*/
          ELSE
          DO:
             ASSIGN
                machemp.end_time = stoptime
                machemp.END_date = v-today.

             {custom/calctime.i &file="machemp"}
          END.

          RELEASE machemp.
       END. /* avail machemp */
    END. /* avail machtran */

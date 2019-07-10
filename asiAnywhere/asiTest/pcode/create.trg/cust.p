&Scoped-define TABLENAME cust

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.

{methods/triggers/create.i}

{custom/globdefs.i}

ASSIGN
 {&TABLENAME}.company = g_company
 {&TABLENAME}.cust-no = FILL(" ",100) + STRING(USERID("nosweat"),"x(20)") + STRING(TIME,"9999999999")
 {&TABLENAME}.loc     = g_loc
 {&TABLENAME}.active  = "A".

FIND FIRST b-{&TABLENAME}
    WHERE b-{&TABLENAME}.company EQ g_company
      AND b-{&TABLENAME}.active  EQ "X"
    NO-LOCK NO-ERROR.
IF AVAIL b-{&TABLENAME} THEN
  BUFFER-COPY b-{&TABLENAME} EXCEPT company loc cust-no active rec_key
                                    name addr city state zip date-field
                                    area-code phone fax email ytd-sales
                                    lyr-sales cost comm ytd-msf lyytd-msf
                                    hibal hibal-date num-inv lpay lpay-date
                                    avg-pay ord-bal acc-bal on-account 
                             TO {&TABLENAME}.

ELSE DO:
  ASSIGN
   {&TABLENAME}.over-pct   = 10
   {&TABLENAME}.under-pct  = 10
   {&TABLENAME}.cust-level = 1
   {&TABLENAME}.fob-code   = "DEST"
   {&TABLENAME}.frt-pay    = "P".

  FIND FIRST company WHERE company.company EQ {&TABLENAME}.company NO-LOCK NO-ERROR.
  IF AVAIL company THEN {&TABLENAME}.curr-code = company.curr-code.
END.

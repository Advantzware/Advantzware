/* custom/asifax.i FAx include file using Window Fax Console */
DEF VAR fax-ret-code AS INT NO-UNDO.
/*DEF VAR ls-fax-file AS cha NO-UNDO. defined from report generating program */
DEF VAR lv-type AS cha NO-UNDO.

IF "{&TYPE}" = "CUSTOMER"  THEN DO:
   IF {&begin_cust} <> {&end_cust} THEN DO:
       MESSAGE "Beginning Customer and Ending Customer must be the same for Fax."
                    VIEW-AS ALERT-BOX ERROR.
              APPLY "entry" TO {&END_cust} .
              RETURN NO-APPLY.
   END.
   lv-type = "CUSTOMER".
END.
ELSE IF "{&TYPE}" = "VENDOR" THEN DO:
   IF {&begin_cust} <> {&end_cust} THEN DO:
      MESSAGE "Beginning Vendor and Ending Vendor must be the same for E-mail."
                    VIEW-AS ALERT-BOX ERROR.
              APPLY "entry" TO {&END_cust} .
              RETURN NO-APPLY.
   END.
   lv-type = "VENDOR".
END.
ELSE IF "{&TYPE}" = "MULTI" THEN DO:
   lv-type = "MULTI".
END.
ELSE lv-type = "".


ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".txt". 
OS-COPY VALUE({&fax-file}) VALUE(ls-fax-file).

run custom/asifax.p (lv-type,ls-fax-file,{&begin_cust},
                                '{&fax-subject}',
                                '{&fax-body}',OUTPUT fax-ret-code).
IF fax-ret-code <> 0 THEN
     MESSAGE "Error: " fax-ret-code view-as alert-box ERROR.

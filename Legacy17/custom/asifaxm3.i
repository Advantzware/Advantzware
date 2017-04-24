/* custom/asifaxm.i FAx include file using Window Fax Console */
DEF VAR fax-ret-code2 AS INT NO-UNDO.
DEF VAR ls-fax-file2 AS cha NO-UNDO.
DEF VAR lv-type2 AS cha NO-UNDO.

IF "{&TYPE}" = "CUSTOMER"  THEN DO:
   IF {&begin_cust} <> {&end_cust} THEN DO:
       MESSAGE "Beginning Customer and Ending Customer must be the same for Fax."
                    VIEW-AS ALERT-BOX ERROR.
              APPLY "entry" TO {&end-widget} IN FRAME {&FRAME-NAME}.
              RETURN NO-APPLY.
   END.
   lv-type2 = "CUSTOMER".
END.
ELSE IF "{&TYPE}" = "VENDOR" THEN DO:
   IF {&begin_cust} <> {&end_cust} THEN DO:
      MESSAGE "Beginning Vendor and Ending Vendor must be the same for E-mail."
                    VIEW-AS ALERT-BOX ERROR.
              APPLY "entry" TO {&end-widget} IN FRAME {&FRAME-NAME} .
              RETURN NO-APPLY.
   END.
   lv-type2 = "VENDOR".
END.
ELSE IF "{&TYPE}" = "MULTI" THEN DO:
   lv-type2 = "MULTI".
END.
ELSE lv-type2 = "".

ls-fax-file2 = "c:\tmp\fax" + STRING(TIME) + ".txt".

OS-COPY VALUE({&fax-file}) VALUE(ls-fax-file2).

run custom/asifax.p (lv-type2,ls-fax-file2,{&begin_cust},
                                '{&fax-subject}',
                                '{&fax-body}',OUTPUT fax-ret-code2).

IF fax-ret-code2 <> 0 THEN
     MESSAGE "Error: " fax-ret-code2 view-as alert-box ERROR.

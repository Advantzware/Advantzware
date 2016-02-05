/*util/cvtpoven.p Populate po-ordl.vend-no*/

DISABLE TRIGGERS FOR LOAD OF po-ordl.

MESSAGE "Are you ready to Populate P.O. Line Item Vendor #?" VIEW-AS ALERT-BOX
    QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.


IF ll-ans THEN
DO:
   SESSION:SET-WAIT-STATE ("general").

   FOR EACH po-ord NO-LOCK,
       EACH po-ordl WHERE
            po-ordl.company EQ po-ord.company AND
            po-ordl.po-no EQ po-ord.po-no:

       po-ordl.vend-no = po-ord.vend-no.
   END.

   MESSAGE "Process Completed. " VIEW-AS ALERT-BOX.
END.







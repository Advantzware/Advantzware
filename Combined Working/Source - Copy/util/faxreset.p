DEF VAR lv-fax-area AS cha NO-UNDO.
lv-fax-area = "205".
UPDATE "Enter Area Code to Reset" lv-fax-area WITH FRAME upd NO-LABEL.
/*
FOR EACH vend.
    IF vend.fax <> "" THEN substring(vend.fax,1,3) = "215".
    DISP vend.fax.
END.
  */

MESSAGE "Are you sure you want to blank out all area code for Customers and Vendors?"
    VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans AS LOG.

IF ll-ans THEN DO:


FOR EACH cust.
    IF LENGTH(trim(cust.fax)) = 10 THEN DO:
       IF SUBSTRING(cust.fax,1,3) = lv-fax-area THEN
          SUBSTRING(cust.fax,1,3) = "   ".
       DISP cust.cust-no cust.fax LENGTH(trim(cust.fax)).
       PAUSE 0.
    END.
    
END.


FOR EACH vend.
    IF vend.fax-area = lv-fax-area THEN DO:
       DISP vend.vend-no vend.fax-area vend.fax.
       vend.fax-area = "".
       PAUSE 0.
    END.
    
END.

END.



  DEF VAR cPrimComp AS CHAR NO-UNDO.
  DEF VAR iNextNum AS INT NO-UNDO.
  DEF VAR li-nxt-r-no AS INT NO-UNDO.
  DEF BUFFER bf-rctd FOR rm-rctd.
 
 message "Enter the primary company number:" update cPrimComp.
 
  
  FIND FIRST company WHERE company.company = cPrimComp NO-LOCK NO-ERROR.
  IF NOT AVAIL(company) THEN DO:
      MESSAGE "Invalid company code entered"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

  IF AVAIL(company)  THEN DO:
      find last po-ord where po-ord.company eq cPrimComp 
          NO-LOCK NO-ERROR.
     IF AVAIL po-ord THEN
     DYNAMIC-CURRENT-VALUE("po_Seq" + company.spare-char-1, "ASI") = po-ord.po-no + 1.
  END.

 
  
  MESSAGE "Process Complete! Only run this once."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

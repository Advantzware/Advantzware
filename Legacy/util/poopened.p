
SESSION:SET-WAIT-STATE ("general").

FOR EACH po-ord:
  po-ord.opened = po-ord.stat EQ "C".
      
  FOR EACH po-ordl
      WHERE po-ordl.company EQ po-ord.company
        AND po-ordl.po-no   EQ po-ord.po-no:
    
    po-ordl.opened = po-ord.opened.
  END.
END.

SESSION:SET-WAIT-STATE ("").

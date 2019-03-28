/* Local delete for po-ord */
FOR EACH po-ordl WHERE po-ordl.company EQ po-ord.company
   AND po-ordl.po-no EQ po-ord.po-no 
   NO-LOCK:
   /* 25447 - Removing Reftable reference */
END.

def input parameter iprOeRel as rowid no-undo. 
def output parameter oprShipTo as rowid no-undo.  
def buffer bf-oe-rel for oe-rel.  
def buffer bf-cust for cust.  
  find bf-oe-rel where rowid(bf-oe-rel) eq iprOeRel no-lock no-error.  
  IF AVAIL bf-oe-rel THEN do:  
    find first bf-cust where bf-cust.company eq bf-oe-rel.company 
      and bf-cust.cust-no eq bf-oe-rel.cust-no no-lock no-error.  
    FIND shipto
      WHERE shipto.company EQ bf-oe-rel.company
        AND shipto.cust-no EQ bf-oe-rel.cust-no
        AND shipto.ship-id EQ bf-oe-rel.ship-id
      NO-LOCK NO-ERROR.
  IF NOT AVAIL shipto THEN
    FIND FIRST shipto 
      WHERE shipto.company EQ bf-cust.company
        AND shipto.cust-no EQ bf-cust.cust-no
        AND shipto.ship-id EQ bf-cust.cust-no
      NO-LOCK NO-ERROR.  
     
  if avail shipto then
    oprShipTo = rowid(shipto).  
  end.

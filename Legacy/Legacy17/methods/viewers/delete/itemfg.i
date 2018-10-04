/* itemfg.i */


define buffer bf-poline for po-ordl.

IF  itemfg.i-no <> "" THEN
DO:
   If can-find(first bf-poline where bf-poline.company = itemfg.company
                                    and bf-poline.i-no    = itemfg.i-no 
                                    and bf-poline.opened  = yes) then DO:
       message 'Cannot delete item with open purchase orders' view-as alert-box error.
       return error.
   end.
END.

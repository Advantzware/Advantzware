/* itemfg.i */


define buffer bf-poline for po-ordl.
define buffer bf-oe-ordl for oe-ordl.
IF  itemfg.i-no <> "" THEN
DO:
   If can-find(first bf-poline where bf-poline.company = itemfg.company
                                    and bf-poline.i-no    = itemfg.i-no 
                                    and bf-poline.opened  = yes) then DO:
       message 'Cannot delete item with open purchase orders' view-as alert-box error.
       return error.
   end.
   If can-find(first bf-oe-ordl where bf-oe-ordl.company = itemfg.company
                                    and bf-oe-ordl.i-no    = itemfg.i-no ) then DO:
       message 'Cannot delete item with Orders.' skip
               'You can make it Inactive...'      view-as alert-box info.
       return error.
   end.
END.

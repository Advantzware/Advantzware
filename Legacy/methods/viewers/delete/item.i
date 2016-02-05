/* item.i */

DEF BUFFER bf-item for item.
define buffer bf-poline for po-ordl.

IF ITEM.i-no <> "" THEN DO:
   If can-find(first bf-poline where bf-poline.company = item.company
                                      and bf-poline.i-no    = item.i-no
                                      and bf-poline.opened  = yes) then DO:
      message 'Cannot delete item with open purchase orders' view-as alert-box error.
      return error.
   END.
end.

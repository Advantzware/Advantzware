/* ------------------------------------------------- rm/itemcopy.i 1/95 CTS   */
/* raw material item file copy record include                                 */
/* -------------------------------------------------------------------------- */
/*
{sys/inc/var.i shared}

def buffer xitem for item.
def buffer xe-item for e-item.
def buffer xe-item-v for e-item-vend.
def buffer xstack-s for stack-size.
def buffer xstack-f for stack-flute.

def var v-i-no like item.i-no no-undo.
def var to-i-no like item.i-no no-undo.
def var v-sure as log init true.

def shared var v-new-i-no like item.i-no no-undo.
                                             /* From AutoRM to create new RM */
def shared var v-new-wid like item.s-wid init 0 no-undo.
def shared var v-new-len like item.s-len init 0 no-undo.

find item where recid(item) eq fil_id exclusive-lock no-error.

if avail item then do:
  if v-new-i-no eq "" then do:
    hide message no-pause.
    message " Copy Item" item.i-no "to Item" update to-i-no.
    hide message no-pause.
  end.
  
  else to-i-no = v-new-i-no.
  
  if to-i-no ne "" then do:
    v-sure = true.
    if v-new-i-no eq "" then do:
      message " About to Copy" item.i-no "to" to-i-no ". Are you Sure?"
              update v-sure.
      hide message no-pause.
    end.
    
    if v-sure then do with frame f-itemcopy:
      v-i-no = item.i-no.

      find first xitem
          where xitem.company eq cocode
            and xitem.i-no    eq to-i-no
          no-lock no-error.
      if avail xitem then do:
        bell.
        message "Item" to-i-no
                "Already Exists. Please Copy to Different Item.".
        pause.
        leave.
      end.
      
      for each e-item
          where e-item.company eq cocode
            and e-item.i-no    eq to-i-no:
        delete e-item.
      end.
         
      for each e-item-vend
          where e-item-vend.company eq cocode
            and e-item-vend.i-no    eq to-i-no:
        delete e-item-vend.
      end.     

      for each e-item of item no-lock:
        create xe-item.
        buffer-copy e-item to xe-item
        assign
         xe-item.i-no = to-i-no.
      end.
        
      for each e-item-vend of item no-lock:
        create xe-item-v.
        buffer-copy e-item-vend to xe-item-v
        assign
         xe-item-v.i-no = to-i-no.
      end.

      create xitem.
      buffer-copy item to xitem
      assign
       xitem.i-no     = to-i-no
       xitem.q-onh    = 0
       xitem.q-ono    = 0
       xitem.q-comm   = 0
       xitem.q-back   = 0
       xitem.q-avail  = 0
       xitem.pur-uom  = if item.pur-uom  EQ "" then "MSF"
                                               else item.pur-uom
       xitem.cons-uom = if item.cons-uom EQ "" then "EA"
                                               else item.cons-uom.

      if v-new-i-no ne "" then do:
        assign
         xitem.pur-uom   = if item.pur-uom EQ "MSF" then "MSF"
                           else if item.pur-uom EQ "MSH" then "M"
                           else item.pur-uom
         xitem.cons-uom  = if item.cons-uom EQ "MSF" then "EA"
                           else if item.cons-uom EQ "TON" then "LF"
                           else if item.cons-uom EQ "MSH" then "EA"
                           else item.cons-uom
         xitem.pur-man   = TRUE
         xitem.i-code    = "R"
         xitem.pur-rcode = "Estimate"
         xitem.s-wid     = v-new-wid
         xitem.s-len     = v-new-len.
      end.
      
      else do:
        message "Update Item Purchase UOM" update item.pur-uom.
        message "Update Item Consumption UOM" update item.cons-uom.
      end.
      
      find item where recid(item) eq recid(xitem) no-lock no-error.
      
      fil_id = if avail item then recid(item) else ?.

      {rm/cp-pall.i}
    end.
  end.
end.
*/
/* end ---------------------------------- copr. 1992  advanced software, inc. */

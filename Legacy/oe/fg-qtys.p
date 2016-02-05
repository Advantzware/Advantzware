/* --------------------------------------------------- oe/fg-qtys.p 11/96 JLF */
/*                                                                            */
/* Order entry Lines - o/e module - FG quantities                             */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input parameter v-recid as   recid.

{sys/inc/var.i shared}

def var v as INT NO-UNDO.
def var kk as INT NO-UNDO.
def var v-alloc like itemfg.q-alloc NO-UNDO.
def var v-type as CHAR NO-UNDO.

def shared frame fg-qtys.

FIND first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

{sys/inc/oereordr.i}

find itemfg where recid(itemfg) eq v-recid no-lock no-error.
  
IF oereordr-log OR oereordr-log EQ ? THEN
   RUN oe/oereordr.p (BUFFER itemfg, INPUT oereordr-log, OUTPUT v-alloc).
ELSE v-alloc = itemfg.q-alloc.

do with frame fg-qtys:
  /*color display value(col-head) fg-qtys-h[2] fg-qtys-h[3].

  color display value(col-norm)
        itemfg.q-onh
        itemfg.q-ono
        itemfg.q-alloc
        itemfg.q-back
        itemfg.q-avail
        itemfg.ord-level.

  display itemfg.i-no
          fg-qtys-h[2]
          fg-qtys-h[3]
          itemfg.q-onh
          itemfg.q-ono
          v-alloc @ itemfg.q-alloc
          itemfg.q-back
          (itemfg.q-onh + itemfg.q-ono - v-alloc) @ itemfg.q-avail
          itemfg.ord-level.
          
  if itemfg.q-onh + itemfg.q-ono - v-alloc le itemfg.ord-level and
     itemfg.ord-level gt 0                                     then do:
     
    color display value ("White/Red") itemfg.ord-level.
    
    do kk = 1 to 1:
      itemfg.ord-level:hidden = yes.
      pause .1 no-message.
      itemfg.ord-level:visible = yes.
      pause 1 no-message.
    end.
  end. */
end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */


/* sman_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'sman_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="sman" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="sman" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="sman" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="sman" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

def var i as int no-undo.
def var v-first as log no-undo.


DISPLAY
  sman.sman LABEL "Sales Rep"
  sman.sname LABEL "Sales Rep Name"
  sman.commbasis
  sman.territory
  sman.scomm.
  
if tb_matrix then
for each sman-mtx
    where sman-mtx.company eq sman.company
      and sman-mtx.sman    eq sman.sman
    no-lock
    
    WITH FRAME sman-mtx down WIDTH 132 STREAM-IO:
    
  find first custype
      where custype.company eq sman-mtx.company
        and custype.custype eq sman-mtx.custype
      no-lock no-error.
    
  display sman-mtx.custype at 10
          custype.dscr when avail custype
          sman-mtx.type-comm label "Commission".
           
  v-first = yes.
           
  do i = 1 to 10:
    if sman-mtx.procat[i] ne "" then do:
      if v-first then put skip(1).
           
      v-first = no.
     
      display sman-mtx.procat[i] at 19
              sman-mtx.dscr[i]
              sman-mtx.comm[i]

          WITH FRAME sman-mtx2 down NO-BOX WIDTH 132 STREAM-IO.
      down WITH FRAME sman-mtx2.                        
    end.
  end.
  
  if not v-first then put skip(1).        
end.
    
{methods/lstlogic/shownote.i &db_table="sman" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="sman" &col="5" &frame-name="f-miscflds"}

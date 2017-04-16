/* estbsiz_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'estbsiz_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="eb" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="eb" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="eb" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="eb" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

/* --------------------------------------------- sys/ref/estbysiz.p 03/01 JLF */
/* Estimates by Size Report                                                   */
/* -------------------------------------------------------------------------- */

{custom/gcompany.i}

def var cocode      as   char                                       no-undo.
def var v-indus     as   char format "!"     init "B"               no-undo.
def var v-cust      like eb.cust-no extent 2 init ["","zzzzzzzz"]   no-undo.
def var v-styl      like eb.style   extent 2 init ["","zzzz"]       no-undo.
def var v-flut      like eb.flute   extent 2 init ["","zzz"]        no-undo.
def var v-test      like eb.test    extent 2 init ["","zzzzzz"]     no-undo.

def var v-indtype   as   char format "x(15)".

{custom/getcmpny.i}

form header "Industry:"
            v-indtype
            skip(1)
            
    with no-box no-attr-space width 132 page-top frame r-top.


assign
 v-cust[1] = begin_cust-no
 v-cust[2] = end_cust-no
 v-styl[1] = begin_eb_style
 v-styl[2] = end_eb_style
 v-flut[1] = begin_eb_flute
 v-flut[2] = end_eb_flute
 v-test[1] = begin_eb_test
 v-test[2] = end_eb_test
 v-indus   = rd-industry
 cocode    = gcompany.

for each eb
    where eb.company eq cocode
      and eb.cust-no ge v-cust[1]
      and eb.cust-no le v-cust[2]
      and eb.style   ge v-styl[1]
      and eb.style   le v-styl[2]
      and eb.flute   ge v-flut[1]
      and eb.flute   le v-flut[2]
      and eb.test    ge v-test[1]
      and eb.test    le v-test[2]
      and (v-indus   eq "B"                       or
           (v-indus  eq "C" and eb.est-type ge 5) or
           (v-indus  eq "F" and eb.est-type le 4))
    no-lock,
      
    first ef
    where ef.e-num   eq eb.e-num
      and ef.form-no eq eb.form-no
    no-lock,
      
    first est
    where est.e-num eq eb.e-num
    no-lock
      
    break by (if eb.est-type ge 5 then 1 else 2)
          by eb.len
          by eb.wid
          by eb.dep
          by eb.style
          by ef.board
          by eb.cust-no
          by eb.est-no:
            
  if first-of(if eb.est-type ge 5 then 1 else 2) then do:
    hide frame r-top no-pause.
    v-indtype = if eb.est-type ge 5 then "Corrugated" else "Folding".
    view frame r-top.
    page.
  end.
    
  find first cust
      where cust.company eq cocode
        and cust.cust-no eq eb.cust-no
      no-lock no-error.
      
  display eb.len                      column-label "Length"
          eb.wid                      column-label "Width"
          eb.dep                      column-label "Depth"
          eb.style                    column-label "Style"
          ef.board                    column-label "Material"
          ef.adder[1]                 column-label "Adder 1"
          ef.adder[2]                 column-label "Adder 2"
          ef.adder[3]                 column-label "Adder 3"
          eb.part-no                  column-label "Customer Part#"
          trim(est.est-no)            format "x(5)"
                                      column-label "Est#"
          est.ord-date                column-label "Last Ord!  Date"
          eb.ship-name                column-label "Customer Name"
                                      format "x(24)"
                                                      
        with down no-box width 132.
end.

/* end ---------------------------------- copr. 2001  advanced software, inc. */

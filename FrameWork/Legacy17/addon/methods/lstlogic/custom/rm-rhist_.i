/* rm-rhist_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'rm-rhist_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="item" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="item" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="item" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="item" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

/*DISPLAY
  item.i-no. */
  
/* ---------------------------------------------- rm/rep/rm-rhist.p 06/00 DJK */
/* raw materials - transactions edit list                                     */
/* -------------------------------------------------------------------------- */

/*{sys/inc/var.i shared}
  {sys/form/s-top.f}*/

def var save_id as recid.

def var time_stamp as char.
time_stamp = string(time, "hh:mmam").

def var v-fitem like rm-rcpth.i-no.
def var v-titem like v-fitem                  init "zzzzzzzzzz".
def var v-fpcat like item.procat.
def var v-tpcat like v-fpcat                  init "zzzzz".
def var v-fdate as   date format "99/99/9999".
def var v-tdate like v-fdate                  init today.
def var v-floc  like rm-rcpth.loc.
def var v-tloc  like v-floc                   initial "zzzzz".
def var v-type  as   char format "x(5)"       init "RITAC".
def var v-code  like rm-rcpth.rita-code.
def var v-job-no as char format "x(9)".
def var v-fvend like item.vend-no.
def var v-tvend like item.vend-no init "zzzzzzzzzz".
def var v-msf as dec format  "->,>>>,>>9.9999".
def var v-tot-msf-item as dec format   "->>>,>>>,>>9.99".
def var v-tot-msf-day as dec format   "->>>,>>>,>>9.99".
def var v-grand-tot-msf as dec format "->>>,>>>,>>9.99".
def var v-mtype as char format "x(47)".
def var v-sqft as dec.
def var v-tot-sqft as dec.
def var jmctr as int.
def var dayctr as int.
def var itemctr as int.
def var v-tot-qty like rm-rdtlh.qty format "->>>,>>>,>>9.99".
def var v-grand-tot-qty like rm-rdtlh.qty format "->>>,>>>,>>9.99".
def var v-len like po-ordl.s-len.
def var v-wid like po-ordl.s-wid.
def var str-tit4 like str-tit3.
def var str-tit5 like str-tit3.
def var str-tit6 like str-tit3.
def var str-tit7 like str-tit3.

for each mat no-lock:
  v-mtype = v-mtype + mat.mat.
end.

{sys/form/r-top5w.f}

form skip(1)
     "From Date:"         to 20 v-fdate      help " Enter Starting Date "
     "To Date:" to 57 v-tdate       help " Enter Ending Date " skip(1)
     "From Item:"         to 20 v-fitem       help  " Enter Starting Raw Material Item "
     "To Item:" to 57 v-titem       help  " Enter Ending Raw Material Item " skip(1) 
     "From Vendor:"     to 20 v-fvend     help  " Enter Starting Vendor "
     "To Vendor:" to 57 v-tvend   help   " Enter Ending Vendor " skip(1)
     "Material Types:" to 20  v-mtype  help " Enter Raw Material Type(s) " skip(1)
     "1 = Polyethylene"                     at 6       "F = Foil / Stamping"  at 33      "T = Tape"  at 56 skip 
     "2 = Polyurethane"                    at 6        "G = Glue"                 at 33     "V = Varnish & Coating" at 56 skip   
     "3 = Expanded Polystyrene"      at 6       "I = Ink"                      at 33     "W = Wax & Window"    at 56 skip  
     "4 = Polystyrene"                       at 6      "J = Banding"              at 33      "Z = Truck Trailer" at 56 skip   
     "9 = Wood"                               at 6       "L = Laminate"            at 33                                    skip   
     "A = Adders"                             at 6       "M = Misc Materials"    at 33                                  skip   
     "B = Board"                               at 6     "P = PVC / Paper"         at 33                                  skip   
     "C = Cases / Bundles"              at 6      "R = Wrap for Rigid Boxes"     at 33                                    skip
     "D = Pallet / Bales"                   at 6       "S = Stitching"              at 33                                    skip
    with title "  R A W     M A T E R I A L     R E C E I P T S  "
         frame selec row 3 centered  no-labels width 79
         color value(col-title) prompt value(col-input).
form 
     rm-rcpth.trans-date                            label "Date"                    /*8*/
     rm-rcpth.i-no                                      label "RM Item#"            /*10*/
     rm-rcpth.po-no                                    label "PO#"                  /*9*/
     po-ord.vend-no                                   label "Vendor"                /*8*/
     v-job-no                                               label "Job#"                /*9*/
     rm-rdtlh.qty format "->>>,>>>,>>9.99"  label "Quantity"            /*15*/
     v-msf                                    label "Total MSF"         /*15*/
    with frame itemx down no-box width 132.

{ce/msfcalc.i}

v-fdate = date(01,01,year(today)).

outers:
repeat:
  update v-fdate v-tdate v-fitem v-titem v-fvend v-tvend v-mtype
      with frame selec editing:
    readkey.        
    if frame-field eq "v-fitem" then do:
        if keyfunction(lastkey) eq "end-error" then do:
            fil_id = ?.
            next-prompt v-fdate with frame selec.
            next.
        end.        
        else
        if keyfunction(lastkey) eq "choices" then do:
            find first item where item.company eq cocode 
              and item.i-no begins input v-fitem
            use-index i-code no-lock no-error.
            if avail item then 
                fil_id = recid(item).
            run sys/look/itemx.p.
            if keyfunction(lastkey) ne "end-error" then
                find item where recid(item) = fil_id no-lock no-error.
            else
                release item.
            if avail item then do:
                display item.i-no @ v-fitem with frame selec.
                v-fitem = item.i-no.
            end.
        end.
        else if keyfunction(lastkey) eq "page-up" then do:
                find prev item where item.company eq cocode use-index i-code no-lock no-error.
                if not avail item then 
                    find last item where item.company eq cocode use-index i-code no-lock no-error.
                if avail item then do:
                    display item.i-no @ v-fitem with frame selec.
                    v-fitem = item.i-no.
                end.
        end.
        else if keyfunction(lastkey) eq "page-down" then do:
                find next item where item.company eq cocode use-index i-code no-lock no-error.
                if not avail item then 
                    find first item where item.company eq cocode use-index i-code no-lock no-error.
                if avail item then do:
                    display item.i-no @ v-fitem with frame selec.
                    v-fitem = item.i-no.
                end.
        end.
        else apply lastkey.
    end.        
    else
    if frame-field eq "v-titem" then do:
        if keyfunction(lastkey) eq "end-error" then do:
            fil_id = ?.
            next-prompt v-fdate with frame selec.
            next.
        end.        
        else
        if keyfunction(lastkey) eq "choices" then do:
            find first item where item.company eq cocode 
              and item.i-no begins input v-titem
            use-index i-code no-lock no-error.
            if avail item then 
                fil_id = recid(item).
            run sys/look/itemx.p.
            if keyfunction(lastkey) ne "end-error" then
                find item where recid(item) = fil_id no-lock no-error.
            else 
                release item.
            if avail item then do:
                display item.i-no @ v-titem with frame selec.
                v-titem = item.i-no.
            end.
        end.
        else if keyfunction(lastkey) eq "page-up" then do:
                find prev item where item.company eq cocode use-index i-code no-lock no-error.
                if not avail item then 
                    find last item where item.company eq cocode use-index i-code no-lock no-error.
                if avail item then do:
                    display item.i-no @ v-titem with frame selec.
                    v-titem = item.i-no.
                end.
        end.
        else if keyfunction(lastkey) eq "page-down" then do:
                find next item where item.company eq cocode use-index i-code no-lock no-error.
                if not avail item then 
                    find first item where item.company eq cocode use-index i-code no-lock no-error.
                if avail item then do:
                    display item.i-no @ v-titem with frame selec.
                    v-titem = item.i-no.
                end.
        end.
        else apply lastkey.
    end.        
    else
        if frame-field eq "v-fvend" then do:
           if keyfunction(lastkey) eq "choices" then do:
          find first vend
              {ap/ap-vend.w}
              and vend.vend-no begins input v-fvend
              no-lock no-error.
          fil_id = if avail vend then recid(vend) else ?.
          vrow = 3. vcol = 1 . /* popup location */
          run sys/look/fvenname.p.    /* fvenvend.p. */
          if keyfunction(lastkey) eq "end-error" then fil_id = ?.
          find vend where recid(vend) = fil_id no-lock no-error.
                if avail vend then do:
                    display vend.vend-no @ v-fvend with frame selec.
                    v-fvend = vend.vend-no.
                end.
           end. /* if keyfunction(lastkey) eq "choices" */
        else if keyfunction(lastkey) eq "page-up" then do:
                find prev vend {ap/ap-vend.w} no-lock no-error.
                if not avail vend then 
                    find last vend {ap/ap-vend.w} no-lock no-error.
                if avail vend then do:
                    display vend.vend-no @ v-fvend with frame selec.
                    v-fvend = vend.vend-no.
                end.
        end.
        else if keyfunction(lastkey) eq "page-down" then do:
                find next vend {ap/ap-vend.w} no-lock no-error.
                if not avail vend then 
                    find first vend {ap/ap-vend.w} no-lock no-error.
                if avail vend then do:
                    display vend.vend-no @ v-fvend with frame selec.
                    v-fvend = vend.vend-no.
                end.
        end.
            else if keyfunction(lastkey) eq "end-error" then do:
                if frame-field ne "v-fdate" then do:
                    next-prompt v-fdate with frame selec.
                    next.
                end.
            end.
           else apply lastkey.
    end.                
    else
        if frame-field eq "v-tvend" then do:
           if keyfunction(lastkey) eq "choices" then do:
          find first vend
              {ap/ap-vend.w}
              and vend.vend-no begins input v-tvend
              no-lock no-error.
          fil_id = if avail vend then recid(vend) else ?.
          vrow = 3. vcol = 1 . /* popup location */
          run sys/look/fvenname.p.    /* fvenvend.p. */
          if keyfunction(lastkey) eq "end-error" then fil_id = ?.
          find vend where recid(vend) = fil_id no-lock no-error.
                if avail vend then do:
                    display vend.vend-no @ v-tvend with frame selec.
                    v-tvend = vend.vend-no.
                end.
           end. /* if keyfunction(lastkey) eq "choices" */
            else if keyfunction(lastkey) eq "end-error" then do:
                if frame-field ne "v-fdate" then do:
                    next-prompt v-fdate with frame selec.
                    next.
                end.
            end.
        else if keyfunction(lastkey) eq "page-up" then do:
                find prev vend {ap/ap-vend.w} no-lock no-error.
                if not avail vend then 
                    find last vend {ap/ap-vend.w} no-lock no-error.
                if avail vend then do:
                    display vend.vend-no @ v-tvend with frame selec.
                    v-tvend = vend.vend-no.
                end.
        end.
        else if keyfunction(lastkey) eq "page-down" then do:
                find next vend {ap/ap-vend.w} no-lock no-error.
                if not avail vend then 
                    find first vend {ap/ap-vend.w} no-lock no-error.
                if avail vend then do:
                    display vend.vend-no @ v-tvend with frame selec.
                    v-tvend = vend.vend-no.
                end.
        end.
           else apply lastkey.
    end.                
    else if keyfunction(lastkey) eq "end-error" then do:
        if frame-field ne "v-fdate" then do:
            next-prompt v-fdate with frame selec.
            next.
        end.
        else 
            return.            
    end.
    else apply lastkey.
    end. /*editing*/

    str-tit  = coname + " - " + loname.
    str-tit2 = "RAW MATERIAL RECEIPTS HISTORY".
    str-tit3 = "Date: " + string(v-fdate) + " - "  + string(v-tdate).

    if string(v-fitem) ne "" then
        str-tit4 = "Item: " + string(v-fitem)  + " - "  + string(v-titem).
    else
        str-tit4 = "Item: " + string("*blank*")  + " - "  + string(v-titem).
    
    if string(v-fvend) ne "" then 
        str-tit5 = "Vend: " + string(v-fvend) + " - " + string(v-tvend).
    else
        str-tit5 = "Vend: " + string("*blank*") + " - " + string(v-tvend).

    str-tit6 = "Types: " + v-mtype.

    x = (112 - length(str-tit)) / 2.
    str-tit  = fill(" ",x) + str-tit .

    x = (112 - length(str-tit2)) / 2.
    str-tit2 = fill(" ",x) + str-tit2 .

    x = (132 - length(str-tit3)) / 2.
    str-tit3 = fill(" ",x) + str-tit3 .

    x = (132 - length(str-tit4)) / 2.
    str-tit4 = fill(" ",x) + str-tit4 .

    x = (132 - length(str-tit5)) / 2.
    str-tit5 = fill(" ",x) + str-tit5 .

    x = (132 - length(str-tit6)) / 2.
    str-tit6 = fill(" ",x) + str-tit6 .

  {sys/inc/print2.i}
  if keyfunction(lastkey) eq "end-error" or not choice then next outers.

  {sys/msg/print.i print}
  {sys/inc/outprint.i 55}
  view frame r-top.

  find first ap-ctrl where ap-ctrl.company eq cocode no-lock.

  {sa/sa-sls01.i}

  assign
   dayctr  = 0
   itemctr = 0.
    
  for each rm-rcpth
      where rm-rcpth.company    eq cocode
        and rm-rcpth.trans-date ge v-fdate
        and rm-rcpth.trans-date le v-tdate
        and rm-rcpth.i-no       ge v-fitem
        and rm-rcpth.i-no       le v-titem
      use-index i-no no-lock,
        
      each rm-rdtlh
      where rm-rdtlh.r-no       eq rm-rcpth.r-no
      no-lock,

      first item
      where item.company eq cocode
        and item.i-no    eq rm-rcpth.i-no
        and index(v-mtype,item.mat-type) gt 0
      no-lock

      break by rm-rcpth.trans-date
            by rm-rcpth.i-no:
            
    find first po-ord
        where po-ord.company eq cocode
          and po-ord.po-no   eq int(rm-rcpth.po-no)
        no-lock no-error.

    if (avail po-ord and
        po-ord.vend-no ge v-fvend and
        po-ord.vend-no le  v-tvend)   or
       (not avail po-ord)             then do:
        
      assign
       dayctr   = dayctr + 1
       itemctr  = itemctr + 1
       v-job-no = ""
       v-len    = item.s-len
       v-wid    = if item.r-wid gt 0 then item.r-wid else item.s-wid.
             
      if trim(rm-rdtlh.job-no) ne "" then 
        v-job-no = trim(rm-rdtlh.job-no) + "-" + string(rm-rdtlh.job-no2,"99").
        
      /* run do-items. */
      
      release po-ordl.
      
      if avail po-ord then
      for each po-ordl
          where po-ordl.company eq cocode
            and po-ordl.po-no   eq po-ord.po-no
            and po-ordl.i-no    eq rm-rcpth.i-no
            and po-ordl.job-no  eq rm-rcpth.job-no
            and po-ordl.job-no2 eq rm-rcpth.job-no2
          no-lock
          by po-ordl.s-num desc:
          
        assign
         v-len = po-ordl.s-len
         v-wid = po-ordl.s-wid.
  
        if po-ordl.s-num eq rm-rdtlh.s-num then leave.
      end.
      
      if not avail po-ordl and rm-rcpth.job-no ne "" then
      for each job-mat
          where job-mat.company eq cocode
            and job-mat.rm-i-no eq item.i-no
            and job-mat.job-no  eq rm-rcpth.job-no
            and job-mat.job-no2 eq rm-rcpth.job-no2
          no-lock
          by job-mat.frm desc:
          
        assign
         v-len = job-mat.len
         v-wid = job-mat.wid.
  
        if job-mat.frm eq rm-rdtlh.s-num then leave.  
      end.
      
      if v-len eq 0 then v-len = 12.
      if v-wid eq 0 then v-wid = 12.

      v-msf = (if v-corr then (v-len * v-wid * .007)
                         else (v-len * v-wid / 144)) * rm-rdtlh.qty / 1000.
                         
      assign
       v-tot-msf-item  = v-tot-msf-item + v-msf
       v-tot-msf-day   = v-tot-msf-day + v-msf
       v-grand-tot-msf = v-grand-tot-msf + v-msf
       v-tot-qty       = v-tot-qty + rm-rdtlh.qty.

      display rm-rcpth.trans-date when first-of(rm-rcpth.trans-date)
              rm-rcpth.i-no  
              rm-rcpth.po-no 
              po-ord.vend-no      when avail po-ordl
              v-job-no  
              rm-rdtlh.qty 
              v-msf         
          with frame itemx.
      down with frame itemx.
    end.

    if last-of(rm-rcpth.i-no) then do:
      if itemctr gt 0 then do:
        display "--------------------" @ rm-rdtlh.qty 
                "--------------------" @ v-msf 
           with frame itemx.
        down with frame itemx.
        
        display v-tot-qty       @ rm-rdtlh.qty
                v-tot-msf-item  @ v-msf
            with frame itemx.
        down with frame itemx.
      end.                    
      
      assign
       v-tot-msf-item  = 0
       itemctr         = 0
       v-grand-tot-qty = v-grand-tot-qty + v-tot-qty
       v-tot-qty       = 0.
    end.

    if last-of(rm-rcpth.trans-date) then do:
      if dayctr gt 0 then do:
        display " Total MSF For Day:" @ rm-rcpth.i-no
                v-tot-msf-day         @ v-msf
                skip(1)
            with frame itemx.
        down with frame itemx.
      end.
      
      assign
       v-tot-msf-day = 0
       dayctr        = 0.
    end.
    
    if last(rm-rcpth.trans-date) then do:
      display "Grand Totals:" @ rm-rcpth.i-no
              v-grand-tot-qty @ rm-rdtlh.qty
              v-grand-tot-msf @ v-msf
          with frame itemx.                            
      down with frame itemx.
    end.
  end.  /*main for each*/

  {sys/inc/close.i}
  leave.
end.

hide all no-pause.

/* end ---------------------------------- copr. 1998  advanced software, inc. */


procedure do-items:
if item.i-code eq "E" then do:
    
  /*If no form number then see how many job materials 
    there are.  If there is only one, then use that one
    to calculate the MSF.  Else, if there is more than
    one, we don't know which job-mat to use, so we 
    cant't calculate the MSF.  If there are no job-mat 
    recs, then obviously we can't calculate the MSF*/

  if rm-rdtlh.s-num eq 0 then do:
    jmctr = 0.
    for each job-mat no-lock 
        where job-mat.company eq item.company 
          and job-mat.rm-i-no eq item.i-no
          and job-mat.job-no  eq rm-rcpth.job-no
          and job-mat.job-no2 eq rm-rcpth.job-no2:
      jmctr = jmctr + 1.
    end.
    
    if jmctr eq 1 then do:
      find first job-mat 
          where job-mat.company eq item.company 
            and job-mat.rm-i-no eq item.i-no
            and job-mat.job-no  eq rm-rcpth.job-no
            and job-mat.job-no2 eq rm-rcpth.job-no2
          no-lock no-error.

      if avail job-mat then do:
        if job-mat.len gt 0 and job-mat.wid gt 0 then do:
          v-sqft = (round((((if v-corr then (job-mat.len * job-mat.wid) * .007
                             else (job-mat.len * job-mat.wid) / 144) * 
                                                    rm-rdtlh.qty) / 1000),2)).
          v-tot-sqft = v-tot-sqft + v-sqft.
        end.
      end.  /*avail job-mat*/
    end.  /*jmctr eq 1*/
  end.   /*snum eq 0*/
end. /*"E"*/

else do:
  for each job-mat no-lock 
      where job-mat.company eq item.company 
        and job-mat.rm-i-no eq item.i-no
        and job-mat.job-no  eq rm-rcpth.job-no
        and job-mat.job-no2 eq rm-rcpth.job-no2
        and job-mat.frm     eq rm-rdtlh.s-num:
           
    if job-mat.len gt 0 and job-mat.wid gt 0 then do:
      v-sqft = (round((((if v-corr then (job-mat.len * job-mat.wid) * .007
                         else (job-mat.len * job-mat.wid) / 144) *
                                                rm-rdtlh.qty) / 1000),1)).
      v-tot-sqft = v-tot-sqft + v-sqft.
    end.    
  end. /*for each*/
end.
end procedure.





/* rm-trans_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'rm-trans_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="item" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="item" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="item" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="item" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/
/* ---------------------------------------------- rm/rep/rm-trans.p 07/98 JLF */
/* raw materials - transactions edit list                                     */
/* -------------------------------------------------------------------------- */
/*
{sys/inc/var.i shared}
{sys/form/s-top.f}
*/
{custom/gcompany.i}
def var save_id as recid.

def var time_stamp as char.
time_stamp = string(time, "hh:mmam").

def var v-fitem like rm-rcpth.i-no.
def var v-titem like v-fitem                  init "zzzzzzzzzz".
def var v-fpcat like item.procat.
def var v-tpcat like v-fpcat                  init "zzzzz".
def var v-fdate as   date format "99/99/9999" init 01/01/0001.
def var v-tdate like v-fdate                  init today.
def var v-floc  like rm-rcpth.loc.
def var v-tloc  like v-floc                   initial "zzzzz".
def var v-type  as   char format "x(5)"       init "RITAC".
def var v-code  like rm-rcpth.rita-code.

def var v-value as dec format "->>,>>>,>>9.99".
def var v-job-no as char format "x(9)".
def var v-qty like rm-rdtlh.qty extent 3.
def var v-val like v-value extent 3.
def var v-first as log extent 3.
def var cocode as char no-undo.
/*
{sys/form/r-topw.f}

assign
 str-tit  = coname + " - " + loname
 str-tit2 = "RAW MATERIALS - TRANSACTION HISTORY"
 x = (112 - length(str-tit)) / 2
 str-tit  = fill(" ",x) + str-tit
 x = (112 - length(str-tit2)) / 2
 str-tit2 = fill(" ",x) + str-tit2
 x = (112 - length(str-tit3)) / 2.

form skip(1)
     "From Item:"         to 20 v-fitem      "To Item:" to 57 v-titem   skip(1)
     "From Prod Cat:"     to 20 v-fpcat  "To Prod Cat:" to 57 v-tpcat   skip(1)
     "From Date:"         to 20 v-fdate      "To Date:" to 57 v-tdate   skip(1)
     "From Warehouse:"    to 20 v-floc  "To Warehouse:" to 57 v-tloc    skip(1)
     "Transaction Types:" to 20 v-type
     "(R)eceipt (I)ssue (T)ransfer (A)djustment (C)ount  "              skip(1)

    with title "  RAW MATERIAL TRANSACTION HISTORY  "
	 frame selec row 5 centered overlay no-labels
	 color value(col-title) prompt value(col-input).
*/

{custom/getcmpny.i}
form rm-rcpth.trans-date label "DATE"
     rm-rcpth.i-no label "ITEM"
     rm-rcpth.i-name format "x(14)" label "DESCRIPTION"
     rm-rcpth.po-no label "P.O.#"
     rm-rcpth.rita-code label "TY"
     v-job-no label "   Job #"
     rm-rdtlh.tag label "TAG#"
     rm-rdtlh.qty format "->>>>>9.99<<" label "QUANTITY"
     rm-rdtlh.loc label "WHSE"
     rm-rdtlh.loc-bin label "BIN"
     rm-rdtlh.loc2 column-label "WHSE! TO"
     rm-rdtlh.loc-bin2 column-label "BIN! TO"
     rm-rdtlh.cost format "->>>>>9.99<<<<" label "COST"
     space(0)
     v-value label "VALUE"
     skip
    with frame itemx no-box down width 132.

/*
outers:
repeat:
  update v-fitem v-titem v-fpcat v-tpcat v-fdate v-tdate v-floc v-tloc
	 v-type help "Remove those Transaction Types you don't wish to print"
      with frame selec.

  {sys/inc/print2.i}
  if keyfunction(lastkey) eq "end-error" then next outers.

  if choice then do with frame itemx:
    {sys/msg/print.i print}
    {sys/inc/outprint.i 55}
  
*/  
  assign 
        v-type = (if TOGGLE-1=yes then "R" else "") +
                 (if TOGGLE-2=yes then "I" else "") +
                 (if TOGGLE-3=yes then "T" else "") +
                 (if TOGGLE-4=yes then "A" else "") +
                 (if TOGGLE-5=yes then "C" else "")
        v-fitem = begin_i-no
        v-titem = end_i-no
        v-fpcat = begin_procat
        v-tpcat = end_procat
        v-fdate = begin_date
        v-floc = begin_whse
        v-tloc = end_whse
        cocode = gcompany.
        
                
  message cocode v-type v-titem v-tpcat v-tloc view-as alert-box.               
                 
    view frame r-top.

    find first ap-ctrl where ap-ctrl.company eq cocode no-lock.

    {rm/sa-sls01.i}

    for each rm-rcpth
	where rm-rcpth.company                 eq cocode
	  and rm-rcpth.i-no                    ge v-fitem
	  and rm-rcpth.i-no                    le v-titem
	  and rm-rcpth.trans-date              ge v-fdate
	  and rm-rcpth.trans-date              le v-tdate
	  and index(caps(v-type),rm-rcpth.rita-code) gt 0
	use-index i-no no-lock,

	each rm-rdtlh
	where rm-rdtlh.r-no      eq rm-rcpth.r-no
	  and rm-rdtlh.rita-code eq rm-rcpth.rita-code
	  and rm-rdtlh.loc                     ge v-floc
	  and rm-rdtlh.loc                     le v-tloc
	no-lock,

	first item
	where item.company eq cocode
	  and item.i-no    eq rm-rcpth.i-no
	  and item.procat                      ge v-fpcat
	  and item.procat                      le v-tpcat
	no-lock

	break by rm-rcpth.trans-date
	      by rm-rcpth.rita-code

	transaction:

      if first-of(rm-rcpth.rita-code)  then v-first[1] = yes.
      if first-of(rm-rcpth.trans-date) then v-first[2] = yes.

      assign
       v-job-no = fill(" ",6 - length(trim(rm-rdtlh.job-no))) +
		  trim(rm-rdtlh.job-no) + "-" + string(rm-rdtlh.job-no2,"99")
       v-value  = rm-rdtlh.cost * rm-rdtlh.qty.

      if v-job-no begins "-" then v-job-no = "".

      display rm-rcpth.trans-date when first-of(rm-rcpth.trans-date)
	      rm-rcpth.i-no
	      rm-rcpth.i-name
	      rm-rcpth.po-no
	      rm-rcpth.rita-code
	      rm-rdtlh.tag
	      v-job-no
	      rm-rdtlh.qty
	      rm-rdtlh.loc
	      rm-rdtlh.loc-bin
	      rm-rdtlh.loc2
	      rm-rdtlh.loc-bin2
	      rm-rdtlh.cost
	      v-value
	  with frame itemx.
      down with frame itemx.

      assign
       v-qty[1] = v-qty[1] + rm-rdtlh.qty
       v-val[1] = v-val[1] + v-value.

      if last-of(rm-rcpth.rita-code) then do:
	if not v-first[1] then do:
	  underline rm-rdtlh.qty
		    v-value
	      with frame itemx.

	  display " TYPE TOTALS" @ rm-rcpth.i-name
		  v-qty[1]       @ rm-rdtlh.qty
		  v-val[1]       @ v-value
	      with frame itemx.
	end.

	if not last-of(rm-rcpth.trans-date) then put skip(1).

	assign
	 v-qty[2] = v-qty[2] + v-qty[1]
	 v-val[2] = v-val[2] + v-val[1]

	 v-qty[1] = 0
	 v-val[1] = 0.
      end.

      if last-of(rm-rcpth.trans-date) then do:
	if not v-first[2] then do:
	  underline rm-rdtlh.qty
		    v-value
	      with frame itemx.

	  display " DATE TOTALS" @ rm-rcpth.i-name
		  v-qty[2]       @ rm-rdtlh.qty
		  v-val[2]       @ v-value
	      with frame itemx.
	end.

	put skip(2).

	assign
	 v-qty[3] = v-qty[3] + v-qty[2]
	 v-val[3] = v-val[3] + v-val[2]

	 v-qty[2] = 0
	 v-val[2] = 0.
      end.

      v-first[1] = no.
      if last-of(rm-rcpth.rita-code) then v-first[2] = no.

      if last(rm-rcpth.trans-date) then do:
	underline rm-rdtlh.qty
		  v-value
	    with frame itemx.

	display "GRAND TOTALS" @ rm-rcpth.i-name
		v-qty[3]       @ rm-rdtlh.qty
		v-val[3]       @ v-value
	      with frame itemx.
      end.

      v-code = rm-rcpth.rita-code.

      if v-code ne "T" then do:
      
	find first costtype
	    where costtype.company   eq cocode
	      and costtype.loc       eq rm-rdtlh.loc
	      and costtype.cost-type eq item.cost-type
	    no-lock no-error.

/*
	create report.
	assign
	 report.term-id = v-term
	 report.key-01  = if v-code eq "R" and v-value gt 0 then
			    ap-ctrl.payables else
			    if avail costtype then 			    
			      if rm-rcpth.rita-code eq "I" or v-value gt 0 then
			        costtype.inv-asset                         else
			        costtype.cons-exp
      			    else "Cost Type not found"
	 report.key-02  = string(v-value,"->>,>>>,>>9.99").

/*	v-value = v-value * -1. */
	if rm-rcpth.rita-code ne "I" then v-value = v-value * -1.

	create report.
	assign
	 report.term-id = v-term
	 report.key-01  = if avail costtype then
			    if rm-rcpth.rita-code eq "I" or v-value gt 0 then
			      costtype.inv-asset                         else
			      costtype.cons-exp
			  else "Cost Type not found"
	 report.key-02  = string(v-value,"->>,>>>,>>9.99").
*/

       if v-code eq "R" then
       do:
	  create report.
	  assign
	    report.term-id = v-term
	    report.key-01  = if avail costtype then costtype.inv-asset
       			 else "Cost Type not found"
	    report.key-02  = string(v-value,"->>,>>>,>>9.99").
       end.
       
       else
       do:
         create report.
	  assign
	    report.term-id = v-term
	    report.key-01  = if avail costtype then costtype.cons-exp
			       else "Cost Type not found"
	    report.key-02  = string(v-value,"->>,>>>,>>9.99").    
	end.
	    
      end.
    end.

    v-value = 0.

    for each report where report.term-id eq v-term,
	first account
	where account.company eq cocode
	  and account.actnum  eq report.key-01
	no-lock
	break by report.key-01
	transaction:

      if first(report.key-01) then page.

      v-value = v-value + dec(report.key-02).

      if last-of(report.key-01) then do:
	display account.actnum
		account.dscr
		v-value  label "Amount" (total)      format "->>,>>>,>>9.99"
	    with width 132.

	v-value = 0.
      end.

      delete report.
    end.

/*    {sys/inc/close.i}
  end.

  leave.
end.      /* outers */

hide all no-pause.
*/
/* end ---------------------------------- copr. 1998  advanced software, inc. */


/* ------------------------------------------------- cec/blkeqsht.i 03/98 JLF */

  def var v-cum-alf as char no-undo.
  def var v-cum-wid as dec no-undo.
  def var v-cum-len as dec no-undo.
  DEF VAR ll-ans as log no-undo.
  def buffer bf-ef for ef.
  def buffer bf-eb for eb.
  DEF VAR v-num-dec AS INT NO-UNDO.

  IF v-cecscrn-char NE "Decimal" THEN
     v-num-dec = 3.
  ELSE
     v-num-dec = 6.

  for each bf-ef OF xest
      where bf-ef.form-no ne 0
        and can-find(bf-eb of bf-ef)   /* only check when 1 blank per form */
      no-lock,
      each bf-eb OF bf-ef no-lock:

    IF bf-ef.xgrain EQ "B" AND v-cecscrn-char NE "Decimal" THEN DO:
      if round(bf-ef.trim-w,v-num-dec) ne round(bf-eb.t-len * bf-eb.num-len,v-num-dec) or
         round(bf-ef.trim-l,v-num-dec) ne round(bf-eb.t-wid * bf-eb.num-wid,v-num-dec) then do:
         ll-ans = yes.
         message "WARNING:  Blank width and/or length not matching sheet on Form "
                + trim(string(bf-ef.form-no,">>")) /*+ ","
                "F4 to cancel"*/
                skip
"W:"                bf-ef.trim-w "," bf-eb.t-len bf-eb.num-len  skip
"L:"                bf-ef.trim-l "," bf-eb.t-wid bf-eb.num-wid  skip

                view-as alert-box warning button ok-cancel update ll-ans.
         if not ll-ans then return error.
      end.
    END.

    IF bf-ef.xgrain NE "B" AND v-cecscrn-char NE "Decimal" THEN
         DO:
      if round(bf-ef.trim-w,v-num-dec) ne round(bf-eb.t-wid * bf-eb.num-len,v-num-dec) or
         round(bf-ef.trim-l,v-num-dec) ne round(bf-eb.t-len * bf-eb.num-wid,v-num-dec) then do:
         ll-ans = yes.
         message "WARNING:  Blank width and/or length not matching sheet on Form "
                + trim(string(bf-ef.form-no,">>")) /*+ ","
                "F4 to cancel"*/
                skip
"W:"                bf-ef.trim-w "," bf-eb.t-wid bf-eb.num-len  skip
"L:"                bf-ef.trim-l "," bf-eb.t-len bf-eb.num-wid  skip

                view-as alert-box warning button ok-cancel update ll-ans.
         if not ll-ans then return error.
      end.
    END.

    find first box-design-hdr {cec/est-6W.i box-design-hdr}
          and box-design-hdr.form-no   eq bf-eb.form-no
          and box-design-hdr.blank-no  eq bf-eb.blank-no
        no-lock no-error.
    if avail box-design-hdr then do:
      assign
       v-cum-wid = 0
       v-cum-len = 0.

      for each box-design-line of box-design-hdr no-lock:
        if box-design-line.wcum-score ne "" then
          v-cum-wid = dec(box-design-line.wcum-score) NO-ERROR.
      end.

      if box-design-hdr.lcum-score ne "" then do:
        do i = length(box-design-hdr.lcum-score) to 1 by -1:
          if substr(box-design-hdr.lcum-score,i,1) eq " " then do:
            v-cum-len = dec(substr(box-design-hdr.lcum-score,i + 1,
                                      length(trim(box-design-hdr.lcum-score)))).
            leave.
          end.
        end.

        if v-cum-len eq 0 then v-cum-len = dec(trim(box-design-hdr.lcum-score)).
      end.

      {sys/inc/k16bb.i v-cum-wid}
      {sys/inc/k16bb.i v-cum-len}

      if ( 
         round(v-cum-wid,v-num-dec) ne round(bf-eb.t-wid,v-num-dec) or
         round(v-cum-len,v-num-dec) ne round(bf-eb.t-len,v-num-dec) 
         ) 
         AND 
         (
          v-cecscrn-char NE "Decimal" 
         ) 
          then do:
         ll-ans = yes. 
         message "WARNING:  Blank width and/or length not matching Box Design Page."
                /*"F4 to abort print.". */
                skip
                "W: " v-cum-wid bf-eb.t-wid skip
                "L:" v-cum-len bf-eb.t-len
                      
               view-as alert-box warning button ok-cancel update ll-ans.
      /*  pause 10 no-message.  */
        if not ll-ans then return error.
      end.
    end.
  end.
/*
  if keyfunction(lastkey) eq "end-error" then undo, leave.
*/
/* end ---------------------------------- copr. 1998  advanced software, inc. */

/* cec/refestg.i  will be called from v-est4.w (Farm/Misc)
                  from cec/refest5aW.i  {1} - MAT or LAB 
                                       {2} - array index */  
def var lv-ref-rec-qty as recid no-undo.
def var lv-ref-rec-cst as recid no-undo.

find est where est.company = ef.company and
               est.est-no = ef.est-no
               no-lock no-error.   
               
if ef.mis-cost[{2}]:screen-value <> ""  /*and
   self:screen-value <> "" */ then do:

      RUN cec/refestg1.p (ROWID(eb), "{1}", {2},
                          OUTPUT lv-ref-rec-qty,
                          OUTPUT lv-ref-rec-cst). 

      RUN cec/d-refest.w (lv-ref-rec-qty, lv-ref-rec-cst, "{1}").

      FIND reftable WHERE RECID(reftable) EQ lv-ref-rec-cst NO-LOCK.

      SELF:SCREEN-VALUE = STRING(reftable.val[1]).
end.   
else do:
      /*  delete reftable */
      for each reftable            where reftable.reftable eq "EST-MISC"
	       and reftable.company  eq ef.company
	       and reftable.loc      eq ef.loc
	       and reftable.code     eq trim(ef.est-no) + string(ef.form-no,"/99")
	       and reftable.code2    eq "{1}-QTY" + string({2},"99")
              exclusive-lock:
          delete reftable.
      end.
      for each reftable     where reftable.reftable eq "EST-MISC"
	       and reftable.company  eq ef.company
	       and reftable.loc      eq ef.loc
	       and reftable.code     eq trim(ef.est-no) + string(ef.form-no,"/99")
	       and reftable.code2    eq "{1}-CST" + string({2},"99")      
                 exclusive-lock:
          delete reftable.
     end.
     
end.

apply "tab" to ef.mis-{1}m[{2}].
return no-apply.

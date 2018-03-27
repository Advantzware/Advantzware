/* cec/refestg2.i  will be called from v-est4.w (Farm/Misc)
                  from cec/refest5aW.i  {1} - MAT or LAB                                        
                                       {2} - array index */  
/*                                       
def var lv-ref-rec-qty as recid no-undo.
def var lv-ref-rec-cst as recid no-undo.
*/
DEF BUFFER b-eb-{1} FOR eb.

find est where est.company = ef.company and
               est.est-no = ef.est-no
               no-lock no-error. 

FIND FIRST b-eb-{1}
    WHERE b-eb-{1}.company  EQ eb.company
      AND b-eb-{1}.est-no   EQ eb.est-no
      AND b-eb-{1}.form-no  EQ eb.form-no
      AND b-eb-{1}.eqty     EQ eb.eqty
      AND b-eb-{1}.blank-no EQ INT(ef.mis-bnum[{2}]:SCREEN-VALUE)
    NO-LOCK NO-ERROR.
IF NOT AVAIL b-eb-{1} THEN
FIND b-eb-{1} WHERE ROWID(b-eb-{1}) EQ ROWID(eb) NO-LOCK NO-ERROR.
               
if ef.mis-cost[{2}]:screen-value <> ""  /*and
   self:screen-value <> "" */ then do:

      RUN cec/refestg1.p (ROWID(b-eb-{1}), "{1}", {2},
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
     ASSIGN ef.mis-matf[{2}]:SCREEN-VALUE = "0.00"
            ef.mis-labf[{2}]:SCREEN-VALUE = "0.00"
            ef.mis-matm[{2}]:SCREEN-VALUE = "0.00"
            ef.mis-labm[{2}]:SCREEN-VALUE = "0.00"            
            ef.mis-simon[{2}]:SCREEN-VALUE = ""
            ef.mis-mkup[{2}]:SCREEN-VALUE = "0.00"
            .
     
end.

/*apply "tab" to ef.mis-{1}m[{2}].
return no-apply.
*/

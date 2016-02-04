/* oe/getqpri2.p */
def input param v-term like report.term-id format "x(20)" .

def shared var fil_id as recid no-undo.
def shared var s-part-no like quoteit.part-no no-undo.
def shared var s-part-no2 like s-part-no no-undo.
def shared var s-est-no like est.est-no no-undo.
def shared var s-qty like quoteit.qty no-undo.
def var v-rel as dec no-undo.

{sys/inc/var.i shared}

for each quotehd where quotehd.company eq cocode
                   and quotehd.loc     eq locode
                   and quotehd.est-no   eq s-est-no no-lock,
      each quoteitm  of quotehd where (quoteitm.part-no  eq s-part-no or
                            (quoteitm.part-no eq s-part-no2 and s-part-no2 ne ""))
             no-lock,
      each quoteqty where quoteqty.q-no = quotehd.q-no and
                          quoteqty.line = quoteitm.line
                     and (quoteqty.qty lt s-qty )
       no-lock by quotehd.q-no  desc by quoteqty.qty desc:
    
    create report.
    assign
     report.term-id = v-term
     report.key-01  = string(i,"9999999999")
     report.key-02  = string(quotehd.q-no,">>>>>>")
     report.rec-id  = recid(quoteqty)
     i              = i + 1
     fil_id         = recid(report) .
/*     
    if est.est-type eq 3  or
       est.est-type eq 4  or 
       quoteitm.line gt 10 then
      run sys/ref/convctod.p (quoteqty.prof-on, 1, output v-rel).
    else  */
      run sys/ref/convctod.p (quoteqty.prof-on,1, output v-rel).

    report.key-03 = string(int(v-rel),">>").
    leave.
  end.

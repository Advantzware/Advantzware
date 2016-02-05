/* ----------------------------------------------- ce/rep/jobtick.i 03/99 JLF */
/* factory ticket                                                             */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer b-eb             for eb.
def buffer b-job-hdr        for job-hdr.
def buffer b-job-mch        for job-mch.
def buffer b-itemfg         for itemfg.

def {1} var save_id         as   recid.

def {1} var v-last-page     as   int.
def {1} var v-page-tot      as   dec format ">>9".
def {1} var v-linecount     as   int.

def {1} var v-slsmn         as   char format "x(28)".
def {1} var v-ovrun         as   char format "x(22)".
def {1} var v-unrun         as   char format "x(22)".
def {1} var v-qc-inst       like est-inst.inst extent 6.
def {1} var v-form          as   int.
def {1} var v-forms         as   int.
def {1} var v-cust          as   char format "x(50)" extent 8.
def {1} var v-date-ent      as   date.

def {1} var v-break         as   log init no.
def {1} var v-last-ord      like oe-ord.ord-no.
def {1} var v-item-id       like eb.blank-no.
def {1} var v-alloc         as   char.

def {1} workfile w-inst
  field rec-id      as   recid.

def {1} workfile w-bot
  field seq         as   int
  field ink         like item.i-no
  field ink-d       like item.i-name
  field ink-a       as   char format "x(48)"
  field ink-q       as   dec  format ">>>,>>9.9<<"
  field cad         like eb.cad-no
  field cad-a       like w-bot.ink-a
  field mch         like job-mch.m-code
  field units       as   int format ">>,>>>,>>9"
  field mr-hr       like job-mch.mr-hr
  field speed       as   dec.

def {1} workfile w-item-id
  field i-no        like job-hdr.i-no
  field id          like v-item-id.


find first job     no-error.
find first job-hdr no-error.

format header
       skip(3)
       trim(string(page-number - v-last-page,">9")) + " of " +
         trim(string(v-page-tot,">9"))  at 79   format "x(8)"
       trim(job-hdr.est-no)             at 103  format "x(5)"
       trim(job-hdr.job-no) + "-" + string(job-hdr.job-no2,"99")
                                        at 133  format "x(9)"
       skip(2)
       job-hdr.cust-no                  at 79
       v-slsmn                          at 103
       v-ovrun                          at 133
       v-qc-inst[1]                     at 156
       v-qc-inst[2]                     at 156
       v-date-ent                       at 79   FORMAT "99/99/99"
       trim(string(v-form,">9")) + " of " + trim(string(v-forms,">9"))
                                        at 103  format "x(8)"
       v-unrun                          at 133
       v-qc-inst[3]                     at 156
       v-qc-inst[4]                     at 156
       v-qc-inst[5]                     at 156
       v-cust[1]                        at 6
       v-cust[5]                        at 79
       v-qc-inst[6]                     at 156
       v-cust[2]                        at 6
       v-cust[6]                        at 79
       v-cust[3]                        at 6
       v-cust[7]                        at 79
       v-cust[4]                        at 6
       v-cust[8]                        at 79
       skip(2)

    with page-top no-box frame head no-labels STREAM-IO width 225.

{ce/msfcalc.i}

/* end ---------------------------------- copr. 1999  advanced software, inc. */

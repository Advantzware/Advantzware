/* ----------------------------------------------- sys/inc/numout.p 04/05 JLF */
/* Calculate Number of Net Sheets on Sheet input to this machine operation    */
/* -------------------------------------------------------------------------- */

def input  param row-id as rowid.
def output param v-on-f as dec.

def var vn-out like ef.n-out-l init 1.
def var v-outw like ef.n-out.
def var v-outl like ef.n-out-l.
def var v-outf as dec.
def var v-on-l as dec.
def var vh-tmp as dec.
def var v-widp as log.
def var v-line like est-op.line.
def var v-loop as log.

def buffer b-op for est-op.

find est-op where rowid(est-op) eq row-id no-lock.

find first ef
    where ef.company eq est-op.company
      and ef.est-no  eq est-op.est-no
      and ef.form-no eq est-op.s-num
    no-lock.

/* Calculate v-on-f based on the full n-out of the form */
assign
 v-outw = if ef.n-out   eq 0 then 1 else ef.n-out
 v-outl = if ef.n-out-l eq 0 then 1 else ef.n-out-l
 vn-out = v-outw * v-outl
 v-on-l = vn-out
 v-on-f = vn-out
 v-outf = 0
 v-widp = v-outw gt v-outl and v-outw gt 1
 v-line = if est-op.line gt 500 then 500 else 0
 v-loop = no.

/* for each b-op where the dept is on the list and line # is before current mach, decrease v-outl and increase v-outf, then recalc v-on-f */
for each b-op
    where b-op.company                    eq est-op.company
      and b-op.est-no                     eq est-op.est-no
      and b-op.s-num                      eq est-op.s-num
      and b-op.line                       lt v-line + 500
      and b-op.line                       gt v-line
      and b-op.line                       lt est-op.line
      and b-op.qty                        eq est-op.qty
      and lookup(b-op.dept,"CR,RC,RS,SS,GU") ne 0
    no-lock
    by b-op.d-seq desc by b-op.line desc:

  if v-widp then
    assign
     vh-tmp = v-outl
     v-outl = v-outw
     v-outw = vh-tmp
     v-on-l = v-on-l / if v-outf ne 0 then v-outf else 1
     v-outf = 0
     v-widp = no.

  if b-op.n-out ne 0 THEN DO:
    assign
     v-loop = yes
     v-outl = v-outl - b-op.n-out
     v-outf = v-outf + b-op.n-out
     v-on-f = v-on-l / v-outf.

  END.
  else leave.

  if v-outl lt 1 then v-widp = not v-widp.

  if v-on-f eq ? then v-on-f = 1.
  if v-on-l eq ? then v-on-l = 1.
end.

/* find first b-op where the dept is on the list and line number is before current machine, adjust v-on-f */
if not v-loop then do:
  find first b-op
    where b-op.company                    eq est-op.company
      and b-op.est-no                     eq est-op.est-no
      and b-op.s-num                      eq est-op.s-num
      and b-op.line                       lt v-line + 500
      and b-op.line                       gt v-line
      and b-op.line                       lt est-op.line
      and b-op.qty                        eq est-op.qty
      and lookup(b-op.dept,"CR,RC,RS,SS,GU") ne 0
    no-lock no-error.

  if avail b-op then
    v-on-f = v-on-f / (if ef.n-out   eq 0 then 1 else ef.n-out) /
                       if ef.n-out-l eq 0 then 1 else ef.n-out-l.
  
end.

v-on-f = if v-on-f lt 1 then 1 else trunc(v-on-f,0).

/* end ---------------------------------- copr. 1996  advanced software, inc. */

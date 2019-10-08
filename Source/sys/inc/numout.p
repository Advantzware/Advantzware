/* ----------------------------------------------- sys/inc/numout.p 09/96 JLF */
/* Calculate Number of Net Sheets on Sheet input to this machine operation    */
/* -------------------------------------------------------------------------- */

def input  param rec-id as recid no-undo.
def output param v-on-f as dec no-undo.

def var vn-out like ef.n-out-l init 1 no-undo.
def var v-outw like ef.n-out no-undo.
def var v-outl like ef.n-out-l no-undo.
def var v-outf as dec no-undo.
def var v-on-l as dec no-undo.
def var vh-tmp as dec no-undo.
def var v-widp as log no-undo.
def var v-line like est-op.line no-undo.
def var v-loop as log no-undo.
DEF VAR ll-foam AS LOG NO-UNDO.
DEF VAR lv-pass-no LIKE ef-nsh.pass-no NO-UNDO.
DEF VAR lv-sheet-no LIKE ef-nsh.sheet-no NO-UNDO.

def buffer b-op for est-op.
DEF BUFFER b-nsh FOR ef-nsh.


find est-op where recid(est-op) eq rec-id no-lock no-error.

IF AVAIL est-op THEN
find first ef
    where ef.company eq est-op.company
      and ef.est-no  eq est-op.est-no
      and ef.form-no eq est-op.s-num
    no-lock no-error.

IF NOT AVAIL ef THEN DO:
  v-on-f = 1.
  RETURN.
END.

/*Foam Logic Commented out Per Joe - 26560 */
/*RUN cec/isitfoam.p (ROWID(ef), OUTPUT ll-foam).                         */
/*                                                                        */
/*IF ll-foam THEN                                                         */
/*FIND FIRST ef-nsh OF ef NO-LOCK NO-ERROR.                               */
/*                                                                        */
/*IF AVAIL ef-nsh  THEN DO:                                               */
/*  v-on-f = 0.                                                           */
/*                                                                        */
/*  RELEASE ef-nsh.                                                       */
/*                                                                        */
/*  FOR EACH ef-nsh OF ef                                                 */
/*      WHERE (ef-nsh.pass-no EQ est-op.op-pass OR                        */
/*             LOOKUP(est-op.dept,"CR,RC,DC") LE 0)                       */
/*      NO-LOCK                                                           */
/*      BREAK BY ef-nsh.sheet-no:                                         */
/*                                                                        */
/*    IF LAST-OF(ef-nsh.sheet-no) THEN DO:                                */
/*      vn-out = 1.                                                       */
/*      FOR EACH b-nsh OF ef                                              */
/*          WHERE b-nsh.sheet-no EQ ef-nsh.sheet-no                       */
/*            AND (b-nsh.pass-no LT ef-nsh.pass-no OR                     */
/*                 LOOKUP(est-op.dept,"CR,RC,DC") LE 0)                   */
/*          NO-LOCK:                                                      */
/*        vn-out = vn-out * b-nsh.n-out-l * b-nsh.n-out-w * b-nsh.n-out-d.*/
/*      END.                                                              */
/*      v-on-f = v-on-f + vn-out.                                         */
/*    END.                                                                */
/*  END.                                                                  */
/*END.                                                                    */
/*                                                                        */
/*ELSE DO:                                                                */
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

  for each b-op
      where b-op.company                    eq est-op.company
        and b-op.est-no                     eq est-op.est-no
        and b-op.s-num                      eq est-op.s-num
        and b-op.line                       lt v-line + 500
        and b-op.line                       gt v-line
        and b-op.line                       ge est-op.line
        and b-op.qty                        eq est-op.qty
        and lookup(b-op.dept,"CR,RC,SS,GU,RS") ne 0
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
  
    if b-op.n-out ne 0 then
      assign
       v-loop = yes
       v-outl = v-outl - b-op.n-out
       v-outf = v-outf + b-op.n-out
       v-on-f = v-on-l / v-outf.

    else leave.

    if v-outl lt 1 then v-widp = not v-widp.

    if v-on-f eq ? then v-on-f = 1.
    if v-on-l eq ? then v-on-l = 1.
  end.

  if not v-loop then do:
    find first b-op
      where b-op.company                    eq est-op.company
        and b-op.est-no                     eq est-op.est-no
        and b-op.s-num                      eq est-op.s-num
        and b-op.line                       lt v-line + 500
        and b-op.line                       gt v-line
        and b-op.line                       ge est-op.line
        and b-op.qty                        eq est-op.qty
        and lookup(b-op.dept,"CR,RC,SS,GU,RS") ne 0
      no-lock no-error.

    if avail b-op then
      v-on-f = v-on-f / (if ef.n-out   eq 0 then 1 else ef.n-out) /
                         if ef.n-out-l eq 0 then 1 else ef.n-out-l.
  end.

  v-on-f = if v-on-f lt 1 then 1 else trunc(v-on-f,0).
/*END.*/

/* end ---------------------------------- copr. 1996  advanced software, inc. */

/* --------------------------------------------- sys/inc/flm-prep.p  9/94 gb  */
/*                                                                            */
/* create/update est-prep for Film and Plate color or Coating                 */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input parameter rec_id as recid no-undo.
def input parameter s-num like est-prep.s-num.
def output parameter prep-qty like est-prep.qty no-undo.
def buffer bf-est for est.
def buffer bf-eb for eb .
def buffer bf-ef for ef .

DEF VAR li AS INT NO-UNDO.


find bf-est where recid(bf-est) eq rec_id no-lock no-error.
if not avail bf-est then leave.

find first bf-ef where bf-ef.company = bf-est.company 
                  and bf-ef.est-no  eq bf-est.est-no
                  and bf-ef.form-no eq s-num
                  no-lock no-error.
if not avail bf-ef then leave.

if bf-est.est-type ne 4 and bf-est.est-type ne 8 then
for each bf-eb where  bf-eb.company = bf-est.company 
                and bf-eb.est-no  eq bf-ef.est-no
                and bf-eb.form-no eq bf-ef.form-no
                no-lock break by bf-eb.form-no:

    prep-qty = prep-qty + (if bf-est.est-type ne 3 or first(bf-eb.form-no) then
			   bf-eb.i-coat + bf-eb.i-col else bf-eb.yld-qty).
end.

else prep-qty = prep-qty + bf-ef.f-coat + bf-ef.f-col.

for each bf-eb where  bf-eb.company eq bf-est.company 
                 and bf-eb.est-no   eq bf-ef.est-no
                 and bf-eb.form-no  eq bf-ef.form-no
               no-lock break by bf-eb.form-no:
  IF bf-eb.est-type LE 4 THEN
  DO li = 1 to 20:
    IF CAN-FIND(FIRST item 
                WHERE item.company  EQ bf-est.company
                  AND item.i-no     EQ bf-eb.i-code2[li]
                  AND item.ink-type EQ "A") THEN prep-qty = prep-qty - 1.
  END.

  ELSE
  DO li = 1 to 10:
    IF CAN-FIND(FIRST item 
                WHERE item.company  EQ bf-est.company
                  AND item.i-no     EQ bf-eb.i-code[li]
                  AND item.ink-type EQ "A") THEN prep-qty = prep-qty - 1.
  END.
end.

IF prep-qty LT 0 THEN prep-qty = 0.

/* end ---------------------------------- copr. 1992  advanced software, inc. */


/***************************************************************************\
*****************************************************************************
**  Program: cec/kpallet.p
**       By: Chris Schreiber, Advanced Software, Inc.
** Descript: Calculates stacks/layer and unit counts for a given estimate
**
*****************************************************************************
\***************************************************************************/

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

/* input/output parameters */
def input parameter v-eb-id as recid.
def output parameter v-op-cas-pal like eb.cas-pal.
def output parameter v-op-tr-cnt like eb.tr-cnt.
def output parameter v-op-numstacks as int.
def output parameter v-op-stackcode as char.
def output parameter v-op-error as log.

/* local vars - used in cec/kstyle.i */
def var op as ch extent 100.
def var nextop as int.
def var num as dec extent 100.
def var curnum as char.
def var kar as char format "x".
def var formule as dec extent 12.

/* local vars - internal use */
def var v-kdf-width as int.
def var v-kdf-length as int.
def var v-code like reftable.code. /* stacking code */
def var v-row as int.
def var v-col as int.
def var v-counter as int.
def var v-index as int.
def var v-pal-vol as dec.
def var k_frac as dec init 6.25 no-undo.
def var lv-layers as dec no-undo.
DEF VAR ll-assem-part AS LOG NO-UNDO.
DEF VAR li2 AS INT NO-UNDO.
DEF VAR ld-array LIKE eb.k-len-array2 NO-UNDO.
DEF VAR ld-small-cell AS DEC NO-UNDO.
DEF VAR li AS INT NO-UNDO.

DO TRANSACTION:
  {sys/inc/cecunit.i} 
END.
{sys/inc/f16to32.i}


if not avail xeb then find xeb where recid(xeb) = v-eb-id no-lock no-error.

RUN est/assmpart.p (BUFFER xeb, OUTPUT ll-assem-part).

IF xeb.cas-wt NE 0 THEN DO:
  ASSIGN
   v-op-cas-pal   = xeb.cas-pal
   v-op-numstacks = 1
   v-op-stackcode = "D".

  RUN autocalc.

  RETURN.
END.

IF ll-assem-part THEN DO:
  ld-small-cell = 0.

  FOR EACH eb NO-LOCK
      WHERE eb.company EQ xeb.company
        AND eb.est-no  EQ xeb.est-no
        AND ROWID(eb)  NE ROWID(xeb)
      BREAK BY eb.part-no:

    IF FIRST-OF(eb.part-no) THEN DO:
      ASSIGN
       li2          = 0
       ld-array     = 0
       v-kdf-width  = eb.t-wid
       v-kdf-length = v-kdf-length + eb.t-len.

      DO li = 1 TO EXTENT(eb.k-len-array2):
        IF eb.k-len-array2[li] GT 0 THEN
          ASSIGN
           li2           = li2 + 1
           ld-array[li2] = eb.k-len-array2[li].
      END.

      DO li = 1 TO EXTENT(ld-array):
        IF (li EQ 1 OR li EQ li2)                                AND
           (ld-small-cell EQ 0 OR ld-array[li] LT ld-small-cell) THEN
          ld-small-cell = ld-array[li].
      END.
    END.
  END.

  v-kdf-length = v-kdf-length - (ld-small-cell * 2).
END.

ELSE DO:
  find first item
      {sys/look/itemgsW.i}
        and item.i-no eq xeb.adhesive
      no-lock no-error.
  find first style
      where style.company eq xeb.company
        and style.style   eq xeb.style
      no-lock no-error.
  if not avail style then do:
    v-op-error = yes.
    message "ERROR: Style record could not be found!" view-as alert-box.
    return.
  end.

  if style.type eq "F" then do:
    assign
     v-pal-vol      = xeb.tr-len * xeb.tr-wid * xeb.tr-dep
     v-pal-vol      = v-pal-vol / (xeb.t-len * xeb.t-wid * xeb.t-dep)
     v-op-cas-pal   = trunc(v-pal-vol / xeb.cas-cnt,0)
     v-op-tr-cnt    = v-op-cas-pal * xeb.cas-cnt
     v-op-numstacks = 1
     v-op-stackcode = "D"
     v-op-error     = no.

    RUN autocalc.

    leave.
  end.

  /*** CTS added 4/4/96 *********************************************
   *** if the style kdf-length and kdf-width are not filled in
   *** then just leave without calculating anything
   ***/

  if style.kdf-width  eq "" or
     style.kdf-length eq "" then do:
    assign
     v-op-cas-pal   = 0
     v-op-tr-cnt    = 0
     v-op-numstacks = 0
     v-op-stackcode = ""
     v-op-error     = no.
    return.
  end.

  /*** CTS end ******************************************************/

  /* STEP 1
  * 1) use kdf-width and kdf-length forumlas
  * 2) convert to dec values using forumla conversion routines
  * 3) round up to nearest int
  */
  assign
    /* This will calcualte the kdf-width and store in formule[1] */
    tmpstore = style.kdf-width.
  {cec/kstyle.i
    &for=1 &l=xeb.len &w=xeb.wid &d=xeb.dep
    &k=xeb.k-wid &t=xeb.tuck &g=xeb.gluelap &b=xeb.fpanel
    &f=xeb.dust &o=xeb.lock &i=style.dim-fit
    }

  assign
    /* This will calcualte the kdf-length and store in formule[2] */
    tmpstore = style.kdf-length.
  {cec/kstyle.i
    &for=2 &l=xeb.len &w=xeb.wid &d=xeb.dep
    &k=xeb.k-wid &t=xeb.tuck &g=xeb.gluelap &b=xeb.fpanel
    &f=xeb.dust &o=xeb.lock &i=style.dim-fit
    }

  assign
    v-kdf-width  = if (int(formule[1]) eq formule[1]) then int(formule[1])
                                                      else int(formule[1] + 0.5)
    v-kdf-length = if (int(formule[2]) eq formule[2]) then int(formule[2])
                                                      else int(formule[2] + 0.5).

  /* STEP 2
  * 1) use width/length from STEP 1 to find stacking code
  *    from the stack-size file
  * 2) find the reftable to find out how many units per layer
  */
END.

find stack-size
    where stack-size.company eq cocode
      and stack-size.loc     eq locode
      and stack-size.pallet  eq xeb.tr-no
      and stack-size.line#   eq v-kdf-width
    no-lock no-error.

if not avail stack-size then
find first stack-size
    where stack-size.company eq cocode
      and stack-size.loc     eq locode
      and stack-size.pallet  eq xeb.tr-no
    no-lock no-error.

if not avail stack-size then do:
  v-op-error = yes.
  message "ERROR: Stack Size record could not be found for width:"
    string(v-kdf-width) + "!" view-as alert-box error.
  return.
end.

v-code = substr(stack-size.vals,v-kdf-length,1).

if v-code eq "" then v-code = substr(stack-size.vals,74,1).

if v-code eq "" then do:
  v-op-error = yes.
  message "ERROR: Stack Size record could not be found for length:"
    string(v-kdf-length) + "!" view-as alert-box error.
  return.
end.

FIND stackPattern
     where stackPattern.stackCode eq v-code
    no-lock no-error.
if not avail stackPattern then do:
  v-op-error = yes.
  message "ERROR: Reference table record not found for stacking type:"
    v-code + "!" view-as alert-box error.
  return.
end.
assign
 v-op-numstacks = stackPattern.stackCount
 v-op-stackcode = stackPattern.stackCode.

IF cecunit-chr EQ "FluteMtx" AND NOT ll-assem-part THEN DO:
  /* STEP 3
  * 1) use the stacking code from STEP 2, pallet code, flute and test
  *    to find the stack-flute file
  * 2) find the unit count for that pallet
  */

  if not can-find(first stack-flute
                  where stack-flute.company eq cocode
                    and stack-flute.loc     eq locode
                    and stack-flute.code    eq xeb.flute
                    and stack-flute.pallet  eq xeb.tr-no) then do:
    v-op-error = yes.
    message "ERROR: Stacking Flute record could not be found for pallet:"
      xeb.tr-no + "!" view-as alert-box error.
    return.
  end.

  assign
    v-row = ?
    v-col = ?.

  for each stack-flute
      where stack-flute.company eq cocode
        and stack-flute.loc     eq locode
        and stack-flute.code    eq xeb.flute
        and stack-flute.pallet  eq xeb.tr-no
      no-lock:

    findtestloop:
    repeat v-counter = 1 TO EXTENT(stack-flute.row-value):
      if stack-flute.row-value[v-counter] = xeb.test then do:
        v-row = v-counter.
        leave findtestloop.
      end.
    end.

    if v-row ne ? then
    findstackloop:
    repeat v-counter = 1 TO EXTENT(stack-flute.col-value):
      if stack-flute.col-value[v-counter] = stackPattern.stackCode then do:
        v-col = v-counter.
        leave findstackloop.
      end.
    end.

    if v-col ne ? then leave.

    else
      assign
       v-row = ?
       v-col = ?.
  end.

  if v-row = ? then
  do:
    v-op-error = yes.
    message "ERROR: Stacking Flute Test value:" xeb.test "NOT found!" view-as alert-box error.
    return.
  end.

  if v-col = ? then
  do:
    v-op-error = yes.
    message "ERROR: Stacking Flute Stacking code value:"
      stackPattern.stackCode "NOT found!" view-as alert-box error.
    return.
  end.

  assign
   v-index = ((v-row * 10) + v-col)
   v-op-tr-cnt = stack-flute.vals[v-index] * style.balecount
   v-op-cas-pal = v-op-tr-cnt / xeb.cas-cnt
   v-op-error = no.
END.  /* cecunit-chr EQ "FluteMtx" */

ELSE DO:
  v-op-cas-pal = xeb.cas-pal.

  RUN autocalc.
END.

RETURN.

PROCEDURE autocalc:

  IF cecunit-chr EQ "AUTOCALC" THEN DO:      
    lv-layers = v-op-cas-pal / v-op-numstacks.
    {sys/inc/roundup.i lv-layers}
  
    RUN cec/d-layers.w (NO, ROWID(xeb), xeb.tr-no, xeb.tr-dep, xeb.cas-cnt,
                        v-op-numstacks, INPUT-OUTPUT lv-layers).
     
    ASSIGN
     v-op-cas-pal = v-op-numstacks * lv-layers
     v-op-tr-cnt  = v-op-cas-pal * xeb.cas-cnt.
     
  END.  

END PROCEDURE.

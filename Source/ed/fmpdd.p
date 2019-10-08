{ed/sharedv.i}
def var i as int no-undo.
def var tq as int format "->>>>>" no-undo Column-label "Total!Units".
def var ta as int format "->>>>>" no-undo column-label "Total!Dollars".
def var n  as int format "->>>>>" no-undo column-label "Nbr!Pers".
def var aq as int format "->>>>>" no-undo column-label "Avg!Units".
def var aa as dec no-undo column-label "Avg!Dollars".
def var asp as dec no-undo column-label "Avg!S.P.".
{rc/scrvars.i}
{rc/ftopcl.i}
find pdh where pdh.recnum = ws_int no-lock no-error.
if not avail pdh then return.
display pdh with frame f-top row 1.
form
with frame f-det down row 5 center.
form
with frame f-details row 15 NO-LABELS center.
{rc/scrfm3.i
&FUNCTIONS  = "NNNY"
&INIT =
"F-DETAILS-TITLE =
'  JAN   FEB   MAR   APR   MAY   JUN   JUL   AUG   SEP   OCT   NOV   DEC'. "
&ROWS       = 5
&TITLE      = "Product Activity Data"
&FILE       = "pdd"
&INDEX      = "use-index RecItem"
&CONDITION  = "WHERE recnum = ws_int"
&POSIT      = " "
&DETFUNCT   =
"
assign tq = 0 ta = 0 n = 0.
do i = 1 to 12:
    assign
    tq = tq + pdd.qty[i]
    ta = ta + pdd.amt[i]
    n = n + (if pdd.qty[i] <> 0 then 1 else 0).
end.
assign
aq = tq / n
aa = ta / n
asp = ta / tq.
display
  Yr
  Activity-code column-label 'AC'
  price-code    column-label 'PC' format 'xx'
  suspense no-label format '*/ '
  tq
  ta
  n
  aq
  aa
  asp
  with frame f-det.
"
&CHOOSE     = "item-no"
&KEYEDIT    = " "
&DISPLAYF   =
"
"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     =
"
  pdd.Qty[1 for 12] no-label format '->>>>' skip
  pdd.Amt[1 for 12] no-label format '->>>>'
"
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = " "
}
/*
&DATAGO     = " "
&DETGO      = " "
&HASHDISP   = " "
&HASHPLUS   = " "
&HASHMINUS  = " "
&ADDPOST    = " "
*/

{ed/sharedv.i}
{rc/statline.i}
{rc/scrvars.i}
{rc/scrfm.i
&FUNCTIONS  = "NNNN"
&ROWS       = 10
&TITLE      = "Product Activity Inquiry"
&FILE       = "pdh"
&INDEX      = "use-index byCust"
&CONDITION  = "WHERE TRUE"
&POSIT      = " "
&DETFUNCT   = " "
&CHOOSE     = "cust"
&KEYEDIT    = "assign ws_int = pdh.recnum."
&DISPLAYF   =
"
  pdh.cust
  by-code
  Dept
  Mic-code
  Recnum
  Last-Line
"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     = " "
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = " "
&DATAGO     = "run ed/fmpdd.p."
}
/*
&DETGO      = " "
&HASHDISP   = " "
&HASHPLUS   = " "
&HASHMINUS  = " "
&ADDPOST    = " "
*/

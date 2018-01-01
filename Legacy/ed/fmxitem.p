{ed/sharedv.i}
{rc/callvar.i}
{rc/statline.i}
{rc/viewline.i &displayf="ws_partner"}
if not called then do:
{ed/getpart.i}
end.
else display ws_partner with frame f-view.
{rc/scrvars.i}
{rc/scrfm.i
&FUNCTIONS  = "YYYY"
&ROWS       = 10
&TITLE      = " Partner Item Cross-Reference Setup "
&FILE       = "EDicxref"
&INDEX      = " "
&CONDITION  = "WHERE edicxref.partner = ws_partner"
&POSIT      = " "
&DETFUNCT   = " "
&CHOOSE     = "cust-item"
&KEYEDIT    = " "
&DISPLAYF   = "EDICXref.Item-no"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     = " "
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = "assign edicxref.partner = ws_partner."
}
/*
&ADDPOST    = " "
&DATAGO     = " "
&DETGO      = " "
&HASHDISP   = " "
&HASHPLUS   = " "
&HASHMINUS  = " "
*/

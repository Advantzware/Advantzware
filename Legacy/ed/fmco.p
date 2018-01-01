{ed/sharedv.i}
{rc/statline.i}
{rc/viewline.i &displayf=" "}
view frame f-view.
form
with frame f-det row 3 center.
form
with frame f-details row 12 center side-labels.
{rc/scrvars.i}
{rc/scrfm3.i
&FUNCTIONS  = "YYYY"
&ROWS       = 5
&TITLE      = " EDI Controls Setup "
&FILE       = "edco"
&INDEX      = " "
&CONDITION  = "WHERE TRUE"
&POSIT      = " "
&DETFUNCT   = " "
&CHOOSE     = "company"
&KEYEDIT    = " "
&DISPLAYF   = "edco.translator edco.system edco.sf-code edco.asn-on-inv"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     = "edco.path-in colon 10 edco.path-out colon 10
    edco.path-err colon 10"
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
hide frame f-details no-pause.
hide frame f-det no-pause.

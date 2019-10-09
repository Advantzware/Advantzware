{ed/sharedv.i}
{rc/callvar.i}
{rc/statline.i}
{rc/viewline.i &displayf=" "}
view frame f-view.
{rc/scrvars.i}
{rc/scrfm.i
&FUNCTIONS  = "YYYY"
&ROWS       = 10
&TITLE      = " Carrier Code to SCAC Cross-Reference Setup "
&FILE       = "EDShipvia"
&INDEX      = " "
&CONDITION  = "WHERE true"
&POSIT      = " "
&DETFUNCT   = " "
&CHOOSE     = "carrier"
&KEYEDIT    = " "
&DISPLAYF   =
  "carrier
    HELP 'Enter carrier code as defined in application system'
    carrier-code
    help 'Enter corresponding Standard Carrier Alpha Code (SCAC)' "
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     = " "
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = " "
}
/*
&ADDPOST    = " "
&DATAGO     = " "
&DETGO      = " "
&HASHDISP   = " "
&HASHPLUS   = " "
&HASHMINUS  = " "
*/

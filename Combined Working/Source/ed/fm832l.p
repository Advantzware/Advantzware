{ed/sharedv.i}
def shared var ws_edcat_rec as recid no-undo.
def new shared var ws_catline like edcatline.line no-undo.
{rc/stripvar.i new}
{rc/statline.i}
form
    edcatline.line format '>>>>'
with frame f-det down row 3 column 2.
form
with frame f-details side-labels.
find edcat
where recid(edcat) = ws_edcat_rec no-lock no-error.
if not avail edcat then do:
    bell.
    message color value(c_err) 
    "Cannot locate catalog header".
    pause.
    return.
end.    
{rc/scrvars.i}
{rc/scrfm3.i
&FUNCTIONS  = "NYYY"
&ROWS       = 5
&TITLE      = "CATALOG ITEM EDITOR"
&FILE       = "edcatline"
&INDEX      = " "
&CONDITION  = "of edcat"
&POSIT      = "where true"
&DETFUNCT   = " "
&CHOOSE     = "line"
&KEYEDIT    = " "
&DISPLAYF   =
"
EDCatline.Vendor-Item       format 'x(10)'          column-label 'Ven Item'  
EDCatline.Description[1]    format 'x(35)'
EDCatline.Price 
EDCatline.Pack-size         column-label 'Size'
EDCatline.Size-ID           column-label 'SzID'
"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     = "
EDCatline.Description[2] 
EDCatline.Dimensions[1] 
EDCatline.Dimensions[2] 
EDCatline.Dimensions[3] 
EDCatline.Dim-uom 
EDCatline.Uom-code 
EDCatline.Qty 
EDCatline.Qty-Max 
EDCatline.Qty-Min 
EDCatline.Pack-Volume 
EDCatline.Volume-uom 
EDCatline.Pack-wght       
EDCatline.Wght-uom
EDCatline.Inner-units 
EDCatline.PID-Code 
EDCatline.Config-code 
EDCatline.UPC 
EDCatline.Color-ID 
EDCatline.Commodity-code 
EDCatline.Commodity-Grouping 
EDCatline.Commodity-qual 
EDCatline.Last-line 
EDCatline.Lines 
"
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = " "
&ADDPOST    =
"repeat:
 strip-list = 'Pricing,Exit'.
 run rc/strip.p.
 if strip-f4 then leave.
 if strip-sel[1] begins 'Pricing'     then run ed/fm832l.p. 
 else if strip-sel[1] begins 'exit' then leave.
 end. /* menu repeat */
 "
&delcode    = "
    for each edcatprice of edcatline:
        delete edcatprice.
    end.    
"
}
/*
&DETGO      = " "
&DATAGO     = " "
&HASHDISP   = " "
&HASHPLUS   = " "
&HASHMINUS  = " "
*/
hide frame f-details no-pause.
hide frame f-det no-pause.

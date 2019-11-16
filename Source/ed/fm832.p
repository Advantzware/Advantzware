{ed/sharedv.i}
def new shared var ws_edcat_rec as recid no-undo.
{rc/stripvar.i new}
{rc/statline.i}
{rc/viewline.i &displayf="ws_partner"}
if not called then do:
    {ed/getpart.i}
end.
else do:    /* 9810 CAH: this allows scroll to position on current seq */
    find eddoc where recid(eddoc) = lib_recid_ret no-lock no-error.
    find edcat of eddoc no-lock no-error.
    lib_recid_ret = if avail edcat then recid(edcat) else ?.
    release edcat.
end.
form
with frame f-det down row 3 column 2.
form
with frame f-details side-labels.
{rc/scrvars.i}
{rc/scrfm3.i
&FUNCTIONS  = "NYYY"
&ROWS       = 5
&TITLE      = "CATALOG EDITOR"
&FILE       = "edcat"
&INDEX      = " "
&CONDITION  = "where edcat.partner = ws_partner"
&POSIT      = " "
&DETFUNCT   = " "
&CHOOSE     = "seq"
&KEYEDIT    = "ws_edcat_rec = recid(edcat)."
&DISPLAYF   =
"
EDCat.Seq 
EDCat.Vn-code
EDCat.Purpose-code 
EDCat.Catalog-purpose 
EDCat.Start-date 
EDCat.Catalog 
EDCat.Lines 
"
&DATAEDIT   = " "
&TERMKEY    = " "
&UPFLDS     = "
EDCat.Catalog-number 
EDCat.Catalog-version 
EDCat.Catalog-revision 
EDCat.Description[1] 
EDCat.Description[2] 
EDCat.Avail-Date 
EDCat.Expire-Date 
EDCat.Leadtime-Code 
EDCat.Leadtime-days 
EDCat.Leadtime-Unit 
EDCat.Last-line 
EDCat.Ship-method-code 
EDCat.Terms 
EDCat.Vendor 
"
&HELPKEY    = " "
&DETEDIT    = " "
&ADDCODE    = " "
&ADDPOST    =
"repeat:
 strip-list = 'Lines,Exit'.
 run rc/strip.p.
 if strip-f4 then leave.
 if strip-sel[1] begins 'Lines'     then run ed/fm832l.p. 
 else if strip-sel[1] begins 'exit' then leave.
 end. /* menu repeat */
 "
&delcode    = "
for each edcatline of edcat: 
    for each edcatprice of edcatline:
        delete edcatprice.
    end.    
    delete edcatline.
end."
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

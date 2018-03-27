{ed/sharedv.i "new"}
    DEF new SHARED STREAM s-out.
    DEF new SHARED STREAM s-err.
    output stream s-out to c:\temp\s-out.txt.
    
    ws_edi_path = "c:\temp\edi".

    find first edcode where edcode.partner = "AMAZ" and edcode.setid = "810".
    find first eddoc where eddoc.partner = "AMAZ" and eddoc.setid = "810".
    EDDoc.Posted = TRUE.
    
    ws_edcode_rec = recid(edcode).
  top-debug = FALSE.
 
run ed/tdf/o8103060.p.

/* fg/chkporun.i   Check PO qty overrun */
/*------------------------------------------------------------------------
    File        : fg/chkporun.i
    Purpose     : 

    Syntax      :

    Description : Check PO qty overrun

    Author(s)   : Sewa Singh
    Created     : Tus Sep 17 19:29:35 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/
    
    DEFINE VARIABLE iPox AS INTEGER NO-UNDO.
    DEFINE VARIABLE dRecQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ld AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lOverUnder as LOGICAL no-undo.
    DEFINE VARIABLE dCompQty AS DECIMAL NO-UNDO.

    DEFINE BUFFER xfg-rctd FOR fg-rctd.


    RELEASE po-ordl.

    find first po-ord where po-ord.company eq cocode
                        and po-ord.po-no   eq int(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
                        no-lock no-error.
    iPox = if avail po-ord then po-ord.po-no else 0.
    if iPox ne 0 then do:
        find first po-ordl no-lock
            where po-ordl.company   eq cocode
              and po-ordl.po-no     eq iPox
              and po-ordl.i-no      eq fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
              and po-ordl.job-no    eq fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
              and po-ordl.job-no2   eq int(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
             no-error.
        if not avail po-ordl then iPox = 0.
    end.
      
    if avail po-ordl then do:
        FIND FIRST item
            WHERE item.company EQ po-ordl.company
              AND item.i-no    EQ po-ordl.i-no
            NO-LOCK NO-ERROR.

        ld = INT(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name}) .

        dRecQty = po-ordl.t-rec-qty + ld.
        
        for each xfg-rctd
            where xfg-rctd.company   eq cocode
              and xfg-rctd.i-no      eq fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
              and xfg-rctd.rita-code eq "R"
              and xfg-rctd.po-no     eq (fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
              and xfg-rctd.job-no    eq fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
              and xfg-rctd.job-no2   eq int(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
              AND RECID(xfg-rctd) <> RECID(fg-rctd)   NO-LOCK:
            dRecQty = dRecQty + xfg-rctd.t-qty. 
        end.


         dCompQty = po-ordl.ord-qty * (1 + (po-ordl.over-pct / 100)).
 
         IF po-ordl.pr-qty-uom NE "EA" THEN
             run sys/ref/convquom.p(po-ordl.pr-qty-uom , "EA" ,
                                (IF AVAIL ITEM THEN item.basis-w ELSE 0), 
                                (IF AVAIL ITEM THEN po-ordl.s-len ELSE 0), 
                                (IF AVAIL ITEM THEN po-ordl.s-wid ELSE 0),
                                (IF AVAIL ITEM THEN item.s-dep ELSE 0),
                                 dCompQty, output dCompQty).

        if dRecQty gt dCompQty THEN
        do:         
           
            message "This PO has a maximum permitted quantity of " string(dCompQty)
                 " (Including Overs allowed)"  VIEW-AS ALERT-BOX INFO .
            
                APPLY "entry" TO fg-rctd.cases .
                op-error = YES .
        END.
    END.
        
    
    

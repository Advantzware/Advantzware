OUTPUT TO c:\tmp\loadtag.dat.


FOR EACH loadtag WHERE item-type = NO BY loadtag.tag-no:
    FIND FIRST fg-bin WHERE fg-bin.company = loadtag.company
                        AND fg-bin.tag = loadtag.tag-no
                        AND fg-bin.i-no = loadtag.i-no
                      /*  AND fg-bin.loc = loadtag.loc
                       AND fg-bin.loc-bin = loadtag.loc-bin*/  NO-LOCK NO-ERROR.

    IF AVAIL fg-bin AND loadtag.case-bundle <> fg-bin.cases THEN DO:
       disp loadtag.tag-no loadtag.i-no 
           loadtag.loc LABEL "Loc"
           loadtag.loc-bin LABEL "Bin"
           loadtag.case-bundle LABEL "Units/Pallet"
           fg-bin.cases COLUMN-LABEL "Bin Unit"
           loadtag.qty-case LABEL "Unit Count"
           loadtag.pallet-count LABEL "Qty/Pallet"
           loadtag.tot-cases LABEL "Total Unit"        
           WITH DOWN WIDTH 150 STREAM-IO .
         /*loadtag.case-bundle = fg-bin.cases.*/
    END.
        

END.

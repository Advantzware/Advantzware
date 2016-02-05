/* t-flute.p    convert program from reftable to flute for GUI  */

for EACH reftable WHERE reftable.reftable = "FLUTE"
                    AND ASI.reftable.company = ""
                    AND ASI.reftable.loc = "" NO-LOCK :
    create flute.
    assign flute.company = "001"
           flute.loc = "MAIN"
           flute.code = reftable.code
           flute.thickness = reftable.val[1]
           flute.dscr = reftable.dscr       
           flute.class = reftable.code2.
                    
end.                    


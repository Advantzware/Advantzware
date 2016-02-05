    DEF VAR ss AS cha FORM "x(30)".

    FOR EACH box-design-hdr.
    
    IF box-image BEGINS "c:\rcode" THEN 
       BOX-image = REPLACE(box-image,"c:\rcode","c:\asi_gui9").
    IF box-3d-image BEGINS "c:\rcode" THEN 
      BOX-3d-image = replace(box-3d-image,"c:\rcode","c:\asi_gui9").
     
     ss = REPLACE(box-image,"c:\rcode","c:\asi_gui9").

    DISP design-no 
        box-image FORM "x(30)"
        ss
        .
    PAUSE 0.
END.

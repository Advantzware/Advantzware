/*
FOR EACH _file WHERE _file-name < "_" ,
    EACH _field OF _file WHERE _field-name = "shift".
    DISP _file-name _field-name _format.
END.
*/


/*
FOR EACH machemp :

    IF INDEX("A,B,D,M,N,O,P",machemp.shift) > 0 THEN lv-new-sht = "1".
    ELSE IF INDEX("H,I,K,L,P",machemp.shift) > 0 THEN lv-new-sht = "2".
    ELSE IF machemp.shift = "R" THEN lv-new-sht = "3".
    ELSE IF machemp.shift = "G" THEN lv-new-sht = "4".
    ELSE IF machemp.shift = "S" THEN lv-new-sht = "5".
    ELSE IF index("F,E",machemp.shift) > 0  THEN lv-new-sht = "6".
    ELSE IF machemp.shift = "C" THEN lv-new-sht = "7".
    ELSE IF INDEX("J,U",machemp.shift) > 0 THEN  lv-new-sht = "8".

END.
*/
OUTPUT TO value("c:\tmp\" + "{1}.dat").
FOR EACH {1}.
    EXPORT {1}.
END.
OUTPUT CLOSE.

lv-new-sht = "".
FOR EACH {1} :
    lv-new-sht = {2}.
    IF INDEX("A,B,D,M,N,O,P",{2}) > 0 THEN lv-new-sht = "1".
    ELSE IF INDEX("H,I,K,L,P",{2}) > 0 THEN lv-new-sht = "2".
    ELSE IF {2} = "R" THEN lv-new-sht = "3".
    ELSE IF {2} = "G" THEN lv-new-sht = "4".
    ELSE IF {2} = "S" THEN lv-new-sht = "5".
    ELSE IF index("F,E",{2}) > 0  THEN lv-new-sht = "6".
    ELSE IF {2} = "C" THEN lv-new-sht = "7".
    ELSE IF INDEX("J,U",{2}) > 0 THEN  lv-new-sht = "8".
    
    {2} = lv-new-sht.

    /*MESSAGE "{1}" {2} lv-new-sht VIEW-AS ALERT-BOX.
    IF {2} <> "" THEN LEAVE.
    */
END.

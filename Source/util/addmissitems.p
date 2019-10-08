
/*------------------------------------------------------------------------
    File        : addmissitems.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Sep 10 18:26:12 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF TEMP-TABLE ttItem LIKE item.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FOR EACH ITEM NO-LOCK WHERE
    ITEM.company EQ "002" AND 
    ITEM.mat-type NE "8":
    CREATE ttItem.
    BUFFER-COPY ITEM TO ttItem.
END.

FOR EACH ttItem:
    IF CAN-FIND(FIRST ITEM WHERE 
        ITEM.company = "001" AND 
        ITEM.i-no = ttItem.i-no) THEN DO:
        DELETE ttItem.
        NEXT.
    END.
    ASSIGN 
        ttItem.company = "001"
        ttItem.loc = "COMM".
END.

OUTPUT TO c:\tmp\fixitem.d.
FOR EACH ttItem:
    EXPORT ttItem.
END.
OUTPUT CLOSE.

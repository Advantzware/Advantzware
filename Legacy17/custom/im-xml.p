/* custom/im-xml.p  Import data from xml file */


DEFINE VARIABLE hXML        AS HANDLE     NO-UNDO.
DEFINE VARIABLE hRoot       AS HANDLE     NO-UNDO.
DEFINE VARIABLE hRecord     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hXMLField   AS HANDLE     NO-UNDO.
DEFINE VARIABLE hFieldValue AS HANDLE     NO-UNDO.

DEFINE VARIABLE iNumRecords AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNumFields  AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNumValues  AS INTEGER    NO-UNDO.

DEFINE VARIABLE hBuffer AS HANDLE     NO-UNDO.
DEFINE VARIABLE hField  AS HANDLE     NO-UNDO.

CREATE X-DOCUMENT hXML.
CREATE X-NODEREF  hRoot.
CREATE X-NODEREF  hRecord.
CREATE X-NODEREF  hXMLField.
CREATE X-NODEREF  hFieldValue.

hXML:LOAD('FILE':U, "c:\temp\xmlpo.xml":U , FALSE).

/*Get the root element handle*/
hXML:GET-DOCUMENT-ELEMENT(hRoot).

MESSAGE hroot:NAME skip
            hroot:TYPE SKIP
         hroot:SUBTYPE SKIP
        hroot:NUM-CHILDREN

          VIEW-AS ALERT-BOX.



CREATE BUFFER hBuffer FOR TABLE hRoot:name.
    
    
DO TRANSACTION:

REPEAT iNumRecords = 1 TO hRoot:NUM-CHILDREN:

    hRoot:GET-CHILD(hRecord, iNumRecords).

    MESSAGE "record: " hrecord:NAME hrecord:SUBTYPE ","
             hrecord:NUM-CHILDREN
           VIEW-AS ALERT-BOX.
    


    IF hRecord:SUBTYPE NE 'ELEMENT':U THEN NEXT.

    
    hBuffer:BUFFER-CREATE().

    
        REPEAT iNumFields = 1 TO hRecord:NUM-CHILDREN:
             
             /*Get the field element handle*/
             hRecord:GET-CHILD(hXMLField, iNumFields).
             
             /*In this tree level just the element nodes (Fields) are 
required*/
             MESSAGE "Field : " hxmlfield:NAME 
                      "," hxmlfield:SUBTYPE ","
                      hxmlfield:NODE-VALUE ","
                      hxmlfield:NUM-CHILDREN
                   VIEW-AS ALERT-BOX.

             IF hXMLField:SUBTYPE NE 'ELEMENT':U THEN NEXT.

             

             hField = hBuffer:BUFFER-FIELD(hXMLField:NAME).

             REPEAT iNumValues = 1 TO hXMLField:NUM-CHILDREN:
                 /*Get the text value of field element*/
                 hXMLField:GET-CHILD(hFieldValue, iNumValues).
                 MESSAGE "value :" inumvalues ","
                     hfieldvalue:NAME "," 
                     hfieldvalue:SUBTYPE ","
                     hfieldvalue:NODE-VALUE 
                       VIEW-AS ALERT-BOX.

                 hField:BUFFER-VALUE = hFieldValue:NODE-VALUE.

             END. /*REPEAT iNumValues = 1 TO 
hNumFields:NUM-CHILDREN:*/
         END. /*REPEAT iNumFields = 1 TO hRecord:NUM-CHILDREN:*/

END. /*REPEAT iNumRecords = 1 TO hRoot:NUM-CHILDREN:*/
END. /*DO TRANSACTION:*/



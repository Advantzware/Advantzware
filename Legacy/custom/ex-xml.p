/* custom/ex-xml.p */

&SCOPED-DEFINE xpTable customer

DEFINE VARIABLE hXML        AS HANDLE     NO-UNDO.
DEFINE VARIABLE hRoot       AS HANDLE     NO-UNDO.
DEFINE VARIABLE hFieldName  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hFieldValue AS HANDLE     NO-UNDO.
DEFINE VARIABLE hRecord     AS HANDLE     NO-UNDO.
DEFINE VARIABLE hText       AS HANDLE     NO-UNDO.

DEFINE VARIABLE hQuery  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hBuffer AS HANDLE     NO-UNDO.
DEFINE VARIABLE hField  AS HANDLE     NO-UNDO.

DEFINE VARIABLE iNumFields AS INTEGER    NO-UNDO.

CREATE X-DOCUMENT hXML.
CREATE X-NODEREF  hRoot.
CREATE X-NODEREF  hRecord.
CREATE X-NODEREF  hFieldName.
CREATE X-NODEREF  hFieldValue.
CREATE X-NODEREF  hText.

CREATE QUERY hQuery.
CREATE BUFFER hBuffer FOR TABLE "{&xpTable}":U.

hquery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH ":U + hBuffer:TABLE + " NO-LOCK":U).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

/*Create de Root element with table name as element name*/
hXML:CREATE-NODE(hRoot, hBuffer:TABLE, "ELEMENT":U).
hXML:APPEND-CHILD(hRoot).

/*Get the description value for the specified table and set it as
  node attribute*/
FIND FIRST _file WHERE _file._file-name = hBuffer:TABLE NO-LOCK.
IF _file._Desc NE "" THEN
hRoot:SET-ATTRIBUTE("Description":U, _file._Desc).

REPEAT:

   /*Create a 'Record' element.*/
   hXML:CREATE-NODE(hRecord, "Record":U, "ELEMENT":U).

   /*The hText element is used just for create a new line
     after each element*/
   hXML:CREATE-NODE(hText, "", "TEXT":U).
   hText:NODE-VALUE = "~n    ".
   
   hRoot:APPEND-CHILD(hText).
   hRoot:APPEND-CHILD(hRecord).
   
        DO iNumFields = 1 TO hBuffer:NUM-FIELDS:
  
        ASSIGN hField = hBuffer:BUFFER-FIELD(iNumFields).

        /*In order to improve performance decrease the .xml file size
         the fields with a "" value will be not added as element*/
            IF hField:BUFFER-VALUE = "" THEN NEXT.

               hXML:CREATE-NODE(hText, "", "TEXT":U).
               hText:NODE-VALUE = "~n        ".
               hRecord:APPEND-CHILD(hText).

               /*Create the field name as element*/
               hXML:CREATE-NODE(hFieldName, hField:NAME, 'ELEMENT':U).
               hRecord:APPEND-CHILD(hFieldName).

               /*Create the field value as text*/
               hXML:CREATE-NODE(hFieldValue, "text":U, 'TEXT':U).
               hFieldName:APPEND-CHILD(hFieldValue).
               hFieldValue:NODE-VALUE = hField:BUFFER-VALUE.
               
        END. /*DO iNumFields = 1 TO hBuffer:NUM-FIELDS:*/

        hXML:CREATE-NODE(hText, "", "TEXT":U).
        hText:NODE-VALUE = "~n    ".
        hRecord:APPEND-CHILD(hText).

       hQuery:GET-NEXT().
       IF hQuery:QUERY-OFF-END THEN LEAVE.
END. /*repeat*/

   hXML:CREATE-NODE(hText, "", "TEXT":U).
   hText:NODE-VALUE = "~n".
   hRoot:APPEND-CHILD(hText).

hXML:SAVE("FILE":U, hBuffer:TABLE + ".xml").


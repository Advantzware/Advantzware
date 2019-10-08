/* RelXMLDefs.i */

DEFINE VARIABLE dsRelease AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttOERelh NO-UNDO
  FIELD company AS CHAR
  FIELD cust-no AS CHAR
  FIELD ship-id AS CHAR
  FIELD ship-date AS DATE
  FIELD s-code AS CHAR.

DEFINE TEMP-TABLE ttOERell NO-UNDO
  FIELD company AS CHAR
  FIELD cust-no AS CHAR
  FIELD ord-no AS INT
  FIELD po-no AS CHAR
  FIELD i-no AS CHAR
  FIELD tag AS CHAR
  FIELD rfid AS CHAR
  FIELD qty AS DEC
  FIELD cases AS INT
  FIELD qty-case AS DEC
  FIELD partial AS DEC.

DEFINE DATASET dsRelease FOR ttOERelh, ttOERell
  DATA-RELATION hRel FOR ttOERelh, ttOERell
  RELATION-FIELDS(company,company,cust-no,cust-no) NESTED.
/*
DEFINE VARIABLE lRetOK  AS LOGICAL NO-UNDO.

DEFINE DATASET dsRelease FOR ttOERelh, ttOERell
  DATA-RELATION hRel FOR ttOERelh, ttOERell
  RELATION-FIELDS(ord-no,ord-no) NESTED.

CREATE ttoerelh.
ASSIGN
  ttoerelh.company = '001'
  ttoerelh.cust-no = 'RStark'
  ttoerelh.ord-no = 201929
  ttoerelh.ship-id = '1'
  ttoerelh.ship-date = TODAY
  ttoerelh.s-code = 'S'
  .
CREATE ttoerell.
ASSIGN
  ttoerell.ord-no = 201929
  ttoerell.qty = 999.99
  ttoerell.po-no = 'PO 1'
  ttoerell.i-no = 'Item Number 1'
  ttoerell.tag = 'Tag 1'
  ttoerell.qty-case = 0
  .
CREATE ttoerell.
ASSIGN
  ttoerell.ord-no = 201929
  ttoerell.qty = 999.99
  ttoerell.po-no = 'PO 2'
  ttoerell.i-no = 'Item Number 2'
  ttoerell.tag = 'Tag 2'
  ttoerell.qty-case = 0
  .

DEFINE DATA-SOURCE dsOERelh FOR ttOERelh.
DEFINE DATA-SOURCE dsOERell FOR ttOERell.

lRetOK = DATASET dsRelease:WRITE-XML('File','RelXML\import\release.xml',YES,?,?,FALSE,FALSE,FALSE).
*/

/*
DEFINE VARIABLE xsdFile AS CHARACTER NO-UNDO.

xsdFile = 'RelXML\import\release.xsd'.
lRetOK = DATASET dsRelease:WRITE-XMLSCHEMA('File',xsdFile,TRUE,?,FALSE).
*/

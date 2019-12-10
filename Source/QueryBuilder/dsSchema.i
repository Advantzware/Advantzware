/*------------------------------------------------------------------------

  Name : ttSchema.i
  Desc : Definition for ttTable / ttField

  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttSchemaTable NO-UNDO RCODE-INFORMATION
  FIELD cTableName    AS CHARACTER LABEL "Table"       FORMAT "X(20)"
  FIELD cTableDesc    AS CHARACTER LABEL "Description" FORMAT "X(50)"
  FIELD lSelected     AS LOGICAL   LABEL "Selected"    FORMAT "yes/no"
  FIELD iOrder        AS INTEGER   LABEL "Order"       FORMAT ">>>>>9"
  FIELD cLinkedTo     AS CHARACTER LABEL "Linked to"   FORMAT "X(20)"
  
  INDEX idxPrim IS PRIMARY cTableName
  INDEX idxSec cTableName
  INDEX idxOrder lSelected iOrder
  .

DEFINE TEMP-TABLE ttSchemaField NO-UNDO RCODE-INFORMATION
  FIELD cTableName    AS CHARACTER
  FIELD cFieldName    AS CHARACTER LABEL "Name"        FORMAT "X(40)"
                                                        
  FIELD cFullName     AS CHARACTER LABEL "Name"        FORMAT "X(40)"    /* fieldname incl extent     */
  FIELD lSelected     AS LOGICAL   LABEL "Selected"    FORMAT "yes/no"
  FIELD iOrder        AS INTEGER   LABEL "Order"       FORMAT ">>>>>9"   /* user defined order        */
  FIELD lShow         AS LOGICAL   LABEL ""                              /* toggle box                */
  FIELD cDataType     AS CHARACTER LABEL "Type"        FORMAT "X(16)"
  FIELD cFormat       AS CHARACTER LABEL "Format"      FORMAT "X(80)"    /* user defined format       */
  FIELD iWidth        AS INTEGER   LABEL "Width"        
  FIELD cLabel        AS CHARACTER LABEL "Label"       FORMAT "X(50)"
  FIELD cDesc         AS CHARACTER LABEL "Description" FORMAT "x(80)"

  INDEX idxPrim IS PRIMARY cTableName cFieldName
  INDEX idxOrder iOrder
  .
  
DEFINE DATASET dsSchema FOR ttSchemaTable, ttSchemaField.
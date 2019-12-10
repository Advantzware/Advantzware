/*------------------------------------------------------------------------
    File        : dsQuery.i
    Purpose     : Dataset definition for query

    Author(s)   : Patrick Tingen
    Created     : 2019

  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttQuery NO-UNDO {&REFERENCE-ONLY} LIKE qbQuery
  FIELD hQuery    AS HANDLE.
  
DEFINE TEMP-TABLE ttTable NO-UNDO {&REFERENCE-ONLY} LIKE qbTable
  FIELD TableDesc AS CHARACTER.
  
DEFINE TEMP-TABLE ttField NO-UNDO {&REFERENCE-ONLY} LIKE qbField
  FIELD hBuffer   AS HANDLE
  FIELD iColumnNr AS INTEGER
  FIELD hField    AS HANDLE
  FIELD hColumn   AS HANDLE
  FIELD hFilter   AS HANDLE
  FIELD lModified AS LOGICAL
  FIELD iExtent   AS INTEGER
  .

DEFINE DATASET dsQuery {&REFERENCE-ONLY} FOR ttQuery, ttTable, ttField.
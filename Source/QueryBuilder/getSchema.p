/*------------------------------------------------------------------------
  Name : getSchema.p
  Desc : Get the schema of the dictdb database

  Notes:
    This is in a separate .p because the alias is set for the dictdb.
    The reason this is done is because reading the schema statically
    is much faster than reading it via a dynamic query (factor 4 or 5)

  Input parameter ttTable should be passed BY-REFERENCE
  ----------------------------------------------------------------------*/

{dsSchema.i}
DEFINE INPUT  PARAMETER pcUserType AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET FOR dsSchema. /* by-reference */

DEFINE TEMP-TABLE ttRestriction
  FIELD cLine AS CHARACTER.
  
DEFINE VARIABLE i AS INTEGER NO-UNDO.

/* Option to exclude files and fields from the schema 
** This can be enabled for all user types. Even for super admins because
** there may be tables you don't even want them to mess with
**
** Name the files: 
**   SchemaRestrictions-normal.txt
**   SchemaRestrictions-admin.txt
**   SchemaRestrictions-superadmin.txt  
*/
FILE-INFORMATION:FILE-NAME = 'SchemaRestrictions-' + pcUserType + '.txt'.
IF FILE-INFORMATION:FULL-PATHNAME <> ? THEN
DO:
  INPUT FROM VALUE(FILE-INFORMATION:FULL-PATHNAME).
  REPEAT:
    CREATE ttRestriction.
    IMPORT ttRestriction.cLine.
  END.
END.
INPUT CLOSE. 


FIND FIRST dictdb._Db NO-ERROR.

#TableLoop:
FOR EACH dictdb._File NO-LOCK
  WHERE dictdb._File._File-Number > 0
    AND dictdb._File._File-Number < 32768
    AND (IF dictdb._Db._Db-slave THEN dictdb._File._For-Type = 'TABLE' ELSE TRUE):

  /* Skip if forbidden table */
  IF CAN-FIND(FIRST ttRestriction WHERE ttRestriction.cLine = dictdb._File._file-name) THEN NEXT #TableLoop. 

  CREATE ttSchemaTable.
  ASSIGN
    ttSchemaTable.cTableName  = dictdb._File._file-name
    ttSchemaTable.cTableDesc  = (IF dictdb._File._file-label <> ? AND dictdb._File._file-label <> '' THEN dictdb._File._file-label ELSE dictdb._File._desc)
    .

  #FieldLoop:
  FOR EACH dictdb._Field OF dictdb._File NO-LOCK:
  
    /* Skip if forbidden field */
    IF CAN-FIND(FIRST ttRestriction 
                WHERE ttRestriction.cLine = dictdb._File._file-name + '.' + dictdb._Field._Field-name) THEN NEXT #FieldLoop. 

    DO i = (IF dictdb._Field._Extent = 0 THEN 0 ELSE 1) TO (IF dictdb._Field._Extent = 0 THEN 0 ELSE dictdb._Field._Extent):
  
      CREATE ttSchemaField.
      ASSIGN 
        ttSchemaField.cTableName    = ttSchemaTable.cTableName
        ttSchemaField.cFieldName    = dictdb._Field._Field-name + (IF i = 0 THEN '' ELSE SUBSTITUTE('[&1]',i))
                                                       
        ttSchemaField.cFullName     = ttSchemaTable.cTableName + '.' + ttSchemaField.cFieldName
        ttSchemaField.iOrder        = dictdb._Field._order
        ttSchemaField.lShow         = TRUE
        ttSchemaField.cDataType     = dictdb._Field._data-type
        ttSchemaField.cFormat       = dictdb._Field._format
        ttSchemaField.cLabel        = (IF dictdb._Field._label <> ? THEN dictdb._Field._label ELSE ttSchemaField.cFieldName)
        ttSchemaField.cDesc         = dictdb._Field._desc
        ttSchemaField.iWidth        = dictdb._Field._width
        .
    END. /* do i */
  END. /* for each _field */
END. /* for each _file */

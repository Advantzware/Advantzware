define temp-table ttIndex no-undo
  field iTableNr   as integer
  field cIndexName as character
  field lUnique    as logical
  field lPrimary   as logical
  field cFields    as character
  field iNrFields  as integer
  field lKey       as logical
  index prim is primary unique
    iTableNr cIndexName
  index sor1
    lUnique iNrFields
  index sor2
    lKey iNrFields
  index sor3
    lKey cFields
  .
define temp-table ttColumn no-undo
  field iColumnNr   as integer
  field iTableNr    as integer
  field iFieldNr    as integer
  field lProcessed  as logical
  field iFieldNrAlt as integer
  index prim is primary unique
    iColumnNr
  index rel1
    iFieldNr
  index alt1 is unique
    iTableNr iFieldNr
  .
define temp-table ttRelation no-undo
  field iTableNrParent   as integer
  field iTableNrChild    as integer
  field cColumnsParent   as character
  field cColumnsChild    as character
  field cIndexNameParent as character
  field cIndexNameChild  as character
  index prim is primary unique
    iTableNrParent iTableNrChild cColumnsChild
  .
define temp-table ttTable no-undo
  field iTableNr   as integer
  field cDbName    as character
  field cTableName as character
  field iNrRecords as integer
  index prim is primary unique
    iTableNr
  index alt1 is unique
    cDbName cTableName
  .
define temp-table ttField no-undo
  field iFieldNr   as integer
  field cFieldName as character
  field cFieldType as character
  index prim is primary unique
    iFieldNr
  index alt1 is unique
    cFieldName cFieldType
  .


procedure fillRelations:
  run fillColumn.
  run fillIndex.

  run findPrimPrim.
  run findSecPrim.
  run findPrimAll.
  run findPrimTable.
end procedure.


procedure getRelatedTables:
  define input  parameter picTableName as character no-undo.
  define output parameter pocParents   as character no-undo.
  define output parameter pocChilds    as character no-undo.
  
  define buffer bufTable    for ttTable.
  define buffer bufRelation for ttRelation.
  
  assign
    pocParents = ""
    pocChilds  = ""
    .
  find ttTable where ttTable.cTableName = picTableName no-error.
  if available ttTable then
  do:
    for each ttRelation
      where ttRelation.iTableNrParent = ttTable.iTableNr:
      find bufTable where bufTable.iTableNr = ttRelation.iTableNrChild.

      pocChilds = pocChilds + ',' + bufTable.cTableName.
    end.
    for each ttRelation
      where ttRelation.iTableNrChild = ttTable.iTableNr:
      find bufTable where bufTable.iTableNr = ttRelation.iTableNrParent.

      pocParents = pocParents + ',' + bufTable.cTableName.
    end.
  end.
  
  assign
    pocParents  = trim(pocParents,',')
    pocChilds   = trim(pocChilds,',')
    .
end procedure. /* getRelatedTables */



procedure fillColumn:
  /* Fill the ttColumn Table */
  define variable iTableSeq  as integer     no-undo.
  define variable iFieldSeq  as integer     no-undo.
  define variable iColumnSeq as integer     no-undo.
   
  assign
    iTableSeq  = 1
    iFieldSeq  = 1
    iColumnSeq = 1
    .
  
  for each dictdb._File
    where dictdb._File._File-number > 0
      and dictdb._File._File-number < 32768
    no-lock
     by dictdb._file._file-name:
  
    find ttTable
      where ttTable.cDbName  = ldbname("dictdb")
        and ttTable.cTableName = dictdb._file._file-name
      no-error.
  
    if not available ttTable then
    do:
      create ttTable.
      assign
        ttTable.iTableNr   = iTableSeq
        ttTable.cDbName    = ldbname("dictdb")
        ttTable.cTableName = dictdb._file._file-name
        ttTable.iNrRecords = ?
        .
      iTableSeq = iTableSeq + 1.
    end.
  
    for each dictdb._field of dictdb._file no-lock:
  
      find ttField
        where ttField.cFieldName = dictdb._field._field-name
          and ttField.cFieldType = dictdb._field._data-type
        no-error.
  
      if not available ttField then
      do: 
        create ttField.
        assign
          ttField.iFieldNr   = iFieldSeq
          ttField.cFieldName = dictdb._field._field-name
          ttField.cFieldType = dictdb._field._data-type
          .
        iFieldSeq = iFieldSeq + 1.
      end. 
  
      find ttColumn
        where ttColumn.iTableNr = ttTable.iTableNr
          and ttColumn.iFieldNr = ttField.iFieldNr
        no-error.
  
      if not available ttColumn then
      do: 
        create ttColumn.
        assign
          ttColumn.iColumnNr = iColumnSeq
          ttColumn.iTableNr  = ttTable.iTableNr
          ttColumn.iFieldNr  = ttField.iFieldNr
          .
        iColumnSeq = iColumnSeq + 1.
      end.
    end.
  end.
end procedure. /* fillColumn */

procedure fillIndex:
  /* Fill ttIndex Table */  
  define variable cFieldsList as character   no-undo.
  define variable iNrFields   as integer     no-undo.

  for each dictdb._File
    where dictdb._File._File-number > 0
      and dictdb._File._File-number < 32768
    no-lock
    by dictdb._file._file-name:
  
    for each dictdb._index of dictdb._file
      no-lock:
  
      assign
        cFieldsList = ""
        iNrFields   = 0 
        .
      for each dictdb._Index-field of dictdb._index
        no-lock
        by dictdb._index-field._index-recid
        by dictdb._index-field._Index-seq:
  
        for each dictdb._field of dictdb._file
          where recid(dictdb._field) eq dictdb._index-field._field-recid
          no-lock:
          
          find ttField 
            where ttField.cFieldName = dictdb._field._field-name
              and ttField.cFieldType = dictdb._field._data-type
            no-error.
  
          if available ttField then
            assign
              cFieldsList = substitute('&1&2,',cFieldsList,ttField.iFieldNr)
              iNrFields   = iNrFields + 1
              .
        end.  
      end.
  
      find ttTable
        where ttTable.cDbName    = ldbname("dictdb")
          and ttTable.cTableName = dictdb._file._file-name
        no-error.
  
      if available ttTable then
      do:
        find ttIndex
          where ttIndex.iTableNr    = ttTable.iTableNr
            and ttIndex.cIndexName = dictdb._index._index-name
          no-error.
  
        if not available ttIndex then
        do:
          create ttIndex.
          assign
            ttIndex.iTableNr   = ttTable.iTableNr
            ttIndex.cIndexName = dictdb._index._index-name
            .
        end.
  
        assign
          ttIndex.lUnique   = dictdb._index._unique
          ttIndex.lPrimary  = ( recid(dictdb._index) eq dictdb._file._prime-index )
          ttIndex.lKey      = ttIndex.lPrimary
          ttIndex.cFields   = cFieldsList
          ttIndex.iNrFields = iNrFields
          .
      end.
    end.
  end.
end procedure. /* fillIndex */

/****************************************************
            F I N D   R E L A T I O N S
****************************************************/

procedure findPrimPrim:
/* Omschrijving : Zoekt de combinatie van velden uit een primaire (unieke) index
                van de ene tabel in de primaire-indexen van de andere tabellen
                op en slaat de gevonden 1:1 relatie op in de ttRelation tabel.
                Stap 1 in het zoeken van de relaties.
****************************************************************************/
  define variable cColumnsParent as character no-undo.
  define variable cColumnsChild  as character no-undo.
  define variable iNrFields      as integer   no-undo.
  define variable lOk            as logical   no-undo.
  define variable iMain          as integer   no-undo.

  define buffer bufIndexParent  for ttIndex.
  define buffer bufColumn       for ttColumn.
  define buffer bufTable        for ttTable.
  define buffer bufRelation     for ttRelation.

  for each ttIndex
    where ttIndex.lPrimary  = true
      and ttIndex.iNrFields > 0
    by ttIndex.lPrimary
      by ttIndex.iNrFields descending
    :

    for each bufIndexParent
      where bufIndexParent.lPrimary  =  true
        and bufIndexParent.cFields   =  ttIndex.cFields
        and bufIndexParent.iTableNr <> ttIndex.iTableNr
      :

      assign
        cColumnsParent = ""
        cColumnsChild  = ""
        .
      do iNrFields = 1 to ttIndex.iNrFields:
        find bufColumn
          where bufColumn.iTableNr = ttIndex.iTableNr
            and bufColumn.iFieldNr = integer(entry(iNrFields, ttIndex.cFields)).
        cColumnsChild = cColumnsChild + string(bufColumn.iColumnNr) + ",".

        find bufColumn
          where bufColumn.iTableNr = bufIndexParent.iTableNr
            and bufColumn.iFieldNr = integer(entry(iNrFields, ttIndex.cFields)).
        cColumnsParent = cColumnsParent + string(bufColumn.iColumnNr) + ",".
      end.

      run find1to1Direction (  input ttIndex.iTableNr
                            ,  input bufIndexParent.iTableNr
                            , output iMain
                            ).
      
      if iMain = ttIndex.iTableNr then
        bufIndexParent.lKey = false.
      if iMain = bufIndexParent.iTableNr then
      do:
        ttIndex.lKey = false.

        find ttRelation
          where ttRelation.iTableNrChild  = ttIndex.iTableNr
            and ttRelation.iTableNrParent = bufIndexParent.iTableNr
            and ttRelation.cColumnsChild  = cColumnsChild
          no-error.
          
        if not available ttRelation then
        do:
          create ttRelation.
          assign
            ttRelation.iTableNrChild    = ttIndex.iTableNr
            ttRelation.iTableNrParent   = bufIndexParent.iTableNr
            ttRelation.cColumnsChild    = cColumnsChild
            ttRelation.cColumnsParent   = cColumnsParent
            ttRelation.cIndexNameParent = bufIndexParent.cIndexName
            ttRelation.cIndexNameChild  = ttIndex.cIndexName
            .
        end.
      end.
    end.
  end.
  
  /* Most probable case for 1:1 relationship: there is 1 'main' table that holds all
     relations to other child table, this main table has the most records */
  /* Clean-up */
  for each ttTable
    where ttTable.iNrRecords <> ?
    by ttTable.iNrRecords descending
    :
    find first ttRelation 
      where ttRelation.iTableNrParent = ttTable.iTableNr  
      no-error.
    
    if available ttRelation then
    do:
      /* it's a parent: so delete all 1:1 references where its is a child */
      for each bufRelation
        where bufRelation.iTableNrChild = ttTable.iTableNr
        :
        delete bufRelation.
      end. 
      /* remove all other reference where this table isn't the parent
         but the same indexfields are in use */
      for each bufRelation
        where bufRelation.iTableNrParent <> ttTable.iTableNr
          and bufRelation.cColumnsParent  = ttRelation.cColumnsParent
        :
        delete bufRelation.
      end. 
    end.  
  end.
  
end procedure. /* findPrimPrim */

procedure findSecPrim:
/* Omschrijving : Zoekt de combinatie van velden uit een niet-unieke index van de
                ene tabel in de primaire-indexen van de andere tabellen op
                en slaat de gevonden relatie op in de ttRelation tabel.
                Stap 1 in het zoeken van de relaties.
****************************************************************************/
  define variable cColumnsParent as character no-undo.
  define variable cColumnsChild  as character no-undo.
  define variable iNrFields      as integer   no-undo.
  define variable lOk            as logical   no-undo.

  define buffer bufIndexParent for ttIndex.
  define buffer bufColumn      for ttColumn.

  for each ttIndex
    where ttIndex.lUnique   = false
      and ttIndex.iNrFields > 0
    by ttIndex.lUnique
      by ttIndex.iNrFields descending
    :

    for each bufIndexParent
      where bufIndexParent.lKey      =  true
        and bufIndexParent.cFields   =  ttIndex.cFields
/*        and ttIndex.cFields begins bufIndexParent.cFields            ? idee, hoe omgaan met processed columns? */
        and bufIndexParent.iTableNr <> ttIndex.iTableNr
      :

      lOk = true.

      do iNrFields = 1 to ttIndex.iNrFields:
        find ttColumn
          where ttColumn.iTableNr = bufIndexParent.iTableNr
            and ttColumn.iFieldNr = integer(entry(iNrFields, ttIndex.cFields)).
        lOk = lOk and not ttColumn.lProcessed.
      end.

      if lOk then
      do:
        assign
          cColumnsParent = ""
          cColumnsChild  = ""
          .
        do iNrFields = 1 to ttIndex.iNrFields:
          find bufColumn
            where bufColumn.iTableNr = ttIndex.iTableNr
              and bufColumn.iFieldNr = integer(entry(iNrFields, ttIndex.cFields)).
          assign
            bufColumn.lProcessed = true
            cColumnsChild        = cColumnsChild + string(bufColumn.iColumnNr) + ","
            .

          find bufColumn
            where bufColumn.iTableNr = bufIndexParent.iTableNr
              and bufColumn.iFieldNr = integer(entry(iNrFields, ttIndex.cFields)).
          cColumnsParent = cColumnsParent + string(bufColumn.iColumnNr) + ",".
        end.

        find ttRelation
          where ttRelation.iTableNrChild  = ttIndex.iTableNr
            and ttRelation.iTableNrParent = bufIndexParent.iTableNr
            and ttRelation.cColumnsChild  = cColumnsChild
          no-error.

        if not available ttRelation then
        do:
          create ttRelation.
          assign
            ttRelation.iTableNrChild    = ttIndex.iTableNr
            ttRelation.iTableNrParent   = bufIndexParent.iTableNr
            ttRelation.cColumnsChild    = cColumnsChild
            ttRelation.cColumnsParent   = cColumnsParent
            ttRelation.cIndexNameParent = bufIndexParent.cIndexName
            ttRelation.cIndexNameChild  = ttIndex.cIndexName
            .
        end.
        else
          next.
      end.
    end.
  end.
end procedure. /* findSecPrim */


procedure findPrimAll:
/* Omschrijving : Zoekt de combinatie van velden uit een primaire (unieke) index
                van de ene tabel in alle indexen van de andere tabellen
                op en slaat de gevonden relatie op in de ttRelation tabel.
                Stap 2 in het zoeken van de relaties.
****************************************************************************/
  define variable cColumnsParent as character no-undo.
  define variable cColumnsChild  as character no-undo.
  define variable iNrFields      as integer   no-undo.
  define variable lOk            as logical   no-undo.

  define buffer bufIndexChild for ttIndex.
  define buffer bufColumn     for ttColumn.

  for each ttIndex
    where ttIndex.lKey      = true
      and ttIndex.iNrFields > 0
    by ttIndex.lKey
      by ttIndex.iNrFields descending
    :

    for each bufIndexChild
      where bufIndexChild.iTableNr <> ttIndex.iTableNr
        and bufIndexChild.cFields  begins ttIndex.cFields
      by bufIndexChild.lKey descending
        by bufIndexChild.cIndexName
      :

      lOk = true.

      do iNrFields = 1 to ttIndex.iNrFields:
        find ttColumn
          where ttColumn.iTableNr = bufIndexChild.iTableNr
            and ttColumn.iFieldNr = integer(entry(iNrFields, ttIndex.cFields)).
        lOk = lOk and not ttColumn.lProcessed.
      end.

      if lOk then
      do:
        assign
          cColumnsParent = ""
          cColumnsChild  = ""
          .
        do iNrFields = 1 to ttIndex.iNrFields:
          find bufColumn
            where bufColumn.iTableNr = bufIndexChild.iTableNr
              and bufColumn.iFieldNr = integer(entry(iNrFields, ttIndex.cFields)).
          assign
            bufColumn.lProcessed = true
            cColumnsChild        = cColumnsChild + string(bufColumn.iColumnNr) + ","
            .

          find bufColumn
            where bufColumn.iTableNr = ttIndex.iTableNr
              and bufColumn.iFieldNr = integer(entry(iNrFields, ttIndex.cFields)).
          cColumnsParent = cColumnsParent + string(bufColumn.iColumnNr) + ",".
        end.

        find ttRelation
          where ttRelation.iTableNrParent = ttIndex.iTableNr
            and ttRelation.iTableNrChild  = bufIndexChild.iTableNr
            and ttRelation.cColumnsChild  = cColumnsChild
          no-error.

        if not available ttRelation then
        do:
          create ttRelation.
          assign
            ttRelation.iTableNrParent   = ttIndex.iTableNr
            ttRelation.iTableNrChild    = bufIndexChild.iTableNr
            ttRelation.cColumnsChild    = cColumnsChild
            ttRelation.cColumnsParent   = cColumnsParent
            ttRelation.cIndexNameParent = ttIndex.cIndexName
            ttRelation.cIndexNameChild  = bufIndexChild.cIndexName
            .
        end.
        else
          next.
      end.
    end.
  end.
end procedure. /* findPrimAll */

procedure findPrimTable:
/* Omschrijving : Zoekt de combinatie van velden uit de primaire index van de
                ene tabel in de andere tabellen op en slaat de gevonden
                relatie op in de relatie.rela tabel.
                Stap 3 in het zoeken van de relaties.
****************************************************************************/
  define variable cColumnsParent as character no-undo.
  define variable cColumnsChild  as character no-undo.
  define variable iNrFields      as integer   no-undo.
  define variable lCompletely    as logical   no-undo.

  define buffer bufColumnNext  for ttColumn.
  define buffer bufColumnFirst for ttColumn.

  for each ttIndex
    where ttIndex.lKey      = true
      and ttIndex.iNrFields > 0
    by ttIndex.lKey
      by ttIndex.iNrFields descending
    :

    for each bufColumnFirst
      where bufColumnFirst.iFieldNr    =  integer(entry(1, ttIndex.cFields))
        and bufColumnFirst.iTableNr   <> ttIndex.iTableNr
        and bufColumnFirst.lProcessed  =  false
      :

      lCompletely = true.

      do iNrFields = 2 to ttIndex.iNrFields:
        lCompletely = can-find( bufColumnNext
                                     where bufColumnNext.iTableNr   = bufColumnFirst.iTableNr
                                       and bufColumnNext.iFieldNr   = integer(entry(iNrFields, ttIndex.cFields))
                                       and bufColumnNext.lProcessed = false
                                 )
                       and lCompletely
                       .
      end.

      if lCompletely then
      do:
        assign
          cColumnsParent = ""
          cColumnsChild  = ""
          .
        do iNrFields = 1 to ttIndex.iNrFields:
          find ttColumn
            where ttColumn.iTableNr = bufColumnFirst.iTableNr
              and ttColumn.iFieldNr = integer(entry(iNrFields, ttIndex.cFields)).
          assign
            ttColumn.lProcessed = true
            cColumnsChild       = cColumnsChild + string(ttColumn.iColumnNr) + ","
            .

          find ttColumn
            where ttColumn.iTableNr = ttIndex.iTableNr
              and ttColumn.iFieldNr = integer(entry(iNrFields, ttIndex.cFields)).
         cColumnsParent = cColumnsParent + string(ttColumn.iColumnNr) + ",".
        end.

        find ttRelation
          where ttRelation.iTableNrParent = ttIndex.iTableNr
            and ttRelation.iTableNrChild  = bufColumnFirst.iTableNr
            and ttRelation.cColumnsChild  = cColumnsChild
          no-error.

        if not available ttRelation then
        do:
          create ttRelation.
          assign
            ttRelation.iTableNrParent   = ttIndex.iTableNr
            ttRelation.iTableNrChild    = bufColumnFirst.iTableNr
            ttRelation.cColumnsChild    = cColumnsChild
            ttRelation.cColumnsParent   = cColumnsParent
            ttRelation.cIndexNameParent = ttIndex.cIndexName
            ttRelation.cIndexNameChild  = "-= No Index =-"
            .
        end.
        else
          next.
      end.
    end.
  end.
end procedure. /*findPrimTable */

procedure find1to1Direction:
  define  input parameter piiTableNrChild  as integer no-undo.
  define  input parameter piiTableNrParent as integer no-undo.
  define output parameter poiMain          as integer no-undo.
  
  define buffer bufTableChild  for ttTable.
  define buffer bufTableParent for ttTable.
  
  define variable cTable        as character  no-undo.
  define variable hBufferParent as handle     no-undo.
  define variable hBufferChild  as handle     no-undo.
  define variable hQuery        as handle     no-undo.
  define variable cQuery        as character  no-undo.
  define variable i             as integer    no-undo.
  define variable iRecordCount  as integer    no-undo.
  
  find bufTableChild  where bufTableChild.iTableNr = piiTableNrChild.
  find bufTableParent where bufTableParent.iTableNr = piiTableNrParent.
 
  create query hQuery.

  if bufTableParent.iNrRecords = ? then
  do:
    cTable = substitute("&1.&2", bufTableParent.cDbName, bufTableParent.cTableName).
    create buffer hBufferParent for table cTable.
  
    hQuery:set-buffers(hBufferParent).
    cQuery = substitute("for each &1 no-lock", cTable).
    
    hQuery:query-prepare(cQuery).
    hQuery:query-open().
    hQuery:get-first().

    iRecordCount = 0.
    do while not hQuery:query-off-end:
      iRecordCount = iRecordCount + 1.      
      hQuery:get-next().
    end.
    bufTableParent.iNrRecords = iRecordCount.

  end.

  if bufTableChild.iNrRecords = ? then
  do:      
    cTable  = substitute("&1.&2", bufTableChild.cDbName , bufTableChild.cTableName).
    create buffer hBufferChild for table cTable.
  
    hQuery:set-buffers(hBufferChild).
    cQuery = substitute("for each &1 no-lock", cTable).
    
    hQuery:query-prepare(cQuery).
    hQuery:query-open().
    hQuery:get-first().

    iRecordCount = 0.
    do while not hQuery:query-off-end:
      iRecordCount = iRecordCount + 1.      
      hQuery:get-next().
    end.
    bufTableChild.iNrRecords  = iRecordCount.

  end.
  
  poiMain = ?.
  if bufTableParent.iNrRecords > bufTableChild.iNrRecords then 
    poiMain = bufTableParent.iTableNr.
  if bufTableChild.iNrRecords > bufTableParent.iNrRecords then 
    poiMain = bufTableChild.iTableNr.

  delete object hQuery.
  hQuery = ?.
  delete object hBufferParent no-error.
  hBufferParent = ?.
  delete object hBufferChild no-error.
  hBufferChild = ?.

  
end procedure.

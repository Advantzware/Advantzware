&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Includes     ************************** */
{ version.i }
{ htmldict.i }

/* ***************************  Parameters   ************************** */
define input  parameter pcDatabase as character  no-undo.
define output parameter table for ttArea          .                                
define output parameter table for ttField         .                                
define output parameter table for ttFile          .                                
define output parameter table for ttFileTrig      .                                
define output parameter table for ttIndex         .                                
define output parameter table for ttIndexField    .                                
define output parameter table for ttSequence      .                                
define output parameter table for ttStorageObject .                                


/* ***************************  Variables   ************************** */
define variable giFileNrStart    as integer   no-undo.
define variable giFileNrEnd      as integer   no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 6.48
         WIDTH              = 41.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Set filenumber limits */
if pcDatabase = '_progress' then
  assign
    giFileNrStart    = -32768
    giFileNrEnd      = 0
    .
else
  assign
    giFileNrStart    = 0
    giFileNrEnd      = 32768
    .

run copyFiles.
run copySequences.
run copyAreas. /* only for v9+ */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-copyAreas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyAreas Procedure 
PROCEDURE copyAreas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{&v9_begin}
  for each dictdb._area no-lock:
    create ttArea.
    assign 
      ttArea.iAreaNumber    = dictdb._area._area-number
      ttArea.cAreaName      = dictdb._area._area-name
      .
  
    storageObjectLoop:
    for each dictdb._StorageObject of dictdb._area no-lock:

      case dictdb._StorageObject._object-type:
        /* Table */
        when 1 then do:
          find ttFile where ttFile.iFileNumber = dictdb._StorageObject._object-number no-error.
          if not available ttFile then next storageObjectLoop.
          assign ttFile.cAreaName = ttArea.cAreaName.
        end.

        /* Index */
        when 2 then do:
          find ttIndex where ttIndex.iIndexNumber = dictdb._StorageObject._object-number no-error.
          if not available ttIndex then next storageObjectLoop.
          assign ttIndex.cAreaName = ttArea.cAreaName.
        end.
      end case. /* objecttype */

      create ttStorageObject.
      assign 
        ttStorageObject.iAreaNumber    = dictdb._StorageObject._area-number
        ttStorageObject.iObjectNumber  = dictdb._StorageObject._object-number
        ttStorageObject.iObjectType    = dictdb._StorageObject._object-type
        .
    end. /* _storageobject */
  end. /* _area */
{&v9_end}

end procedure. /* copyAreas */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copyFiles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyFiles Procedure 
PROCEDURE copyFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  for each dictdb._file 
    where dictdb._file._file-number > giFileNrStart
      and dictdb._file._file-number < giFileNrEnd  no-lock:
  
    create ttFile.
    assign 
      ttFile.cFileName       = dictdb._file._file-name
      ttFile.cFileDumpname   = dictdb._file._dump-name
      ttFile.iFileNumber     = dictdb._file._file-number
      ttFile.cFileLabel      = dictdb._file._file-label 
      ttFile.iFileCrc        = dictdb._file._crc
      ttFile.cFileDesc       = dictdb._file._desc
      ttFile.cFileValexp     = dictdb._file._valexp 
      ttFile.cFileValmsg     = dictdb._file._valmsg
      .

  
    /* Fill fields of table */
    for each dictdb._field of dictdb._file no-lock:
      create ttField.
      assign 
        ttField.cFileName            = dictdb._file._file-name   
        ttField.cFieldName           = dictdb._field._field-name 
        ttField.iFieldOrder          = dictdb._field._order 
        ttField.iFieldExtent         = dictdb._field._extent 
        ttField.cFieldDataType       = dictdb._field._data-type 
        ttField.iFieldDecimals       = dictdb._field._decimals 
        ttField.cFieldLabel          = dictdb._field._label 
        ttField.cFieldColLabel       = dictdb._field._col-label 
        ttField.cFieldFormat         = dictdb._field._format 
        ttField.cFieldInitial        = dictdb._field._initial 
        ttField.lFieldMandatory      = dictdb._field._mandatory 
        ttField.cFieldViewAs         = dictdb._field._view-as 
        ttField.cFieldDesc           = dictdb._field._desc
        .
    end. /* _field */
  
    for each dictdb._file-trig of dictdb._file no-lock:
      create ttFileTrig.
      assign 
        ttFileTrig.cFileName         = dictdb._file._file-name   
        ttFileTrig.cFileTrigEvent    = dictdb._file-trig._event
        ttFileTrig.cFileTrigProcName = dictdb._file-trig._proc-name
        ttFileTrig.lFileTrigOverride = dictdb._file-trig._override
        ttFileTrig.iFileTrigTrigCrc  = dictdb._file-trig._trig-crc
        .
    end. /* _file-trig */
  
    /* Skip default indexes */
    for each dictdb._index of dictdb._file no-lock
      where dictdb._index._index-name <> '' :

      create ttIndex.
      assign 
        ttIndex.cFileName            = dictdb._file._file-name   
        ttIndex.cIndexName           = dictdb._index._index-name
        ttIndex.iIndexNumber         = dictdb._index._idx-num
        ttIndex.lIndexPrimeIndex     = (dictdb._file._prime-index = recid(dictdb._index))
        ttIndex.lIndexUnique         = dictdb._index._unique
        ttIndex.lIndexWordIdx        = (dictdb._index._wordidx ne ? and dictdb._index._wordidx ne 0)
        ttIndex.lIndexActive         = dictdb._index._active
        .
  
      for each dictdb._index-field of dictdb._index no-lock, 
        dictdb._field of dictdb._index-field no-lock:
  
        create ttIndexField.
        assign 
          ttIndexField.cFileName            = dictdb._file._file-name   
          ttIndexField.cIndexName           = dictdb._index._index-name
          ttIndexField.iIndexFieldSeq       = dictdb._index-field._index-seq
          ttIndexField.cFieldName           = dictdb._field._field-name
          ttIndexField.lIndexFieldAscending = dictdb._index-field._ascending
          .
      end. /* _index-field */
    end. /* _index */
  end. /* _file */

end procedure. /* copyFiles */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copySequences) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copySequences Procedure 
PROCEDURE copySequences :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  for each dictdb._sequence no-lock:
    create ttSequence.
    assign 
      ttSequence.cSequenceName    = dictdb._sequence._seq-name
      ttSequence.iSequenceNumber  = dictdb._sequence._seq-num
      ttSequence.iSequenceInit    = dictdb._sequence._seq-init
      ttSequence.iSequenceIncr    = dictdb._sequence._seq-incr
      ttSequence.iSequenceMin     = dictdb._sequence._seq-min
      ttSequence.iSequenceMax     = dictdb._sequence._seq-max
      ttSequence.lSequenceCycleOk = dictdb._sequence._cycle-ok
      .
  end.

end procedure. /* copySequences */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


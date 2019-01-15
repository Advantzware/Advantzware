&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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

/* ***************************  Definitions  ************************** */

define temp-table ttFile no-undo 
  field cFileName            as character
  field cFileDumpName        as character
  field iFileNumber          as integer
  field cFileLabel           as character
  field iFileCrc             as integer
  field cFileDesc            as character
  field cFileValexp          as character
  field cFileValmsg          as character
  /* ********** extra fields ****************************** */
  field cFileFieldList       as character
  field cFileFirstChar       as character
  field cFilePrevUrl         as character
  field cFilePrevName        as character
  field cFileSelfUrl         as character
  field cFileNextUrl         as character
  field cFileNextName        as character
  field cAreaName            as character
  index iPrim is primary unique cFileName
  index iIdx2 cFileFirstChar cFileName.

define temp-table ttField no-undo
  field cFileName            as character
  field cFieldName           as character
  field iFieldOrder          as integer
  field iFieldExtent         as integer
  field cFieldDataType       as character
  field iFieldDecimals       as integer
  field cFieldLabel          as character
  field cFieldColLabel       as character
  field cFieldFormat         as character
  field cFieldInitial        as character
  field lFieldMandatory      as logical
  field cFieldViewAs         as character
  field cFieldDesc           as character
  /* ********** extra fields ****************************** */
  field cFieldFirstChar      as character
  field cMasterFileUrl       as character 
  field cCrossIndexUrl       as character 
  index iPrim is primary unique cFileName cFieldName
  index iIdx2 cFileName iFieldOrder
  .

define temp-table ttArea no-undo
  field iAreaNumber          as integer
  field cAreaName            as character
  index iPrim is primary unique iAreaNumber.

define temp-table ttStorageObject no-undo
  field iAreaNumber          as integer
  field iObjectType          as integer
  field iObjectNumber        as integer
  index iPrim is primary unique iAreaNumber iObjectType iObjectNumber
  index iIdx2 iObjectType iObjectNumber.


define temp-table ttFileTrig no-undo 
  field cFileName            as character
  field cFileTrigEvent       as character
  field cFileTrigProcName    as character
  field lFileTrigOverride    as logical 
  field iFileTrigTrigCrc     as integer   
  /* ********** extra fields ****************************** */
  field cFileTrigUrl         as character
  index iPrim is primary unique cFileName cFileTrigEvent.

define temp-table ttIndex no-undo
  field cFileName            as character
  field cIndexName           as character
  field iIndexNumber         as integer  
  field lIndexPrimeIndex     as logical
  field lIndexUnique         as logical
  field lIndexWordIdx        as logical
  field lIndexActive         as logical
  /* ********** extra fields ****************************** */
  field cIndexFieldList      as character
  field cAreaName            as character
  index iPrim is primary unique cFileName cIndexName
  index iIdx2 iIndexNumber
  index iIdx3 lIndexPrimeIndex cIndexName
  index iIdx4 lIndexUnique.

define temp-table ttIndexField no-undo
  field cFileName            as character
  field cIndexName           as character
  field iIndexFieldSeq       as integer
  field cFieldName           as character
  field lIndexFieldAscending as logical
  index iPrim is primary unique cFileName cIndexName iIndexFieldSeq.

define temp-table ttSequence no-undo
  field cSequenceName      as character
  field iSequenceNumber    as integer
  field iSequenceInit      as integer
  field iSequenceIncr      as integer
  field iSequenceMin       as integer
  field iSequenceMax       as integer
  field lSequenceCycleOk   as logical 
  index iPrim is primary unique cSequenceName.

/* tt for the cross reference on fields */
define temp-table ttCrossIndex no-undo
  field cFieldName         as character
  field cFileName          as character
  field cFirstChar         as character
  field cFileUrl           as character
  index p-f is primary
    cFieldName ascending
    cFileName ascending
  index p-first cFirstChar.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 4.52
         WIDTH              = 41.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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

&global-define buildnr 16
&global-define buildname "Sweet-16 edition"
          
/* TT for field data to link DataDiggers to each other */
define temp-table ttLinkInfo no-undo rcode-information
  field cField as character
  field cValue as character
  index iPrim is primary cField.


/* TT for the tables of a db */
define temp-table ttTable no-undo rcode-information
  field cTableName  as character label 'Table'     format 'x(22)'
  field cDatabase   as character label 'DB'        format 'x(10)'
  field cTableDesc  as character label 'Desc'      
  field lHidden     as logical   label 'H'         
  field iNumQueries as integer   label '#'         format 'zzzzz'
  field tLastUsed   as datetime  label 'Last Used' format '99/99/9999 HH:MM:SS'
  index iPrim is primary cDatabase cTableName.


/* TT for the saved queries of a table */
define temp-table ttQuery no-undo rcode-information
  field cDatabase as character
  field cTable    as character
  field iQueryNr  as integer
  field cQueryTxt as character
  index iQueryPrim is primary iQueryNr.


/* TT for the fields of a table */
define temp-table ttField no-undo rcode-information
  field lShow        as logical                     label ''          
  field iOrder       as decimal                     label 'Order'     format '>>>>>9'
  field cFieldName   as character                   label 'Name'      format 'x(40)'
  field cFullName    as character                   label 'Name'      format 'x(40)'
  field cDataType    as character                   label 'Type'      format 'x(16)'
  field cInitial     as character                   label 'Initial' 
  field cFormat      as character                   label 'Format'    format 'x(24)'
  field cFormatOrg   as character                   label 'Format' 
  field cLabel       as character                   label 'Label'     format 'x(24)'
  field iOrderOrg    as decimal                                                        
  field iExtent      as integer 
  field lPrimary     as logical                     label 'Pr' 
  field lVisible     as logical                     label 'Pr' 
  field lMetaField   as logical   /* if yes, show in field browse */
  field lDataField   as logical   /* if yes, show in data browse */
  field cFilterValue as character
  field cNewValue    as character                   label 'New value' format 'x(256)'
  field cOldValue    as character                   label 'Old value' format 'x(256)'
  field hColumn      as handle
  field hFilter      as handle
  index iPrim is primary iOrder.


/* TT for the indexfields of a table */
define temp-table ttIndex no-undo rcode-information
  field cIndexName   as character          label 'Name'        format 'x(20)'
  field cIndexFlags  as character          label 'Flags'       format 'x(14)'
  field cIndexFields as character          label 'Fields'      format 'x(80)'
  field cFieldList   as character          label 'Field List'  format 'x(80)'
  field lIndexActive as logical            
  .

/* TT for counting windowLocks  (WindowsUpdateLock) */
define temp-table ttWindowLock no-undo rcode-information
  field hWindow      as handle 
  field iLockCounter as integer 
  index iPrim is primary hWindow.

/* TT for filters on top of data browse */
define temp-table ttFilter no-undo rcode-information
  field cFieldName    as character
  field hFieldHandle  as handle 
  field hColumnHandle as handle 
  index iPrim is primary cFieldName
  index iFieldHandle  hFieldHandle
  index iColumnHandle hColumnHandle
  .

/* TT for favourites */
define temp-table ttConnection no-undo rcode-information
  field iConnectionNr as integer
  field cLogicalName  as character column-label "Logical Name" format "x(20)"
  field cDescription  as character column-label "Description"  format "x(28)"
  field cDatabaseName as character column-label "Database"     format "x(20)"
  field cParameters   as character
  field lConnected    as logical   column-label "Con"
  index iPrim is primary unique iConnectionNr.

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
         HEIGHT             = 6
         WIDTH              = 35.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* Global-defines for testing purposes */
&if defined(UIB_is_Running) ne 0  &then
   &global-define invar  variable 
   &global-define iovar  variable 
   &global-define outvar variable    
&else
   &global-define invar   input parameter      
   &global-define iovar   input-output parameter      
   &global-define outvar  output parameter
&endif

/* Forward defs */
function formatQueryString returns character
  ( input pcQueryString as character
  , input plExpanded    as logical ) in super.

function getDatabaseList returns character in super.

function getImagePath returns character 
  ( pcImage as character ) in super.

function getKeyList      returns character in super.

function getLinkInfo          returns character
  ( input pcFieldName as character
  ) in super.

function getMatchesValue returns character
  ( hFillIn as handle ) in super. 

function getMaxLength    returns integer   
  ( pcSection as character
  ) in super.

function getOsErrorDesc returns character
  ( input piOsError as integer
  ) in super.

function getPrimaryFields returns character
  ( input pcDatabaseName as character
  , input pcTableName    as character  
  ) in super. 

function getProgramDir returns character in super.

function getQuery returns character
  ( input pcDatabase as character
  , input pcTable    as character
  , input piQuery    as integer
  ) in super.

function getReadableQuery returns character 
  ( input pcQuery as character 
  ) in super. 

function getRegistry returns character 
  ( pcSection as character
  , pcKey     as character 
  ) in super.

function getTableList returns character
  ( input  pcDatabaseFilter   as character
  , input  pcTableFilter      as character
  , input  plShowHiddenTables as logical  
  , input  pcSortField        as character
  , input  plAscending        as logical  
  ) in super.

function getUsername returns character in super.

function isDefaultFontsChanged returns logical in super. 

function isFileLocked returns logical 
  ( pcFileName as character ) in super. 

function resolveOsVars returns character
  ( pcString as character ) in super. 

function setFilterFieldColor returns logical
  ( phWidget as handle ) in super.

function setLinkInfo returns logical 
  ( input pcFieldName as character
  , input pcValue     as character
  ) in super. 

function setRegistry returns character 
    ( pcSection as character
    , pcKey     as character
    , pcValue   as character
    ) in super.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



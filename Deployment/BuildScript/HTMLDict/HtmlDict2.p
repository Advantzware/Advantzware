&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*----------------------------------------------------------------------------
  Name         : HtmlDict2.p

  Description  : DataBase Dictionary Generator, part two.
                 This section of code will process one database

  Parameters   : I  handle      pihParent      = handle of the procedure library
                 I  character   gpcDatabaseList    = name of the db
                 I  character   gpcOutputDir     = base dir
                 I  logical     plCheckUpdate = check if update is needed
                 I  character   picFieldSort   = sort order for fields

  ----------------------------------------------------------------------------
  See HtmlDict.w for history details.
  --------------------------------------------------------------------------- */

                                      
define input  parameter gpcDatabaseList    as character  no-undo.
define input  parameter gpcOutputDir       as character  no-undo.
define input  parameter gplUseFrames       as logical    no-undo.
define input  parameter gplCheckForChanges as logical    no-undo.
define input  parameter gpcTriggerPath     as character  no-undo.
define input  parameter gpcFieldSort       as character  no-undo.

/* ***************************  Definitions  ************************** */
  
/* Preprocessors */
{ version.i }

/* Temptables for the schema */
{ htmldict.i }

/* tt for the file crc's */
define temp-table ttOldCrc no-undo
  field cFirstChar   as character
  field cFileName    as character
  field iCrc         as integer
  index p-filecrc as primary unique cFileName
  index p-first   cFirstChar.

define temp-table ttStyleSheet no-undo
  field cName  as character
  field cFile  as character
  index ttStyleSheetPrim is primary cName.

/* Global vars */
define variable gcInstallDir       as character  no-undo.
define variable gcTargetTop        as character  no-undo.
define variable gcTargetMain       as character  no-undo.
define variable giCommonStartPos   as integer    no-undo.
define variable ghRelations        as handle     no-undo.

/* Streams */
define stream sTrgOut.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fcBodyTag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fcBodyTag Procedure 
FUNCTION fcBodyTag returns character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fcHtmlHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fcHtmlHeader Procedure 
FUNCTION fcHtmlHeader returns character
  ( pcTitle     as character
  , pcRootPath  as character 
    ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fcHtmlSafe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fcHtmlSafe Procedure 
FUNCTION fcHtmlSafe returns character
  ( pcString as character ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fcNavigationLinks) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fcNavigationLinks Procedure 
FUNCTION fcNavigationLinks returns character
  ( cLinkList   as character
  , pcRelPath   as character ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fcPageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fcPageFooter Procedure 
FUNCTION fcPageFooter returns character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fcPageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fcPageHeader Procedure 
FUNCTION fcPageHeader returns character
  ( cNavigation as character
  , cPrevious   as character
  , cNext       as character
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fcVersion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fcVersion Procedure 
FUNCTION fcVersion returns character
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
         HEIGHT             = 24.57
         WIDTH              = 44.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

define variable iDatabase as integer    no-undo.

/* Initialize */
{&v9_begin}
publish "HtmlDictStatus" ("Initializing ...").
{&v9_end}
run initializeObject.


/* Install the static files */
{&v9_begin}
publish "HtmlDictStatus" ("Installing static files.").
{&v9_end}

run installStaticFiles.


/* Process all dataBases */
do iDatabase = 1 to num-entries( gpcDatabaseList ):
  run ProcessDatabase 
    ( input entry(iDatabase, gpcDatabaseList )
    , input gpcOutputDir 
    ).
end.

{&v9_begin}
publish "HtmlDictStatus" ("Installing main page and version info.").
{&v9_end}

/* Write the frontpage */
run putMainPage.

/* Version history */
run createVersionInfoFile.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-checkForChanges) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkForChanges Procedure 
PROCEDURE checkForChanges :
/*----------------------------------------------------------------------------
  Name         : checkForChanges
  Description  : Check whether an update is needed by reading in the CRC file
                 and holding the values in this file against the current CRC's
  --------------------------------------------------------------------------- */
    
  define input  parameter pcBaseDir        as character  no-undo.
  define output parameter plChangeDetected as logical    no-undo initial true.

  define variable cFile as character  no-undo.

  define buffer bFile   for ttFile.
  define buffer bOldCrc for ttOldCrc.

  /* find data files in db-dir */
  cFile = search(pcBaseDir + '/00-crc.txt').

  /* If it is not present, then regenerate */
  if cFile = ? then 
  do:
    assign plChangeDetected = true.
    return.
  end.

  /* Clean up crc list */
  for each bOldCrc: delete bOldCrc. end.

  /* Read in old CRC values for this db */
  input from value(cFile).
  repeat:
    create bOldCrc.
    import bOldCrc.cFileName
           bOldCrc.iCrc.
  end.
  input close.

  /* remove blank records */
  for each bOldCrc where bOldCrc.cFileName = '':
    delete bOldCrc.
  end.
  
  /* compare files in schema with CRC-tt */
  for each bFile:
    find bOldCrc where bOldCrc.cFileName = bFile.cFileName no-error.

    /* If not found, then this is a new table */
    if not available bOldCrc then
    do:
      plChangeDetected = true.
      return.
    end. /* n/a bOldCrc */
  end. /* f/e _file */

  /* compare CRC-tt with files in schema */
  for each bOldCrc:
    find bFile where bFile.cFileName eq bOldCrc.cFileName no-error.

    /* If not found, then the table is deleted. */
    if not available bFile
      or bOldCrc.iCrc ne bFile.iFileCrc then
    do:
      plChangeDetected = true.
      return.
    end. /* n/a bOldCrc */
  end. /* f/e _file */

end procedure. /* checkForChanges */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-copyFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyFile Procedure 
PROCEDURE copyFile :
/*----------------------------------------------------------------------------
  Name         : copyFile
  Description  : Copy a file to the output directory. This is used to copy
                 the static files. 
  --------------------------------------------------------------------------- */

  define input  parameter pcFile      as character  no-undo.

  define variable cFile as character  no-undo.
  
  cFile = search( pcFile ).
  if cFile = ? then 
    cFile = search( gcInstallDir + pcFile ).

  if cFile <> ? then
    os-copy value(search(cFile)) value( gpcOutputDir ).

end procedure. /* copyFile */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createVersionInfoFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createVersionInfoFile Procedure 
PROCEDURE createVersionInfoFile :
/*----------------------------------------------------------------------------
  Name         : createVersionInfoFile
  Description  : Create page with version history, based on htmldict.txt
  --------------------------------------------------------------------------- */
  
  define variable cLine as character no-undo. 
  define variable cFile as character  no-undo.

  /* Version info */
  output to value(gpcOutputDir + 'htmldict.html').

  /* Title of page */
  put unformatted 
         fcHtmlHeader('HtmlDict version info','')
         fcBodyTag()
    skip fcPageHeader( fcNavigationLinks('Databases','')
                     , ''
                     , ''
                     )
    skip ' '
    skip '<h1>HtmlDict version info</h1>'
    skip '<pre>'
    .
    
  /* Find version info file */
  cFile = search( 'htmldict.txt').
  if cFile = ? then cFile = search( gcInstallDir + 'htmldict.txt').

  if cFile <> ? then
  do:
    input from value( cFile ).
    repeat:
      import unformatted cLine.
      if cLine = '' then cLine = ' '.

      do while lookup('&',cLine,' ') > 0:
        entry(lookup('&',cLine,' '),cLine,' ') = '&amp;'.
      end.

      put unformatted skip cLine.
    end.
    input close. 
  end.
  else 
    put unformatted skip 'Unable to find version info file "HtmlDict.txt".'.

  put unformatted 
    skip '</pre>'
    skip '</body>'
    skip '</html>'
    .

  output close. 

end procedure. /* createVersionInfoFile */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initializeObject) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Procedure 
PROCEDURE initializeObject :
/*----------------------------------------------------------------------------
  Name         : initializeObject
  Description  : local initialize
  --------------------------------------------------------------------------- */
  
  define variable cDir      as character  no-undo.
  define variable iSubDir   as integer    no-undo.
  define variable iDatabase as integer    no-undo.
  define variable cBasedir  as character  no-undo.

  /* Determine the dir where all the install files reside */
  gcInstallDir = replace(this-procedure:file-name,'/', '\').
  gcInstallDir = substring(gcInstallDir,1,r-index(gcInstallDir,'~\':u), 'character':u).

  /* For frames and frameless version */
  if gplUseFrames = true then
    assign 
      gcTargetTop  = 'target="_top"'
      gcTargetMain = 'target="main"'.
  else 
    assign 
      gcTargetTop  = ''
      gcTargetMain = ''.


  /* Make sure outputdir ends with slash */
  gpcOutputDir = replace(gpcOutputDir,'~\','/').
  gpcOutputDir = right-trim(gpcOutputDir,'/') + '/'.

  /* Make sure outputdirs exists */
  do iDatabase = 1 to num-entries(gpcDatabaseList):
    cDir = ''.
    cBaseDir = gpcOutputDir + '/' + entry(iDatabase,gpcDatabaseList).

    do iSubDir = 1 to num-entries(cBaseDir,'/'):
      cDir = cDir + entry(iSubDir,cBaseDir,'/') + '/'.
      if iSubDir > 1 then 
      do:
        os-create-dir value(cDir).
    
        if os-error ne 0 then
        do:
          {&v9_begin}
          publish "HtmlDictStatus" (substitute("Cannot create directory '&1'",cDir)).
          {&v9_end}
          return.
        end.
      end.
    end.
  end.

  /* Start/initialize proc for finding relations. */
/*   if search('tabrela.p') <> ? then                             */
/*     run value(search('tabrela.p')) persistent set ghRelations. */
/*   else if search( gcInstallDir + 'tabrela.p') <> ? then        */
    run value( gcInstallDir + 'tabrela.p') persistent set ghRelations.

  if valid-handle(ghRelations) then
    run fillRelations in ghRelations.

end procedure. /* initializeObject */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-installStaticFiles) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE installStaticFiles Procedure 
PROCEDURE installStaticFiles :
/*----------------------------------------------------------------------------
  Name         : installStaticFiles
  Description  : Copy all static files to the output directory and create the
                 stylechange combobox. 
  --------------------------------------------------------------------------- */

  define variable cBase    as character no-undo.
  define variable cAbs     as character no-undo.
  define variable cToken   as character no-undo.
  define variable cStyle   as character no-undo.
  define variable cDirList as character no-undo.
  define variable iDir     as integer   no-undo. 
  
  /* Copy default files */
/*   run copyFile ( input 'htmldict.ico' ). */
/*   run copyFile ( input 'swapstyle.js' ). */
/*   run copyFile ( input 'htmldict.js' ).  */
/*   run copyFile ( input 'vcss.png' ).     */
/*   run copyFile ( input 'vhtml401.png' ). */

  /* Copy all css files from installdir */
  input from os-dir( gcInstallDir + 'static' ).

  copyCss:
  repeat:
    import cBase cAbs cToken.
    if not cToken begins 'F' then next copyCss.
    /* if cBase matches '*~.css' then */
    run copyFile ( input cAbs ).
  end. /* copyCss */
  input close.

  /* Create dbchooser.js */
  /* First find out how many directories are present. If there is only one, there is no 
   * need to create a combo with all possible choices 
   */
  input from os-dir(gpcOutputDir).
  repeat:
    import cBase cAbs cToken.
    if cToken begins 'D'
      and cBase ne '.'
      and cBase ne '..' then cDirList = trim(cDirList + chr(0) + cBase,chr(0)).
  end.
  input close.
  
  /* Now create the file */ 
  output to value( gpcOutputDir + "dbchooser.js" ).
  
  put unformatted
    skip 'function writeDbChooser()'
    skip '~{'.
  
  if num-entries(cDirList,chr(0)) > 1 then
  do:
    put unformatted
      skip ' document.write(~"<form name=~'selDb~'>~");' 
      skip ' document.write(~"  <select class=~'stylecombo~' name=~'db~' size=~'1~' onchange=~'parent.document.location=document.selDb.db.options[document.selDb.db.selectedIndex].value~'>~");'
      skip ' document.write(~"    <option value=~' ~' >Database:</option>)~" );'
      .
     
    do iDir = 1 to num-entries(cDirList,chr(0)):    
      put unformatted 
        skip substitute(' document.write(~"    <option value=~'../&1/00-index.html~' >&1 </option>)~" );'
                       , entry(iDir,cDirList,chr(0))
                       ).
    end.
      
    put unformatted 
      skip ' document.write(~"  </select>~" );'  
      skip ' document.write(~"</form>~" );'      
      .
  end.
         
  put unformatted 
    skip '~}'
    .
  output close. 
  
  
  /* Clean up schema temp-table */
  for each ttStyleSheet: delete ttStyleSheet. end.

  /* Read all existing css files in outputdir */
  input from os-dir( gpcOutputDir).
  output to value( gpcOutputDir + "stylechooser.js" ).

  put unformatted 
    skip 'function writeStyleChooser()'
    skip '~{'
    skip ' document.write("Style:"); '
    skip ' document.write("<select class=''stylecombo'' onchange=~'var tmpVal=this.selectedIndex;eval(this.options[tmpVal].value);this.options[tmpVal].selected=true;~'>"); '
    skip ' document.write("  <option value=~'~'> </option>");'
    .

  repeat:
    import cBase cAbs cToken.

    if cToken begins 'F'
      and cBase matches ('*~.css')
      and cBase <> "print.css" then
    do:
      create ttStyleSheet.
      assign 
        ttStyleSheet.cName = entry(1,cBase,'.') 
        ttStyleSheet.cFile = cBase  .            

      put unformatted 
        skip substitute(' document.write("  <option value=~'changeStyle(\"&1\");~'>&2</option>");'
                       , ttStyleSheet.cName
                       , ttStyleSheet.cFile
                       ).
    end.
  end.

  put unformatted 
    skip '~}'.

  input close.
  output close.

end procedure. /* installStaticFiles */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processAreas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processAreas Procedure 
PROCEDURE processAreas :
/*----------------------------------------------------------------------------
  Name         : processAreas
  Description  : Process all areas and create a page for each one. 
  --------------------------------------------------------------------------- */

  define input  parameter pcDatabase  as character  no-undo.
  define input  parameter pcOutputDir as character  no-undo.

  &scoped-define cols 4
  
  define variable iColNr    as integer    no-undo.
  define variable iRowNr    as integer    no-undo.
  define variable cRowStyle as character  no-undo.

  define buffer bArea          for ttArea.
  define buffer bStorageObject for ttStorageObject.
  define buffer bFile          for ttFile.
  define buffer bIndex         for ttIndex.

  /* ********************** list of areas ******************************** */

  output to value(pcOutputDir + "/00-area.html").

  /* Write title of web page */
  put unformatted
    fcHtmlHeader( "Areas database " + pcDatabase, '../' )
    fcBodyTag().

  /* Add Navigation links */
  put unformatted
    skip '<table style="width:100%;">'
    skip '  <tr>'
    skip '    <td class="navbar" style="width:80%;">' fcNavigationLinks('db,!areas,tables,x-idx,sequences','') '</td>'
    skip '    <td class="navbar" style="width:20%; text-align:right;"><a href="../htmldict.html"' gcTargetTop '>HtmlDict {&buildnr}</a></td>'
    skip '  </tr>'
    skip '</table>'
    skip ' '.

  
  put unformatted
    skip substitute('<h1>Areas &1</h1>',pcDatabase)
    skip '<hr>'
    skip '<table style="width:100%;">'
    skip '  <tr>'
    skip '    <th colspan="{&cols}">Tables</th> '
    skip '  </tr> '
    skip .

  /* Perform this for v9+ databases only, otherwise create empty file */
  {&v9_begin}

  iRownr = 0.
  /* Tables */
  for each bArea 
    where bArea.iAreaNumber > 5
          no-lock,
     each bStorageObject 
    where bStorageObject.iAreaNumber   = bArea.iAreaNumber
      and bStorageObject.iObjectType   = 1 /* 1=Tables 2=Indexes */
      and bStorageObject.iObjectNumber > 0
          no-lock,
     each bFile 
    where bFile.iFileNumber = bStorageObject.iObjectNumber
          no-lock
    break by bArea.iAreaNumber
          by bFile.cFileName :

    if first-of(bArea.iAreaNumber) then 
    do:
      iRownr = iRowNr + 1.
      /* Determine style for the new row */
      case iRowNr modulo 2:
        when 0 then cRowStyle = 'class="evenrow"'.
        when 1 then cRowStyle = 'class="oddrow"'.
      end case.

      put unformatted
        skip substitute('  <tr &1>', cRowStyle)
        skip substitute('    <td>&1</td>', bArea.cAreaName).
      iColNr = 0.
    end.

    put unformatted
      substitute('    <td><a href="&1">&2</a></td> '
                , bFile.cFileSelfUrl
                , bFile.cFileName
                ).

    iColNr = iColNr + 1.
    if iColNr = {&cols} - 1 then do:
      put unformatted '  </tr>'. /* Skip to new line and blank first cell */
      iColNr = 0.

      if not last-of(bArea.iAreaNumber) then
        put unformatted 
          skip substitute('  <tr &1>',cRowStyle)
          skip            '    <td>&nbsp;</td>'.
    end.

    if last-of(bArea.iAreaNumber) then
    do:
      if iColNr > 0 then
        put unformatted
          fill('~n    <td>&nbsp;</td>',{&cols} - 1 - iColNr) 
          skip '  </tr>'.
    end.
  end.

  /* Index overview */
  put unformatted
    skip '  <tr><td colspan="{&cols}"><hr></td></tr>'
    skip '  <tr>'
    skip '    <th colspan="{&cols}">Indexes</th>'
    skip '  </tr> '
    .

  iRowNr = 0.
  /* Indexes */
  for each bArea
    where bArea.iAreaNumber > 5
          no-lock,
     each bStorageObject
    where bStorageObject.iAreaNumber   = bArea.iAreaNumber
      and bStorageObject.iObjectType   = 2 /* 1=Tables 2=Indexes */
      and bStorageObject.iObjectNumber > 0
          no-lock,
     each bIndex
    where bIndex.iIndexNumber = bStorageObject.iObjectNumber
          no-lock, 
     each bFile of bIndex
          no-lock
    break by bArea.iAreaNumber
          by bFile.cFileName
          by bIndex.cIndexname:

    if first-of(bArea.iAreaNumber) then 
    do:
      iRownr = iRowNr + 1.
      /* Determine style for the new row */
      case iRowNr modulo 2:
        when 0 then cRowStyle = 'class="evenrow"'.
        when 1 then cRowStyle = 'class="oddrow"'.
      end case.

      put unformatted
        skip substitute('  <tr &1>', cRowStyle)
        skip substitute('    <td>&1</td>', bArea.cAreaName).
      iColNr = 0.
    end.

    put unformatted
      substitute('    <td><a href="&1">&2</a></td> '
                , bFile.cFileSelfUrl
                , bIndex.cIndexName
                ).

    iColNr = iColNr + 1.
    if iColNr = {&cols} - 1 then do:
      put unformatted '  </tr>'. /* Skip to new line and blank first cell */
      iColNr = 0.

      if not last-of(bArea.iAreaNumber) then
        put unformatted 
          skip substitute('  <tr &1>',cRowStyle)
          skip            '    <td>&nbsp;</td>'.
    end.

    if last-of(bArea.iAreaNumber) then
    do:
      if iColNr > 0 then
        put unformatted
          fill('~n    <td>&nbsp;</td>',{&cols} - 1 - iColNr) 
          skip '  </tr>'.
    end.
  end. /* for each bArea... */
  {&v9_end}

  put unformatted
    skip '</table>'.

  /* Finish page */
  put unformatted
    skip fcPageFooter()
    skip '</body>'
    skip '</html>'.
  output close.

end procedure. /* processAreas */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processCrossIndex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processCrossIndex Procedure 
PROCEDURE processCrossIndex :
/*----------------------------------------------------------------------------
  Name         : processCrossIndex
  Description  : Create cross index page.
  --------------------------------------------------------------------------- */
  
  define input  parameter pcDatabase  as character  no-undo.
  define input  parameter pcOutputDir as character  no-undo.

  define variable cRowStyle as character  no-undo.
  define variable iRowNr    as integer    no-undo.
  define variable iChar     as integer    no-undo.
  define variable cFile     as character  no-undo.

  do iChar = 0 to 26:

    /* check to see if we need to do anything */
    if    iChar > 0
      and not can-find(first ttCrossIndex
                       where ttCrossIndex.cFirstChar = chr( 64 + iChar )) then
      next.

    assign cFile = lower( '/00-cross' + (if iChar = 0 then '' else '-' + chr(64 + iChar)) + '.html').
    output to value(pcOutputDir + cFile).

    put unformatted
      fcHtmlHeader( 'Crossindex fields ' + pcDatabase, '../' )
      fcBodyTag().

    /* Add Navigation links */
    put unformatted
      skip '<table style="width:100%;">'
      skip '  <tr>'
      skip '    <td class="navbar" style="width:80%;">' fcNavigationLinks('db,areas,tables,!x-idx,sequences','') '</td>'
      skip '    <td class="navbar" style="width:20%; text-align:right"><a href="../htmldict.html"' gcTargetTop '>HtmlDict {&buildnr}</a></td>'
      skip '  </tr>'
      skip '</table>'
      skip ' '
      skip '<h1>Cross index</h1>'
      skip .

    /* Make a line with starting chars */
    put unformatted
      '<table><tr><td>|</td>' skip.

    for each ttCrossIndex break by ttCrossIndex.cFirstChar:
      if first-of( ttCrossIndex.cFirstChar ) then
        put unformatted
          '  <td><a href="00-cross-' ttCrossIndex.cFirstChar '.html">'
          '&nbsp;' + ttCrossIndex.cFirstChar + '&nbsp;</a>&nbsp;|</td>' skip.
    end.
    put unformatted '</tr></table>' skip.

    /* Start the table */
    put unformatted skip '<table style="width:100%;">'.

    for each ttCrossIndex no-lock
      where ttCrossIndex.cFirstChar = lower( chr( 64 + iChar ))
      break by ttCrossIndex.cFirstChar
            by ttCrossIndex.cFieldName
            by ttCrossIndex.cFileName:

      /* New char */
      if first-of(ttCrossIndex.cFirstChar) then
        put unformatted
          skip '  <tr class="separator" />'
          skip '  <tr>'
          skip '     <th colspan="2" style="text-align:center;"><a name="' ttCrossIndex.cFirstChar '"></a>'
                    '- ' caps(ttCrossIndex.cFirstChar) ' -</th>'
          skip '  </tr>'.

      /* New field */
      if first-of(ttCrossIndex.cFieldName) then
      do:
        assign iRowNr = iRowNr + 1.

        /* Determine style for the new row */
        case iRowNr modulo 2:
          when 0 then cRowStyle = 'class="evenrow"'.
          when 1 then cRowStyle = 'class="oddrow"'.
        end case.

        put unformatted
          skip substitute('  <tr &1>', cRowStyle)
          skip '      <td> <a name="' ttCrossIndex.cFieldName '"></a>' ttCrossIndex.cFieldName '</td>'
          skip '      <td>'.
      end. /* new char */

      /* field reference */
      put unformatted '<a href="' ttCrossIndex.cFileUrl '">' ttCrossIndex.cFileName '</a>&nbsp;&nbsp; '.

      if last-of(ttCrossIndex.cFieldName) then
        put unformatted '</td>'
          skip '  </tr>'.
    end.

    put unformatted
      skip '  </table>'
      skip fcPageFooter()
      skip '</body>'
      skip '</html>'
      skip.

    output close.
  end. /* iChar */

end procedure. /* processCrossIndex */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processDatabase) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processDatabase Procedure 
PROCEDURE processDatabase :
/*----------------------------------------------------------------------------
  Name         : processDatabase
  Description  : Process the database: collect metaschema info, check whether
                 an update is needed and start the generating part. 
  --------------------------------------------------------------------------- */
  
  define input  parameter pcDatabase  as character  no-undo.
  define input  parameter pcOutputDir as character  no-undo.

  define variable lSchemaHasChanged as logical    no-undo.
  define variable cBaseDir          as character  no-undo.

  define buffer bArea          for ttArea         .
  define buffer bField         for ttField        .
  define buffer bFile          for ttFile         .
  define buffer bFileTrig      for ttFileTrig     .
  define buffer bIndex         for ttIndex        .
  define buffer bIndexField    for ttIndexField   .
  define buffer bSequence      for ttSequence     .
  define buffer bStorageObject for ttStorageObject.

  /* Set the alias 'dictdb' to the selected database */
  if pcDatabase <> '_progress' then
    create alias 'dictdb' for database value(pcDatabase).

  /* Set basedir for this db */
  assign 
    cBaseDir = replace(pcOutputDir + pcDatabase,'\','/').

  {&v9_begin}
  publish "HtmlDictStatus" (substitute("Collecting schema information for database &1",pcDatabase)).
  {&v9_end}

  /* Get the schema of the database */
  run value( gcInstallDir + 'htmldict0.p') 
    ( input pcDatabase
    , output table bArea         
    , output table bField        
    , output table bFile         
    , output table bFileTrig     
    , output table bIndex        
    , output table bIndexField   
    , output table bSequence     
    , output table bStorageObject
    ).

  /* 
   * Determine whether an update of the html pages is  
   * needed by looking at the CRC's.
   */
  if gplCheckForChanges then 
  do:
    {&v9_begin}
    publish "HtmlDictStatus" (substitute("Checking for changes in schema of database &1",pcDatabase)).
    {&v9_end}

    run checkForChanges ( input cBaseDir, output lSchemaHasChanged ).
    if not lSchemaHasChanged then return.
  end.

  /*
   * Determine order and url of files
   */
  {&v9_begin}
  publish "HtmlDictStatus" (substitute("Analyzing schema of database &1",pcDatabase)).
  {&v9_end}
  run processTempTables.

  /* Write new CRC file */
  output to value(cBaseDir + '/00-crc.txt').
  for each bFile:
    export bFile.cFileName
           bFile.iFileCrc.
  end.
  output close.


  /* Write main page and list of files for this db */
  run putTableListPage 
    ( input pcDatabase 
    , input cBaseDir
    ).

  /* Write pages for all tables */
  for each bFile:
    {&v9_begin}
    if etime > 1000 then
    do:
      publish "HtmlDictStatus" (substitute("Processing table '&1' of database '&2'",bFile.cFileName,pcDatabase)).
      etime(yes).
    end.
    {&v9_end}

    run processTable
      ( input pcDatabase 
      , input bFile.cFileName
      , input cBaseDir
      ).
  end.

  /* Write page for areas */
  {&v9_begin}
  publish "HtmlDictStatus" (substitute("Writing area page of database &1",pcDatabase)).
  {&v9_end}

  run processAreas
    ( input pcDatabase 
    , input cBaseDir
    ).

  /* Write page for sequences */
  {&v9_begin}
  publish "HtmlDictStatus" (substitute("Writing sequence page of database &1",pcDatabase)).
  {&v9_end}

  run processSequences
    ( input pcDatabase 
    , input cBaseDir
    ).

  
  /* Write page for cross index */
  {&v9_begin}
  publish "HtmlDictStatus" (substitute("Writing cross index page of database &1",pcDatabase)).
  {&v9_end}

  run processCrossIndex
    ( input pcDatabase 
    , input cBaseDir
    ).


end procedure. /* processDatabase */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processSequences) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processSequences Procedure 
PROCEDURE processSequences :
/*----------------------------------------------------------------------------
  Name         : processSequences
  Description  : Process the sequences and generate a page for them.
  --------------------------------------------------------------------------- */
  
  define input  parameter pcDatabase  as character  no-undo.
  define input  parameter pcOutputDir as character  no-undo.

  define variable iRowNr     as integer    no-undo.
  define variable iColNr     as integer    no-undo.
  define variable cRowStyle  as character  no-undo.

  /* ********************** Sequences     ******************************** */
  
  /* Define master file */
  output to value(pcOutputDir + "/00-sequences.html").

  /* Write title of web page */
  put unformatted
    fcHtmlHeader( "Sequences database " + pcDatabase, '../' )
    fcBodyTag()
    .

  /* Add Navigation links */
  put unformatted
    skip '<table style="width:100%;">'
    skip '  <tr>'
    skip '    <td class="navbar" style="width:80%;">' fcNavigationLinks('!sequences,db,x-idx,tables,areas','') '</td>'
    skip '    <td class="navbar" style="width:20%; text-align:right"><a href="../htmldict.html"' gcTargetTop '>HtmlDict {&buildnr}</a></td>'
    skip '  </tr>'
    skip '</table>'
    skip ' '
    skip substitute('<h1>Sequences &1</h1>',pcDatabase)
    skip '<hr>'
    skip '<table>'
    skip '  <tr style="text-align:left;">'
    skip '    <th> Sequence name </th> '
         '    <th> Initial       </th> '
         '    <th> Increment     </th> '
         '    <th> Minimum       </th> '
         '    <th> Maximum       </th> '
         '    <th> Cycle         </th> '
    skip '  </tr>'.

  if pcDatabase <> '_Progress' then
  do:
    iRowNr = 0.
    for each ttSequence:
      iRowNr = iRowNr + 1.

      /* Determine style for the new row */
      case iRowNr modulo 2:
        when 0 then cRowStyle = 'class="evenrow"'.
        when 1 then cRowStyle = 'class="oddrow"'.
      end case.

      put unformatted
        skip substitute('  <tr &1>', cRowStyle)
        skip '    <td style="width:50%;">'        ttSequence.cSequenceName '</td>'
             '    <td style="text-align:right;">' ttSequence.iSequenceInit '</td>'
             '    <td style="text-align:right;">' ttSequence.iSequenceIncr '</td>'
             '    <td style="text-align:right">'  ttSequence.iSequenceMin  '</td>'
             '    <td style="text-align:right">'  ttSequence.iSequenceMax  '</td>'
             '    <td style="text-align:left" >'  ttSequence.lSequenceCycleOk '</td>'
        skip '  </tr>'.
    end.
  end.

  put unformatted '</table>'.

  /* Finish page */
  put unformatted
    skip fcPageFooter()
    skip '</body>'
    skip '</html>'.
  output close.

end procedure. /* processSequences */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processTable Procedure 
PROCEDURE processTable :
/*----------------------------------------------------------------------------
  Name         : ProcessTable
  Description  : Process a single table.
  --------------------------------------------------------------------------- */
    
  define input parameter pcDatabase  as character no-undo. /* database */
  define input parameter pcTableName as character no-undo. /* table to process */
  define input parameter pcOutputDir as character no-undo. /* dir to write to */

  define variable cViewAs          as character  no-undo.
  define variable cDesc            as character  no-undo.
  define variable cMasterTable     as character  no-undo.
  define variable cCrossIndexLink  as character  no-undo.
  define variable cExtent          as character  no-undo.
  define variable cDecimals        as character  no-undo.
  define variable cStyleList       as character  no-undo.
  define variable iStyle           as integer    no-undo.
  define variable cRelatedParents  as character  no-undo.
  define variable cRelatedChilds   as character  no-undo.
  define variable iCounter         as integer    no-undo.
  define variable iRowNr           as integer    no-undo.
  define variable cRowStyle        as character  no-undo.

  define buffer bFile for ttFile. 
  define buffer bufFile for ttFile. 

  /* ********** Open the page **************************************** */

  /* Find yourself */
  find bFile where bFile.cFileName = pcTableName no-lock.

  output to value(pcOutputDir + "/" + bFile.cFileSelfUrl).


  /* ********** Header stuff ***************************************** */

  put unformatted
    /* html header */
    skip fcHtmlHeader( 'Schema ' + pcDatabase + '.' + pcTableName, '../' )
    skip fcBodyTag()
    .


  /* ********** List Of Tables (no frames) *************************** */
  if not gplUseFrames then 
  do:
    put unformatted
      skip '<div class="tablelist">'
      skip '<script type="text/javascript">'
      skip '  <!--'
      skip '    writeDbChooser();'
      skip '  // -->'
      skip '</script>'
    
      skip substitute('<div class="tablelistheader">&1</div>',pcDatabase)
      .
  
    for each bufFile by bufFile.cFileName:
      put unformatted 
        skip substitute('<div class="tablelistitem"><a href="&1">&2</a></div>'
                       , bufFile.cFileSelfUrl
                       , bufFile.cFileName
                       ).
    end.
  
    put unformatted 
      skip '</div>'
      skip '<div class="tabledetail">'.
  end.


  /* ********** Header stuff (cont) ********************************** */
  put unformatted
    skip ' '
    /* Navigation links */
    skip fcPageHeader( fcNavigationLinks('db,areas,tables,x-idx,sequences','')
                     , substitute('  &&lt;- <a href="&1">&2</a>', bFile.cFilePrevUrl, bFile.cFilePrevName)
                     , substitute('  <a href="&1">&2</a> -&&gt;', bFile.cFileNextUrl, bFile.cFileNextName)
                     )
    /* title */
    skip substitute('<h1>&1.&2</h1>', pcDatabase, pcTableName)
    skip substitute('<p>&1</p>',bFile.cFileDesc)
    .


  /* ********** Table details **************************************** */

  put unformatted substitute(
    '~n<hr>' +
    '~n<div class="clearboth">' +
    '~n  <div class="tbdetcol">' +
    '~n    <h2>Table details</h2>' +
    '~n    <table style="border-style:none;" >' +
    '~n      <tr class="shadedrow"><th>Area</th>' +
    '~n                            <td><a href="00-area.html">&1</a></td></tr>' +
    '~n      <tr class="shadedrow"><th>Dumpname </th>' +
    '~n                            <td> &2      </td></tr>' +
    '~n      <tr class="shadedrow"><th>Label    </th>     ' +
    '~n                            <td> &3      </td></tr>' +
    '~n      <tr class="shadedrow"><th>Val.exp  </th>     ' +
    '~n                            <td> &4      </td></tr>' +
    '~n      <tr class="shadedrow"><th>Val.msg  </th>     ' +
    '~n                            <td> &5      </td></tr>' +
    '~n    </table>' +
    '~n  </div>'
    , bFile.cAreaName 
    , bFile.cFileDumpName
    , bFile.cFileLabel
    , bFile.cFileValexp
    , bFile.cFileValmsg
    ).


  /* ********** Table triggers *************************************** */
  run putTableTriggers
    ( input pcDatabase 
    , input pcTableName 
    ).

  /* ********** Fields of this table ********************************* */
  run putTableFields
    ( input pcDatabase 
    , input pcTableName 
    ).

  /* ********** Indexes ********************************************** */
  run putTableIndex
    ( input pcDatabase 
    , input pcTableName 
    ).
      
  /* ********** Table Relations ************************************** */

  {&v9_begin}
  /* Show table detail info relations */
  run getRelatedTables in ghRelations
    (  input pcTableName
    , output cRelatedParents
    , output cRelatedChilds
    ).

  if cRelatedParents  <> ""
    or cRelatedChilds <> "" then
  do:
    put unformatted
      '~n  <h2>Related tables</h2>'.

    if cRelatedParents  <> "" then
    do:
       put unformatted
        '~n  <table style="width:50%; float:left;">'
        '~n    <tr>'
        '~n      <th>Parent tables</th>'
        '~n    </tr>'
        '~n    <tr class="shadedrow">'
        '~n      <td>'
        .

      do iCounter = 1 to num-entries(cRelatedParents):

        find first bFile where bFile.cFileName = entry(iCounter, cRelatedParents) no-lock.
        put unformatted
          substitute('<a href="&1">&2</a>&3&3 '
                    , bFile.cFileSelfUrl
                    , bFile.cFileName
                    , '&nbsp;'
                    ).
      end.

      put unformatted
             '</td>'
        skip '    </tr>'
        skip '  </table>'
        .
    end. /* cRelatedParents  <> "" */

    if cRelatedChilds <> "" then
    do:
       put unformatted
        '~n  <table style="width:50%; float:left;">'
        '~n    <tr>'
        '~n      <th>Child tables</th>'
        '~n    </tr>'
        '~n    <tr class="shadedrow">'
        '~n      <td>'
        .

      do iCounter = 1 to num-entries(cRelatedChilds):

        find first bFile where bFile.cFileName = entry(iCounter, cRelatedChilds) no-lock.
        put unformatted
          substitute('<a href="&1">&2</a>&3&3 '
                    , bFile.cFileSelfUrl
                    , bFile.cFileName
                    , '&nbsp;'
                    ).
      end.

      put unformatted
             '</td>'
        skip '    </tr>'
        skip '  </table>'
        .
    end. /* cRelatedChilds <> "" */

    put unformatted
      skip '<hr style="clear:both">'.

  end.
  {&v9_end}

  /* Finish page */
  put unformatted
    skip fcPageFooter()
    .

  /* End DIV in case of non-framed version */
  if not gplUseFrames then put unformatted skip '</div>'.

  put unformatted 
    skip '</body>' 
    skip '</html>' 
    skip
    .
  output close.

end procedure. /* ProcessTable */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processTempTables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processTempTables Procedure 
PROCEDURE processTempTables :
/*----------------------------------------------------------------------------
  Name         : processTempTables
  Description  : Post-processing of the temptables that hold the information
                 about the metaschema. 
  --------------------------------------------------------------------------- */
    
  define variable cUrl             as character  no-undo.
  define variable cName            as character  no-undo.
  define variable cTableFileName   as character  no-undo.
  define variable iNrOfTables      as integer    no-undo.
  define variable iTotalNrOfTables as integer    no-undo.
  define variable cFirstTableName  as character  no-undo.
  
  define buffer bFile          for ttFile         .
  define buffer bFileMaster    for ttFile         .
  define buffer bStorageObject for ttStorageObject.
  define buffer bArea          for ttArea         .
  define buffer bIndex         for ttIndex        .
  define buffer bIndexField    for ttIndexField   .
  define buffer bField         for ttField        .
  define buffer bCrossIndex    for ttCrossIndex   .


  /*** Search common start position in table names ******************************************/

  /* Count nr of tables */
  for each bFile no-lock: iTotalNrOfTables = iTotalNrOfTables + 1. end.

  if iTotalNrOfTables = 1 then
  do:
    assign 
      giCommonStartPos = 1.
  end.
  else 
  do:
    find first bFile.

    assign 
      cFirstTableName = bFile.cFileName
      giCommonStartPos = 1.

    findCommon:
    repeat:
      assign iNrOfTables = 0.
  
      for each bFile no-lock
         where bFile.cFileName begins substring(cFirstTableName,1,giCommonStartPos,"character"):
        iNrOfTables = iNrOfTables + 1.
      end.
  
      if iNrOfTables < iTotalNrOfTables then
        leave findCommon.
  
      assign giCommonStartPos = giCommonStartPos + 1.
    end. /* findCommon */
  end.


  /*** Next & prev *************************************************************/

  /* Walk through the list of tables to set the link to the next and the prev table. 
   * From beginning to end 
   */
  for each bFile by bFile.cFileName:

    if bFile.cFileDumpName = ? 
      then cTableFileName = bFile.cFileName.
      else cTableFileName = bFile.cFileDumpName .

    /* Determine url of this file */
    assign bFile.cFileSelfUrl  = replace( cTableFileName, "#":U, "%23":U) + ".html".

    /* Determine first letter of the name */
    assign bFile.cFileFirstChar = substring(bFile.cFileName,giCommonStartPos,1).

    /* Set desc to label if unknown */
    if    (bFile.cFileDesc   = ? or bFile.cFileDesc   = "") 
      and (bFile.cFileLabel <> ? or bFile.cFileLabel <> ? ) then 
      assign bFile.cFileDesc = bFile.cFileLabel.


    /* Link to previous file */
    assign 
      bFile.cFilePrevUrl  = cUrl
      bFile.cFilePrevName = cName
      cUrl                = bFile.cFileSelfUrl
      cName               = bFile.cFileName
      .
  end.

  /* And now backwards */
  cUrl = ''.
  cName = ''.
  for each bFile by bFile.cFileName descending:

    /* Determine url of this file */
    assign 
      bFile.cFileNextUrl  = cUrl
      bFile.cFileNextName = cName
      cUrl                = bFile.cFileSelfUrl
      cName               = bFile.cFileName
      .
  end.


  /*** Find areas **************************************************************/

  /* Find the area for each table (v9+ only) */
  {&v9_begin}
  for each bFile
           no-lock, 
      each bStorageObject
     where bStorageObject.iObjectType   = 1 /* 1=file 2=index */
       and bStorageObject.iObjectNumber = bFile.iFileNumber
           no-lock,
      each bArea
     where bArea.iAreaNumber = bStorageObject.iAreaNumber
           no-lock:

    assign bFile.cAreaName = bArea.cAreaName.
  end.

  /* and the area for each index */
  for each bIndex 
           no-lock, 
      each bStorageObject
     where bStorageObject.iObjectType   = 2 /* 1=file 2=index */
       and bStorageObject.iObjectNumber = bFile.iFileNumber
           no-lock,
      each bArea
     where bArea.iAreaNumber = bStorageObject.iAreaNumber
           no-lock:
    
    assign bIndex.cAreaName = bArea.cAreaName.
  end.
  {&v9_end}


  /*** Find master table ******************************************************************/

  /* If this field is the only field in the primary index of another table, place
   * a link to that table (except if this /IS/ that table of course. 
   */
  for each bIndex:
    for each bIndexField 
       where bIndexField.cFileName  = bIndex.cFileName
         and bIndexField.cIndexName = bIndex.cIndexName
          by bIndexField.cFileName
          by bIndexField.cIndexName
          by bIndexField.iIndexFieldSeq:
      assign bIndex.cIndexFieldList = bIndex.cIndexFieldList + ',' + bIndexField.cFieldName. 
    end.

    assign bIndex.cIndexFieldList = trim(bIndex.cIndexFieldList, ','). 
  end.

  for each bFile
           no-lock, 
      each bField 
     where bField.cFileName = bFile.cFileName 
           no-lock:

    /* Set first letter */
    assign bField.cFieldFirstChar = substring(bField.cFieldName,1,1).

    /* Add to cross index */
    create bCrossIndex.
    assign bCrossIndex.cFieldName = bField.cFieldName 
           bCrossIndex.cFileName  = bField.cFileName
           bCrossIndex.cFirstChar = lower(substring(bField.cFieldName,1,1))
           bCrossIndex.cFileUrl   = bFile.cFileSelfUrl
      .
    assign bField.cCrossIndexUrl  = substitute('<a href="00-cross-&1.html#&2"><small>X</small></a>'
                                              , bCrossIndex.cFirstChar
                                              , bField.cFieldName
                                              )
           .

    /* If this field is the only field in a unique index of another table, 
     * create a link to it */
    find first bIndex
      where bIndex.lIndexUnique    =  true
        and bIndex.cIndexFieldList =  bField.cFieldName
        and bIndex.cFileName       <> bField.cFileName
            no-error.

    if available bIndex then
    do:
      find bFileMaster where bFileMaster.cFileName = bIndex.cFileName.
      assign bField.cMasterFileUrl = bFileMaster.cFileSelfUrl.
    end.
  end.

end procedure. /* processTempTables */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-processTriggerCode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE processTriggerCode Procedure 
PROCEDURE processTriggerCode :
/*----------------------------------------------------------------------------
  Name         : processTriggerCode
  Description  : Create an html page for a table trigger. 
  --------------------------------------------------------------------------- */
    
  define input  parameter pcDatabase        as character no-undo. /* database */
  define input  parameter pcTriggerFileName as character   no-undo.
  define input  parameter pcEventName       as character   no-undo.
  define input  parameter pcFileName        as character   no-undo.
  define output parameter pcTriggerFile     as character   no-undo.

  define variable cTrgPath     as character   no-undo.
  define variable cTempDir     as character   no-undo.
  define variable cTempFile    as character   no-undo.
  define variable cTrgFileName as character   no-undo.
  define variable cFileLine    as character   no-undo.

  define variable hFileSystemObject as com-handle no-undo.
  define variable hFile             as com-handle no-undo.

  create "Scripting.FileSystemObject" hFileSystemObject.
  assign hFile = hFileSystemObject:OpenTextFile( pcTriggerFileName
                                               , 1 /* ForReading */
                                               , false
                                               , 0 /* TristateFalse */
                                               ).
  assign
    cTempDir     = session:temp-directory
    cTrgFileName = substitute( "&1_&2.html", pcFileName, pcEventName)
    cTempFile    = substitute( "&1/&2",cTempDir,cTrgFileName).

  /* Create trigger subdirectory */
  cTrgPath = substitute("&1/&2/trg", gpcOutputDir, pcDatabase).
  os-create-dir value(cTrgPath).
  if os-error ne 0 then
  do:
    message "Cannot create directory ":U cTrgPath view-as alert-box.
    return.
  end.

  output stream sTrgOut to value(cTempFile).

  put stream sTrgOut unformatted
    fcHtmlHeader( substitute('Trigger code: &1 of &2',pcEventName,pcFileName), '../../' )
    fcBodyTag().

  put  stream sTrgOut unformatted
      skip '<table style="width:100%;">'
      skip '  <tr>'
      skip '    <td class="navbar" style="width:80%;">' fcNavigationLinks('db,areas,tables,x-idx,sequences','../../') '</td>'
      skip '    <td class="navbar" style="width:20%; text-align:right;"><a href="../../htmldict.html"' gcTargetTop '>HtmlDict {&buildnr}</a></td>'
      skip '  </tr>'
      skip '</table>'
      skip ' '
      skip substitute('<h1>Trigger code: &1 of &2</h1>',pcEventName,pcFileName)
      skip '<div class="progressCode">'.


  do while hFile:AtEndOfStream = false:
    cFileLine = hFile:ReadLine.

    if cFileLine = "":u then
      put stream sTrgOut unformatted skip(1).
    else
      put stream sTrgOut unformatted skip cFileLine.
  end.

  put stream sTrgOut unformatted
    skip '</div><!--progresscode-->'  /* signal for src2Html to stop parsing */
    skip '</body>'
    skip '</html>'
    skip.

  hFile:close().

  release object hFileSystemObject.
  release object hFile.

  output stream sTrgOut close.

  run value( gcInstallDir + 'src2html.p')
    ( input cTempDir
    , input cTrgPath
    , input cTrgFileName
    ).

  os-delete value(cTempFile).

  pcTriggerFile = substitute( "trg/&2",cTrgPath,cTrgFileName).

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-putMainPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE putMainPage Procedure 
PROCEDURE putMainPage :
/*----------------------------------------------------------------------------
  Name         : putMainPage
  Description  : Create the main page. 
  --------------------------------------------------------------------------- */
  
  define variable cLine  as character no-undo.
  define variable cBase  as character no-undo.
  define variable cAbs   as character no-undo.
  define variable cToken as character no-undo.
  define variable cFile  as character  no-undo.

  /* master index */
  output to value(gpcOutputDir + "/00-index.html").

  /* Title of page */
  put unformatted 
    skip fcHtmlHeader('Progress Database Schema','')
    skip fcBodyTag()
    /* Add Navigation links */
    skip '<table style="width:100%;" summary="Navigation panel">'
    skip '  <tr class="navbar">'
    skip '    <td style="width:20%; text-align:left;">'
    skip '      <script type="text/javascript">'
    skip '        <!--'
    skip '          writeStyleChooser();'
    skip '        // -->'
    skip '      </script>'
    skip '    </td>'
    skip substitute('    <td style="width:80%; text-align:right;"><small><a href="htmldict.html" &1 >HtmlDict {&buildnr}</a></small></td>'
                   , if gplUseFrames then 'target="_top"' else ''
                   )
    skip '  </tr>'
    skip '</table>'
    skip ' '
    skip '<h1>Progress Database Schema</h1>'

    /* List all databases in a table */
    skip '<table summary="list of databases" style="text-align:center; margin-left:auto; margin-right:auto;">'
    skip '  <tr><th>Select dictionary:</th></tr>'
    .

  /* read dirs in master index' directory.*/
  input from os-dir(gpcOutputDir).
  repeat:
    import cBase cAbs cToken.
    if cToken begins 'D'
      and cBase ne '.'
      and cBase ne '..' then
    do:
      put unformatted 
        skip substitute('  <tr class="shadedrow"><td><a href="&1/00-index.html">&2</a></td></tr>'
                       , cBase
                       , caps(substring(cBase,1,1)) + lc(substring(cBase,2))
                       ).
    end.
  end.
  input close.


  put unformatted
    skip '</table>'
    skip ' '
    skip fcPageFooter()
    skip ' '
    skip '<p style="text-align:right">'
    skip '  <a href="http://validator.w3.org/"><img'
    skip '    src="vhtml401.png"'
    skip substitute('    alt="Valid HTML 4.01 &1" height="31" width="88"', if gplUseFrames then 'Transitional' else 'Strict')
    skip '    style="border:0px;"></a>'
    skip '  <a href="http://jigsaw.w3.org/css-validator/"><img'
    skip '    src="vcss.png"'
    skip '    alt="Valid CSS" height="31" width="88"'
    skip '    style="border:0px;"></a>'
    skip '</p>'
    skip ' '
    skip '</body>'
    skip '</html>'
    .

  output close.

end procedure. /* putMainPage */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-putTableFields) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE putTableFields Procedure 
PROCEDURE putTableFields :
/*----------------------------------------------------------------------------
  Name         : putTableFields
  Description  : Put the fields of a table, both in field order and in _order
                 order. 
  --------------------------------------------------------------------------- */
    
  define input parameter pcDatabase  as character no-undo. /* database */
  define input parameter pcTableName as character no-undo. /* table to process */

  define variable iCounter        as integer    no-undo.
  define variable iRowNr          as integer    no-undo.
  define variable cFieldNameLink  as character  no-undo.
  define variable cCrossIndexLink as character  no-undo.
  define variable cDecimals       as character  no-undo.
  define variable cExtent         as character  no-undo.
  define variable cRowStyle       as character  no-undo.
  define variable cViewAs         as character  no-undo.
  define variable cDesc           as character  no-undo.

  define buffer bCrossIndex for ttCrossIndex.
  define buffer bField      for ttField.

  define query qField for bField.

  /* Show fields of table header */
  put unformatted
    '~n<div class="clearboth"></div>' +
    '~n<hr style="margin-top:20px;">' +
    '~n<h2>Fields</h2>' +
    '~n'
    .

  /****************/
  do iCounter = 1 to 2:

    if iCounter = 1 then /* name */
    do:
      put unformatted
        substitute('~n<div id=SortName &1>',(if gpcFieldSort = '_Field-Name' then '' else ' style="display:none;"'))
        .
      open query qField for each bField where bField.cFileName = pcTableName no-lock by bField.cFieldName.
    end.
    else
    if iCounter = 2 then /* Order */
    do:
      put unformatted
        substitute('~n<div id=SortOrder &1>',(if gpcFieldSort = '_Order' then '' else ' style="display:none;"'))
        .
      open query qField for each bField where bField.cFileName = pcTableName no-lock by bField.iFieldOrder.
    end.

    put unformatted
      '~n<table style="width:100%; border-style:none;">' +
      '~n  <tr>'
      .

   if iCounter = 1 then
     put unformatted
       '~n    <th><script type="text/javascript">' +
       '~n    <!--' +
       '~n    writeToggleBegin();' +
       '~n    // -->' +
       '~n    </script>Order<script type="text/javascript">' +
       '~n    <!--' +
       '~n    writeToggleEnd();' +
       '~n    // -->' +
       '~n    </script></th>' +
       '~n    <th>Field Name</th>'
       .
   else
   if iCounter = 2 then
     put unformatted
       '~n    <th>Order</th>' +
       '~n    <th><script type="text/javascript">' +
       '~n    <!--' +
       '~n    writeToggleBegin();' +
       '~n    // -->' +
       '~n    </script>Field Name<script type="text/javascript">' +
       '~n    <!--' +
       '~n    writeToggleEnd();' +
       '~n    // -->' +
       '~n    </script></th>'
       .

    put unformatted
      '~n    <th>x</th>' +
      '~n    <th>Label</th>' +
      '~n    <th>Datatype</th>' +
      '~n    <th>Format</th>' +
      '~n    <th>Initial</th>' +
      '~n    <th>Mand</th>' +
      '~n    <th>Description</th>' +
      '~n  </tr>'
      .

    iRowNr = 1.
    get first qField.
    repeat while not query-off-end('qField'):

      /* Link to master table */
      if bField.cMasterFileUrl <> '' then 
        cFieldNameLink = substitute('<a href="&1">&2</a>', bField.cMasterFileUrl, bField.cFieldName).
      else
        cFieldNameLink = bField.cFieldName.

      /* Decimals */
      cDecimals = (if bField.cFieldDataType = 'decimal' then substitute('-&1',bField.iFieldDecimals) else '' ).

      /* Extent */
      cExtent = (if bField.iFieldExtent <> 0 then substitute(' [&1]', bField.iFieldExtent) else '').

      /* Determine style for the new row */
      case iRowNr modulo 2:
        when 0 then cRowStyle = 'class="evenrow"'.
        when 1 then cRowStyle = 'class="oddrow"'.
      end case.

      /* print html line */
      put unformatted substitute(
        '~n  <tr &1>' +
        '~n    <td>&2</td>' +
        '~n    <td>&3</td>' +
        '~n    <td>&4</td>' +
        '~n    <td>&5</td>' +
        '~n    <td>&6</td>' +
        '~n    <td>&7</td>' +
        '~n    <td>&8</td>' +
        '~n    <td style="text-align: center;"> <input type="checkbox" disabled &9 /> </td>'
        , cRowStyle
        , string(bField.iFieldOrder)
        , cFieldNameLink
        , bField.cCrossIndexUrl
        , fcHtmlSafe( bField.cFieldLabel )
        , bField.cFieldDataType + cDecimals + cExtent
        , fcHtmlSafe( bField.cFieldFormat )
        , fcHtmlSafe( bField.cFieldInitial )
        , string(bField.lFieldMandatory, "checked/")
        ).

      assign
        cViewAs = ''
        cDesc   = ''.

      if bField.cFieldViewAs <> ? and bField.cFieldViewAs <> '' then cViewAs = 'V: ':U + fcHtmlSafe( bField.cFieldViewAs ).
      if bField.cFieldDesc   <> ? and bField.cFieldDesc   <> '' then cDesc   = 'D: ':U + fcHtmlSafe( bField.cFieldDesc   ).

      if cViewAs <> '' and cDesc <> '' then cViewAs = cViewAs + '<br>'.
      cViewAs = replace(cViewAs, 'V: VIEW-AS',           'V:').
      cViewAs = replace(cViewAs, 'COMBO-BOX',            '<i>cb</i>').
      cViewAs = replace(cViewAs, 'EDITOR',               '<i>e</i>').
      cViewAs = replace(cViewAs, 'FILL-IN',              '<i>fi</i>').
      cViewAs = replace(cViewAs, 'INNER-LINES',          '<i>il</i>').
      cViewAs = replace(cViewAs, 'LIST-ITEM-PAIRS',      '<i>lip</i>').
      cViewAs = replace(cViewAs, 'LIST-ITEMS',           '<i>li</i>').
      cViewAs = replace(cViewAs, 'RADIO-BUTTONS',        '<i>rb</i>').
      cViewAs = replace(cViewAs, 'RADIO-SET',            '<i>rs</i>').
      cViewAs = replace(cViewAs, 'SCROLLBAR-HORIZONTAL', '<i>sh</i>').
      cViewAs = replace(cViewAs, 'HORIZONTAL',           '<i>h</i>').
      cViewAs = replace(cViewAs, 'SCROLLBAR-VERTICAL',   '<i>sv</i>').
      cViewAs = replace(cViewAs, 'VERTICAL',             '<i>v</i>').
      cViewAs = replace(cViewAs, 'SELECTION-LIST',       '<i>sl</i>').
      cViewAs = replace(cViewAs, 'SIZE-CHARS',           '<i>sc</i>').
      cViewAs = replace(cViewAs, 'SIZE',                 '<i>sz</i>').
      cViewAs = replace(cViewAs, 'SLIDER',               '<i>s</i>').
      cViewAs = replace(cViewAs, 'TEXT',                 '<i>tx</i>').
      cViewAs = replace(cViewAs, 'TOGGLE-BOX',           '<i>tb</i>').
      cViewAs = replace(cViewAs, 'TOOLTIP',              '<i>tt</i>').

      put unformatted substitute(
        '~n    <td>&1&2</td>' +
        '~n  </tr>'
        ,cViewAs, cDesc).


      iRowNr = iRowNr + 1.
      get next qField.
    end. /* f/e field */

    put unformatted
      '~n</table>'
      '~n</div>'
      .
  end. /* do iCounter = 1 to 2 */
  /******************/

  put unformatted
    '~n' +
    '~n<hr>' +
    '~n'
    .

end procedure. /* putTableFields */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-putTableIndex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE putTableIndex Procedure 
PROCEDURE putTableIndex :
/*----------------------------------------------------------------------------
  Name         : putTableIndex
  Description  : Write html for the indexes of a table. 
  --------------------------------------------------------------------------- */
    
  define input parameter pcDatabase  as character no-undo. /* database */
  define input parameter pcTableName as character no-undo. /* table to process */

  define variable iRowNr          as integer    no-undo.
  define variable cRowStyle       as character  no-undo.

  define buffer bFile       for ttFile.
  define buffer bIndex      for ttIndex.
  define buffer bIndexField for ttIndexField.
  define buffer bField      for ttField.

  /* Show index info index */
  put unformatted
    '~n<h2>Indexes</h2>'
    '~n<table style="width:100%;">'
    '~n  <tr>'
    '~n    <th>Index name</th>'
    '~n    <th>Flags</th>'
    '~n    <th>Fields</th>'
    '~n    <th>Datatype</th>'
    '~n  </tr>'
    .

  iRowNr = 1.
  /* Find the file to be processed */
  for each bIndex
    where bIndex.cFileName = pcTableName 
          no-lock
    break by bIndex.lIndexPrimeIndex descending
          by bIndex.cIndexName:

    /* Determine style for the new row */
    case iRowNr modulo 2:
      when 0 then cRowStyle = 'class="evenrow"'.
      when 1 then cRowStyle = 'class="oddrow"'.
    end case.

    put unformatted
      skip substitute('  <tr &1>', cRowStyle)
      skip '    <td>' bIndex.cIndexName '</td>'
      skip '    <td>' string(bIndex.lIndexPrimeIndex,'P /')
                      string(bIndex.lIndexUnique   ,'U /')
                      string(bIndex.lIndexActive   ,'/I ')
                      string(bIndex.lIndexWordidx  ,'W /') '</td>'.

    for each bIndexField
      where bIndexField.cFileName  = bIndex.cFileName
        and bIndexField.cIndexName = bIndex.cIndexName
            no-lock
      break by bIndexField.cFileName
            by bIndexField.cIndexName
            by bIndexField.iIndexFieldSeq:

      if not first-of(bIndexField.cIndexName) then
        put unformatted
          skip substitute('  <tr &1>', cRowStyle)
          skip '    <td>&nbsp;</td>'
          skip '    <td>&nbsp;</td>'
        .

      find bField 
        where bField.cFileName  = bIndexField.cFileName
          and bField.cFieldName = bIndexField.cFieldName
              no-error.

      if available bField then
        put unformatted
          skip '    <td>' bField.cFieldName + string(bIndexField.lIndexFieldAscending,' (+)/ (-)') '</td>'
          skip '    <td>' bField.cFieldDataType '</td>'
          skip '  </tr>'.
      else 
        put unformatted
          skip '    <td> &nbsp; </td>'
          skip '    <td> &nbsp; </td>'
          skip '  </tr>'.
    end. /* for each bIndexField */

    iRowNr = iRowNr + 1.
  end. /*  for each bIndex */

  put unformatted
    skip '</table>'
    skip '<table style="width:100%; margin-top:10px;">'
    skip '  <tr>'
    skip '    <td>+ = Ascending</td>'
    skip '    <td>- = Descending</td>'
    skip '    <td>P = Primary</td>'
    skip '    <td>U = Unique</td>'
    skip '    <td>I = Inactive</td>'
    skip '    <td>W = Wordindex</td>'
    skip '  </tr>'
    skip '</table> '
    .

end procedure. /* putTableIndex */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-putTableListPage) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE putTableListPage Procedure 
PROCEDURE putTableListPage :
/*----------------------------------------------------------------------------
  Name         : putTableListPage
  Description  : Create a page containing the list of all tables. 
  --------------------------------------------------------------------------- */
    
  define input  parameter pcDatabase  as character  no-undo.
  define input  parameter pcOutputDir as character  no-undo.

  define variable cMasterFile       as character  no-undo.
  define variable cTarget           as character  no-undo.
  define variable iRowNr            as integer    no-undo.
  define variable iColNr            as integer    no-undo.
  define variable cRowStyle         as character  no-undo.
  define variable cFirstCharLink    as character  no-undo.

  define buffer bFile for ttFile.

  /* ********************** frameset ************************************* */

  if gplUseFrames then 
  do:
    output to value(pcOutputDir + "/00-index.html":U).
  
    put unformatted
      fcHtmlHeader( 'Schema database ' + pcDatabase, '../' )
      /* no BODY tag on a frameset */
      skip '<frameset cols="14%,*">                 '
      skip '  <frame name="list" src="00-list.html">'
      skip '  <frame name="main" src="00-main.html">'
      skip '</frameset>                             '
      skip '</html>                                 '
      .
    output close.
  end.

  /* ********************** table list *********************************** */

  /* Table list for framed version */
  output to value(pcOutputDir + "/00-list.html":U).

  put unformatted
    fcHtmlHeader( pcDatabase, '../' )
    fcBodyTag()
    skip '<script type="text/javascript">'
    skip '  <!--'
    skip '    writeDbChooser();'
    skip '  // -->'
    skip '</script>'    
    skip '<h1>' pcDatabase '</h1>'
    skip '<table width="100%">'
    .

  for each bFile break by bFile.cFileFirstChar by bFile.cFileName:
    
    if first-of(bFile.cFileFirstChar) then
      cFirstCharLink = substitute('<a name="&1" />', bFile.cFileFirstChar).
    else
      cFirstCharLink = ''.
    
    put unformatted 
      skip substitute('  <tr><td>&1<a href="&2" target="main">&3</a></td></tr>'
                     , cFirstCharLink
                     , bFile.cFileSelfUrl
                     , bFile.cFileName
                     ).
  end.

  put unformatted
    skip '</table>'
    skip '</body>'
    skip '</html>'
    .
  output close. 


  /* ********************** main page ************************************ */
  
  /* Define master file */
  if gplUseFrames 
    then output to value(pcOutputDir + '/00-main.html').
    else output to value(pcOutputDir + '/00-index.html').
  
  /* Write title of web page */
  put unformatted
    fcHtmlHeader( 'Schema database ' + pcDatabase, '../' )
    skip fcBodyTag()
    skip fcPageHeader( fcNavigationLinks('db,!tables,areas,x-idx,sequences',''), '', '')
    skip '<h1>Schema database ' pcDatabase '</h1>'
    .

  /* Make a line with starting chars */
  put unformatted
    skip '<table><tr><td>|</td>'.

  for each bFile break by bFile.cFileFirstChar:
    if first-of(bFile.cFileFirstChar) then
    do:
      if gplUseFrames then
        put unformatted
          skip '  <td><a href="#' bFile.cFileFirstChar '" onClick="parent.list.location=''00-list.html#' bFile.cFileFirstChar '''">'
               '&nbsp;' + lower(bFile.cFileFirstChar) + '&nbsp;</a>&nbsp;|</td>'.

      else
        put unformatted
          skip '  <td><a href="#' bFile.cFileFirstChar '">'
               '&nbsp;' + lower(bFile.cFileFirstChar) + '&nbsp;</a>&nbsp;|</td>'.
    end.
  end.

  put unformatted '</tr></table>' skip.


  /*
   * Write list of tables in order
   */
  put unformatted '<table style="width:100%;">' skip.
  assign
    iRowNr = 1
    iColNr = 1.

  /* start line */
  for each bFile 
    break by bFile.cFileFirstChar
          by bFile.cFileName:

    /* Start of a new row? */
    if first-of(bFile.cFileFirstChar) or iColNr = 1 then
    do:
      /* Put visual mark for char start */
      if first-of(bFile.cFileFirstChar) then
      do:
        put unformatted
          skip '<tr><td colspan="5" >&nbsp;</td></tr>'
          skip '<tr><th colspan="5" style="text-align:center;">'
               '<a name="' bFile.cFileFirstChar '"></a>'
          .
        if gplUseFrames then
          put unformatted
            '<a class="thlink" href="#top" onClick="parent.list.location=''00-list.html#top''"> - ' bFile.cFileFirstChar ' - </a></th></tr>' .
        else
          put unformatted
            '<a class="thlink" href="#top"> - ' bFile.cFileFirstChar ' -</a></th></tr>'.

        iRowNr = 1.
      end.

      /* Determine style for the new row */
      case iRowNr modulo 2:
        when 0 then cRowStyle = 'class="evenrow"'.
        when 1 then cRowStyle = 'class="oddrow"'.
      end case.

      /* Start table row */
      put unformatted skip substitute('  <tr &1>', cRowStyle).
    end. /* first-of */


    /* Write table name in current cell */
    put unformatted
      skip '    <td> <a href="' bFile.cFileSelfUrl '">' bFile.cFileName '</a></td>'
      skip '    <td> ' bFile.cFileDesc '</td>'.


    /* Put visual mark for char start */
    if last-of(bFile.cFileFirstChar) then
    do:
      /* Complete the row */
      if iColNr = 1 then
      do:
        assign iColNr = iColNr + 1.
        put unformatted
          skip '    <td>&nbsp;</td>'
          skip '    <td>&nbsp;</td>'
          .
      end. /* col = 1 */
    end. /* last of char */

    assign
      iColNr = iColNr + 1.

    /* End of the row? */
    if iColNr = 3 then
    do:
      assign
        iRowNr = iRowNr + 1
        iColNr = 1.

      /* terminate table row */
      put unformatted skip '</tr>'.
    end. /* col=3 */
  end. /* f/e bFile */

  
  /* Finish page */
  put unformatted
    skip "</table>"
    skip fcPageFooter()
    . 

  output close.

end procedure. /* putTableListPage */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-putTableTriggers) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE putTableTriggers Procedure 
PROCEDURE putTableTriggers :
/*----------------------------------------------------------------------------
  Name         : putTableTriggers
  Description  : Create html for the triggers of a table. 
  --------------------------------------------------------------------------- */
    
  define input parameter pcDatabase  as character no-undo. /* database */
  define input parameter pcTableName as character no-undo. /* table to process */

  define variable cTrgFullPath     as character  no-undo.
  define variable iTrgPathCount    as integer    no-undo.
  define variable cPath            as character  no-undo.
  define variable cTrgTbField      as character  no-undo.
  define variable cTriggerFileName as character  no-undo.
  define variable cRowStyle        as character  no-undo.

  define buffer bFileTrig   for ttFileTrig.

  if can-find( first bFileTrig
               where bFileTrig.cFileName = pcTableName ) then
  do:
    /* Show fields of triggers header */
    put unformatted
      '~n  <div class="tbcol" >' +
      '~n    <h2>Triggers</h2>' +
      '~n    <table style="border-style:none;">' +
      '~n      <tr>' +
      '~n        <th>Event</th>' +
      '~n        <th>Procedure</th>' +
      '~n        <th>Overr</th>' +
      '~n        <th>CRC</th>' +
      '~n      </tr>'
      .

    for each bFileTrig no-lock
      where bFileTrig.cFileName = pcTableName:

      /* Try to find trigger-code in supplied trigger-path
       * (first try current propath)
       */
      cTrgFullPath = search(bFileTrig.cFileTrigProcName).
      if cTrgFullPath = ? then
      do iTrgPathCount = 1 to num-entries(gpcTriggerPath,";":u):
         cPath = trim(entry(iTrgPathCount,gpcTriggerPath,";":u),"/":u).
         cTrgFullPath = search(substitute("&1/&2",cPath,bFileTrig.cFileTrigProcName)).
         if cTrgFullPath <> ? then leave.
      end.

      if cTrgFullPath <> ? then
      do:
        run processTriggerCode (  input pcDatabase
                               ,  input cTrgFullPath
                               ,  input lc(bFileTrig.cFileTrigEvent)
                               ,  input lc(pcTableName)
                               , output cTriggerFileName
                               ).
        cTrgTbField = substitute('<a href="&1">&2</a>'
                                , cTriggerFileName
                                , bFileTrig.cFileTrigProcName
                                ).
      end.
      else
        cTrgTbField = bFileTrig.cFileTrigProcName.

      /* print html line */
      put unformatted substitute(
        '~n      <tr class="shadedrow">' +
        '~n        <td>&2</td>' +
        '~n        <td>&3</td>' +
        '~n        <td style="text-align: center;"> <input type="checkbox" disabled &4 /> </td>' +
        '~n        <td style="text-align: center;"> <input type="checkbox" disabled &5 /> </td>' +
        '~n      </tr>'
        , cRowStyle
        , bFileTrig.cFileTrigEvent
        , cTrgTbField
        , string(bFileTrig.lFileTrigOverride, "checked/")
        , string(bFileTrig.iFileTrigTrigCrc <> ?, "checked/")
        ).

    end.

    put unformatted
      '~n    </table>' +
      '~n  </div>'
      .
  end.

  if not gplUseFrames then
    put unformatted '~n</div>'.

end procedure. /* putTableTriggers */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fcBodyTag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fcBodyTag Procedure 
FUNCTION fcBodyTag returns character
  ( /* parameter-definitions */ ) :

/*----------------------------------------------------------------------------
  Name         : fcBodyTag
  Description  : Return the open tag for <BODY>
  --------------------------------------------------------------------------- */

  return '~n<body onload="useStyleAgain(''HtmlDict'');" onunload="rememberStyle(''HtmlDict'',10);">'
       + '~n<div><a name="top"></a></div>'.

end function. /* fcBodyTag */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fcHtmlHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fcHtmlHeader Procedure 
FUNCTION fcHtmlHeader returns character
  ( pcTitle     as character
  , pcRootPath  as character 
    ):
  /*----------------------------------------------------------------------------
    Name         : fcHtmlHeader
    Description  : Give back a formatted list with the start of a html page
    --------------------------------------------------------------------------- */

  define variable cReturnString as character  no-undo.

  if gplUseFrames then
    cReturnString = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">'.
  else
    cReturnString = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">'.

  cReturnString = cReturnString
    + '~n<html>'
    + '~n<head>'
    + '~n  <meta name="generator" content="HtmlDict.w version {&buildnr}">'
    + '~n  <meta name="author"    content="Patrick Tingen [patrick at tingen dot nl]">'
    + '~n  <link rel="stylesheet" title="Default" type="text/css" href="' + pcRootPath + 'default.css">'
    .

  for each ttStyleSheet:
    cReturnString = cReturnString 
      + substitute('~n  <link rel="alternate stylesheet" title="&1" type="text/css" href="' + pcRootPath + '&2">'
                  , ttStyleSheet.cName
                  , ttStyleSheet.cFile
                  ).
  end.

  cReturnString = cReturnString
    + '~n  <link rel="shortcut icon" href="' + pcRootPath + 'htmldict.ico">'
    + '~n  <link rel="stylesheet" media="print" type="text/css" href="' + pcRootPath + 'print.css">'
    + '~n  <script src="' + pcRootPath + 'swapstyle.js"    type="text/javascript"></script>'
    + '~n  <script src="' + pcRootPath + 'stylechooser.js" type="text/javascript"></script>'
    + '~n  <script src="' + pcRootPath + 'dbchooser.js"    type="text/javascript"></script>'
    + '~n  <script src="' + pcRootPath + 'htmldict.js"     type="text/javascript"></script>'
    + '~n  <title> ' + pcTitle + '</title>'
    + '~n</head>'
    .

  return cReturnString.
end function. /* fcHtmlHeader */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fcHtmlSafe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fcHtmlSafe Procedure 
FUNCTION fcHtmlSafe returns character
  ( pcString as character ):
  /*----------------------------------------------------------------------------
    Name         : fcHtmlSafe
    Description  : Give back a string from which special tokens have been 
                   replaced with html-safe tokens. 
    --------------------------------------------------------------------------- */
  define variable cHtmlSafe as character no-undo. 

  assign 
    cHtmlSafe = pcString
    cHtmlSafe = replace(cHtmlSafe,'<','&lt;')
    cHtmlSafe = replace(cHtmlSafe,'>','&gt;')
    .
  if cHtmlSafe = ? then cHtmlSafe = '?'.

  return cHtmlSafe.

end function. /* fcHtmlSafe */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fcNavigationLinks) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fcNavigationLinks Procedure 
FUNCTION fcNavigationLinks returns character
  ( cLinkList   as character
  , pcRelPath   as character ):

  /*----------------------------------------------------------------------------
    Name         : fcNavigationLinks
    Description  : Give back a formatted list that can be outputted as a list
                   of links to other pages. 
    --------------------------------------------------------------------------- */

  define variable cHeader     as character  no-undo.
  define variable cTargetMain as character  no-undo.
  define variable cTargetTop  as character  no-undo.
  define variable cTablePage  as character no-undo.
  
  if gplUseFrames then
    assign
      cTargetMain = 'target="main"'
      cTargetTop  = 'target="_top"'
      cTablePage  = '00-main.html'
      .
  else
    assign
      cTargetMain = ''
      cTargetTop  = ''
      cTablePage  ='00-index.html'
      .

  if lookup('Databases',cLinkList) > 0 then
    cHeader = cHeader + ' | <a href="' + pcRelPath + '00-index.html"' + cTargetTop + '>Databases</a>~n'.
  if lookup('DB',cLinkList) > 0 then
    cHeader = cHeader + ' <a href="' + pcRelPath + '../00-index.html"' + cTargetTop + '>DB</a>~n'.

  if lookup('Areas',cLinkList) > 0 then
    cHeader = cHeader + ' | <a href="' + pcRelPath + '00-area.html"' + cTargetMain + '>Areas</a>~n'.
  if lookup('!Areas',cLinkList) > 0 then
    cHeader = cHeader + ' | Areas ~n'.
  if lookup('A',cLinkList) > 0 then
    cHeader = cHeader + ' <a href="' + pcRelPath + '00-area.html"' + cTargetMain + '>A</a>~n'.

  if lookup('X-Idx',cLinkList) > 0 then
    cHeader = cHeader + ' | <a href="' + pcRelPath + '00-cross.html"' + cTargetMain + '>X-idx</a>~n'.
  if lookup('!X-Idx',cLinkList) > 0 then
    cHeader = cHeader + ' | X-idx ~n'.
  if lookup('X',cLinkList) > 0 then
    cHeader = cHeader + ' <a href="' + pcRelPath + '00-cross.html"' + cTargetMain + '>X</a>~n'.

  if lookup('Tables',cLinkList) > 0 then
    cHeader = cHeader + ' | <a href="' + pcRelPath + cTablePage + '"' + cTargetMain + '>Tables</a>~n'.
  if lookup('!Tables',cLinkList) > 0 then
    cHeader = cHeader + ' | Tables ~n'.
  if lookup('Tb',cLinkList) > 0 then
    cHeader = cHeader + ' <a href="' + pcRelPath + cTablePage + '"' + cTargetMain + '>Tb</a>~n'.

  if lookup('Sequences',cLinkList) > 0 then
    cHeader = cHeader + ' | <a href="' + pcRelPath + '00-sequences.html"' + cTargetMain + '>Sequences</a>~n'.
  if lookup('!Sequences',cLinkList) > 0 then
    cHeader = cHeader + ' | Sequences ~n'.
  if lookup('Sq',cLinkList) > 0 then
    cHeader = cHeader + ' <a href="' + pcRelPath + '00-sequences.html"' + cTargetMain + '>Sq</a>~n'.

  assign 
    cHeader = trim(cHeader,'| '). 

  return cHeader. 

end function. /* fcNavigationLinks */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fcPageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fcPageFooter Procedure 
FUNCTION fcPageFooter returns character
  ( /* parameter-definitions */ ) :

  /*----------------------------------------------------------------------------
    Name         : fcPageFooter
    Description  : Give back html code for the foot of the page (timestamp).
    --------------------------------------------------------------------------- */

  define variable cTimeStamp as character  no-undo.

  cTimeStamp = substitute('<i><small>generated &1-&2-&3 &4</small></i>'
                         , day(today)
                         , month(today)
                         , year(today)
                         , string(time, 'hh:mm')
                         ).
  return
    '<hr>' + 
    '~n<table style="width:100%; margin-top:10px;">' + 
    '~n  <tr><td style="text-align:right;">' + cTimeStamp + '</td></tr>' + 
    '~n</table>'.

end function. /* fcPageFooter */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fcPageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fcPageHeader Procedure 
FUNCTION fcPageHeader returns character
  ( cNavigation as character
  , cPrevious   as character
  , cNext       as character
  ) :

  /*----------------------------------------------------------------------------
    Name         : fcPageHeader
    Description  : Give back html code for the start of the page (navigation bar).
    --------------------------------------------------------------------------- */

  define variable cVersion     as character  no-undo.
  define variable cReturnValue as character  no-undo.

  cVersion = fcVersion().

  if cPrevious = ''
    and cNext = '' then 
    cReturnValue =  substitute( '~n<table style="width:100%;">'
                              + '~n  <tr>'
                              + '~n    <td class="navbar" style="width:80%;">&1</td>'
                              + '~n    <td class="navbar" style="width:20%; text-align:right;">&2</td>' 
                              + '~n  </tr>' 
                              + '~n</table>' 
                              , cNavigation
                              , cVersion
                              ).
  else 
    cReturnValue =  substitute( '~n<table style="width:100%;">'
                              + '~n  <tr>'
                              + '~n    <td class="navbar" style="width:40%;">&1</td>'
                              + '~n    <td class="navbar" style="width:20%;">&2</td>' 
                              + '~n    <td class="navbar" style="width:20%;">&3</td>' 
                              + '~n    <td class="navbar" style="width:20%; text-align:right;">&4</td>' 
                              + '~n  </tr>' 
                              + '~n</table>' 
                              , cNavigation
                              , cPrevious
                              , cNext
                              , cVersion
                              ).

  return cReturnValue.

end function. /* fcPageHeader */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fcVersion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fcVersion Procedure 
FUNCTION fcVersion returns character
  ( /* parameter-definitions */ ) :

  /*----------------------------------------------------------------------------
    Name         : fcVersion
    Description  : Give back html code for the version number.
    --------------------------------------------------------------------------- */

  return '<a href="../htmldict.html"' + gcTargetTop + '>HtmlDict {&buildnr}</a>'.

end function. /* fcVersion */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


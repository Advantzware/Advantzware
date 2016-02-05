&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------

File    : myDump.p
Purpose : Customized dump data from DataDigger (template)

Notes:  DataDigger will call this file with the following parameters: 
        
          input pcAction   (character) - type of dump (Delete | Save)
          input pcDatabase (character) - name of the database
          input pcTable    (character) - name of the table
          input phData     (handle   ) - handle to a dynamic tt with the data to dump
          input pcFilename (character) - filename as suggested by DataDigger
          
        The phData parameter is a handle to a temp-table that holds only the 
        data you would like to dump. So it contains 1 record in case of a save 
        action and 1 or more in case of a delete action. 
        
        You can give these parameters back to DataDigger:
        
          output pcMessage   (character) - a message to show the user
          output plDefaultDump  (logical  ) - perform regular DD dump or not
          output plContinue  (logical  ) - perform regular DD dump or not
         
        pcMessage: 
          This can be an informative message, a warning or an error. This
          will be shown to the user by DataDigger. 
          
        plDefaultDump
          This tells DD whether to perform its own dump or not. 
          
        plContinue
          This tells DD whether the action should be committed. You 
          can use this to prevent certain delete or save actions. 


        Note that if you do your own dump, using the filename DD suggests in pcFilename
        and you set plDefaultDump to TRUE, DD will overwrite your file because it too
        uses this filename to dump its data. 
        
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

define input  parameter pcAction      as character   no-undo.
define input  parameter pcDatabase    as character   no-undo.
define input  parameter pcTable       as character   no-undo.
define input  parameter phData        as handle      no-undo.
define input  parameter pcFilename    as character   no-undo.
                                     
define output parameter pcMessage     as character   no-undo.
define output parameter plDefaultDump as logical     no-undo.
define output parameter plContinue    as logical     no-undo.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Ask if user is really sure */                                 
if pcAction = 'Delete' then 
do:
  message 'Are you really really really sure you want to delete these precious records?'
    view-as alert-box info buttons yes-no update plContinue.
end.

else 
do:
  /* This will export your data just like DataDigger does */
  
  phData:write-xml
    ( 'file'        /* TargetType     */
    , pcFileName    /* File           */
    , yes           /* Formatted      */
    , ?             /* Encoding       */
    , ?             /* SchemaLocation */
    , no            /* WriteSchema    */
    , no            /* MinSchema      */
    ).

end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
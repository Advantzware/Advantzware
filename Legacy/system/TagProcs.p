
/*------------------------------------------------------------------------
    File        : TagProcs.p
    Purpose     : Houses library of procedures and functions for manipulation and querying of tags

    Syntax      :

    Description : Procedures and Functions for Tags

    Author(s)   : BV
    Created     : Sat Sep 22 19:17:37 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE gTypeInactive AS CHARACTER NO-UNDO INIT "I".

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION IsActive RETURNS LOGICAL 
	( ipcRecKey AS CHARACTER ) FORWARD.


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE AddTag:
/*------------------------------------------------------------------------------
 Purpose:  Adds a tag to a given reckey based on provided parameters
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLinkTable AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcTagType AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcNote LIKE tag.Note NO-UNDO.

CREATE tag.
ASSIGN 
    tag.linkRecKey = ipcLinkRecKey
    tag.linkTable = ipcLinkTable
    tag.tagType = ipcTagType
    tag.Note = ipcNote
    tag.createUser = USERID(LDBNAME(1))
    tag.createDT = NOW 
    .
    
END PROCEDURE.

PROCEDURE AddTagInactive:
/*------------------------------------------------------------------------------
 Purpose: Wrapper to Add tag, specifically for Inactive
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLinkTable AS CHARACTER NO-UNDO.

DEFINE VARIABLE cNote LIKE tag.note.

cNote[1] = "Inactive " + ipcLinkTable.
RUN AddTag(ipcLinkRecKey, ipcLinkTable, gTypeInactive , cNote).

END PROCEDURE.

PROCEDURE ClearTagsInactive:
/*------------------------------------------------------------------------------
 Purpose: Wrapper for ClearTagsOfType specifically for inactive
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

RUN ClearTagsOfType(ipcLInkRecKey,gTypeInactive).


END PROCEDURE.

PROCEDURE ClearTagsOfType:
/*------------------------------------------------------------------------------
 Purpose: Clears all tags of a particular type
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcTagType AS CHARACTER NO-UNDO.

FOR EACH tag EXCLUSIVE-LOCK 
    WHERE tag.linkRecKey EQ ipcLinkRecKey
    AND tag.tagType EQ ipcTagType:
    DELETE tag.
END.

END PROCEDURE.

/* ************************  Function Implementations ***************** */


FUNCTION IsActive RETURNS LOGICAL 
	( ipcRecKey AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:  Given a recKey - returns True if there are no inactive tags linked to it
 Notes:
------------------------------------------------------------------------------*/	
FIND FIRST tag NO-LOCK
    WHERE tag.linkRecKey EQ ipcRecKey
    AND tag.tagType EQ gTypeInactive /*Inactive Type*/
    NO-ERROR.

RETURN NOT AVAILABLE tag.
		
END FUNCTION.


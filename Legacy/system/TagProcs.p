
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
    DEFINE VARIABLE gTypeHold AS CHARACTER NO-UNDO INIT "H".

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION IsActive RETURNS LOGICAL 
	( ipcLinkRecKey AS CHARACTER ) FORWARD.


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE AddTag:
/*------------------------------------------------------------------------------
 Purpose:  Adds a tag to a given reckey based on provided parameters
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTagGroup AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTagType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTagName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTagValue AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipclinkField AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkValue AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcNote AS CHARACTER NO-UNDO.

    FIND FIRST tag EXCLUSIVE WHERE  
        tag.linkReckey  EQ ipcLinkRecKey AND 
        tag.linkTable   EQ ipcLinkTable AND 
        tag.tagGroup    EQ ipcTagGroup AND 
        tag.tagType     EQ ipcTagType AND 
        tag.tagName     EQ ipcTagName
        NO-ERROR.
    IF NOT AVAIL tag THEN DO: 
        CREATE tag.
        ASSIGN 
            tag.linkReckey  = ipcLinkRecKey
            tag.linkTable   = ipcLinkTable
            tag.tagGroup    = ipcTagGroup
            tag.tagType     = ipcTagType
            tag.tagName     = ipcTagName.
        /* Allow create trigger to populate other fields */
    END.
    ELSE ASSIGN 
        tag.description = ipcDescription
        tag.tagValue    = ipcTagValue
        tag.linkField   = ipcLinkField
        tag.linkValue   = ipcLinkValue
        tag.note        = ipcNote.
        /* Allow write trigger to populate other fields */
    
END PROCEDURE.

PROCEDURE AddTagInactive:
/*------------------------------------------------------------------------------
 Purpose: Wrapper to Add tag, specifically for Inactive
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkTable AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cNote LIKE tag.note.

    ASSIGN 
        cNote[1] = "Inactive " + ipcLinkTable.

    FIND FIRST tag NO-LOCK WHERE 
        tag.rec_key EQ ipcLinkRecKey and
        tag.linkTable EQ ipcLinkTable AND 
        tag.tagType = "Inactive" AND 
        tag.tagName = "Inactive" AND 
        tag.tagValue = "Y"
        NO-ERROR.
    IF NOT AVAIL tag THEN RUN AddTag (ipcLinkRecKey,
                                      ipcLinkTable,
                                      "",
                                      "Inactive",
                                      "Inactive",
                                      "Record is INACTIVE",
                                      "",
                                      "Y",
                                      "",
                                      "",
                                      cNote).
END PROCEDURE.

PROCEDURE ClearTagsByRecKey:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    FOR EACH tag EXCLUSIVE-LOCK WHERE 
        tag.linkRecKey EQ ipcLinkRecKey:
        DELETE tag.
    END.

END PROCEDURE.

PROCEDURE ClearTagsHold:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    RUN ClearTagsOfType(ipcLInkRecKey,gTypeHold).


END PROCEDURE.

PROCEDURE ClearTagsInactive:
/*------------------------------------------------------------------------------
 Purpose: Wrapper for ClearTagsOfType specifically for inactive
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    RUN ClearTagsOfType(ipcLInkRecKey,gTypeInactive).

END PROCEDURE.


PROCEDURE ClearTagsByName:
/*------------------------------------------------------------------------------
 Purpose: Clears all tags of a particular type
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTagName AS CHARACTER NO-UNDO.

    FOR EACH tag EXCLUSIVE-LOCK WHERE 
        tag.linkRecKey EQ ipcLinkRecKey AND 
        tag.tagName EQ ipcTagName:
        DELETE tag.
    END.

END PROCEDURE.


PROCEDURE ClearTagsOfType:
/*------------------------------------------------------------------------------
 Purpose:   Clears all tags of a particular type
 Notes:     Possible values = INFO,HOLD,INACTIVE,DELETE
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTagType AS CHARACTER NO-UNDO.

    FOR EACH tag EXCLUSIVE-LOCK WHERE 
        tag.linkRecKey EQ ipcLinkRecKey AND 
        tag.tagType EQ ipcTagType:
        DELETE tag.
    END.

END PROCEDURE.

/* ************************  Function Implementations ***************** */


FUNCTION IsActive RETURNS LOGICAL 
	( ipcLinkRecKey AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:  Given a recKey - returns True if there are no inactive tags linked to it
 Notes:
------------------------------------------------------------------------------*/	
    FIND FIRST tag NO-LOCK WHERE 
        tag.linkRecKey EQ ipcLinkRecKey AND 
        tag.tagType EQ gTypeInactive /*Inactive Type*/
        NO-ERROR.

    RETURN NOT AVAILABLE tag.
		
END FUNCTION.


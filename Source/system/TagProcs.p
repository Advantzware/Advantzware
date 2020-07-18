
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
DEFINE VARIABLE gcTypeInactive AS CHARACTER NO-UNDO INITIAL "INACTIVE".
DEFINE VARIABLE gcTypeHold AS CHARACTER NO-UNDO INITIAL "HOLD".
DEFINE VARIABLE gcTypeInfo AS CHARACTER NO-UNDO INITIAL "INFO".
DEFINE VARIABLE gcTypeRelease AS CHARACTER NO-UNDO INITIAL "RELEASE".

DEFINE VARIABLE iCtr AS INTEGER NO-UNDO.
DEFINE VARIABLE cNote LIKE tag.note.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION IsActive RETURNS LOGICAL 
	( ipcLinkRecKey AS CHARACTER ) FORWARD.

FUNCTION IsOnHold RETURNS LOGICAL 
    ( ipcLinkRecKey AS CHARACTER ) FORWARD.

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE AddTagHold:
/*------------------------------------------------------------------------------
 Purpose: Wrapper to Add tag, specifically for HOLD
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.

    FIND FIRST tag NO-LOCK 
        WHERE tag.linkRecKey  EQ ipcLinkRecKey
        AND tag.tagType     EQ gcTypeHold 
        AND tag.linkTable   EQ ipcLinkTable
        AND tag.description EQ ipcDescription
        NO-ERROR.
    IF NOT AVAILABLE  tag THEN 
        RUN pAddTag (ipcLinkRecKey, gcTypeHold, ipcLinkTable, ipcDescription).
        
END PROCEDURE.

PROCEDURE AddTagHoldInfo:
/*------------------------------------------------------------------------------
 Purpose: Wrapper to Add tag, specifically for HOLDINFO
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.

    FIND FIRST tag NO-LOCK 
        WHERE tag.linkRecKey  EQ ipcLinkRecKey
        AND tag.tagType     EQ gcTypeInfo 
        AND tag.linkTable   EQ ipcLinkTable
        AND tag.description EQ ipcDescription
        NO-ERROR.
    IF NOT AVAILABLE  tag THEN 
        RUN pAddTag (ipcLinkRecKey, gcTypeInfo, ipcLinkTable, ipcDescription).
        
END PROCEDURE.

PROCEDURE AddTagInactive:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper to Add tag, specifically for Inactive
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkTable AS CHARACTER NO-UNDO.

    ASSIGN 
        cNote = ""
        cNote[1] = "Inactive " + ipcLinkTable.

    FIND FIRST tag NO-LOCK 
        WHERE tag.linkRecKey  EQ ipcLinkRecKey 
        AND tag.tagType     EQ gcTypeInactive 
        AND tag.linkTable   EQ ipcLinkTable 
        NO-ERROR.
    IF NOT AVAILABLE tag THEN 
        RUN pAddTag (ipcLinkRecKey, gcTypeInactive, ipcLinkTable,"Record is inactive").

END PROCEDURE.


PROCEDURE AddTagRelease:
/*------------------------------------------------------------------------------
 Purpose: Wrapper to Add tag, specifically for Release
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkTable AS CHARACTER NO-UNDO.
    
      FIND FIRST tag NO-LOCK 
        WHERE tag.linkRecKey  EQ ipcLinkRecKey 
        AND tag.tagType     EQ gcTypeRelease
        AND tag.linkTable EQ ipcLinkTable
        NO-ERROR.
    IF NOT AVAIL tag THEN 
        RUN pAddTag (ipcLinkRecKey, gcTypeRelease,"Record is manually released from hold").
                                       
END PROCEDURE.



PROCEDURE ClearTagsByRecKey:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey,
                     "RecKey",
                     "").

END PROCEDURE.



PROCEDURE ClearTagsHold:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey,
                     "TagType",
                     gcTypeHold).

    RUN pDeleteTags (ipcLinkRecKey,
                     "TagType",
                     gcTypeInfo).
    
END PROCEDURE.

PROCEDURE ClearTagsInactive:
/*------------------------------------------------------------------------------
 Purpose: Wrapper for ClearTagsOfType specifically for inactive
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey,
                     "TagType",
                     gcTypeInactive).

END PROCEDURE.


PROCEDURE ClearTagsOfType:
/*------------------------------------------------------------------------------
 Purpose:   Clears all tags of a particular type
 Notes:     Possible values = INFO,HOLD,HOLDINFO,RELEASE,INACTIVE,DELETE
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTagType AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey,
                     "TagType",
                     ipcTagType).

END PROCEDURE.

PROCEDURE ClearTagsRelease:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey,
                     "TagType",
                     gcTypeRelease).

    
END PROCEDURE.


PROCEDURE pAddTag PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:   Adds a tag to a given reckey based on provided parameters
     Notes:     ipcLinkRecKey and ipcTagType are mandatory; others are optional
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTagType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.

    CREATE tag.
    ASSIGN 
        tag.linkReckey  = ipcLinkRecKey
        tag.tagType     = ipcTagType     
        tag.linkTable   = ipcLinkTable
        tag.description = ipcDescription.
    
    RELEASE tag.
        
END PROCEDURE.



PROCEDURE pDeleteTags PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcScope AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcValue AS CHARACTER NO-UNDO.

    CASE ipcScope:
        WHEN "RecKey" THEN DO:
            FOR EACH tag EXCLUSIVE WHERE 
                tag.linkRecKey  EQ ipcLinkRecKey:
                DELETE tag.
            END.
        END.
        WHEN "TagType" THEN DO:
            FOR EACH tag EXCLUSIVE WHERE 
                tag.linkRecKey  EQ ipcLinkRecKey AND 
                tag.tagtype     EQ ipcValue:
                DELETE tag.
            END.
        END.
        WHEN "TestName" THEN DO:
            FOR EACH tag EXCLUSIVE WHERE 
                tag.linkRecKey  EQ ipcLinkRecKey AND 
                tag.tagtype     EQ "HOLD" AND 
                tag.description EQ ipcValue:
                    DELETE tag.
            END.
        END.
    END CASE.

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
        tag.tagType EQ gcTypeInactive /*Inactive Type*/
        NO-ERROR.

    RETURN NOT AVAILABLE tag.
		
END FUNCTION.

FUNCTION IsOnHold RETURNS LOGICAL 
    ( ipcLinkRecKey AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:  Given a recKey - returns True if there are no inactive tags linked to it
 Notes:
------------------------------------------------------------------------------*/    

    RETURN CAN-FIND(FIRST tag WHERE  
        tag.linkReckey  EQ ipcLinkRecKey AND 
        tag.tagType     EQ gcTypeHold) 
        AND NOT CAN-FIND(FIRST tag WHERE  
        tag.linkReckey  EQ ipcLinkRecKey AND 
        tag.tagType     EQ "RELEASE" ). 
        
END FUNCTION.


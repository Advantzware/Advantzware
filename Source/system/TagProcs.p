
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
DEFINE VARIABLE gcTypeHold     AS CHARACTER NO-UNDO INITIAL "HOLD".
DEFINE VARIABLE gcTypeInfo     AS CHARACTER NO-UNDO INITIAL "INFO".
DEFINE VARIABLE gcTypeRelease  AS CHARACTER NO-UNDO INITIAL "RELEASE".

DEFINE VARIABLE iCtr  AS INTEGER    NO-UNDO.
DEFINE VARIABLE cNote LIKE tag.note NO-UNDO.

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
    DEFINE INPUT PARAMETER ipcLinkRecKey  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkTable   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcNotes       AS CHARACTER NO-UNDO.

    IF NOT CAN-FIND(FIRST tag 
                    WHERE tag.linkRecKey  EQ ipcLinkRecKey
                      AND tag.tagType     EQ gcTypeHold 
                      AND tag.linkTable   EQ ipcLinkTable
                      AND tag.description EQ ipcDescription) THEN 
    RUN pAddTag(
        INPUT ipcLinkRecKey,
        INPUT gcTypeHold,
        INPUT ipcLinkTable,
        INPUT ipcDescription,
        INPUT ipcNotes
        ).
        
END PROCEDURE.

PROCEDURE AddTagHoldInfo:
/*------------------------------------------------------------------------------
 Purpose: Wrapper to Add tag, specifically for HOLDINFO
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkTable   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcNotes       AS CHARACTER NO-UNDO.

    IF NOT CAN-FIND(FIRST tag 
                    WHERE tag.linkRecKey  EQ ipcLinkRecKey
                      AND tag.tagType     EQ gcTypeInfo 
                      AND tag.linkTable   EQ ipcLinkTable
                      AND tag.description EQ ipcDescription) THEN 
    RUN pAddTag(
        INPUT ipcLinkRecKey,
        INPUT gcTypeInfo,
        INPUT ipcLinkTable,
        INPUT ipcDescription,
        INPUT ipcNotes
        ).
        
END PROCEDURE.

PROCEDURE AddTagInactive:
/*------------------------------------------------------------------------------
 Purpose: Wrapper to Add tag, specifically for Inactive
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkTable  AS CHARACTER NO-UNDO.

    ASSIGN 
        cNote = ""
        cNote[1] = "Inactive " + ipcLinkTable
        .
    IF NOT CAN-FIND(FIRST tag 
                    WHERE tag.linkRecKey EQ ipcLinkRecKey 
                      AND tag.tagType    EQ gcTypeInactive 
                      AND tag.linkTable  EQ ipcLinkTable) THEN 
    RUN pAddTag (ipcLinkRecKey, gcTypeInactive, ipcLinkTable, "Record is inactive").

END PROCEDURE.

PROCEDURE AddTagRelease:
/*------------------------------------------------------------------------------
 Purpose: Wrapper to Add tag, specifically for Release
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkTable  AS CHARACTER NO-UNDO.
    
    IF NOT CAN-FIND(FIRST tag 
                    WHERE tag.linkRecKey EQ ipcLinkRecKey 
                      AND tag.tagType    EQ gcTypeRelease
                      AND tag.linkTable  EQ ipcLinkTable) THEN 
    RUN pAddTag (ipcLinkRecKey, gcTypeRelease, "Record is manually released from hold").
                                       
END PROCEDURE.

PROCEDURE ClearTagsByRecKey:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey, "RecKey", "").

END PROCEDURE.

PROCEDURE ClearTagsHold:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey, "TagType", gcTypeHold).
    RUN pDeleteTags (ipcLinkRecKey, "TagType", gcTypeInfo).
    
END PROCEDURE.

PROCEDURE ClearTagsInactive:
/*------------------------------------------------------------------------------
 Purpose: Wrapper for ClearTagsOfType specifically for inactive
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey, "TagType", gcTypeInactive).

END PROCEDURE.

PROCEDURE ClearTagsOfType:
/*------------------------------------------------------------------------------
 Purpose:   Clears all tags of a particular type
 Notes:     Possible values = INFO,HOLD,HOLDINFO,RELEASE,INACTIVE,DELETE
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTagType AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey, "TagType", ipcTagType).

END PROCEDURE.

PROCEDURE ClearTagsRelease:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey, "TagType", gcTypeRelease).
    
END PROCEDURE.

PROCEDURE pAddTag PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:   Adds a tag to a given reckey based on provided parameters
     Notes:     ipcLinkRecKey and ipcTagType are mandatory; others are optional
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTagType     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkTable   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcNotes       AS CHARACTER NO-UNDO.
    
    CREATE tag.
    ASSIGN 
        tag.linkReckey  = ipcLinkRecKey
        tag.tagType     = ipcTagType     
        tag.linkTable   = ipcLinkTable
        tag.description = ipcDescription
        tag.createDT    = NOW
        tag.updateDT    = tag.createDT
        tag.createUser  = USERID("ASI")
        tag.updateUser  = tag.createUser
        tag.note[1]     = ipcNotes
        .    
    RELEASE tag.
        
END PROCEDURE.

PROCEDURE pDeleteTags PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcScope      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcValue      AS CHARACTER NO-UNDO.

    CASE ipcScope:
        WHEN "RecKey" THEN
        FOR EACH tag EXCLUSIVE-LOCK
            WHERE tag.linkRecKey EQ ipcLinkRecKey
            :
            DELETE tag.
        END.
        WHEN "TagType" THEN
        FOR EACH tag EXCLUSIVE-LOCK
            WHERE tag.linkRecKey EQ ipcLinkRecKey
              AND tag.tagtype    EQ ipcValue
              :
            DELETE tag.
        END.
        WHEN "TestName" THEN
        FOR EACH tag EXCLUSIVE-LOCK
            WHERE tag.linkRecKey  EQ ipcLinkRecKey
              AND tag.tagtype     EQ "HOLD"
              AND tag.description EQ ipcValue
            :
            DELETE tag.
        END.
    END CASE.

END PROCEDURE.

PROCEDURE Tag_IsTagRecordAvailable:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLinkTable  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplAvailable  AS LOGICAL   NO-UNDO.
    
    oplAvailable = CAN-FIND(FIRST tag 
                            WHERE tag.linkRecKey EQ ipcLinkRecKey
                              AND tag.linkTable  EQ ipcLinkTable
                            ).
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION IsActive RETURNS LOGICAL 
	( ipcLinkRecKey AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:  Given a recKey - returns True if there are no inactive tags linked to it
 Notes:
------------------------------------------------------------------------------*/	
    RETURN NOT CAN-FIND(FIRST tag
                        WHERE tag.linkRecKey EQ ipcLinkRecKey
                          AND tag.tagType    EQ gcTypeInactive). /*Inactive Type*/
		
END FUNCTION.

FUNCTION IsOnHold RETURNS LOGICAL 
    ( ipcLinkRecKey AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:  Given a recKey - returns True if there are no inactive tags linked to it
 Notes:
------------------------------------------------------------------------------*/    
    RETURN CAN-FIND(FIRST tag
                    WHERE tag.linkReckey  EQ ipcLinkRecKey
                      AND tag.tagType     EQ gcTypeHold) AND 
       NOT CAN-FIND(FIRST tag
                    WHERE tag.linkReckey  EQ ipcLinkRecKey
                      AND tag.tagType     EQ "RELEASE" ). 
END FUNCTION.

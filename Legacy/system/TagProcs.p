
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
DEFINE VARIABLE gTypeInactive AS CHARACTER NO-UNDO INIT "INACTIVE".
DEFINE VARIABLE gTypeHold AS CHARACTER NO-UNDO INIT "HOLD".
DEFINE VARIABLE gTypeRelease AS CHARACTER NO-UNDO INIT "RELEASE".
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

PROCEDURE addTagHold:
/*------------------------------------------------------------------------------
 Purpose: Wrapper to Add tag, specifically for HOLD/INFO
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcSubType AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER ipcTestProc AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcMessage AS CHARACTER NO-UNDO.

    ASSIGN 
        cNote = ""
        cNote[1] = ipcMessage.

    FIND FIRST tag NO-LOCK WHERE 
        tag.tagType     EQ gTypeHold AND 
        tag.groupCode   EQ ipcSubType AND 
        tag.linkRecKey  EQ ipcLinkRecKey AND 
        tag.description EQ ipcTestProc AND 
        tag.linkTable   EQ ipcLinkTable
        NO-ERROR.
    IF NOT AVAIL tag THEN RUN pAddTag (ipcLinkRecKey,
                                      gTypeHold,
                                      ipcLinkTable,
                                      IF ipcSubType EQ "HOLD" THEN "HOLD" ELSE "",
                                      ipcSubType,
                                      ipcTestProc,
                                      cNote).
END PROCEDURE.


PROCEDURE addTagInactive:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper to Add tag, specifically for Inactive
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLinkTable AS CHARACTER NO-UNDO.

    ASSIGN 
        cNote = ""
        cNote[1] = "Inactive " + ipcLinkTable.

    FIND FIRST tag NO-LOCK WHERE 
        tag.linkRecKey  EQ ipcLinkRecKey AND 
        tag.tagType     EQ gTypeInactive AND 
        tag.linkTable   EQ ipcLinkTable 
        NO-ERROR.
    IF NOT AVAIL tag THEN RUN pAddTag (ipcLinkRecKey,
            gTypeInactive,
            ipcLinkTable,
            "Inactive",
            "",
            "Record is INACTIVE",
            cNote).
END PROCEDURE.


PROCEDURE addTagRelease:
/*------------------------------------------------------------------------------
 Purpose: Wrapper to Add tag, specifically for Inactive
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    ASSIGN 
        cNote = "".

    FIND FIRST tag NO-LOCK WHERE 
        tag.linkRecKey  EQ ipcLinkRecKey AND 
        tag.tagType     EQ gTypeRelease 
        NO-ERROR.
        
    IF NOT AVAIL tag THEN RUN pAddTag (ipcLinkRecKey,
                                       gTypeRelease,
                                       "",
                                       "",
                                       "",
                                       "",
                                       cNote).
                                       
END PROCEDURE.



PROCEDURE clearTagsByRecKey:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey,
                     "RecKey",
                     "").

END PROCEDURE.


PROCEDURE clearTagsByTestName:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTestName AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey,
        "TestName",
        ipcTestName).

END PROCEDURE.


PROCEDURE clearTagsHold:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey,
                     "TagType",
                     gTypeHold).


END PROCEDURE.

PROCEDURE clearTagsInactive:
/*------------------------------------------------------------------------------
 Purpose: Wrapper for ClearTagsOfType specifically for inactive
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey,
                     "TagType",
                     gTypeInactive).

END PROCEDURE.


PROCEDURE clearTagsOfType:
/*------------------------------------------------------------------------------
 Purpose:   Clears all tags of a particular type
 Notes:     Possible values = INFO,HOLD,INACTIVE,DELETE
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTagType AS CHARACTER NO-UNDO.

    RUN pDeleteTags (ipcLinkRecKey,
                     "TagType",
                     ipcTagType).

END PROCEDURE.

PROCEDURE pAddTag PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:   Adds a tag to a given reckey based on provided parameters
     Notes:     ipcLinkRecKey and ipcTagType are mandatory; others are optional
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTagType AS CHARACTER NO-UNDO.

    DEFINE INPUT PARAMETER ipcLinkTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcStatusCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcGroupCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcNote AS CHARACTER EXTENT 5 NO-UNDO.

    CREATE tag.
    ASSIGN 
        tag.linkReckey  = ipcLinkRecKey
        tag.tagType     = ipcTagType.
    ASSIGN         
        tag.linkTable   = ipcLinkTable
        tag.statusCode  = ipcStatusCode
        tag.groupCode   = ipcGroupCode
        tag.description = ipcDescription.
    DO iCtr = 1 TO 5:        
        ASSIGN 
            tag.note[iCtr] = ipcNote[iCtr].
    END.
    
    RELEASE tag.
        
END PROCEDURE.


PROCEDURE pCheckTagHold PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcRecKey AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.

    IF CAN-FIND(FIRST tag WHERE  
        tag.linkReckey  EQ ipcRecKey AND 
        tag.tagType     EQ "HOLD" AND 
        tag.statusCode  EQ "HOLD") 
    AND NOT CAN-FIND(FIRST tag WHERE  
        tag.linkReckey  EQ ipcRecKey AND 
        tag.tagType     EQ "RELEASE" ) THEN ASSIGN 
            oplError = TRUE.
    ELSE ASSIGN 
            oplError = FALSE. 

END PROCEDURE.


PROCEDURE pDeleteTags PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLinkRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCriteria AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcValue AS CHARACTER NO-UNDO.

    CASE ipcCriteria:
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
        tag.tagType EQ gTypeInactive /*Inactive Type*/
        NO-ERROR.

    RETURN NOT AVAILABLE tag.
		
END FUNCTION.

FUNCTION IsOnHold RETURNS LOGICAL 
    ( ipcLinkRecKey AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:  Given a recKey - returns True if there are no inactive tags linked to it
 Notes:
------------------------------------------------------------------------------*/    
    DEF VAR lReturn AS LOG NO-UNDO.
    
    RUN pCheckTagHold (ipcLinkRecKey, OUTPUT lReturn). 

    RETURN lReturn.
        
END FUNCTION.


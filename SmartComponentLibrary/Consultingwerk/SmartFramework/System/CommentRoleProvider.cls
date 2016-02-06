/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : CommentRoleProvider
    Purpose     : SmartFramework dependent ICommentRoleProvider implementation
                  Retrieves comment roles from the SmartDB 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 07 23:07:10 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                         FROM PROPATH . 
USING Consultingwerk.OERA.*                              FROM PROPATH . 
USING Consultingwerk.SmartFramework.ICommentRoleProvider FROM PROPATH .
USING Consultingwerk.Windows.Framework.System.*          FROM PROPATH .  
USING Progress.Lang.*                                    FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.CommentRoleProvider 
    IMPLEMENTS ICommentRoleProvider: 

    {Consultingwerk/SmartFramework/ttCommentRole.i &NO-BEFORE}  
    {Consultingwerk/SmartFramework/System/dsCommentRole.i}
    
    DEFINE VARIABLE lReadFromBackend AS LOGICAL NO-UNDO INIT FALSE .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the ttCommentRole Temp-Table with the list of available 
                 comment roles
        Notes:   
        @param ttCommentRole OUTPUT Temp-Table with the list of available comment roles
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetCommentRoles (OUTPUT TABLE ttCommentRole):
       
        IF lReadFromBackend THEN 
            RETURN . 
       
        EMPTY TEMP-TABLE ttCommentRole . 
       
        FrameworkSettings:ServiceAdapter:RetrieveData ("":U, 
                                                       "Consultingwerk.SmartFramework.System.CommentRoleBusinessEntity":U,
                                                       NEW FetchDataRequest ("eSmartCommentRole":U),
                                                       OUTPUT DATASET dsCommentRole) .
        
        ASSIGN lReadFromBackend = TRUE . 
        
        FOR EACH eSmartCommentRole:
        
            CREATE ttCommentRole .
            BUFFER-COPY eSmartCommentRole TO ttCommentRole
                ASSIGN ttCommentRole.CommentRoleKey = eSmartCommentRole.CommentRoleGuid .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the Count of Comment Roles
        Notes:   
        @return The count of comment roles
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC INTEGER GetCount ():
         
        DEFINE VARIABLE i AS INTEGER NO-UNDO. 
         
        IF NOT lReadFromBackend THEN 
            THIS-OBJECT:GetCommentRoles (OUTPUT TABLE ttCommentRole) .
            
        FOR EACH ttCommentRole:
            i = i + 1 . 
        END.        
            
        RETURN i .     
    END.

END CLASS.

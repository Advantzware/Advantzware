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
    File        : CommentRoleBusinessEntity
    Purpose     : Business Entity for CommentRole
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 24.11.2012 14:58:58
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.*                  FROM PROPATH .
USING Consultingwerk.SmartFramework.*        FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.CommentRoleBusinessEntity 
    INHERITS SmartBusinessEntity: 

    { Consultingwerk/SmartFramework/System/dsCommentRole.i }

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the CommentRoleBusinessEntity class                                                                     
        Notes:   Passes the handle of the dataset instance to the base class and 
                 sets the default DataAccessName                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CommentRoleBusinessEntity ():
        SUPER (DATASET dsCommentRole:HANDLE).
        
        THIS-OBJECT:DataAccessName = "Consultingwerk.SmartFramework.System.CommentRoleDataAccess":U .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook to modify data in the ProDataset after Read and 
                 Update operations (i.e. population of aggregated values)                                                                     
        Notes:   Invoked during FetchData () and SaveChanges ()                                                                     
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ReceiveData ():
        
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Provides a hook for high level data validation before Update 
                 operations                                                                     
        Notes:   Invoked during SaveChanges (). When the ERROR flag of the ProDataset
                 is set, the Update operation will be cancelled before writing back
                 the data to the database using the DataAccess object                                                                      
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID ValidateData ():
        
    END METHOD.

END CLASS.

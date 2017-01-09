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
    File        : CommentRole
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:47:37,798+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.System.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.System.CommentRoleDatasetModel
    INHERITS CommentRoleDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CommentRoleDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CommentRoleDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CommentRoleDatasetModel class
        Notes:   
        @param pCommentRoleGUID The filter value for the eSmartCommentRole.CommentRoleGUID field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CommentRoleDatasetModel (pCommentRoleGUID AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartCommentRole:Filter:CommentRoleGUID:EQ (pCommentRoleGUID) .

        THIS-OBJECT:SmartCommentRole:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.

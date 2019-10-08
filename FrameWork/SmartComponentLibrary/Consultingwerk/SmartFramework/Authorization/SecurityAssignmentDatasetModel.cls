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
    File        : SecurityAssignment
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner
    Created     : 21/12/2013 00:39:37,219+01:00
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.OERA.* FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartFramework.Authorization.SecurityAssignmentDatasetModel
    INHERITS SecurityAssignmentDatasetModel_Generated: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SecurityAssignmentDatasetModel class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SecurityAssignmentDatasetModel ():
        SUPER ().

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SecurityAssignmentDatasetModel class
        Notes:   
        @param pSecurityAssignmentGuid The filter value for the eSmartSecurityAssignment.SecurityAssignmentGuid field
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SecurityAssignmentDatasetModel (pSecurityAssignmentGuid AS CHARACTER):
        SUPER ().

        THIS-OBJECT:SmartSecurityAssignment:Filter:SecurityAssignmentGuid:EQ (pSecurityAssignmentGuid) .

        THIS-OBJECT:SmartSecurityAssignment:Filter:Run () .

    END CONSTRUCTOR.

END CLASS.

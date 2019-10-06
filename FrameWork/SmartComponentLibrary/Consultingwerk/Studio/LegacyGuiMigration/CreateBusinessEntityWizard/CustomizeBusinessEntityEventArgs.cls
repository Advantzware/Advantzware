/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : CustomizeBusinessEntityEventArgs
    Purpose     : Event argument class for the CustomzizeBusinessEntity event
                  of the CreateBusinessEntityWizardForm class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Mar 09 20:20:53 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                                      FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.*                      FROM PROPATH .
USING Consultingwerk.Studio.LegacyGuiMigration.CreateBusinessEntityWizard.* FROM PROPATH .  
USING Progress.Lang.*                                                       FROM PROPATH .

CLASS Consultingwerk.Studio.LegacyGuiMigration.CreateBusinessEntityWizard.CustomizeBusinessEntityEventArgs 
    INHERITS CancelableEventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Business Entity Dataset controller
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Controller AS BusinessEntityDatasetController NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the Business Entity design dataset
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Dataset AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the CustomizeBusinessEntityEventArgs class
        Notes:
        @param phDataset The handle of the Business Entity design dataset
        @param poController The reference to the BusinessEntityDatasetController instance
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC CustomizeBusinessEntityEventArgs (phDataset AS HANDLE,
                                                         poController AS BusinessEntityDatasetController):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Dataset    = phDataset 
               THIS-OBJECT:Controller = poController . 
        
    END CONSTRUCTOR.

END CLASS.


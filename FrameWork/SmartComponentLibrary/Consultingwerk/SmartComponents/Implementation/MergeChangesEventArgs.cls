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
    File        : MergeChangesEventArgs
    Purpose     : Event Argument for the BeforeMergeChanges and 
                  AfterMergeChanges events of the SmartBusinessEntityAdapter
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Oct 01 22:53:59 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.MergeChangesEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the dataset with changes 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ChangeDataset AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the original dataset
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY OriginalDataset AS HANDLE NO-UNDO 
    GET.
    PRIVATE SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the MergeChangesEventArgs class
        Notes:   
        @param phChangeDataset The handle of the dataset with changes  
        @param phOriginalDataset The handle of the original dataset
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC MergeChangesEventArgs (phChangeDataset AS HANDLE,
                                              phOriginalDataset AS HANDLE):
        SUPER ().
        
        ASSIGN THIS-OBJECT:ChangeDataset   = phChangeDataset
               THIS-OBJECT:OriginalDataset = phOriginalDataset .
        
    END CONSTRUCTOR.

END CLASS.

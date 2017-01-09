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
    File        : TempTableAstNodeAvailbleEventArgs
    Purpose     : Event argument for the TempTableAstNodeAvailable event
                  of the TempTableAstWalker class
    Syntax      : 
    Description : 
    Author(s)   : MIke Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 20 06:55:41 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.EventArgs                         FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Services.* FROM PROPATH .
USING Consultingwerk.Framework.Collections.*           FROM PROPATH .   
USING org.prorefactor.core.JPNode                      FROM ASSEMBLY .
USING Progress.Lang.*                                  FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Services.TempTableAstNodeAvailableEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Refernce to the JPNode that contains the Temp-Table
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AstNode AS JPNode NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Reference to the Temp-Table that is available
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Controller AS BusinessEntityDatasetController NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the Reference to the Temp-Table that is available
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY List AS CharacterList NO-UNDO 
    GET.
    PROTECTED SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Returns the Reference to the Temp-Table that is available
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TempTableAstNodeAvailbleEventArgs class      
        Notes:                                                          
        @param pcTableName The name of the Temp-Table that is available
        @param poNode The reference to the JPNode object
        @param poController The reference to the BusinessEntityDatasetController used in this context
        @param poList The reference to a CharacterList used in this context              
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TempTableAstNodeAvailableEventArgs (pcTableName AS CHARACTER,
                                                           poNode AS JPNode,
                                                           poController AS BusinessEntityDatasetController,
                                                           poList AS CharacterList):
        SUPER ().
        
        ASSIGN THIS-OBJECT:TableName  = pcTableName
               THIS-OBJECT:AstNode    = poNode
               THIS-OBJECT:Controller = poController
               THIS-OBJECT:List       = poList . 
        
    END CONSTRUCTOR.

END CLASS.

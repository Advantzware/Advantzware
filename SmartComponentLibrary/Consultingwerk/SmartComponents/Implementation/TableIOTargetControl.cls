/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : TableIOTargetControl
    Purpose     : Controls enabled Table IO Tools for the SmartToolbarController
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Sep 04 09:40:36 CEST 2014
    Notes       : See http://jira.consultingwerkcloud.com/browse/SCL-442 
                  for details
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.TableIOTargetControl: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current TableIOState of the SmartTableIOTarget
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SmartTableIOState AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the SmartTableIOTarget
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SmartTableIOTarget AS ISmartTableIOTarget NO-UNDO 
	GET.
	SET. 
	
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the Add function should be disabled regardless of 
                 it's state
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY TableIOAddDisabled AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the Cancel function should be disabled regardless of 
                 it's state
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableIOCancelDisabled AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the Copy function should be disabled regardless of 
                 it's state
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableIOCopyDisabled AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the Delete function should be disabled regardless of 
                 it's state
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableIODeleteDisabled AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the Save function should be disabled regardless of 
                 it's state
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableIOSaveDisabled AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets if the Update function should be disabled regardless of 
                 it's state
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TableIOUpdateDisabled AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

	/*------------------------------------------------------------------------------
	    Purpose: Returns if the tool with the given ToolKey is Disabled regardless of  
	             it's state
	    Notes:
        @param pcToolKey The key of the tool to return the Disabled state for
        @return True when the tool is disabled, false when it'S enabeld
	------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL GetToolDisabled (pcToolKey AS CHARACTER):
		
        CASE pcToolKey:
            WHEN "TableIOAdd":U    THEN RETURN THIS-OBJECT:TableIOAddDisabled    . 
            WHEN "TableIOCancel":U THEN RETURN THIS-OBJECT:TableIOCancelDisabled . 
            WHEN "TableIOCopy":U   THEN RETURN THIS-OBJECT:TableIOCopyDisabled   . 
            WHEN "TableIODelete":U THEN RETURN THIS-OBJECT:TableIODeleteDisabled . 
            WHEN "TableIOSave":U   THEN RETURN THIS-OBJECT:TableIOSaveDisabled   . 
            WHEN "TableIOUpdate":U THEN RETURN THIS-OBJECT:TableIOUpdateDisabled . 
        END.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets if the tool with the given ToolKey is Disabled regardless of  
                 it's state
        Notes:
        @param pcToolKey The key of the tool to assign Disabled state for
        @param plDisabled Disabled or Enabled
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetToolDisabled (pcToolKey AS CHARACTER, 
                                        plDisabled AS LOGICAL):
        
        CASE pcToolKey:
            WHEN "TableIOAdd":U    THEN ASSIGN THIS-OBJECT:TableIOAddDisabled    = plDisabled . 
            WHEN "TableIOCancel":U THEN ASSIGN THIS-OBJECT:TableIOCancelDisabled = plDisabled . 
            WHEN "TableIOCopy":U   THEN ASSIGN THIS-OBJECT:TableIOCopyDisabled   = plDisabled . 
            WHEN "TableIODelete":U THEN ASSIGN THIS-OBJECT:TableIODeleteDisabled = plDisabled . 
            WHEN "TableIOSave":U   THEN ASSIGN THIS-OBJECT:TableIOSaveDisabled   = plDisabled . 
            WHEN "TableIOUpdate":U THEN ASSIGN THIS-OBJECT:TableIOUpdateDisabled = plDisabled . 
        END.

    END METHOD.

END CLASS.

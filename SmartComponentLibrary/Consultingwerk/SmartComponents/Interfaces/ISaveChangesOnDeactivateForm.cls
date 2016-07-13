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
    File        : ISaveChangesOnDeactivateForm
    Purpose     : Interface for classes (SmartViewerControl / SmartUpdatableBrowser
                  that support prompting the user to save changes when a 
                  Form is deactivated
    Syntax      : 
    Description : The behavior is similar to the ICheckOkToClose behaviour 
                  with the difference, that the Deactivate will not be 
                  prevented
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Aug 06 09:17:23 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISaveChangesOnDeactivateForm:  
  
    /*------------------------------------------------------------------------------
        Purpose: Saves pending changes when a Form is deactivated 
        Notes:   
        @param plShowMessage When TRUE a message is asking the user if values should be saved. When FALSE values will be saved silently
        @return True when the user did choose not to save or the save was successful, False when the control remains in Update state
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL SaveChangesOnDeactivateForm (plShowMessage AS LOGICAL).

END INTERFACE.

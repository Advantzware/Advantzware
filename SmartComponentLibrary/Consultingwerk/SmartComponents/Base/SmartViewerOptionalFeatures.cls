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
    File        : SmartViewerOptionalFeatures
    Purpose     : Contains toggles that allow to turn on or off optional
                  hooks in the SmartViewerControl code base  
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 19 16:15:57 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Base.SmartViewerOptionalFeatures: 

    /*------------------------------------------------------------------------------
        Purpose: Allows to turn on if the SmartViewerControl should call into the 
                 overridable method CustomInitializeControlHandler
        Notes:   SCL-814   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY CallCustomInitializeControlHandler AS LOGICAL INITIAL FALSE NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Allows directly enable the Data Source's UpdateState when fields are
                 enabeld in a viewer
        Notes:   SCL-829   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BeginUpdateStateOnEnableFields AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    SET. 

END CLASS.

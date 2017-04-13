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
    File        : SmartComponentsDesignCustomizer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 15 07:10:04 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.* FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Design.SmartComponents.SmartComponentsDesignCustomizer: 
  
    DEFINE PUBLIC STATIC PROPERTY SmartViewerDesignerControl_FormClosingPrompt AS CHARACTER 
       INITIAL "You are currently designing a Dynamic Viewer Control. Are you sure that you want to close?"{&TRAN} NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Private Constructor to disable instance creation                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE SmartComponentsDesignCustomizer ():
        SUPER ().
        
    END CONSTRUCTOR.

END CLASS.
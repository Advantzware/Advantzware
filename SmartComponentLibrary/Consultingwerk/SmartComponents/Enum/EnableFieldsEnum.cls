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
    File        : EnableFieldEnum
    Purpose     : Enumeration for possible EnableFields values
    Syntax      : 
    Description : Used as a parameter to the EnableFields method of the 
                  SmartViewerControl and SmartupdatableBrowser 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 19 10:39:24 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.* FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Enum.EnableFieldsEnum : 

    /*------------------------------------------------------------------------------
        Purpose: The Viewer/Browser should enable those fields only required for
                 Add                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC STATIC PROPERTY Add AS CHARACTER NO-UNDO INIT "Add":U 
    GET .

    /*------------------------------------------------------------------------------
        Purpose: The Viewer/Browser should enable those fields required for Update                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC STATIC PROPERTY Update AS CHARACTER NO-UNDO INIT "Update":U 
    GET .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the EnableFieldsEnum class
        Notes:   Disable instance creation   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE EnableFieldsEnum ():
        SUPER ().
        
    END CONSTRUCTOR.


END CLASS.
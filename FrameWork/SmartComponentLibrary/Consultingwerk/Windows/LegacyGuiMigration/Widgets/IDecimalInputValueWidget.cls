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
    File        : IDecimalInputValueWidget
    Purpose     : Interface for Widget representations that provide a 
                  DECIMAL INPUT-VALUE
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Sep 06 00:56:48 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.* FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

INTERFACE Consultingwerk.Windows.LegacyGuiMigration.Widgets.IDecimalInputValueWidget
&IF NOT PROVERSION BEGINS "10" &THEN    
    INHERITS IWidgetFacade
&ENDIF        
    :  
  
    /*------------------------------------------------------------------------------
        Purpose: Used for data-representation widgets, such as field-level widgets 
                 that represent variables or database fields
        Notes:   The value for the INPUT-VALUE attribute is the unformatted 
                 SCREEN-VALUE of a widget                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY INPUT-VALUE AS DECIMAL NO-UNDO 
    GET.
    SET. 

END INTERFACE.

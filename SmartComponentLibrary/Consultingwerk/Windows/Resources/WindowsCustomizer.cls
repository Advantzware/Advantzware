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
    File        : SmartComponentLibraryCustomizer
    Purpose     : This class is an entry point for customization and 
                  translation of strings used for messages for the 
                  components in the Consultingwerk.Windows package.  
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Fri Dec 16 12:53:33 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Progress.Lang.*.

CLASS Consultingwerk.Windows.Resources.WindowsCustomizer: 

    /*------------------------------------------------------------------------------
        Purpose: Allows customization of the Caption of the Close button on the UltraGridColumnChooserForm                                                                   
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ColumnChooserCloseButtonCaption AS CHARACTER INITIAL "&Close"{&TRAN} NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Allows customization of the Text property of the UltraGridColumnChooserForm                                                                   
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ColumnChooserFormCaption AS CHARACTER INITIAL "Column Chooser"{&TRAN} NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE WindowsCustomizer ():
        SUPER ().
        
    END CONSTRUCTOR.

END CLASS.

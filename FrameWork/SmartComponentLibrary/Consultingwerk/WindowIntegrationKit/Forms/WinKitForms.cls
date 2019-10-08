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
    File        : WinKitForms
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Aug 20 13:32:01 CEST 2013
    Notes       : Avoid the usage of certain static members in hybrid 
                  classes on OpenEdge 11.2 and 11.3 due to OE defect OE00240707 
                  http://knowledgebase.progress.com/articles/Article/000042229?popup=true
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.Forms.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.Forms.WinKitForms: 

    /*------------------------------------------------------------------------------
        Purpose: Defines the default for the MakeMdiChild property 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY DefaultMakeMdiChild AS LOGICAL NO-UNDO INIT TRUE 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the default Col offset used when positioning the 
                 WindowContainer control in this .NET Form.      
        Notes:   Static property, sets default for NEW instances of this class                                                                           
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC STATIC PROPERTY DefaultWindowContainerColOffset AS INTEGER NO-UNDO 
    GET.
    SET.
    
     /*------------------------------------------------------------------------------
        Purpose: Gets and Sets the default Row offset used when positioning the 
                 WindowContainer control in this .NET Form.      
        Notes:   Static property, sets default for NEW instances of this class                                                                           
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC STATIC PROPERTY DefaultWindowContainerRowOffset AS INTEGER NO-UNDO 
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Controls the BGCOLOR of the ABL Window and TabFolder created 
                 for the Tab Pages
        Notes:                                                                              
    ------------------------------------------------------------------------------*/            
    DEFINE PUBLIC STATIC PROPERTY TabFolderBGColor AS INTEGER INITIAL 100 NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Locates the IEmbeddedWindowForm instance that embeds the WINDOW                                                                        
        Notes:                                                                  
        @param phWindow The handle of the WINDOW widget
        @return The reference to the IEmbeddedWindowForm instance       
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC IEmbeddedWindowForm FromEmbeddedWindow (phWindow AS HANDLE):
        
        DEFINE VARIABLE oForm     AS Progress.Lang.Object NO-UNDO .
        DEFINE VARIABLE oEmbedded AS IEmbeddedWindowForm  NO-UNDO . 

        IF phWindow:TYPE = "WINDOW":U THEN DO:
        
            oForm = SESSION:FIRST-OBJECT .  
            
            /* Mike Fechner, Consultingwerk Ltd. 24.11.2011
               First check the primary window container */
            DO WHILE VALID-OBJECT (oForm):
                IF TYPE-OF (oForm, IEmbeddedWindowForm) THEN DO:
                
                    oEmbedded = CAST (oForm, IEmbeddedWindowForm) .                
                
                    IF oEmbedded:EmbeddedWindow = phWindow THEN 
                        RETURN oEmbedded . 
                END . 
                
                oForm = oForm:Next-Sibling .
            END.
        END.

        /* Mike Fechner, Consultingwerk Ltd. 24.11.2011
           Now check the tab pages */
        oForm = SESSION:FIRST-OBJECT .                     

        DO WHILE VALID-OBJECT (oForm):
            IF TYPE-OF (oForm, IEmbeddedWindowForm) THEN DO:
            
                oEmbedded = CAST (oForm, IEmbeddedWindowForm) .                

                IF VALID-OBJECT (oEmbedded:WindowContainerOfWidget (phWindow)) THEN             
                    RETURN oEmbedded . 
            END . 
            
            oForm = oForm:Next-Sibling .
        END.
        
        RETURN ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the EmbeddedWindowForm instance that belongs to the given 
                 Procedure Handle                                                                         
        Notes:                   
        @param phProcedure The procedure handle to search for
        @return The IEmbeddedWindowForm instance that belongs to the Procedure handle                                                     
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC IEmbeddedWindowForm FromProcedureHandle (phProcedure AS HANDLE):
        
        DEFINE VARIABLE oForm     AS Progress.Lang.Object NO-UNDO .
        DEFINE VARIABLE oEmbedded AS IEmbeddedWindowForm  NO-UNDO . 

        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phProcedure Consultingwerk.WidgetTypeEnum:Procedure} .

        oForm = SESSION:FIRST-OBJECT .  
        
        /* Mike Fechner, Consultingwerk Ltd. 24.11.2011
           First check the primary window container */
        DO WHILE VALID-OBJECT (oForm):
            IF TYPE-OF (oForm, IEmbeddedWindowForm) THEN DO:
            
                oEmbedded = CAST (oForm, IEmbeddedWindowForm) .                
            
                IF oEmbedded:ProcedureHandle = phProcedure THEN 
                    RETURN oEmbedded . 
            END . 
            
            oForm = oForm:Next-Sibling .
        END.

        RETURN ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Menu-bar of the given Window regardless if the window
                 is embedded or not.
        Notes:   If the window is not embedded, the MENU-BAR property of the Window 
                 will be returned. If it is embedded, the MENU-BAR property of the 
                 IEmbeddedWindowForm will be returned.
        @param phWindow The handle of hte WINDOW widget
        @return The handle of the MENU-BAR of the window
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC HANDLE MenuBarOfWindow (phWindow AS HANDLE):
        
        DEFINE VARIABLE oForm AS Consultingwerk.WindowIntegrationKit.Forms.IEmbeddedWindowForm NO-UNDO . 
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phWindow Consultingwerk.WidgetTypeEnum:Window} .
        
        IF VALID-HANDLE (phWindow:MENU-BAR) THEN
             RETURN phWindow:MENU-BAR . 
             
        oForm = WinKitForms:FromEmbeddedWindow (phWindow) .
        
        IF VALID-OBJECT (oForm) THEN
            RETURN oForm:MENU-BAR .
             
        RETURN ? . 

    END METHOD.
    
END CLASS.

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
    File        : BaseFormHelper
    Purpose     : Helper class for IBaseForm classes
    Syntax      : 
    Description : Delegate for BaseForm, BaseRadForm and BaseRadRibbonForm
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 19 17:11:35 CET 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Forms.* FROM PROPATH .  
USING Progress.Lang.*        FROM PROPATH .
USING Consultingwerk.Framework.* FROM PROPATH.
USING System.Windows.Forms.* FROM ASSEMBLY.
USING Consultingwerk.Framework.Authorization.* FROM PROPATH.
USING Consultingwerk.Framework.Authorization.Exceptions.* FROM PROPATH.

CLASS Consultingwerk.Forms.BaseFormHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BaseFormHelper class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE BaseFormHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Invoked FormAuthorizationProvider to verify if the current user 
                 is allowed to launch the current form
        Notes:   Throws FormNotAuthorizedException
        @param poForm The IBaseForm to store the settings for 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID CheckFormAuthorizationProvider (poForm AS IBaseForm):
        
        DEFINE VARIABLE oFormAuthorizationProvider AS IFormAuthorizationProvider NO-UNDO . 

        IF NOT poForm:DesignTime THEN DO:
            oFormAuthorizationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.Authorization.IFormAuthorizationProvider} .
            
            IF VALID-OBJECT (oFormAuthorizationProvider) THEN DO:
                IF oFormAuthorizationProvider:IsFormStartupRestricted (poForm,
                                                                       poForm:GetClass():TypeName) THEN 
                    UNDO, THROW NEW FormNotAuthorizedException (poForm:GetClass():TypeName) .
            END.
        END.        

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Closes the Forms Dependent Forms
        Notes:
        @param poForm The IBaseForm to store the settings for 
        @param e The FormClosingEventArgs of the curernt FormClosing event handler
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID CloseDependentForms (poForm AS IBaseForm,
                                                   e AS FormClosingEventArgs):
        
        DEFINE VARIABLE oForm AS Progress.Windows.Form NO-UNDO . 
            
        IF NOT poForm:DesignTime AND NOT e:Cancel THEN DO:
            
            /* Mike Fechner, Consultingwerk Ltd. 08.11.2011
               Loop thru the DependensForms and attempt to close them. 
               When they remain open (reference still valid and IsClosed
               equals FALSE) we do not proceed and Close this Form instance */
            DO WHILE poForm:DependentForms:Count > 0 ON ERROR UNDO, THROW:
                
                ASSIGN oForm = poForm:DependentForms[0] NO-ERROR .
            
                IF VALID-OBJECT (oForm) THEN 
                    oForm:Close () .
                
                IF VALID-OBJECT (oForm) AND
                   TYPE-OF (oForm, IBaseForm) AND
                   CAST (oForm, IBaseForm):IsClosed = FALSE THEN DO:

                   e:Cancel = TRUE .
                   RETURN .
                END.

                CATCH sysex AS System.NullReferenceException:
                        
                END CATCH.
                
                FINALLY:
                    poForm:DependentForms:RemoveAt (0) .        
                END FINALLY.
            END.
        END.    

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Loads stored window settings from the registry                                                                     
        Notes:     
        @param poForm The IBaseForm to store the settings for 
        @param pcSettingsKey The key under which the settings for this Form are stored (e.g. Position)
        @return The string containing the stored window settings 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER LoadSettings (poForm AS IBaseForm,
                                                 pcSettingsKey AS CHARACTER):
    
        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        
        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        IF NOT VALID-OBJECT (oSettingsService) THEN 
            RETURN "":U . 
        
        RETURN oSettingsService:GetSetting (poForm:WindowPositionRegistryKey, 
                                            pcSettingsKey) .
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Restores the stored window position                                                                        
        Notes:                                                                        
        @param poForm The IBaseForm to store the settings for 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID RestoreWindowPosition (poForm AS IBaseForm):
        
        DEFINE VARIABLE cPosition    AS CHARACTER                 NO-UNDO.
        DEFINE VARIABLE iTop         AS INTEGER                   NO-UNDO.
        DEFINE VARIABLE iLeft        AS INTEGER                   NO-UNDO.
        DEFINE VARIABLE iWidth       AS INTEGER                   NO-UNDO.
        DEFINE VARIABLE iHeight      AS INTEGER                   NO-UNDO.
        DEFINE VARIABLE cWindowState AS CHARACTER                 NO-UNDO.
        
        DEFINE VARIABLE oForm        AS System.Windows.Forms.Form NO-UNDO . 
        
        oForm = CAST (CAST (poForm, Progress.Lang.Object), System.Windows.Forms.Form) .
        
        ASSIGN cPosition = BaseFormHelper:LoadSettings (poForm, 
                                                        "Position":U) .
                           
        IF NUM-ENTRIES(cPosition) >= 2 THEN DO:                             
            ASSIGN iTop    = INTEGER(ENTRY(1, cPosition))
                   iLeft   = INTEGER(ENTRY(2, cPosition)) 
                   iWidth  = INTEGER(ENTRY(3, cPosition)) 
                   iHeight = INTEGER(ENTRY(4, cPosition)) NO-ERROR . 
                   
            IF iTop <> ? AND iLeft <> ? THEN DO:
                /* Ensure that the LoginDialog is displayed on an existing Display.
                   If it is not, the dialog will be displayed on the PrimaryScreen
                   of the system. */
                IF Consultingwerk.Util.FormHelper:IsFormOnValidDisplay(INPUT-OUTPUT iTop, 
                                                                       INPUT-OUTPUT iLeft,
                                                                       iWidth,
                                                                       iHeight) THEN 
                DO:
                    ASSIGN oForm:StartPosition = FormStartPosition:Manual  
                           oForm:Top           = iTop  
                           oForm:Left          = iLeft .
                    
                    IF iWidth <> ? AND iHeight <> ? AND 
                       Progress.Util.EnumHelper:AreEqual (oForm:FormBorderStyle, 
                                                          FormBorderStyle:Sizable) OR 
                       Progress.Util.EnumHelper:AreEqual (oForm:FormBorderStyle, 
                                                          FormBorderStyle:SizableToolWindow) THEN                                                                               
                        ASSIGN oForm:Width  = iWidth
                               oForm:Height = iHeight .
                END.             
            END.    
            
            ASSIGN cWindowState = ENTRY(5, cPosition) NO-ERROR . 
            
            IF cWindowState > "":U THEN DO:
                oForm:WindowState = CAST(System.Enum:Parse (Progress.Util.TypeHelper:GetType("System.Windows.Forms.FormWindowState":U),
                                                            cWindowState), FormWindowState) .
            END.                                  
        END.                           
        
        CATCH e AS Progress.Lang.Error :
            /* nothing, ignore errors */           
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores window settings to the registry                                                                       
        Notes:       
        @param poForm The IBaseForm to store the settings for 
        @param pcSettings The string containing the window settings to be stored
        @param pcSettingsKey The key under which the settings for this Form are stored (e.g. Position)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StoreSettings (poForm AS IBaseForm,
                                             pcSettings AS CHARACTER,
                                             pcSettingsKey AS CHARACTER):
        
        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        
        oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .
        
        IF VALID-OBJECT (oSettingsService) THEN 
            oSettingsService:StoreSetting (poForm:WindowPositionRegistryKey,
                                           pcSettingsKey, 
                                           pcSettings) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Stores the window position to the resgistry                                                                        
        Notes:                                                                        
        @param poForm The IBaseForm to store the settings for 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID StoreWindowPosition (poForm AS IBaseForm):
        
        DEFINE VARIABLE cPosition AS CHARACTER                 NO-UNDO.
        
        DEFINE VARIABLE oForm     AS System.Windows.Forms.Form NO-UNDO . 
        
        oForm = CAST (CAST (poForm, Progress.Lang.Object), System.Windows.Forms.Form) .

        IF Progress.Util.EnumHelper:AreEqual (oForm:WindowState, 
                                              FormWindowState:Normal) THEN 
            ASSIGN cPosition = SUBSTITUTE ("&1,&2,&3,&4,&5":U,
                                           oForm:Top, 
                                           oForm:Left, 
                                           oForm:Width, 
                                           oForm:Height,
                                           oForm:WindowState:ToString()) .
        ELSE 
            ASSIGN cPosition = SUBSTITUTE ("&1,&2,&3,&4,&5":U,
                                           oForm:RestoreBounds:Top, 
                                           oForm:RestoreBounds:Left, 
                                           oForm:RestoreBounds:Width, 
                                           oForm:RestoreBounds:Height,
                                           oForm:WindowState:ToString()) .        
        
        BaseFormHelper:StoreSettings (poForm, cPosition, "Position":U) .

    END METHOD.


END CLASS.

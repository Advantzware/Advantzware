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
    File        : ValueLists
    Purpose     : A general purpose provider for framework related 
                  Infragistics ValueLists
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 09 11:24:15 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.SmartFramework.*    FROM PROPATH . 
USING Consultingwerk.Windows.Framework.* FROM PROPATH .  
USING Consultingwerk.Util.*              FROM PROPATH . 
USING Infragistics.Win.*                 FROM ASSEMBLY . 
USING Progress.Lang.*                    FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.ValueLists: 

    {Consultingwerk/SmartFramework/ttAttachmentRole.i &ACCESS="STATIC" &NO-BEFORE &REFERENCE-ONLY=REFERENCE-ONLY}  
    {Consultingwerk/SmartFramework/ttCommentRole.i &ACCESS="STATIC" &NO-BEFORE &REFERENCE-ONLY=REFERENCE-ONLY}  
    {Consultingwerk/SmartFramework/ttLanguage.i &ACCESS="STATIC" &NO-BEFORE &REFERENCE-ONLY=REFERENCE-ONLY}  

    DEFINE STATIC VARIABLE oImageProvider AS IImageProvider NO-UNDO . 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ValueLists class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR STATIC ValueLists ():
		
        ASSIGN oImageProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.Framework.IImageProvider} .
        
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PRIVATE ValueLists ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Adds a single item to a value list
        Notes:   
        @param poValueList The ValueList to add an item to
        @param poKeyValue The key value for the ValueListItem
        @param pcDisplayText The DisplayText for the ValueListItem
        @param pcImageKey The key of the Image to retrieve from the ImageProvider service
        @return The new ValueListItem
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC ValueListItem AddValueListItem (poValueList AS ValueList,
        	                                             poKeyValue AS System.Object,
        	                                             pcDisplayText AS CHARACTER,
        	                                             pcImageKey AS CHARACTER):
		
		DEFINE VARIABLE oValueListItem AS ValueListItem NO-UNDO . 
		
		{Consultingwerk/Assertion/ObjectAssert/IsValid.i poValueList """poValueList"":U"} .
        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poKeyValue """poKeyValue"":U"} .
		
		ASSIGN oValueListItem = poValueList:ValueListItems:Add (poKeyValue) . 

        IF pcDisplayText > "":U THEN 
            oValueListItem:DisplayText = pcDisplayText .
            
        IF pcImageKey > "":U AND VALID-OBJECT (oImageProvider) THEN 
            oValueListItem:Appearance:Image = oImageProvider:ImageForKey (pcImageKey) .

        RETURN oValueListItem .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a value list for attachement roles
        Notes:   
        @return The ValueList containing the values (key and display value)   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC ValueList GetAttachmentRoleValueList ():
		
        DEFINE VARIABLE oValueList              AS ValueList               NO-UNDO . 
        DEFINE VARIABLE oAttachmentRoleProvider AS IAttachmentRoleProvider NO-UNDO . 

        ASSIGN oAttachmentRoleProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.IAttachmentRoleProvider} .  
        
        oAttachmentRoleProvider:GetAttachmentRoles (OUTPUT TABLE ttAttachmentRole) .
        
        oValueList = ValueListHelper:FromTempTableAndImageProvider (TEMP-TABLE ttAttachmentRole:HANDLE,
                                    "AttachmentRoleKey":U,
                                    "AttachmentRoleName":U,
                                    "&1":U,
                                    "AttachmentRoleImageSmall":U,
                                    ?,
                                    ?) .
        
        RETURN oValueList .
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a value list for comment roles
        Notes:   
        @return The ValueList containing the values (key and display value)   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ValueList GetCommentRoleValueList ():
        
        DEFINE VARIABLE oValueList           AS ValueList            NO-UNDO . 
        DEFINE VARIABLE oCommentRoleProvider AS ICommentRoleProvider NO-UNDO . 

        ASSIGN oCommentRoleProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ICommentRoleProvider} .  
        
        oCommentRoleProvider:GetCommentRoles (OUTPUT TABLE ttCommentRole) .
        
        oValueList = ValueListHelper:FromTempTableAndImageProvider (TEMP-TABLE ttCommentRole:HANDLE,
                                    "CommentRoleKey":U,
                                    "CommentRoleName":U,
                                    "&1":U,
                                    "CommentRoleImageSmall":U,
                                    ?,
                                    ?) .
        
        RETURN oValueList .
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a ValueList to visualize the availbility of Attachments in 
                 a grid
        Notes:   
        @return The ValueList with the Paperclip to show the availability of an attachment
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC ValueList GetAttachmentValueList ():
		
        DEFINE VARIABLE oValueList        AS ValueList         NO-UNDO . 

        oValueList = NEW ValueList () .
        
        oValueList:DisplayStyle = ValueListDisplayStyle:Picture .

        ValueLists:AddValueListItem (oValueList,
                                     BOX (TRUE),
                                     "":U,
                                     "Consultingwerk\Windows\Framework\Images\SmartFramework\paperclip_16.png":U) .

        ValueLists:AddValueListItem (oValueList,
                                     BOX (FALSE),
                                     "":U,
                                     ?) .

        RETURN oValueList . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a ValueList to visualize the availbility of Comments in 
                 a grid
        Notes:   
        @return The ValueList with the Comments icon to show the availability of an comments
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ValueList GetCommentsValueList ():
        
        DEFINE VARIABLE oValueList        AS ValueList         NO-UNDO . 

        oValueList = NEW ValueList () .
        
        oValueList:DisplayStyle = ValueListDisplayStyle:Picture .

        ValueLists:AddValueListItem (oValueList,
                                     BOX (TRUE),
                                     "":U,
                                     "Consultingwerk\Windows\Framework\Images\SmartFramework\message_16.png":U) .

        ValueLists:AddValueListItem (oValueList,
                                     BOX (FALSE),
                                     "":U,
                                     ?) .

        RETURN oValueList . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a value list for languages
        Notes:   
        @return The ValueList containing the values (key and display value)   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ValueList GetLanguageValueList ():

        RETURN ValueLists:GetLanguageValueList (?, ?) .

    END METHOD . 

    /*------------------------------------------------------------------------------
        Purpose: Returns a value list for languages, optionally with a flag value (empty/all/etc)
        Notes:   
        @param pcFlagValue The CHARACTER value used for a Flag value (e.g. all or none)        
        @param pcFlagText The Display Value for the flag value
        @return The ValueList containing the values (key and display value)   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC ValueList GetLanguageValueList (pcFlagValue AS CHARACTER,
                                                         pcFlagText AS CHARACTER):
		
        DEFINE VARIABLE oValueList        AS ValueList         NO-UNDO . 
        DEFINE VARIABLE oLanguageProvider AS ILanguageProvider NO-UNDO . 

        ASSIGN oLanguageProvider = {Consultingwerk/get-service.i Consultingwerk.SmartFramework.ILanguageProvider} .  
        
        oLanguageProvider:GetAvailableLanguages (OUTPUT TABLE ttLanguage) .
        
        oValueList = ValueListHelper:FromTempTableAndImageProvider (TEMP-TABLE ttLanguage:HANDLE,
                                    "LanguageKey":U,
                                    "LanguageName,LanguageIsoCode":U,
                                    "&1 (&2)":U,
                                    "LanguageImage":U,
                                    pcFlagValue,
                                    pcFlagText) .
        
        RETURN oValueList .
        
	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a value list with message types, optionally a flag value
        Notes:   
        @param pcFlagValue The CHARACTER value used for a Flag value (e.g. all or none)        
        @param pcFlagText The Display Value for the flag value
        @return The value list containing the message types 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC ValueList GetMessageTypeValueList (pcFlagValue AS CHARACTER,
                                                            pcFlagText AS CHARACTER):

        DEFINE VARIABLE oValueList     AS ValueList     NO-UNDO .
        
        oValueList = ValueLists:GetMessageTypeValueList () .
        
        IF pcFlagValue > "":U THEN DO:
            oValueList:ValueListItems:Insert (0, 
                                              BOX (pcFlagValue), 
                                              pcFlagText) .
        END.    
        
        RETURN oValueList . 
        
    END METHOD .     
        
    /*------------------------------------------------------------------------------
        Purpose: Returns a value list with message types, optionally a flag value
        Notes:   
        @return The value list containing the message types 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC ValueList GetMessageTypeValueList ():
		
        DEFINE VARIABLE oValueList     AS ValueList      NO-UNDO .

        oValueList = NEW ValueList () .
        
        oValueList:DisplayStyle = ValueListDisplayStyle:DisplayTextAndPicture .

        ValueLists:AddValueListItem (oValueList,
                                     BOX ("Error":U),
                                     "Error"{&TRAN},
                                     "Consultingwerk\Windows\Framework\Images\error_16.png":U) .

        ValueLists:AddValueListItem (oValueList,
                                     BOX ("Info":U),
                                     "Information"{&TRAN},
                                     "Consultingwerk\Windows\Framework\Images\information_16.png":U) .

        ValueLists:AddValueListItem (oValueList,
                                     BOX ("Question":U),
                                     "Question"{&TRAN},
                                     "Consultingwerk\Windows\Framework\Images\question_16.png":U) .

        ValueLists:AddValueListItem (oValueList,
                                     BOX ("Warning":U),
                                     "Warning"{&TRAN},
                                     "Consultingwerk\Windows\Framework\Images\sign_warning_16.png":U) .

        RETURN oValueList . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a ValueList to visualize the UI translation type 
        Notes:   
        @return The ValueList with the possible UI translation types
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC ValueList GetTranslationTypeValueList ():
        
        DEFINE VARIABLE oValueList        AS ValueList         NO-UNDO . 

        oValueList = NEW ValueList () .
        
        oValueList:DisplayStyle = ValueListDisplayStyle:DisplayTextAndPicture .

        ValueLists:AddValueListItem (oValueList,
                                     BOX ("1":U),
                                     "Local translation by key"{&TRAN},
                                     "Consultingwerk\Windows\Framework\Images\SmartFramework\window_application_edit_16.png":U) .

        ValueLists:AddValueListItem (oValueList,
                                     BOX ("2":U),
                                     "Local translation by partial key and original text"{&TRAN},
                                     "Consultingwerk\Windows\Framework\Images\SmartFramework\window_application_edit_16.png":U) .

        ValueLists:AddValueListItem (oValueList,
                                     BOX ("3":U),
                                     "Local translation by original text"{&TRAN},
                                     "Consultingwerk\Windows\Framework\Images\SmartFramework\window_application_edit_16.png":U) .

        ValueLists:AddValueListItem (oValueList,
                                     BOX ("4":U),
                                     "Global by resource key"{&TRAN},
                                     "Consultingwerk\Windows\Framework\Images\SmartFramework\key_16.png":U) .

        ValueLists:AddValueListItem (oValueList,
                                     BOX ("5":U),
                                     "Global by partial key and original text"{&TRAN},
                                     "Consultingwerk\Windows\Framework\Images\SmartFramework\key_edit_16.png":U) .

        ValueLists:AddValueListItem (oValueList,
                                     BOX ("6":U),
                                     "Global by original text"{&TRAN},
                                     "Consultingwerk\Windows\Framework\Images\SmartFramework\pencil_16.png":U) .

        RETURN oValueList . 

	END METHOD .

END CLASS.

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
    File        : WidgetAttributes
    Purpose     : Can verify widget attribute names
    Syntax      : 
    Description : Useful as the LIST-QUERY-ATTRS ABL function does return
                  attribute names and methods. This class provides a way
                  to verify if an attribute name is valid.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 13 20:13:17 CEST 2015
    Notes       : Relies in widgetattributes.xml created by 
                  Consultingwerk.Studio.OpenEdgeDocs.ParseIndexXmlForAttributes
                  Used by WidgetHelper:ShowWidgetProperties and LogManager:WriteWidgetProperties
                  to avoid calling into GET-ATTR-CALL-TYPE type calls for widget
                  methods (as the LIST-QUERY-ATTRS does not allow to distinguish)
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Consultingwerk.*                       FROM PROPATH .  
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.WidgetAttributes: 

    DEFINE STATIC TEMP-TABLE ttWidgetAttributes NO-UNDO 
        FIELD AttributeName AS CHARACTER
        INDEX AttributeName IS UNIQUE PRIMARY AttributeName . 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the WidgetAttributes class
        Notes:   Disallow instance creation   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE WidgetAttributes ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the WidgetAttributes class
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR STATIC WidgetAttributes ():
        
        IF FileHelper:Exists ("Consultingwerk/widgetattributes.xml":U) THEN 
            TEMP-TABLE ttWidgetAttributes:READ-XML ("file":U, FileHelper:FindFile ("Consultingwerk/widgetattributes.xml":U), ?, ?, ?) .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Verifies that an Attribute name is valid
        Notes:   
        @param pcAttributeName The name of the attribute to verify
        @return Logical value indicating that the attribute name is valid
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL IsValid (pcAttributeName AS CHARACTER):
        
        RETURN CAN-FIND (ttWidgetAttributes WHERE ttWidgetAttributes.AttributeName = pcAttributeName) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a List of all widget attributes
        Notes:   
        @return The CharacterList of all widget attributes 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CharacterList ToList ():
        
        DEFINE VARIABLE oList AS CharacterList NO-UNDO.

        oList = NEW CharacterList () . 
        
        FOR EACH ttWidgetAttributes:
            oList:Add (ttWidgetAttributes.AttributeName) . 
        END.

        RETURN oList .

    END METHOD .

END CLASS.

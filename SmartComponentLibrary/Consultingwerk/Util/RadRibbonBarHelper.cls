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
    File        : RadRibbonBarHelper
    Purpose     : Helper methods for Telerik RadRibbonBar
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat May 02 17:31:20 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.Collections.* FROM PROPATH.
USING Consultingwerk.Util.*                  FROM PROPATH .  
USING Progress.Lang.*                        FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 AND  DEFINED (TelerikRadControls) NE 0 &THEN
USING Telerik.WinControls.*                  FROM ASSEMBLY .
USING Telerik.WinControls.UI.*               FROM ASSEMBLY.
&ENDIF

CLASS Consultingwerk.Util.RadRibbonBarHelper: 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RadRibbonBarHelper class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RadRibbonBarHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 AND  DEFINED (TelerikRadControls) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Returns a List of all RadItems in the RadRibbonBar with the given Tag
        Notes:
        @param poRibbonBar The reference to the RadRibbonBar
        @param pcTag The Tag value to search for
        @return The List of RadItems with the given Tag
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC "System.Collections.Generic.List<RadItem>" GetAllItemsWithTag (poRibbonBar AS RadRibbonBar,
                                                                                        pcTag AS CHARACTER):
        
        DEFINE VARIABLE oList AS "System.Collections.Generic.List<RadItem>" NO-UNDO . 

        oList = NEW "System.Collections.Generic.List<RadItem>" () .
        
        {Consultingwerk/foreach.i RibbonTab oTab in poRibbonBar:CommandTabs}
        
            RadRibbonBarHelper:GetAllItemsWithTag (oTab,
                                                   oList,
                                                   pcTag) .
        END.
        
        RETURN oList . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds all RadItems in the RibbonTab with the given Tag to the list
        Notes:
        @param poRibbonTab The reference to the RibbonTab
        @param poList The List to add the items to
        @param pcTag The Tag value to search for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GetAllItemsWithTag (poRibbonTab AS RibbonTab,
                                                  poList as "System.Collections.Generic.List<RadItem>",
                                                  pcTag AS CHARACTER):

        {Consultingwerk/foreach.i RadItem oItem in poRibbonTab:Items}

            IF RadRibbonBarHelper:TagIsInList (oItem:Tag, pcTag) THEN 
                poList:Add (oItem) .      
        
            IF TYPE-OF (oItem, IItemsElement) THEN          
                RadRibbonBarHelper:GetAllItemsWithTag (CAST (oItem, IItemsElement),
                                                       poList,
                                                       pcTag) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a List of all RadItems in the IITemsElement with the given Tag
        Notes:
        @param poItem The reference to the current item owner
        @param poList The List to add the items to
        @param pcTag The Tag value to search for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GetAllItemsWithTag (poItem AS IItemsElement,
                                                  poList as "System.Collections.Generic.List<RadItem>",
                                                  pcTag AS CHARACTER):

        {Consultingwerk/foreach.i RadItem oItem in poItem:Items}
        
            IF RadRibbonBarHelper:TagIsInList (oItem:Tag, pcTag) THEN 
                poList:Add (oItem) .      
        
            IF TYPE-OF (oItem, IItemsElement) THEN          
                RadRibbonBarHelper:GetAllItemsWithTag (CAST (oItem, IItemsElement),
                                                       poList,
                                                       pcTag) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns a CharacterList of Tag values in the RadRibbonBar 
        Notes:
        @param poRibbonBar The reference to the RadRibbonBar
        @return The List of RadItems with the given Tag
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CharacterList GetAllTagValues (poRibbonBar AS RadRibbonBar):
        
        DEFINE VARIABLE oList AS CharacterList NO-UNDO . 

        oList = NEW CharacterList () .
        
        {Consultingwerk/foreach.i RibbonTab oTab in poRibbonBar:CommandTabs}
        
            RadRibbonBarHelper:GetAllTagValues (oTab,
                                                oList) .
        END.
        
        RETURN oList . 

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Adds all Tag values in the RibbonTab to the list
        Notes:
        @param poRibbonTab The reference to the RibbonTab
        @param poList The List to add the items to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GetAllTagValues (poRibbonTab AS RibbonTab,
                                               poList as CharacterList):

        {Consultingwerk/foreach.i RadItem oItem in poRibbonTab:Items}
        
            IF VALID-OBJECT (oItem:Tag) AND TYPE-OF (oItem:Tag, System.String) AND 
               UNBOX (oItem:Tag) > "":U AND 
               NOT poList:ContainsValue (UNBOX (oItem:Tag)) THEN 
                
                poList:Add (UNBOX (oItem:Tag)) .
        
            IF TYPE-OF (oItem, IItemsElement) THEN          
                RadRibbonBarHelper:GetAllTagValues (CAST (oItem, IItemsElement),
                                                    poList) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds all Tag values in the IItemsElement to the list
        Notes:
        @param poItem The reference to the current item owner
        @param poList The List to add the items to
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID GetAllTagValues (poItem AS IItemsElement,
                                               poList as CharacterList):

        {Consultingwerk/foreach.i RadItem oItem in poItem:Items}
        
            IF VALID-OBJECT (oItem:Tag) AND TYPE-OF (oItem:Tag, System.String) AND 
               UNBOX (oItem:Tag) > "":U AND 
               NOT poList:ContainsValue (UNBOX (oItem:Tag)) THEN 
                
                poList:Add (UNBOX (oItem:Tag)) .
        
            IF TYPE-OF (oItem, IItemsElement) THEN          
                RadRibbonBarHelper:GetAllTagValues (CAST (oItem, IItemsElement),
                                                    poList) .
        END.

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Returns if the provided Tag (System.Object) is in the given 
                 list of tags
        Notes:
        @param poTag The reference to the current Tag value
        @param pcTag The CHARACTER value to compare
        @return Logical value indicating if the two values are equal
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL TagIsInList (poTag AS System.Object,
                                              pcTag AS CHARACTER):
        
        IF NOT VALID-OBJECT (poTag) THEN 
            RETURN FALSE . 
            
        IF NOT TYPE-OF (poTag, System.String) THEN 
            RETURN FALSE . 
        
        RETURN LOOKUP (UNBOX (poTag), pcTag) > 0 .

    END METHOD.
&ENDIF

END CLASS.

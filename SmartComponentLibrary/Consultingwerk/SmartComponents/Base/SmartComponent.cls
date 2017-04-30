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
    File        : SmartComponent
    Purpose     : Base class for custom Component's in the SmartComponent
                  Library Framework.
    Syntax      :
    Description : The SmartComponent inherits from the
                  Consultingwerk.SmartComponents.ComponentBase class. This
                  base class defines the "Genius" icon for the toolbox palette
                  and makes sure that the Component get's added to the
                  component container (.NET Visual Designer)
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue May 26 22:42:30 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Framework.*                    FROM PROPATH .
USING Consultingwerk.SmartComponents.*              FROM ASSEMBLY .
USING Consultingwerk.SmartComponents.ComponentBase  FROM ASSEMBLY .
USING System.ComponentModel.*                       FROM ASSEMBLY .
USING System.ComponentModel.Design.*                FROM ASSEMBLY .
USING Progress.Lang.*                               FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Base.SmartComponent
    INHERITS ComponentBase
    IMPLEMENTS ICustomTypeDescriptor,
               IHasDesignerVerbs
    USE-WIDGET-POOL:

    DEFINE PRIVATE VARIABLE oDesignerVerbHelper AS Consultingwerk.SmartComponents.DesignerVerbHelper NO-UNDO .

    /* Mike Fechner, Consultingwerk Ltd. 22.04.2010
       Core implementation of ICustomTypeDescriptor */
    {Consultingwerk/SmartComponents/Base/ICustomTypeDescriptor.i}

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to generate Designer Verbs (actions for the
                 Visual Designer)
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY DesignerVerbs AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 will be excluded from the .NET TypeDescriptor properties list
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY HiddenProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Property that is used to mark properties (comma-delimited list) that
                 should not show up in the Visual Designers Property Grid
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY NonBrowsableProperties AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Propery returns if the component's DesignerHost is currently
                 loading the document
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PROTECTED PROPERTY DesingerHostLoading AS LOGICAL NO-UNDO
    GET:
        IF VALID-OBJECT (THIS-OBJECT:Container) AND TYPE-OF (THIS-OBJECT:Container, IDesignerHost) THEN
            RETURN CAST (THIS-OBJECT:Container, IDesignerHost):Loading .
        ELSE
            RETURN FALSE .

    END GET .

    /*------------------------------------------------------------------------------
        Purpose: Propery flags if the current instance of the SmartBindingSource
                 is running at design time or runtime.
        Notes:   Set during constructor using a check at the
                 System.ComponentModel.LicenseManager
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DesignTime AS LOGICAL NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartComponent class. Set's the
                 DesignTime property using the LicenseManager.
        Notes:   According to
                 http://dotnetfacts.blogspot.com/2009/01/identifying-run-time-and-design-mode.html
                 that is only possible during the constructor of the class.
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartComponent ( ):
        SUPER ().

        THIS-OBJECT:DesignTime = Progress.Util.EnumHelper:AreEqual(System.ComponentModel.LicenseManager:UsageMode,
                                                                   System.ComponentModel.LicenseUsageMode:Designtime) .

        /* Mike Fechner, Consultingwerk Ltd. 27.10.2011
           Design Time override - allow full functionality temporarily in a custom Visual Designer */
        IF THIS-OBJECT:DesignTime = TRUE AND
           FrameworkSettings:AllowDesignFeatures = TRUE THEN

           THIS-OBJECT:DesignTime = FALSE .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartComponent class. Set's the
                 DesignTime property using the LicenseManager.
        Notes:   According to
                 http://dotnetfacts.blogspot.com/2009/01/identifying-run-time-and-design-mode.html
                 that is only possible during the constructor of the class.
        @param poContainer The IContainer to add the current BindingSource to.
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartComponent (poContainer AS CLASS System.ComponentModel.IContainer):

        SUPER (poContainer) .

        THIS-OBJECT:DesignTime = Progress.Util.EnumHelper:AreEqual(System.ComponentModel.LicenseManager:UsageMode,
                                                                   System.ComponentModel.LicenseUsageMode:Designtime) .

        /* Mike Fechner, Consultingwerk Ltd. 27.10.2011
           Design Time override - allow full functionality temporarily in a custom Visual Designer */
        IF THIS-OBJECT:DesignTime = TRUE AND
           FrameworkSettings:AllowDesignFeatures = TRUE THEN

           THIS-OBJECT:DesignTime = FALSE .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Creates Designer Verbs based on the DesignerVerbs property
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateVerbs ():

        DEFINE VARIABLE oHost     AS System.ComponentModel.Design.IDesignerHost NO-UNDO .
        DEFINE VARIABLE oDesigner AS System.ComponentModel.Design.IDesigner     NO-UNDO .

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        IF THIS-OBJECT:DesignTime AND THIS-OBJECT:DesignerVerbs > "":U THEN DO:

            IF VALID-OBJECT (THIS-OBJECT:Site) THEN DO:

                IF THIS-OBJECT:DesignTime AND NOT VALID-OBJECT (oDesignerVerbHelper) THEN
                    oDesignerVerbHelper = NEW Consultingwerk.SmartComponents.DesignerVerbHelper (THIS-OBJECT) .

                ASSIGN oHost = CAST (THIS-OBJECT:Site:GetService (Progress.Util.TypeHelper:GetType ("System.ComponentModel.Design.IDesignerHost":U)),
                                    System.ComponentModel.Design.IDesignerHost).

                IF VALID-OBJECT (oHost) THEN DO:

                    oDesigner = oHost:GetDesigner (THIS-OBJECT) .

                    IF VALID-OBJECT (oDesigner) AND VALID-OBJECT (oDesigner:Verbs) THEN DO:
                        DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:DesignerVerbs):
                            oDesigner:Verbs:Add (oDesignerVerbHelper:CreateDesignerVerb (ENTRY(i, THIS-OBJECT:DesignerVerbs))) .
                        END.
                    END.
                END.
            END .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event Handler method for Designer Verbs
        Notes:   This method is intended to be overridden
        @param pcVerbText The label of the verb the developer clicked on in the Visual Designer
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OnVerbClicked (pcVerbText AS CHARACTER):

        /* noop */

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartComponent class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartComponent ():

        IF VALID-OBJECT (oDesignerVerbHelper) THEN
            DELETE OBJECT oDesignerVerbHelper .

    END DESTRUCTOR.

END CLASS.

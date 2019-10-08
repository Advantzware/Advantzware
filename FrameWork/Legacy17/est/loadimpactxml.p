/* ArdenXml_Sax.p  Import data from Impact CAD XML
   1-Layouts with One-Up Insertions         : Layout->Form->Design
   2-One-Ups and Layouts - No Insertions    : One_Ups-> Design
                                              Layout->Form
   3-One-Up Designs - No Layouts            : One_Ups->Design
   4-Only ACTIVE One-Up designs and Layouts :  same as #2
   
   
   ---------
   New <Impact XML tags>

ImpactProject -  ProjectInfo

                 Layout - FORM - design  - SINGLE Est : 1 Form/Design per Layout

                 Layout - FORM - Design  - Tandem : Multi Form with single Single Design per Layout
                          FORM - Design
                          
                 Layout - FORM - Design  - Combo
                               - Design
                               - Design


                 Layout - FORM - Design  - SET
                               - Design
                        - FORM - Design
                               - Design
Tables: ttXMLProjectInfo
        ttXmlLayouts
        ttXMLLayoutForm
        ttXmlLayouyFormDesign
        
   */
def input param ipXmlFile as cha no-undo.
def output param opProjectid as dec no-undo.

DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReadMode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL NO-UNDO.
DEFINE VARIABLE cFieldTypeMapping AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVerifySchemaMode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEncoding AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted AS LOGICAL NO-UNDO.
DEFINE VARIABLE lWriteSchema AS LOGICAL NO-UNDO.
DEFINE VARIABLE lMinSchema AS LOGICAL NO-UNDO.
DEFINE VARIABLE lWriteBeforeImage AS LOGICAL NO-UNDO.


DEF TEMP-TABLE ImpactProject like xmlProject
  .
DEF TEMP-TABLE ProjectInfo
    FIELD ImpactProject_Id AS RECID /*XML-NODE-TYPE "HIDDEN"*/
    Field ProjectName as cha 
    Field ProjectCode as cha 
    Field ProjectRevision as cha 
    Field ProjectDesc as cha 
    Field ProjectSetPartNumber as cha 
    Field ProjectCustomer as cha 
    Field ProjectCustomerContactName as cha 
    Field ProjectMarketCategory as cha 
    Field ProjectCreatedDate as cha 
    Field ProjectCreatedBy as cha 
    Field ProjectModifiedDate as cha 
    Field ProjectModifiedBy as cha 
    INDEX ID AS PRIMARY
        ImpactProject_id.

DEF TEMP-TABLE Layouts
    FIELD ProjectCode AS cha
    FIELD Layouts_id AS RECID /*XML-NODE-TYPE "HIDDEN"  */
    INDEX ID AS PRIMARY
        Layouts_id
    .
               
DEF TEMP-TABLE LayoutsForm
    FIELD ProjectCode AS cha /*XML-NODE-TYPE "HIDDEN"*/
    FIELD Layouts_id AS RECID /*XML-NODE-TYPE "HIDDEN"*/
    FIELD LayoutsForm_id AS recid /*XML-NODE-TYPE "HIDDEN"*/
    Field LayoutName as cha 
    Field LayoutLayerStatus as cha 
    Field LayoutLayerVersion as cha 
    Field LayoutNumber as cha 
    Field LayoutFluteGrainDirection as cha 
    Field LayoutMachineDirection as cha 
    Field LayoutNumberUp as cha 
    Field LayoutGrossStockSheetX as cha 
    Field LayoutGrossStockSheetY as cha 
    Field LayoutNetFittedSheetX as cha 
    Field LayoutNetFittedSheetY as cha 
    Field LayoutDieKnifeToKnifeX as cha 
    Field LayoutDieKnifeToKnifeY as cha 
    Field LayoutTotalInches as cha 
    Field LayoutImageName as cha 
    INDEX ID AS PRIMARY
        Layouts_id
        LayoutsForm_id
        .

DEF TEMP-TABLE LayoutsFormDesign
    FIELD ProjectCode AS cha /*XML-NODE-TYPE "HIDDEN"*/
    FIELD Layouts_id AS RECID /*XML-NODE-TYPE "HIDDEN"*/
    FIELD LayoutsForm_id AS RECID /*XML-NODE-TYPE "HIDDEN"*/
    FIELD LayoutFormDesign_id AS recid /*XML-NODE-TYPE "HIDDEN"*/
    Field LayoutName as cha 
    Field OneUpLayerName as cha 
    Field OneUpLayerStatus as cha 
    Field OneUpLayerVersion as cha 
    Field OneUpLayerDescription as cha 
    Field OneUpLayerMaterialName as cha 
    Field OneUpFluteGrainDirection as cha 
    Field OneUpMachineDirection as cha 
    Field OneUpDesignNumber as cha 
    Field OneUpItemDescription as cha 
    Field OneUpBoxLength as cha 
    Field OneUpBoxWidth as cha 
    Field OneUpBoxDepth as cha 
    Field OneUpBoxStyle as cha 
    Field OneUpBoxJoint as cha 
    Field OneUpBoardGrade as cha 
    Field OneUpBlankX as cha 
    Field OneUpBlankY as cha 
    Field OneUpComponentSetNumber as cha 
    Field OneUpQtyPerSet as cha 
    Field OneUpNumberUpX as cha 
    Field OneUpNumberUpY as cha 
    Field OneUpPanelScoring_X1 as cha 
    Field OneUpPanelScoring_X2 as cha 
    Field OneUpPanelScoring_X3 as cha 
    Field OneUpPanelScoring_X4 as cha 
    Field OneUpPanelScoring_X5 as cha 
    Field OneUpPanelScoring_X6 as cha 
    Field OneUpPanelScoring_X7 as cha 
    Field OneUpPanelScoring_X8 as cha 
    Field OneUpPanelScoring_X9 as cha 
    Field OneUpPanelScoring_X10 as cha 
    Field OneUpPanelScoring_X11 as cha 
    Field OneUpPanelScoring_X12 as cha 
    Field OneUpPanelScoring_X13 as cha 
    Field OneUpPanelScoring_X14 as cha 
    Field OneUpPanelScoring_X15 as cha 
    Field OneUpPanelScoring_X16 as cha 
    Field OneUpPanelScoring_X17 as cha 
    Field OneUpPanelScoring_X18 as cha 
    Field OneUpPanelScoring_X19 as cha 
    Field OneUpPanelScoring_X20 as cha 
    Field OneUpPanelScoring_X21 as cha 
    Field OneUpPanelScoring_X22 as cha 
    Field OneUpPanelScoring_X23 as cha 
    Field OneUpPanelScoring_X24 as cha 
    Field OneUpPanelScoring_Y1 as cha 
    Field OneUpPanelScoring_Y2 as cha 
    Field OneUpPanelScoring_Y3 as cha 
    Field OneUpPanelScoring_Y4 as cha 
    Field OneUpPanelScoring_Y5 as cha 
    Field OneUpPanelScoring_Y6 as cha 
    Field OneUpPanelScoring_Y7 as cha 
    Field OneUpPanelScoring_Y8 as cha 
    Field OneUpPanelScoring_Y9 as cha 
    Field OneUpPanelScoring_Y10 as cha 
    Field OneUpPanelScoring_Y11 as cha 
    Field OneUpPanelScoring_Y12 as cha 
    Field OneUpPanelScoring_Y13 as cha 
    Field OneUpPanelScoring_Y14 as cha 
    Field OneUpPanelScoring_Y15 as cha 
    Field OneUpPanelScoring_Y16 as cha 
    Field OneUpPanelScoring_Y17 as cha 
    Field OneUpPanelScoring_Y18 as cha 
    Field OneUpPanelScoring_Y19 as cha 
    Field OneUpPanelScoring_Y20 as cha 
    Field OneUpPanelScoring_Y21 as cha 
    Field OneUpPanelScoring_Y22 as cha 
    Field OneUpPanelScoring_Y23 as cha 
    Field OneUpPanelScoring_Y24 as cha 
    Field OneUpTotalInches as cha 
    Field OneUpImageName as cha 
    INDEX ID AS PRIMARY
        Layouts_id
        LayoutsForm_id 
        .


DEFINE DATASET dsImpactProject FOR  ImpactProject, ProjectInfo,Layouts, LayoutsForm, LayoutsFormDesign
    /*DATA-RELATION r1 FOR ProjectInfo, Layouts RELATION-FIELDS (ProjectCode, ProjectCode) NESTED*/
    DATA-RELATION r1 FOR Layouts, LayoutsForm RELATION-FIELDS (Layouts_id, Layouts_id) NESTED
    DATA-RELATION r1 FOR LayoutsForm, LayoutsFormDesign RELATION-FIELDS (LayoutsForm_id, LayoutsForm_id) NESTED
    .

    DEFINE VARIABLE hPDS AS HANDLE NO-UNDO.
    DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO. 

    hPDS = DATASET dsImpactProject:HANDLE.

    ASSIGN cSourceType = "FILE"
            /*cFile = "C:\temp\ardenxml\P-ASN-000177_0_Rev.xml"  Single Est Xml*/
            /*cFile = "C:\temp\ardenxml\Example Tandem Job_0_Rev.xml" */
            /*cFile = "C:\temp\ardenxml\FC Combo Die_0_Rev.xml" */
            /*cFile = "C:\temp\ardenxml\Display Set_0_Rev.xml"  */
            /*=========*/
            cFile = ipXmlFile /*"C:\ASI\FOL TEST - DO NOT USE_2.xml" */
            /*cFile = "C:\temp\ardenxml\newxml\Example Tandem Job_0.xml" */
            /*cFile = "C:\temp\ardenxml\newxml\FC Combo Die_1.xml"*/
            /*cFile = "C:\temp\ardenxml\newxml\Display Set_0.xml"  */
              
            cReadMode = "EMPTY"
            cSchemaLocation = ?
            lOverrideDefaultMapping = NO.


    lReturn = hPDS:READ-XML (cSourceType, cFile, cReadMode, cSchemaLocation, lOverrideDefaultMapping).

IF lReturn THEN DO:

   /*
   FOR EACH Impactproject.
        DISP impactproject WITH TITLE "ImpactProject".
   END.


   FOR EACH ProjectInfo:
       DISP ProjectInfo WITH 2 COL TITLE "ProjectInfo".
   END.
   

   FOR EACH layouts:
       DISP recid(layouts) layouts.layouts_id  WITH TITLE "layouts".
   END.
   
   FOR EACH LayoutsForm:
       DISP LayoutsForm.layoutname WITH TITLE "Form".
   END.

   FOR EACH LayoutsFormDesign:
       DISP LayoutsFormDesign.oneuplayername WITH TITLE "Design".
   
   END.
   */
   /*
   FOR EACH layouts,
       EACH layoutsForm WHERE LayoutsForm.Layouts_id = Layouts.Layouts_id:
       DISP layoutsForm.layouts_id LayoutsForm.layoutname FORM "x(30)" WITH TITLE "Form".
       FOR EACH layoutsFormDesign WHERE layoutsFormDesign.LayoutsForm_id = LayoutsForm.LayoutsForm_id :
           DISP layoutsFormDesign.LayoutsForm_id LayoutsFormDesign.oneuplayername FORM "x(30)" WITH TITLE "Design".
       END.
   END.
   */
/*
   FOR EACH layouts,
       EACH layoutsForm WHERE LayoutsForm.Layouts_id = Layouts.Layouts_id:
       DISP layoutsForm.layouts_id LayoutsForm.layoutname FORM "x(30)" WITH TITLE "Form".
       FOR EACH layoutsFormDesign WHERE layoutsFormDesign.LayoutName = LayoutsForm.LayoutName :
           DISP layoutsFormDesign.LayoutsForm_id LayoutsFormDesign.oneuplayername FORM "x(30)" WITH TITLE "Design".
       END.
   END.
*/

   DEF VAR ldNextProjectId AS DEC NO-UNDO.

   /* build XML tables xmlProject, xmlProjectInfo, xmlLayouts, xmlLayoutForm, xmlLayoutFormDesign */
   FIND LAST xmlProjectInfo USE-INDEX id NO-LOCK NO-ERROR.
   ldNextProjectId = IF AVAIL xmlProjectInfo THEN xmlProjectInfo.projectID + 1 ELSE 10001.
   
   FOR EACH ProjectInfo:
       
   
       CREATE xmlProjectInfo.
           BUFFER-COPY projectInfo TO xmlProjectInfo.
           ASSIGN xmlProjectInfo.projectId = ldNextProjectId.

       CREATE xmlProject.
       aSSIGN xmlProject.projectID = ldNextProjectId.
              
       FOR EACH layouts:
           CREATE xmlLayouts.
           BUFFER-COPY layouts TO xmlLayouts.
           ASSIGN xmllayOuts.projectid = ldNextProjectId.

           FOR EACH layoutsForm WHERE LayoutsForm.Layouts_id = Layouts.Layouts_id:
               CREATE xmlLayoutsForm.
               BUFFER-COPY layoutsForm TO xmlLayoutsForm.
               ASSIGN xmlLayoutsForm.projectid = ldNextProjectId.
           
               FOR EACH layoutsFormDesign WHERE layoutsFormDesign.LayoutName = LayoutsForm.LayoutName :
                 CREATE xmlLayoutsFormDesign.
                 BUFFER-COPY layoutsFormDesign TO xmlLayoutsFormDesign.
                 ASSIGN xmlLayoutsFormDesign.projectId = ldNextProjectId.
               END.
           END.


       END.
       opProjectId = ldNextProjectId.
       
       ldNextProjectId = ldNextProjectId + 1.


   END.


END.


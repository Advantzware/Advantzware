/*------------------------------------------------------------------------
    File        : FoldInks.p
    Purpose     : Corrugated Box

    Syntax      :

    Description : Return a Dataset of all Corrugated Box

    Author(s)   : 
    Created     : 24 feb 2009 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  DEFinitions  ************************** */
DEFINE TEMP-TABLE ttFoldInks NO-UNDO
    FIELD  vEstNum     AS CHAR 
    FIELD  vCustPart   AS CHAR
    FIELD  vEstDate    AS DATETIME    
    FIELD  vFormNo     AS INT     
    FIELD  vFormQty    AS INT     
    FIELD  vBlankNo    AS INT      
    FIELD  vBlankQty   AS INT

    FIELD  vColor      AS INT     
    FIELD  vPasses     AS INT     
    FIELD  vCoat       AS INT     
    FIELD  vCoatPass   AS INT 
    FIELD  vDscr       AS CHAR
    FIELD  vPs1        AS INT 
    FIELD  vPs2        AS INT
    FIELD  vPs3        AS INT 
    FIELD  vPs4        AS INT
    FIELD  vPs5        AS INT 
    FIELD  vPs6        AS INT
    FIELD  vPs7        AS INT 
    FIELD  vPs8        AS INT
    FIELD  vPs9        AS INT 
    FIELD  vPs10        AS INT
    FIELD  vPs11        AS INT 
    FIELD  vPs12        AS INT
    FIELD  vPs13        AS INT 
    FIELD  vPs14        AS INT
    FIELD  vPs15        AS INT 

    FIELD  vCode1       AS CHAR
    FIELD  vCode2       AS CHAR
    FIELD  vCode3       AS CHAR
    FIELD  vCode4       AS CHAR
    FIELD  vCode5       AS CHAR
    FIELD  vCode6       AS CHAR
    FIELD  vCode7       AS CHAR
    FIELD  vCode8       AS CHAR
    FIELD  vCode9       AS CHAR
    FIELD  vCode10      AS CHAR
    FIELD  vCode11      AS CHAR
    FIELD  vCode12      AS CHAR
    FIELD  vCode13      AS CHAR
    FIELD  vCode14      AS CHAR
    FIELD  vCode15      AS CHAR

    FIELD  vDscr1       AS CHARACTER
    FIELD  vDscr2       AS CHARACTER
    FIELD  vDscr3       AS CHARACTER
    FIELD  vDscr4       AS CHARACTER
    FIELD  vDscr5       AS CHARACTER
    FIELD  vDscr6       AS CHARACTER
    FIELD  vDscr7       AS CHARACTER
    FIELD  vDscr8       AS CHARACTER
    FIELD  vDscr9       AS CHARACTER
    FIELD  vDscr10      AS CHARACTER
    FIELD  vDscr11      AS CHARACTER
    FIELD  vDscr12      AS CHARACTER
    FIELD  vDscr13      AS CHARACTER
    FIELD  vDscr14      AS CHARACTER
    FIELD  vDscr15      AS CHARACTER

    FIELD  vPer1        AS INT
    FIELD  vPer2        AS INT
    FIELD  vPer3        AS INT
    FIELD  vPer4        AS INT
    FIELD  vPer5        AS INT
    FIELD  vPer6        AS INT
    FIELD  vPer7        AS INT
    FIELD  vPer8        AS INT
    FIELD  vPer9        AS INT
    FIELD  vPer10       AS INT
    FIELD  vPer11       AS INT
    FIELD  vPer12       AS INT
    FIELD  vPer13       AS INT
    FIELD  vPer14       AS INT
    FIELD  vPer15       AS INT
    FIELD  vUnit1       AS DECIMAL
    FIELD  vUnit2       AS DECIMAL
    FIELD  vUnit3       AS DECIMAL
    FIELD  vUnit4       AS DECIMAL
    FIELD  vUnit5       AS DECIMAL
    FIELD  vUnit6       AS DECIMAL
    FIELD  vUnit7       AS DECIMAL
    FIELD  vUnit8       AS DECIMAL
    FIELD  vUnit9       AS DECIMAL
    FIELD  vUnit10      AS DECIMAL
    FIELD  vUnit11      AS DECIMAL
    FIELD  vUnit12      AS DECIMAL
    FIELD  vUnit13      AS DECIMAL
    FIELD  vUnit14      AS DECIMAL
    FIELD  vUnit15      AS DECIMAL

    FIELD  vPackCode    AS CHAR 
    FIELD  vUnitLen     AS DECIMAL
    FIELD  vUnitWid     AS DECIMAL
    FIELD  vUnitDep     AS DECIMAL 
    FIELD  vLayerPad    AS CHAR
    FIELD  vLayerLen    AS DECIMAL
    FIELD  vLayerWid    AS DECIMAL
    FIELD  vLayerDep    AS DECIMAL
    FIELD  vDel         AS CHAR 
    FIELD  vDelLen      AS DECIMAL
    FIELD  vDelWid      AS DECIMAL
    FIELD  vDelDep      AS DECIMAL
    FIELD  vPackQty     AS DECIMAL
    FIELD  vLayerQty    AS DECIMAL
    FIELD  vDelQty      AS DECIMAL
    
    FIELD  vCost        AS DECIMAL 
    FIELD  vBoxCode     AS INTEGER 
    FIELD  vPallet       AS INTEGER
    FIELD  vWtUnit      AS DECIMAL     
    FIELD  vUnit        AS CHAR  
    FIELD  vLength      AS DEC       
    FIELD  vWidth       AS DEC     
    FIELD  vHeight      AS DEC     
    FIELD  vCost2       AS DECIMAL     
    FIELD  vCount       AS INT     
    FIELD  vLayer       AS INT 

    FIELD  vNote        AS CHAR    
    FIELD  vFrCharge    AS CHAR     
    FIELD  vWeiPer      AS DEC
    FIELD  vCarrier     AS CHARACTER
    FIELD  vCarrDscr    AS CHARACTER
    FIELD  vDelZon      AS CHARACTER 
    FIELD  vFreifgt     AS DEC   
    FIELD  vFreOut      AS DEC
    FIELD  vStock       AS CHAR

    FIELD  vSide1       AS CHARACTER    
    FIELD  vSide2       AS CHARACTER    
    FIELD  vSide3       AS CHARACTER    
    FIELD  vSide4       AS CHARACTER    
    FIELD  vSide5       AS CHARACTER    
    FIELD  vSide6       AS CHARACTER    
    FIELD  vSide7       AS CHARACTER    
    FIELD  vSide8       AS CHARACTER    
    FIELD  vSide9       AS CHARACTER    
    FIELD  vSide10      AS CHARACTER    
    FIELD  vSide11      AS CHARACTER    
    FIELD  vSide12      AS CHARACTER    
    FIELD  vSide13      AS CHARACTER    
    FIELD  vSide14      AS CHARACTER    
    FIELD  vSide15      AS CHARACTER    
    FIELD  order       AS CHAR

    .
DEFINE DATASET dsFoldInks FOR ttFoldInks.

DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstNum      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFormNo      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmColor       AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmPass        AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmCoat        AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmCoatPass    AS INT NO-UNDO. 
DEFINE INPUT PARAMETER prmDscr        AS CHAR NO-UNDO. 

DEFINE INPUT PARAMETER prmPs1         AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmPs2         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs3         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs4         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs5         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs6         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs7         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs8         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs9         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs10        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs11        AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmPs12        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs13        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs14        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPs15        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmCode1       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmCode2       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode3       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode4       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode5       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode6       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode7       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode8       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode9       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode10      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode11      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmCode12      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode13      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode14      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCode15      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr1       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmDscr2       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr3       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr4       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr5       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr6       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr7       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr8       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr9       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr10      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr11      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmDscr12      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr13      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr14      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr15      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmPer1        AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmPer2        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer3        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer4        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer5        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer6        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer7        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer8        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer9        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer10       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer11       AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmPer12       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer13       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer14       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPer15       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmUnit1       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnit2       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnit3       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnit4       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnit5       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnit6       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnit7       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnit8       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnit9       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnit10      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnit11      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnit12      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnit13      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnit14      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnit15      AS DECIMAL NO-UNDO.

DEFINE INPUT PARAMETER prmPackCode    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmUnitLen     AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnitWid     AS DECIMAL NO-UNDO. 
DEFINE INPUT PARAMETER prmUnitDep     AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmLayerPad    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmLayerLen    AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmLayerWid    AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmLayerDep    AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmDivider     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDividerLen  AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmDividerWid  AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmDividerDep  AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmCostEa      AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmBoxCode     AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmPallet      AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmWTUnit      AS DECIMAL NO-UNDO. 
DEFINE INPUT PARAMETER prmPackQty     AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmLayerQty    AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmDivQty      AS DECIMAL NO-UNDO.

DEFINE INPUT PARAMETER prmUnit        AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmCost2       AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmCount       AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmLength      AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmWidth       AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmHeight      AS DEC NO-UNDO. 
DEFINE INPUT PARAMETER prmLayer       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmNote        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFrCharge    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmWeightPer   AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmCarrier     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmCarrDscr    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmDelZon      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmFreight     AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmFreight2    AS DEC NO-UNDO.  
DEFINE INPUT PARAMETER prmBlankno     AS INT NO-UNDO.

DEFINE INPUT PARAMETER prmSide1       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSide2       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSide3       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSide4       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSide5       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSide6       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSide7       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSide8       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSide9       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSide10      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSide11      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSide12      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSide13      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSide14      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSide15      AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFoldInks.
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

IF prmUser        = ?  THEN ASSIGN    prmUser         = "".
IF prmAction      = ?  THEN ASSIGN    prmAction       = "".
IF prmComp        = ?  THEN ASSIGN    prmComp         = "".
IF prmEstNum      = ?  THEN ASSIGN    prmEstNum       = "".
IF prmFormNo      = ?  THEN ASSIGN    prmFormNo       = 0.
IF prmColor       = ?  THEN ASSIGN    prmColor        = 0.
IF prmPass        = ?  THEN ASSIGN    prmPass         = 0.
IF prmCoat        = ?  THEN ASSIGN    prmCoat         = 0.
IF prmCoatPass    = ?  THEN ASSIGN    prmCoatPass     = 0.      
IF prmDscr        = ?  THEN ASSIGN    prmDscr         = "".
                                                   
IF prmPs1         = ?  THEN ASSIGN    prmPs1          = 0.  
IF prmPs2         = ?  THEN ASSIGN    prmPs2          = 0.  
IF prmPs3         = ?  THEN ASSIGN    prmPs3          = 0.             
IF prmPs4         = ?  THEN ASSIGN    prmPs4          = 0.     
IF prmPs5         = ?  THEN ASSIGN    prmPs5          = 0. 
IF prmPs6         = ?  THEN ASSIGN    prmPs6          = 0.              
IF prmPs7         = ?  THEN ASSIGN    prmPs7          = 0.     
IF prmPs8         = ?  THEN ASSIGN    prmPs8          = 0. 
IF prmPs9         = ?  THEN ASSIGN    prmPs9          = 0. 
IF prmPs10        = ?  THEN ASSIGN    prmPs10         = 0.    
IF prmPs11        = ?  THEN ASSIGN    prmPs11         = 0.  
IF prmPs12        = ?  THEN ASSIGN    prmPs12         = 0.  
IF prmPs13        = ?  THEN ASSIGN    prmPs13         = 0.  
IF prmPs14        = ?  THEN ASSIGN    prmPs14         = 0.  
IF prmPs15        = ?  THEN ASSIGN    prmPs15         = 0.  
IF prmCode1       = ?  THEN ASSIGN    prmCode1        = "".  
IF prmCode2       = ?  THEN ASSIGN    prmCode2        = "".  
IF prmCode3       = ?  THEN ASSIGN    prmCode3        = "".  
IF prmCode4       = ?  THEN ASSIGN    prmCode4        = "".  
IF prmCode5       = ?  THEN ASSIGN    prmCode5        = "".  
IF prmCode6       = ?  THEN ASSIGN    prmCode6        = "".  
IF prmCode7       = ?  THEN ASSIGN    prmCode7        = "".  
IF prmCode8       = ?  THEN ASSIGN    prmCode8        = "".  
IF prmCode9       = ?  THEN ASSIGN    prmCode9        = "".  
IF prmCode10      = ?  THEN ASSIGN    prmCode10       = "".  
IF prmCode11      = ?  THEN ASSIGN    prmCode11       = "".  
IF prmCode12      = ?  THEN ASSIGN    prmCode12       = "".
IF prmCode13      = ?  THEN ASSIGN    prmCode13       = "". 
IF prmCode14      = ?  THEN ASSIGN    prmCode14       = "". 
IF prmCode15      = ?  THEN ASSIGN    prmCode15       = "". 
IF prmDscr1       = ?  THEN ASSIGN    prmDscr1        = "".
IF prmDscr2       = ?  THEN ASSIGN    prmDscr2        = "".
IF prmDscr3       = ?  THEN ASSIGN    prmDscr3        = "".
IF prmDscr4       = ?  THEN ASSIGN    prmDscr4        = "".
IF prmDscr5       = ?  THEN ASSIGN    prmDscr5        = "".
IF prmDscr6       = ?  THEN ASSIGN    prmDscr6        = "".
IF prmDscr7       = ?  THEN ASSIGN    prmDscr7        = "".
IF prmDscr8       = ?  THEN ASSIGN    prmDscr8        = "".
IF prmDscr9       = ?  THEN ASSIGN    prmDscr9        = "".
IF prmDscr10      = ?  THEN ASSIGN    prmDscr10       = "".                
IF prmDscr11      = ?  THEN ASSIGN    prmDscr11       = "".
IF prmDscr12      = ?  THEN ASSIGN    prmDscr12       = "".
IF prmDscr13      = ?  THEN ASSIGN    prmDscr13       = "".
IF prmDscr14      = ?  THEN ASSIGN    prmDscr14       = "".
IF prmDscr15      = ?  THEN ASSIGN    prmDscr15       = "".
IF prmPer1        = ?  THEN ASSIGN    prmPer1         =  0.
IF prmPer2        = ?  THEN ASSIGN    prmPer2         = 0.
IF prmPer3        = ?  THEN ASSIGN    prmPer3         = 0.
IF prmPer4        = ?  THEN ASSIGN    prmPer4         = 0.
IF prmPer5        = ?  THEN ASSIGN    prmPer5         = 0.
IF prmPer6        = ?  THEN ASSIGN    prmPer6         = 0.
IF prmPer7        = ?  THEN ASSIGN    prmPer7         = 0.
IF prmPer8        = ?  THEN ASSIGN    prmPer8         = 0.
IF prmPer9        = ?  THEN ASSIGN    prmPer9         = 0.
IF prmPer10       = ?  THEN ASSIGN    prmPer10        = 0.
IF prmPer11       = ?  THEN ASSIGN    prmPer11        = 0.
IF prmPer12       = ?  THEN ASSIGN    prmPer12        = 0.
IF prmPer13       = ?  THEN ASSIGN    prmPer13        = 0.
IF prmPer14       = ?  THEN ASSIGN    prmPer14        = 0.
IF prmPer15       = ?  THEN ASSIGN    prmPer15        = 0.
IF prmUnit1       = ?  THEN ASSIGN    prmUnit1        = 0.
IF prmUnit2       = ?  THEN ASSIGN    prmUnit2        = 0.
IF prmUnit3       = ?  THEN ASSIGN    prmUnit3        = 0.
IF prmUnit4       = ?  THEN ASSIGN    prmUnit4        = 0.
IF prmUnit5       = ?  THEN ASSIGN    prmUnit5        = 0.
IF prmUnit6       = ?  THEN ASSIGN    prmUnit6        = 0.
IF prmUnit7       = ?  THEN ASSIGN    prmUnit7        = 0. 
IF prmUnit8       = ?  THEN ASSIGN    prmUnit8        = 0. 
IF prmUnit9       = ?  THEN ASSIGN    prmUnit9        = 0. 
IF prmUnit10      = ?  THEN ASSIGN    prmUnit10       = 0. 
IF prmUnit11      = ?  THEN ASSIGN    prmUnit11       = 0. 
IF prmUnit12      = ?  THEN ASSIGN    prmUnit12       = 0. 
IF prmUnit13      = ?  THEN ASSIGN    prmUnit13       = 0. 
IF prmUnit14      = ?  THEN ASSIGN    prmUnit14       = 0. 
IF prmUnit15      = ?  THEN ASSIGN    prmUnit15       = 0. 
                                                      
IF prmPackCode    = ?  THEN ASSIGN    prmPackCode     = "". 
IF prmUnitLen     = ?  THEN ASSIGN    prmUnitLen      = 0. 
IF prmUnitWid     = ?  THEN ASSIGN    prmUnitWid      = 0. 
IF prmUnitDep     = ?  THEN ASSIGN    prmUnitDep      = 0. 
IF prmLayerPad    = ?  THEN ASSIGN    prmLayerPad     = "". 
IF prmLayerLen    = ?  THEN ASSIGN    prmLayerLen     = 0. 
IF prmLayerWid    = ?  THEN ASSIGN    prmLayerWid     = 0. 
IF prmLayerDep    = ?  THEN ASSIGN    prmLayerDep     = 0. 
IF prmDivider     = ?  THEN ASSIGN    prmDivider      = "". 
IF prmDividerLen  = ?  THEN ASSIGN    prmDividerLen   = 0.
IF prmDividerWid  = ?  THEN ASSIGN    prmDividerWid   = 0. 
IF prmDividerDep  = ?  THEN ASSIGN    prmDividerDep   = 0. 
IF prmCostEa      = ?  THEN ASSIGN    prmCostEa       = 0. 
IF prmBoxCode     = ?  THEN ASSIGN    prmBoxCode      = 0. 
IF prmPallet      = ?  THEN ASSIGN    prmPallet       = 0. 
IF prmWTUnit      = ?  THEN ASSIGN    prmWTUnit       = 0. 
                                                       
IF prmUnit        = ?  THEN ASSIGN    prmUnit         = "". 
IF prmCost2       = ?  THEN ASSIGN    prmCost2        = 0. 
IF prmCount       = ?  THEN ASSIGN    prmCount        = 0. 
IF prmLength      = ?  THEN ASSIGN    prmLength       = 0. 
IF prmWidth       = ?  THEN ASSIGN    prmWidth        = 0. 
IF prmHeight      = ?  THEN ASSIGN    prmHeight       = 0. 
IF prmLayer       = ?  THEN ASSIGN    prmLayer        = 0. 
IF prmNote        = ?  THEN ASSIGN    prmNote         = "". 
IF prmFrCharge    = ?  THEN ASSIGN    prmFrCharge     = "". 
IF prmWeightPer   = ?  THEN ASSIGN    prmWeightPer    = 0. 
IF prmCarrier     = ?  THEN ASSIGN    prmCarrier      = "". 
IF prmCarrDscr    = ?  THEN ASSIGN    prmCarrDscr     = "". 
IF prmDelZon      = ?  THEN ASSIGN    prmDelZon       = "".
IF prmFreight     = ?  THEN ASSIGN    prmFreight      = 0.
IF prmFreight2    = ?  THEN ASSIGN    prmFreight2     = 0.
IF prmPackQty     = ?  THEN ASSIGN    prmPackQty      = 0.
IF prmLayerQty    = ?  THEN ASSIGN    prmLayerQty     = 0.
IF prmDivQty      = ?  THEN ASSIGN    prmDivQty       = 0.
IF prmBlankno     = ?  THEN ASSIGN    prmBlankno      = 0.

IF prmSide1       = ?  THEN ASSIGN    prmSide1      = "".
IF prmSide2       = ?  THEN ASSIGN    prmSide2      = "".
IF prmSide3       = ?  THEN ASSIGN    prmSide3      = "".
IF prmSide4       = ?  THEN ASSIGN    prmSide4      = "".
IF prmSide5       = ?  THEN ASSIGN    prmSide5      = "".
IF prmSide6       = ?  THEN ASSIGN    prmSide6      = "".
IF prmSide7       = ?  THEN ASSIGN    prmSide7      = "".
IF prmSide8       = ?  THEN ASSIGN    prmSide8      = "".
IF prmSide9       = ?  THEN ASSIGN    prmSide9      = "".
IF prmSide10      = ?  THEN ASSIGN    prmSide10     = "".
IF prmSide11      = ?  THEN ASSIGN    prmSide11     = "".
IF prmSide12      = ?  THEN ASSIGN    prmSide12     = "".
IF prmSide13      = ?  THEN ASSIGN    prmSide13     = "".
IF prmSide14      = ?  THEN ASSIGN    prmSide14     = "".
IF prmSide15      = ?  THEN ASSIGN    prmSide15     = "".


DEF NEW SHARED var cocode as  CHAR  no-undo.
def NEW SHARED var locode as char  no-undo.

{est/fold-inksvarn.i NEW}
                
DEF BUFFER b-ef FOR ef.
DEF BUFFER b-eb FOR eb.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-i-ps LIKE eb.i-ps2 NO-UNDO.
DEF VAR lv-i-code LIKE eb.i-code2 NO-UNDO.
DEF VAR lv-i-dscr LIKE eb.i-dscr2 NO-UNDO.
DEF VAR lv-i-% LIKE eb.i-%2 NO-UNDO.
DEF VAR lv-side AS CHAR EXTENT 30 NO-UNDO.

DEFINE VAR bi AS INT NO-UNDO.
DEFINE VAR vEstimate AS CHAR NO-UNDO.
def var li-num-of-code as int no-undo.
DEF VAR ll-foam AS LOG NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR v-side-string AS CHAR NO-UNDO.
DEF VAR v-side-string-2 AS CHAR NO-UNDO.


    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_DEFault = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN 
    cocode = prmComp
    locode = "MAIN" .

vEstimate =  FILL(" ",8 - LENGTH(TRIM(prmEstNum))) + TRIM(prmEstNum).


/*************************************prmAction***************************************************/  

IF prmAction = "FoldsUpdate" THEN DO:
   
      li-num-of-code = 0.
      if prmCode1 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode2 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode3 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode4 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode5 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode6 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode7 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode8 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode9 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode10 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode11 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode12 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode13 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode14 <> "" then li-num-of-code = li-num-of-code + 1.
      if prmCode15 <> "" then li-num-of-code = li-num-of-code + 1.
      

       if li-num-of-code <> ((prmColor) + (prmCoat)) 
      then do:
          ASSIGN
          cError =  "Invalid Number of Color and Coating." .
          return .
      end. 

      if prmCarrier <> ""  THEN DO:
      FIND first carrier where carrier.carrier = prmCarrier AND carrier.company = prmComp NO-LOCK NO-ERROR.
      IF NOT AVAIL carrier then do:
          ASSIGN
         cError =  "Invalid Carrier. Try Help." .
         return .
      end.
      END.      

      if prmPackCode <> "" THEN DO:
       FIND FIRST item where item.company = prmComp and item.i-no = prmPackCode  and item.mat-type = "C" NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM then do:
            ASSIGN
            cError = "Invalid Packing Code. Try Help." .
            return .
      end.
      END.
      
      if prmUnit <> "" THEN DO:
       FIND FIRST  item where item.company = prmComp and item.i-no = prmUnit NO-LOCK NO-ERROR.
       IF NOT AVAIL ITEM then do:
           ASSIGN
            cError =  "Invalid Unit#. Try Help." .
            return .
      end.
      END.
      
      IF prmDelZon <> "" THEN DO:
      FIND FIRST carr-mtx  WHERE carr-mtx.company  EQ prmComp 
                      /*AND carr-mtx.loc      EQ locode*/
                      AND carr-mtx.carrier  EQ prmCarrier
                      AND carr-mtx.del-zone EQ prmDelZon NO-LOCK NO-ERROR.
     IF NOT AVAIL carr-mtx THEN DO:
         ASSIGN
      cError =  "Invalid Delivery Zone, try help..." .
      RETURN .
    END.
    END.    

    if prmLayerPad <> "" THEN DO:
     FIND FIRST  item where item.company = prmComp and item.i-no = prmLayerPad and item.mat-type = "5" NO-LOCK NO-ERROR.
     IF NOT AVAIL ITEM  then do:
         ASSIGN
             cError =  "Invalid Layer Pad , try help...".
              return.
       end.
    END.    
    
    if prmDivider <> "" THEN DO:
    FIND FIRST  item where item.company = prmComp 
                           and item.i-no = prmDivider
                           and item.mat-type = "6"  NO-LOCK NO-ERROR.
    IF NOT AVAIL ITEM then do:
        ASSIGN
            cError =  "Invalid Divider, try help..." .
      return.
    end.
  END.  
    
    IF DEC(prmBoxCode) NE 0 AND DEC(prmWTUnit)  NE 0 THEN DO:
       ASSIGN
       cError =  "You may enter EITHER  BoxCode OR  PackUnit " .
       RETURN.
       END.

    IF prmCode1 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode1  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code1, try help..." .
        RETURN .
      END.
      END.
    IF prmCode2 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode2  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code2, try help..." .
        RETURN .
      END.
      END.
    IF prmCode3 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode3  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code3, try help..." .
        RETURN .
      END.
      END.
    IF prmCode4 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode4  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code4, try help..." .
        RETURN .
      END.
      END.
    IF prmCode5 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode5  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code5, try help..." .
        RETURN .
      END.
      END.
    IF prmCode6 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode6  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code6, try help..." .
        RETURN .
      END.
      END.
    IF prmCode7 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode7  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code7, try help..." .
        RETURN .
      END.
      END.
    IF prmCode8 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode8  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code8, try help..." .
        RETURN .
      END.
      END.
    IF prmCode9 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode9  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code9, try help..." .
        RETURN .
      END.
      END.
    IF prmCode10 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode10  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code10, try help..." .
        RETURN .
      END.
      END.

      IF prmCode11 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode11  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code11, try help..." .
        RETURN .
      END.
      END.
    IF prmCode12 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode12  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code12, try help..." .
        RETURN .
      END.
      END.
    IF prmCode13 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode13  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code13, try help..." .
        RETURN .
      END.
      END.
    IF prmCode14 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode14  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code14, try help..." .
        RETURN .
      END.
      END.      

    IF prmCode15 <> "" THEN DO:
      FIND FIRST ITEM where item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V")
          AND item.i-no EQ prmCode15  NO-LOCK NO-ERROR.
        IF NOT AVAIL ITEM THEN DO:
        cError =  "Invalid Code15, try help..." .
        RETURN .
      END.
      END.

  
END. /*end of validation*/



IF prmAction = "FoldsUpdate" THEN DO:
 
FIND FIRST est where est.company = prmComp AND est.est-no = vEstimate EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL est THEN DO:
    ASSIGN
        est.updated-id  = prmUser.
    END.

FIND FIRST ef where ef.company = prmComp AND ef.est-no = vEstimate AND ef.form-no = prmFormno EXCLUSIVE-LOCK NO-ERROR.
     find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno EXCLUSIVE-LOCK NO-ERROR.     
      
 IF AVAIL ef  AND AVAIL eb  THEN DO:
       ASSIGN 
              eb.i-col     = prmColor    
              eb.i-pass    = prmPass     
              eb.i-coat    = prmCoat     
              eb.i-coat-p  = prmCoatPass 
              eb.i-coldscr = prmDscr     
              eb.i-ps2[1]   = prmPs1      
              eb.i-ps2[2]   = prmPs2      
              eb.i-ps2[3]   = prmPs3      
              eb.i-ps2[4]   = prmPs4      
              eb.i-ps2[5]   = prmPs5      
              eb.i-ps2[6]   = prmPs6      
              eb.i-ps2[7]    = prmPs7      
              eb.i-ps2[8]    = prmPs8      
              eb.i-ps2[9]    = prmPs9      
              eb.i-ps2[10]   = prmPs10
              eb.i-ps2[11]   = prmPs11      
              eb.i-ps2[12]   = prmPs12      
              eb.i-ps2[13]   = prmPs13      
              eb.i-ps2[14]   = prmPs14      
              eb.i-ps2[15]   = prmPs15      

              eb.i-code2[1]  = prmCode1    
              eb.i-code2[2]  = prmCode2    
              eb.i-code2[3]  = prmCode3    
              eb.i-code2[4]  = prmCode4                                                 
              eb.i-code2[5]  = prmCode5                                          
              eb.i-code2[6]  = prmCode6                                         
              eb.i-code2[7]  = prmCode7    
              eb.i-code2[8]  = prmCode8    
              eb.i-code2[9]  = prmCode9    
              eb.i-code2[10] = prmCode10 
              eb.i-code2[11]  = prmCode11    
              eb.i-code2[12]  = prmCode12    
              eb.i-code2[13]  = prmCode13    
              eb.i-code2[14]  = prmCode14                                                 
              eb.i-code2[15]  = prmCode15 

              eb.i-dscr2[1]  = prmDscr1    
              eb.i-dscr2[2]  = prmDscr2    
              eb.i-dscr2[3]  = prmDscr3    
              eb.i-dscr2[4]  = prmDscr4    
              eb.i-dscr2[5]  = prmDscr5    
              eb.i-dscr2[6]  = prmDscr6    
              eb.i-dscr2[7]  = prmDscr7    
              eb.i-dscr2[8]  = prmDscr8    
              eb.i-dscr2[9]  = prmDscr9   
              eb.i-dscr2[10] = prmDscr10 
              eb.i-dscr2[11]  = prmDscr11    
              eb.i-dscr2[12]  = prmDscr12    
              eb.i-dscr2[13]  = prmDscr13    
              eb.i-dscr2[14]  = prmDscr14    
              eb.i-dscr2[15]  = prmDscr15    

              eb.i-%2[1]     = prmPer1 
              eb.i-%2[2]     = prmPer2     
              eb.i-%2[3]     = prmPer3     
              eb.i-%2[4]     = prmPer4     
              eb.i-%2[5]     = prmPer5     
              eb.i-%2[6]     =  prmPer6     
              eb.i-%2[7]     =  prmPer7     
              eb.i-%2[8]     =  prmPer8     
              eb.i-%2[9]     =  prmPer9     
              eb.i-%2[10]    =  prmPer10
              eb.i-%2[11]     = prmPer11 
              eb.i-%2[12]     = prmPer12     
              eb.i-%2[13]     = prmPer13     
              eb.i-%2[14]     = prmPer14     
              eb.i-%2[15]     = prmPer15 
                                          
              eb.cas-no       =  prmPackCode 
              eb.cas-len      =  prmUnitLen
              eb.cas-wid      =  prmUnitWid
              eb.cas-dep      =  prmUnitDep 
              /*              =  vPackQty */
              eb.layer-pad    =  prmLayerPad
              eb.lp-len       =  prmLayerLen
              eb.lp-wid       =  prmLayerWid
              /*              =  lpdep*/
              eb.lp-up        =  prmLayerQty
              eb.divider      =  prmDivider
              eb.div-len      =  prmDividerLen
              eb.div-wid      =  prmDividerWid
              /*     divdep  */
              eb.div-up       =  prmDivQty
              eb.cas-cost     =  prmCostEa
              eb.cas-cnt      =  prmBoxCode 
               
              eb.cas-pal      =  prmPallet    
              eb.cas-wt       =  prmWTUnit   
              eb.tr-no        =  prmUnit     
              eb.tr-cost      =  prmCost2    
              eb.tr-cnt       =  prmCount    
              eb.tr-len       =  prmLength   
              eb.tr-wid       =  prmWidth    
              eb.tr-dep       =  prmHeight   
              eb.tr-cas       =  prmLayer  
              eb.chg-method   =  prmFrCharge 
              eb.weight-m     =  prmWeightPer
              eb.carrier      =  prmCarrier  
              eb.carr-dscr    =  prmCarrDscr 
              eb.dest-code    =  prmDelZon   
              eb.fr-out-c     =  prmFreight  
              eb.fr-out-m     =  prmFreight2   .

     END.


     ASSIGN  
     v-side-string = (IF prmSide1 = "" THEN " " ELSE prmSide1)
                   + (IF prmSide2 = "" THEN " " ELSE prmSide2)
                   + (IF prmSide3 = "" THEN " " ELSE prmSide3)
                   + (IF prmSide4 = "" THEN " " ELSE prmSide4)
                   + (IF prmSide5 = "" THEN " " ELSE prmSide5)
                   + (IF prmSide6 = "" THEN " " ELSE prmSide6)
                   + (IF prmSide7 = "" THEN " " ELSE prmSide7)
                   + (IF prmSide8 = "" THEN " " ELSE prmSide8)
                   + (IF prmSide9 = "" THEN " " ELSE prmSide9)
                   + (IF prmSide10 = "" THEN " " ELSE prmSide10)
                   + (IF prmSide11 = "" THEN " " ELSE prmSide11)
                   + (IF prmSide12 = "" THEN " " ELSE prmSide12)
         
         .

       RUN find-create-unit# (ROWID(eb), 0, OUTPUT lv-rowid).
       FIND reftable WHERE ROWID(reftable) EQ lv-rowid NO-ERROR.
       
       IF AVAIL reftable THEN DO:
           ASSIGN
               reftable.val[1]  = prmUnit1  
               reftable.val[2]  = prmUnit2
               reftable.val[3]  = prmUnit3
               reftable.val[4]  = prmUnit4  
               reftable.val[5]  = prmUnit5
               reftable.val[6]  = prmUnit6
               reftable.val[7]  = prmUnit7 
               reftable.val[8]  = prmUnit8
               reftable.val[9]  = prmUnit9
               reftable.val[10] = prmUnit10
               reftable.val[11] = prmUnit11
               reftable.val[12] = prmUnit12
               reftable.dscr    = v-side-string
               .
       END.
           
       RUN find-create-unit# (ROWID(eb), 1, OUTPUT lv-rowid).
       FIND reftable WHERE ROWID(reftable) EQ lv-rowid NO-ERROR.

       IF AVAIL reftable THEN DO:
           v-side-string-2 = (IF prmSide13 = "" THEN " " ELSE prmSide13)
                    + (IF prmSide14 = "" THEN " " ELSE prmSide14)
                    + (IF prmSide15 = "" THEN " " ELSE prmSide15).

           ASSIGN
               reftable.val[1]  = prmUnit13  
               reftable.val[2]  = prmUnit14
               reftable.val[3]  = prmUnit15    
               reftable.dscr    = v-side-string-2
               .
              
        END.
        
        FIND FIRST itemfg  WHERE itemfg.company EQ eb.company 
            AND itemfg.i-no    EQ eb.stock-no AND eb.stock-no    NE "" EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL itemfg THEN 
            ASSIGN
            itemfg.prod-notes = prmNote .

        find item where item.company = eb.company AND 
              item.i-no = eb.layer-pad AND  item.mat-type = "5" EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE ITEM THEN 
            ASSIGN 
            ITEM.box-case =  eb.lp-up. 
        find item where item.company = eb.company and
            item.i-no = eb.divider AND item.mat-type = "6" EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE ITEM THEN
            ASSIGN 
            ITEM.box-case =  eb.div-up.    

        ASSIGN prmAction = "Select".
        
 END.


 IF prmAction = "ColorChange" THEN DO:
     FIND FIRST est where est.company = prmComp AND est.est-no = vEstimate EXCLUSIVE-LOCK NO-ERROR.
     FIND FIRST ef where ef.company = prmComp AND ef.est-no = vEstimate AND ef.form-no = prmFormno EXCLUSIVE-LOCK NO-ERROR.
     find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno EXCLUSIVE-LOCK NO-ERROR.

    
     CREATE ttFoldInks.

     ASSIGN ttFoldInks.vPasses = DEC(prmPass).

     IF DEC(prmColor) EQ 0 THEN 
        ASSIGN ttFoldInks.vPasses = 0.
     ELSE
        IF DEC(prmPass) EQ 0 THEN ASSIGN  ttFoldInks.vPasses = 1.

     IF DEC(prmColor) GE 1 AND prmPass EQ 0 THEN ASSIGN prmPass = 1.

     {ce/defInks.i}
 END.

 IF prmAction = "CoatChange" THEN DO:
    FIND FIRST est where est.company = prmComp AND est.est-no = vEstimate EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST ef where ef.company = prmComp AND ef.est-no = vEstimate AND ef.form-no = prmFormno EXCLUSIVE-LOCK NO-ERROR.
    find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno EXCLUSIVE-LOCK NO-ERROR.

    CREATE ttFoldInks.

    ASSIGN ttFoldInks.vCoatPass = DEC(prmCoatPass).

    IF DEC(prmCoat) EQ 0 THEN 
        ASSIGN ttFoldInks.vCoatPass = 0.
    ELSE 
        IF DEC(prmCoatPass) EQ 0 THEN ASSIGN  ttFoldInks.vCoatPass = 1. 

    IF DEC(prmCoat) GE 1 AND prmCoatPass EQ 0 THEN ASSIGN prmCoatPass = 1.

    {ce/defInks.i}
 END.

 IF prmAction = "CoatPassChange" THEN DO:
    FIND FIRST est where est.company = prmComp AND est.est-no = vEstimate EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST ef where ef.company = prmComp AND ef.est-no = vEstimate AND ef.form-no = prmFormno EXCLUSIVE-LOCK NO-ERROR.
    find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno EXCLUSIVE-LOCK NO-ERROR.

    CREATE ttFoldInks.    

    {ce/defInks.i}
 END.

 IF prmAction = "PassesChange" THEN DO:
    FIND FIRST est where est.company = prmComp AND est.est-no = vEstimate EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST ef where ef.company = prmComp AND ef.est-no = vEstimate AND ef.form-no = prmFormno EXCLUSIVE-LOCK NO-ERROR.
    find FIRST eb where eb.company = prmComp AND eb.est-no = ef.est-no AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno EXCLUSIVE-LOCK NO-ERROR.

    CREATE ttFoldInks.    

    {ce/defInks.i}
 END.
        

 IF prmAction = "Select" THEN DO:
     
   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH b-ef
        WHERE b-ef.company EQ est.company
          AND b-ef.est-no  EQ vEstimate
        BREAK BY b-ef.eqty:
      IF FIRST-OF(b-ef.eqty) THEN li = 0.
      li = li + 1.
    END.
    IF li NE est.form-qty THEN DO:
      FIND CURRENT est EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL est THEN est.form-qty = li.
      FIND CURRENT est NO-LOCK.
    END.
   
    /*DISABLE TRIGGERS FOR LOAD OF ef.*/
    FIND FIRST ef
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ prmFormNo
        NO-LOCK NO-ERROR.
    IF AVAIL ef THEN DO:
      FOR EACH b-eb
          WHERE b-eb.company EQ ef.company
            AND b-eb.est-no  EQ ef.est-no
            AND b-eb.form-no EQ ef.form-no
          NO-LOCK:
        bi = bi + 1.
      END.
      IF li NE ef.blank-qty THEN DO:
        FIND CURRENT ef EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL ef THEN ef.blank-qty = bi.
        FIND CURRENT ef NO-LOCK.
      END.
    END.
  

  FIND FIRST eb WHERE eb.company = prmComp AND eb.est-no = est.est-no  AND eb.form-no = prmFormNo AND eb.blank-no = prmBlankno NO-LOCK NO-ERROR.
      IF AVAIL eb THEN DO:
          CREATE ttFoldInks.
          ASSIGN
              ttFoldInks.vEstNum       = est.est-no
              ttFoldInks.vCustPart     = eb.part-no
              ttFoldInks.vEstDate      = est.est-date
              ttFoldInks.vFormNo       = eb.form-no
              ttFoldInks.vFormQty      = est.form-qty
              ttFoldInks.vBlankNo      = eb.blank-no
              ttFoldInks.vBlankQty     = bi
                                       
              ttFoldInks.vColor        = eb.i-col
              ttFoldInks.vPasses       = eb.i-pass
              ttFoldInks.vCoat         = eb.i-coat
              ttFoldInks.vCoatPass     = eb.i-coat-p
              ttFoldInks.vDscr         = eb.i-coldscr
              ttFoldInks.vPs1          = eb.i-ps2[1]
              ttFoldInks.vPs2          = eb.i-ps2[2]
              ttFoldInks.vPs3          = eb.i-ps2[3]
              ttFoldInks.vPs4          = eb.i-ps2[4]
              ttFoldInks.vPs5          = eb.i-ps2[5]
              ttFoldInks.vPs6          = eb.i-ps2[6]
              ttFoldInks.vPs7          = eb.i-ps2[7] 
              ttFoldInks.vPs8          = eb.i-ps2[8]
              ttFoldInks.vPs9          = eb.i-ps2[9]
              ttFoldInks.vPs10         = eb.i-ps2[10]
              ttFoldInks.vPs11         = eb.i-ps2[11]
              ttFoldInks.vPs12         = eb.i-ps2[12]
              ttFoldInks.vPs13         = eb.i-ps2[13]
              ttFoldInks.vPs14         = eb.i-ps2[14]
              ttFoldInks.vPs15         = eb.i-ps2[15]
              
              ttFoldInks.vCode1        = eb.i-code2[1]
              ttFoldInks.vCode2        = eb.i-code2[2]
              ttFoldInks.vCode3        = eb.i-code2[3]
              ttFoldInks.vCode4        = eb.i-code2[4]
              ttFoldInks.vCode5        = eb.i-code2[5]
              ttFoldInks.vCode6        = eb.i-code2[6]
              ttFoldInks.vCode7        = eb.i-code2[7]
              ttFoldInks.vCode8        = eb.i-code2[8]
              ttFoldInks.vCode9        = eb.i-code2[9]
              ttFoldInks.vCode10       = eb.i-code2[10]
              ttFoldInks.vCode11       = eb.i-code2[11]
              ttFoldInks.vCode12       = eb.i-code2[12]  
              ttFoldInks.vCode13       = eb.i-code2[13]
              ttFoldInks.vCode14       = eb.i-code2[14]
              ttFoldInks.vCode15       = eb.i-code2[15]
             
              ttFoldInks.vDscr1        = eb.i-dscr2[1]
              ttFoldInks.vDscr2        = eb.i-dscr2[2]
              ttFoldInks.vDscr3        = eb.i-dscr2[3]
              ttFoldInks.vDscr4        = eb.i-dscr2[4]
              ttFoldInks.vDscr5        = eb.i-dscr2[5]
              ttFoldInks.vDscr6        = eb.i-dscr2[6]
              ttFoldInks.vDscr7        = eb.i-dscr2[7]
              ttFoldInks.vDscr8        = eb.i-dscr2[8]
              ttFoldInks.vDscr9        = eb.i-dscr2[9]   
              ttFoldInks.vDscr10       = eb.i-dscr2[10]
              ttFoldInks.vDscr11       = eb.i-dscr2[11]
              ttFoldInks.vDscr12       = eb.i-dscr2[12]
              ttFoldInks.vDscr13       = eb.i-dscr2[13]
              ttFoldInks.vDscr14       = eb.i-dscr2[14]
              ttFoldInks.vDscr15       = eb.i-dscr2[15]
               
              ttFoldInks.vPer1         = eb.i-%2[1]
              ttFoldInks.vPer2         = eb.i-%2[2] 
              ttFoldInks.vPer3         = eb.i-%2[3]
              ttFoldInks.vPer4         = eb.i-%2[4]
              ttFoldInks.vPer5         = eb.i-%2[5]
              ttFoldInks.vPer6         = eb.i-%2[6]
              ttFoldInks.vPer7         = eb.i-%2[7]
              ttFoldInks.vPer8         = eb.i-%2[8]
              ttFoldInks.vPer9         = eb.i-%2[9]
              ttFoldInks.vPer10        = eb.i-%2[10]
              ttFoldInks.vPer12        = eb.i-%2[11]
              ttFoldInks.vPer11        = eb.i-%2[12]
              ttFoldInks.vPer13        = eb.i-%2[13]
              ttFoldInks.vPer14        = eb.i-%2[14]
              ttFoldInks.vPer15        = eb.i-%2[15]
                  
                                    
              ttFoldInks.vPackCode     = eb.cas-no     
              ttFoldInks.vUnitLen      = eb.cas-len    
              ttFoldInks.vUnitWid      = eb.cas-wid   
              ttFoldInks.vUnitDep      = eb.cas-dep    
              ttFoldInks.vLayerPad     = eb.layer-pad    
              ttFoldInks.vLayerLen     = eb.lp-len    
              ttFoldInks.vLayerWid     = eb.lp-wid 
              ttFoldInks.vDel          = eb.divider      
              ttFoldInks.vDelLen       = eb.div-len    
              ttFoldInks.vDelWid       = eb.div-wid  
              ttFoldInks.vPackQty      = 0      
              ttFoldInks.vLayerQty     = eb.lp-up
              ttFoldInks.vDelQty       = eb.div-up
                                         
              ttFoldInks.vCost         = eb.cas-cost 
              ttFoldInks.vBoxCode      = eb.cas-cnt 
              ttFoldInks.vPallet       = eb.cas-pal   
              ttFoldInks.vWtUnit       = eb.cas-wt    
              ttFoldInks.vUnit         = eb.tr-no  
              ttFoldInks.vLength       = eb.tr-len  
              ttFoldInks.vWidth        = eb.tr-wid  
              ttFoldInks.vHeight       = eb.tr-dep
              ttFoldInks.vCost2        = eb.tr-cost
              ttFoldInks.vCount        = eb.tr-cnt
              ttFoldInks.vLayer        = eb.tr-cas
                    
              ttFoldInks.vFrCharge     = eb.chg-method
              ttFoldInks.vWeiPer       = eb.weight-m  
              ttFoldInks.vCarrier      = eb.carrier   
              ttFoldInks.vCarrDscr     = eb.carr-dscr 
              ttFoldInks.vDelZon       = eb.dest-code 
              ttFoldInks.vFreifgt      = eb.fr-out-c  
              ttFoldInks.vFreOut       = eb.fr-out-m   
              ttFoldInks.vStock        = eb.stock-no
              ttFoldInks.order        = STRING(eb.ord-no)
             
              .
          IF ttFoldInks.vFrCharge = "" THEN
              ASSIGN ttFoldInks.vFrCharge = "P" .
         END. /*ettFoldInksnd of  eb*/
         
    RUN find-create-unit# (ROWID(eb), 0, OUTPUT lv-rowid).
    FIND reftable WHERE ROWID(reftable) EQ lv-rowid NO-LOCK NO-ERROR.
    IF AVAIL reftable THEN
      ASSIGN
       ttFoldInks.vUnit1   = reftable.val[1]
       ttFoldInks.vUnit2   = reftable.val[2]
       ttFoldInks.vUnit3   = reftable.val[3]
       ttFoldInks.vUnit4   = reftable.val[4]
       ttFoldInks.vUnit5   = reftable.val[5]
       ttFoldInks.vUnit6   = reftable.val[6]
       ttFoldInks.vUnit7   = reftable.val[7]
       ttFoldInks.vUnit8   = reftable.val[8]
       ttFoldInks.vUnit9   = reftable.val[9]
       ttFoldInks.vUnit10  = reftable.val[10]
       ttFoldInks.vUnit11  = reftable.val[11]
       ttFoldInks.vUnit12  = reftable.val[12]
       ttFoldInks.vSide1   = SUBSTRING(reftable.dscr,1,1)
       ttFoldInks.vSide2   = SUBSTRING(reftable.dscr,2,1)
       ttFoldInks.vSide3   = SUBSTRING(reftable.dscr,3,1)
       ttFoldInks.vSide4   = SUBSTRING(reftable.dscr,4,1)
       ttFoldInks.vSide5   = SUBSTRING(reftable.dscr,5,1)
       ttFoldInks.vSide6   = SUBSTRING(reftable.dscr,6,1)
       ttFoldInks.vSide7   = SUBSTRING(reftable.dscr,7,1)
       ttFoldInks.vSide8   = SUBSTRING(reftable.dscr,8,1)
       ttFoldInks.vSide9   = SUBSTRING(reftable.dscr,9,1)
       ttFoldInks.vSide10  = SUBSTRING(reftable.dscr,10,1)
       ttFoldInks.vSide11  = SUBSTRING(reftable.dscr,11,1)
       ttFoldInks.vSide12  = SUBSTRING(reftable.dscr,12,1) 
        .

    RUN find-create-unit# (ROWID(eb), 1, OUTPUT lv-rowid).
    FIND reftable WHERE ROWID(reftable) EQ lv-rowid NO-LOCK NO-ERROR.

    IF AVAIL reftable THEN
      ASSIGN
       ttFoldInks.vUnit13  = reftable.val[1]
       ttFoldInks.vUnit14  = reftable.val[2]
       ttFoldInks.vUnit15  = reftable.val[3] 
       ttFoldInks.vSide13  = SUBSTRING(reftable.dscr,13,1)
       ttFoldInks.vSide14  = SUBSTRING(reftable.dscr,14,1)
       ttFoldInks.vSide15  = SUBSTRING(reftable.dscr,15,1)           
       .
    

  FIND FIRST itemfg
      WHERE itemfg.company EQ eb.company
        AND itemfg.i-no    EQ eb.stock-no
        AND eb.stock-no    NE ""
      NO-LOCK NO-ERROR.
  ttFoldInks.vNote  = IF AVAIL itemfg THEN itemfg.prod-notes ELSE "".
    FIND CURRENT eb EXCLUSIVE-LOCK NO-ERROR.
    find item where item.company = eb.company AND item.i-no = eb.layer-pad and
         item.mat-type = "5" no-lock no-error.
    IF AVAILABLE ITEM THEN 
        ASSIGN 
        ttFoldInks.vLayerDep = ITEM.case-d  
        eb.lp-up = ITEM.box-case.

   find item where item.company = eb.company AND item.i-no = eb.divider and
         item.mat-type = "6" no-lock no-error.
   IF AVAILABLE ITEM THEN
       ASSIGN 
       ttFoldInks.vDelDep = ITEM.case-d  
       eb.div-up = ITEM.box-case.
   
 
 END. /* end of select*/

/**********************procedure*******************************/


PROCEDURE find-create-unit# :

  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
  DEF INPUT PARAM ip-int AS INT NO-UNDO.
  DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.

  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-rt FOR reftable.


  FIND b-eb WHERE ROWID(b-eb) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL b-eb THEN DO TRANSACTION:
    FIND FIRST b-rt
        WHERE b-rt.reftable EQ "ce/v-est3.w Unit#" + TRIM(STRING(ip-int,">"))
          AND b-rt.company  EQ b-eb.company
          AND b-rt.loc      EQ b-eb.est-no
          AND b-rt.code     EQ STRING(b-eb.form-no,"9999999999")
          AND b-rt.code2    EQ STRING(b-eb.blank-no,"9999999999")
        NO-LOCK NO-ERROR.
    IF NOT AVAIL b-rt THEN DO:
      CREATE b-rt.
      ASSIGN
       b-rt.reftable = "ce/v-est3.w Unit#" + TRIM(STRING(ip-int,">"))
       b-rt.company  = b-eb.company
       b-rt.loc      = b-eb.est-no
       b-rt.code     = STRING(b-eb.form-no,"9999999999")
       b-rt.code2    = STRING(b-eb.blank-no,"9999999999").
    END.

    op-rowid = ROWID(b-rt).
  END.

END PROCEDURE.

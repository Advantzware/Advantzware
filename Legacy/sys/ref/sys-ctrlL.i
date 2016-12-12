/*************************************************************************
    Programe Name :  Sys/ref/sys-ctrl.i                                  * 
    Author        :                                                      * 
    Date          :                                                      * 
    Desc          :  Include file for sys control paramaters             * 
                                                                         *
*************************************************************************/
DEF VAR name-fld-list AS CHAR NO-UNDO.

name-fld-list =
/*    1      2       3     4        5        6      7        8      9       10      11      12       13      14      15       16     17   18      19        20     21     22      23   24     25      26       27       28     29       30      31      32      33       34      35      36       37   38    39     40        41     42       43      44       45 */
"quoprint,invprint,cerun,msfcalc,2piececd,fastoe,setprint,poprint,relprint,bolfmt,chkfmt,addrelse,ackhead,jobcard,boxdesum,job qty,job#,bolwhse,closejob,relpost,pocost,loadtag,arexp,cemenu,ceroute,oeautopo,sht calc,ceround,bolcert,cerunc,cerunf,jobcardc,jobcardf,oecomm,oecarier,autopost,apinq,poqty,arinq,schedule,fgitem#,fgsetrec,bolpost,lastship,oereleas," +
"ap gl#,autoissu,quoprice,rmissue,custsize,poexport,cewhschg,fgpostgl,foamcost,logocolr,cepanel,sellpric,oepopup,oeclose,invdate,boldate,oeautofg,quosheet,fgemails,tspost,fgrecpt,pouom,fgwhsbin,cadcam,efbrowse,rmrecpt,fgbrowse,rmwhsbin,rmpostgl,sstransf,payvend,ou6brows,cewhatif,oereordr,oeship,fginvrec,inexport,cecopy,ceprint,ceprep,cecscrn,ceprice,relmerge," +
/* 46     47        48      49      50       51       52        53       54        55      56      57       58      59      60     61       62       63       64      65     66      67     68      69      70      71      72       73        74       75      76       77       78       79      80      81       82      83     84      85     86      87      88  */
/*  89       90       91     92      93      94       95    96   97     98      99       100       101      102     103    104      105       106    107   108       109       110       111      112       113       114      115       116       117    118      119      120       121     122        123        124         125     126     127     128        129  */
"stmtprint,TSFinish,appaper,cecunit,fgpost,corsuply,fgitemsf,GP,oeprep,celayout,1099misc,RFQPrint,oecredit,maxbreak,aptax,rmemails,sspostfg,bolfmtx,schdcard,TSTIME,FGReOrder,ARMEMO,APCheckFile,OEPrompt,SSBOLSCAN,INVCOPYS,CORRCHOICE,BORELDATE,OEINQ,ECBROWSE,VENDXFER,CUSTXFER,ORDERXFER,MISCJOBCL,RMUnderOver,CEPREPPRICE,RELTYPE,SSMoveFG,CEMISC,BolPostTime,CEDeliveryZone," +
/* 130         131      132     133           134        135      136      137      138      139    140      141           142       143           144      145         146      147        148        149     150           151       152                 153*/
"BOLFreight,CESAMPLE,SSRMISSUE,CorrTrim,CustShipToImp,OEScreen,fgoecost,runship,InvStatus,AGEDAYS,FGPostCmp,AckMaster,ChkFmtACH,OeDateChange,SSBOLEMAIL,FGRecptUnit,FGBrowseIA,AlliFlutes,SSBOLPRINT,POScreen,SSScanVendor,BOLFMTTran,BOLMaster,CEMarkupMatrixLookup".
                                  /* 126*/

DEF VAR str-init  AS CHAR EXTENT 155 NO-UNDO.
    
ASSIGN
 str-init[1]  = "quoprint 1,quoprint 2"
 str-init[2]  = "invprint 1,invprint 2"
 str-init[3]  = "ASI,Clevelnd,McLean,Suthrlnd,HOP,Brick,Peachtre"
 str-init[4]  = "Foldware,Corrware"
 str-init[5]  = "D Pallet,Z Trailr"
 str-init[6]  = "ASI,Argrov,Beeler,ILWalker"
 str-init[7]  = "ASI,McLean"
 str-init[8]  = "poprint 1,poprint 2"
 str-init[9]  = "relprint 1,relprint 2"
 str-init[10] = "bolfmt 1,bolfmt 2"
 str-init[11] = "chkfmt 1,chkfmt 2"
 str-init[12] = "ASI,Royal,Bin/Tag,No Tags".
ASSIGN
 str-init[13] = "ackhead 1,ackhead 2"
 str-init[14] = "10 Pitch,17 Pitch,Boxtech,Phoenix,TriState,Triad,RFC,HOP," +
                "Brick,Hartford"
 str-init[15] = "Inches,MM,Both"
 str-init[16] = ",Net Shts"
 str-init[17] = ",Order#,YYMMSEQ#,PLine&Order#,Estimate#"
 str-init[18] = "FG BIN,SHIPTO,FIFO,ShipFromWhse"
 str-init[19] = "Manually,FGPost,OrdClose,TS"
 str-init[20] = "Nothing,Invoice,BOL,BOL/TAG,BOL/REL"
 str-init[21] = "JobFile,Vendor,Vendor/MSH"
 str-init[22] = "ASI,Triad,BarOne,SSBarOne,SSLabel,CentBox"
 str-init[23] = "ASI,Sonoco,Inland,Excel"
 str-init[24] = "Corrware,Foldware,Both"
 str-init[26] = "Auto,Manual,AutoRM"
 str-init[27] = "QOH>QEst,AllItems,QAvail>0"
 str-init[28] = "Penny,Dollar"
 str-init[29] = "bolcert 1,bolcert 2"
 str-init[30] = "cerunc 1,cerunc 2"
 str-init[31] = "cerunf 1,cerunf 2"
 str-init[32] = "jobcardc 1,jobcardc 2"
 str-init[33] = "jobcardf 1,jobcardf 2" /*MWFibre=Keystone*/
 str-init[34] = "Manual,Matrix"
 str-init[35] = "ShipTo,Header"
 str-init[36] = "ShipTo,FGFile"
 str-init[37] = "Triad,Brick"
 str-init[38] = "JobQty,Net Shts"
 str-init[39] = "ASI,Fibre"
 str-init[40] = /*Planner,Kiwi,*/ "None,PlanDate,NoDate"
 str-init[41] = "Manual,%$$$$$$####A&&,@$$$$$$$$####A>,$$$$$$$$#####A>,None,Hold" /* %$$$$$$####A&&:Hughes,@$$$$$$$$####A>:Fibre*/
 str-init[42] = "Item,Set" 
 str-init[43] = ",Bin>Qty"
 str-init[44] = "Fibre,ASI,Stock/Custom" 
 str-init[45] = "Due Date,LastShip,DueDate+1Day" 
 str-init[46] = "Expense,Vendor,ExpVend,Asset" 
 str-init[47] = "JobClose,FGPost" 
 str-init[48] = "EA,M" 
 str-init[49] = "Gross,Net" 
 str-init[50] = "ASI,Hughes"  .
ASSIGN
 str-init[51] = "None,CorrTrim,Alliance,HRMS,CorSuply,Corr-U-KraftII,GP," +
                "Vendor,Kiwi,Smurfit,CorrChoice,Pratt,AlliFlutes,iPaper"  
 str-init[52] = "Percent,$/Pallet,$/MSF"
 str-init[53] = "AllItems,POOnly,None" 
 str-init[54] = "Sheet,Blank" 
 str-init[55] = "BLACK,RED"  
 str-init[56] = "WminLmin,PminPmax"
 str-init[57] = "Matrix,LastPric"
 str-init[58] = ""
 str-init[59] = "Complete,OnHand=0"
 str-init[60] = "BOL,Current"    
 str-init[61] = "Current,Release"
 str-init[62] = "NonStock,LotCntrl,Avail<0,Any"
 str-init[63] = "SouthPak,Xprint,APC,StClair"  /* quosheet */
 str-init[64] = "None,Underrun,Receipts"  /* fgemails */ 
 str-init[65] = "Standard,Actual"  /* tspost */ 
 str-init[66] = "AUTOPOST,TSPOSTFG,Manual,LoadTag,TSPARTS"  /* fgrecpt */ 
 str-init[67] = "Stock,Purchase"  /* pouom */
 str-init[68] = "AUTOPOST,FGITEM"  /* fgwhsbin */
 str-init[69] = ",Artios,Arden,Artios2" /* cadcam */  
 str-init[70] = "Std,Std & Paper1/2" /* efbrowse */  
 str-init[71] = ",ByItem,POPUP,RMTAG" /* rmrecpt */
 str-init[72] = ",Bronze/Maroon" /* fgbrowse */
 str-init[74] = ",REALONLY,ALLITEMS" /* rmpostgl */
 str-init[75] = "ItemFle,Pallet" /* SSTRANSF character value was ",Pallet,Case" */
 str-init[76] = "ASI,MAS90,WestInd" /* payvend */
 str-init[77] = "OEINQ,Order Entry" /* ou6brows */
 str-init[78] = ",PerMSF,CM%+Freight" /* cewhatif */
 str-init[79] = ",XOnOrder" /* oereordr */
 str-init[80] = "EstShipto,OEShipto" /* oeship */
 str-init[81] = "Inv/Item,AllItems" /* fginvrec */
 str-init[82] = ",CIT,ContSrvc,Goodman"  /*Inexport*/  
 str-init[83] = "Both,Corr,Fold,Neither" /* cecopy */
 str-init[84] = "Consolidate,Segment,Text" /* ceprint */
 str-init[85] = "Penny,Dollar,FiveDollar" /* ceprep */  
 str-init[86] = ",16th's,32nd's,Decimal" /* cecscrn */
 str-init[87] = "RunOnly,MR+Run" /* ceprice */
 str-init[88] = ",AllOrders,SameOrderOnly,SamePo#Only,AllOrders&ShipFromWhse,SamePO#&ShipFromWhse,SameOrder&SameShipFrom,SameOrder&SameShipFrom&SamePO,AllOrders&NotRunShip" /* relmerge */       
 str-init[89] = "stmtprint 1,stmtprint 2"  /*stmtprin*/
 str-init[90] = "All Machines,Last Machine,NO"  /*TSFinish*/
 str-init[91] = "MSF,PO UOM"  /*appaper*/
 str-init[92] = "FluteMtx,AUTOCALC"  /*appaper*/
 str-init[93] = ",Before,After,Both"  /*fgpost*/
 str-init[94] = ",CSC,Southpak,TrePaper"  /* corsuply */
 str-init[95] = "Blank,GrossSH"  /* fgitemsf */
 str-init[96] = ",Michcor,Trilakes,Woodland,PremierPkg,St.Clair,NStock,Blue,Freedman"  /* GP */
 str-init[97] = "DC Only,OE & DC"   /* oeprep */
 str-init[98] = "None,1/8"  /* celayout */
 str-init[99] = "Fibre"
 str-init[100] = "Customer,CustX"  /* 1099-MISC */
 str-init[101] = ",Skip Open Order Calc"
 str-init[102] = ""
 str-init[103] = "Item,Vendor,All,NoTax"   /* aptax */ 
 str-init[104] = "None,Overrun,Receipts"  /* rmemails */     
 str-init[105] = ",loadtag"
 str-init[106] = "Fibrex"
 str-init[107] = ",Indiana,Dee"
 str-init[108] = "Workstation,Server"  
 str-init[109] = "Reorder Point"   
 str-init[110] = ",HOP,Xprint,AllWest,PremierPkg,SouleMed,Soule"  
 str-init[111] = ",Positive Pay"
 str-init[112] = ",DuplicateFGDayClient" 
 str-init[113] = ",Trailer" 
 str-init[114] = ",Overrun/Underrun"
 str-init[115] = ",NStock,PremierPkg,McElroy,PkgSpec,Bell"
 str-init[116] = "Today,Release Date"
 str-init[117] = ",Invoice Price,Order Price"
 str-init[118] = ",Partitions"
 str-init[119] = ""
 str-init[120] = "" 
 str-init[121] = ""
 str-init[122] = "ASIJobCL,MidYorkJobCL,CentBoxJobCL,ASIJobCR,ACPIJobCor"
 str-init[123] = "UnderRuns and OverRun,OverRuns Only"
 str-init[124] = "Profit,Cost Markup"
 str-init[128] = "BolCreation,Fixed Time".

ASSIGN str-init[125] = "Ship Only,Invoice Only,Bill and Ship,Transfer Only"
       str-init[126] = "FG Lot#,Whs/Bin"
       str-init[127] = "S,I,M,O,N"
       str-init[129] = "SHIPTO,FreightClass"
       str-init[130] = "Shipto Delivery Zone,FGFreightClass"
       str-init[131] = "Partitions,Premier"
       str-init[132] = "JobRequired,ScanTagOnly"
       str-init[133] = ",Fibre,Welsh"
       str-init[134] = ",Premier"
       str-init[135] = ",Qty-Item,Item-Qty"
       str-init[136] = ",TransferCost"
       str-init[137] = ",Run&Ship Prompt,DefaultOnly"
       str-init[138] = ",Batch,One BOL Only"
       str-init[139] = ",Exclude Finance Charges"
       str-init[140] = ",NoAdjustments"
       str-init[141] = "AckMaster 1,AckMaster 2" /*"3CPack"*/
       str-init[142] = "ASI,Badger"
       str-init[143] = ",Release Date,Promise Date,Order Line Due Date,Release Due Date"
       str-init[144] = ",Added,Deleted,OverUnder"
       str-init[145] = "Order Item Counts,Pallet Counts"
       str-init[146] = ",TEAL"
       str-init[147] = ",PremierPkg,McElroy,Bell,Capitol,Rudd,Trepaper,StClair"
       str-init[148] = ",NoWarning,OverShipWarning,UnderShipWarning,OverUnderShipWarning"
       str-init[149] = ",Job-Item,Item-Job"
       str-init[150] = ",RMLot"
       str-init[151] = "bolfmt1"
       str-init[152] = "Trailer#,ShipTo"
       str-init[153] = "Board Cost,Square Feet"
    .

FOR EACH ASI.item-spec FIELDS(CODE) WHERE
    ASI.item-spec.company = g_company AND
    ASI.item-spec.i-no = '' NO-LOCK  
    BREAK BY item-spec.CODE:

    IF FIRST-OF(item-spec.CODE) THEN
       str-init[58] = str-init[58] + 
                      IF str-init[58] <> "" THEN "," + item-spec.CODE 
                      ELSE item-spec.CODE.
END.


FOR EACH ASI.company  NO-LOCK :

    IF AVAIL ASI.company THEN DO:   
      ASSIGN
       str-init[119] = str-init[119] + 
                      IF str-init[119] <> "" THEN "," + company.company 
                      ELSE company.company
       str-init[120] = str-init[120] + 
                      IF str-init[120] <> "" THEN "," + company.company 
                      ELSE company.company
       str-init[121] = str-init[121] + 
                      IF str-init[121] <> "" THEN "," + company.company 
                      ELSE company.company.
    END.
END.

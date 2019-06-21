using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Web.Caching;
using ASINET;
using ASIDataNS;

using Progress.Open4GL.Proxy;

/// <summary>
/// Summary description for custitem
/// </summary>
[System.ComponentModel.DataObject]
public class Corrugated : AppServerConnect.AppServer
{
    public Corrugated()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrugateEstimateDataSet SelectCorrugateEstimate(string prmAction, string prmUser, string prmEstimate, string prmCust, string prmCustPart, string prmShipTo, string prmItemName, string prmFgItem, decimal prmEstQty, string prmStyle, string prmFlute, string prmTest, string prmBoard, decimal prmCalliper, string prmCategory, decimal prmLength, decimal prmWidth, decimal prmDepth, Int32 prmFrom, Int32 prmBlank, string prmTab, Int32 prmColor, Int32 prmPasses, Int32 prmCoating, Int32 prmCoatPasses, decimal prmQtySet, Int32 prmInkFrom, Int32 prmPassesFrom, Int32 prmCoatingFrom, Int32 prmCoatPassesFrom, string prmPurchManuf, DateTime prmEstDate, string prmType, string prmMassType, decimal prmEstQty2, decimal prmEstQty3, decimal prmEstQty4, decimal prmEstQty5, decimal prmEstQty6, decimal prmEstQty7, decimal prmEstQty8, decimal prmEstQty9, decimal prmEstQty10, decimal prmEstQty11, decimal prmEstQty12, decimal prmEstQty13, decimal prmEstQty14, decimal prmEstQty15, decimal prmEstQty16, decimal prmEstQty17, decimal prmEstQty18, decimal prmEstQty19, decimal prmEstQty20, decimal prmRelQty1, decimal prmRelQty2, decimal prmRelQty3, decimal prmRelQty4, decimal prmRelQty5, decimal prmRelQty6, decimal prmRelQty7, decimal prmRelQty8, decimal prmRelQty9, decimal prmRelQty10, decimal prmRelQty11, decimal prmRelQty12, decimal prmRelQty13, decimal prmRelQty14, decimal prmRelQty15, decimal prmRelQty16, decimal prmRelQty17, decimal prmRelQty18, decimal prmRelQty19, decimal prmRelQty20, string prmlvcopied)
    {
        string cError = "";
        dsCorrugateEstimateDataSet dsCorrugateEstimate = new dsCorrugateEstimateDataSet();
        dsCorrugateEstimate = null;
        AppServerConnect();
        aoObject.CorrugateEstimate(prmAction, prmUser, prmEstimate, prmCust, prmCustPart, prmShipTo, prmItemName, prmFgItem, prmEstQty, prmStyle, prmFlute, prmTest, prmBoard, prmCalliper, prmCategory, prmLength, prmWidth, prmDepth, prmFrom, prmBlank, prmTab, prmColor, prmPasses, prmCoating, prmCoatPasses, prmQtySet, prmInkFrom, prmPassesFrom, prmCoatingFrom, prmCoatPassesFrom, prmPurchManuf, prmEstDate, prmType, prmMassType, prmEstQty2, prmEstQty3, prmEstQty4, prmEstQty5, prmEstQty6, prmEstQty7, prmEstQty8, prmEstQty9, prmEstQty10, prmEstQty11, prmEstQty12, prmEstQty13, prmEstQty14, prmEstQty15, prmEstQty16, prmEstQty17, prmEstQty18, prmEstQty19, prmEstQty20, prmRelQty1, prmRelQty2, prmRelQty3, prmRelQty4, prmRelQty5, prmRelQty6, prmRelQty7, prmRelQty8, prmRelQty9, prmRelQty10, prmRelQty11, prmRelQty12, prmRelQty13, prmRelQty14, prmRelQty15, prmRelQty16, prmRelQty17, prmRelQty18, prmRelQty19, prmRelQty20,prmlvcopied, out cError, ref dsCorrugateEstimate);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }

        return dsCorrugateEstimate;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrugatedBoxDataSet SelectCorrugatedBox(string prmUser, string prmAction, string prmType, string prmComp, string prmEstimate, string prmCustomer, string prmCustpart, string prmFgItem, string prmStyle, string prmShipTo, string prmDie, string prmCad, string prmPlate, string prmSingle, string prmSet, string prmTandem, decimal prmWidthFrom, decimal prmWidthTo, decimal prmLenFrom, decimal prmLenTo, decimal prmDepFrom, decimal prmDepTo, string prmPartDscr)
    {        
        dsCorrugatedBoxDataSet dsCorrugatedBox = new dsCorrugatedBoxDataSet();
        dsCorrugatedBox = null;
        AppServerConnect();
        aoObject.BrwsCEstimate(prmUser, prmAction,prmType, prmComp, prmEstimate, prmCustomer, prmCustpart, prmFgItem, prmStyle, prmShipTo, prmDie, prmCad, prmPlate, prmSingle, prmSet, prmTandem, prmWidthFrom, prmWidthTo, prmLenFrom, prmLenTo, prmDepFrom, prmDepTo, prmPartDscr, ref dsCorrugatedBox);
        AppServerDisconnect();

        return dsCorrugatedBox;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFluteLookDataSet FluteCode(string prmAction, string prmUser,string prmField,string prmText)
    {
        dsFluteLookDataSet dsFluteLook = new dsFluteLookDataSet();
        dsFluteLook = null;
        AppServerConnect();
        aoObject.FluteLookup(prmAction, prmUser, prmField, prmText, ref dsFluteLook);
        AppServerDisconnect();

        return dsFluteLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTestLookDataSet TestCode(string prmAction, string prmField, string prmText, string prmUser, string prmComp, string prmLoc, string prmFlute,string prmStyle)
    {
        dsTestLookDataSet dsTestLook = new dsTestLookDataSet();
        dsTestLook = null;
        AppServerConnect();
        aoObject.TestLookup(prmAction, prmField, prmText, prmUser, prmComp, prmLoc, prmFlute,prmStyle, ref dsTestLook);
        AppServerDisconnect();

        return dsTestLook;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldingEstimateDataSet SelectFoldingEstimate(string prmAction, string prmUser, string prmEstimate, string prmCust, string prmCustPart, string prmShipTo, string prmItemName, string prmFgItem, decimal prmEstQty, string prmStyle, string prmPaper1, string prmPaper2, string prmBoard, decimal prmCalliper, string prmCategory, decimal prmLength, decimal prmWidth, decimal prmDepth, Int32 prmFrom, Int32 prmBlank, string prmTab, Int32 prmColor, Int32 prmPasses, Int32 prmCoating, Int32 prmCoatPasses, decimal prmQtySet, Int32 prmInkFrom, Int32 prmPassesFrom, Int32 prmCoatingFrom, Int32 prmCoatPassesFrom, string prmPurchManuf, DateTime prmEstDate, string prmType, decimal prmDiein, decimal prmEstQty2, decimal prmEstQty3, decimal prmEstQty4, decimal prmEstQty5, decimal prmEstQty6, decimal prmEstQty7, decimal prmEstQty8, decimal prmEstQty9, decimal prmEstQty10, decimal prmEstQty11, decimal prmEstQty12, decimal prmEstQty13, decimal prmEstQty14, decimal prmEstQty15, decimal prmEstQty16, decimal prmEstQty17, decimal prmEstQty18, decimal prmEstQty19, decimal prmEstQty20, decimal prmRelQty1, decimal prmRelQty2, decimal prmRelQty3, decimal prmRelQty4, decimal prmRelQty5, decimal prmRelQty6, decimal prmRelQty7, decimal prmRelQty8, decimal prmRelQty9, decimal prmRelQty10, decimal prmRelQty11, decimal prmRelQty12, decimal prmRelQty13, decimal prmRelQty14, decimal prmRelQty15, decimal prmRelQty16, decimal prmRelQty17, decimal prmRelQty18, decimal prmRelQty19, decimal prmRelQty20)
    {
        string cError = "";
        dsFoldingEstimateDataSet dsFoldingEstimate = new dsFoldingEstimateDataSet();
        dsFoldingEstimate = null;
        AppServerConnect();
        aoObject.FoldingEstimate(prmAction, prmUser, prmEstimate, prmCust, prmCustPart, prmShipTo, prmItemName, prmFgItem, prmEstQty, prmStyle, prmPaper1, prmPaper2, prmBoard, prmCalliper, prmCategory, prmLength, prmWidth, prmDepth, prmFrom, prmBlank, prmTab, prmColor, prmPasses, prmCoating, prmCoatPasses, prmQtySet, prmInkFrom, prmPassesFrom, prmCoatingFrom, prmCoatPassesFrom, prmPurchManuf, prmEstDate, prmType, prmDiein, prmEstQty2, prmEstQty3, prmEstQty4, prmEstQty5, prmEstQty6, prmEstQty7, prmEstQty8, prmEstQty9, prmEstQty10, prmEstQty11, prmEstQty12, prmEstQty13, prmEstQty14, prmEstQty15, prmEstQty16, prmEstQty17, prmEstQty18, prmEstQty19, prmEstQty20, prmRelQty1, prmRelQty2, prmRelQty3, prmRelQty4, prmRelQty5, prmRelQty6, prmRelQty7, prmRelQty8, prmRelQty9, prmRelQty10, prmRelQty11, prmRelQty12, prmRelQty13, prmRelQty14, prmRelQty15, prmRelQty16, prmRelQty17, prmRelQty18, prmRelQty19, prmRelQty20, out cError, ref dsFoldingEstimate);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsFoldingEstimate;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrSpecsDataSet CorrugatedSpecs(string prmUser, string prmAction, string prmCustNum, string prmShipTo, string prmFgItem, string prmCustPart, string prmItemName, string prmPartDscr, string prmDieNum, string prmCadNum, string prmSpcNum, string prmPlateNum, string prmImage, string prmUpcNum, string prmSman, string prmSmanDscr, decimal prmComm, string prmFgCat, string prmFgCatDscr, string prmStyle, string prmStyDscr, string prmBoard, string prmBrdDscr, decimal prmLength, decimal prmWidth, decimal prmDepth, string prmFlute, string prmTest, string prmTab, string prmMetric, string prmJointMat, decimal prmDustFlap, decimal prmBotFlap, decimal prmLockTab, decimal prmTabWid, decimal prmScWid, decimal prmScLen, decimal prmTuck, decimal prmJointLen, decimal prmBlankWid, decimal prmBlankLen, decimal prmBlankSqFt, string prmEstNum, DateTime prmFromDate, DateTime prmEstDate, DateTime prmModDate, Int32 prmOrderNum, DateTime prmOrdDate, decimal prmQty, decimal prmQtySet, decimal prmMsf, string prmShipName, string prmAddr, string prmAddr2, string prmCity, string prmState, string prmZip, Int32 prmFormno, string prmEstFrom, Int32 prmBlankno, string prmAutocalcSelected)
    {
        string cError = "";
        dsCorrSpecsDataSet dsCorrSpecs = new dsCorrSpecsDataSet();
        dsCorrSpecs = null;
        AppServerConnect();
        aoObject.CorrSpecs(prmUser, prmAction, prmCustNum, prmShipTo, prmFgItem, prmCustPart, prmItemName, prmPartDscr, prmDieNum, prmCadNum, prmSpcNum, prmPlateNum, prmImage, prmUpcNum, prmSman, prmSmanDscr, prmComm, prmFgCat, prmFgCatDscr, prmStyle, prmStyDscr, prmBoard, prmBrdDscr, prmLength, prmWidth, prmDepth, prmFlute, prmTest, prmTab, prmMetric, prmJointMat, prmDustFlap, prmBotFlap, prmLockTab, prmTabWid, prmScWid, prmScLen, prmTuck, prmJointLen, prmBlankWid, prmBlankLen, prmBlankSqFt, prmEstNum, prmFromDate, prmEstDate, prmModDate, prmOrderNum, prmOrdDate, prmQty, prmQtySet, prmMsf, prmShipName, prmAddr, prmAddr2, prmCity, prmState, prmZip, prmFormno,prmEstFrom,prmBlankno,prmAutocalcSelected, ref dsCorrSpecs, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCorrSpecs;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorCustomerDataSet SelectCust(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        dsCorCustomerDataSet dsCorCustomer = new dsCorCustomerDataSet();
        dsCorCustomer = null;
        AppServerConnect();
        aoObject.CorCustLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsCorCustomer);
        AppServerDisconnect();

        return dsCorCustomer;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorStyleLookDataSet Selectstyle(string prmAction, string prmUser, string prmCondition, string prmText, string prmInd)
    {
        dsCorStyleLookDataSet dsCorStyleLook = new dsCorStyleLookDataSet();
        dsCorStyleLook = null;
        AppServerConnect();
        aoObject.CorStyleLook(prmAction, prmUser,  prmCondition, prmText,prmInd, ref dsCorStyleLook);
        AppServerDisconnect();

        return dsCorStyleLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorFgCatLookDataSet SelectFGCat(string prmAction, string prmUser,  string prmCondition, string prmText)
    {
        dsCorFgCatLookDataSet dsCorFgCatLook = new dsCorFgCatLookDataSet();
        dsCorFgCatLook = null;
        AppServerConnect();
        aoObject.CorFgCatLook(prmAction, prmUser,  prmCondition, prmText, ref dsCorFgCatLook);
        AppServerDisconnect();

        return dsCorFgCatLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorLayoutBoardLookDataSet SelectBoard(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmIndustry, string prmEstimate, string prmStyle)
    {
        dsCorLayoutBoardLookDataSet dsCorBoardLook = new dsCorLayoutBoardLookDataSet();
        dsCorBoardLook = null;
        AppServerConnect();
        aoObject.CorLayoutBoardLook(prmAction, prmUser, prmField, prmCondition, prmText,prmIndustry,prmEstimate,prmStyle, ref dsCorBoardLook);
        AppServerDisconnect();

        return dsCorBoardLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorFgCustpartDataSet Selectcustpart(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        dsCorFgCustpartDataSet dsCorFgCustpart = new dsCorFgCustpartDataSet();
        dsCorFgCustpart = null;
        AppServerConnect();
        aoObject.FGCustPartLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsCorFgCustpart);
        AppServerDisconnect();

        return dsCorFgCustpart;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorFgItemDataSet SelectFGItem(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        dsCorFgItemDataSet dsCorFgItem = new dsCorFgItemDataSet();
        dsCorFgItem = null;
        AppServerConnect();
        aoObject.CorFgItemLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsCorFgItem);
        AppServerDisconnect();

        return dsCorFgItem;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrLayoutDataSet SelectCorrugateLayout(string prmUser, string prmAction, string prmType, string prmComp, string prmEstimate, DateTime prmEstDate, Int32 prmForm, Int32 prmFormQty, string prmCustPart, string prmMachine, string prmMachDscr, decimal prmFrontBack, decimal prmSideSide, string prmRevCorr, string prmBoard, string prmBoardName, string prmReal, string prmFlute, string prmTest, decimal prmCostMsf, string prmCostUom, decimal prmWeightt, decimal prmFreightMsf, string prmFreightUom, string prmNc, decimal prmGrosShetWid, decimal prmGrosShetLen, decimal prmGrosShetDep, decimal prmOutWid, decimal prmOutLen, decimal prmOutDep, Int32 prmOutCut, string prmOutTotalUp, string prmOutSqFeet, decimal prmDieInches, decimal prmNetShetWid, decimal prmNetShetLen, decimal prmNetShetDep, decimal prmDieSizeWid, decimal prmDieSizeLen, decimal prmDieSizeDep, decimal prmOnWid, decimal prmOnLen, decimal prmOnDep, Int32 prmOnTotalUp, decimal prmOnSqFeet, decimal prmBlankWid, decimal prmBlankLen, decimal prmBlankDep, string prmAdder1, string prmAdder2, string prmAdder3, string prmAdder4, string prmAdder5, string prmAdder6, string prmAdder7, string prmAdder8, string prmAdder9, string prmAdder10, string prmAdder11, string prmAdder12, string prmWaxLabel1, string prmWaxDesc1, Int32 prmS1, Int32 prmB1, decimal prmLeafWid1, decimal prmLeafLen1, string prmWaxLabel2, string prmWaxDesc2, Int32 prmS2, Int32 prmB2, decimal prmLeafWid2, decimal prmLeafLen2, string prmWaxLabel3, string prmWaxDesc3, Int32 prmS3, Int32 prmB3, decimal prmLeafWid3, decimal prmLeafLen3, string prmWaxLabel4, string prmWaxDesc4, Int32 prmS4, Int32 prmB4, decimal prmLeafWid4, decimal prmLeafLen4, Int32 prmBlankno)
    {
        string cError = "";
        dsCorrLayoutDataSet dsCorrLayout = new dsCorrLayoutDataSet();
        dsCorrLayout = null;
        AppServerConnect();
        aoObject.CorrLayout(prmUser, prmAction, prmType, prmComp, prmEstimate, prmEstDate, prmForm, prmFormQty, prmCustPart, prmMachine, prmMachDscr, prmFrontBack, prmSideSide, prmRevCorr, prmBoard, prmBoardName, prmReal, prmFlute, prmTest, prmCostMsf, prmCostUom, prmWeightt, prmFreightMsf, prmFreightUom, prmNc, prmGrosShetWid, prmGrosShetLen, prmGrosShetDep, prmOutWid, prmOutLen, prmOutDep, prmOutCut, prmOutTotalUp, prmOutSqFeet, prmDieInches, prmNetShetWid, prmNetShetLen, prmNetShetDep, prmDieSizeWid, prmDieSizeLen, prmDieSizeDep, prmOnWid, prmOnLen, prmOnDep, prmOnTotalUp, prmOnSqFeet, prmBlankWid, prmBlankLen, prmBlankDep, prmAdder1, prmAdder2, prmAdder3, prmAdder4, prmAdder5, prmAdder6, prmAdder7, prmAdder8, prmAdder9, prmAdder10, prmAdder11, prmAdder12, prmWaxLabel1, prmWaxDesc1, prmS1, prmB1, prmLeafWid1, prmLeafLen1, prmWaxLabel2, prmWaxDesc2, prmS2, prmB2, prmLeafWid2, prmLeafLen2, prmWaxLabel3, prmWaxDesc3, prmS3, prmB3, prmLeafWid3, prmLeafLen3, prmWaxLabel4, prmWaxDesc4, prmS4, prmB4, prmLeafWid4, prmLeafLen4,prmBlankno, out cError, ref dsCorrLayout);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCorrLayout;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateCorrLayout(string prmUser, string prmAction, string prmType, string prmComp, string prmEstimate, DateTime prmEstDate, Int32 prmForm, Int32 prmFormQty, string prmCustPart, string prmMachine, string prmMachDscr, decimal prmFrontBack, decimal prmSideSide, string prmRevCorr, string prmBoard, string prmBoardName, string prmReal, string prmFlute, string prmTest, decimal prmCostMsf, string prmCostUom, decimal prmWeightt, decimal prmFreightMsf, string prmFreightUom, string prmNc, decimal prmGrosShetWid, decimal prmGrosShetLen, decimal prmGrosShetDep, decimal prmOutWid, decimal prmOutLen, decimal prmOutDep, Int32 prmOutCut, string prmOutTotalUp, string prmOutSqFeet, decimal prmDieInches, decimal prmNetShetWid, decimal prmNetShetLen, decimal prmNetShetDep, decimal prmDieSizeWid, decimal prmDieSizeLen, decimal prmDieSizeDep, decimal prmOnWid, decimal prmOnLen, decimal prmOnDep, Int32 prmOnTotalUp, decimal prmOnSqFeet, decimal prmBlankWid, decimal prmBlankLen, decimal prmBlankDep, string prmAdder1, string prmAdder2, string prmAdder3, string prmAdder4, string prmAdder5, string prmAdder6, string prmAdder7, string prmAdder8, string prmAdder9, string prmAdder10, string prmAdder11, string prmAdder12, string prmWaxLabel1, string prmWaxDesc1, Int32 prmS1, Int32 prmB1, decimal prmLeafWid1, decimal prmLeafLen1, string prmWaxLabel2, string prmWaxDesc2, Int32 prmS2, Int32 prmB2, decimal prmLeafWid2, decimal prmLeafLen2, string prmWaxLabel3, string prmWaxDesc3, Int32 prmS3, Int32 prmB3, decimal prmLeafWid3, decimal prmLeafLen3, string prmWaxLabel4, string prmWaxDesc4, Int32 prmS4, Int32 prmB4, decimal prmLeafWid4, decimal prmLeafLen4, Int32 prmBlankno)
    {
        string cError = "";
        dsCorrLayoutDataSet dsCorrLayout = new dsCorrLayoutDataSet();
        AppServerConnect();
        aoObject.CorrLayout(prmUser, prmAction, prmType, prmComp, prmEstimate, prmEstDate, prmForm, prmFormQty, prmCustPart, prmMachine, prmMachDscr, prmFrontBack, prmSideSide, prmRevCorr, prmBoard, prmBoardName, prmReal, prmFlute, prmTest, prmCostMsf, prmCostUom, prmWeightt, prmFreightMsf, prmFreightUom, prmNc, prmGrosShetWid, prmGrosShetLen, prmGrosShetDep, prmOutWid, prmOutLen, prmOutDep, prmOutCut, prmOutTotalUp, prmOutSqFeet, prmDieInches, prmNetShetWid, prmNetShetLen, prmNetShetDep, prmDieSizeWid, prmDieSizeLen, prmDieSizeDep, prmOnWid, prmOnLen, prmOnDep, prmOnTotalUp, prmOnSqFeet, prmBlankWid, prmBlankLen, prmBlankDep, prmAdder1, prmAdder2, prmAdder3, prmAdder4, prmAdder5, prmAdder6, prmAdder7, prmAdder8, prmAdder9, prmAdder10, prmAdder11, prmAdder12, prmWaxLabel1, prmWaxDesc1, prmS1, prmB1, prmLeafWid1, prmLeafLen1, prmWaxLabel2, prmWaxDesc2, prmS2, prmB2, prmLeafWid2, prmLeafLen2, prmWaxLabel3, prmWaxDesc3, prmS3, prmB3, prmLeafWid3, prmLeafLen3, prmWaxLabel4, prmWaxDesc4, prmS4, prmB4, prmLeafWid4, prmLeafLen4, prmBlankno, out cError, ref dsCorrLayout);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            return false;
        }
        else
        {
            return true;
        }

    }
    

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldSpecsDataSet FoldingSpecs(string prmUser, string prmAction, string prmCustNum, string prmShipTo, string prmFgItem, string prmCustPart, string prmItemName, string prmPartDscr, string prmDieNum, string prmCadNum, string prmSpcNum, string prmPlateNum, string prmImage, string prmUpcNum, string prmSman, string prmSmanDscr, decimal prmComm, string prmFgCat, string prmFgCatDscr, string prmStyle, string prmStyDscr, string prmBoard, string prmBrdDscr, decimal prmLength, decimal prmWidth, decimal prmDepth, string prmMetric, string prmAdhe, decimal prmDustFlap, decimal prmPanel, decimal prmLockTab, decimal prmGlue, decimal prmScWid, decimal prmScLen, decimal prmTuck, decimal prmLinInc, decimal prmBlankWid, decimal prmBlankLen, decimal prmBlankSqFt, string prmEstNum, DateTime prmFromDate, DateTime prmEstDate, DateTime prmModDate, Int32 prmOrderNum, DateTime prmOrdDate, decimal prmQty, string prmShipName, string prmAddr, string prmAddr2, string prmCity, string prmState, string prmZip, Int32 prmFormno, string prmEstFrom, Int32 prmBlankno, string prmAutocalcSelected)
    {
        string cError = "";
        dsFoldSpecsDataSet dsFoldSpecs = new dsFoldSpecsDataSet();
        dsFoldSpecs = null;
        AppServerConnect();
        aoObject.FoldSpecs(prmUser, prmAction, prmCustNum, prmShipTo, prmFgItem, prmCustPart, prmItemName, prmPartDscr, prmDieNum, prmCadNum, prmSpcNum, prmPlateNum, prmImage, prmUpcNum, prmSman, prmSmanDscr, prmComm, prmFgCat, prmFgCatDscr, prmStyle, prmStyDscr, prmBoard, prmBrdDscr, prmLength, prmWidth, prmDepth,  prmMetric, prmAdhe, prmDustFlap, prmPanel, prmLockTab, prmGlue, prmScWid, prmScLen, prmTuck, prmLinInc, prmBlankWid, prmBlankLen, prmBlankSqFt, prmEstNum, prmFromDate, prmEstDate, prmModDate, prmOrderNum, prmOrdDate, prmQty, prmShipName, prmAddr, prmAddr2, prmCity, prmState, prmZip, prmFormno,prmEstFrom,prmBlankno,prmAutocalcSelected, ref dsFoldSpecs, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsFoldSpecs;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsAdhesiveLookDataSet SelectAdhesive(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmType, string prmIndustry, string prmEstimate)
    {
        dsAdhesiveLookDataSet dsAdhesiveLook = new dsAdhesiveLookDataSet();
        dsAdhesiveLook = null;
        AppServerConnect();
        aoObject.AdhesiveLook(prmAction, prmUser, prmField, prmCondition, prmText, prmType, prmIndustry,prmEstimate, ref dsAdhesiveLook);
        AppServerDisconnect();

        return dsAdhesiveLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsAdderLookupDataSet SelectAdder(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmIndustry, string prmEstimate)
    {
        dsAdderLookupDataSet dsAdderLookup = new dsAdderLookupDataSet();
        dsAdderLookup = null;
        AppServerConnect();
        aoObject.AdderLookup(prmAction, prmUser, prmField, prmCondition, prmText, prmIndustry, prmEstimate, ref dsAdderLookup);
        AppServerDisconnect();

        return dsAdderLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrInksDataSet CorrInks(string prmUser, string prmAction, string prmComp, string prmEstNum, Int32 prmFormNo, Int32 prmColor, Int32 prmPass, Int32 prmCoat, Int32 prmCoatPass, string prmDscr, Int32 prmPs1, Int32 prmPs2, Int32 prmPs3, Int32 prmPs4, Int32 prmPs5, Int32 prmPs6, Int32 prmPs7, Int32 prmPs8, Int32 prmPs9, Int32 prmPs10, string prmCode1, string prmCode2, string prmCode3, string prmCode4, string prmCode5, string prmCode6, string prmCode7, string prmCode8, string prmCode9, string prmCode10, string prmDscr1, string prmDscr2, string prmDscr3, string prmDscr4, string prmDscr5, string prmDscr6, string prmDscr7, string prmDscr8, string prmDscr9, string prmDscr10, Int32 prmPer1, Int32 prmPer2, Int32 prmPer3, Int32 prmPer4, Int32 prmPer5, Int32 prmPer6, Int32 prmPer7, Int32 prmPer8, Int32 prmPer9, Int32 prmPer10, string prmPackCode, decimal prmUnitLen, decimal prmCostEa, decimal prmUnitWid, Int32 prmBoxCode, decimal prmUnitDep, Int32 prmBundl, decimal prmWTPack, string prmUnit, decimal prmCost2, Int32 prmCount, decimal prmLength, decimal prmWidth, decimal prmHeight, Int32 prmLayer, Int32 prmStack, string prmStCode, string prmFrCharge, decimal prmWeightPer, string prmCarrier, string prmCarrDscr, string prmDelZon, decimal prmFreight, decimal prmFreight2, Int32 prmBlankno)
    {
        string cError = "";
        dsCorrInksDataSet dsCorrInks = new dsCorrInksDataSet();
        dsCorrInks = null;
        AppServerConnect();
        aoObject.CorrInks( prmUser,  prmAction,  prmComp,  prmEstNum,  prmFormNo,  prmColor,  prmPass,  prmCoat,  prmCoatPass,  prmDscr,  prmPs1,  prmPs2,  prmPs3,  prmPs4,  prmPs5,  prmPs6,  prmPs7,  prmPs8,  prmPs9,  prmPs10,  prmCode1,  prmCode2,  prmCode3,  prmCode4,  prmCode5,  prmCode6,  prmCode7,  prmCode8,  prmCode9,  prmCode10,  prmDscr1,  prmDscr2,  prmDscr3,  prmDscr4,  prmDscr5,  prmDscr6,  prmDscr7,  prmDscr8,  prmDscr9,  prmDscr10,  prmPer1,  prmPer2,  prmPer3,  prmPer4,  prmPer5,  prmPer6,  prmPer7,  prmPer8,  prmPer9,  prmPer10,  prmPackCode,  prmUnitLen,  prmCostEa,  prmUnitWid,  prmBoxCode,  prmUnitDep,  prmBundl,  prmWTPack,  prmUnit,  prmCost2,  prmCount,  prmLength,  prmWidth,  prmHeight,  prmLayer,  prmStack,  prmStCode,  prmFrCharge,  prmWeightPer,  prmCarrier,  prmCarrDscr,  prmDelZon,  prmFreight,  prmFreight2,prmBlankno, ref dsCorrInks, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCorrInks;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsDelZoneLookDataSet SelectZone(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmCarrier)
    {
        dsDelZoneLookDataSet dsDelZoneLook = new dsDelZoneLookDataSet();
        dsDelZoneLook = null;
        AppServerConnect();
        aoObject.ZoneLook(prmAction, prmUser, prmField, prmCondition, prmText, prmCarrier, ref dsDelZoneLook);
        AppServerDisconnect();

        return dsDelZoneLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsMachineLookupDataSet SelectMachine(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmEstimate, Int32 prmForm, Int32 prmBlankno, string prmBoard, string prmXgrain)
    {
        dsMachineLookupDataSet dsMachineLookup = new dsMachineLookupDataSet();
        dsMachineLookup = null;
        AppServerConnect();
        aoObject.MachineLookup(prmAction, prmUser, prmField, prmCondition, prmText,prmEstimate, prmForm,prmBlankno, prmBoard,prmXgrain , ref dsMachineLookup);
        AppServerDisconnect();

        return dsMachineLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldInksDataSet FoldInks(string prmUser, string prmAction, string prmComp, string prmEstNum, Int32 prmFormNo, Int32 prmColor, Int32 prmPass, Int32 prmCoat, Int32 prmCoatPass, string prmDscr, Int32 prmPs1, Int32 prmPs2, Int32 prmPs3, Int32 prmPs4, Int32 prmPs5, Int32 prmPs6, Int32 prmPs7, Int32 prmPs8, Int32 prmPs9, Int32 prmPs10, Int32 prmPs11, Int32 prmPs12, Int32 prmPs13, Int32 prmPs14, Int32 prmPs15, string prmCode1, string prmCode2, string prmCode3, string prmCode4, string prmCode5, string prmCode6, string prmCode7, string prmCode8, string prmCode9, string prmCode10, string prmCode11, string prmCode12, string prmCode13, string prmCode14, string prmCode15, string prmDscr1, string prmDscr2, string prmDscr3, string prmDscr4, string prmDscr5, string prmDscr6, string prmDscr7, string prmDscr8, string prmDscr9, string prmDscr10, string prmDscr11, string prmDscr12, string prmDscr13, string prmDscr14, string prmDscr15, Int32 prmPer1, Int32 prmPer2, Int32 prmPer3, Int32 prmPer4, Int32 prmPer5, Int32 prmPer6, Int32 prmPer7, Int32 prmPer8, Int32 prmPer9, Int32 prmPer10, Int32 prmPer11, Int32 prmPer12, Int32 prmPer13, Int32 prmPer14, Int32 prmPer15, decimal prmUnit1, decimal prmUnit2, decimal prmUnit3, decimal prmUnit4, decimal prmUnit5, decimal prmUnit6, decimal prmUnit7, decimal prmUnit8, decimal prmUnit9, decimal prmUnit10, decimal prmUnit11, decimal prmUnit12, decimal prmUnit13, decimal prmUnit14, decimal prmUnit15, string prmPackCode, decimal prmUnitLen, decimal prmUnitWid, decimal prmUnitDep, string prmLayerPad, decimal prmLayerLen, decimal prmLayerWid, decimal prmLayerDep, string prmDivider, decimal prmDividerLen, decimal prmDividerWid, decimal prmDividerDep, decimal prmCostEa, Int32 prmBoxCode, Int32 prmPallet, decimal prmWTUnit, decimal prmPackQty, decimal prmLayerQty, decimal prmDivQty, string prmUnit, decimal prmCost2, Int32 prmCount, decimal prmLength, decimal prmWidth, decimal prmHeight, Int32 prmLayer, string prmNote, string prmFrCharge, decimal prmWeightPer, string prmCarrier, string prmCarrDscr, string prmDelZon, decimal prmFreight, decimal prmFreight2, Int32 prmBlankno, string prmSide1, string prmSide2, string prmSide3, string prmSide4, string prmSide5, string prmSide6, string prmSide7, string prmSide8, string prmSide9, string prmSide10, string prmSide11, string prmSide12, string prmSide13, string prmSide14, string prmSide15)
    {
        string cError = "";
        dsFoldInksDataSet dsFoldInks = new dsFoldInksDataSet();
        dsFoldInks = null;
        AppServerConnect();
        aoObject.FoldInks( prmUser, prmAction,  prmComp,  prmEstNum,  prmFormNo,  prmColor,  prmPass,  prmCoat,  prmCoatPass,  prmDscr,  prmPs1,  prmPs2,  prmPs3,  prmPs4,  prmPs5,  prmPs6,  prmPs7,  prmPs8,  prmPs9,  prmPs10,  prmPs11,  prmPs12,  prmPs13,  prmPs14,  prmPs15,  prmCode1,  prmCode2,  prmCode3,  prmCode4,  prmCode5,  prmCode6,  prmCode7,  prmCode8,  prmCode9,  prmCode10,  prmCode11,  prmCode12,  prmCode13,  prmCode14,  prmCode15,  prmDscr1,  prmDscr2,  prmDscr3,  prmDscr4,  prmDscr5,  prmDscr6,  prmDscr7,  prmDscr8,  prmDscr9,  prmDscr10,  prmDscr11,  prmDscr12,  prmDscr13,  prmDscr14,  prmDscr15,  prmPer1,  prmPer2,  prmPer3,  prmPer4,  prmPer5,  prmPer6,  prmPer7,  prmPer8,  prmPer9,  prmPer10,  prmPer11,  prmPer12,  prmPer13,  prmPer14,  prmPer15,  prmUnit1,  prmUnit2,  prmUnit3,  prmUnit4,  prmUnit5,  prmUnit6,  prmUnit7,  prmUnit8,  prmUnit9,  prmUnit10,  prmUnit11,  prmUnit12,  prmUnit13,  prmUnit14,  prmUnit15,  prmPackCode,  prmUnitLen,  prmUnitWid,  prmUnitDep,  prmLayerPad,  prmLayerLen,  prmLayerWid,  prmLayerDep,  prmDivider,  prmDividerLen,  prmDividerWid,  prmDividerDep,  prmCostEa,  prmBoxCode,  prmPallet,  prmWTUnit,  prmPackQty,  prmLayerQty,  prmDivQty,  prmUnit,  prmCost2,  prmCount,  prmLength,  prmWidth,  prmHeight,  prmLayer,  prmNote,  prmFrCharge,  prmWeightPer,  prmCarrier,  prmCarrDscr,  prmDelZon,  prmFreight,  prmFreight2, prmBlankno, prmSide1, prmSide2, prmSide3, prmSide4, prmSide5, prmSide6, prmSide7, prmSide8, prmSide9, prmSide10, prmSide11, prmSide12, prmSide13, prmSide14, prmSide15, ref dsFoldInks, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsFoldInks;
    }





    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldLayoutDataSet SelectFoldLayout(string prmUser, string prmAction, string prmType, string prmComp, string prmEstimate, DateTime prmEstDate, Int32 prmForm, Int32 prmFormQty, string prmCustPart, string prmMachine, string prmMachDscr, decimal prmFrontBack, decimal prmSideSide, string prmXgrain, string prmBoard, string prmBoardName, string prmReal, decimal prmCaliper, decimal prmCostMsf, string prmCostUom, decimal prmWeightt, decimal prmFreightMsf, string prmFreightUom, string prmNc, string prmRoll, decimal prmRollWid, decimal prmGrosShetWid, decimal prmGrosShetLen, decimal prmOutWid, decimal prmOutLen, Int32 prmOutCut, decimal prmDieInches, decimal prmMachFeedWid, decimal prmMachFeedLen, decimal prmDieSizeWid, decimal prmDieSizeLen, decimal prmOnWid, decimal prmOnLen, decimal prmOnTotalUp, decimal prmBlankWid, decimal prmBlankLen, decimal prmBlankSqInch, string prmLeaf1, string prmLeaf2, string prmLeaf3, string prmLeaf4, string prmLeafDesc1, string prmLeafDesc2, string prmLeafDesc3, string prmLeafDesc4, Int32 prmS1, Int32 prmB1, decimal prmLeafWid1, decimal prmLeafLen1, Int32 prmS2, Int32 prmB2, decimal prmLeafWid2, decimal prmLeafLen2, Int32 prmS3, Int32 prmB3, decimal prmLeafWid3, decimal prmLeafLen3, Int32 prmS4, Int32 prmB4, decimal prmLeafWid4, decimal prmLeafLen4, Int32 prmBlankno, string prmAuto)
    {
        string cError = "";
        dsFoldLayoutDataSet dsFoldLayout = new dsFoldLayoutDataSet();
        dsFoldLayout = null;
        AppServerConnect();
        aoObject.FoldLayout(prmUser, prmAction, prmType, prmComp, prmEstimate, prmEstDate, prmForm, prmFormQty, prmCustPart, prmMachine, prmMachDscr, prmFrontBack, prmSideSide, prmXgrain, prmBoard, prmBoardName, prmReal, prmCaliper, prmCostMsf, prmCostUom, prmWeightt, prmFreightMsf, prmFreightUom, prmNc, prmRoll, prmRollWid, prmGrosShetWid, prmGrosShetLen, prmOutWid, prmOutLen, prmOutCut, prmDieInches, prmMachFeedWid, prmMachFeedLen, prmDieSizeWid, prmDieSizeLen, prmOnWid, prmOnLen, prmOnTotalUp, prmBlankWid, prmBlankLen, prmBlankSqInch, prmLeaf1, prmLeaf2, prmLeaf3, prmLeaf4, prmLeafDesc1, prmLeafDesc2, prmLeafDesc3, prmLeafDesc4, prmS1, prmB1, prmLeafWid1, prmLeafLen1, prmS2, prmB2, prmLeafWid2, prmLeafLen2, prmS3, prmB3, prmLeafWid3, prmLeafLen3, prmS4, prmB4, prmLeafWid4, prmLeafLen4,prmBlankno,prmAuto, out cError, ref dsFoldLayout);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsFoldLayout;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateFoldLayout(string prmUser, string prmAction, string prmType, string prmComp, string prmEstimate, DateTime prmEstDate, Int32 prmForm, Int32 prmFormQty, string prmCustPart, string prmMachine, string prmMachDscr, decimal prmFrontBack, decimal prmSideSide, string prmXgrain, string prmBoard, string prmBoardName, string prmReal, decimal prmCaliper, decimal prmCostMsf, string prmCostUom, decimal prmWeightt, decimal prmFreightMsf, string prmFreightUom, string prmNc, string prmRoll, decimal prmRollWid, decimal prmGrosShetWid, decimal prmGrosShetLen, decimal prmOutWid, decimal prmOutLen, Int32 prmOutCut, decimal prmDieInches, decimal prmMachFeedWid, decimal prmMachFeedLen, decimal prmDieSizeWid, decimal prmDieSizeLen, decimal prmOnWid, decimal prmOnLen, decimal prmOnTotalUp, decimal prmBlankWid, decimal prmBlankLen, decimal prmBlankSqInch, string prmLeaf1, string prmLeaf2, string prmLeaf3, string prmLeaf4, string prmLeafDesc1, string prmLeafDesc2, string prmLeafDesc3, string prmLeafDesc4, Int32 prmS1, Int32 prmB1, decimal prmLeafWid1, decimal prmLeafLen1, Int32 prmS2, Int32 prmB2, decimal prmLeafWid2, decimal prmLeafLen2, Int32 prmS3, Int32 prmB3, decimal prmLeafWid3, decimal prmLeafLen3, Int32 prmS4, Int32 prmB4, decimal prmLeafWid4, decimal prmLeafLen4, Int32 prmBlankno, string prmAuto)
    {
        string cError = "";
        dsFoldLayoutDataSet dsFoldLayout = new dsFoldLayoutDataSet();
        AppServerConnect();
        aoObject.FoldLayout(prmUser, prmAction, prmType, prmComp, prmEstimate, prmEstDate, prmForm, prmFormQty, prmCustPart, prmMachine, prmMachDscr, prmFrontBack, prmSideSide, prmXgrain, prmBoard, prmBoardName, prmReal, prmCaliper, prmCostMsf, prmCostUom, prmWeightt, prmFreightMsf, prmFreightUom, prmNc, prmRoll, prmRollWid, prmGrosShetWid, prmGrosShetLen, prmOutWid, prmOutLen, prmOutCut, prmDieInches, prmMachFeedWid, prmMachFeedLen, prmDieSizeWid, prmDieSizeLen, prmOnWid, prmOnLen, prmOnTotalUp, prmBlankWid, prmBlankLen, prmBlankSqInch, prmLeaf1, prmLeaf2, prmLeaf3, prmLeaf4, prmLeafDesc1, prmLeafDesc2, prmLeafDesc3, prmLeafDesc4, prmS1, prmB1, prmLeafWid1, prmLeafLen1, prmS2, prmB2, prmLeafWid2, prmLeafLen2, prmS3, prmB3, prmLeafWid3, prmLeafLen3, prmS4, prmB4, prmLeafWid4, prmLeafLen4, prmBlankno, prmAuto, out cError, ref dsFoldLayout);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
           // HttpContext.Current.Response.Write("<script>history.back()</script>");
            return false;

        }
        else
        {
            return true;
        }
    }



    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrPrepDataSet SelectPrep(string prmUser, string prmAction, string prmActSelect, string prmComp, string prmEstNum, Int32 prmFormNo, Int32 prmSnum, Int32 prmBnum, string prmCode, decimal prmQty, string prmDesc, decimal prmCost, string prmMl, string prmSimon, decimal prmMark, decimal prmAmort,Int32 prmLine,Int32 prmBlank, ref string  vmessage)
    {
        string cError = "";
        string cLogicError = "";
        dsCorrPrepDataSet dsCorrPrep = new dsCorrPrepDataSet();
        dsCorrPrep = null;
        AppServerConnect();
        aoObject.CorrPrep(prmUser, prmAction, prmActSelect, prmComp, prmEstNum, prmFormNo, prmSnum, prmBnum, prmCode, prmQty, prmDesc, prmCost, prmMl, prmSimon, prmMark, prmAmort, prmLine,prmBlank, ref dsCorrPrep, out cError, out cLogicError);
        AppServerDisconnect();
        if (cLogicError != "")
        {
            vmessage = cLogicError;
        }
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCorrPrep;        
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsPrepLookDataSet SelectPreplook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        dsPrepLookDataSet dsPrepLook = new dsPrepLookDataSet();
        dsPrepLook = null;
        AppServerConnect();
        aoObject.PrepLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsPrepLook);
        AppServerDisconnect();

        return dsPrepLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrRouteDataSet SelectRoute(string prmUser, string prmAction, string prmActSelect, string prmComp, string prmEstNum, Int32 prmFormNo, Int32 prmSnum, Int32 prmBnum, string prmMcode, string prmMdscr, Int32 prmOppass, Int32 prmNout, decimal prmOpmr, Int32 prmOpwaste, Int32 prmOpspeed, decimal prmOpspoil, decimal prmOpcrew, decimal prmOpcrew2, decimal prmOpRate, decimal prmOpRate2, Int32 prmPlates, Int32 prmFountains, string prmAtype1, Int32 prmAtqty1, string prmAtype2, Int32 prmAtqty2, string prmAtype3, Int32 prmAtqty3, Int32 prmLine,Int32 prmQty)
    {
        string cError = "";
        dsCorrRouteDataSet dsCorrRoute = new dsCorrRouteDataSet();
        dsCorrRoute = null;
        AppServerConnect();
        aoObject.CorrRoute(prmUser, prmAction, prmActSelect, prmComp, prmEstNum, prmFormNo, prmSnum, prmBnum, prmMcode, prmMdscr, prmOppass, prmNout, prmOpmr, prmOpwaste, prmOpspeed, prmOpspoil, prmOpcrew, prmOpcrew2, prmOpRate, prmOpRate2, prmPlates, prmFountains, prmAtype1, prmAtqty1, prmAtype2, prmAtqty2, prmAtype3, prmAtqty3, prmLine,prmQty, ref dsCorrRoute, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCorrRoute;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrMiscSubDataSet SelectCorrMiscSub(string prmUser, string prmAction, string prmType, string prmComp, string prmEstimate, DateTime prmEstDate, Int32 prmForm, Int32 prmFormQty, Int32 prmBlk, Int32 prmBlkQty, string prmCustPart, Int32 prmS1, Int32 prmB1, string prmCost1, decimal prmMatf1, decimal prmMatm1, decimal prmLabf1, decimal prmLabm1, string prmSimon1, decimal prmMarkup1, Int32 prmS2, Int32 prmB2, string prmCost2, decimal prmMatf2, decimal prmMatm2, decimal prmLabf2, decimal prmLabm2, string prmSimon2, decimal prmMarkup2, Int32 prmS3, Int32 prmB3, string prmCost3, decimal prmMatf3, decimal prmMatm3, decimal prmLabf3, decimal prmLabm3, string prmSimon3, decimal prmMarkup3, Int32 prmS4, Int32 prmB4, string prmCost4, decimal prmMatf4, decimal prmMatm4, decimal prmLabf4, decimal prmLabm4, string prmSimon4, decimal prmMarkup4, Int32 prmS5, Int32 prmB5, string prmCost5, decimal prmMatf5, decimal prmMatm5, decimal prmLabf5, decimal prmLabm5, string prmSimon5, decimal prmMarkup5, Int32 prmS6, Int32 prmB6, string prmCost6, decimal prmMatf6, decimal prmMatm6, decimal prmLabf6, decimal prmLabm6, string prmSimon6, decimal prmMarkup6, string prmItem1, string prmItemDesc1, decimal prmQty1, string prmUom1, string prmItem2, string prmItemDesc2, decimal prmQty2, string prmUom2, string prmItem3, string prmItemDesc3, decimal prmQty3, string prmUom3, string prmItem4, string prmItemDesc4, decimal prmQty4, string prmUom4, string prmItem5, string prmItemDesc5, decimal prmQty5, string prmUom5, string prmItem6, string prmItemDesc6, decimal prmQty6, string prmUom6, string prmItem7, string prmItemDesc7, decimal prmQty7, string prmUom7, string prmItem8, string prmItemDesc8, decimal prmQty8, string prmUom8, Int32 prmBlank)
    {
        string cError = "";
        dsCorrMiscSubDataSet dsCorrMiscSub = new dsCorrMiscSubDataSet();
        dsCorrMiscSub = null;
        AppServerConnect();
        aoObject.CorrMiscSub(prmUser, prmAction, prmType, prmComp, prmEstimate, prmEstDate, prmForm, prmFormQty, prmBlk, prmBlkQty, prmCustPart, prmS1, prmB1, prmCost1, prmMatf1, prmMatm1, prmLabf1, prmLabm1, prmSimon1, prmMarkup1, prmS2, prmB2, prmCost2, prmMatf2, prmMatm2, prmLabf2, prmLabm2, prmSimon2, prmMarkup2, prmS3, prmB3, prmCost3, prmMatf3, prmMatm3, prmLabf3, prmLabm3, prmSimon3, prmMarkup3, prmS4, prmB4, prmCost4, prmMatf4, prmMatm4, prmLabf4, prmLabm4, prmSimon4, prmMarkup4, prmS5, prmB5, prmCost5, prmMatf5, prmMatm5, prmLabf5, prmLabm5, prmSimon5, prmMarkup5, prmS6, prmB6, prmCost6, prmMatf6, prmMatm6, prmLabf6, prmLabm6, prmSimon6, prmMarkup6, prmItem1, prmItemDesc1, prmQty1, prmUom1, prmItem2, prmItemDesc2, prmQty2, prmUom2, prmItem3, prmItemDesc3, prmQty3, prmUom3, prmItem4, prmItemDesc4, prmQty4, prmUom4, prmItem5, prmItemDesc5, prmQty5, prmUom5, prmItem6, prmItemDesc6, prmQty6, prmUom6, prmItem7, prmItemDesc7, prmQty7, prmUom7, prmItem8, prmItemDesc8, prmQty8, prmUom8,prmBlank, out cError, ref dsCorrMiscSub);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCorrMiscSub;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldMiscSubDataSet SelectFoldMiscSub(string prmUser, string prmAction, string prmType, string prmComp, string prmEstimate, DateTime prmEstDate, Int32 prmForm, Int32 prmFormQty, Int32 prmBlk, Int32 prmBlkQty, string prmCustPart, Int32 prmS1, Int32 prmB1, string prmCost1, decimal prmMatf1, decimal prmMatm1, decimal prmLabf1, decimal prmLabm1, string prmSimon1, decimal prmMarkup1, Int32 prmS2, Int32 prmB2, string prmCost2, decimal prmMatf2, decimal prmMatm2, decimal prmLabf2, decimal prmLabm2, string prmSimon2, decimal prmMarkup2, Int32 prmS3, Int32 prmB3, string prmCost3, decimal prmMatf3, decimal prmMatm3, decimal prmLabf3, decimal prmLabm3, string prmSimon3, decimal prmMarkup3, Int32 prmS4, Int32 prmB4, string prmCost4, decimal prmMatf4, decimal prmMatm4, decimal prmLabf4, decimal prmLabm4, string prmSimon4, decimal prmMarkup4, Int32 prmS5, Int32 prmB5, string prmCost5, decimal prmMatf5, decimal prmMatm5, decimal prmLabf5, decimal prmLabm5, string prmSimon5, decimal prmMarkup5, Int32 prmS6, Int32 prmB6, string prmCost6, decimal prmMatf6, decimal prmMatm6, decimal prmLabf6, decimal prmLabm6, string prmSimon6, decimal prmMarkup6, string prmItem1, string prmItemDesc1, decimal prmQty1, string prmUom1, string prmItem2, string prmItemDesc2, decimal prmQty2, string prmUom2, string prmItem3, string prmItemDesc3, decimal prmQty3, string prmUom3, string prmItem4, string prmItemDesc4, decimal prmQty4, string prmUom4, string prmItem5, string prmItemDesc5, decimal prmQty5, string prmUom5, string prmItem6, string prmItemDesc6, decimal prmQty6, string prmUom6, string prmItem7, string prmItemDesc7, decimal prmQty7, string prmUom7, string prmItem8, string prmItemDesc8, decimal prmQty8, string prmUom8, Int32 prmBlank)
    {
        string cError = "";
        dsFoldMiscSubDataSet dsFoldMiscSub = new dsFoldMiscSubDataSet();
        dsFoldMiscSub = null;
        AppServerConnect();
        aoObject.FoldMiscSub(prmUser, prmAction, prmType, prmComp, prmEstimate, prmEstDate, prmForm, prmFormQty, prmBlk, prmBlkQty, prmCustPart, prmS1, prmB1, prmCost1, prmMatf1, prmMatm1, prmLabf1, prmLabm1, prmSimon1, prmMarkup1, prmS2, prmB2, prmCost2, prmMatf2, prmMatm2, prmLabf2, prmLabm2, prmSimon2, prmMarkup2, prmS3, prmB3, prmCost3, prmMatf3, prmMatm3, prmLabf3, prmLabm3, prmSimon3, prmMarkup3, prmS4, prmB4, prmCost4, prmMatf4, prmMatm4, prmLabf4, prmLabm4, prmSimon4, prmMarkup4, prmS5, prmB5, prmCost5, prmMatf5, prmMatm5, prmLabf5, prmLabm5, prmSimon5, prmMarkup5, prmS6, prmB6, prmCost6, prmMatf6, prmMatm6, prmLabf6, prmLabm6, prmSimon6, prmMarkup6, prmItem1, prmItemDesc1, prmQty1, prmUom1, prmItem2, prmItemDesc2, prmQty2, prmUom2, prmItem3, prmItemDesc3, prmQty3, prmUom3, prmItem4, prmItemDesc4, prmQty4, prmUom4, prmItem5, prmItemDesc5, prmQty5, prmUom5, prmItem6, prmItemDesc6, prmQty6, prmUom6, prmItem7, prmItemDesc7, prmQty7, prmUom7, prmItem8, prmItemDesc8, prmQty8, prmUom8,prmBlank, out cError, ref dsFoldMiscSub);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsFoldMiscSub;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsItemSpecLookupDataSet SelectSpecItem(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmType, string prmIndustry, string prmEstimate)
    {
        dsItemSpecLookupDataSet dsItemSpecLookup = new dsItemSpecLookupDataSet();
        dsItemSpecLookup = null;
        AppServerConnect();
        aoObject.ItemLookup(prmAction, prmUser, prmField, prmCondition, prmText, prmType, prmIndustry, prmEstimate, ref dsItemSpecLookup);
        AppServerDisconnect();

        return dsItemSpecLookup;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrAdderLookupDataSet SelectCorrAdder(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmComp, string prmMachine)
    {
        dsCorrAdderLookupDataSet dsCorrAdderLookup = new dsCorrAdderLookupDataSet();
        dsCorrAdderLookup = null;
        AppServerConnect();
        aoObject.CorAdderLook(prmAction, prmUser, prmField, prmCondition, prmText,prmComp,prmMachine, ref dsCorrAdderLookup);
        AppServerDisconnect();

        return dsCorrAdderLookup;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrQtyDataSet SelectCorrQty(string prmUser, string prmAction, string prmActSelect, string prmComp, string prmEstNum, Int32 prmFormNo, Int32 prmQty, string prmReckey)
    {
        string cError = "";
        dsCorrQtyDataSet dsCorrQty = new dsCorrQtyDataSet();
        dsCorrQty = null;
        AppServerConnect();
        aoObject.CorrQty(prmUser, prmAction, prmActSelect, prmComp, prmEstNum, prmFormNo, prmQty,prmReckey, out cError, ref dsCorrQty);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCorrQty;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrPrintDataSet SelectCorrPrint(string prmUser, string prmAction, string prmType, string prmComp, string prmEstimate, DateTime prmEstDate, Int32 prmForm, Int32 prmFormQty, Int32 prmBlk, Int32 prmBlkQty, string prmCustPart, Int32 prmQty, decimal prmTotalFactCost, decimal prmFullCost, decimal prmMargin, decimal prmGross, decimal prmNet, decimal prmSellPrice, Int32 prmTotalSheet, string prmQty2, decimal prmPriceBsf, DateTime prmProbeDate, decimal prmBoardM, decimal prmBoard, decimal prmBoardContM, decimal prmBoardCont, string prmProbeBy, decimal prmTotalMsf, string prmTime, Int32 prmLine,Int32 prmBlank)
    {
        string cError = "";
        dsCorrPrintDataSet dsCorrPrint = new dsCorrPrintDataSet();
        dsCorrPrint = null;
        AppServerConnect();
        aoObject.CorrPrint(prmUser, prmAction, prmType, prmComp, prmEstimate, prmEstDate, prmForm, prmFormQty, prmBlk, prmBlkQty, prmCustPart, prmQty, prmTotalFactCost, prmFullCost, prmMargin, prmGross, prmNet, prmSellPrice, prmTotalSheet, prmQty2, prmPriceBsf, prmProbeDate, prmBoardM, prmBoard, prmBoardContM, prmBoardCont, prmProbeBy, prmTotalMsf, prmTime, prmLine,prmBlank, out cError, ref dsCorrPrint);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCorrPrint;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldPrintDataSet SelectFoldPrint(string prmUser, string prmAction, string prmType, string prmComp, string prmEstimate, DateTime prmEstDate, Int32 prmForm, Int32 prmFormQty, Int32 prmBlk, Int32 prmBlkQty, string prmCustPart, Int32 prmQty, decimal prmTotalFactCost, decimal prmFullCost, decimal prmMargin, decimal prmComm, decimal prmNet, decimal prmSellPrice, Int32 prmTotalSheet, string prmQty2, decimal prmPriceBsf, DateTime prmProbeDate, string prmProbeBy, decimal prmShipWeight, decimal prmTotalMsf, string prmTime, Int32 prmLine, decimal prmGross, Int32 prmBlank)
    {
        string cError = "";        
        dsFoldPrintDataSet dsFoldPrint = new dsFoldPrintDataSet();
        dsFoldPrint = null;
        AppServerConnect();
        aoObject.FoldPrint(prmUser, prmAction, prmType, prmComp, prmEstimate, prmEstDate, prmForm, prmFormQty, prmBlk, prmBlkQty, prmCustPart, prmQty, prmTotalFactCost, prmFullCost, prmMargin, prmComm, prmNet, prmSellPrice, prmTotalSheet, prmQty2, prmPriceBsf, prmProbeDate, prmProbeBy, prmShipWeight, prmTotalMsf, prmTime, prmLine,prmGross,prmBlank, out cError, ref dsFoldPrint);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsFoldPrint;
    }



    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldRouteDataSet SelectFoldRoute(string prmUser, string prmAction, string prmActSelect, string prmComp, string prmEstNum, Int32 prmFormNo, Int32 prmSnum, Int32 prmBnum, string prmMcode, string prmMdscr , Int32 prmNout, decimal prmOpmr, Int32 prmOpwaste, Int32 prmOpspeed, decimal prmOpspoil, decimal prmOpcrew, decimal prmOpcrew2, decimal prmOpRate, decimal prmOpRate2, Int32 prmPlates, Int32 prmFountains, Int32 prmInk, Int32 prmCoat, Int32 prmLine, Int32 prmQty)
    {
        string cError = "";
        dsFoldRouteDataSet dsFoldRoute = new dsFoldRouteDataSet();
        dsFoldRoute = null;
        AppServerConnect();
        aoObject.FoldRoute(prmUser, prmAction, prmActSelect, prmComp, prmEstNum, prmFormNo, prmSnum, prmBnum, prmMcode, prmMdscr,  prmNout, prmOpmr, prmOpwaste, prmOpspeed, prmOpspoil, prmOpcrew, prmOpcrew2, prmOpRate, prmOpRate2, prmPlates, prmFountains, prmInk, prmCoat, prmLine, prmQty, ref dsFoldRoute, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsFoldRoute;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsListCorrPrepDataSet SelectListPrep(string prmUser, string prmAction, string prmActSelect, string prmComp, string prmEstNum, Int32 prmFormNo)
    {
        string cError = "";
        dsListCorrPrepDataSet dsListCorrPrep = new dsListCorrPrepDataSet();
        dsListCorrPrep = null;
        AppServerConnect();
        aoObject.ListCorrPrep(prmUser, prmAction, prmActSelect, prmComp, prmEstNum, prmFormNo, ref dsListCorrPrep, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsListCorrPrep;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool PrepVal(string prmUser, string prmAction, string prmComp, string prmEstNum, Int32 prmFormNo, Int32 prmSnum, Int32 prmBnum, string prmCode, decimal prmQty, string prmDesc, decimal prmCost, string prmMl, string prmSimon, decimal prmMark, decimal prmAmort, Int32 prmLine,Int32 prmOldQty)
    {
        string cError = "";       
        AppServerConnect();
        aoObject.PrepValidate(prmUser, prmAction, prmComp, prmEstNum, prmFormNo, prmSnum, prmBnum, prmCode, prmQty, prmDesc, prmCost, prmMl, prmSimon, prmMark, prmAmort, prmLine,prmOldQty, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
            return false;

        }
        else
        {
            return true;
        }
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool RouteVal(string prmUser, string prmAction, string prmComp, string prmEstNum, Int32 prmFormNo, Int32 prmSnum, Int32 prmBnum, string prmMcode, string prmMdscr, Int32 prmOppass, Int32 prmNout, decimal prmOpmr, Int32 prmOpwaste, Int32 prmOpspeed, decimal prmOpspoil, decimal prmOpcrew, decimal prmOpcrew2, decimal prmOpRate, decimal prmOpRate2, Int32 prmPlates, Int32 prmFountains, string prmAtype1, Int32 prmAtqty1, string prmAtype2, Int32 prmAtqty2, string prmAtype3, Int32 prmAtqty3, Int32 prmLine, Int32 prmQty)
    {
        string cError = "";        
        AppServerConnect();
        aoObject.RouteValidate(prmUser, prmAction, prmComp, prmEstNum, prmFormNo, prmSnum, prmBnum, prmMcode, prmMdscr, prmOppass, prmNout, prmOpmr, prmOpwaste, prmOpspeed, prmOpspoil, prmOpcrew, prmOpcrew2, prmOpRate, prmOpRate2, prmPlates, prmFountains, prmAtype1, prmAtqty1, prmAtype2, prmAtqty2, prmAtype3, prmAtqty3, prmLine, prmQty, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
            return false;

        }
        else 
        {
            return true;
        }
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool FoldRouteVal(string prmUser, string prmAction,  string prmComp, string prmEstNum, Int32 prmFormNo, Int32 prmSnum, Int32 prmBnum, string prmMcode, string prmMdscr, Int32 prmNout, decimal prmOpmr, Int32 prmOpwaste, Int32 prmOpspeed, decimal prmOpspoil, decimal prmOpcrew, decimal prmOpcrew2, decimal prmOpRate, decimal prmOpRate2, Int32 prmPlates, Int32 prmFountains, Int32 prmInk, Int32 prmCoat, Int32 prmLine, Int32 prmQty)
    {
        string cError = "";        
        AppServerConnect();
        aoObject.FoldRouteVal(prmUser, prmAction,  prmComp, prmEstNum, prmFormNo, prmSnum, prmBnum, prmMcode, prmMdscr, prmNout, prmOpmr, prmOpwaste, prmOpspeed, prmOpspoil, prmOpcrew, prmOpcrew2, prmOpRate, prmOpRate2, prmPlates, prmFountains, prmInk, prmCoat, prmLine, prmQty,  out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
            return false;

        }
        else
        {
            return true;
        }       
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrBoxDesignDataSet SelectBoxDesign(string prmUser, string prmAction, string prmComp, Int32 prmDesignNo, string prmDesignDscr, string prmLScore, string prmLCumScore, Int32 prmEnum, Int32 prmFormNo, Int32 prmBlankNo, string prmEstNo, decimal prmEqty, string prmBoxImage, string prmWScore, string prmWCumScore, string prmBoxText, string prmBox3DImage)
    {
        //string cError = "";
        dsCorrBoxDesignDataSet dsCorrBoxDesign = new dsCorrBoxDesignDataSet();
        dsCorrBoxDesign = null;
        AppServerConnect();
        aoObject.CorrBoxDesign(prmUser, prmAction, prmComp, prmDesignNo, prmDesignDscr, prmLScore, prmLCumScore, prmEnum, prmFormNo, prmBlankNo, prmEstNo, prmEqty, prmBoxImage, prmWScore, prmWCumScore, prmBoxText, prmBox3DImage, ref dsCorrBoxDesign);
        AppServerDisconnect();
        //if (cError != "")
        //{
        //    HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
        //    HttpContext.Current.Response.Write("<script>history.back()</script>");

        //}
        return dsCorrBoxDesign;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldBoxDesignDataSet SelectFoldBoxDesign(string prmUser, string prmAction, string prmComp, string prmEstNo, Int32 prmFormNo, Int32 prmDesignNo, string prmDesignDscr, string prmBoxImage, string prmBox3DImage)
    {
        //string cError = "";
        dsFoldBoxDesignDataSet dsFoldBoxDesign = new dsFoldBoxDesignDataSet();
        dsFoldBoxDesign = null;
        AppServerConnect();
        aoObject.FoldBoxDesign(prmUser, prmAction, prmComp, prmEstNo, prmFormNo, prmDesignNo, prmDesignDscr, prmBoxImage, prmBox3DImage, ref dsFoldBoxDesign);
        AppServerDisconnect();
        //if (cError != "")
        //{
        //    HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
        //    HttpContext.Current.Response.Write("<script>history.back()</script>");

        //}
        return dsFoldBoxDesign;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsEstUpdateSetDataSet SelectUpdateSetEst(string prmUser, string prmAction, string prmStock, string prmPartno, string prmPartDscr, string prmPartDscr2, string prmProcat, decimal prmlen, decimal prmWid, decimal prmDep, string prmAllo, string prmUnit, string prmEst,Int32 prmType)
    {
        string cError = "";
        dsEstUpdateSetDataSet dsEstUpdateSet = new dsEstUpdateSetDataSet();
        dsEstUpdateSet = null;
        AppServerConnect();
        aoObject.estupdateset(prmUser,  prmAction,  prmStock,  prmPartno,  prmPartDscr,  prmPartDscr2,  prmProcat,  prmlen,  prmWid,  prmDep,  prmAllo,  prmUnit,  prmEst,prmType, ref dsEstUpdateSet ,out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsEstUpdateSet;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrGoToDataSet SelectGoTo(string prmUser, string prmAction, string prmComp, string prmEstNum, Int32 prmFormNo, Int32 prmBlankNo, string prmPartno, Int32 prmBlQty, Int32 prmYldQty, string prmPrice, decimal prmWid, decimal prmLen, decimal prmUp)
    {
        string cError = "";
        dsCorrGoToDataSet dsCorrGoTo = new dsCorrGoToDataSet();
        dsCorrGoTo = null;
        AppServerConnect();
        aoObject.Est_Goto(prmUser, prmAction, prmComp, prmEstNum, prmFormNo, prmBlankNo, prmPartno, prmBlQty, prmYldQty, prmPrice, prmWid, prmLen, prmUp, ref dsCorrGoTo, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCorrGoTo;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateCorrEst(string prmAction, string prmUser, string prmEstimate, string prmCust, string prmCustPart, string prmShipTo, string prmItemName, string prmFgItem, decimal prmEstQty, string prmStyle, string prmFlute, string prmTest, string prmBoard, decimal prmCalliper, string prmCategory, decimal prmLength, decimal prmWidth, decimal prmDepth, Int32 prmFrom, Int32 prmBlank, string prmTab, Int32 prmColor, Int32 prmPasses, Int32 prmCoating, Int32 prmCoatPasses, decimal prmQtySet, Int32 prmInkFrom, Int32 prmPassesFrom, Int32 prmCoatingFrom, Int32 prmCoatPassesFrom, string prmPurchManuf, DateTime prmEstDate, string prmType, string prmMassType)
    {
        string cError = "";

        AppServerConnect();
        aoObject.CorrEstValidate(prmAction, prmUser, prmEstimate, prmCust, prmCustPart, prmShipTo, prmItemName, prmFgItem, prmEstQty, prmStyle, prmFlute, prmTest, prmBoard, prmCalliper, prmCategory, prmLength, prmWidth, prmDepth, prmFrom, prmBlank, prmTab, prmColor, prmPasses, prmCoating, prmCoatPasses, prmQtySet, prmInkFrom, prmPassesFrom, prmCoatingFrom, prmCoatPassesFrom, prmPurchManuf, prmEstDate, prmType, prmMassType, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            return false;
        }
        else
        {
            return true;
        }
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateFodingEst(string prmAction, string prmUser, string prmEstimate, string prmCust, string prmCustPart, string prmShipTo, string prmItemName, string prmFgItem, decimal prmEstQty, string prmStyle, string prmFlute, string prmTest, string prmBoard, decimal prmCalliper, string prmCategory, decimal prmLength, decimal prmWidth, decimal prmDepth, Int32 prmFrom, Int32 prmBlank, string prmTab, Int32 prmColor, Int32 prmPasses, Int32 prmCoating, Int32 prmCoatPasses, decimal prmQtySet, Int32 prmInkFrom, Int32 prmPassesFrom, Int32 prmCoatingFrom, Int32 prmCoatPassesFrom, string prmPurchManuf, DateTime prmEstDate, string prmType, decimal prmDiein)
    {
        string cError = "";

        AppServerConnect();
        aoObject.FoldEstValidate(prmAction, prmUser, prmEstimate, prmCust, prmCustPart, prmShipTo, prmItemName, prmFgItem, prmEstQty, prmStyle, prmFlute, prmTest, prmBoard, prmCalliper, prmCategory, prmLength, prmWidth, prmDepth, prmFrom, prmBlank, prmTab, prmColor, prmPasses, prmCoating, prmCoatPasses, prmQtySet, prmInkFrom, prmPassesFrom, prmCoatingFrom, prmCoatPassesFrom, prmPurchManuf, prmEstDate, prmType, prmDiein, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            return false;
        }
        else
        {
            return true;
        }
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrBomDataSet SelectBOM(string prmUser, string prmAction, string prmComp, string prmEstimate, Int32 prmForm, string prmBomItem1, string prmBomItem2, string prmBomItem3, string prmBomItem4, string prmBomItem5, string prmBomItem6, string prmBomItem7, string prmBomItem8, string prmLamCode, string prmAdhesive, string prmBomItemDscr1, string prmBomItemDscr2, string prmBomItemDscr3, string prmBomItemDscr4, string prmBomItemDscr5, string prmBomItemDscr6, string prmBomItemDscr7, string prmBomItemDscr8, string prmBomItemDscr9, string prmBomItemDscr10, decimal prmShrink1, decimal prmShrink2, decimal prmShrink3, decimal prmShrink4, decimal prmShrink5, decimal prmShrink6, decimal prmShrink7, decimal prmShrink8, decimal prmSqInch1, decimal prmSqInch2, decimal prmSqInch3, decimal prmSqInch4, decimal prmSqInch5, decimal prmSqInch6, decimal prmSqInch7, decimal prmSqInch8, decimal prmSqInch9, decimal prmSqInch10)
    {
        string cError = "";
        dsCorrBomDataSet dsCorrBom = new dsCorrBomDataSet();
        dsCorrBom = null;
        AppServerConnect();
        aoObject.corr_bom(prmUser, prmAction, prmComp, prmEstimate, prmForm, prmBomItem1, prmBomItem2, prmBomItem3, prmBomItem4, prmBomItem5, prmBomItem6, prmBomItem7, prmBomItem8, prmLamCode, prmAdhesive, prmBomItemDscr1, prmBomItemDscr2, prmBomItemDscr3, prmBomItemDscr4, prmBomItemDscr5, prmBomItemDscr6, prmBomItemDscr7, prmBomItemDscr8, prmBomItemDscr9, prmBomItemDscr10, prmShrink1, prmShrink2, prmShrink3, prmShrink4, prmShrink5, prmShrink6, prmShrink7, prmShrink8, prmSqInch1, prmSqInch2, prmSqInch3, prmSqInch4, prmSqInch5, prmSqInch6, prmSqInch7, prmSqInch8, prmSqInch9, prmSqInch10, out cError, ref dsCorrBom );
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCorrBom;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrBomLaminateLookupDataSet BOMItemLaminateLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmComp, string prmMatType, string prmIndustry, string prmEstimate, Int32 prmForm)
    {
        dsCorrBomLaminateLookupDataSet dsCorrBomLaminateLookup = new dsCorrBomLaminateLookupDataSet();
        dsCorrBomLaminateLookup = null;
        AppServerConnect();
        aoObject.CorBomLaminateLook(prmAction, prmUser, prmField, prmCondition, prmText, prmComp, prmMatType, prmIndustry, prmEstimate, prmForm, ref dsCorrBomLaminateLookup);
        AppServerDisconnect();
        return dsCorrBomLaminateLookup;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrBomLookupDataSet BOMItemLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmComp, string prmMatType, string prmItem, string prmEstimate, Int32 prmForm)
    {
        dsCorrBomLookupDataSet dsCorrBomLookup = new dsCorrBomLookupDataSet();
        dsCorrBomLookup = null;
        AppServerConnect();
        aoObject.CorBomLook(prmAction, prmUser, prmField, prmCondition, prmText, prmComp, prmMatType, prmItem, prmEstimate, prmForm, ref dsCorrBomLookup);
        AppServerDisconnect();
        return dsCorrBomLookup;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrBomAdhesiveLookupDataSet BOMAdhesiveLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmComp, string prmIndustry, string prmMatType, string prmItem, string prmEstimate, Int32 prmForm)
    {
        dsCorrBomAdhesiveLookupDataSet dsCorrBomAdhesiveLookup = new dsCorrBomAdhesiveLookupDataSet();
        dsCorrBomAdhesiveLookup = null;
        AppServerConnect();
        aoObject.CorBomAdhesiveLook(prmAction, prmUser, prmField, prmCondition, prmText, prmComp, prmIndustry, prmMatType, prmItem, prmEstimate, prmForm, ref dsCorrBomAdhesiveLookup);
        AppServerDisconnect();
        return dsCorrBomAdhesiveLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldBomDataSet SelectFoldBom(string prmUser, string prmAction, string prmComp, string prmEstimate, Int32 prmForm, string prmMedium, string prmLiner, string prmLamCode, string prmAdhesive, decimal prmFlute, decimal prmSqInch)
    {
        string cError = "";
        dsFoldBomDataSet dsFoldBom = new dsFoldBomDataSet();
        dsFoldBom = null;
        AppServerConnect();
        aoObject.fold_bom(prmUser, prmAction, prmComp, prmEstimate, prmForm, prmMedium, prmLiner, prmLamCode, prmAdhesive, prmFlute, prmSqInch, out cError, ref dsFoldBom);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsFoldBom;
    }





    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool FoldingSpecValidate(string prmUser, string prmAction, string prmCustNum, string prmShipTo, string prmFgItem, string prmCustPart, string prmItemName, string prmPartDscr, string prmDieNum, string prmCadNum, string prmSpcNum, string prmPlateNum, string prmImage, string prmUpcNum, string prmSman, string prmSmanDscr, decimal prmComm, string prmFgCat, string prmFgCatDscr, string prmStyle, string prmStyDscr, string prmBoard, string prmBrdDscr, decimal prmLength, decimal prmWidth, decimal prmDepth, string prmMetric, string prmAdhe, decimal prmDustFlap, decimal prmPanel, decimal prmLockTab, decimal prmGlue, decimal prmScWid, decimal prmScLen, decimal prmTuck, decimal prmLinInc, decimal prmBlankWid, decimal prmBlankLen, decimal prmBlankSqFt, string prmEstNum, DateTime prmFromDate, DateTime prmEstDate, DateTime prmModDate, Int32 prmOrderNum, DateTime prmOrdDate, decimal prmQty, string prmShipName, string prmAddr, string prmAddr2, string prmCity, string prmState, string prmZip, Int32 prmFormno, string prmEstFrom, Int32 prmBlankno)
    {
        string cError = "";       
        AppServerConnect();
        aoObject.FoldSpecValidate(prmUser, prmAction, prmCustNum, prmShipTo, prmFgItem, prmCustPart, prmItemName, prmPartDscr, prmDieNum, prmCadNum, prmSpcNum, prmPlateNum, prmImage, prmUpcNum, prmSman, prmSmanDscr, prmComm, prmFgCat, prmFgCatDscr, prmStyle, prmStyDscr, prmBoard, prmBrdDscr, prmLength, prmWidth, prmDepth, prmMetric, prmAdhe, prmDustFlap, prmPanel, prmLockTab, prmGlue, prmScWid, prmScLen, prmTuck, prmLinInc, prmBlankWid, prmBlankLen, prmBlankSqFt, prmEstNum, prmFromDate, prmEstDate, prmModDate, prmOrderNum, prmOrdDate, prmQty, prmShipName, prmAddr, prmAddr2, prmCity, prmState, prmZip, prmFormno, prmEstFrom, prmBlankno,  out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
            return false;
        }
        else
        {
            return true;
        }
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool  ValidateCorrSpec(string prmUser, string prmAction, string prmCustNum, string prmShipTo, string prmFgItem, string prmCustPart, string prmItemName, string prmPartDscr, string prmDieNum, string prmCadNum, string prmSpcNum, string prmPlateNum, string prmImage, string prmUpcNum, string prmSman, string prmSmanDscr, decimal prmComm, string prmFgCat, string prmFgCatDscr, string prmStyle, string prmStyDscr, string prmBoard, string prmBrdDscr, decimal prmLength, decimal prmWidth, decimal prmDepth, string prmFlute, string prmTest, string prmTab, string prmMetric, string prmJointMat, decimal prmDustFlap, decimal prmBotFlap, decimal prmLockTab, decimal prmTabWid, decimal prmScWid, decimal prmScLen, decimal prmTuck, decimal prmJointLen, decimal prmBlankWid, decimal prmBlankLen, decimal prmBlankSqFt, string prmEstNum, DateTime prmFromDate, DateTime prmEstDate, DateTime prmModDate, Int32 prmOrderNum, DateTime prmOrdDate, decimal prmQty, decimal prmQtySet, decimal prmMsf, string prmShipName, string prmAddr, string prmAddr2, string prmCity, string prmState, string prmZip, Int32 prmFormno, string prmEstFrom, Int32 prmBlankno)
    {
        string cError = "";
        AppServerConnect();
        aoObject.CorrSpecValidate(prmUser, prmAction, prmCustNum, prmShipTo, prmFgItem, prmCustPart, prmItemName, prmPartDscr, prmDieNum, prmCadNum, prmSpcNum, prmPlateNum, prmImage, prmUpcNum, prmSman, prmSmanDscr, prmComm, prmFgCat, prmFgCatDscr, prmStyle, prmStyDscr, prmBoard, prmBrdDscr, prmLength, prmWidth, prmDepth, prmFlute, prmTest, prmTab, prmMetric, prmJointMat, prmDustFlap, prmBotFlap, prmLockTab, prmTabWid, prmScWid, prmScLen, prmTuck, prmJointLen, prmBlankWid, prmBlankLen, prmBlankSqFt, prmEstNum, prmFromDate, prmEstDate, prmModDate, prmOrderNum, prmOrdDate, prmQty, prmQtySet, prmMsf, prmShipName, prmAddr, prmAddr2, prmCity, prmState, prmZip, prmFormno, prmEstFrom, prmBlankno,  out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
            return false;
        }
        else
        {
            return true;
        }        
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrEstimateListDataSet CorrugatedEstimatePrint(string prmUser, string prmAction, string prmOut, string vFromDept, string vToDept, string vTbPrtBox, string vTbPrtNote, string prmEstimate, Int32 prmFormNo, Int32 prmBlankNo, Int32 prmLine)
    {
        string cError = "";
        dsCorrEstimateListDataSet dsCorrEstimateList = new dsCorrEstimateListDataSet();
        dsCorrEstimateList = null;
        AppServerConnect();
        aoObject.CorrEstimatePrint(prmUser, prmAction, prmOut, vFromDept, vToDept, vTbPrtBox, vTbPrtNote, prmEstimate, prmFormNo, prmBlankNo, prmLine, ref dsCorrEstimateList, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsCorrEstimateList;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrEstimateBoxListDataSet CorrugatedEstimatePrintBox(string prmUser, string prmAction, string prmOut, string prmEstimate, Int32 prmFormNo, Int32 prmBlankNo, Int32 prmLine)
    {
        string cError = "";
        dsCorrEstimateBoxListDataSet dsCorrEstimateBoxList = new dsCorrEstimateBoxListDataSet();
        dsCorrEstimateBoxList = null;
        AppServerConnect();
        aoObject.CorrEstimatePrintBox(prmUser, prmAction, prmOut, prmEstimate, prmFormNo, prmBlankNo, prmLine, ref dsCorrEstimateBoxList, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsCorrEstimateBoxList;
    }
    

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorLayHandMachLookupDataSet CorrLayHandMach(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmEstimate, Int32 prmForm, Int32 prmBlankno, string prmBoard, string prmStyle, string prmMach, string prmXgrain)
    {       
        dsCorLayHandMachLookupDataSet dsCorLayHandMachLookup = new dsCorLayHandMachLookupDataSet();
        dsCorLayHandMachLookup = null;
        AppServerConnect();
        aoObject.CorLayHandMach(prmAction, prmUser, prmField, prmCondition, prmText, prmEstimate, prmForm, prmBlankno, prmBoard, prmStyle, prmMach, prmXgrain, ref dsCorLayHandMachLookup);
        AppServerDisconnect();
        return dsCorLayHandMachLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrLayNumLenLookDataSet CorrLayoutLen(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmEstimate, string prmMach, Int32 prmForm, Int32 prmBlankno, string prmBoard, string prmXgrain, decimal prmNout, decimal prmNoutLen, decimal prmNumLen, decimal prmNumWid, decimal prmNumUp, decimal prmGrossLen, decimal prmGrossWid, decimal prmGrossDep, decimal prmNetLen, decimal prmNetWid, decimal prmNetDep)
    {
        dsCorrLayNumLenLookDataSet dsCorrLayNumLenLook = new dsCorrLayNumLenLookDataSet();
        dsCorrLayNumLenLook = null;
        AppServerConnect();
        aoObject.CorrLayNumLen(prmAction, prmUser, prmField, prmCondition, prmText, prmEstimate,prmMach, prmForm, prmBlankno, prmBoard,  prmXgrain,  prmNout,  prmNoutLen,  prmNumLen,  prmNumWid,  prmNumUp,prmGrossLen,  prmGrossWid,  prmGrossDep,  prmNetLen,  prmNetWid,  prmNetDep, ref dsCorrLayNumLenLook);
        AppServerDisconnect();
        return dsCorrLayNumLenLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldHandMachLookupDataSet FoldLayMachine(string prmAction, string prmUser, string prmEstimate, Int32 prmForm, Int32 prmBlankno, string prmBoard, string prmStyle, string prmMach, string prmXgrain, string prmRoll, decimal prmFront, decimal prmSide)
    {
        dsFoldHandMachLookupDataSet dsFoldHandMachLookup = new dsFoldHandMachLookupDataSet();
        dsFoldHandMachLookup = null;
        AppServerConnect();
        aoObject.FoldLayHandMach(prmAction, prmUser,  prmEstimate,  prmForm,  prmBlankno,  prmBoard,  prmStyle,  prmMach,  prmXgrain,  prmRoll,  prmFront,  prmSide, ref dsFoldHandMachLookup);
        AppServerDisconnect();
        return dsFoldHandMachLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsSheetCalFoldLayOutDataSet foldsheetcal(string prmAction, string prmUser, string prmEstimate, string prmMach, Int32 prmForm, Int32 prmBlankno, string prmRMitem, string prmSeaItem, string prmMaxYield)
    {
        string cError = "";
        dsSheetCalFoldLayOutDataSet dsSheetCalFoldLayOut = new dsSheetCalFoldLayOutDataSet();
        dsSheetCalFoldLayOut = null;
        AppServerConnect();
        aoObject.Sheet_Cal(prmAction,  prmUser,  prmEstimate,  prmMach,  prmForm,  prmBlankno,  prmRMitem,  prmSeaItem,  prmMaxYield, out cError, ref dsSheetCalFoldLayOut);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsSheetCalFoldLayOut;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGsaFileDataSet SelectGsa(string prmAction, string prmUser, string prmRowId, Int32 prmIpQty, Int32 prmIpRels, string prmEst, Int32 prmQty, decimal prmBoard, decimal prmMaterial, decimal prmLabor, decimal prmWHMark, decimal prmBrComm, Int32 prmUnCount, Int32 prmPallCount, decimal prmTotUnit, Int32 prmUnitprPal, decimal prmPaHandCh, Int32 prmPallDelMon, Int32 prmTotCharge, decimal prmCostPerPall, Int32 prmTotPall, Int32 prmSeq, Int32 prmGetqty, Int32 prmForm, Int32 prmBlank, decimal prmMatchup, string prmDoGsa, string prmDoMr, string prmDoSpeed, string prmDropRc, string prmInkAlForms, string prmEstList, string prmVendor, decimal prmGsaMat, decimal prmGsaLab, decimal prmGsaWar, decimal prmGsaFm, Int32 prmGsaMonth)
    {
        string cError = "";
        dsGsaFileDataSet dsGsaFile = new dsGsaFileDataSet();
        dsGsaFile = null;
        AppServerConnect();
        aoObject.gsa(prmAction,prmUser,  prmRowId,  prmIpQty,  prmIpRels,  prmEst,  prmQty,  prmBoard,  prmMaterial,  prmLabor,  prmWHMark,  prmBrComm,  prmUnCount,  prmPallCount,  prmTotUnit,  prmUnitprPal,  prmPaHandCh,  prmPallDelMon,  prmTotCharge,  prmCostPerPall,  prmTotPall, prmSeq , prmGetqty,  prmForm,  prmBlank,  prmMatchup,  prmDoGsa,  prmDoMr,  prmDoSpeed,  prmDropRc,  prmInkAlForms,  prmEstList ,prmVendor,prmGsaMat,  prmGsaLab,  prmGsaWar,  prmGsaFm,  prmGsaMonth, ref dsGsaFile, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsGsaFile;
    }



    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGetqtyDataSet SelectQuantity(string prmUser, string prmAction, Int32 prmQty1, Int32 prmQty2, Int32 prmQty3, Int32 prmQty4, Int32 prmQty5, Int32 prmQty6, Int32 prmQty7, Int32 prmQty8, Int32 prmQty9, Int32 prmQty10, Int32 prmQty11, Int32 prmQty12, Int32 prmQty13, Int32 prmQty14, Int32 prmQty15, Int32 prmQty16, Int32 prmQty17, Int32 prmQty18, Int32 prmQty19, Int32 prmQty20, Int32 prmQty21, Int32 prmQty22, Int32 prmQty23, Int32 prmQty24, Int32 prmQty25, Int32 prmQty26, Int32 prmQty27, Int32 prmQty28, Int32 prmRels1, Int32 prmRels2, Int32 prmRels3, Int32 prmRels4, Int32 prmRels5, Int32 prmRels6, Int32 prmRels7, Int32 prmRels8, Int32 prmRels9, Int32 prmRels10, Int32 prmRels11, Int32 prmRels12, Int32 prmRels13, Int32 prmRels14, Int32 prmRels15, Int32 prmRels16, Int32 prmRels17, Int32 prmRels18, Int32 prmRels19, Int32 prmRels20, Int32 prmRels21, Int32 prmRels22, Int32 prmRels23, Int32 prmRels24, Int32 prmRels25, Int32 prmRels26, Int32 prmRels27, Int32 prmRels28, decimal prmMatchup, string prmDoGsa, string prmDoMr, string prmDoSpeed, string prmDropRc, string prmInkAlForms, string prmEstList, string prmEstimate, Int32 prmForm, Int32 prmBlank, string prmLvoverride, string prmVendor, ref string prmRecId)
    {
        string cError = "";
        string prmRecId1 = "";
        dsGetqtyDataSet dsGetqty = new dsGetqtyDataSet();
        dsGetqty = null;
        AppServerConnect();
        aoObject.GetQty(prmUser, prmAction, prmQty1, prmQty2, prmQty3, prmQty4, prmQty5, prmQty6, prmQty7, prmQty8, prmQty9, prmQty10, prmQty11, prmQty12, prmQty13, prmQty14, prmQty15, prmQty16, prmQty17, prmQty18, prmQty19, prmQty20, prmQty21, prmQty22, prmQty23, prmQty24, prmQty25, prmQty26, prmQty27, prmQty28, prmRels1, prmRels2, prmRels3, prmRels4, prmRels5, prmRels6, prmRels7, prmRels8, prmRels9, prmRels10, prmRels11, prmRels12, prmRels13, prmRels14, prmRels15, prmRels16, prmRels17, prmRels18, prmRels19, prmRels20, prmRels21, prmRels22, prmRels23, prmRels24, prmRels25, prmRels26, prmRels27, prmRels28, prmMatchup, prmDoGsa, prmDoMr, prmDoSpeed, prmDropRc, prmInkAlForms, prmEstList, prmEstimate, prmForm, prmBlank, prmLvoverride,prmVendor, ref dsGetqty, out cError, out prmRecId1);
        AppServerDisconnect();

        prmRecId = prmRecId1;

        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGetqty;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGetDefqtyDataSet SelectDefaultQuantity(string prmUser, string prmAction, string prmEstimate)
    {
        string cError = "";
        dsGetDefqtyDataSet dsGetDefqty = new dsGetDefqtyDataSet();
        dsGetDefqty = null;
        AppServerConnect();
        aoObject.GetDefaultQty(prmUser, prmAction, prmEstimate, ref dsGetDefqty, out cError);
        AppServerDisconnect();

        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGetDefqty;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsNewEstimateDataSet GetNewEstimate(string prmAction, string prmUser, string prmComp)
    {
        string cError = "";
        dsNewEstimateDataSet dsNewEstimate = new dsNewEstimateDataSet();
        dsNewEstimate = null;
        AppServerConnect();
        aoObject.NewEst(prmAction, prmUser, prmComp, out cError ,ref dsNewEstimate);
        AppServerDisconnect();

        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");            

        }
        return dsNewEstimate;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGetDefaultQty1DataSet SelectDefaultValues(string prmUser, string prmAction, string prmEstimate)
    {
        string cError = "";
        dsGetDefaultQty1DataSet dsGetDefaultQty1 = new dsGetDefaultQty1DataSet();
        dsGetDefaultQty1 = null;
        AppServerConnect();
        aoObject.GetDefaultQty1(prmUser, prmAction, prmEstimate, ref dsGetDefaultQty1, out cError);
        AppServerDisconnect();

        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGetDefaultQty1;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGet3qtyDataSet SelectQuantity3(string prmUser, string prmAction, Int32 prmQty1, Int32 prmQty2, Int32 prmQty3, Int32 prmQty4, Int32 prmQty5, Int32 prmQty6, Int32 prmQty7, Int32 prmQty8, Int32 prmQty9, Int32 prmQty10, Int32 prmQty11, Int32 prmQty12, Int32 prmQty13, Int32 prmQty14, Int32 prmQty15, Int32 prmQty16, Int32 prmQty17, Int32 prmQty18, Int32 prmQty19, Int32 prmQty20, Int32 prmQty21, Int32 prmQty22, Int32 prmQty23, Int32 prmQty24, Int32 prmQty25, Int32 prmQty26, Int32 prmQty27, Int32 prmQty28, Int32 prmRels1, Int32 prmRels2, Int32 prmRels3, Int32 prmRels4, Int32 prmRels5, Int32 prmRels6, Int32 prmRels7, Int32 prmRels8, Int32 prmRels9, Int32 prmRels10, Int32 prmRels11, Int32 prmRels12, Int32 prmRels13, Int32 prmRels14, Int32 prmRels15, Int32 prmRels16, Int32 prmRels17, Int32 prmRels18, Int32 prmRels19, Int32 prmRels20, Int32 prmRels21, Int32 prmRels22, Int32 prmRels23, Int32 prmRels24, Int32 prmRels25, Int32 prmRels26, Int32 prmRels27, Int32 prmRels28, decimal prmMatchup, string prmDoGsa, string prmDoMr, string prmDoSpeed, string prmDropRc, string prmInkAlForms, string prmEstList, string prmEstimate, Int32 prmForm, Int32 prmBlank, string prmLvoverride, string prmVendor, ref string prmRecId)
    {
        string cError = "";
        string prmRecId1 = "";
        dsGet3qtyDataSet dsGet3qty = new dsGet3qtyDataSet();
        dsGet3qty = null;
        AppServerConnect();
        aoObject.GetQty3(prmUser, prmAction, prmQty1, prmQty2, prmQty3, prmQty4, prmQty5, prmQty6, prmQty7, prmQty8, prmQty9, prmQty10, prmQty11, prmQty12, prmQty13, prmQty14, prmQty15, prmQty16, prmQty17, prmQty18, prmQty19, prmQty20, prmQty21, prmQty22, prmQty23, prmQty24, prmQty25, prmQty26, prmQty27, prmQty28, prmRels1, prmRels2, prmRels3, prmRels4, prmRels5, prmRels6, prmRels7, prmRels8, prmRels9, prmRels10, prmRels11, prmRels12, prmRels13, prmRels14, prmRels15, prmRels16, prmRels17, prmRels18, prmRels19, prmRels20, prmRels21, prmRels22, prmRels23, prmRels24, prmRels25, prmRels26, prmRels27, prmRels28, prmMatchup, prmDoGsa, prmDoMr, prmDoSpeed, prmDropRc, prmInkAlForms, prmEstList, prmEstimate, prmForm, prmBlank, prmLvoverride,prmVendor, ref dsGet3qty, out cError, out prmRecId1);
        AppServerDisconnect();

        prmRecId = prmRecId1;

        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGet3qty;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGetQtyTandDataSet SelectQtyTandem(string prmUser, string prmAction, string prmDoGsa, string prmDoMr, string prmDoSpeed, string prmDropRc, string prmEstimate, Int32 prmForm, Int32 prmBlank, string prmLvoverride, string prmVendor)
    {
        string cError = "";

        dsGetQtyTandDataSet dsGetQtyTand = new dsGetQtyTandDataSet();
        dsGetQtyTand = null;
        AppServerConnect();
        aoObject.GetQtyTandem(prmUser, prmAction, prmDoGsa, prmDoMr, prmDoSpeed, prmDropRc, prmEstimate, prmForm, prmBlank, prmLvoverride,prmVendor, ref dsGetQtyTand, out cError);
        AppServerDisconnect();


        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGetQtyTand;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGsa3FileDataSet SelectGsa3(string prmAction, string prmUser, string prmRowId, Int32 prmIpQty, Int32 prmIpRels, string prmEst, Int32 prmQty, decimal prmBoard, decimal prmMaterial, decimal prmLabor, decimal prmWHMark, decimal prmBrComm, Int32 prmUnCount, Int32 prmPallCount, decimal prmTotUnit, Int32 prmUnitprPal, decimal prmPaHandCh, Int32 prmPallDelMon, Int32 prmTotCharge, decimal prmCostPerPall, Int32 prmTotPall, Int32 prmSeq, Int32 prmGetqty, Int32 prmForm, Int32 prmBlank, decimal prmMatchup, string prmDoGsa, string prmDoMr, string prmDoSpeed, string prmDropRc, string prmInkAlForms, string prmEstList, string prmVendor, decimal prmGsaMat, decimal prmGsaLab, decimal prmGsaWar, decimal prmGsaFm, Int32 prmGsaMonth)
    {
        string cError = "";
        dsGsa3FileDataSet dsGsa3File = new dsGsa3FileDataSet();
        dsGsa3File = null;
        AppServerConnect();
        aoObject.gsa3(prmAction, prmUser, prmRowId, prmIpQty, prmIpRels, prmEst, prmQty, prmBoard, prmMaterial, prmLabor, prmWHMark, prmBrComm, prmUnCount, prmPallCount, prmTotUnit, prmUnitprPal, prmPaHandCh, prmPallDelMon, prmTotCharge, prmCostPerPall, prmTotPall,prmSeq,  prmGetqty, prmForm, prmBlank, prmMatchup,  prmDoGsa,  prmDoMr,  prmDoSpeed,  prmDropRc,  prmInkAlForms,  prmEstList,prmVendor, prmGsaMat,  prmGsaLab,  prmGsaWar,  prmGsaFm,  prmGsaMonth,ref dsGsa3File, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGsa3File;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGsaFile2DataSet SelectGsa2(string prmAction, string prmUser, string prmRowId, Int32 prmIpQty, Int32 prmIpRels, string prmEst, Int32 prmQty, decimal prmBoard, decimal prmMaterial, decimal prmLabor, decimal prmWHMark, decimal prmBrComm, Int32 prmUnCount, Int32 prmPallCount, decimal prmTotUnit, Int32 prmUnitprPal, decimal prmPaHandCh, Int32 prmPallDelMon, Int32 prmTotCharge, decimal prmCostPerPall, Int32 prmTotPall, Int32 prmSeq, Int32 prmGetqty, Int32 prmForm, Int32 prmBlank, string prmDoGsa, string prmDoMr, string prmDoSpeed, string prmDropRc, string prmVendor, decimal prmGsaMat, decimal prmGsaLab, decimal prmGsaWar, decimal prmGsaFm, Int32 prmGsaMonth)
    {
        string cError = "";
        dsGsaFile2DataSet dsGsaFile2 = new dsGsaFile2DataSet();
        dsGsaFile2 = null;
        AppServerConnect();
        aoObject.gsa2(prmAction, prmUser, prmRowId, prmIpQty, prmIpRels, prmEst, prmQty, prmBoard, prmMaterial, prmLabor, prmWHMark, prmBrComm, prmUnCount, prmPallCount, prmTotUnit, prmUnitprPal, prmPaHandCh, prmPallDelMon, prmTotCharge, prmCostPerPall, prmTotPall, prmSeq, prmGetqty, prmForm, prmBlank, prmDoGsa, prmDoMr, prmDoSpeed, prmDropRc,prmVendor, prmGsaMat,  prmGsaLab,  prmGsaWar,  prmGsaFm,  prmGsaMonth, ref dsGsaFile2, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGsaFile2;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldGetqtyDataSet SelectFoldGetQty(string prmUser, string prmAction, Int32 prmQty1, Int32 prmQty2, Int32 prmQty3, Int32 prmQty4, Int32 prmQty5, Int32 prmQty6, Int32 prmQty7, Int32 prmQty8, Int32 prmQty9, Int32 prmQty10, Int32 prmQty11, Int32 prmQty12, Int32 prmQty13, Int32 prmQty14, Int32 prmQty15, Int32 prmQty16, Int32 prmQty17, Int32 prmQty18, Int32 prmQty19, Int32 prmQty20, Int32 prmQty21, Int32 prmQty22, Int32 prmQty23, Int32 prmQty24, Int32 prmQty25, Int32 prmQty26, Int32 prmQty27, Int32 prmQty28, Int32 prmRels1, Int32 prmRels2, Int32 prmRels3, Int32 prmRels4, Int32 prmRels5, Int32 prmRels6, Int32 prmRels7, Int32 prmRels8, Int32 prmRels9, Int32 prmRels10, Int32 prmRels11, Int32 prmRels12, Int32 prmRels13, Int32 prmRels14, Int32 prmRels15, Int32 prmRels16, Int32 prmRels17, Int32 prmRels18, Int32 prmRels19, Int32 prmRels20, Int32 prmRels21, Int32 prmRels22, Int32 prmRels23, Int32 prmRels24, Int32 prmRels25, Int32 prmRels26, Int32 prmRels27, Int32 prmRels28,  string prmDoGsa, string prmDoMr, string prmDoSpeed, string prmEstList, string prmEstimate, Int32 prmForm, Int32 prmBlank, string prmLvoverride, string prmVendor)
    {
        string cError = "";
        dsFoldGetqtyDataSet dsFoldGetqty = new dsFoldGetqtyDataSet();
        dsFoldGetqty = null;
        AppServerConnect();
        aoObject.FoldGetQty(prmUser, prmAction, prmQty1, prmQty2, prmQty3, prmQty4, prmQty5, prmQty6, prmQty7, prmQty8, prmQty9, prmQty10, prmQty11, prmQty12, prmQty13, prmQty14, prmQty15, prmQty16, prmQty17, prmQty18, prmQty19, prmQty20, prmQty21, prmQty22, prmQty23, prmQty24, prmQty25, prmQty26, prmQty27, prmQty28, prmRels1, prmRels2, prmRels3, prmRels4, prmRels5, prmRels6, prmRels7, prmRels8, prmRels9, prmRels10, prmRels11, prmRels12, prmRels13, prmRels14, prmRels15, prmRels16, prmRels17, prmRels18, prmRels19, prmRels20, prmRels21, prmRels22, prmRels23, prmRels24, prmRels25, prmRels26, prmRels27, prmRels28, prmDoGsa, prmDoMr, prmDoSpeed, prmEstList, prmEstimate, prmForm, prmBlank, prmLvoverride, prmVendor, ref dsFoldGetqty, out cError);
        AppServerDisconnect();
                
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsFoldGetqty;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldGsaFileDataSet SelectFoldGsa(string prmAction, string prmUser, string prmRowId, Int32 prmIpQty, Int32 prmIpRels, string prmEst, Int32 prmQty, decimal prmBoard, decimal prmMaterial, decimal prmLabor, decimal prmWHMark, decimal prmGsafm, Int32 prmSeq, Int32 prmGetqty, Int32 prmForm, Int32 prmBlank, string prmDoGsa, string prmDoMr, string prmDoSpeed, string prmEstList, string prmVendor, decimal prmGsaMat, decimal prmGsaLab, decimal prmGsaWar, Int32 prmGsaMonth)
    {
        string cError = "";
        dsFoldGsaFileDataSet dsFoldGsaFile = new dsFoldGsaFileDataSet();
        dsFoldGsaFile = null;
        AppServerConnect();
        aoObject.foldgsa(prmAction, prmUser, prmRowId, prmIpQty, prmIpRels, prmEst, prmQty, prmBoard, prmMaterial, prmLabor, prmWHMark, prmGsafm, prmSeq, prmGetqty, prmForm, prmBlank, prmDoGsa, prmDoMr, prmDoSpeed, prmEstList, prmVendor, prmGsaMat,  prmGsaLab,  prmGsaWar,  prmGsaMonth, ref dsFoldGsaFile, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsFoldGsaFile;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldGsaFile2DataSet SelectFoldGsa2(string prmAction, string prmUser, string prmRowId, Int32 prmIpQty, Int32 prmIpRels, string prmEst, Int32 prmQty, decimal prmBoard, decimal prmMaterial, decimal prmLabor, decimal prmWHMark, decimal prmGsafm, Int32 prmSeq, Int32 prmGetqty, Int32 prmForm, Int32 prmBlank, string prmDoGsa, string prmDoMr, string prmDoSpeed, string prmVendor, decimal prmGsaMat, decimal prmGsaLab, decimal prmGsaWar, Int32 prmGsaMonth)
    {
        string cError = "";
        dsFoldGsaFile2DataSet dsFoldGsaFile2 = new dsFoldGsaFile2DataSet();
        dsFoldGsaFile2 = null;
        AppServerConnect();
        aoObject.foldgsa2(prmAction, prmUser, prmRowId, prmIpQty, prmIpRels, prmEst, prmQty, prmBoard, prmMaterial, prmLabor, prmWHMark, prmGsafm, prmSeq, prmGetqty, prmForm, prmBlank, prmDoGsa, prmDoMr, prmDoSpeed,prmVendor, prmGsaMat,  prmGsaLab,  prmGsaWar,  prmGsaMonth, ref dsFoldGsaFile2, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsFoldGsaFile2;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGetFoldQtyTandemDataSet SelectFoldQtyTandem(string prmUser, string prmAction, string prmDoGsa, string prmDoMr, string prmDoSpeed, string prmEstimate, Int32 prmForm, Int32 prmBlank, string prmLvoverride, string prmVendor)
    {
        string cError = "";

        dsGetFoldQtyTandemDataSet dsGetFoldQtyTandem = new dsGetFoldQtyTandemDataSet();
        dsGetFoldQtyTandem = null;
        AppServerConnect();
        aoObject.GetFoldQtyTandem(prmUser, prmAction, prmDoGsa, prmDoMr, prmDoSpeed, prmEstimate, prmForm, prmBlank, prmLvoverride,prmVendor, ref dsGetFoldQtyTandem, out cError);
        AppServerDisconnect();


        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGetFoldQtyTandem;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldSetQtyDataSet FoldSetQty(string prmUser, string prmAction, Int32 prmQty1, Int32 prmQty2, Int32 prmQty3, Int32 prmQty4, Int32 prmQty5, Int32 prmQty6, Int32 prmQty7, Int32 prmQty8, Int32 prmQty9, Int32 prmQty10, Int32 prmQty11, Int32 prmQty12, Int32 prmQty13, Int32 prmQty14, Int32 prmQty15, Int32 prmQty16, Int32 prmQty17, Int32 prmQty18, Int32 prmQty19, Int32 prmQty20, Int32 prmQty21, Int32 prmQty22, Int32 prmQty23, Int32 prmQty24, Int32 prmQty25, Int32 prmQty26, Int32 prmQty27, Int32 prmQty28, Int32 prmRels1, Int32 prmRels2, Int32 prmRels3, Int32 prmRels4, Int32 prmRels5, Int32 prmRels6, Int32 prmRels7, Int32 prmRels8, Int32 prmRels9, Int32 prmRels10, Int32 prmRels11, Int32 prmRels12, Int32 prmRels13, Int32 prmRels14, Int32 prmRels15, Int32 prmRels16, Int32 prmRels17, Int32 prmRels18, Int32 prmRels19, Int32 prmRels20, Int32 prmRels21, Int32 prmRels22, Int32 prmRels23, Int32 prmRels24, Int32 prmRels25, Int32 prmRels26, Int32 prmRels27, Int32 prmRels28, string prmDoGsa, string prmDoMr, string prmDoSpeed, string prmEstList, string prmEstimate, Int32 prmForm, Int32 prmBlank, string prmLvoverride, string prmVendor)
    {
        string cError = "";
        dsFoldSetQtyDataSet dsFoldSetQty = new dsFoldSetQtyDataSet();
        dsFoldSetQty = null;
        AppServerConnect();
        aoObject.FoldSetQty(prmUser, prmAction, prmQty1, prmQty2, prmQty3, prmQty4, prmQty5, prmQty6, prmQty7, prmQty8, prmQty9, prmQty10, prmQty11, prmQty12, prmQty13, prmQty14, prmQty15, prmQty16, prmQty17, prmQty18, prmQty19, prmQty20, prmQty21, prmQty22, prmQty23, prmQty24, prmQty25, prmQty26, prmQty27, prmQty28, prmRels1, prmRels2, prmRels3, prmRels4, prmRels5, prmRels6, prmRels7, prmRels8, prmRels9, prmRels10, prmRels11, prmRels12, prmRels13, prmRels14, prmRels15, prmRels16, prmRels17, prmRels18, prmRels19, prmRels20, prmRels21, prmRels22, prmRels23, prmRels24, prmRels25, prmRels26, prmRels27, prmRels28, prmDoGsa, prmDoMr, prmDoSpeed, prmEstList, prmEstimate, prmForm, prmBlank, prmLvoverride,prmVendor, ref dsFoldSetQty, out cError);
        AppServerDisconnect();

        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsFoldSetQty;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFoldSetGsaDataSet FoldSetGsa(string prmAction, string prmUser, string prmRowId, Int32 prmIpQty, Int32 prmIpRels, string prmEst, Int32 prmQty, decimal prmBoard, decimal prmMaterial, decimal prmLabor, decimal prmWHMark, decimal prmGsafm, Int32 prmSeq, Int32 prmGetqty, Int32 prmForm, Int32 prmBlank, string prmDoGsa, string prmDoMr, string prmDoSpeed, string prmEstList, string prmVendor, decimal prmGsaMat, decimal prmGsaLab, decimal prmGsaWar, Int32 prmGsaMonth)
    {
        string cError = "";
        dsFoldSetGsaDataSet dsFoldSetGsa = new dsFoldSetGsaDataSet();
        dsFoldSetGsa = null;
        AppServerConnect();
        aoObject.foldsetgsa(prmAction, prmUser, prmRowId, prmIpQty, prmIpRels, prmEst, prmQty, prmBoard, prmMaterial, prmLabor, prmWHMark, prmGsafm, prmSeq, prmGetqty, prmForm, prmBlank, prmDoGsa, prmDoMr, prmDoSpeed, prmEstList,prmVendor, prmGsaMat,  prmGsaLab,  prmGsaWar,  prmGsaMonth, ref dsFoldSetGsa, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsFoldSetGsa;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorrdoponoDataSet VendorCost(string prmUser, string prmAction, string prmComp, string prmEstNum, Int32 prmFormNo, Int32 prmBlankNo, string prmItem)
    {
        string cError = "";
        dsCorrdoponoDataSet dsCorrdopono = new dsCorrdoponoDataSet();
        dsCorrdopono = null;
        AppServerConnect();
        aoObject.Corr_Po(prmUser,  prmAction,  prmComp,  prmEstNum,  prmFormNo, prmBlankNo,prmItem, ref dsCorrdopono, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsCorrdopono;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsEstVendorGetDataSet SelectVendor(string prmAction, string prmUser, string prmEstimate)
    {
        
        dsEstVendorGetDataSet dsEstVendorGet = new dsEstVendorGetDataSet();
        dsEstVendorGet = null;
        AppServerConnect();
        aoObject.Estvendor(prmAction,prmUser, prmEstimate, ref dsEstVendorGet);
        AppServerDisconnect();        
        return dsEstVendorGet;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsEItemVendorDataSet SelectVendorItem(string prmAction, string prmUser, string prmEst, Int32 prmFormNo, Int32 prmBlanck, string prmItem, string prmStdum, string prmVendItem, string prmVendNO, string prmDate, decimal prmRunQty1, decimal prmRunQty2, decimal prmRunQty3, decimal prmRunQty4, decimal prmRunQty5, decimal prmRunQty6, decimal prmRunQty7, decimal prmRunQty8, decimal prmRunQty9, decimal prmRunQty10, decimal prmSetups1, decimal prmSetups2, decimal prmSetups3, decimal prmSetups4, decimal prmSetups5, decimal prmSetups6, decimal prmSetups7, decimal prmSetups8, decimal prmSetups9, decimal prmSetups10, decimal prmRunCost1, decimal prmRunCost2, decimal prmRunCost3, decimal prmRunCost4, decimal prmRunCost5, decimal prmRunCost6, decimal prmRunCost7, decimal prmRunCost8, decimal prmRunCost9, decimal prmRunCost10, decimal prmRollw1, decimal prmRollw2, decimal prmRollw3, decimal prmRollw4, decimal prmRollw5, decimal prmRollw6, decimal prmRollw7, decimal prmRollw8, decimal prmRollw9, decimal prmRollw10, decimal prmRollw11, decimal prmRollw12, decimal prmRollw13, decimal prmRollw14, decimal prmRollw15, decimal prmRollw16, decimal prmRollw17, decimal prmRollw18, decimal prmRollw19, decimal prmRollw20, decimal prmRollw21, decimal prmRollw22, decimal prmRollw23, decimal prmRollw24, decimal prmRollw25, decimal prmRollw26, decimal prmRollw27, decimal prmRollw28, decimal prmRollw29, decimal prmRollw30, string prmRowid1, string prmRowid2, decimal prmWidthmin, decimal prmLengthmin, decimal prmWidthcst, decimal prmLengthcst) 
    {
        string cError = "";
        dsEItemVendorDataSet dsEItemVendor = new dsEItemVendorDataSet();
        dsEItemVendor = null;
        AppServerConnect();
        aoObject.corr_eitem(prmAction, prmUser, prmEst, prmFormNo, prmBlanck, prmItem, prmStdum, prmVendItem, prmVendNO, prmDate, prmRunQty1, prmRunQty2, prmRunQty3, prmRunQty4, prmRunQty5, prmRunQty6, prmRunQty7, prmRunQty8, prmRunQty9, prmRunQty10, prmSetups1, prmSetups2, prmSetups3, prmSetups4, prmSetups5, prmSetups6, prmSetups7, prmSetups8, prmSetups9, prmSetups10, prmRunCost1, prmRunCost2, prmRunCost3, prmRunCost4, prmRunCost5, prmRunCost6, prmRunCost7, prmRunCost8, prmRunCost9, prmRunCost10, prmRollw1, prmRollw2, prmRollw3, prmRollw4, prmRollw5, prmRollw6, prmRollw7, prmRollw8, prmRollw9, prmRollw10, prmRollw11, prmRollw12, prmRollw13, prmRollw14, prmRollw15, prmRollw16, prmRollw17, prmRollw18, prmRollw19, prmRollw20, prmRollw21, prmRollw22, prmRollw23, prmRollw24, prmRollw25, prmRollw26, prmRollw27, prmRollw28, prmRollw29, prmRollw30,prmRowid1,prmRowid2, prmWidthmin, prmLengthmin, prmWidthcst, prmLengthcst, ref dsEItemVendor, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsEItemVendor;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorUomLookDataSet SelectUomLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmItem)
    {

        dsCorUomLookDataSet dsCorUomLook = new dsCorUomLookDataSet();
        dsCorUomLook = null;
        AppServerConnect();
        aoObject.CorUomLook(prmAction, prmUser, prmField, prmCondition, prmText, prmItem, ref dsCorUomLook);
        AppServerDisconnect();

        return dsCorUomLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCorVendorLookDataSet VendorLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmActive)
    {

        dsCorVendorLookDataSet dsCorVendorLook = new dsCorVendorLookDataSet();
        dsCorVendorLook = null;
        AppServerConnect();
        aoObject.VendorLookup(prmAction, prmUser, prmField, prmCondition, prmText, prmActive, ref dsCorVendorLook);
        AppServerDisconnect();

        return dsCorVendorLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFgItemVendDataSet SelectVendFGItem(string prmAction, string prmUser, string prmEst, Int32 prmFormNo, Int32 prmBlanck, string prmItem, string prmStdum, string prmVendItem, string prmVendNO, string prmCust, decimal prmRunQty1, decimal prmRunQty2, decimal prmRunQty3, decimal prmRunQty4, decimal prmRunQty5, decimal prmRunQty6, decimal prmRunQty7, decimal prmRunQty8, decimal prmRunQty9, decimal prmRunQty10, decimal prmSetups1, decimal prmSetups2, decimal prmSetups3, decimal prmSetups4, decimal prmSetups5, decimal prmSetups6, decimal prmSetups7, decimal prmSetups8, decimal prmSetups9, decimal prmSetups10, decimal prmRunCost1, decimal prmRunCost2, decimal prmRunCost3, decimal prmRunCost4, decimal prmRunCost5, decimal prmRunCost6, decimal prmRunCost7, decimal prmRunCost8, decimal prmRunCost9, decimal prmRunCost10, decimal prmRollw1, decimal prmRollw2, decimal prmRollw3, decimal prmRollw4, string prmRowid1, string prmRowid2, decimal prmMarkup, string prmTbSel, string prmTbSel1, string prmTbSel2, string prmTbSel3, string prmTbSel4, string prmTbSel5, string prmTbSel6, string prmTbSel7, string prmTbSel8, string prmTbSel9, string prmTbSel10)
    {
        string cError = "";
        dsFgItemVendDataSet dsFgItemVend = new dsFgItemVendDataSet();
        dsFgItemVend = null;
        AppServerConnect();
        aoObject.corr_fgitem(prmAction, prmUser, prmEst, prmFormNo, prmBlanck, prmItem, prmStdum, prmVendItem, prmVendNO, prmCust, prmRunQty1, prmRunQty2, prmRunQty3, prmRunQty4, prmRunQty5, prmRunQty6, prmRunQty7, prmRunQty8, prmRunQty9, prmRunQty10, prmSetups1, prmSetups2, prmSetups3, prmSetups4, prmSetups5, prmSetups6, prmSetups7, prmSetups8, prmSetups9, prmSetups10, prmRunCost1, prmRunCost2, prmRunCost3, prmRunCost4, prmRunCost5, prmRunCost6, prmRunCost7, prmRunCost8, prmRunCost9, prmRunCost10, prmRollw1, prmRollw2, prmRollw3, prmRollw4, prmRowid1, prmRowid2, prmMarkup, prmTbSel, prmTbSel1, prmTbSel2, prmTbSel3, prmTbSel4, prmTbSel5, prmTbSel6, prmTbSel7, prmTbSel8, prmTbSel9, prmTbSel10, ref dsFgItemVend, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsFgItemVend;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsProbeWhitifCalDataSet ProbeWatif(string prmAction, string prmUser, string prmChange, decimal prmGross, decimal prmNet, decimal prmSellPrice, string prmQ, decimal prmBoardM, decimal prmBoard, decimal prmBoardContM, decimal prmBoardCont, decimal prmMarketprice, decimal prmFactCost, decimal prmFullCost, string prmEstimate, Int32 prmLine)
    {

        dsProbeWhitifCalDataSet dsProbeWhitifCal = new dsProbeWhitifCalDataSet();
        dsProbeWhitifCal = null;
        AppServerConnect();
        aoObject.probewhatif(prmAction, prmUser, prmChange, prmGross, prmNet, prmSellPrice, prmQ, prmBoardM, prmBoard, prmBoardContM, prmBoardCont, prmMarketprice, prmFactCost, prmFullCost, prmEstimate, prmLine, ref dsProbeWhitifCal);
        AppServerDisconnect();

        return dsProbeWhitifCal;
    }



    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsprobeitestimateDataSet SelectEstimateProbeit(string prmUser, string prmAction, string prmEstimate, Int32 prmLine, decimal prmSellPrice, string prmPartNo)
    {
        string cError = "";
        dsprobeitestimateDataSet dsprobeitestimate = new dsprobeitestimateDataSet();
        dsprobeitestimate = null;
        AppServerConnect();
        aoObject.est_probeit(prmUser, prmAction, prmEstimate, prmLine, prmSellPrice, prmPartNo, ref dsprobeitestimate, out cError);
        AppServerDisconnect();

        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsprobeitestimate;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsfoldprobeitestimateDataSet SelectFoldEstimateProbeit(string prmUser, string prmAction, string prmEstimate, Int32 prmLine, decimal prmSellPrice, string prmPartNo)
    {
        string cError = "";
        dsfoldprobeitestimateDataSet dsfoldprobeitestimate = new dsfoldprobeitestimateDataSet();
        dsfoldprobeitestimate = null;
        AppServerConnect();
        aoObject.fold_est_probeit(prmUser, prmAction, prmEstimate, prmLine, prmSellPrice, prmPartNo, ref dsfoldprobeitestimate, out cError);
        AppServerDisconnect();

        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsfoldprobeitestimate;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsQuotearinvpopupDataSet QuoteItemPupup(string prmAction, string prmUser, Int32 prmQuote, string prmCondition, string prmText)
    {
        dsQuotearinvpopupDataSet dsQuotearinvpopup = new dsQuotearinvpopupDataSet();
        dsQuotearinvpopup = null;
        AppServerConnect();
        aoObject.quotearinv(prmAction, prmUser,  prmQuote,  prmCondition,  prmText, ref dsQuotearinvpopup);
        AppServerDisconnect();

        return dsQuotearinvpopup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCopyEstimateDataSet SelectCopyEstimate(string prmAction, string prmUser, string prmFromComp, string prmFromEstimate, string prmCopyroute, string prmCopydie, string prmCopyplate, string prmCopyfgitem, string prmCopycost, string prmCopynote, string prmTocompany, string prmToestimate, string prmNewcust, string prmnewcustpart, string prmCopyItemName, string prmCopyItemDesc, string prmCopyItemDesc2)
    {
        string cError = "";
        string cError2 = "";
        dsCopyEstimateDataSet dsCopyEstimate = new dsCopyEstimateDataSet();
        dsCopyEstimate = null;
        AppServerConnect();
        aoObject.CopyEstimate(prmAction, prmUser, prmFromComp, prmFromEstimate, prmCopyroute, prmCopydie, prmCopyplate, prmCopyfgitem, prmCopycost, prmCopynote, prmTocompany, prmToestimate, prmNewcust, prmnewcustpart, prmCopyItemName, prmCopyItemDesc, prmCopyItemDesc2, out cError, out cError2, ref dsCopyEstimate);
        AppServerDisconnect();

        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCopyEstimate;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateCopyEstimate(string prmAction, string prmUser, string prmFromComp, string prmFromEstimate, string prmCopyroute, string prmCopydie, string prmCopyplate, string prmCopyfgitem, string prmCopycost, string prmCopynote, string prmTocompany, string prmToestimate, string prmNewcust, string prmnewcustpart, string prmCopyItemName, string prmCopyItemDesc, string prmCopyItemDesc2)
    {
        string cError = "";
        string cError2 = "";
        dsCopyEstimateDataSet dsCopyEstimate = new dsCopyEstimateDataSet();
        AppServerConnect();
        aoObject.CopyEstimate(prmAction, prmUser, prmFromComp, prmFromEstimate, prmCopyroute, prmCopydie, prmCopyplate, prmCopyfgitem, prmCopycost, prmCopynote, prmTocompany, prmToestimate, prmNewcust, prmnewcustpart, prmCopyItemName, prmCopyItemDesc, prmCopyItemDesc2, out cError, out cError2, ref dsCopyEstimate);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            return false;
        }
        else
        {
            return true;
        }

    }
}
   
    
            
      
      
        
    
   
 
   

     
    
     
  

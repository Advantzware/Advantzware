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
public class custitem : AppServerConnect.AppServer
{
    public custitem()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsbrwscustitemDataSet SelectCustitem(string prmUser, string prmReckey, string prmActCitem, string prmCust, string prmLoc, string prmItem, decimal prmQty, decimal prmConsum, string prmUpdateCust, string prmUpdateloc, string prmUpdateItem)
    {
        string cError = "";
        dsbrwscustitemDataSet dsbrwscustitem = new dsbrwscustitemDataSet();
        dsbrwscustitem = null;
        AppServerConnect();
        aoObject.custitem(prmUser, prmReckey, prmActCitem, prmCust, prmLoc, prmItem, prmQty, prmConsum, prmUpdateCust,  prmUpdateloc,  prmUpdateItem, ref dsbrwscustitem, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsbrwscustitem;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsbrwsWarehouseCustDataSet SelectCustPlant(string prmUser, string prmReckey, string prmActPlant, string prmCust, string prmShipid, string prmPlantid, string prmDeptCode, string prmPlantName, string prmVanderCode)
    {
        string cError = "";
        dsbrwsWarehouseCustDataSet dsbrwsWarehouseCust = new dsbrwsWarehouseCustDataSet();
        dsbrwsWarehouseCust = null;
        AppServerConnect();
        aoObject.CustPlant(prmUser,  prmReckey,  prmActPlant,  prmCust,  prmShipid,  prmPlantid,  prmDeptCode,  prmPlantName,  prmVanderCode, out cError, ref dsbrwsWarehouseCust);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsbrwsWarehouseCust;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsItemUsageDataSet Selectdailyusege(string prmUser, string prmAction, Int32 prmSeqNum, string prmComp, DateTime prmRcptDate, string prmCustPoNum, string prmCustPartNum, string prmFgItemNum)
    {
        dsItemUsageDataSet dsItemUsage = new dsItemUsageDataSet();
        dsItemUsage = null;
        AppServerConnect();
        aoObject.ItemDailyUsage(prmUser, prmAction, prmSeqNum, prmComp, prmRcptDate, prmCustPoNum, prmCustPartNum, prmFgItemNum, ref dsItemUsage);
        AppServerDisconnect();
        return dsItemUsage;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUpdateUsageDataSet UpdateCustPlant(string prmUser, string prmAction, Int32 prmSeqNum, string prmComp, DateTime prmRcptDate, Decimal prmQtyUsed, string prmCustPoNum, Int32 prmCustLineNum, string prmCustPartNum, string prmFgItemNum, string prmCustVnCode, string prmCustPlantId, string prmCustDptCode,Int32 prmCustOrdNum, string prmCustJobNum, Int32 prmCustJob2Num, Decimal prmSellingPrice, Decimal prmOnHandQty, string prmTranType)
    {
        string cError = "";
        dsUpdateUsageDataSet dsUpdateUsage = new dsUpdateUsageDataSet();
        dsUpdateUsage = null;
        AppServerConnect();
        aoObject.UpdateDailyUsage(prmUser,  prmAction,  prmSeqNum,  prmComp,  prmRcptDate,  prmQtyUsed,  prmCustPoNum,  prmCustLineNum,  prmCustPartNum,  prmFgItemNum,  prmCustVnCode,  prmCustPlantId,  prmCustDptCode, prmCustOrdNum,  prmCustJobNum,  prmCustJob2Num,  prmSellingPrice,  prmOnHandQty,  prmTranType, out cError, ref dsUpdateUsage);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsUpdateUsage;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCustInventoryDataSet SelectCustInv(string prmUser, string prmAction,  string prmComp, string prmCustNum , string prmCustPartNum,  string prmFgItmNum, string prmVendCode, string prmVendPlantId, Decimal prmAnnUsageQty, Decimal prmOnHandQty)
    {
        
        dsCustInventoryDataSet dsCustInventory = new dsCustInventoryDataSet();
        dsCustInventory = null;
        AppServerConnect();
        aoObject.CustomerInventory( prmUser,  prmAction,  prmComp,  prmCustNum ,  prmCustPartNum,   prmFgItmNum, prmVendCode,  prmVendPlantId,  prmAnnUsageQty,  prmOnHandQty, ref dsCustInventory);
        AppServerDisconnect();
        
        return dsCustInventory;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUpdateCustInvDataSet updateCustInv(string prmUser, string prmAction, string prmComp, string prmCustNum, string prmRevision, string prmCustPartNum, string prmFgItmNum, string prmVendCode, string prmVendPlantId, string prmVendDeptCode, string prmOsoleteDate, Decimal prmAnnUsageQty, Decimal prmOnHandQty, string prmRecId)
    {
        string cError = "";
        dsUpdateCustInvDataSet dsUpdateCustInv = new dsUpdateCustInvDataSet();
        dsUpdateCustInv = null;
        AppServerConnect();
        aoObject.UpdateCustInventory(prmUser, prmAction, prmComp, prmCustNum, prmRevision, prmCustPartNum, prmFgItmNum, prmVendCode, prmVendPlantId, prmVendDeptCode, prmOsoleteDate, prmAnnUsageQty, prmOnHandQty, prmRecId, out cError, ref dsUpdateCustInv);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsUpdateCustInv;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsItemReceiptDataSet Selectreceipt(string prmUser, string prmAction, Int32 prmSeqNum, string prmComp, DateTime prmRcptDate, string prmCustPoNum, string prmCustPartNum, string prmFgItemNum)
    {
        dsItemReceiptDataSet dsItemReceipt = new dsItemReceiptDataSet();
        dsItemReceipt = null;
        AppServerConnect();
        aoObject.ItemDailyReceipt(prmUser, prmAction, prmSeqNum, prmComp, prmRcptDate, prmCustPoNum, prmCustPartNum, prmFgItemNum, ref dsItemReceipt);
        AppServerDisconnect();
        return dsItemReceipt;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUpdateReceiptDataSet UpdateReceipt(string prmUser, string prmAction, Int32 prmSeqNum, Int32 prmVendBolNo, string prmComp, DateTime prmTranDate, Decimal prmQtyUsed, string prmCustPoNum, Int32 prmCustLineNum, string prmPartNum, string prmFgItmNum, string prmCustVnCode, string prmCustPlantId, string prmCustDptCode, Int32 prmCustOrdNum, string prmCustJobNum, Int32 prmCustJob2Num, Decimal prmSellingPrice, Decimal prmOnHandQty, string prmTranType)
    {
        string cError = "";
        dsUpdateReceiptDataSet dsUpdateReceipt = new dsUpdateReceiptDataSet();
        dsUpdateReceipt = null;
        AppServerConnect();
        aoObject.UpdateDailyReceipt( prmUser,  prmAction,  prmSeqNum,  prmVendBolNo,  prmComp,  prmTranDate,  prmQtyUsed,  prmCustPoNum,  prmCustLineNum,  prmPartNum,  prmFgItmNum,  prmCustVnCode,  prmCustPlantId,  prmCustDptCode,  prmCustOrdNum,  prmCustJobNum,  prmCustJob2Num,  prmSellingPrice,  prmOnHandQty,  prmTranType, out cError, ref dsUpdateReceipt);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsUpdateReceipt;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateUpdateReceipt(string prmUser, string prmAction, Int32 prmSeqNum, Int32 prmVendBolNo, string prmComp, DateTime prmTranDate, Decimal prmQtyUsed, string prmCustPoNum, Int32 prmCustLineNum, string prmPartNum, string prmFgItmNum, string prmCustVnCode, string prmCustPlantId, string prmCustDptCode, Int32 prmCustOrdNum, string prmCustJobNum, Int32 prmCustJob2Num, Decimal prmSellingPrice, Decimal prmOnHandQty, string prmTranType)
    {
        string cError = "";
        dsUpdateReceiptDataSet dsUpdateReceipt = new dsUpdateReceiptDataSet();
       
        AppServerConnect();
        aoObject.UpdateDailyReceipt(prmUser, prmAction, prmSeqNum, prmVendBolNo, prmComp, prmTranDate, prmQtyUsed, prmCustPoNum, prmCustLineNum, prmPartNum, prmFgItmNum, prmCustVnCode, prmCustPlantId, prmCustDptCode, prmCustOrdNum, prmCustJobNum, prmCustJob2Num, prmSellingPrice, prmOnHandQty, prmTranType, out cError, ref dsUpdateReceipt);
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
    public dsVendorCrossReferenceDataSet Selectvendorcross(string prmUser, string prmAction, string prmCustNo, string prmVendorCode, string prmCustName, string prmComp, string prmUpdateCustno)
    {
        string cError = "";
        dsVendorCrossReferenceDataSet dsVendorCrossReference = new dsVendorCrossReferenceDataSet();
        dsVendorCrossReference = null;
        AppServerConnect();
        aoObject.VendorCrossReference(prmUser, prmAction, prmCustNo, prmVendorCode, prmCustName, prmComp, prmUpdateCustno, out cError, ref dsVendorCrossReference);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsVendorCrossReference;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsWarehouseHistoryDataSet Selectwarehousehist(string prmUser, string prmAction, Int32 prmSeqNo, string prmTransDate, string prmCustomersPoNo, string prmCustPart, string prmFgItem, string prmReckey, string prmComp)
    {
        string cError = "";
        dsWarehouseHistoryDataSet dsWarehouseHistory = new dsWarehouseHistoryDataSet();
        dsWarehouseHistory = null;
        AppServerConnect();
        aoObject.WarehouseHistory(prmUser, prmAction, prmSeqNo, prmTransDate, prmCustomersPoNo, prmCustPart, prmFgItem, prmReckey, prmComp, out cError, ref dsWarehouseHistory);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsWarehouseHistory;
    }    
}
   
    
            
      
      
        
    
   
 
   

     
    
     
  

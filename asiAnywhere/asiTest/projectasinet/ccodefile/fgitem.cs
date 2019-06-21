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
/// Summary description for fgitem
/// </summary>

[System.ComponentModel.DataObject]
public class fgitem : AppServerConnect.AppServer
{
    public fgitem()
    {
        //
        // TODO: Add constructor logic here
        //
    }



    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsOrdItemDataSet SelectFgItem(string prmUser, string prmCust, string prmAction, string prmOrderNum, string prmItemNum, string prmNewItem, string prmPart, string prmName, string prmDscr, string prmDscr2, string prmDscr3, string prmIsSet, string prmIsCust, string prmCustName, string prmTax, string prmPurchase, string prmEstimate, string prmStyle, string prmdie, string prmPlate, string prmCad, string prmSPC, string prmUPC, string prmSell, string prmUom, string prmCurr, string prmCateg, string prmRpt, string prmWareHouse, string prmBin, string prmCount, string prmWeight, string prmFreight, string prmFreClass, string prmInventory, string prmCycle, string prmProduction, string prmPacking, string prmMat, string prmLab, string prmVar, string prmFix, string prmStd, string prmAvg, string prmLast, string prmlUom, string prmExempt, string prmStat, string prmstock, string prmcasepal, string prmtypecode)
   
    {
        string cError = "";
        dsOrdItemDataSet dsOrdItem = new dsOrdItemDataSet();
        dsOrdItem = null;
        AppServerConnect();
        aoObject.OrdItem(prmCust, prmUser, prmAction, prmOrderNum, prmItemNum,prmNewItem, prmPart, prmName, prmDscr, prmDscr2, prmDscr3, prmIsSet, prmIsCust, prmCustName, prmTax, prmPurchase, prmEstimate, prmStyle, prmdie, prmPlate, prmCad, prmSPC, prmUPC, prmSell, prmUom, prmCurr, prmCateg, prmRpt, prmWareHouse, prmBin, prmCount, prmWeight, prmFreight, prmFreClass, prmInventory, prmCycle, prmProduction, prmPacking, prmMat, prmLab, prmVar, prmFix, prmStd, prmAvg, prmLast, prmlUom, prmExempt, prmStat, prmstock, prmcasepal, prmtypecode, ref dsOrdItem, out cError);
       
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }

        return dsOrdItem;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Update, true)]
    public dsOrdItemDataSet Update(string prmUser, string prmCust, string prmAction, string prmOrderNum, string prmItemNum, string prmNewItem, string prmPart, string prmName, string prmDscr, string prmDscr2, string prmDscr3, string prmIsSet, string prmIsCust, string prmCustName, string prmTax, string prmPurchase, string prmEstimate, string prmStyle, string prmdie, string prmPlate, string prmCad, string prmSPC, string prmUPC, string prmSell, string prmUom, string prmCurr, string prmCateg, string prmRpt, string prmWareHouse, string prmBin, string prmCount, string prmWeight, string prmFreight, string prmFreClass, string prmInventory, string prmCycle, string prmProduction, string prmPacking, string prmMat, string prmLab, string prmVar, string prmFix, string prmStd, string prmAvg, string prmLast, string prmlUom, string prmExempt, string prmStat, string prmstock, string prmcasepal, string prmtypecode)
   
    {
        string cError = "";
        dsOrdItemDataSet dsOrdItem = new dsOrdItemDataSet();
        dsOrdItem = null;
        AppServerConnect();
        aoObject.OrdItem(prmCust, prmUser, prmAction, prmOrderNum, prmItemNum,prmNewItem, prmPart, prmName, prmDscr, prmDscr2, prmDscr3, prmIsSet, prmIsCust, prmCustName, prmTax, prmPurchase, prmEstimate, prmStyle, prmdie, prmPlate, prmCad, prmSPC, prmUPC, prmSell, prmUom, prmCurr, prmCateg, prmRpt, prmWareHouse, prmBin, prmCount, prmWeight, prmFreight, prmFreClass, prmInventory, prmCycle, prmProduction, prmPacking, prmMat, prmLab, prmVar, prmFix, prmStd, prmAvg, prmLast, prmlUom, prmExempt, prmStat, prmstock, prmcasepal, prmtypecode, ref dsOrdItem , out cError);
       
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
                        
        }
        return dsOrdItem;

    }



    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Insert, true)]
    public dsOrdItemDataSet InsertFgItem(string prmUser, string prmCust, string prmAction, string prmOrderNum, string prmItemNum, string prmNewItem, string prmPart, string prmName, string prmDscr, string prmDscr2, string prmDscr3, string prmIsSet, string prmIsCust, string prmCustName, string prmTax, string prmPurchase, string prmEstimate, string prmStyle, string prmdie, string prmPlate, string prmCad, string prmSPC, string prmUPC, string prmSell, string prmUom, string prmCurr, string prmCateg, string prmRpt, string prmWareHouse, string prmBin, string prmCount, string prmWeight, string prmFreight, string prmFreClass, string prmInventory, string prmCycle, string prmProduction, string prmPacking, string prmMat, string prmLab, string prmVar, string prmFix, string prmStd, string prmAvg, string prmLast, string prmlUom, string prmExempt, string prmStat, string prmstock, string prmcasepal, string prmtypecode)
  
    {
        string cError = "";
        dsOrdItemDataSet dsOrdItem = new dsOrdItemDataSet();
        dsOrdItem = null;
        AppServerConnect();
        aoObject.OrdItem(prmCust, prmUser, prmAction, prmOrderNum, prmItemNum,prmNewItem, prmPart, prmName, prmDscr, prmDscr2, prmDscr3, prmIsSet, prmIsCust, prmCustName, prmTax, prmPurchase, prmEstimate, prmStyle, prmdie, prmPlate, prmCad, prmSPC, prmUPC, prmSell, prmUom, prmCurr, prmCateg, prmRpt, prmWareHouse, prmBin, prmCount, prmWeight, prmFreight, prmFreClass, prmInventory, prmCycle, prmProduction, prmPacking, prmMat, prmLab, prmVar, prmFix, prmStd, prmAvg, prmLast, prmlUom, prmExempt, prmStat, prmstock, prmcasepal, prmtypecode, ref dsOrdItem, out cError);
      
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsOrdItem;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsSetPartFGitemDataSet SelectSetPart(string prmUser, string prmAction, string prmComp, Int32 prmQty, string prmNewFgItem, string prmName, Int32 prmOnHand, Int32 prmPoJob, Int32 prmToOrder, Int32 prmBackOrder, Int32 prmAvail, string prmFGItem, string prmReckey )
    {
        string cError = "";
        dsSetPartFGitemDataSet dsSetPartFGitem = new dsSetPartFGitemDataSet();
        dsSetPartFGitem = null;
        AppServerConnect();
        aoObject.SetPart(prmUser, prmAction, prmComp, prmQty, prmNewFgItem, prmName, prmOnHand, prmPoJob, prmToOrder, prmBackOrder, prmAvail, prmFGItem, prmReckey, ref dsSetPartFGitem, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsSetPartFGitem;
    }



    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool SetPartVal(string prmUser, string prmAction, string prmComp, Int32 prmQty, string prmNewFgItem)
    {
        string cError = "";
        AppServerConnect();
        aoObject.FGSetValidate(prmUser, prmAction, prmComp, prmQty, prmNewFgItem, out cError);
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
   


}

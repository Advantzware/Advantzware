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
/// Summary description for ItemDetail
/// </summary>
[System.ComponentModel.DataObject]
public class OrderRelease : AppServerConnect.AppServer
{
    public OrderRelease()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsOrderReleaseDataSet SelectOrderRelease(string prmUser, string prmItem)
    {

        dsOrderReleaseDataSet dsOrderRelease = new dsOrderReleaseDataSet();
        dsOrderRelease = null;
        AppServerConnect();
        aoObject.OrderRelease(prmUser, prmItem, ref dsOrderRelease);
        AppServerDisconnect();

        return dsOrderRelease;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsShipLookDataSet SelectShipToLook(string prmAction, string prmUser, string prmItemNum, string prmField, string prmCondition, string prmText)
    {

        dsShipLookDataSet dsShipLook = new dsShipLookDataSet();
        dsShipLook = null;
        AppServerConnect();
        aoObject.Shiplook(prmAction, prmUser, prmItemNum, prmField, prmCondition, prmText, ref dsShipLook);
        AppServerDisconnect();
        return dsShipLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCarrierLookDataSet SelectCarrierLookUp(string prmAction, string prmUser, string prmItemNum, string prmField, string prmCondition, string prmText)
    {

        dsCarrierLookDataSet dsCarrierLook = new dsCarrierLookDataSet();
        dsCarrierLook = null;
        AppServerConnect();
        aoObject.CarrierLook(prmAction, prmUser, prmItemNum, prmField, prmCondition, prmText, ref dsCarrierLook);
        AppServerDisconnect();
        return dsCarrierLook;
    }
 
}



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
/// Summary description for Order
/// </summary>
[System.ComponentModel.DataObject]
public class jobprod : AppServerConnect.AppServer
{
    public jobprod()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsjobhdrDataSet jobhdr(string prmUser, string prmOrderNum, string vLine)
    {

        dsjobhdrDataSet dsjobhdr = new dsjobhdrDataSet();
        dsjobhdr = null;
        AppServerConnect();
        aoObject.jobhdr(prmUser, vLine, prmOrderNum, ref dsjobhdr);
        AppServerDisconnect();

        return dsjobhdr;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsViewJobDataSet job(string prmUser, string prmOrderNum, string vLine)
    {

        dsViewJobDataSet dsViewJob = new dsViewJobDataSet();
        dsViewJob = null;
        AppServerConnect();
        aoObject.ViewJob(prmUser, vLine, prmOrderNum, ref dsViewJob);
        AppServerDisconnect();

        return dsViewJob;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsMatInfoDataSet MatInfo(string prmUser, string prmOrderNum, string vLine)
    {

        dsMatInfoDataSet dsMatInfo = new dsMatInfoDataSet();
        dsMatInfo = null;
        AppServerConnect();
        aoObject.MatInfo(prmUser, vLine, prmOrderNum, ref dsMatInfo);
        AppServerDisconnect();

        return dsMatInfo;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsMachHrDataSet MachHr(string prmUser, string prmOrderNum, string vLine)
    {

        dsMachHrDataSet dsMachHr = new dsMachHrDataSet();
        dsMachHr = null;
        AppServerConnect();
        aoObject.MachHr(prmUser, vLine, prmOrderNum, ref dsMachHr);
        AppServerDisconnect();

        return dsMachHr;
    }
    public dsMaterialDataSet Material(string prmUser, string prmOrderNum, string vLine)
    {

        dsMaterialDataSet dsMaterial = new dsMaterialDataSet();
        dsMaterial = null;
        AppServerConnect();
        aoObject.Material(prmUser, vLine, prmOrderNum, ref dsMaterial);
        AppServerDisconnect();

        return dsMaterial;
    }
    public dsMaterialIntDataSet MaterialInt(string prmUser, string prmOrderNum, string vLine, string prmAction, string vFormNo, string vBlankNo, string prmItem)
    {

        dsMaterialIntDataSet dsMaterialInt = new dsMaterialIntDataSet();
        dsMaterialInt = null;
        AppServerConnect();
        aoObject.MaterialInt(prmUser, vLine, prmOrderNum,prmAction,vFormNo,vBlankNo,prmItem, ref dsMaterialInt);
        AppServerDisconnect();

        return dsMaterialInt;
    }
    public dsMachHrIntDataSet MachHrInt(string prmUser, string prmOrderNum, string vLine, string prmAction, string vFormNo, string vBlankNo, string prmItem)
    {

        dsMachHrIntDataSet dsMachHrInt = new dsMachHrIntDataSet();
        dsMachHrInt = null;
        AppServerConnect();
        aoObject.MachHrInt(prmUser, vLine, prmOrderNum, prmAction, vFormNo, vBlankNo, prmItem, ref dsMachHrInt);
        AppServerDisconnect();

        return dsMachHrInt;
    }
    public dsMachHrHdrIntDataSet MachHrHdrInt(string prmUser, string prmOrderNum, string vLine, string prmAction, string vFormNo, string vBlankNo, string prmItem)
    {

        dsMachHrHdrIntDataSet MachHrHdrInt = new dsMachHrHdrIntDataSet();
        MachHrHdrInt = null;
        AppServerConnect();
        aoObject.MachHrHdrInt(prmUser, vLine, prmOrderNum, prmAction, vFormNo, vBlankNo, prmItem, ref MachHrHdrInt);
        AppServerDisconnect();

        return MachHrHdrInt;
    }
    public dsMachQtyDataSet MachQty(string prmUser, string prmOrderNum, string vLine)
    {

        dsMachQtyDataSet dsMachQty = new dsMachQtyDataSet();
        dsMachQty = null;
        AppServerConnect();
        aoObject.MachQty(prmUser, vLine, prmOrderNum, ref dsMachQty);
        AppServerDisconnect();

        return dsMachQty;
    }
    public dsMachWasteDataSet MachWaste(string prmUser, string prmOrderNum, string vLine)
    {

        dsMachWasteDataSet dsMachWaste = new dsMachWasteDataSet();
        dsMachWaste = null;
        AppServerConnect();
        aoObject.MachWaste(prmUser, vLine, prmOrderNum, ref dsMachWaste);
        AppServerDisconnect();

        return dsMachWaste;
    }
    public dsMachCostsDataSet MachCosts(string prmUser, string prmOrderNum, string vLine)
    {

        dsMachCostsDataSet dsMachCosts = new dsMachCostsDataSet();
        dsMachCosts = null;
        AppServerConnect();
        aoObject.MachCosts(prmUser, vLine, prmOrderNum, ref dsMachCosts);
        AppServerDisconnect();

        return dsMachCosts;
    }
    public dsMachDLDataSet MachDL(string prmUser, string prmOrderNum, string vLine)
    {

        dsMachDLDataSet dsMachDL = new dsMachDLDataSet();
        dsMachDL = null;
        AppServerConnect();
        aoObject.MachDL(prmUser, vLine, prmOrderNum, ref dsMachDL);
        AppServerDisconnect();

        return dsMachDL;
    }
    public dsMachVarOHDataSet MachVarOH(string prmUser, string prmOrderNum, string vLine)
    {

        dsMachVarOHDataSet dsMachVarOH = new dsMachVarOHDataSet();
        dsMachVarOH = null;
        AppServerConnect();
        aoObject.MachVarOH(prmUser, vLine, prmOrderNum, ref dsMachVarOH);
        AppServerDisconnect();

        return dsMachVarOH;
    }
    public dsMachFixOHDataSet MachFixOH(string prmUser, string prmOrderNum, string vLine)
    {

        dsMachFixOHDataSet dsMachFixOH = new dsMachFixOHDataSet();
        dsMachFixOH = null;
        AppServerConnect();
        aoObject.MachFixOH(prmUser, vLine, prmOrderNum, ref dsMachFixOH);
        AppServerDisconnect();

        return dsMachFixOH;
    }
 
}



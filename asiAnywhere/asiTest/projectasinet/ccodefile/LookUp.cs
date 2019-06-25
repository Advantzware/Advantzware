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
/// Summary description for browsinvoice
/// </summary>
[System.ComponentModel.DataObject]
public class LookUp : AppServerConnect.AppServer
{
    public LookUp()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsQFgItemLookDataSet QFgItemLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText, string prmCust, string prmType)
    {

        dsQFgItemLookDataSet dsQFgItemLook = new dsQFgItemLookDataSet();
        dsQFgItemLook = null;
        AppServerConnect();
        aoObject.QFgItemLook(prmAction, prmUser, prmField, prmCondition, prmText, prmCust,prmType, ref dsQFgItemLook);
        AppServerDisconnect();
        return dsQFgItemLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsQPartLookDataSet QPartLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsQPartLookDataSet dsQPartLook = new dsQPartLookDataSet();
        dsQPartLook = null;
        AppServerConnect();
        aoObject.QPartLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsQPartLook);
        AppServerDisconnect();
        return dsQPartLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsBoardLookDataSet BoardLook(string prmAction, string prmUser, string prmIndustry, string prmField, string prmCondition, string prmText)
    {

        dsBoardLookDataSet dsBoardLook = new dsBoardLookDataSet();
        dsBoardLook = null;
        AppServerConnect();
        aoObject.BoardLookup(prmAction, prmUser, prmIndustry, prmField, prmCondition, prmText, ref dsBoardLook);
        AppServerDisconnect();
        return dsBoardLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCategoryLookDataSet CategoryLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsCategoryLookDataSet dsCategoryLook = new dsCategoryLookDataSet();
        dsCategoryLook = null;
        AppServerConnect();
        aoObject.CategoryLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsCategoryLook);
        AppServerDisconnect();
        return dsCategoryLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsStyleLookDataSet StyleLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsStyleLookDataSet dsStyleLook = new dsStyleLookDataSet();
        dsStyleLook = null;
        AppServerConnect();
        aoObject.StyleLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsStyleLook);
        AppServerDisconnect();
        return dsStyleLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsMatLookDataSet MatLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText, string MatType)
    {

        dsMatLookDataSet dsMatLook = new dsMatLookDataSet();
        dsMatLook = null;
        AppServerConnect();
        aoObject.MatLook(prmAction, prmUser, prmField, prmCondition, prmText, MatType, ref dsMatLook);
        AppServerDisconnect();
        return dsMatLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsAdderLookDataSet AdderLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsAdderLookDataSet dsAdderLook = new dsAdderLookDataSet();
        dsAdderLook = null;
        AppServerConnect();
        aoObject.AdderLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsAdderLook);
        AppServerDisconnect();
        return dsAdderLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsShipIdLookDataSet ShipIdLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText, string prmShip)
    {

        dsShipIdLookDataSet dsShipIdLook = new dsShipIdLookDataSet();
        dsShipIdLook = null;
        AppServerConnect();
        aoObject.ShippingLook(prmAction, prmUser, prmField, prmCondition, prmText, prmShip, ref dsShipIdLook);
        AppServerDisconnect();
        return dsShipIdLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsPrintLookDataSet PrintLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsPrintLookDataSet dsPrintLook = new dsPrintLookDataSet();
        dsPrintLook = null;
        AppServerConnect();
        aoObject.PCodeLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsPrintLook);
        AppServerDisconnect();
        return dsPrintLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsShipCarLookDataSet ShipCarLook(string prmAction, string prmUser, string prmShip, string prmField, string prmCondition, string prmText)
    {

        dsShipCarLookDataSet dsShipCarLook = new dsShipCarLookDataSet();
        dsShipCarLook = null;
        AppServerConnect();
        aoObject.ShipCarrLook(prmAction, prmUser, prmShip, prmField, prmCondition, prmText, ref dsShipCarLook);
        AppServerDisconnect();
        return dsShipCarLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsLocationLookDataSet SelLocationLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsLocationLookDataSet dsLocationLook = new dsLocationLookDataSet();
        dsLocationLook = null;
        AppServerConnect();
        aoObject.LocationLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsLocationLook);
        AppServerDisconnect();
        return dsLocationLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCompLookDataSet SelCompLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsCompLookDataSet dsCompLook = new dsCompLookDataSet();
        dsCompLook = null;
        AppServerConnect();
        aoObject.CompanyLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsCompLook);
        AppServerDisconnect();
        return dsCompLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsStateLookDataSet SelStateCodeLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsStateLookDataSet dsStateLook = new dsStateLookDataSet();
        dsStateLook = null;
        AppServerConnect();
        aoObject.StateCodeLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsStateLook);
        AppServerDisconnect();
        return dsStateLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsZipCodeLookDataSet SelZipCode(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsZipCodeLookDataSet dsZipCodeLook = new dsZipCodeLookDataSet();
        dsZipCodeLook = null;
        AppServerConnect();
        aoObject.ZipCodeLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsZipCodeLook);
        AppServerDisconnect();
        return dsZipCodeLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsTerrotLookDataSet SelTerritory(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsTerrotLookDataSet dsTerrotLook = new dsTerrotLookDataSet();
        dsTerrotLook = null;
        AppServerConnect();
        aoObject.TerrLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsTerrotLook);
        AppServerDisconnect();
        return dsTerrotLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsUserSalesLookDataSet SelSalesRep(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsUserSalesLookDataSet dsUserSalesLook = new dsUserSalesLookDataSet();
        dsUserSalesLook = null;
        AppServerConnect();
        aoObject.SalesRepLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsUserSalesLook);
        AppServerDisconnect();
        return dsUserSalesLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsUserVendLookDataSet SelUserVendLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsUserVendLookDataSet dsUserVendLook = new dsUserVendLookDataSet();
        dsUserVendLook = null;
        AppServerConnect();
        aoObject.UserVendorLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsUserVendLook);
        AppServerDisconnect();
        return dsUserVendLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsTitleLookDataSet SelTitle(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsTitleLookDataSet dsTitleLook = new dsTitleLookDataSet();
        dsTitleLook = null;
        AppServerConnect();
        aoObject.TitleLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsTitleLook);
        AppServerDisconnect();
        return dsTitleLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsTypeLookUpDataSet SelCustype(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsTypeLookUpDataSet dsTypeLookUp = new dsTypeLookUpDataSet();
        dsTypeLookUp = null;
        AppServerConnect();
        aoObject.CustypeLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsTypeLookUp);
        AppServerDisconnect();
        return dsTypeLookUp;
    }
    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    //public dsSalesNameLookUpDataSet SelSalesName(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    //{

    //    dsSalesNameLookUpDataSet dsSalesNameLookUp = new dsSalesNameLookUpDataSet();
    //    dsSalesNameLookUp = null;
    //    AppServerConnect();
    //    aoObject.SalesNameLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsSalesNameLookUp);
    //    AppServerDisconnect();
    //    return dsSalesNameLookUp;
    //}
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsSalesNameLookDataSet SelectSman(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsSalesNameLookDataSet dsSalesNameLook = new dsSalesNameLookDataSet();
        dsSalesNameLook = null;
        AppServerConnect();
        aoObject.SalesNameLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsSalesNameLook);
        AppServerDisconnect();
        return dsSalesNameLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsPackingLookDataSet PackingLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string MatType)
    {

        dsPackingLookDataSet dsPackingLook = new dsPackingLookDataSet();
        dsPackingLook = null;
        AppServerConnect();
        aoObject.PackingCode(prmAction, prmUser, prmField, prmCondition, prmText, MatType, ref dsPackingLook);
        AppServerDisconnect();
        return dsPackingLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dstcodeLookDataSet SeTermsLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dstcodeLookDataSet dstcodeLook = new dstcodeLookDataSet();
        dstcodeLook = null;
        AppServerConnect();
        aoObject.TermsLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dstcodeLook);
        AppServerDisconnect();
        return dstcodeLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dstaxcodeLookDataSet SelectTaxLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dstaxcodeLookDataSet dstaxcodeLook = new dstaxcodeLookDataSet();
        dstaxcodeLook = null;
        AppServerConnect();
        aoObject.TaxCodeLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dstaxcodeLook);
        AppServerDisconnect();
        return dstaxcodeLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dstypeOrdLookDataSet SelectTypeLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dstypeOrdLookDataSet dstypeOrdLook = new dstypeOrdLookDataSet();
        dstypeOrdLook = null;
        AppServerConnect();
        aoObject.TypeOrdLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dstypeOrdLook);
        AppServerDisconnect();
        return dstypeOrdLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsDueLookDataSet SeDueLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsDueLookDataSet dsDueLook = new dsDueLookDataSet();
        dsDueLook = null;
        AppServerConnect();
        aoObject.Due(prmAction, prmUser, prmField, prmCondition, prmText, ref dsDueLook);
        AppServerDisconnect();
        return dsDueLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsFGNameLookDataSet SelectfgLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmCustomer, Int32 prmQuote, Int32 prmQty)
    {

        dsFGNameLookDataSet dsFGNameLook = new dsFGNameLookDataSet();
        dsFGNameLook = null;
        AppServerConnect();
        aoObject.fgnamelook(prmAction, prmUser, prmField, prmCondition, prmText, prmCustomer, prmQuote, prmQty, ref dsFGNameLook);
        AppServerDisconnect();
        return dsFGNameLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsBoardPoEntryLookDataSet SelectBoardEntryLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsBoardPoEntryLookDataSet dsBoardPoEntryLook = new dsBoardPoEntryLookDataSet();
        dsBoardPoEntryLook = null;
        AppServerConnect();
        aoObject.BoardPoEntryLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsBoardPoEntryLook);
        AppServerDisconnect();
        return dsBoardPoEntryLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCarrierLookupDataSet shipcarLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsCarrierLookupDataSet dsCarrierLookup = new dsCarrierLookupDataSet();
        dsCarrierLookup = null;
        AppServerConnect();
        aoObject.CarrierLookup(prmAction, prmUser, prmField, prmCondition, prmText, ref dsCarrierLookup);
        AppServerDisconnect();
        return dsCarrierLookup;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsuomDataSet SelectUomLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsuomDataSet dsuom = new dsuomDataSet();
        dsuom = null;
        AppServerConnect();
        aoObject.UomLookUp(prmAction, prmUser, prmField, prmCondition, prmText, ref dsuom);
        AppServerDisconnect();
        return dsuom;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsupcLookDataSet SelectUpcLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsupcLookDataSet dsupcLook = new dsupcLookDataSet();
        dsupcLook = null;
        AppServerConnect();
        aoObject.Upclook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsupcLook);
        AppServerDisconnect();
        return dsupcLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsDieLookDataSet SelectDieLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmPlateType, string prmText)
    {

        dsDieLookDataSet dsDieLook = new dsDieLookDataSet();
        dsDieLook = null;
        AppServerConnect();
        aoObject.Dielook(prmAction, prmUser, prmField, prmCondition, prmPlateType, prmText, ref dsDieLook);
        AppServerDisconnect();
        return dsDieLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsrfqpallet1LookDataSet SePalletLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsrfqpallet1LookDataSet dsrfqpallet1Look = new dsrfqpallet1LookDataSet();
        dsrfqpallet1Look = null;
        AppServerConnect();
        aoObject.PalletLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsrfqpallet1Look);
        AppServerDisconnect();
        return dsrfqpallet1Look;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCadItemLookDataSet SelectCadLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsCadItemLookDataSet dsCadItemLook = new dsCadItemLookDataSet();
        dsCadItemLook = null;
        AppServerConnect();
        aoObject.CadLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsCadItemLook);
        AppServerDisconnect();
        return dsCadItemLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsBoardPoRepLookDataSet SelectBoardRepLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsBoardPoRepLookDataSet dsBoardPoRepLook = new dsBoardPoRepLookDataSet();
        dsBoardPoRepLook = null;
        AppServerConnect();
        aoObject.BoardPoRepLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsBoardPoRepLook);
        AppServerDisconnect();
        return dsBoardPoRepLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsuserlookDataSet SeUserLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsuserlookDataSet dsuserlook = new dsuserlookDataSet();
        dsuserlook = null;
        AppServerConnect();
        aoObject.UserLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsuserlook);
        AppServerDisconnect();
        return dsuserlook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsUpdEBLookDataSet SelectEstLookup(string prmUser, string prmEst)
    {

        dsUpdEBLookDataSet dsUpdEBLook = new dsUpdEBLookDataSet();
        dsUpdEBLook = null;
        AppServerConnect();
        aoObject.UpdEBLook(prmUser, prmEst, ref dsUpdEBLook);
        AppServerDisconnect();
        return dsUpdEBLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsItemUpLookupDataSet SeEstUpLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsItemUpLookupDataSet dsItemUpLookup = new dsItemUpLookupDataSet();
        dsItemUpLookup = null;
        AppServerConnect();
        aoObject.ItemUpLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsItemUpLookup);
        AppServerDisconnect();
        return dsItemUpLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsestitemupdateDataSet ItemUpdateLookup(string prmUser, string prmAction, string prmCondition, string prmField, string prmText, Int32 prmOrder)
    {

        dsestitemupdateDataSet dsItemUpdateLookup = new dsestitemupdateDataSet();
        dsItemUpdateLookup = null;
        AppServerConnect();
        aoObject.EstItemLook(prmUser, prmAction, prmCondition, prmField, prmText, prmOrder, ref dsItemUpdateLookup);
        AppServerDisconnect();
        return dsItemUpdateLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsUpdateItemEstDataSet UpdateItemLookup(string prmUser, Int32 prmOrder, string prmEst)
    {

        dsUpdateItemEstDataSet dsUpdateItemLookup = new dsUpdateItemEstDataSet();
        dsUpdateItemLookup = null;
        AppServerConnect();
        aoObject.UpdateEstItemLook(prmUser, prmOrder, prmEst, ref dsUpdateItemLookup);
        AppServerDisconnect();
        return dsUpdateItemLookup;
    }

    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    //public dsEstItemFgLookDataSet SeEstItemFgLookup(string prmUser, Int32 prmOrder, string prmEst, string prmItem)
    //{

    //    dsEstItemFgLookDataSet dsEstItemFgLook = new dsEstItemFgLookDataSet();
    //    dsEstItemFgLook = null;
    //    AppServerConnect();
    //    aoObject.EstItemFgLook(prmUser, prmOrder, prmEst, prmItem, ref dsEstItemFgLook);
    //    AppServerDisconnect();
    //    return dsEstItemFgLook;

    //}
    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    //public dsClassLookDataSet SelectClassLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    //{

    //    dsClassLookDataSet dsClassLook = new dsClassLookDataSet();
    //    dsClassLook = null;
    //    AppServerConnect();
    //    aoObject.classlook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsClassLook);
    //    AppServerDisconnect();
    //    return dsClassLook;
    //}
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsProcatLookDataSet SelectProcatLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsProcatLookDataSet dsProcatLook = new dsProcatLookDataSet();
        dsProcatLook = null;
        AppServerConnect();
        aoObject.ProcatLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsProcatLook);
        AppServerDisconnect();
        return dsProcatLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsBinLookDataSet SelectBinLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsBinLookDataSet dsBinLook = new dsBinLookDataSet();
        dsBinLook = null;
        AppServerConnect();
        aoObject.BinLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsBinLook);
        AppServerDisconnect();
        return dsBinLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsFgAbjLookDataSet SelectFGadjLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsFgAbjLookDataSet dsFgAbjLook = new dsFgAbjLookDataSet();
        dsFgAbjLook = null;
        AppServerConnect();
        aoObject.FgAdjLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsFgAbjLook);
        AppServerDisconnect();
        return dsFgAbjLook;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsBinAdjLookDataSet SelectBinAdjLook(string prmAction, string prmItem, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsBinAdjLookDataSet dsBinAdjLook = new dsBinAdjLookDataSet();
        dsBinAdjLook = null;
        AppServerConnect();
        aoObject.BinAdjLook(prmAction, prmItem, prmUser, prmField, prmCondition, prmText, ref dsBinAdjLook);
        AppServerDisconnect();
        return dsBinAdjLook;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsShipIdValDataSet SeWhareHouse(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsShipIdValDataSet dsShipIdVal = new dsShipIdValDataSet();
        dsShipIdVal = null;
        AppServerConnect();
        aoObject.ShipIdVal(prmAction, prmUser, prmField, prmCondition, prmText, ref dsShipIdVal);
        AppServerDisconnect();
        return dsShipIdVal;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsQuantityLookDataSet QuantityLook(string prmUser, Int32 prmQuote)
    {

        dsQuantityLookDataSet dsQuantityLook = new dsQuantityLookDataSet();
        dsQuantityLook = null;
        AppServerConnect();
        aoObject.QuantityLook(prmUser, prmQuote, ref dsQuantityLook);
        AppServerDisconnect();
        return dsQuantityLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsItemPriceLookDataSet PriceLook(string prmAction, string prmUser, string prmItem, string prmUom, string prmCust)
    {

        dsItemPriceLookDataSet dsItemPriceLook = new dsItemPriceLookDataSet();
        dsItemPriceLook = null;
        AppServerConnect();
        aoObject.ItemPriceLook(prmAction, prmUser, prmItem, prmUom, prmCust, ref dsItemPriceLook);
        AppServerDisconnect();
        return dsItemPriceLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsInveClassLookDataSet SelectInventoryClass(string prmAction, string prmUser)
    {

        dsInveClassLookDataSet dsInveClassLook = new dsInveClassLookDataSet();
        dsInveClassLook = null;
        AppServerConnect();
        aoObject.InventoryClass(prmAction, prmUser, ref dsInveClassLook);
        AppServerDisconnect();
        return dsInveClassLook;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dscustShipIdLookDataSet SelectShipLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmShip)
    {
        dscustShipIdLookDataSet dscustShipIdLook = new dscustShipIdLookDataSet();
        dscustShipIdLook = null;
        AppServerConnect();
        aoObject.custshiptolook(prmAction, prmUser, prmField, prmCondition, prmText, prmShip, ref dscustShipIdLook);
        AppServerDisconnect();
        return dscustShipIdLook;
    }  

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsQuoteUpLookupDataSet SeQuoteLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText,string prmCust)
    {

        dsQuoteUpLookupDataSet dsQuoteUpLookup = new dsQuoteUpLookupDataSet();
        dsQuoteUpLookup = null;
        AppServerConnect();
        aoObject.QuoteUpLook(prmAction, prmUser, prmField, prmCondition, prmText, prmCust, ref dsQuoteUpLookup);
        AppServerDisconnect();
        return dsQuoteUpLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsEstimateUpLookupDataSet SeEstimateLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmCust)
    {
        dsEstimateUpLookupDataSet dsEstimateUpLookup = new dsEstimateUpLookupDataSet();
        dsEstimateUpLookup = null;
        AppServerConnect();
        aoObject.EstimateUpLook(prmAction, prmUser, prmField, prmCondition, prmText, prmCust, ref dsEstimateUpLookup);
        AppServerDisconnect();
        return dsEstimateUpLookup;
    }    

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsFGitemLookupDataSet SedsFGitemLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsFGitemLookupDataSet dsFGitemLookup = new dsFGitemLookupDataSet();
        dsFGitemLookup = null;
        AppServerConnect();
        aoObject.fgitemlook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsFGitemLookup);
        AppServerDisconnect();
        return dsFGitemLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsItemPartLookDataSet ItemPartLookup(string prmCust, string prmAction, string prmUser, string prmCustPart)
    {

        dsItemPartLookDataSet dsItemPartLook = new dsItemPartLookDataSet();
        dsItemPartLook = null;
        AppServerConnect();
        aoObject.ItemPartLook(prmCust,  prmAction,  prmUser, prmCustPart, ref dsItemPartLook);
        AppServerDisconnect();
        return dsItemPartLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCurrencyCodeLookDataSet CurrencyCodeLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsCurrencyCodeLookDataSet dsCurrencyCodeLook = new dsCurrencyCodeLookDataSet();
        dsCurrencyCodeLook = null;
        AppServerConnect();
        aoObject.CurrencyLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsCurrencyCodeLook);
        AppServerDisconnect();
        return dsCurrencyCodeLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCustPalletLookDataSet SePalletLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmIndustry)
    {

        dsCustPalletLookDataSet dsCustPalletLook = new dsCustPalletLookDataSet();
        dsCustPalletLook = null;
        AppServerConnect();
        aoObject.custpalletlook(prmAction, prmUser, prmField, prmCondition, prmText, prmIndustry, ref dsCustPalletLook);
        AppServerDisconnect();
        return dsCustPalletLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCustBinLookDataSet CustBinLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmLoc)
    {

        dsCustBinLookDataSet dsCustBinLook = new dsCustBinLookDataSet();
        dsCustBinLook = null;
        AppServerConnect();
        aoObject.CustBinLook(prmAction, prmUser, prmField, prmCondition, prmText, prmLoc, ref dsCustBinLook);
        AppServerDisconnect();
        return dsCustBinLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCustCarrierLookDataSet CustCarrLookup(string prmAction, string prmUser, string prmLoc, string prmField, string prmCondition, string prmText)
    {

        dsCustCarrierLookDataSet dsCustCarrierLook = new dsCustCarrierLookDataSet();
        dsCustCarrierLook = null;
        AppServerConnect();
        aoObject.CustCarrierLook(prmAction, prmUser,prmLoc, prmField, prmCondition, prmText, ref dsCustCarrierLook);
        AppServerDisconnect();
        return dsCustCarrierLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsItemQuoteLookDataSet ItemQuoteLookup(string prmAction, string prmUser,  string prmField, string prmCondition, string prmText,Int32 prmQuote, string prmCust)
    {

        dsItemQuoteLookDataSet dsItemQuoteLook = new dsItemQuoteLookDataSet();
        dsItemQuoteLook = null;
        AppServerConnect();
        aoObject.itemquotelook(prmAction, prmUser, prmField, prmCondition, prmText, prmQuote,prmCust, ref dsItemQuoteLook);
        AppServerDisconnect();
        return dsItemQuoteLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCustSoldLookDataSet SoldToLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmSold)
    {

        dsCustSoldLookDataSet dsCustSoldLook = new dsCustSoldLookDataSet();
        dsCustSoldLook = null;
        AppServerConnect();
        aoObject.CustSoldLook(prmAction, prmUser, prmField, prmCondition, prmText, prmSold, ref dsCustSoldLook);
        AppServerDisconnect();
        return dsCustSoldLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsQuoteHandLookDataSet QuoteHandLook(string prmAction, string prmUser, Int32 prmQuote, string prmCust,string prmItem)
    {

        dsQuoteHandLookDataSet dsQuoteHandLook = new dsQuoteHandLookDataSet();
        dsQuoteHandLook = null;
        AppServerConnect();
        aoObject.QuoteHand(prmAction, prmUser, prmQuote, prmCust,prmItem, ref dsQuoteHandLook);
        AppServerDisconnect();
        return dsQuoteHandLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsEbStockLookDataSet SelectEbStockLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, Int32 prmType, string prmEstimate)
    {

        dsEbStockLookDataSet dsEbStockLook = new dsEbStockLookDataSet();
        dsEbStockLook = null;
        AppServerConnect();
        aoObject.EbStockLook(prmAction, prmUser, prmField, prmCondition, prmText, prmType, prmEstimate, ref dsEbStockLook);
        AppServerDisconnect();
        return dsEbStockLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsItemQuantityLookDataSet ItemQuantityLook(string prmUser, Int32 prmQuote, string prmItem, string prmCustPart)
    {

        dsItemQuantityLookDataSet dsItemQuantityLook = new dsItemQuantityLookDataSet();
        dsItemQuantityLook = null;
        AppServerConnect();
        aoObject.ItemQuantityLook(prmUser, prmQuote,prmItem,  prmCustPart, ref dsItemQuantityLook);
        AppServerDisconnect();
        return dsItemQuantityLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsTopFormNoLookDataSet TopFormNoLook(string prmUser, string prmComp, string prmEstNo)
    {

        dsTopFormNoLookDataSet dsTopFormNoLook = new dsTopFormNoLookDataSet();
        dsTopFormNoLook = null;
        AppServerConnect();
        aoObject.TopFormNoLook(prmUser, prmComp, prmEstNo, ref dsTopFormNoLook);
        AppServerDisconnect();
        return dsTopFormNoLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsTopDeptLookDataSet TopDeptLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText, string prmCode, string prmDscr)
    {

        dsTopDeptLookDataSet dsTopDeptLook = new dsTopDeptLookDataSet();
        dsTopDeptLook = null;
        AppServerConnect();
        aoObject.TopDeptLook(prmUser, prmAction, prmField, prmCondition, prmText, prmCode, prmDscr, ref dsTopDeptLook);
        AppServerDisconnect();
        return dsTopDeptLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsTopSpecLookDataSet SpecLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsTopSpecLookDataSet dsTopSpecLook = new dsTopSpecLookDataSet();
        dsTopSpecLook = null;
        AppServerConnect();
        aoObject.TopSpecLook(prmUser, prmAction, prmField, prmCondition, prmText, ref dsTopSpecLook);
        AppServerDisconnect();
        return dsTopSpecLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsTopGroupLookDataSet GroupLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsTopGroupLookDataSet dsTopGroupLook = new dsTopGroupLookDataSet();
        dsTopGroupLook = null;
        AppServerConnect();
        aoObject.GroupLookup(prmUser, prmAction, prmField, prmCondition, prmText, ref dsTopGroupLook);
        AppServerDisconnect();
        return dsTopGroupLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsUsagPoLookDataSet UsagPoLook(string prmAction, string prmUser, string prmComp, string prmField, string prmCondition, string prmText)
    {

        dsUsagPoLookDataSet dsUsagPoLook = new dsUsagPoLookDataSet();
        dsUsagPoLook = null;
        AppServerConnect();
        aoObject.UsagPoLook(prmAction, prmUser, prmComp, prmField, prmCondition, prmText, ref dsUsagPoLook);
        AppServerDisconnect();
        return dsUsagPoLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsUsagOrderLookDataSet UsagOrderLook(string prmAction, string prmUser, string prmComp, string prmField, string prmCondition, string prmText, string prmItem, Int32 prmOrder)
    {

        dsUsagOrderLookDataSet dsUsagOrderLook = new dsUsagOrderLookDataSet();
        dsUsagOrderLook = null;
        AppServerConnect();
        aoObject.UsagOrderLook(prmAction, prmUser, prmComp, prmField, prmCondition, prmText, prmItem, prmOrder, ref dsUsagOrderLook);
        AppServerDisconnect();
        return dsUsagOrderLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsUsagPlantCodeLookDataSet UsagPlantLook(string prmAction, string prmUser, string prmComp, string prmField, string prmCondition, string prmText, string prmFGItem, string prmVanderCode)
    {

        dsUsagPlantCodeLookDataSet dsUsagPlantCodeLook = new dsUsagPlantCodeLookDataSet();
        dsUsagPlantCodeLook = null;
        AppServerConnect();
        aoObject.UsagPlantLook(prmAction, prmUser, prmComp, prmField, prmCondition, prmText, prmFGItem, prmVanderCode, ref dsUsagPlantCodeLook);
        AppServerDisconnect();
        return dsUsagPlantCodeLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsUsagCustPartLookDataSet UsagCustPartLook(string prmAction, string prmUser, string prmComp, string prmField, string prmCondition, string prmText, string prmCustPart)
    {

        dsUsagCustPartLookDataSet dsUsagCustPartLook = new dsUsagCustPartLookDataSet();
        dsUsagCustPartLook = null;
        AppServerConnect();
        aoObject.UsagCustPartLook(prmAction, prmUser, prmComp, prmField, prmCondition, prmText, prmCustPart, ref dsUsagCustPartLook);
        AppServerDisconnect();
        return dsUsagCustPartLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCrossRefLookupDataSet Cross_Ref_Lookup(string prmAction, string prmUser,  string prmField, string prmCondition, string prmText)
    {

        dsCrossRefLookupDataSet dsCrossRefLookup = new dsCrossRefLookupDataSet();
        dsCrossRefLookup = null;
        AppServerConnect();
        aoObject.CrossRef_Look(prmAction, prmUser,  prmField, prmCondition, prmText,  ref dsCrossRefLookup);
        AppServerDisconnect();
        return dsCrossRefLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsFreightLookDataSet FreightclassLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        dsFreightLookDataSet dsFrieghtLookup = new dsFreightLookDataSet();
        dsFrieghtLookup = null;
        AppServerConnect();
        aoObject.FreightLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsFrieghtLookup);
        AppServerDisconnect();
        return dsFrieghtLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsEbSerchLookDataSet SelectEstSearch( string prmCust, string prmStat, string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        dsEbSerchLookDataSet dsEbSerchLook = new dsEbSerchLookDataSet();
        dsEbSerchLook = null;
        AppServerConnect();
        aoObject.Eb_Serch_Look(prmCust, prmStat, prmAction, prmUser, prmField, prmCondition, prmText, ref dsEbSerchLook);
        AppServerDisconnect();
        return dsEbSerchLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsQuoteSerchLookDataSet SelectQuoteSearch(string prmCust, string prmStat, string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        dsQuoteSerchLookDataSet dsQuoteSerchLook = new dsQuoteSerchLookDataSet();
        dsQuoteSerchLook = null;
        AppServerConnect();
        aoObject.QuoteSerchLook(prmCust, prmStat, prmAction, prmUser, prmField, prmCondition, prmText, ref dsQuoteSerchLook);
        AppServerDisconnect();
        return dsQuoteSerchLook;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsVendorBolLookupDataSet VendorBolLook(string prmUser, string prmAction, string prmField, string prmCondition, string prmText)
    {

        dsVendorBolLookupDataSet dsVendorBolLookup = new dsVendorBolLookupDataSet();
        dsVendorBolLookup = null;
        AppServerConnect();
        aoObject.VendorBolLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsVendorBolLookup);
        AppServerDisconnect();
        return dsVendorBolLookup;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsMainItem3LookDataSet SelectItem3Lookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmComp)
    {
        dsMainItem3LookDataSet dsMainItem3Look = new dsMainItem3LookDataSet();
        dsMainItem3Look = null;
        AppServerConnect();
        aoObject.Item3Look(prmAction, prmUser, prmField, prmCondition, prmText, prmComp, ref dsMainItem3Look);
        AppServerDisconnect();
        return dsMainItem3Look;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsReleaseLookDataSet SelectReleaseLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmComp, Int32 prmRelease)
    {
        string cError = "";
        dsReleaseLookDataSet dsReleaseLook = new dsReleaseLookDataSet();
        dsReleaseLook = null;
        AppServerConnect();
        aoObject.ReleaseLook(prmAction, prmUser, prmField, prmCondition, prmText, prmComp, prmRelease, ref dsReleaseLook, out cError);
        AppServerDisconnect();
        /*if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }*/
        return dsReleaseLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsTrailerLookDataSet SelectTrailerLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmComp, string prmTrailer, Int32 prmRelease)
    {
        string cError = "";
        dsTrailerLookDataSet dsTrailerLook = new dsTrailerLookDataSet();
        dsTrailerLook = null;
        AppServerConnect();
        aoObject.TrailerLook(prmAction, prmUser, prmField, prmCondition, prmText, prmComp, prmTrailer, prmRelease, ref dsTrailerLook, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsTrailerLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsTagLookDataSet SelectTagLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmComp, Int32 prmRelease, string prmTag)
    {
        string cError = "";
        dsTagLookDataSet dsTagLook = new dsTagLookDataSet();
        dsTagLook = null;
        AppServerConnect();
        aoObject.TagLook(prmAction, prmUser, prmField, prmCondition, prmText, prmComp, prmRelease, prmTag, out cError, ref dsTagLook);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsTagLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCustPlantCodeLookDataSet CustPlantLook(string prmAction, string prmUser, string prmComp, string prmField, string prmCondition, string prmText, string prmCustNo)
    {

        dsCustPlantCodeLookDataSet dsCustPlantCodeLook = new dsCustPlantCodeLookDataSet();
        dsCustPlantCodeLook = null;
        AppServerConnect();
        aoObject.CustPlantLook(prmAction, prmUser, prmComp, prmField, prmCondition, prmText, prmCustNo, ref dsCustPlantCodeLook);
        AppServerDisconnect();
        return dsCustPlantCodeLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsCustShipLookDataSet selectCustShipLook(string prmAction, string prmUser, string prmItemNum, string prmField, string prmCondition, string prmText)
    {

        dsCustShipLookDataSet dsCustShipLook = new dsCustShipLookDataSet();
        dsCustShipLook = null;
        AppServerConnect();
        aoObject.custshiplook(prmAction, prmUser, prmItemNum, prmField, prmCondition, prmText, ref dsCustShipLook);
        AppServerDisconnect();
        return dsCustShipLook;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsLcPartQuoteLookDataSet LcPartLookup(string prmAction, string prmUser,  string prmField, string prmCondition, string prmText, Int32 prmQuote)
    {

        dsLcPartQuoteLookDataSet dsLcPartQuoteLook = new dsLcPartQuoteLookDataSet();
        dsLcPartQuoteLook = null;
        AppServerConnect();
        aoObject.lcpartlook(prmAction, prmUser, prmField, prmCondition, prmText,prmQuote, ref dsLcPartQuoteLook);
        AppServerDisconnect();
        return dsLcPartQuoteLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsAccountNoLookDataSet AccountLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmjournal)
    {

        dsAccountNoLookDataSet dsAccountNoLook = new dsAccountNoLookDataSet();
        dsAccountNoLook = null;
        AppServerConnect();
        aoObject.AccountLook(prmAction, prmUser, prmField, prmCondition, prmText, prmjournal, ref dsAccountNoLook);
        AppServerDisconnect();
        return dsAccountNoLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsDepartmentLookDataSet SelectDept(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        dsDepartmentLookDataSet dsDepartmentLook = new dsDepartmentLookDataSet();
        dsDepartmentLook = null;
        AppServerConnect();
        aoObject.DeptLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsDepartmentLook);
        AppServerDisconnect();
        return dsDepartmentLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsEstimateLookupDataSet SelectEstimateLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        dsEstimateLookupDataSet dsEstimateLookup = new dsEstimateLookupDataSet();
        dsEstimateLookup = null;
        AppServerConnect();
        aoObject.EstimateLookup(prmAction, prmUser, prmField, prmCondition, prmText, ref dsEstimateLookup);
        AppServerDisconnect();
        return dsEstimateLookup;
    } 
}
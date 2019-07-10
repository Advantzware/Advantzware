using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
//using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class view_pord : System.Web.UI.Page
{
    public view_pord()
    {
        //
        // TODO: Add constructor logic here
        //
    }
        

    protected void Page_Load(object sender, EventArgs e)
    {
                
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Session["Rowuser"] = UserLogin.UserName;
        Session["prmUser"] = UserLogin.UserName;       
      
        if (!Page.IsPostBack)
        {            
            
        }

        if (Convert.ToString(Session["add_po_list_buton"]) == "add")
        {
            FormView1.ChangeMode(FormViewMode.Insert);
            Session["add_po_list_buton"] = null;
        }
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";

        //ImageButton brwsorder = (ImageButton)Master.FindControl("view_po");
        //brwsorder.ImageUrl = "Images/viewPo1_1.jpg";
        Image movebutton = (Image)Master.FindControl("Image5");
        movebutton.Visible = false;
              
          
        if (Session["User"] != null)
        {

            string vUserId = UserLogin.UserName;
            string vPage = "Brwslist_po.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }

            /*if (aUsers == "external")
            {
                customerid.Visible = false;

            }
            */

        }

    }

    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Label po = (Label)FormView1.FindControl("vPoNolabel");

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Deletepo";
        ObjectDataSource1.SelectParameters["prmPoNo"].DefaultValue = po.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = po.Text.Trim();
        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Label po = (Label)FormView1.FindControl("vPoNoTextBox");
        TextBox podate = (TextBox)FormView1.FindControl("vPoDateTextBox");
        TextBox type = (TextBox)FormView1.FindControl("vTypeTextBox");
        Label stat = (Label)FormView1.FindControl("vStatTextBox");
        TextBox vend = (TextBox)FormView1.FindControl("vVendNoTextBox");
        TextBox vendna = (TextBox)FormView1.FindControl("vVendNameTextBox");
        TextBox shipid = (TextBox)FormView1.FindControl("vShipIdTextBox");
        TextBox shipname = (TextBox)FormView1.FindControl("vShipNameTextBox");
        TextBox shipadd1 = (TextBox)FormView1.FindControl("vShipAddr1TextBox");
        TextBox shipadd2 = (TextBox)FormView1.FindControl("vShipAddr2TextBox");
        TextBox shipcity = (TextBox)FormView1.FindControl("vShipCityTextBox");
        TextBox shipstat = (TextBox)FormView1.FindControl("vShipStateTextBox");
        TextBox shipzip = (TextBox)FormView1.FindControl("vShipZipTextBox");
        TextBox shiparecode = (TextBox)FormView1.FindControl("vShipAreaCodeTextBox");
        TextBox shiphone = (TextBox)FormView1.FindControl("vShipPhoneTextBox");
        TextBox buyer = (TextBox)FormView1.FindControl("vBuyerTextBox");
        TextBox contant = (TextBox)FormView1.FindControl("vContactTextBox");
        TextBox duedate = (TextBox)FormView1.FindControl("vDueDateTextBox");
        TextBox lastshipdate = (TextBox)FormView1.FindControl("vLastShipDateTextBox");
        TextBox underpct = (TextBox)FormView1.FindControl("vUnderPctTextBox");
        TextBox overpct = (TextBox)FormView1.FindControl("vOverPctTextBox");
        TextBox carrier = (TextBox)FormView1.FindControl("vCarrierTextBox");
        TextBox taxgr = (TextBox)FormView1.FindControl("vTaxGrTextBox");
        TextBox terms = (TextBox)FormView1.FindControl("vTermsTextBox");
        RadioButtonList frtpay = (RadioButtonList)FormView1.FindControl("FreightPaymentRadioButtonList");
        RadioButtonList fobcode = (RadioButtonList)FormView1.FindControl("FOBRadioButtonList1");
        TextBox tfreight = (TextBox)FormView1.FindControl("vTFreightTextBox");
        TextBox tax = (TextBox)FormView1.FindControl("vTaxTextBox");
        TextBox cost = (TextBox)FormView1.FindControl("vTCostTextBox");
        TextBox reckey = (TextBox)FormView1.FindControl("vRecKeyTextBox");

       browspo poord = new browspo();

        bool check = poord.ValidateListPo(UserLogin.UserName, "validateadd", Convert.ToInt32(po.Text.Trim()),podate.Text.Trim(),type.Text.Trim(),stat.Text.Trim(),vend.Text.Trim(),vendna.Text.Trim(),"","","","","","","",shipid.Text.Trim(),shipname.Text.Trim(),"","","","","","",buyer.Text.Trim(),"",duedate.Text.Trim(),lastshipdate.Text.Trim(),0,0,carrier.Text.Trim(),taxgr.Text.Trim(),terms.Text.Trim(),"","",0,0,0,"");

        if (check)
        {

            Session["pur_ord_po"] = po.Text.Trim();
            Session["order_rec_key"] = reckey.Text.Trim();
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
            ObjectDataSource1.SelectParameters["prmPoNo"].DefaultValue = po.Text.Trim();
            ObjectDataSource1.SelectParameters["prmPoDate"].DefaultValue = podate.Text.Trim();
            ObjectDataSource1.SelectParameters["prmType"].DefaultValue = type.Text.Trim();
            ObjectDataSource1.SelectParameters["prmStat"].DefaultValue = stat.Text.Trim();
            ObjectDataSource1.SelectParameters["prmVendNo"].DefaultValue = vend.Text.Trim();
            ObjectDataSource1.SelectParameters["prmVendName"].DefaultValue = vendna.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShipId"].DefaultValue = shipid.Text.Trim();
            ObjectDataSource1.SelectParameters["prmShipName"].DefaultValue = shipname.Text.Trim();
            ObjectDataSource1.SelectParameters["prmBuyer"].DefaultValue = buyer.Text.Trim();
            ObjectDataSource1.SelectParameters["prmContact"].DefaultValue = contant.Text.Trim();
            ObjectDataSource1.SelectParameters["prmDueDate"].DefaultValue = duedate.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLastShipDate"].DefaultValue = lastshipdate.Text.Trim();
            ObjectDataSource1.SelectParameters["prmUnderPct"].DefaultValue = underpct.Text.Trim();
            ObjectDataSource1.SelectParameters["prmOverPct"].DefaultValue = overpct.Text.Trim();
            ObjectDataSource1.SelectParameters["prmCarrier"].DefaultValue = carrier.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTaxGr"].DefaultValue = taxgr.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTerms"].DefaultValue = terms.Text.Trim();
            ObjectDataSource1.SelectParameters["prmFrtPay"].DefaultValue = frtpay.SelectedValue;
            ObjectDataSource1.SelectParameters["prmFobCode"].DefaultValue = fobcode.SelectedValue;
            ObjectDataSource1.SelectParameters["prmTFreight"].DefaultValue = tfreight.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTax"].DefaultValue = tax.Text.Trim();
            ObjectDataSource1.SelectParameters["prmTCost"].DefaultValue = cost.Text.Trim();
            //ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = po.Text.Trim();

            FormView1.ChangeMode(FormViewMode.ReadOnly);
            Session["view_pord_line_add_insert"] = "Add";
            Response.Write("<script>window.location.href='viewitem_po.aspx'</script>");
        }
        
    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Label po = (Label)FormView1.FindControl("vPoNoTextBox");
        TextBox podate = (TextBox)FormView1.FindControl("vPoDateTextBox");
        TextBox type = (TextBox)FormView1.FindControl("vTypeTextBox");
        Label stat = (Label)FormView1.FindControl("vStatTextBox");
        TextBox vend = (TextBox)FormView1.FindControl("vVendNoTextBox");
        TextBox vendna = (TextBox)FormView1.FindControl("vVendNameTextBox");
        TextBox shipid = (TextBox)FormView1.FindControl("vShipIdTextBox");
        TextBox shipname = (TextBox)FormView1.FindControl("vShipNameTextBox");
        TextBox shipadd1 = (TextBox)FormView1.FindControl("vShipAddr1TextBox");
        TextBox shipadd2 = (TextBox)FormView1.FindControl("vShipAddr2TextBox");
        TextBox shipcity = (TextBox)FormView1.FindControl("vShipCityTextBox");
        TextBox shipstat = (TextBox)FormView1.FindControl("vShipStateTextBox");
        TextBox shipzip = (TextBox)FormView1.FindControl("vShipZipTextBox");
        TextBox shiparecode = (TextBox)FormView1.FindControl("vShipAreaCodeTextBox");
        TextBox shiphone = (TextBox)FormView1.FindControl("vShipPhoneTextBox");
        TextBox buyer = (TextBox)FormView1.FindControl("vBuyerTextBox");
        TextBox contant = (TextBox)FormView1.FindControl("vContactTextBox");
        TextBox duedate = (TextBox)FormView1.FindControl("vDueDateTextBox");
        TextBox lastshipdate = (TextBox)FormView1.FindControl("vLastShipDateTextBox");
        TextBox underpct = (TextBox)FormView1.FindControl("vUnderPctTextBox");
        TextBox overpct = (TextBox)FormView1.FindControl("vOverPctTextBox");
        TextBox carrier = (TextBox)FormView1.FindControl("vCarrierTextBox");
        TextBox taxgr = (TextBox)FormView1.FindControl("vTaxGrTextBox");
        TextBox terms = (TextBox)FormView1.FindControl("vTermsTextBox");
        RadioButtonList frtpay = (RadioButtonList)FormView1.FindControl("FreightPaymentRadioButtonList");
        RadioButtonList fobcode = (RadioButtonList)FormView1.FindControl("FOBRadioButtonList1");
        TextBox tfreight = (TextBox)FormView1.FindControl("vTFreightTextBox");
        TextBox tax = (TextBox)FormView1.FindControl("vTaxTextBox");
        TextBox cost = (TextBox)FormView1.FindControl("vTCostTextBox");

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmPoNo"].DefaultValue = po.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPoDate"].DefaultValue = podate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmType"].DefaultValue = type.Text.Trim();
        ObjectDataSource1.SelectParameters["prmStat"].DefaultValue = stat.Text.Trim();
        ObjectDataSource1.SelectParameters["prmVendNo"].DefaultValue = vend.Text.Trim();
        ObjectDataSource1.SelectParameters["prmVendName"].DefaultValue = vendna.Text.Trim();
        ObjectDataSource1.SelectParameters["prmShipId"].DefaultValue = shipid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmShipName"].DefaultValue = shipname.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBuyer"].DefaultValue = buyer.Text.Trim();
        ObjectDataSource1.SelectParameters["prmContact"].DefaultValue = contant.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDueDate"].DefaultValue = duedate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLastShipDate"].DefaultValue = lastshipdate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmUnderPct"].DefaultValue = underpct.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOverPct"].DefaultValue = overpct.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCarrier"].DefaultValue = carrier.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTaxGr"].DefaultValue = taxgr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTerms"].DefaultValue = terms.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFrtPay"].DefaultValue = frtpay.SelectedValue;
        ObjectDataSource1.SelectParameters["prmFobCode"].DefaultValue = fobcode.SelectedValue;
        ObjectDataSource1.SelectParameters["prmTFreight"].DefaultValue = tfreight.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTax"].DefaultValue = tax.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTCost"].DefaultValue = cost.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = po.Text.Trim();
        FormView1.ChangeMode(FormViewMode.ReadOnly);
                
    }


    protected void FormView1_Unload(object sender, EventArgs e)
    {
        try
        {
            Label po = (Label)FormView1.FindControl("vPoNoLabel");
            Session["pur_ord_po"] = po.Text;

        }
        catch { }
    }

 protected void FormView1_DataBound(object sender, EventArgs e)
    {
        
            if (FormView1.CurrentMode == FormViewMode.Edit)
            {
                try
                {

                    UserClass.CheckLogin(Page);
                    UserClass UserLogin = (UserClass)Session["User"];
                    TextBox podate = (TextBox)FormView1.FindControl("vPoDateTextBox");
                    podate.Focus();
                }
                catch { }
            }
            if (FormView1.CurrentMode == FormViewMode.Insert)
            {
                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];

                Label po = (Label)FormView1.FindControl("vPoNoTextBox");
                TextBox podate = (TextBox)FormView1.FindControl("vPoDateTextBox");
                TextBox type = (TextBox)FormView1.FindControl("vTypeTextBox");
                Label stat = (Label)FormView1.FindControl("vStatTextBox");
                TextBox vend = (TextBox)FormView1.FindControl("vVendNoTextBox");
                TextBox vendna = (TextBox)FormView1.FindControl("vVendNameTextBox");
                TextBox shipid = (TextBox)FormView1.FindControl("vShipIdTextBox");
                TextBox shipname = (TextBox)FormView1.FindControl("vShipNameTextBox");
                TextBox shipadd1 = (TextBox)FormView1.FindControl("vShipAddr1TextBox");
                TextBox shipadd2 = (TextBox)FormView1.FindControl("vShipAddr2TextBox");
                TextBox shipcity = (TextBox)FormView1.FindControl("vShipCityTextBox");
                TextBox shipstat = (TextBox)FormView1.FindControl("vShipStateTextBox");
                TextBox shipzip = (TextBox)FormView1.FindControl("vShipZipTextBox");
                TextBox shiparecode = (TextBox)FormView1.FindControl("vShipAreaCodeTextBox");
                TextBox shiphone = (TextBox)FormView1.FindControl("vShipPhoneTextBox");
                TextBox buyer = (TextBox)FormView1.FindControl("vBuyerTextBox");
                TextBox contant = (TextBox)FormView1.FindControl("vContactTextBox");
                TextBox duedate = (TextBox)FormView1.FindControl("vDueDateTextBox");
                TextBox lastshipdate = (TextBox)FormView1.FindControl("vLastShipDateTextBox");
                TextBox underpct = (TextBox)FormView1.FindControl("vUnderPctTextBox");
                TextBox overpct = (TextBox)FormView1.FindControl("vOverPctTextBox");
                TextBox carrier = (TextBox)FormView1.FindControl("vCarrierTextBox");
                TextBox taxgr = (TextBox)FormView1.FindControl("vTaxGrTextBox");
                TextBox terms = (TextBox)FormView1.FindControl("vTermsTextBox");
                RadioButtonList frtpay = (RadioButtonList)FormView1.FindControl("FreightPaymentRadioButtonList");
                RadioButtonList fobcode = (RadioButtonList)FormView1.FindControl("FOBRadioButtonList1");
                TextBox tfreight = (TextBox)FormView1.FindControl("vTFreightTextBox");
                TextBox tax = (TextBox)FormView1.FindControl("vTaxTextBox");
                TextBox cost = (TextBox)FormView1.FindControl("vTCostTextBox");
                TextBox reckey = (TextBox)FormView1.FindControl("vRecKeyTextBox");


                browspo pobrws = new browspo();
                DataSet dspo = new DataSet();

                dspo = pobrws.SelectViewPo(UserLogin.UserName, "Addnewpo", 0, "","","", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "","","","","","","",0,0,"","","","","",0,0,0,"");

                po.Text = dspo.Tables[0].Rows[0][0].ToString();
                podate.Text = dspo.Tables[0].Rows[0][1].ToString();
                type.Text = dspo.Tables[0].Rows[0][2].ToString();
                stat.Text = dspo.Tables[0].Rows[0][3].ToString();
                buyer.Text = dspo.Tables[0].Rows[0][21].ToString();
                duedate.Text = dspo.Tables[0].Rows[0][24].ToString();
                lastshipdate.Text = dspo.Tables[0].Rows[0][25].ToString();
                underpct.Text = dspo.Tables[0].Rows[0][26].ToString();
                overpct.Text = dspo.Tables[0].Rows[0][27].ToString();                
                shipid.Text = dspo.Tables[0].Rows[0][13].ToString();
                shipname.Text = dspo.Tables[0].Rows[0][14].ToString();
                shipadd1.Text = dspo.Tables[0].Rows[0][15].ToString();
                shipadd2.Text = dspo.Tables[0].Rows[0][16].ToString();
                shipcity.Text = dspo.Tables[0].Rows[0][17].ToString();
                shipstat.Text = dspo.Tables[0].Rows[0][18].ToString();
                shipzip.Text = dspo.Tables[0].Rows[0][19].ToString();
                reckey.Text = dspo.Tables[0].Rows[0][35].ToString();
                podate.Focus();
                buyer.Text = UserLogin.UserName;
                frtpay.SelectedIndex = 0;
                fobcode.SelectedIndex = 1;
                
            }
       
     
     }

 protected void InsertCancelButton_Click(object sender, EventArgs e)
 {
     UserClass.CheckLogin(Page);
     UserClass UserLogin = (UserClass)Session["User"];
     Label po = (Label)FormView1.FindControl("vPoNoTextBox");

     ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Deletepo";     
     ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = po.Text.Trim();
     FormView1.ChangeMode(FormViewMode.ReadOnly);
 }
 protected void vender_TextChange(object sender, EventArgs e)
 {
     UserClass.CheckLogin(Page);
     UserClass UserLogin = (UserClass)Session["User"];

     TextBox vend = (TextBox)FormView1.FindControl("vVendNoTextBox");
     TextBox vendna = (TextBox)FormView1.FindControl("vVendNameTextBox");

     TextBox vendadd1 = (TextBox)FormView1.FindControl("vVendAdd1TextBox");
     TextBox vendadd2 = (TextBox)FormView1.FindControl("vVendAdd2TextBox");
     TextBox vendcity = (TextBox)FormView1.FindControl("vVendCityTextBox");
     TextBox vendstat = (TextBox)FormView1.FindControl("vVendStateTextBox");
     TextBox vendzip = (TextBox)FormView1.FindControl("vVendZipTextBox");
     TextBox vendarecode = (TextBox)FormView1.FindControl("vVendAreaCodeTextBox");
     TextBox vendhone = (TextBox)FormView1.FindControl("vVendPhoneTextBox");

     TextBox buyer = (TextBox)FormView1.FindControl("vBuyerTextBox");
     TextBox contant = (TextBox)FormView1.FindControl("vContactTextBox");
     TextBox duedate = (TextBox)FormView1.FindControl("vDueDateTextBox");
     TextBox lastshipdate = (TextBox)FormView1.FindControl("vLastShipDateTextBox");
     TextBox underpct = (TextBox)FormView1.FindControl("vUnderPctTextBox");
     TextBox overpct = (TextBox)FormView1.FindControl("vOverPctTextBox");
     TextBox carrier = (TextBox)FormView1.FindControl("vCarrierTextBox");
     TextBox taxgr = (TextBox)FormView1.FindControl("vTaxGrTextBox");
     TextBox terms = (TextBox)FormView1.FindControl("vTermsTextBox");
     RadioButtonList frtpay = (RadioButtonList)FormView1.FindControl("FreightPaymentRadioButtonList");
     RadioButtonList fobcode = (RadioButtonList)FormView1.FindControl("FOBRadioButtonList1");
     TextBox tfreight = (TextBox)FormView1.FindControl("vTFreightTextBox");
     TextBox tax = (TextBox)FormView1.FindControl("vTaxTextBox");
     TextBox cost = (TextBox)FormView1.FindControl("vTCostTextBox");

     if (vend.Text == "")
     {
         vend.Focus();
         return;
     }
     try
    {
         Corrugated corr = new Corrugated();
         DataSet ds = new DataSet();

         ds = corr.VendorLookup("search", UserLogin.UserName, "Vend", "EQUAL", vend.Text.Trim(),"");

         if (ds.Tables[0].Rows.Count == 0)
         {
             HttpContext.Current.Response.Write("<script>alert('Invalid Vendor!')</script>");
             vend.Focus();
         }
         else
         {
             vendna.Text = ds.Tables[0].Rows[0][1].ToString();
             vendadd1.Text = ds.Tables[0].Rows[0][2].ToString();
             vendcity.Text = ds.Tables[0].Rows[0][3].ToString();
             vendstat.Text = ds.Tables[0].Rows[0][4].ToString();
             vendzip.Text = ds.Tables[0].Rows[0][5].ToString();
             vendhone.Text = ds.Tables[0].Rows[0][8].ToString();
             vendarecode.Text = ds.Tables[0].Rows[0][9].ToString();
             carrier.Text = ds.Tables[0].Rows[0][10].ToString();
             contant.Text = ds.Tables[0].Rows[0][11].ToString();
             terms.Text = ds.Tables[0].Rows[0][12].ToString();            
             taxgr.Text = ds.Tables[0].Rows[0][17].ToString();
             vendadd2.Text = ds.Tables[0].Rows[0][18].ToString();
             if (ds.Tables[0].Rows[0][15].ToString() != "0")
             {
                 overpct.Text = ds.Tables[0].Rows[0][15].ToString();
                 underpct.Text = ds.Tables[0].Rows[0][16].ToString();
             }           
             
             vend.Focus();
         }
     }
     catch { }
    
 }
    
}

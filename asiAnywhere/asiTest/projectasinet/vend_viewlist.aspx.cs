
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class vend_viewlist : System.Web.UI.Page
{
    
    protected void Page_Load(object sender, System.EventArgs e)
    {        
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "vendor_list.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        


        

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;
                Session["customer_user_id"] = UserLogin.UserName;

            }

            Label fob = (Label)FormView1.FindControl("activeLabel");
            RadioButton active = (RadioButton)FormView1.FindControl("RD5");
            RadioButton inac = (RadioButton)FormView1.FindControl("RD6");

            /*if (fob.Text == "A")
            {
                active.Checked = true;
            }
            else
            {
                inac.Checked = true;
            }*/
        }
        if (Convert.ToString(Session["vendor_list_add_new_vender"]) == "addvendor")
        {
            FormView1.ChangeMode(FormViewMode.Insert);
            Session["vendor_list_add_new_vender"] = null;
        }
        
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox vend = (TextBox)FormView1.FindControl("vendorTextBox");
            Label actlabel = (Label)FormView1.FindControl("activeLabel");
            RadioButton active = (RadioButton)FormView1.FindControl("RD5");
            RadioButton inac = (RadioButton)FormView1.FindControl("RD6");
            RadioButtonList fob = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
            active.Checked = true;
            fob.SelectedIndex = 2;
            vend.Focus();
            
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox vname = (TextBox)FormView1.FindControl("vnameTextBox");
            Label fob = (Label)FormView1.FindControl("activeLabel");
            RadioButton active = (RadioButton)FormView1.FindControl("RD5");
            RadioButton inac = (RadioButton)FormView1.FindControl("RD6");

            TextBox chk = (TextBox)FormView1.FindControl("vanedivendTextBox");
            CheckBox edi = (CheckBox)FormView1.FindControl("CheckBox1");
            try
            {
                if (fob.Text == "A")
                    active.Checked = true;
                else
                    inac.Checked = true;
            }
            catch { }
            try
            {
                if (chk.Text == "yes")
                    edi.Checked = true;
                else
                    edi.Checked = false;
                vname.Focus();
            }
            catch { }

           
        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            Label fob = (Label)FormView1.FindControl("activeLabel");
            RadioButton active = (RadioButton)FormView1.FindControl("RD5");
            RadioButton inac = (RadioButton)FormView1.FindControl("RD6");
            Label chk = (Label)FormView1.FindControl("vanedivendLabel");
            CheckBox edi = (CheckBox)FormView1.FindControl("CheckBox1");
            try
            {
                if (fob.Text == "A")
                    active.Checked = true;
                else
                    inac.Checked = true;
            }
            catch { }
            try
            {
                if (chk.Text == "yes")
                    edi.Checked = true;
                else
                    edi.Checked = false;
            }
            catch { }
            try
            {
                Label reckey = (Label)FormView1.FindControl("vreckeyLabel");
                Session["vendor_list_vend_reckey"] = reckey.Text.Trim();
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
            }
            catch { }
        }


    }
    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }


   
    
    protected void lnk_Listvend_Click(object sender, EventArgs e)
    {
        Response.Redirect("vendor_list.aspx");
    }
    protected void lnk_viewvend_Click(object sender, EventArgs e)
    {
        Response.Redirect("vend_viewlist.aspx");
    }

    protected void addButton_Click(object sender, EventArgs e)
    {
        string vactive = "";
        string vedi = "";
        TextBox vend = (TextBox)FormView1.FindControl("vendorTextBox");
        TextBox type = (TextBox)FormView1.FindControl("vtypeTextBox");
        TextBox typedscr = (TextBox)FormView1.FindControl("vtypedscrTextBox");
        TextBox vname = (TextBox)FormView1.FindControl("vnameTextBox");
        TextBox contact = (TextBox)FormView1.FindControl("vcontactTextBox");
        TextBox vadd1 = (TextBox)FormView1.FindControl("vadd1TextBox");
        TextBox buyer = (TextBox)FormView1.FindControl("vbuyerTextBox");
        TextBox buyerdscr = (TextBox)FormView1.FindControl("vbuyerdscrTextBox");
        TextBox vadd2 = (TextBox)FormView1.FindControl("vadd2TextBox");
        TextBox areacod = (TextBox)FormView1.FindControl("vareacodeTextBox");
        TextBox phone = (TextBox)FormView1.FindControl("vphoneTextBox");
        TextBox faxarea = (TextBox)FormView1.FindControl("vfaxareaTextBox");
        TextBox fax = (TextBox)FormView1.FindControl("vfaxTextBox");
        TextBox city = (TextBox)FormView1.FindControl("vcityTextBox");
        TextBox state = (TextBox)FormView1.FindControl("vstateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("vzipTextBox");
        TextBox faxprfx = (TextBox)FormView1.FindControl("vfaxprefixTextBox");
        TextBox faxcntry = (TextBox)FormView1.FindControl("vfaxcountryTextBox");
        TextBox contry = (TextBox)FormView1.FindControl("vcountryTextBox");
        TextBox postal = (TextBox)FormView1.FindControl("vPostalTextBox");
        TextBox overpct = (TextBox)FormView1.FindControl("voverpctTextBox");
        TextBox underpct = (TextBox)FormView1.FindControl("vunderpctTextBox");
        TextBox taxid = (TextBox)FormView1.FindControl("vtaxidTextBox");
        TextBox actnum = (TextBox)FormView1.FindControl("vactnumTextBox");
        TextBox actdscr = (TextBox)FormView1.FindControl("vactdscrTextBox");
        TextBox remit = (TextBox)FormView1.FindControl("vremitTextBox");
        TextBox radd1 = (TextBox)FormView1.FindControl("vradd1TextBox");
        TextBox radd2 = (TextBox)FormView1.FindControl("vradd2TextBox");
        TextBox rcity = (TextBox)FormView1.FindControl("vrcityTextBox");
        TextBox rstate = (TextBox)FormView1.FindControl("vrstateTextBox");
        TextBox rzip = (TextBox)FormView1.FindControl("vrzipTextBox");
        TextBox rcontry = (TextBox)FormView1.FindControl("vrcountryTextBox");
        TextBox rpostal = (TextBox)FormView1.FindControl("vrpostalTextBox");
        TextBox checkmemo = (TextBox)FormView1.FindControl("vcheckmemoTextBox");
        TextBox currcode = (TextBox)FormView1.FindControl("vcurrcodeTextBox");
        TextBox currdscr = (TextBox)FormView1.FindControl("vcurrdscrTextBox");
        TextBox taxgr = (TextBox)FormView1.FindControl("vtaxgrTextBox");
        TextBox code1099 = (TextBox)FormView1.FindControl("vcode1099TextBox");
        TextBox anediven = (TextBox)FormView1.FindControl("vanedivendTextBox");
        TextBox terms = (TextBox)FormView1.FindControl("vtermsTextBox");
        TextBox termsdscr = (TextBox)FormView1.FindControl("vtermsdscrTextBox");
        TextBox disc = (TextBox)FormView1.FindControl("vdiscTextBox");
        TextBox poexport = (TextBox)FormView1.FindControl("vpoexportTextBox");
        TextBox discdys = (TextBox)FormView1.FindControl("vdiscdaysTextBox");
        TextBox frtpay = (TextBox)FormView1.FindControl("vfrtpayTextBox");
        TextBox rebate = (TextBox)FormView1.FindControl("vrebateTextBox");
        TextBox carrier = (TextBox)FormView1.FindControl("vcarrierTextBox");
        TextBox carrierdscr = (TextBox)FormView1.FindControl("vcarrierdscrTextBox");
        TextBox reckey = (TextBox)FormView1.FindControl("vreckeyTextBox");
        RadioButtonList fob = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
        RadioButton active = (RadioButton)FormView1.FindControl("RD5");
        RadioButton inac = (RadioButton)FormView1.FindControl("RD6");
        CheckBox edi = (CheckBox)FormView1.FindControl("CheckBox1");
        
        if (active.Checked == true)
            vactive = "A";
        else
            vactive = "I";
        if (edi.Checked == true)
            vedi = "yes";
        else
            vedi = "no";


        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();
        ObjectDataSource1.SelectParameters["prmactive"].DefaultValue = vactive;
        ObjectDataSource1.SelectParameters["prmVendor"].DefaultValue = vend.Text.Trim();
        ObjectDataSource1.SelectParameters["prmName"].DefaultValue = vname.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdd1"].DefaultValue = vadd1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdd2"].DefaultValue = vadd2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcity"].DefaultValue = city.Text.Trim();
        ObjectDataSource1.SelectParameters["prmstate"].DefaultValue = state.Text.Trim();

        ObjectDataSource1.SelectParameters["prmzip"].DefaultValue = zip.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCountry"].DefaultValue = contry.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPostal"].DefaultValue = postal.Text.Trim();
        ObjectDataSource1.SelectParameters["prmtaxid"].DefaultValue = taxid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRemit"].DefaultValue = remit.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRadd1"].DefaultValue = radd1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRadd2"].DefaultValue = radd2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRcity"].DefaultValue = rcity.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRstate"].DefaultValue = rstate.Text.Trim();


        ObjectDataSource1.SelectParameters["prmRzip"].DefaultValue = rzip.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRcountry"].DefaultValue = rcontry.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRpostal"].DefaultValue = rpostal.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCheckmemo"].DefaultValue = checkmemo.Text.Trim();
        ObjectDataSource1.SelectParameters["prmtype"].DefaultValue = type.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcontact"].DefaultValue = contact.Text.Trim();

        ObjectDataSource1.SelectParameters["prmBuyer"].DefaultValue = buyer.Text.Trim();
        ObjectDataSource1.SelectParameters["prmareacode"].DefaultValue = areacod.Text.Trim();
        ObjectDataSource1.SelectParameters["prmphone"].DefaultValue = phone.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFaxarea"].DefaultValue = faxarea.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfax"].DefaultValue = fax.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfaxprefix"].DefaultValue = faxprfx.Text.Trim();

        ObjectDataSource1.SelectParameters["prmfaxcountry"].DefaultValue = faxcntry.Text.Trim();
        ObjectDataSource1.SelectParameters["prmoverpct"].DefaultValue = overpct.Text.Trim();                
        ObjectDataSource1.SelectParameters["prmunderpct"].DefaultValue = underpct.Text.Trim();
        ObjectDataSource1.SelectParameters["prmActnum"].DefaultValue = actnum.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcurrcode"].DefaultValue = currcode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmtaxgr"].DefaultValue = taxgr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCode1099"].DefaultValue = code1099.Text.Trim();

        ObjectDataSource1.SelectParameters["prmAnedivend"].DefaultValue = vedi;
        ObjectDataSource1.SelectParameters["prmterms"].DefaultValue = terms.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdisc"].DefaultValue = disc.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRebate"].DefaultValue = rebate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfrtpay"].DefaultValue = frtpay.Text.Trim();

        ObjectDataSource1.SelectParameters["prmDiscdays"].DefaultValue = discdys.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcarrier"].DefaultValue = carrier.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFobcode"].DefaultValue = fob.SelectedValue;
        ObjectDataSource1.SelectParameters["prmPoexport"].DefaultValue = poexport.Text.Trim(); 

                //Response.Write("<script> window.location.href='vend_viewlist.aspx' </script>");
        FormView1.ChangeMode(FormViewMode.ReadOnly);

    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        string vactive = "";
        string vedi = "";
        TextBox vend = (TextBox)FormView1.FindControl("vendorTextBox");
        TextBox type = (TextBox)FormView1.FindControl("vtypeTextBox");
        TextBox typedscr = (TextBox)FormView1.FindControl("vtypedscrTextBox");
        TextBox vname = (TextBox)FormView1.FindControl("vnameTextBox");
        TextBox contact = (TextBox)FormView1.FindControl("vcontactTextBox");
        TextBox vadd1 = (TextBox)FormView1.FindControl("vadd1TextBox");
        TextBox buyer = (TextBox)FormView1.FindControl("vbuyerTextBox");
        TextBox buyerdscr = (TextBox)FormView1.FindControl("vbuyerdscrTextBox");
        TextBox vadd2 = (TextBox)FormView1.FindControl("vadd2TextBox");
        TextBox areacod = (TextBox)FormView1.FindControl("vareacodeTextBox");
        TextBox phone = (TextBox)FormView1.FindControl("vphoneTextBox");
        TextBox faxarea = (TextBox)FormView1.FindControl("vfaxareaTextBox");
        TextBox fax = (TextBox)FormView1.FindControl("vfaxTextBox");
        TextBox city = (TextBox)FormView1.FindControl("vcityTextBox");
        TextBox state = (TextBox)FormView1.FindControl("vstateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("vzipTextBox");
        TextBox faxprfx = (TextBox)FormView1.FindControl("vfaxprefixTextBox");
        TextBox faxcntry = (TextBox)FormView1.FindControl("vfaxcountryTextBox");
        TextBox contry = (TextBox)FormView1.FindControl("vcountryTextBox");
        TextBox postal = (TextBox)FormView1.FindControl("vPostalTextBox");
        TextBox overpct = (TextBox)FormView1.FindControl("voverpctTextBox");
        TextBox underpct = (TextBox)FormView1.FindControl("vunderpctTextBox");
        TextBox taxid = (TextBox)FormView1.FindControl("vtaxidTextBox");
        TextBox actnum = (TextBox)FormView1.FindControl("vactnumTextBox");
        TextBox actdscr = (TextBox)FormView1.FindControl("vactdscrTextBox");
        TextBox remit = (TextBox)FormView1.FindControl("vremitTextBox");
        TextBox radd1 = (TextBox)FormView1.FindControl("vradd1TextBox");
        TextBox radd2 = (TextBox)FormView1.FindControl("vradd2TextBox");
        TextBox rcity = (TextBox)FormView1.FindControl("vrcityTextBox");
        TextBox rstate = (TextBox)FormView1.FindControl("vrstateTextBox");
        TextBox rzip = (TextBox)FormView1.FindControl("vrzipTextBox");
        TextBox rcontry = (TextBox)FormView1.FindControl("vrcountryTextBox");
        TextBox rpostal = (TextBox)FormView1.FindControl("vrpostalTextBox");
        TextBox checkmemo = (TextBox)FormView1.FindControl("vcheckmemoTextBox");
        TextBox currcode = (TextBox)FormView1.FindControl("vcurrcodeTextBox");
        TextBox currdscr = (TextBox)FormView1.FindControl("vcurrdscrTextBox");
        TextBox taxgr = (TextBox)FormView1.FindControl("vtaxgrTextBox");
        TextBox code1099 = (TextBox)FormView1.FindControl("vcode1099TextBox");
        TextBox anediven = (TextBox)FormView1.FindControl("vanedivendTextBox");
        TextBox terms = (TextBox)FormView1.FindControl("vtermsTextBox");
        TextBox termsdscr = (TextBox)FormView1.FindControl("vtermsdscrTextBox");
        TextBox disc = (TextBox)FormView1.FindControl("vdiscTextBox");
        TextBox poexport = (TextBox)FormView1.FindControl("vpoexportTextBox");
        TextBox discdys = (TextBox)FormView1.FindControl("vdiscdaysTextBox");
        TextBox frtpay = (TextBox)FormView1.FindControl("vfrtpayTextBox");
        TextBox rebate = (TextBox)FormView1.FindControl("vrebateTextBox");
        TextBox carrier = (TextBox)FormView1.FindControl("vcarrierTextBox");
        TextBox carrierdscr = (TextBox)FormView1.FindControl("vcarrierdscrTextBox");
        TextBox reckey = (TextBox)FormView1.FindControl("vreckeyTextBox");
        RadioButtonList fob = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
        RadioButton active = (RadioButton)FormView1.FindControl("RD5");
        RadioButton inac = (RadioButton)FormView1.FindControl("RD6");
        CheckBox edi = (CheckBox)FormView1.FindControl("CheckBox1");
        if (active.Checked == true)
            vactive = "A";
        else
            vactive = "I";
        if (edi.Checked == true)
            vedi = "yes";
        else
            vedi = "no";


        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();
        ObjectDataSource1.SelectParameters["prmactive"].DefaultValue = vactive;
        ObjectDataSource1.SelectParameters["prmVendor"].DefaultValue = vend.Text.Trim();
        ObjectDataSource1.SelectParameters["prmName"].DefaultValue = vname.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdd1"].DefaultValue = vadd1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAdd2"].DefaultValue = vadd2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcity"].DefaultValue = city.Text.Trim();
        ObjectDataSource1.SelectParameters["prmstate"].DefaultValue = state.Text.Trim();

        ObjectDataSource1.SelectParameters["prmzip"].DefaultValue = zip.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCountry"].DefaultValue = contry.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPostal"].DefaultValue = postal.Text.Trim();
        ObjectDataSource1.SelectParameters["prmtaxid"].DefaultValue = taxid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRemit"].DefaultValue = remit.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRadd1"].DefaultValue = radd1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRadd2"].DefaultValue = radd2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRcity"].DefaultValue = rcity.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRstate"].DefaultValue = rstate.Text.Trim();


        ObjectDataSource1.SelectParameters["prmRzip"].DefaultValue = rzip.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRcountry"].DefaultValue = rcontry.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRpostal"].DefaultValue = rpostal.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCheckmemo"].DefaultValue = checkmemo.Text.Trim();
        ObjectDataSource1.SelectParameters["prmtype"].DefaultValue = type.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcontact"].DefaultValue = contact.Text.Trim();

        ObjectDataSource1.SelectParameters["prmBuyer"].DefaultValue = buyer.Text.Trim();
        ObjectDataSource1.SelectParameters["prmareacode"].DefaultValue = areacod.Text.Trim();
        ObjectDataSource1.SelectParameters["prmphone"].DefaultValue = phone.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFaxarea"].DefaultValue = faxarea.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfax"].DefaultValue = fax.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfaxprefix"].DefaultValue = faxprfx.Text.Trim();

        ObjectDataSource1.SelectParameters["prmfaxcountry"].DefaultValue = faxcntry.Text.Trim();
        ObjectDataSource1.SelectParameters["prmoverpct"].DefaultValue = overpct.Text.Trim();                
        ObjectDataSource1.SelectParameters["prmunderpct"].DefaultValue = underpct.Text.Trim();
        ObjectDataSource1.SelectParameters["prmActnum"].DefaultValue = actnum.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcurrcode"].DefaultValue = currcode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmtaxgr"].DefaultValue = taxgr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCode1099"].DefaultValue = code1099.Text.Trim();

        ObjectDataSource1.SelectParameters["prmAnedivend"].DefaultValue = vedi;
        ObjectDataSource1.SelectParameters["prmterms"].DefaultValue = terms.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdisc"].DefaultValue = disc.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRebate"].DefaultValue = rebate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfrtpay"].DefaultValue = frtpay.Text.Trim();

        ObjectDataSource1.SelectParameters["prmDiscdays"].DefaultValue = discdys.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcarrier"].DefaultValue = carrier.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFobcode"].DefaultValue = fob.SelectedValue;

        
        ObjectDataSource1.SelectParameters["prmPoexport"].DefaultValue = poexport.Text.Trim();

        FormView1.ChangeMode(FormViewMode.ReadOnly);
            
                //Response.Write("<script> window.location.href='cust_viewlist.aspx' </script>");
          
    }


    protected void Deletebutton_Click(object sender, EventArgs e)
    {

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        //Response.Write("<script>window.location.href = 'customer_list.aspx';</script>");
    }
    
    protected void lnk_listtot_Click(object sender, EventArgs e)
    {
        Response.Redirect("vendor_total.aspx");
    }
    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
    }
    



}

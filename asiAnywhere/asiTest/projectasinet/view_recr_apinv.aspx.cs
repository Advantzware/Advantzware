
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Collections;
using System.Configuration;
using System.Web;
using System.Threading;
using System.Globalization;
using System.Text;
#endregion

public partial class view_recr_apinv : System.Web.UI.Page
{
    string oldinv = "";
    string recid = "";
   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "RecrApVend_invoice.aspx";
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
            lblUser.Text = UserLogin.UserName;
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

                if (Convert.ToString(Session["receapp_invoice_add_button"]) == "add")
                {
                    FormView1.ChangeMode(FormViewMode.Insert);
                    Session["receapp_invoice_add_button"] = null;
                }
                if (Session["view_recr_apinv_reckey_rec_index"] != null)
                {
                    try
                    {
                        GridView1.SelectedIndex = Convert.ToInt32(Session["view_recr_apinv_reckey_rec_index"]);
                        Session["view_recr_apinv_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }
                else
                {
                    try
                    {
                        GridView1.SelectedIndex = 0;
                        Session["view_recr_apinv_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }

            }
            

        } //  ! Page.IsPostBack

        StringBuilder str = new StringBuilder();
        str.Append("<script language=javascript>");
        str.Append("function update(e){");

        str.Append("document.forms[0].FormView2_actnumTextBox.value=e[1];");
        str.Append("var line = document.getElementById('FormView2_arlineLabel');");
        str.Append("line.value = e[0];");
        str.Append("document.forms[0].FormView2_inv_qtyTextBox.value=e[2];");
        str.Append("var tmsf2 = document.getElementById('FormView2_totl_msfLabel');");
        str.Append("tmsf2.value = e[3];");
        str.Append("var ino = document.getElementById('FormView2_i_noLabel');");
        str.Append("ino.value = e[4];");
        str.Append("var sn = document.getElementById('FormView2_snumLabel');");
        str.Append("sn.value = e[5];");
        str.Append("var accdesc = document.getElementById('FormView2_actdscrlabel');");
        str.Append("accdesc.value = e[6];}");

        str.Append("</script>");

        // register the javascript into the Page
        if (!ClientScript.IsClientScriptBlockRegistered(this.GetType(), "update"))
        {
            Page.RegisterClientScriptBlock("update", str.ToString());
        }
       
       


    }

   protected void FormView1_OnDataBound(object sender, EventArgs e)
   {
     
       if (FormView1.CurrentMode == FormViewMode.Insert)
       {
           TextBox vendor = (TextBox)FormView1.FindControl("vendnoTextBox");
           Label vendorname = (Label)FormView1.FindControl("vendnameLabel");
           
           TextBox taxcode = (TextBox)FormView1.FindControl("taxcodeTextBox");
           TextBox status = (TextBox)FormView1.FindControl("statsTextBox");
           
           TextBox discount = (TextBox)FormView1.FindControl("discountTextBox");
           
           TextBox day = (TextBox)FormView1.FindControl("discdaysTextBox");
           TextBox manual = (TextBox)FormView1.FindControl("mnulchecTextBox");
           TextBox tax = (TextBox)FormView1.FindControl("taxamtTextBox");
           TextBox net = (TextBox)FormView1.FindControl("netTextBox");
           TextBox paid = (TextBox)FormView1.FindControl("paidTextBox");
           TextBox fright = (TextBox)FormView1.FindControl("freightTextBox");
           TextBox baldue = (TextBox)FormView1.FindControl("dueTextBox");
           Label user = (Label)FormView1.FindControl("USRLabel");
           DropDownList drop = (DropDownList)FormView1.FindControl("DropDownList1");
           vendor.Focus();
           drop.SelectedIndex = 2;
           status.Text = "R";
           net.Text = "0";
           paid.Text = "0";
           fright.Text = "0";
           baldue.Text = "0";
           try
           {
               UserClass UserLogin = (UserClass)Session["User"];
               voucherpay con = new voucherpay();
               DataSet ds = new DataSet();
               ds = con.SelectVendor("AddNewInv", "", UserLogin.UserName, "", "", "", "", "", "", 0, 0, 0, 0, 0, "", "", 0, 0, "", 0, "", "", 0, "");

               vendor.Text = Convert.ToString(ds.Tables[0].Rows[0][1]);
               //invdate.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);
               //duedate.Text = Convert.ToString(ds.Tables[0].Rows[0][8]);
               vendorname.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
               discount.Text = Convert.ToString(ds.Tables[0].Rows[0][11]);
               day.Text = Convert.ToString(ds.Tables[0].Rows[0][12]);
               taxcode.Text = Convert.ToString(ds.Tables[0].Rows[0][9]);
               user.Text = UserLogin.UserName; 
              
           }
           catch { }
           
           FormView2.Visible = false;
           GridView1.Visible = false;
           AddNewFormView2Button.Visible = false;

       }
       if (FormView1.CurrentMode == FormViewMode.Edit)
       {
           TextBox vendor = (TextBox)FormView1.FindControl("vendnoTextBox");           
           vendor.Focus();
           FormView2.Visible = false;
           GridView1.Visible = false;
       }
       if (FormView1.CurrentMode == FormViewMode.ReadOnly)
       {
           try
           {
               Button but1 = (Button)FormView1.FindControl("Button1");
               Label stat = (Label)FormView1.FindControl("statsLabel");
               if (stat.Text == "R")
                   but1.OnClientClick = "return confirm('Are you sure you wish to hold this Vendor Invoice?');";
                
               if (stat.Text == "H")
                   but1.OnClientClick = "return confirm('Are you sure you wish to release this Vendor Invoice?');";
           } 
           catch{}
           try
           { 
               
               Label reckey = (Label)FormView1.FindControl("reckeyLabel");
               Session["RecrApVend_invoice_reckey_rec"] = reckey.Text.Trim();
               Session["vendor_invoice_reckey_rec"] = reckey.Text.Trim();
               
           }
           catch { }
           FormView2.Visible = true;
           GridView1.Visible = true;
           
       }
   }
   
   protected void delete_Button_Click(object sender, EventArgs e)
   {

       Label reckey = (Label)FormView1.FindControl("reckeyLabel");
       UserClass UserLogin = (UserClass)Session["User"];

       voucherpay con = new voucherpay();
       
       bool check = con.ValidateUpdateVendor("ValidateDelete", "", UserLogin.UserName, "", "","","","","",0,0,0, 0,0,"","",0,0,"",0,"","",0,reckey.Text.Trim());

            string value = Convert.ToString(check);
            if (value == "True")
            {

                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "DataDelete";
                ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();
                
                FormView1.ChangeMode(FormViewMode.ReadOnly);

                Response.Write("<script>window.location.href='view_recr_apinv.aspx'</script>");
            }

   }
   protected void UpdateButton_Click(object sender, EventArgs e)
   {
       TextBox vendor = (TextBox)FormView1.FindControl("vendnoTextBox");
      
       TextBox taxcode = (TextBox)FormView1.FindControl("taxcodeTextBox");
       TextBox status = (TextBox)FormView1.FindControl("statsTextBox");
       
       TextBox discount = (TextBox)FormView1.FindControl("discountTextBox");
       
       TextBox day = (TextBox)FormView1.FindControl("discdaysTextBox");
       TextBox manual = (TextBox)FormView1.FindControl("mnulchecTextBox");
       TextBox tax = (TextBox)FormView1.FindControl("taxamtTextBox");
       TextBox net = (TextBox)FormView1.FindControl("netTextBox");
       TextBox paid = (TextBox)FormView1.FindControl("paidTextBox");
       TextBox fright = (TextBox)FormView1.FindControl("freightTextBox");
       TextBox baldue = (TextBox)FormView1.FindControl("dueTextBox");
       CheckBox chk = (CheckBox)FormView1.FindControl("CheckBox1");
       DropDownList drop = (DropDownList)FormView1.FindControl("DropDownList1");

       UserClass UserLogin = (UserClass)Session["User"];
       ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
       ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
       
       ObjectDataSource1.SelectParameters["prmvend"].DefaultValue = vendor.Text.Trim();

       
       ObjectDataSource1.SelectParameters["prmTaxamt"].DefaultValue = tax.Text.Trim();
      
       ObjectDataSource1.SelectParameters["prmTaxcode"].DefaultValue = taxcode.Text.Trim();
       ObjectDataSource1.SelectParameters["prmDiscount"].DefaultValue = discount.Text.Trim();
       ObjectDataSource1.SelectParameters["prmDiscdays"].DefaultValue = day.Text.Trim();
       ObjectDataSource1.SelectParameters["prmMnlchac"].DefaultValue = manual.Text.Trim();
       ObjectDataSource1.SelectParameters["prmTxovrwrt"].DefaultValue = Convert.ToString(chk.Checked);
       ObjectDataSource1.SelectParameters["prmCurrcode"].DefaultValue = drop.SelectedValue; 
       
       FormView1.ChangeMode(FormViewMode.ReadOnly);

   }

   protected void InsertButton_Click(object sender, EventArgs e)
   {
       TextBox vendor = (TextBox)FormView1.FindControl("vendnoTextBox");
       
       TextBox taxcode = (TextBox)FormView1.FindControl("taxcodeTextBox");
       TextBox status = (TextBox)FormView1.FindControl("statsTextBox");
       
       TextBox discount = (TextBox)FormView1.FindControl("discountTextBox");
       
       TextBox day = (TextBox)FormView1.FindControl("discdaysTextBox");
       TextBox manual = (TextBox)FormView1.FindControl("mnulchecTextBox");
       TextBox tax = (TextBox)FormView1.FindControl("taxamtTextBox");
       TextBox net = (TextBox)FormView1.FindControl("netTextBox");
       TextBox paid = (TextBox)FormView1.FindControl("paidTextBox");
       TextBox fright = (TextBox)FormView1.FindControl("freightTextBox");
       TextBox baldue = (TextBox)FormView1.FindControl("dueTextBox");
       CheckBox chk = (CheckBox)FormView1.FindControl("CheckBox1");
       DropDownList drop = (DropDownList)FormView1.FindControl("DropDownList1");

       UserClass UserLogin = (UserClass)Session["User"];
       voucherpay con = new voucherpay();
       
       bool check = con.ValidateUpdateVendor("ValidateAdd", "", UserLogin.UserName, vendor.Text.Trim(), "", "", "", "", "", 0, 0, 0, 0, 0, "", taxcode.Text.Trim(),0,0,"",0,"","",0,"");

            string value = Convert.ToString(check);
            if (value == "True")
            {


                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
                
                ObjectDataSource1.SelectParameters["prmvend"].DefaultValue = vendor.Text.Trim();                
                ObjectDataSource1.SelectParameters["prmTaxamt"].DefaultValue = tax.Text.Trim();
                
                ObjectDataSource1.SelectParameters["prmTaxcode"].DefaultValue = taxcode.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDiscount"].DefaultValue = discount.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDiscdays"].DefaultValue = day.Text.Trim();
                ObjectDataSource1.SelectParameters["prmMnlchac"].DefaultValue = manual.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTxovrwrt"].DefaultValue = Convert.ToString(chk.Checked);
                ObjectDataSource1.SelectParameters["prmCurrcode"].DefaultValue = drop.SelectedValue; 

                FormView1.ChangeMode(FormViewMode.ReadOnly);
                FormView2.ChangeMode(FormViewMode.Insert);
            }
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

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }


    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_recr_apinv.aspx");
    }
    protected void lnk_listinvoice(object sender, EventArgs e)
    {
        Response.Redirect("RecrApvend_invoice.aspx");
    }

    protected void custext_Change(object sender, EventArgs e)
    {
        TextBox cust = (TextBox)FormView1.FindControl("custnoTextBox");
        TextBox name = (TextBox)FormView1.FindControl("custnameTextBox");
        TextBox shipid = (TextBox)FormView1.FindControl("shipidTextBox");
        TextBox invoice = (TextBox)FormView1.FindControl("invnoTextBox");
        TextBox discount = (TextBox)FormView1.FindControl("discountTextBox");
        TextBox pono = (TextBox)FormView1.FindControl("ponoTextBox");
        TextBox disday = (TextBox)FormView1.FindControl("discdaysTextBox");
        TextBox invdate = (TextBox)FormView1.FindControl("invdateTextBox");
        TextBox carrier = (TextBox)FormView1.FindControl("carrierTextBox");
        TextBox duedate = (TextBox)FormView1.FindControl("duedateTextBox");
        TextBox taxcode = (TextBox)FormView1.FindControl("taxcodeTextBox");
        TextBox terms = (TextBox)FormView1.FindControl("termsTextBox");
        TextBox fright = (TextBox)FormView1.FindControl("freightTextBox");
        Label shipname = (Label)FormView1.FindControl("shipnameLabel");
        Label termsdesc = (Label)FormView1.FindControl("termsdescLabel");
        Label curr = (Label)FormView1.FindControl("currcodeTextBox");
        Label exrate = (Label)FormView1.FindControl("exrateTextBox");
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            contact con = new contact();
            DataSet ds = new DataSet();

            ds = con.SelectCustInv("CustInfo", "", UserLogin.UserName, cust.Text.Trim(), 0, 0, "", "", "", 0, 0, 0, 0, 0, "", "", "", "", "", "", "", 0, 0, 0, "", 0, "", 0, "");

            name.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
            shipid.Text = Convert.ToString(ds.Tables[0].Rows[0][9]);
            discount.Text = Convert.ToString(ds.Tables[0].Rows[0][16]);
            disday.Text = Convert.ToString(ds.Tables[0].Rows[0][18]);
            carrier.Text = Convert.ToString(ds.Tables[0].Rows[0][19]);
            taxcode.Text = Convert.ToString(ds.Tables[0].Rows[0][13]);
            terms.Text = Convert.ToString(ds.Tables[0].Rows[0][14]);
            fright.Text = Convert.ToString(ds.Tables[0].Rows[0][20]);
            duedate.Text = Convert.ToString(ds.Tables[0].Rows[0][12]);

            shipname.Text = Convert.ToString(ds.Tables[0].Rows[0][10]);
            termsdesc.Text = Convert.ToString(ds.Tables[0].Rows[0][15]);
            curr.Text = Convert.ToString(ds.Tables[0].Rows[0][21]);
            exrate.Text = Convert.ToString(ds.Tables[0].Rows[0][22]);
            name.Focus();
        }
        catch { }

    }

    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["view_recr_apinv_reckey_rec_index"] = GridView1.SelectedIndex;
        Session["view_recr_apinv_reckey_rec"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
    }

    protected void UpdateButton_Formview2_Click(object sender, EventArgs e)
    {
        Label pono = (Label)FormView2.FindControl("ponolabel");
        Label line = (Label)FormView2.FindControl("arlinelabel");
        TextBox actnum = (TextBox)FormView2.FindControl("actnumTextBox");
        Label actname = (Label)FormView2.FindControl("actdscrlabel");
        Label ino = (Label)FormView2.FindControl("i_nolabel");
        TextBox idscr = (TextBox)FormView2.FindControl("i_dscrTextBox");
        TextBox invqty = (TextBox)FormView2.FindControl("inv_qtyTextBox");
        Label consuom = (Label)FormView2.FindControl("cons_uomlabel");
        TextBox unitpr = (TextBox)FormView2.FindControl("unit_priceTextBox");
        TextBox prqtyuom = (TextBox)FormView2.FindControl("qty_uom_priTextBox");
        DropDownList tax = (DropDownList)FormView2.FindControl("DropDownList1");
        TextBox sqft = (TextBox)FormView2.FindControl("sq_ftTextBox");
        TextBox amt = (TextBox)FormView2.FindControl("amtTextBox");
        Label tamt = (Label)FormView2.FindControl("totl_msflabel");
        Label job = (Label)FormView2.FindControl("joblabel");
        Label snum = (Label)FormView2.FindControl("snumlabel");
        Label bnum = (Label)FormView2.FindControl("bnumlabel");
        if (invqty.Text == "")
            invqty.Text = "0";
        if (unitpr.Text == "")
            unitpr.Text = "0";
        if (sqft.Text == "")
            sqft.Text = "0";
        if (amt.Text == "")
            amt.Text = "0";

        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");       
        UserClass UserLogin = (UserClass)Session["User"];

        voucherpay con = new voucherpay();

        bool check = con.ValidateViewVendor("ValidateUpdate", "", UserLogin.UserName, Convert.ToString(Session["RecrApVend_invoice_reckey_rec"]), Convert.ToInt32(pono.Text.Trim()), 0, actnum.Text.Trim(), "", "", idscr.Text.Trim(), Convert.ToDecimal(invqty.Text.Trim()), "", Convert.ToDecimal(unitpr.Text.Trim()), prqtyuom.Text.Trim(), tax.SelectedValue, Convert.ToDecimal(sqft.Text.Trim()), Convert.ToDecimal(amt.Text.Trim()), 0, "", 0, 0, reckey.Text.Trim());

            string value = Convert.ToString(check);
            if (value == "True")
            {

                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Update";
                //ObjectDataSource3.SelectParameters["prmComp"].DefaultValue =  ; 
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmInv"].DefaultValue = Convert.ToString(Session["RecrApVend_invoice_reckey_rec"]);
                ObjectDataSource3.SelectParameters["prmPono"].DefaultValue = pono.Text.Trim(); ;
                ObjectDataSource3.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();
                ObjectDataSource3.SelectParameters["prmActnum"].DefaultValue = actnum.Text.Trim();
                ObjectDataSource3.SelectParameters["prmActdscr"].DefaultValue = actname.Text.Trim();                
                ObjectDataSource3.SelectParameters["prmIno"].DefaultValue = ino.Text.Trim();
                ObjectDataSource3.SelectParameters["prmIdscr"].DefaultValue = idscr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmInvqty"].DefaultValue = invqty.Text.Trim();
                ObjectDataSource3.SelectParameters["prmConsuom"].DefaultValue = consuom.Text.Trim();
                ObjectDataSource3.SelectParameters["prmUnitPrice"].DefaultValue = unitpr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmQtyUomPri"].DefaultValue = prqtyuom.Text.Trim();
                ObjectDataSource3.SelectParameters["prmTax"].DefaultValue = tax.SelectedValue;
                ObjectDataSource3.SelectParameters["prmSqft"].DefaultValue = sqft.Text.Trim();
                ObjectDataSource3.SelectParameters["prmAmt"].DefaultValue = amt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmTotlmsf"].DefaultValue = tamt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmJob"].DefaultValue = job.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSnum"].DefaultValue = snum.Text.Trim();
                ObjectDataSource3.SelectParameters["prmBnum"].DefaultValue = bnum.Text.Trim();
                ObjectDataSource3.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_recr_apinv.aspx'</script>");
            }
    }

    protected void AddButton_Formview2_Click(object sender, EventArgs e)
    {

        TextBox pono = (TextBox)FormView2.FindControl("ponoTextBox");
        TextBox line = (TextBox)FormView2.FindControl("arlineLabel");
        TextBox actnum = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox actname = (TextBox)FormView2.FindControl("actdscrLabel");
        TextBox ino = (TextBox)FormView2.FindControl("i_noLabel");
        TextBox idscr = (TextBox)FormView2.FindControl("i_dscrTextBox");
        TextBox invqty = (TextBox)FormView2.FindControl("inv_qtyTextBox");
        Label consuom = (Label)FormView2.FindControl("cons_uomLabel");
        TextBox unitpr = (TextBox)FormView2.FindControl("unit_priceTextBox");
        TextBox prqtyuom = (TextBox)FormView2.FindControl("qty_uom_priTextBox");
        DropDownList tax = (DropDownList)FormView2.FindControl("DropDownList1");
        TextBox sqft = (TextBox)FormView2.FindControl("sq_ftTextBox");
        TextBox amt = (TextBox)FormView2.FindControl("amtTextBox");
        TextBox tamt = (TextBox)FormView2.FindControl("totl_msfLabel");
        Label job = (Label)FormView2.FindControl("jobLabel");
        TextBox snum = (TextBox)FormView2.FindControl("snumLabel");
        Label bnum = (Label)FormView2.FindControl("bnumLabel");


        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");

        if (invqty.Text == "")
            invqty.Text = "0";
        if (unitpr.Text == "")
            unitpr.Text = "0";
        if (sqft.Text == "")
            sqft.Text = "0";
        if (amt.Text == "")
            amt.Text = "0";
        
        UserClass UserLogin = (UserClass)Session["User"];
        
         voucherpay con = new voucherpay();

         bool check = con.ValidateViewVendor("ValidateAdd", "", UserLogin.UserName, Convert.ToString(Session["RecrApVend_invoice_reckey_rec"]), Convert.ToInt32(pono.Text.Trim()), Convert.ToInt32(line.Text.Trim()), actnum.Text.Trim(), actname.Text.Trim(), ino.Text.Trim(), idscr.Text.Trim(), Convert.ToDecimal(invqty.Text.Trim()), consuom.Text.Trim(), Convert.ToDecimal(unitpr.Text.Trim()), prqtyuom.Text.Trim(), tax.SelectedValue, Convert.ToDecimal(sqft.Text.Trim()), Convert.ToDecimal(amt.Text.Trim()), 0, "", 0, 0, reckey.Text.Trim());

            string value = Convert.ToString(check);
            if (value == "True")
            {
                Session["view_recr_apinv_reckey_rec"] = reckey.Text.Trim();
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Add";                
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmInv"].DefaultValue = Convert.ToString(Session["RecrApVend_invoice_reckey_rec"]);
                ObjectDataSource3.SelectParameters["prmPono"].DefaultValue = pono.Text.Trim(); ;
                ObjectDataSource3.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();
                ObjectDataSource3.SelectParameters["prmActnum"].DefaultValue = actnum.Text.Trim();
                ObjectDataSource3.SelectParameters["prmActdscr"].DefaultValue = actname.Text.Trim();
                
                ObjectDataSource3.SelectParameters["prmIno"].DefaultValue = ino.Text.Trim();
                ObjectDataSource3.SelectParameters["prmIdscr"].DefaultValue = idscr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmInvqty"].DefaultValue = invqty.Text.Trim();
                ObjectDataSource3.SelectParameters["prmConsuom"].DefaultValue = consuom.Text.Trim();
                ObjectDataSource3.SelectParameters["prmUnitPrice"].DefaultValue = unitpr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmQtyUomPri"].DefaultValue = prqtyuom.Text.Trim();
                ObjectDataSource3.SelectParameters["prmTax"].DefaultValue = tax.SelectedValue;
                ObjectDataSource3.SelectParameters["prmSqft"].DefaultValue = sqft.Text.Trim();
                ObjectDataSource3.SelectParameters["prmAmt"].DefaultValue = amt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmTotlmsf"].DefaultValue = tamt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmJob"].DefaultValue = job.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSnum"].DefaultValue = snum.Text.Trim();
                ObjectDataSource3.SelectParameters["prmBnum"].DefaultValue = bnum.Text.Trim();
                ObjectDataSource3.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_recr_apinv.aspx'</script>");
               
            }
        

    }

    protected void FormView2_OnDataBound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
            TextBox pono = (TextBox)FormView2.FindControl("ponoTextBox");
            TextBox line = (TextBox)FormView2.FindControl("arlineLabel");
            TextBox account = (TextBox)FormView2.FindControl("actnumTextBox");
            TextBox actdesc = (TextBox)FormView2.FindControl("actdscrLabel");
            TextBox qty = (TextBox)FormView2.FindControl("inv_qtyTextBox");
            Label uom = (Label)FormView2.FindControl("cons_uomLabel");
            TextBox price = (TextBox)FormView2.FindControl("unit_priceTextBox");
            TextBox uompr = (TextBox)FormView2.FindControl("qty_uom_priTextBox");
            DropDownList tax = (DropDownList)FormView2.FindControl("DropDownList1");
            TextBox sqft = (TextBox)FormView2.FindControl("sq_ftTextBox");
            TextBox amount = (TextBox)FormView2.FindControl("amtTextBox");
            TextBox totmsf = (TextBox)FormView2.FindControl("totl_msfLabel");
            TextBox item = (TextBox)FormView2.FindControl("i_noLabel");
            TextBox desc = (TextBox)FormView2.FindControl("i_dscrTextBox");
            Label job = (Label)FormView2.FindControl("jobLabel");
            TextBox snum = (TextBox)FormView2.FindControl("snumLabel");
            Label bnum = (Label)FormView2.FindControl("bnumLabel");


            TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
            pono.Focus();
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";

            UserClass UserLogin = (UserClass)Session["User"];
            try
            {
                voucherpay val = new voucherpay();

                bool check = val.ValidateViewVendor("AddnewRecValidate", "", UserLogin.UserName, "", 0, 0, "", "", "", "", 0, "", 0, "", "", 0, 0, 0, "", 0, 0, Convert.ToString(Session["RecrApVend_invoice_reckey_rec"]));

                string value = Convert.ToString(check);
                if (value == "True")
                {
                    voucherpay con = new voucherpay();
                    DataSet ds = new DataSet();
                    ds = con.SelectViewVendor("AddnewRec", "", UserLogin.UserName, "", 0, 0, "", "", "", "", 0, "", 0, "", "", 0, 0, 0, "", 0, 0, Convert.ToString(Session["RecrApVend_invoice_reckey_rec"]));

                    pono.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);
                    line.Text = Convert.ToString(ds.Tables[0].Rows[0][1]);
                    account.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
                    actdesc.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);

                    item.Text = Convert.ToString(ds.Tables[0].Rows[0][4]);
                    desc.Text = Convert.ToString(ds.Tables[0].Rows[0][5]);
                    qty.Text = Convert.ToString(ds.Tables[0].Rows[0][6]);
                    uom.Text = Convert.ToString(ds.Tables[0].Rows[0][7]);

                    price.Text = Convert.ToString(ds.Tables[0].Rows[0][8]);
                    uompr.Text = Convert.ToString(ds.Tables[0].Rows[0][9]);
                    //tax.Text = Convert.ToString(ds.Tables[0].Rows[0][10]);
                    sqft.Text = Convert.ToString(ds.Tables[0].Rows[0][11]);
                    amount.Text = Convert.ToString(ds.Tables[0].Rows[0][12]);
                    totmsf.Text = Convert.ToString(ds.Tables[0].Rows[0][13]);
                    job.Text = Convert.ToString(ds.Tables[0].Rows[0][14]);
                    snum.Text = Convert.ToString(ds.Tables[0].Rows[0][15]);
                    bnum.Text = Convert.ToString(ds.Tables[0].Rows[0][16]);

                    reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][18]);
                    if (Convert.ToString(ds.Tables[0].Rows[0][10]) == "yes")
                        tax.SelectedIndex = 0;
                    else
                        tax.SelectedIndex = 1;
                    //}
                    //catch { }

                    GridView1.Visible = false;
                }
                else
                {
                    FormView2.ChangeMode(FormViewMode.ReadOnly);
                }
            }
            catch { }

        }
        if (FormView2.CurrentMode == FormViewMode.Edit)
        {
            TextBox actnum = (TextBox)FormView2.FindControl("actnumTextBox");
            actnum.Focus();
            GridView1.Visible = false;
        }
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
                Session["view_recr_apinv_reckey_rec"] = reckey.Text.Trim();

            }
            catch { }
            GridView1.Visible = true;
            try
            {
                if (FormView2.DataItemCount.ToString() == "0")
                    AddNewFormView2Button.Visible = true;
                else
                    AddNewFormView2Button.Visible = false;
            }
            catch { }
        }
    }
    protected void CancelButton_FormView2_Delete(object sender, EventArgs e)
    {
        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            voucherpay con = new voucherpay();
            DataSet ds = new DataSet();
            ds = con.SelectViewVendor("DataDelete", "001", UserLogin.UserName, Convert.ToString(Session["RecrApVend_invoice_reckey_rec"]), 0, 0, "", "", "", "", 0, "", 0, "", "", 0, 0, 0, "", 0, 0, reckey.Text.Trim());
        }
        catch { }
        FormView1.Visible = true;
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        Response.Write("<script>window.location.href='view_recr_apinv.aspx'</script>");
    }
    protected void deleteButton_FormView2_Click(object sender, EventArgs e)
    {
       
        UserClass UserLogin = (UserClass)Session["User"];
        Label reckey = (Label)FormView2.FindControl("ReckeyLabel");

        voucherpay con = new voucherpay();

        bool check = con.ValidateViewVendor("ValidateDelete", "001", UserLogin.UserName, Convert.ToString(Session["RecrApVend_invoice_reckey_rec"]), 0, 0, "", "", "", "", 0, "", 0, "", "", 0, 0, 0, "", 0, 0, reckey.Text.Trim());
        
            string value = Convert.ToString(check);
            if (value == "True")
            {
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "DataDelete";
                ObjectDataSource3.SelectParameters["prmInv"].DefaultValue = Convert.ToString(Session["RecrApVend_invoice_reckey_rec"]);
                Response.Write("<script>window.location.href='view_recr_apinv.aspx'</script>");
            }

    }

    protected void AddNewFormView2Button_Click(object sender, EventArgs e)
    {
        FormView2.ChangeMode(FormViewMode.Insert);
        AddNewFormView2Button.Visible = false;
    }

    protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }

    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);

    }

    protected void inv_qty_textbox_Change(object sender, EventArgs e)
    {
                
        Label pono = (Label)FormView2.FindControl("ponolabel");
        Label line = (Label)FormView2.FindControl("arlinelabel");
        TextBox account = (TextBox)FormView2.FindControl("actnumTextBox");
        Label actdesc = (Label)FormView2.FindControl("actdscrlabel");
        Label item = (Label)FormView2.FindControl("i_nolabel");
        TextBox desc = (TextBox)FormView2.FindControl("i_dscrTextBox");
        TextBox qty = (TextBox)FormView2.FindControl("inv_qtyTextBox");
        Label uom = (Label)FormView2.FindControl("cons_uomlabel");
        TextBox price = (TextBox)FormView2.FindControl("unit_priceTextBox");
        TextBox uompr = (TextBox)FormView2.FindControl("qty_uom_priTextBox");
        DropDownList tax = (DropDownList)FormView2.FindControl("DropDownList1");
        TextBox sqft = (TextBox)FormView2.FindControl("sq_ftTextBox");
        TextBox amount = (TextBox)FormView2.FindControl("amtTextBox");
        Label totmsf = (Label)FormView2.FindControl("totl_msflabel");
        Label job = (Label)FormView2.FindControl("joblabel");
        Label snum = (Label)FormView2.FindControl("snumlabel");
        Label bnum = (Label)FormView2.FindControl("bnumlabel");
        if (qty.Text == "")
            qty.Text = "0";
        if (price.Text == "")
            price.Text = "0";
        if (sqft.Text == "")
            sqft.Text = "0";
        if (amount.Text == "")
            amount.Text = "0";
        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");

        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            voucherpay con = new voucherpay();
            DataSet ds = new DataSet();
            ds = con.SelectViewVendor("CalcamtonTextBox", "", UserLogin.UserName, "", Convert.ToInt32(pono.Text.Trim()), 0, account.Text, "", item.Text, "", Convert.ToDecimal(qty.Text.Trim()), uom.Text.Trim(), Convert.ToDecimal(price.Text.Trim()), uompr.Text.Trim(), "", Convert.ToDecimal(sqft.Text.Trim()), 0, 0, "", 0, 0, reckey.Text.Trim());


            //sqft.Text = Convert.ToString(ds.Tables[0].Rows[0][11]);
            amount.Text = Convert.ToString(ds.Tables[0].Rows[0][12]);
            totmsf.Text = Convert.ToString(ds.Tables[0].Rows[0][13]);
            price.Focus();
        }
        catch { }

    }

    protected void inv_amount_textbox_Change(object sender, EventArgs e)
    {

        Label pono = (Label)FormView2.FindControl("ponolabel");
        Label line = (Label)FormView2.FindControl("arlinelabel");
        TextBox account = (TextBox)FormView2.FindControl("actnumTextBox");
        Label actdesc = (Label)FormView2.FindControl("actdscrlabel");
        Label item = (Label)FormView2.FindControl("i_nolabel");
        TextBox desc = (TextBox)FormView2.FindControl("i_dscrTextBox");
        TextBox qty = (TextBox)FormView2.FindControl("inv_qtyTextBox");
        Label uom = (Label)FormView2.FindControl("cons_uomlabel");
        TextBox price = (TextBox)FormView2.FindControl("unit_priceTextBox");
        TextBox uompr = (TextBox)FormView2.FindControl("qty_uom_priTextBox");
        DropDownList tax = (DropDownList)FormView2.FindControl("DropDownList1");
        TextBox sqft = (TextBox)FormView2.FindControl("sq_ftTextBox");
        TextBox amount = (TextBox)FormView2.FindControl("amtTextBox");
        Label totmsf = (Label)FormView2.FindControl("totl_msflabel");
        Label job = (Label)FormView2.FindControl("joblabel");
        Label snum = (Label)FormView2.FindControl("snumlabel");
        Label bnum = (Label)FormView2.FindControl("bnumlabel");
        if (qty.Text == "")
            qty.Text = "0";
        if (price.Text == "")
            price.Text = "0";
        if (sqft.Text == "")
            sqft.Text = "0";
        if (amount.Text == "")
            amount.Text = "0";

        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");

        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            voucherpay con = new voucherpay();
            DataSet ds = new DataSet();
            ds = con.SelectViewVendor("amountchangetextbox", "", UserLogin.UserName, "", Convert.ToInt32(pono.Text.Trim()), 0, account.Text, "", item.Text, "", Convert.ToDecimal(qty.Text.Trim()), uom.Text.Trim(), Convert.ToDecimal(price.Text.Trim()), uompr.Text.Trim(), "", Convert.ToDecimal(sqft.Text.Trim()), Convert.ToDecimal(amount.Text.Trim()), 0, "", 0, 0, reckey.Text.Trim());


            //sqft.Text = Convert.ToString(ds.Tables[0].Rows[0][11]);
            price.Text = Convert.ToString(ds.Tables[0].Rows[0][8]);
            totmsf.Text = Convert.ToString(ds.Tables[0].Rows[0][13]);
            amount.Focus();
        }
        catch { }


    }

    protected void Insertinvqty_Change(object sender, EventArgs e)
    {
        TextBox pono = (TextBox)FormView2.FindControl("ponoTextBox");
        TextBox line = (TextBox)FormView2.FindControl("arlinelabel");
        TextBox account = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox actdesc = (TextBox)FormView2.FindControl("actdscrlabel");
        TextBox item = (TextBox)FormView2.FindControl("i_nolabel");
        TextBox desc = (TextBox)FormView2.FindControl("i_dscrTextBox");
        TextBox qty = (TextBox)FormView2.FindControl("inv_qtyTextBox");
        Label uom = (Label)FormView2.FindControl("cons_uomlabel");
        TextBox price = (TextBox)FormView2.FindControl("unit_priceTextBox");
        TextBox uompr = (TextBox)FormView2.FindControl("qty_uom_priTextBox");
        DropDownList tax = (DropDownList)FormView2.FindControl("DropDownList1");
        TextBox sqft = (TextBox)FormView2.FindControl("sq_ftTextBox");
        TextBox amount = (TextBox)FormView2.FindControl("amtTextBox");
        TextBox totmsf = (TextBox)FormView2.FindControl("totl_msflabel");
        Label job = (Label)FormView2.FindControl("joblabel");
        TextBox snum = (TextBox)FormView2.FindControl("snumlabel");
        Label bnum = (Label)FormView2.FindControl("bnumlabel");
        if (qty.Text == "")
            qty.Text = "0";
        if (price.Text == "")
            price.Text = "0";
        if (sqft.Text == "")
            sqft.Text = "0";
        if (amount.Text == "")
            amount.Text = "0";

        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");

        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            voucherpay con = new voucherpay();
            DataSet ds = new DataSet();
            ds = con.SelectViewVendor("CalcamtonTextBox", "", UserLogin.UserName, "", Convert.ToInt32(pono.Text.Trim()), 0, account.Text, "", item.Text, "", Convert.ToDecimal(qty.Text.Trim()), uom.Text.Trim(), Convert.ToDecimal(price.Text.Trim()), uompr.Text.Trim(), "", Convert.ToDecimal(sqft.Text.Trim()), 0, 0, "", 0, 0, reckey.Text.Trim());


            //sqft.Text = Convert.ToString(ds.Tables[0].Rows[0][11]);
            amount.Text = Convert.ToString(ds.Tables[0].Rows[0][12]);
            totmsf.Text = Convert.ToString(ds.Tables[0].Rows[0][13]);
            price.Focus();
        }
        catch { }

    }
    protected void InsertamtTextBox_Change(object sender, EventArgs e)
    {

        TextBox pono = (TextBox)FormView2.FindControl("ponoTextBox");
        TextBox line = (TextBox)FormView2.FindControl("arlinelabel");
        TextBox account = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox actdesc = (TextBox)FormView2.FindControl("actdscrlabel");
        TextBox item = (TextBox)FormView2.FindControl("i_nolabel");
        TextBox desc = (TextBox)FormView2.FindControl("i_dscrTextBox");
        TextBox qty = (TextBox)FormView2.FindControl("inv_qtyTextBox");
        Label uom = (Label)FormView2.FindControl("cons_uomlabel");
        TextBox price = (TextBox)FormView2.FindControl("unit_priceTextBox");
        TextBox uompr = (TextBox)FormView2.FindControl("qty_uom_priTextBox");
        DropDownList tax = (DropDownList)FormView2.FindControl("DropDownList1");
        TextBox sqft = (TextBox)FormView2.FindControl("sq_ftTextBox");
        TextBox amount = (TextBox)FormView2.FindControl("amtTextBox");
        TextBox totmsf = (TextBox)FormView2.FindControl("totl_msflabel");
        Label job = (Label)FormView2.FindControl("joblabel");
        TextBox snum = (TextBox)FormView2.FindControl("snumlabel");
        Label bnum = (Label)FormView2.FindControl("bnumlabel");
        if (qty.Text == "")
            qty.Text = "0";
        if (price.Text == "")
            price.Text = "0";
        if (sqft.Text == "")
            sqft.Text = "0";
        if (amount.Text == "")
            amount.Text = "0";

        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");

        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            voucherpay con = new voucherpay();
            DataSet ds = new DataSet();
            ds = con.SelectViewVendor("amountchangetextbox", "", UserLogin.UserName, "", Convert.ToInt32(pono.Text.Trim()), 0, account.Text, "", item.Text, "", Convert.ToDecimal(qty.Text.Trim()), uom.Text.Trim(), Convert.ToDecimal(price.Text.Trim()), uompr.Text.Trim(), "", Convert.ToDecimal(sqft.Text.Trim()), Convert.ToDecimal(amount.Text.Trim()), 0, "", 0, 0, reckey.Text.Trim());


            //sqft.Text = Convert.ToString(ds.Tables[0].Rows[0][11]);
            price.Text = Convert.ToString(ds.Tables[0].Rows[0][8]);
            totmsf.Text = Convert.ToString(ds.Tables[0].Rows[0][13]);
            amount.Focus();
        }
        catch { }
    }

    protected void load_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("load_recr_apinv.aspx");
    }

}

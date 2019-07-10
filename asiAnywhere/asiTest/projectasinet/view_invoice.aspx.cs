
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class view_invoice : System.Web.UI.Page
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
            string vPage = "cust_invoice.aspx";
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
                
                if (Convert.ToString(Session["cust_invoice_add_button"]) == "add")
                {
                    FormView1.ChangeMode(FormViewMode.Insert);
                    Session["cust_invoice_add_button"] = null;
                }
                if (Session["view_invoice_gridview1_index"] != null)
                {
                    try
                    {
                        GridView1.SelectedIndex = Convert.ToInt32(Session["view_invoice_gridview1_index"]);
                        Session["view_invoice_reckey_id"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }
                else
                {
                    try
                    {
                        GridView1.SelectedIndex = 0;
                        Session["view_invoice_reckey_id"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }

            }
            

        } //  ! Page.IsPostBack
           

       


    }

   protected void FormView1_OnDataBound(object sender, EventArgs e)
   {
     
       if (FormView1.CurrentMode == FormViewMode.Insert)
       {
           TextBox cust = (TextBox)FormView1.FindControl("custnoTextBox");
           TextBox inv = (TextBox)FormView1.FindControl("invnoTextBox");
           Label reckey = (Label)FormView1.FindControl("Label1");
           TextBox invdate = (TextBox)FormView1.FindControl("invdateTextBox");
           invdate.Text = System.DateTime.Today.ToShortDateString();
           cust.Focus();
           try
           {
               UserClass UserLogin = (UserClass)Session["User"];
               contact con = new contact();
               DataSet ds = new DataSet();
               ds = con.SelectCustInv("Addno", "", UserLogin.UserName, "", 0, 0, "", "", "", 0, 0, 0, 0, 0, "", "", "", "", "", "", "", 0, 0, 0, "", 0, "", 0, "");

               inv.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);
               reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][23]);
           }
           catch { }
           FormView2.Visible = false;
           GridView1.Visible = false;
           AddNewFormView2Button.Visible = false;

       }
       if (FormView1.CurrentMode == FormViewMode.Edit)
       {
           TextBox inv = (TextBox)FormView1.FindControl("invnoTextBox");
           TextBox cust = (TextBox)FormView1.FindControl("custnoTextBox");
           HiddenField_oldinv.Value = inv.Text.Trim();
           cust.Focus();
           FormView2.Visible = false;
           GridView1.Visible = false;
       }
       if (FormView1.CurrentMode == FormViewMode.ReadOnly)
       {
           try
           {
               Label reckey = (Label)FormView1.FindControl("Label1");
               Label invoice = (Label)FormView1.FindControl("invnoLabel");
               Session["cust_invoice_invno"] = invoice.Text.Trim();
               Session["cust_invoice_reckey_rec"] = reckey.Text.Trim();
               
           }
           catch { }
           FormView2.Visible = true;
           GridView1.Visible = true;
           
       }
   }
   protected void CancelButton_Delete(object sender, EventArgs e)
   {
       TextBox inv = (TextBox)FormView1.FindControl("invnoTextBox");
       Label reckey = (Label)FormView1.FindControl("Label1");
       UserClass UserLogin = (UserClass)Session["User"];
       try
       {
           contact con = new contact();
           DataSet ds = new DataSet();
           ds = con.SelectCustInv("CancelDelete", "", UserLogin.UserName, "", 0, 0, "", "", "", 0, 0, 0, 0, 0, "", "", "", "", "", "", "", 0, 0, 0, "", 0, "", 0, reckey.Text.Trim());
       }
       catch { }

   }
   protected void delete_Button_Click(object sender, EventArgs e)
   {
       Label inv = (Label)FormView1.FindControl("invnoLabel");
       Label reckey = (Label)FormView1.FindControl("Label1");
       UserClass UserLogin = (UserClass)Session["User"];

       contact con = new contact();
       if (inv.Text == "")
           inv.Text = "0";
       bool check = con.ValidateSelectCustInv("ValidateDelete", "", UserLogin.UserName, "", Convert.ToInt32(inv.Text.Trim()), 0, "", "", "", 0, 0, 0, 0, 0, "", "", "", "", "", "", "", 0, 0, 0, "", 0, "", 0, reckey.Text.Trim());

            string value = Convert.ToString(check);
            if (value == "True")
            {

                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "DataDelete";
                ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();
                FormView1.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_invoice.aspx'</script>");
            }

   }
   protected void UpdateButton_Click(object sender, EventArgs e)
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
       Label reckey = (Label)FormView1.FindControl("Label1");

       
       UserClass UserLogin = (UserClass)Session["User"];
       ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
       ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
       ObjectDataSource1.SelectParameters["prmInv"].DefaultValue = invoice.Text.Trim();
       ObjectDataSource1.SelectParameters["prmOldInv"].DefaultValue = HiddenField_oldinv.Value;

       ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();
       ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = cust.Text.Trim();
       ObjectDataSource1.SelectParameters["prmCustname"].DefaultValue = name.Text.Trim();
       ObjectDataSource1.SelectParameters["prmInvdate"].DefaultValue = invdate.Text.Trim();
       ObjectDataSource1.SelectParameters["prmShipid"].DefaultValue = shipid.Text.Trim();
       ObjectDataSource1.SelectParameters["prmPono"].DefaultValue = pono.Text.Trim();
       ObjectDataSource1.SelectParameters["prmDuedate"].DefaultValue = duedate.Text.Trim();
       ObjectDataSource1.SelectParameters["prmTaxcode"].DefaultValue = taxcode.Text.Trim();
       ObjectDataSource1.SelectParameters["prmTerms"].DefaultValue = terms.Text.Trim();
       ObjectDataSource1.SelectParameters["prmDiscount"].DefaultValue = discount.Text.Trim();
       ObjectDataSource1.SelectParameters["prmDiscdays"].DefaultValue = disday.Text.Trim();
       ObjectDataSource1.SelectParameters["prmCarrier"].DefaultValue = carrier.Text.Trim();
       ObjectDataSource1.SelectParameters["prmFreight"].DefaultValue = fright.Text.Trim();
       
       FormView1.ChangeMode(FormViewMode.ReadOnly);

   }

   protected void InsertButton_Click(object sender, EventArgs e)
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
       Label reckey = (Label)FormView1.FindControl("Label1");
       UserClass UserLogin = (UserClass)Session["User"];
       contact con = new contact();
       if (invoice.Text == "")
           invoice.Text = "0";
        bool check = con.ValidateSelectCustInv("ValidateAdd", "", UserLogin.UserName, cust.Text.Trim(), Convert.ToInt32(invoice.Text.Trim()), 0,"", "", invdate.Text.Trim(),0,0, 0,0, 0, shipid.Text.Trim(), "","",duedate.Text.Trim(),taxcode.Text.Trim(),terms.Text.Trim(),"",0,0,0,carrier.Text.Trim(),0,"",0,reckey.Text.Trim());

            string value = Convert.ToString(check);
            if (value == "True")
            {

                Session["cust_invoice_reckey_rec"] = reckey.Text.Trim();

                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
                ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = Convert.ToString(Session["cust_invoice_reckey_rec"]);
                ObjectDataSource1.SelectParameters["prmInv"].DefaultValue = invoice.Text.Trim();

                ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = cust.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCustname"].DefaultValue = name.Text.Trim();
                ObjectDataSource1.SelectParameters["prmInvdate"].DefaultValue = invdate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmShipid"].DefaultValue = shipid.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPono"].DefaultValue = pono.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDuedate"].DefaultValue = duedate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTaxcode"].DefaultValue = taxcode.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTerms"].DefaultValue = terms.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDiscount"].DefaultValue = discount.Text.Trim();
                ObjectDataSource1.SelectParameters["prmDiscdays"].DefaultValue = disday.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCarrier"].DefaultValue = carrier.Text.Trim();
                ObjectDataSource1.SelectParameters["prmFreight"].DefaultValue = fright.Text.Trim();

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
        Response.Redirect("view_invoice.aspx");
    }
    protected void lnk_listinvoice(object sender, EventArgs e)
    {
        Response.Redirect("cust_invoice.aspx");
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
        Session["view_invoice_gridview1_index"] = GridView1.SelectedIndex;
        Session["view_invoice_reckey_id"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
    }

    protected void UpdateButton_Formview2_Click(object sender, EventArgs e)
    {
        TextBox line = (TextBox)FormView2.FindControl("arlineTextBox");
        TextBox actnum = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox iname = (TextBox)FormView2.FindControl("i_nameTextBox");
        TextBox idscr = (TextBox)FormView2.FindControl("i_dscrTextBox");
        TextBox lotno = (TextBox)FormView2.FindControl("lot_noTextBox");
        TextBox invqty = (TextBox)FormView2.FindControl("inv_qtyTextBox");
        TextBox consuom = (TextBox)FormView2.FindControl("cons_uomTextBox");
        TextBox sfst = (TextBox)FormView2.FindControl("sf_shtTextBox");
        TextBox unitpr = (TextBox)FormView2.FindControl("unit_prTextBox");
        TextBox prqtyuom = (TextBox)FormView2.FindControl("pr_qty_uomTextBox");
        TextBox discount = (TextBox)FormView2.FindControl("discTextBox");
        TextBox calamt = (TextBox)FormView2.FindControl("cal_amtTextBox");
        TextBox amtmsf = (TextBox)FormView2.FindControl("amt_msfTextBox");
        TextBox cost = (TextBox)FormView2.FindControl("costTextBox");
        TextBox dscr1 = (TextBox)FormView2.FindControl("dscr1TextBox");
        TextBox sman1 = (TextBox)FormView2.FindControl("sman1TextBox");
        TextBox spct1 = (TextBox)FormView2.FindControl("s_pct1TextBox");
        TextBox comm1 = (TextBox)FormView2.FindControl("s_comm1TextBox");
        TextBox sman2 = (TextBox)FormView2.FindControl("sman2TextBox");
        TextBox spct2 = (TextBox)FormView2.FindControl("s_pct2TextBox");
        TextBox comm2 = (TextBox)FormView2.FindControl("s_comm2TextBox");
        TextBox sman3 = (TextBox)FormView2.FindControl("sman3TextBox");
        TextBox spct3 = (TextBox)FormView2.FindControl("s_pct3TextBox");
        TextBox comm3 = (TextBox)FormView2.FindControl("s_comm3TextBox");
        if (line.Text == "")
            line.Text = "0";
        Label reckey = (Label)FormView2.FindControl("ReckeyLabel");       
        UserClass UserLogin = (UserClass)Session["User"];

         contact con = new contact();

         bool check = con.ValidateCustInvLine("ValidateUpdate", "", UserLogin.UserName, Convert.ToInt32(Session["cust_invoice_invno"]), Convert.ToInt32(line.Text.Trim()), actnum.Text.Trim(), "", "", "", "", 0, "", 0, 0, prqtyuom.Text.Trim(), 0, 0, 0, 0, dscr1.Text.Trim(), sman1.Text.Trim(), 0, 0, sman2.Text.Trim(), 0, 0, sman3.Text.Trim(), 0, 0, reckey.Text.Trim());

            string value = Convert.ToString(check);
            if (value == "True")
            {

                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Update";
                //ObjectDataSource1.SelectParameters["prmInv"].DefaultValue = invoice.Text.Trim();
                ObjectDataSource3.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();
                ObjectDataSource3.SelectParameters["prmActnum"].DefaultValue = actnum.Text.Trim();
                ObjectDataSource3.SelectParameters["prmIname"].DefaultValue = iname.Text.Trim();
                ObjectDataSource3.SelectParameters["prmIdscr"].DefaultValue = idscr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmLotno"].DefaultValue = lotno.Text.Trim();
                ObjectDataSource3.SelectParameters["prmInvqty"].DefaultValue = invqty.Text.Trim();
                ObjectDataSource3.SelectParameters["prmConsuom"].DefaultValue = consuom.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSfsht"].DefaultValue = sfst.Text.Trim();
                ObjectDataSource3.SelectParameters["prmUnitpr"].DefaultValue = unitpr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmQtyuom"].DefaultValue = prqtyuom.Text.Trim();
                ObjectDataSource3.SelectParameters["prmDisc"].DefaultValue = discount.Text.Trim();
                ObjectDataSource3.SelectParameters["prmCalamt"].DefaultValue = calamt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmAmtmsf"].DefaultValue = amtmsf.Text.Trim();
                ObjectDataSource3.SelectParameters["prmCost"].DefaultValue = cost.Text.Trim();
                ObjectDataSource3.SelectParameters["prmDscr1"].DefaultValue = dscr1.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSman1"].DefaultValue = sman1.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSpct1"].DefaultValue = spct1.Text.Trim();
                ObjectDataSource3.SelectParameters["prmScomm1"].DefaultValue = comm1.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSman2"].DefaultValue = sman2.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSpct2"].DefaultValue = spct2.Text.Trim();
                ObjectDataSource3.SelectParameters["prmScomm2"].DefaultValue = comm2.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSman3"].DefaultValue = sman3.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSpct3"].DefaultValue = spct3.Text.Trim();
                ObjectDataSource3.SelectParameters["prmScomm3"].DefaultValue = comm3.Text.Trim();
                ObjectDataSource3.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_invoice.aspx'</script>");
            }
    }

    protected void AddButton_Formview2_Click(object sender, EventArgs e)
    {

        TextBox line = (TextBox)FormView2.FindControl("arlineTextBox");
        TextBox actnum = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox iname = (TextBox)FormView2.FindControl("i_nameTextBox");
        TextBox idscr = (TextBox)FormView2.FindControl("i_dscrTextBox");
        TextBox lotno = (TextBox)FormView2.FindControl("lot_noTextBox");
        TextBox invqty = (TextBox)FormView2.FindControl("inv_qtyTextBox");
        TextBox consuom = (TextBox)FormView2.FindControl("cons_uomTextBox");
        TextBox sfst = (TextBox)FormView2.FindControl("sf_shtTextBox");
        TextBox unitpr = (TextBox)FormView2.FindControl("unit_prTextBox");
        TextBox prqtyuom = (TextBox)FormView2.FindControl("pr_qty_uomTextBox");
        TextBox discount = (TextBox)FormView2.FindControl("discTextBox");
        TextBox calamt = (TextBox)FormView2.FindControl("cal_amtTextBox");
        TextBox amtmsf = (TextBox)FormView2.FindControl("amt_msfTextBox");
        TextBox cost = (TextBox)FormView2.FindControl("costTextBox");
        TextBox dscr1 = (TextBox)FormView2.FindControl("dscr1TextBox");
        TextBox sman1 = (TextBox)FormView2.FindControl("sman1TextBox");
        TextBox spct1 = (TextBox)FormView2.FindControl("s_pct1TextBox");
        TextBox comm1 = (TextBox)FormView2.FindControl("s_comm1TextBox");
        TextBox sman2 = (TextBox)FormView2.FindControl("sman2TextBox");
        TextBox spct2 = (TextBox)FormView2.FindControl("s_pct2TextBox");
        TextBox comm2 = (TextBox)FormView2.FindControl("s_comm2TextBox");
        TextBox sman3 = (TextBox)FormView2.FindControl("sman3TextBox");
        TextBox spct3 = (TextBox)FormView2.FindControl("s_pct3TextBox");
        TextBox comm3 = (TextBox)FormView2.FindControl("s_comm3TextBox");
        Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
        if (line.Text == "")
            line.Text = "";
        UserClass UserLogin = (UserClass)Session["User"];
        
         contact con = new contact();

         bool check = con.ValidateCustInvLine("ValidateAdd", "", UserLogin.UserName, Convert.ToInt32(Session["cust_invoice_invno"]), Convert.ToInt32(line.Text.Trim()), actnum.Text.Trim(), "", "", "", "", 0, "", 0, 0, prqtyuom.Text.Trim(), 0, 0, 0, 0, dscr1.Text.Trim(), sman1.Text.Trim(), 0, 0, sman2.Text.Trim(), 0, 0, sman3.Text.Trim(), 0, 0, reckey.Text.Trim());

            string value = Convert.ToString(check);
            if (value == "True")
            {
                Session["view_invoice_reckey_id"] = reckey.Text.Trim();
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Add";
                //ObjectDataSource3.SelectParameters["prmInv"].DefaultValue = invoice.Text.Trim();

                ObjectDataSource3.SelectParameters["prmLine"].DefaultValue = line.Text.Trim();
                ObjectDataSource3.SelectParameters["prmActnum"].DefaultValue = actnum.Text.Trim();
                ObjectDataSource3.SelectParameters["prmIname"].DefaultValue = iname.Text.Trim();
                ObjectDataSource3.SelectParameters["prmIdscr"].DefaultValue = idscr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmLotno"].DefaultValue = lotno.Text.Trim();
                ObjectDataSource3.SelectParameters["prmInvqty"].DefaultValue = invqty.Text.Trim();
                ObjectDataSource3.SelectParameters["prmConsuom"].DefaultValue = consuom.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSfsht"].DefaultValue = sfst.Text.Trim();
                ObjectDataSource3.SelectParameters["prmUnitpr"].DefaultValue = unitpr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmQtyuom"].DefaultValue = prqtyuom.Text.Trim();
                ObjectDataSource3.SelectParameters["prmDisc"].DefaultValue = discount.Text.Trim();
                ObjectDataSource3.SelectParameters["prmCalamt"].DefaultValue = calamt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmAmtmsf"].DefaultValue = amtmsf.Text.Trim();

                ObjectDataSource3.SelectParameters["prmCost"].DefaultValue = cost.Text.Trim();
                ObjectDataSource3.SelectParameters["prmDscr1"].DefaultValue = dscr1.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSman1"].DefaultValue = sman1.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSpct1"].DefaultValue = spct1.Text.Trim();
                ObjectDataSource3.SelectParameters["prmScomm1"].DefaultValue = comm1.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSman2"].DefaultValue = sman2.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSpct2"].DefaultValue = spct2.Text.Trim();
                ObjectDataSource3.SelectParameters["prmScomm2"].DefaultValue = comm2.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSman3"].DefaultValue = sman3.Text.Trim();
                ObjectDataSource3.SelectParameters["prmSpct3"].DefaultValue = spct3.Text.Trim();
                ObjectDataSource3.SelectParameters["prmScomm3"].DefaultValue = comm3.Text.Trim();
                ObjectDataSource3.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_invoice.aspx'</script>");
            }
    }

    protected void FormView2_OnDataBound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
            TextBox line = (TextBox)FormView2.FindControl("arlineTextBox");
            TextBox actnum = (TextBox)FormView2.FindControl("actnumTextBox");
            TextBox actdscr = (TextBox)FormView2.FindControl("actdscrTextBox");
            TextBox consuom = (TextBox)FormView2.FindControl("cons_uomTextBox");
            TextBox qtyuom = (TextBox)FormView2.FindControl("pr_qty_uomTextBox");
            TextBox dscr1 = (TextBox)FormView2.FindControl("dscr1TextBox");
            TextBox sman1 = (TextBox)FormView2.FindControl("sman1TextBox");
            TextBox spct1 = (TextBox)FormView2.FindControl("s_pct1TextBox");

            Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
            line.Focus();
            UserClass UserLogin = (UserClass)Session["User"];
            try
            {
                contact con = new contact();
                DataSet ds = new DataSet();
                ds = con.CustInvLine("AddNewLine", "001", UserLogin.UserName, Convert.ToInt32(Session["cust_invoice_invno"]), 0, "", "", "", "", "", 0, "", 0, 0, "", 0, 0, 0, 0, "", "", 0, 0, "", 0, 0, "", 0, 0, "");

                line.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);
                actnum.Text = Convert.ToString(ds.Tables[0].Rows[0][1]);
                actdscr.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
                consuom.Text = Convert.ToString(ds.Tables[0].Rows[0][7]);
                qtyuom.Text = Convert.ToString(ds.Tables[0].Rows[0][10]);
                dscr1.Text = Convert.ToString(ds.Tables[0].Rows[0][15]);
                sman1.Text = Convert.ToString(ds.Tables[0].Rows[0][16]);
                spct1.Text = Convert.ToString(ds.Tables[0].Rows[0][17]);

                reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][26]);
            }
            catch { }

            GridView1.Visible = false;

        }
        if (FormView2.CurrentMode == FormViewMode.Edit)
        {
            TextBox line = (TextBox)FormView2.FindControl("arlineTextBox");
            line.Focus();
            GridView1.Visible = false;
        }
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
                Session["view_invoice_reckey_id"] = reckey.Text.Trim();

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
        Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
        
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            contact con = new contact();
            DataSet ds = new DataSet();
            ds = con.CustInvLine("DataDelete", "001", UserLogin.UserName, Convert.ToInt32(Session["cust_invoice_invno"]), 0, "", "", "", "", "", 0, "", 0, 0, "", 0, 0, 0, 0, "", "", 0, 0, "", 0, 0, "", 0, 0, reckey.Text.Trim());
        }
        catch { }
    }
    protected void deleteButton_FormView2_Click(object sender, EventArgs e)
    {
       
        UserClass UserLogin = (UserClass)Session["User"];
        Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
        
         contact con = new contact();

         bool check = con.ValidateCustInvLine("ValidateDelete", "", UserLogin.UserName, Convert.ToInt32(Session["cust_invoice_invno"]), 0, "", "", "", "", "", 0, "", 0, 0, "", 0, 0, 0, 0, "", "", 0, 0, "", 0, 0, "", 0, 0, reckey.Text.Trim());

            string value = Convert.ToString(check);
            if (value == "True")
            {
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "MainDelete";

                Response.Write("<script>window.location.href='view_invoice.aspx'</script>");
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

}

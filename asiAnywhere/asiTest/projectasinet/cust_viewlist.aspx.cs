
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class customers_viewlist : System.Web.UI.Page
{
    string ACTIVE;
    string TERM;
    string LOC;
    string CARRIER;
    string TERR;
    string DZONE;
    string UNDERPCT;
    string OVERPCT;
    string MARKUP;
    string SHIPDAY;
    string PALLET;
    string CASEBUNDLE;
    string INTFIELD;
    string FRTPAY;
    string FOBCODE;
    string TAXGR;
    string TAXID;
    string CURRCODE;
    string ORDLIM;
    string CRLIM;
    string TYPE;
    string SMAN;
    string SMANDESC;
    string DESCTYPE;
    string DESCTERM;
    string DESCLOC;
    string DESCCARRIER;
    string DESCTERR;
    string DESCZONE;
    string TAXCODEDESC;
    bool INV;

    protected void Page_Load(object sender, System.EventArgs e)
    {
        
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        FormView2.ChangeMode(FormViewMode.Edit);
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "cust_viewlist.aspx";
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

        TextBox active = (TextBox)FormView2.FindControl("vActiveTextBox");
        TextBox terms = (TextBox)FormView2.FindControl("vTermsTextBox");
        TextBox loc = (TextBox)FormView2.FindControl("vLocTextBox");
        TextBox carrier = (TextBox)FormView2.FindControl("vCarrierTextBox");
        TextBox dzone = (TextBox)FormView2.FindControl("vDelzoneTextBox");
        TextBox underpct = (TextBox)FormView2.FindControl("vUnderpctTextBox");
        TextBox overpct = (TextBox)FormView2.FindControl("vOverpctTextBox");
        TextBox markup = (TextBox)FormView2.FindControl("vMarkupTextBox");
        TextBox shipday = (TextBox)FormView2.FindControl("vShipdaysTextBox");
        TextBox pallet = (TextBox)FormView2.FindControl("vPalletTextBox");
        TextBox casebundle = (TextBox)FormView2.FindControl("vCasebundleTextBox");
        TextBox intfield = (TextBox)FormView2.FindControl("vIntfieldTextBox");
        TextBox frtpay = (TextBox)FormView2.FindControl("vFrtpayTextBox");
        TextBox fobcode = (TextBox)FormView2.FindControl("vFobcodeTextBox");
        TextBox taxgr = (TextBox)FormView2.FindControl("vTaxgrTextBox");
        TextBox taxid = (TextBox)FormView2.FindControl("vTaxidTextBox");
        TextBox currcode = (TextBox)FormView2.FindControl("vCurrcodeTextBox");
        TextBox ordlim = (TextBox)FormView2.FindControl("vOrdlimTextBox");
        TextBox orlim = (TextBox)FormView2.FindControl("vOrdlimTextBox");
        TextBox crlim = (TextBox)FormView2.FindControl("vCrlimTextBox");
        TextBox type = (TextBox)FormView2.FindControl("vTypeTextBox");
        TextBox sman = (TextBox)FormView2.FindControl("vSmanTextBox");
        TextBox smandesc = (TextBox)FormView2.FindControl("vdescsmanTextBox");
        TextBox desctype = (TextBox)FormView2.FindControl("vdesctypeTextBox");
        TextBox descterms = (TextBox)FormView2.FindControl("vdesctermsTextBox");
        TextBox descloc = (TextBox)FormView2.FindControl("vdesclocTextBox");
        TextBox desccarrier = (TextBox)FormView2.FindControl("vdescarrierTextBox");
        TextBox descterr = (TextBox)FormView2.FindControl("vdesterrTextBox");
        TextBox desczone = (TextBox)FormView2.FindControl("vdeszoneTextBox");
        TextBox terr = (TextBox)FormView2.FindControl("vterrTextBox");
        TextBox taxcodedesc = (TextBox)FormView2.FindControl("vTaxCodeDescTextbox");
        CheckBox inv = (CheckBox)FormView2.FindControl("vInvmethCheckBox");


        ACTIVE = active.Text;
        TERM = terms.Text;
        LOC = loc.Text;
        CARRIER = carrier.Text;
        DZONE = dzone.Text;
        UNDERPCT = underpct.Text;
        OVERPCT = overpct.Text;
        MARKUP = markup.Text;
        SHIPDAY = shipday.Text;
        PALLET = pallet.Text;
        CASEBUNDLE = casebundle.Text;
        INTFIELD =intfield.Text;
        FRTPAY = frtpay.Text;
        FOBCODE = fobcode.Text;
        TAXGR = taxgr.Text;
        TAXID = taxid.Text;
        CURRCODE = currcode.Text;
        ORDLIM = orlim.Text;
        CRLIM = crlim.Text;
        TYPE = type.Text;
        SMAN = sman.Text;
        SMANDESC = smandesc.Text;
        DESCTYPE = desctype.Text;
        DESCTERM = descterms.Text;
        DESCLOC = descloc.Text;
        DESCCARRIER = desccarrier.Text;
        TERR = terr.Text;
        DESCTERR = descterr.Text;
        DESCZONE = desczone.Text;
        TAXCODEDESC = taxcodedesc.Text;
        TERR = terr.Text;
        //Response.Write(INTFIELD);

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
        }
        if (Session["customer1_list_cust"] == null)
        {
            newaddButton.Visible = true;
        }
        else
        {
            newaddButton.Visible = false;
        }
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            DropDownList active = (DropDownList)FormView1.FindControl("DropDownList2");
            TextBox term = (TextBox)FormView1.FindControl("vtermsTextBox");
            TextBox loc = (TextBox)FormView1.FindControl("vlocTextBox");
            TextBox carr = (TextBox)FormView1.FindControl("vcarrierTextBox");
            TextBox dzone = (TextBox)FormView1.FindControl("vdelzoneTextBox");
            TextBox underpct = (TextBox)FormView1.FindControl("vunderpctTextBox");
            TextBox overpct = (TextBox)FormView1.FindControl("voverpctTextBox");
            TextBox markup = (TextBox)FormView1.FindControl("vmarkupTextBox");
            TextBox shipday = (TextBox)FormView1.FindControl("vshipdaysTextBox");
            TextBox pallet = (TextBox)FormView1.FindControl("vpalletTextBox");
            TextBox casebundle = (TextBox)FormView1.FindControl("vcasebundleTextBox");
            TextBox intfiled = (TextBox)FormView1.FindControl("vintfield1TextBox");
            RadioButtonList frtpay = (RadioButtonList)FormView1.FindControl("FreightPaymentRadioButtonList");
            RadioButtonList fobcode = (RadioButtonList)FormView1.FindControl("FOBRadioButtonList1");
            TextBox taxgr = (TextBox)FormView1.FindControl("vtaxgrTextBox");
            TextBox taxid = (TextBox)FormView1.FindControl("vtaxidTextBox");
            TextBox currcode = (TextBox)FormView1.FindControl("vcurrcodeTextBox");
            //TextBox crdlim = (TextBox)FormView1.FindControl("");
            TextBox orlim = (TextBox)FormView1.FindControl("vcrlimTextBox");
            TextBox crlim = (TextBox)FormView1.FindControl("vcrlimtTextBox");
            TextBox type = (TextBox)FormView1.FindControl("vtypeTextBox");
            TextBox sman = (TextBox)FormView1.FindControl("vsmanTextBox");
            TextBox smandesc = (TextBox)FormView1.FindControl("vdescsmanTextBox");
            TextBox desctype = (TextBox)FormView1.FindControl("vdesctypeTextBox");
            TextBox descterm = (TextBox)FormView1.FindControl("vdesctermsTextBox");
            TextBox descloc = (TextBox)FormView1.FindControl("vdesclocTextBox");
            TextBox desccarrier = (TextBox)FormView1.FindControl("vdescarrierTextBox");
            TextBox terr = (TextBox)FormView1.FindControl("vterrTextBox");
            TextBox desczone = (TextBox)FormView1.FindControl("vdeszoneTextBox");
            TextBox descterr = (TextBox)FormView1.FindControl("vdesterrTextBox");
            TextBox taxcodedesc = (TextBox)FormView1.FindControl("vfobcodeTextBox");

            active.SelectedValue = ACTIVE;
            term.Text = TERM;
            loc.Text = LOC;
            carr.Text = CARRIER;
            dzone.Text = DZONE;
            underpct.Text = UNDERPCT;
            overpct.Text = OVERPCT;
            markup.Text = MARKUP;
            shipday.Text = SHIPDAY;
            pallet.Text = PALLET;
            casebundle.Text = CASEBUNDLE;
            intfiled.Text = INTFIELD;
            taxgr.Text = TAXGR;
            taxid.Text = TAXID;
            currcode.Text = CURRCODE;
            orlim.Text = ORDLIM;
            crlim.Text = CRLIM;
            type.Text = TYPE;
            sman.Text = SMAN;
            smandesc.Text = SMANDESC;
            desctype.Text = DESCTYPE;
            descterm.Text = DESCTERM;
            descloc.Text = DESCLOC;
            desccarrier.Text = DESCCARRIER;
            terr.Text = TERR;
            descterr.Text = DESCTERR;
            taxcodedesc.Text = TAXCODEDESC;
            desczone.Text = DESCZONE;

            if (FOBCODE == "DEST" || FOBCODE == "Dest" || FOBCODE == "dest")
            {
                fobcode.SelectedIndex = 0;
            }
            else
            {
                fobcode.SelectedIndex = 1;
            }
            if (FRTPAY == "B" || FRTPAY == "b")
            {
                frtpay.SelectedIndex = 0;
            }
            if (FRTPAY == "C" || FRTPAY == "c")
            {
                frtpay.SelectedIndex = 1;
            }
            if (FRTPAY == "P" || FRTPAY == "p")
            {
                frtpay.SelectedIndex = 2;
            }
            if (FRTPAY == "T" || FRTPAY == "t")
            {
                frtpay.SelectedIndex = 3;
            }
            //Response.Write(FOBCODE);
            //Response.Write(FRTPAY);
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


    protected void newaddButton_Click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
        newaddButton.Visible = false;
    }
    
    protected void lnk_Listcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("customer_list.aspx");
    }
    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("cust_viewlist.aspx");
    }

    protected void addButton_Click(object sender, EventArgs e)
    {

        TextBox custno = (TextBox)FormView1.FindControl("vcustnoTextBox");
        TextBox custname = (TextBox)FormView1.FindControl("vcustnameTextBox");
        TextBox city = (TextBox)FormView1.FindControl("vcityTextBox");
        TextBox state = (TextBox)FormView1.FindControl("vstateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("vzipTextBox");
        TextBox type = (TextBox)FormView1.FindControl("vtypeTextBox");
        TextBox desctype = (TextBox)FormView1.FindControl("vdesctypeTextBox");

        TextBox sman = (TextBox)FormView1.FindControl("vsmanTextBox");
        TextBox descsman = (TextBox)FormView1.FindControl("vdescsmanTextBox");
        TextBox terr = (TextBox)FormView1.FindControl("vterrTextBox");
        TextBox desterr = (TextBox)FormView1.FindControl("vdesterrTextBox");
        DropDownList active = (DropDownList)FormView1.FindControl("DropDownList2");
        if (active.Text == "Active")
        {
            active.SelectedValue = "A";
        }
        if (active.Text == "Inactive")
        {
            active.SelectedValue = "I";
        }
        if (active.Text == "Inhouse")
        {
            active.SelectedValue = "X";

        }
        if (active.Text == "Statement")
        {
            active.SelectedValue = "S";
        }
        if (active.Text == "Service")
        {
            active.SelectedValue = "E";
        }

        TextBox date1 = (TextBox)FormView1.FindControl("vdate1TextBox");
        TextBox addr1 = (TextBox)FormView1.FindControl("vaddr1TextBox");
        TextBox addr2 = (TextBox)FormView1.FindControl("vaddr2TextBox");

        TextBox email = (TextBox)FormView1.FindControl("vemailTextBox");
        TextBox terms = (TextBox)FormView1.FindControl("vtermsTextBox");
        TextBox descterms = (TextBox)FormView1.FindControl("vdesctermsTextBox");
        TextBox cruse = (TextBox)FormView1.FindControl("vcruseTextBox");
        TextBox crrating = (TextBox)FormView1.FindControl("vcrratingTextBox");
        TextBox crlim = (TextBox)FormView1.FindControl("vcrlimtTextBox");
        TextBox ordlim = (TextBox)FormView1.FindControl("vcrlimTextBox");

        TextBox disc = (TextBox)FormView1.FindControl("vdiscTextBox");
        TextBox currcode = (TextBox)FormView1.FindControl("vcurrcodeTextBox");
        TextBox crholdinvdays = (TextBox)FormView1.FindControl("vcrholdinvdaysTextBox");
        TextBox crholdinvdue = (TextBox)FormView1.FindControl("vcrholdinvdueTextBox");
        TextBox custlevel = (TextBox)FormView1.FindControl("vcustlevelTextBox");

        CheckBox crhold = (CheckBox)FormView1.FindControl("vcrholdCheckBox");
        if (crhold.Checked)
        {
            HiddenField1.Value = "yes";
        }
        else
        {
            HiddenField1.Value = "no";
        }

        CheckBox finchg = (CheckBox)FormView1.FindControl("vfinchgCheckBox");
        if (finchg.Checked)
        {
            HiddenField2.Value = "yes";
        }
        else
        {
            HiddenField2.Value = "no";
        }
        CheckBox autoreprice = (CheckBox)FormView1.FindControl("vautorepriceCheckBox");
        if (autoreprice.Checked)
        {
            HiddenField3.Value = "yes";
        }
        else
        {
            HiddenField3.Value = "no";
        }

        CheckBox anedicust = (CheckBox)FormView1.FindControl("vanedicustCheckBox");
        if (anedicust.Checked)
        {
            HiddenField4.Value = "yes";
        }
        else
        {
            HiddenField4.Value = "no";
        }
        CheckBox factored = (CheckBox)FormView1.FindControl("vfactoredCheckBox");
        if (factored.Checked)
        {
            HiddenField5.Value = "yes";
        }
        else
        {
            HiddenField5.Value = "no";
        }
        RadioButtonList sort = (RadioButtonList)FormView1.FindControl("taxableRadioButtonList1");
        if (sort.Text == "Yes")
        {
            HiddenField8.Value = "Y";
        }
        else
        {
            HiddenField8.Value = "N";
        }

        TextBox taxgr = (TextBox)FormView1.FindControl("vtaxgrTextBox");
        TextBox taxid = (TextBox)FormView1.FindControl("vtaxidTextBox");
        TextBox datefield2 = (TextBox)FormView1.FindControl("vdatefield2TextBox");
        TextBox contact = (TextBox)FormView1.FindControl("vcontactTextBox");
        TextBox areacode = (TextBox)FormView1.FindControl("vareacodeTextBox");

        TextBox phone = (TextBox)FormView1.FindControl("vphoneTextBox");
        TextBox faxprefix = (TextBox)FormView1.FindControl("vfaxprefixTextBox");
        TextBox faxcountry = (TextBox)FormView1.FindControl("vfaxcountryTextBox");
        RadioButtonList frtpay = (RadioButtonList)FormView1.FindControl("FreightPaymentRadioButtonList");
        //TextBox frtpay = (TextBox)FormView1.FindControl("TextBox1");
        RadioButtonList fobcode = (RadioButtonList)FormView1.FindControl("FOBRadioButtonList1");

        if (fobcode.Text == "DEST")
        {
            HiddenField9.Value = "DEST";
        }
        else
        {
            HiddenField9.Value = "ORIG";
        }

        CheckBox shippart = (CheckBox)FormView1.FindControl("vshippartCheckBox");
        if (shippart.Checked)
        {
            HiddenField6.Value = "yes";
        }
        else
        {
            HiddenField6.Value = "no";
        }
        TextBox loc = (TextBox)FormView1.FindControl("vlocTextBox");
        TextBox descloc = (TextBox)FormView1.FindControl("vdesclocTextBox");
        TextBox carrier = (TextBox)FormView1.FindControl("vcarrierTextBox");
        TextBox descarrier = (TextBox)FormView1.FindControl("vdescarrierTextBox");
        TextBox delzone = (TextBox)FormView1.FindControl("vdelzoneTextBox");
        TextBox deszone = (TextBox)FormView1.FindControl("vdeszoneTextBox");
        TextBox underpct = (TextBox)FormView1.FindControl("vunderpctTextBox");

        TextBox overpct = (TextBox)FormView1.FindControl("voverpctTextBox");
        TextBox markup = (TextBox)FormView1.FindControl("vmarkupTextBox");
        TextBox shipdays = (TextBox)FormView1.FindControl("vshipdaysTextBox");
        CheckBox mandatory = (CheckBox)FormView1.FindControl("vmandatoryCheckBox");
        if (mandatory.Checked)
        {
            HiddenField7.Value = "yes";
        }
        else
        {
            HiddenField7.Value = "no";
        }
        TextBox pallet = (TextBox)FormView1.FindControl("vpalletTextBox");
        TextBox casebundle = (TextBox)FormView1.FindControl("vcasebundleTextBox");

        TextBox intfield1 = (TextBox)FormView1.FindControl("vintfield1TextBox");
        TextBox faxcode = (TextBox)FormView1.FindControl("vfaxcodeTextBox");
        TextBox fax = (TextBox)FormView1.FindControl("vfaxTextBox");        
        TextBox flatcomm = (TextBox)FormView1.FindControl("vflatcommTextBox");
        RadioButtonList invmeth = (RadioButtonList)FormView1.FindControl("invoiceRadioButtonList");
        

        Session["customer1_list_cust"] = custno.Text;
        Session["customer_list_shipto"] = custno.Text;
        Session["customer_list_soldto"] = custno.Text;
        Session["customer1_list_cust_name"] = custname.Text;

        contact_list c1 = new contact_list();
        bool check = c1.validatecustomer1(Convert.ToString(Session["Customers_Company"]), Convert.ToString(Session["User"]), "Validate", state.Text, zip.Text, city.Text, type.Text, desctype.Text, sman.Text, descsman.Text, terms.Text, descterms.Text, currcode.Text, loc.Text, descloc.Text, carrier.Text, descarrier.Text, delzone.Text, deszone.Text, terr.Text, desterr.Text, taxgr.Text);

        string chec = Convert.ToString(check);

        if (chec == "True")
        {

            try
            {
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
                ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
                ObjectDataSource1.SelectParameters["prmcustno"].DefaultValue = custno.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcustname"].DefaultValue = custname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcity"].DefaultValue = city.Text.Trim();
                ObjectDataSource1.SelectParameters["prmstate"].DefaultValue = state.Text.Trim();
                ObjectDataSource1.SelectParameters["prmzip"].DefaultValue = zip.Text.Trim();
                ObjectDataSource1.SelectParameters["prmtype"].DefaultValue = type.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdesctype"].DefaultValue = desctype.Text.Trim();
                ObjectDataSource1.SelectParameters["prmsman"].DefaultValue = sman.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdescsman"].DefaultValue = descsman.Text.Trim();
                ObjectDataSource1.SelectParameters["prmterr"].DefaultValue = terr.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdesterr"].DefaultValue = desterr.Text.Trim();

                ObjectDataSource1.SelectParameters["prmactive"].DefaultValue = active.SelectedValue.Trim();
                ObjectDataSource1.SelectParameters["prmdate1"].DefaultValue = date1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmaddr1"].DefaultValue = addr1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmaddr2"].DefaultValue = addr2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmemail"].DefaultValue = email.Text.Trim();
                ObjectDataSource1.SelectParameters["prmterms"].DefaultValue = terms.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdescterms"].DefaultValue = descterms.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcruse"].DefaultValue = cruse.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcrrating"].DefaultValue = crrating.Text.Trim();

                ObjectDataSource1.SelectParameters["prmcrlim"].DefaultValue = crlim.Text.Trim();
                ObjectDataSource1.SelectParameters["prmordlim"].DefaultValue = ordlim.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdisc"].DefaultValue = disc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcrholdinvdays"].DefaultValue = crholdinvdays.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcrholdinvdue"].DefaultValue = crholdinvdue.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcustlevel"].DefaultValue = custlevel.Text.Trim();

                ObjectDataSource1.SelectParameters["prmcrhold"].DefaultValue = HiddenField1.Value.Trim();
                ObjectDataSource1.SelectParameters["prmfinchg"].DefaultValue = HiddenField2.Value.Trim();
                ObjectDataSource1.SelectParameters["prmcurrcode"].DefaultValue = currcode.Text.Trim();
                ObjectDataSource1.SelectParameters["prmautoreprice"].DefaultValue = HiddenField3.Value.Trim();
                ObjectDataSource1.SelectParameters["prmanedicust"].DefaultValue = HiddenField4.Value.Trim();
                ObjectDataSource1.SelectParameters["prmfactored"].DefaultValue = HiddenField5.Value.Trim();

                ObjectDataSource1.SelectParameters["prmsort"].DefaultValue = HiddenField8.Value.Trim();
                ObjectDataSource1.SelectParameters["prmtaxgr"].DefaultValue = taxgr.Text.Trim();
                ObjectDataSource1.SelectParameters["prmtaxid"].DefaultValue = taxid.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdatefield2"].DefaultValue = datefield2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcontact"].DefaultValue = contact.Text.Trim();
                ObjectDataSource1.SelectParameters["prmareacode"].DefaultValue = areacode.Text.Trim();
                ObjectDataSource1.SelectParameters["prmphone"].DefaultValue = phone.Text.Trim();

                ObjectDataSource1.SelectParameters["prmfaxprefix"].DefaultValue = faxprefix.Text.Trim();
                ObjectDataSource1.SelectParameters["prmfaxcountry"].DefaultValue = faxcountry.Text.Trim();
                ObjectDataSource1.SelectParameters["prmfrtpay"].DefaultValue = frtpay.SelectedValue;
                ObjectDataSource1.SelectParameters["prmfobcode"].DefaultValue = HiddenField9.Value.Trim();

                ObjectDataSource1.SelectParameters["prmshippart"].DefaultValue = HiddenField6.Value.Trim();

                ObjectDataSource1.SelectParameters["prmloc"].DefaultValue = loc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdescloc"].DefaultValue = descloc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcarrier"].DefaultValue = carrier.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdescarrier"].DefaultValue = descarrier.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdelzone"].DefaultValue = delzone.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdeszone"].DefaultValue = deszone.Text.Trim();
                ObjectDataSource1.SelectParameters["prmunderpct"].DefaultValue = underpct.Text.Trim();
                ObjectDataSource1.SelectParameters["prmoverpct"].DefaultValue = overpct.Text.Trim();
                ObjectDataSource1.SelectParameters["prmmarkup"].DefaultValue = markup.Text.Trim();
                ObjectDataSource1.SelectParameters["prmshipdays"].DefaultValue = shipdays.Text.Trim();
                ObjectDataSource1.SelectParameters["prmmandatory"].DefaultValue = HiddenField7.Value.Trim();

                ObjectDataSource1.SelectParameters["prmpallet"].DefaultValue = pallet.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcasebundle"].DefaultValue = casebundle.Text.Trim();
                ObjectDataSource1.SelectParameters["prmintfield1"].DefaultValue = intfield1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmfaxcode"].DefaultValue = faxcode.Text.Trim();
                ObjectDataSource1.SelectParameters["prmfax"].DefaultValue = fax.Text.Trim();
                ObjectDataSource1.SelectParameters["prminvmeth"].DefaultValue = invmeth.SelectedValue;

                ObjectDataSource1.SelectParameters["prmflatcomm"].DefaultValue = flatcomm.Text.Trim();
                ObjectDataSource1.SelectParameters["prmflatcomm"].DefaultValue = flatcomm.Text.Trim();
            }
            catch
            {
                return;
            }
            finally
            {
                Response.Write("<script> window.location.href='cust_viewlist.aspx' </script>");
            }
        }
        else
        {
        }

    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {

        TextBox custname = (TextBox)FormView1.FindControl("vcustnameTextBox");
        TextBox city = (TextBox)FormView1.FindControl("vcityTextBox");
        TextBox state = (TextBox)FormView1.FindControl("vstateTextBox");
        TextBox zip = (TextBox)FormView1.FindControl("vzipTextBox");
        TextBox type = (TextBox)FormView1.FindControl("vtypeTextBox");
        TextBox desctype = (TextBox)FormView1.FindControl("vdesctypeTextBox");

        TextBox sman = (TextBox)FormView1.FindControl("vsmanTextBox");
        TextBox descsman = (TextBox)FormView1.FindControl("vdescsmanTextBox");
        TextBox terr = (TextBox)FormView1.FindControl("vterrTextBox");
        TextBox desterr = (TextBox)FormView1.FindControl("vdesterrTextBox");
        DropDownList active = (DropDownList)FormView1.FindControl("DropDownList2");
        if (active.Text == "Active")
        {
            active.SelectedValue = "A";
        }
        if (active.Text == "Inactive")
        {
            active.SelectedValue = "I";
        }
        if (active.Text == "Inhouse")
        {
            active.SelectedValue = "X";

        }
        if (active.Text == "Statement")
        {
            active.SelectedValue = "S";
        }
        if (active.Text == "Service")
        {
            active.SelectedValue = "E";
        }



        TextBox date1 = (TextBox)FormView1.FindControl("vdate1TextBox");
        TextBox addr1 = (TextBox)FormView1.FindControl("vaddr1TextBox");
        TextBox addr2 = (TextBox)FormView1.FindControl("vaddr2TextBox");

        TextBox email = (TextBox)FormView1.FindControl("vemailTextBox");
        TextBox terms = (TextBox)FormView1.FindControl("vtermsTextBox");
        TextBox descterms = (TextBox)FormView1.FindControl("vdesctermsTextBox");
        TextBox cruse = (TextBox)FormView1.FindControl("vcruseTextBox");
        TextBox crrating = (TextBox)FormView1.FindControl("vcrratingTextBox");
        TextBox crlim = (TextBox)FormView1.FindControl("vcrlimtTextBox");
        TextBox ordlim = (TextBox)FormView1.FindControl("vcrlimTextBox");


        TextBox disc = (TextBox)FormView1.FindControl("vdiscTextBox");
        TextBox currcode = (TextBox)FormView1.FindControl("vcurrcodeTextBox");
        TextBox crholdinvdays = (TextBox)FormView1.FindControl("vcrholdinvdaysTextBox");
        TextBox crholdinvdue = (TextBox)FormView1.FindControl("vcrholdinvdueTextBox");
        TextBox custlevel = (TextBox)FormView1.FindControl("vcustlevelTextBox");

        CheckBox crhold = (CheckBox)FormView1.FindControl("vcrholdCheckBox");
        if (crhold.Checked)
        {
            HiddenField1.Value = "yes";
        }
        else
        {
            HiddenField1.Value = "no";
        }

        CheckBox finchg = (CheckBox)FormView1.FindControl("vfinchgCheckBox");
        if (finchg.Checked)
        {
            HiddenField2.Value = "yes";
        }
        else
        {
            HiddenField2.Value = "no";
        }
        CheckBox autoreprice = (CheckBox)FormView1.FindControl("vautorepriceCheckBox");
        if (autoreprice.Checked)
        {
            HiddenField3.Value = "yes";
        }
        else
        {
            HiddenField3.Value = "no";
        }

        CheckBox anedicust = (CheckBox)FormView1.FindControl("vanedicustCheckBox");
        if (anedicust.Checked)
        {
            HiddenField4.Value = "yes";
        }
        else
        {
            HiddenField4.Value = "no";
        }
        CheckBox factored = (CheckBox)FormView1.FindControl("vfactoredCheckBox");
        if (factored.Checked)
        {
            HiddenField5.Value = "yes";
        }
        else
        {
            HiddenField5.Value = "no";
        }
        RadioButtonList sort = (RadioButtonList)FormView1.FindControl("taxableRadioButtonList1");
        if (sort.Text == "Y")
        {
            HiddenField8.Value = "Y";
        }
        else
        {
            HiddenField8.Value = "N";
        }

        TextBox taxgr = (TextBox)FormView1.FindControl("vtaxgrTextBox");
        TextBox taxdesc = (TextBox)FormView1.FindControl("vfobcodeTextBox");
        TextBox taxid = (TextBox)FormView1.FindControl("vtaxidTextBox");
        TextBox datefield2 = (TextBox)FormView1.FindControl("vdatefield2TextBox");
        TextBox contact = (TextBox)FormView1.FindControl("vcontactTextBox");
        TextBox areacode = (TextBox)FormView1.FindControl("vareacodeTextBox");

        TextBox phone = (TextBox)FormView1.FindControl("vphoneTextBox");
        TextBox faxprefix = (TextBox)FormView1.FindControl("vfaxprefixTextBox");
        TextBox faxcountry = (TextBox)FormView1.FindControl("vfaxcountryTextBox");
        //TextBox frtpay = (TextBox)FormView1.FindControl("TextBox4");
        RadioButtonList frtpay = (RadioButtonList)FormView1.FindControl("FreightPaymentRadioButtonList");
        //if (frtpay.Text == "Bill")
        //{
        //    HiddenField10.Value = "B";
        //}
        //if (frtpay.Text == "Collect")
        ////{
        //    HiddenField10.Value = "C";
        //}
        //if (frtpay.Text == "Perpaid")
        //{
        //    HiddenField10.Value = "P";
        //}
        //if (frtpay.Text == "3rd Party")
        //{
        //    HiddenField10.Value = "T";
        //}
        RadioButtonList fobcode = (RadioButtonList)FormView1.FindControl("FOBRadioButtonList1");
        if (fobcode.Text == "DEST")
        {
            HiddenField9.Value = "DEST";
        }
        else
        {
            HiddenField9.Value = "ORIG";
        }

        CheckBox shippart = (CheckBox)FormView1.FindControl("vshippartCheckBox");
        if (shippart.Checked)
        {
            HiddenField6.Value = "yes";
        }
        else
        {
            HiddenField6.Value = "no";
        }
        TextBox loc = (TextBox)FormView1.FindControl("vlocTextBox");
        TextBox descloc = (TextBox)FormView1.FindControl("vdesclocTextBox");
        TextBox carrier = (TextBox)FormView1.FindControl("vcarrierTextBox");
        TextBox descarrier = (TextBox)FormView1.FindControl("vdescarrierTextBox");
        TextBox delzone = (TextBox)FormView1.FindControl("vdelzoneTextBox");
        TextBox deszone = (TextBox)FormView1.FindControl("vdeszoneTextBox");
        TextBox underpct = (TextBox)FormView1.FindControl("vunderpctTextBox");

        TextBox overpct = (TextBox)FormView1.FindControl("voverpctTextBox");
        TextBox markup = (TextBox)FormView1.FindControl("vmarkupTextBox");
        TextBox shipdays = (TextBox)FormView1.FindControl("vshipdaysTextBox");

        CheckBox mandatory = (CheckBox)FormView1.FindControl("vmandatoryCheckBox");
        if (mandatory.Checked)
        {
            HiddenField7.Value = "yes";
        }
        else
        {
            HiddenField7.Value = "no";
        }

        TextBox casebundle = (TextBox)FormView1.FindControl("vcasebundleTextBox");
        TextBox pallet = (TextBox)FormView1.FindControl("vpalletTextBox");
        TextBox intfield1 = (TextBox)FormView1.FindControl("vintfield1TextBox");
        TextBox faxcode = (TextBox)FormView1.FindControl("vfaxcodeTextBox");
        TextBox fax = (TextBox)FormView1.FindControl("vfaxTextBox");        
        TextBox flatcomm = (TextBox)FormView1.FindControl("vflatcommTextBox");
        RadioButtonList invmeth = (RadioButtonList)FormView1.FindControl("invoiceperRadioButtonList1");
        Session["customer1_list_cust_name"] = custname.Text;
        
        contact_list c1 = new contact_list();
        bool check = c1.validatecustomer1(Convert.ToString(Session["Customers_Company"]), Convert.ToString(Session["User"]), "Validate", state.Text, zip.Text, city.Text, type.Text, desctype.Text, sman.Text, descsman.Text, terms.Text, descterms.Text, currcode.Text, loc.Text, descloc.Text, carrier.Text, descarrier.Text, delzone.Text, deszone.Text, terr.Text, desterr.Text, taxgr.Text);

        string chec = Convert.ToString(check);

        if (chec == "True")
        {

            try
            {

                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";

                ObjectDataSource1.SelectParameters["prmcustname"].DefaultValue = custname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcity"].DefaultValue = city.Text.Trim();
                ObjectDataSource1.SelectParameters["prmstate"].DefaultValue = state.Text.Trim();
                ObjectDataSource1.SelectParameters["prmzip"].DefaultValue = zip.Text.Trim();
                ObjectDataSource1.SelectParameters["prmtype"].DefaultValue = type.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdesctype"].DefaultValue = desctype.Text.Trim();
                ObjectDataSource1.SelectParameters["prmsman"].DefaultValue = sman.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdescsman"].DefaultValue = descsman.Text.Trim();
                ObjectDataSource1.SelectParameters["prmterr"].DefaultValue = terr.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdesterr"].DefaultValue = desterr.Text.Trim();

                ObjectDataSource1.SelectParameters["prmactive"].DefaultValue = active.SelectedValue.Trim();
                ObjectDataSource1.SelectParameters["prmdate1"].DefaultValue = date1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmaddr1"].DefaultValue = addr1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmaddr2"].DefaultValue = addr2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmemail"].DefaultValue = email.Text.Trim();
                ObjectDataSource1.SelectParameters["prmterms"].DefaultValue = terms.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdescterms"].DefaultValue = descterms.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcruse"].DefaultValue = cruse.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcrrating"].DefaultValue = crrating.Text.Trim();


                ObjectDataSource1.SelectParameters["prmordlim"].DefaultValue = ordlim.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcrlim"].DefaultValue = crlim.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdisc"].DefaultValue = disc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcrholdinvdays"].DefaultValue = crholdinvdays.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcrholdinvdue"].DefaultValue = crholdinvdue.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcustlevel"].DefaultValue = custlevel.Text.Trim();

                ObjectDataSource1.SelectParameters["prmcrhold"].DefaultValue = HiddenField1.Value.Trim();
                ObjectDataSource1.SelectParameters["prmfinchg"].DefaultValue = HiddenField2.Value.Trim();
                ObjectDataSource1.SelectParameters["prmcurrcode"].DefaultValue = currcode.Text.Trim();
                ObjectDataSource1.SelectParameters["prmautoreprice"].DefaultValue = HiddenField3.Value.Trim();
                ObjectDataSource1.SelectParameters["prmanedicust"].DefaultValue = HiddenField4.Value.Trim();
                ObjectDataSource1.SelectParameters["prmfactored"].DefaultValue = HiddenField5.Value.Trim();

                ObjectDataSource1.SelectParameters["prmsort"].DefaultValue = HiddenField8.Value.Trim();
                ObjectDataSource1.SelectParameters["prmtaxgr"].DefaultValue = taxgr.Text.Trim();
                //ObjectDataSource1.SelectParameters["prmtaxgr"].DefaultValue = taxdesc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmtaxid"].DefaultValue = taxid.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdatefield2"].DefaultValue = datefield2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcontact"].DefaultValue = contact.Text.Trim();
                ObjectDataSource1.SelectParameters["prmareacode"].DefaultValue = areacode.Text.Trim();
                ObjectDataSource1.SelectParameters["prmphone"].DefaultValue = phone.Text.Trim();

                ObjectDataSource1.SelectParameters["prmfaxprefix"].DefaultValue = faxprefix.Text.Trim();
                ObjectDataSource1.SelectParameters["prmfaxcountry"].DefaultValue = faxcountry.Text.Trim();
                ObjectDataSource1.SelectParameters["prmfrtpay"].DefaultValue = frtpay.SelectedValue;
                ObjectDataSource1.SelectParameters["prmfobcode"].DefaultValue = HiddenField9.Value.Trim();
                ObjectDataSource1.SelectParameters["prmshippart"].DefaultValue = HiddenField6.Value.Trim();

                ObjectDataSource1.SelectParameters["prmloc"].DefaultValue = loc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdescloc"].DefaultValue = descloc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcarrier"].DefaultValue = carrier.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdescarrier"].DefaultValue = descarrier.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdelzone"].DefaultValue = delzone.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdeszone"].DefaultValue = deszone.Text.Trim();
                ObjectDataSource1.SelectParameters["prmunderpct"].DefaultValue = underpct.Text.Trim();
                ObjectDataSource1.SelectParameters["prmoverpct"].DefaultValue = overpct.Text.Trim();
                ObjectDataSource1.SelectParameters["prmmarkup"].DefaultValue = markup.Text.Trim();
                ObjectDataSource1.SelectParameters["prmshipdays"].DefaultValue = shipdays.Text.Trim();
                ObjectDataSource1.SelectParameters["prmmandatory"].DefaultValue = HiddenField7.Value.Trim();

                ObjectDataSource1.SelectParameters["prmpallet"].DefaultValue = pallet.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcasebundle"].DefaultValue = casebundle.Text.Trim();
                ObjectDataSource1.SelectParameters["prmintfield1"].DefaultValue = intfield1.Text.Trim();
                ObjectDataSource1.SelectParameters["prmfaxcode"].DefaultValue = faxcode.Text.Trim();
                ObjectDataSource1.SelectParameters["prmfax"].DefaultValue = fax.Text.Trim();
                //ObjectDataSource1.SelectParameters["prminvmeth"].DefaultValue = "True";
                

                ObjectDataSource1.SelectParameters["prmflatcomm"].DefaultValue = flatcomm.Text.Trim();
            }
            catch
            {
                return;
            }
            finally
            {
                Response.Write("<script> window.location.href='cust_viewlist.aspx' </script>");
            }
        }
        else
        {
        }




    }


    protected void Deletebutton_Click(object sender, EventArgs e)
    {

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        Session["customer1_list_index"] = null;
        Response.Write("<script>window.location.href = 'customer_list.aspx';</script>");
    }
    protected void lnk_listship_Click(object sender, EventArgs e)
    {
        Response.Redirect("cust_list_ship.aspx");
    }
    protected void lnk_viewship_Click(object sender, EventArgs e)
    {
        Response.Redirect("cust_view_ship.aspx");
    }
    protected void lnk_viewsold_Click(object sender, EventArgs e)
    {
        Response.Redirect("cust_view_sold.aspx");
    }
    protected void lnk_listsold_Click(object sender, EventArgs e)
    {
        Response.Redirect("cust_list_sold.aspx");
    }



}

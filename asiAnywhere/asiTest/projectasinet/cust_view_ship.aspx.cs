
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class customers_view_ship : System.Web.UI.Page
{


    protected void Page_Load(object sender, System.EventArgs e)
    {


        Label cust1 = (Label)Form.FindControl("Customer");
        cust1.Text = Convert.ToString(Session["customer1_list_cust"]);
        Label cust1name = (Label)Form.FindControl("CustName");
        cust1name.Text = Convert.ToString(Session["customer1_list_cust_name"]);
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "cust_view_ship.aspx";
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
            Session["Customers_ShipCompany"] = labelcompany.Text;
            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;
                Session["customer_user_id"] = UserLogin.UserName;

            }
            if (Session["customer_list_shipto"] == null)
            {
                newaddButton.Visible = true;
            }
            else
            {
                newaddButton.Visible = false;

            }
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

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            RadioButtonList meth = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
            meth.SelectedIndex = 1;
            TextBox code = (TextBox)FormView1.FindControl("vshipidTextBox");
            code.Focus();
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox code = (TextBox)FormView1.FindControl("vshipnameTextBox");
            code.Focus();
            CheckBox broker = (CheckBox)FormView1.FindControl("vbrokerCheckBox");
            CheckBox bill = (CheckBox)FormView1.FindControl("vbillCheckBox");

            Label code1 = (Label)FormView1.FindControl("vshipidLabel");
            if (code1.Text == Convert.ToString(Session["customer1_list_cust"]))
            {
                broker.Enabled = false;
                bill.Enabled = false;
            }
        }
    }

    protected void addButton_Click(object sender, EventArgs e)
    {
        // Session["customer1_list_cust"] = null;
        // Session["Customers_ShipCompany"] = null;

        TextBox shipid = (TextBox)FormView1.FindControl("vshipidTextBox");
        TextBox shipname = (TextBox)FormView1.FindControl("vshipnameTextBox");
        TextBox shipcity = (TextBox)FormView1.FindControl("vshipcityTextBox");
        TextBox shipstate = (TextBox)FormView1.FindControl("vshipstateTextBox");
        TextBox shipzip = (TextBox)FormView1.FindControl("vshipzipTextBox");
        TextBox shipaddr1 = (TextBox)FormView1.FindControl("vshipaddr1TextBox");

        TextBox shipaddr2 = (TextBox)FormView1.FindControl("vshipaddr2TextBox");
        TextBox contact = (TextBox)FormView1.FindControl("vcontactTextBox");
        TextBox areacode = (TextBox)FormView1.FindControl("vareacodeTextBox");
        TextBox phone = (TextBox)FormView1.FindControl("vphoneTextBox");

        TextBox taxcode = (TextBox)FormView1.FindControl("vtaxcodeTextBox");
        CheckBox broker = (CheckBox)FormView1.FindControl("vbrokerCheckBox");
        if (broker.Checked)
        {
            HiddenField1.Value = "yes";
        }
        else
        {
            HiddenField1.Value = "no";
        }
        CheckBox bill = (CheckBox)FormView1.FindControl("vbillCheckBox");
        if (bill.Checked)
        {
            HiddenField2.Value = "yes";
        }
        else
        {
            HiddenField2.Value = "no";
        }

        TextBox dockloc = (TextBox)FormView1.FindControl("vdocklocTextBox");
        TextBox dockhour = (TextBox)FormView1.FindControl("vdockhourTextBox");
        TextBox locbin = (TextBox)FormView1.FindControl("vlocbinTextBox");

        TextBox carrier = (TextBox)FormView1.FindControl("vcarrierTextBox");
        TextBox pallet = (TextBox)FormView1.FindControl("vpalletTextBox");
        RadioButtonList shipmeth = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
        if (shipmeth.SelectedIndex == 0)
        {
            HiddenField3.Value = "yes";
        }
        if (shipmeth.SelectedIndex == 1)
        {
            HiddenField3.Value = "no";
        }

        TextBox delchg = (TextBox)FormView1.FindControl("vdelchgTextBox");
        TextBox deltime = (TextBox)FormView1.FindControl("vdeltimeTextBox");
        TextBox destcode = (TextBox)FormView1.FindControl("vdestcodeTextBox");
        TextBox notes1 = (TextBox)FormView1.FindControl("vnotes1TextBox");

        TextBox notes2 = (TextBox)FormView1.FindControl("vnotes2TextBox");
        TextBox notes3 = (TextBox)FormView1.FindControl("vnotes3TextBox");
        TextBox notes4 = (TextBox)FormView1.FindControl("vnotes4TextBox");

        TextBox faxAreaCode = (TextBox)FormView1.FindControl("vfaxAreaCodeTextBox");
        TextBox faxNumber = (TextBox)FormView1.FindControl("vFaxNumberTextBox");
        TextBox fi_jdedid = (TextBox)FormView1.FindControl("vfi_jdedidTextBox");
        TextBox tb_mandatorytax = (TextBox)FormView1.FindControl("vtb_mandatorytaxTextBox");
        TextBox loc = (TextBox)FormView1.FindControl("vlocTextBox");

        Session["customer_list_shipto"] = shipid.Text;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";

        //ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_ShipCompany"]);
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        ObjectDataSource1.SelectParameters["prmCustomer"].DefaultValue = Convert.ToString(Session["customer1_list_cust"]);
        ObjectDataSource1.SelectParameters["prmshipid"].DefaultValue = shipid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmshipname"].DefaultValue = shipname.Text.Trim();
        ObjectDataSource1.SelectParameters["prmshipcity"].DefaultValue = shipcity.Text.Trim();
        ObjectDataSource1.SelectParameters["prmshipstate"].DefaultValue = shipstate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmshipzip"].DefaultValue = shipzip.Text.Trim();
        ObjectDataSource1.SelectParameters["prmshipaddr1"].DefaultValue = shipaddr1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmshipaddr2"].DefaultValue = shipaddr2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcontact"].DefaultValue = contact.Text.Trim();
        ObjectDataSource1.SelectParameters["prmareacode"].DefaultValue = areacode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmphone"].DefaultValue = phone.Text.Trim();

        ObjectDataSource1.SelectParameters["prmtaxcode"].DefaultValue = taxcode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbroker"].DefaultValue = HiddenField1.Value.Trim();
        ObjectDataSource1.SelectParameters["prmbill"].DefaultValue = HiddenField2.Value.Trim();
        ObjectDataSource1.SelectParameters["prmdockloc"].DefaultValue = dockloc.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdockhour"].DefaultValue = dockhour.Text.Trim();
        ObjectDataSource1.SelectParameters["prmlocbin"].DefaultValue = locbin.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcarrier"].DefaultValue = carrier.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpallet"].DefaultValue = pallet.Text.Trim();

        ObjectDataSource1.SelectParameters["prmshipmeth"].DefaultValue = HiddenField3.Value.Trim();

        ObjectDataSource1.SelectParameters["prmdelchg"].DefaultValue = delchg.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdeltime"].DefaultValue = deltime.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdestcode"].DefaultValue = destcode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmnotes1"].DefaultValue = notes1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmnotes2"].DefaultValue = notes2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmnotes3"].DefaultValue = notes3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmnotes4"].DefaultValue = notes4.Text.Trim();

        ObjectDataSource1.SelectParameters["prmfaxAreaCode"].DefaultValue = faxAreaCode.Text.Trim();

        ObjectDataSource1.SelectParameters["prmfaxNumber"].DefaultValue = faxNumber.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfi_jdedid"].DefaultValue = fi_jdedid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmtb_mandatorytax"].DefaultValue = tb_mandatorytax.Text.Trim();
        ObjectDataSource1.SelectParameters["prmloc"].DefaultValue = loc.Text.Trim();

    }


    protected void UpdateButton_Click(object sender, EventArgs e)
    {



        TextBox shipname = (TextBox)FormView1.FindControl("vshipnameTextBox");
        TextBox shipcity = (TextBox)FormView1.FindControl("vshipcityTextBox");
        TextBox shipstate = (TextBox)FormView1.FindControl("vshipstateTextBox");
        TextBox shipzip = (TextBox)FormView1.FindControl("vshipzipTextBox");
        TextBox shipaddr1 = (TextBox)FormView1.FindControl("vshipaddr1TextBox");

        TextBox shipaddr2 = (TextBox)FormView1.FindControl("vshipaddr2TextBox");
        TextBox contact = (TextBox)FormView1.FindControl("vcontactTextBox");
        TextBox areacode = (TextBox)FormView1.FindControl("vareacodeTextBox");
        TextBox phone = (TextBox)FormView1.FindControl("vphoneTextBox");

        TextBox taxcode = (TextBox)FormView1.FindControl("vtaxcodeTextBox");
        CheckBox broker = (CheckBox)FormView1.FindControl("vbrokerCheckBox");
        if (broker.Checked)
        {
            HiddenField1.Value = "yes";
        }
        else
        {
            HiddenField1.Value = "no";
        }
        CheckBox bill = (CheckBox)FormView1.FindControl("vbillCheckBox");
        if (bill.Checked)
        {
            HiddenField2.Value = "yes";
        }
        else
        {
            HiddenField2.Value = "no";
        }



        TextBox dockloc = (TextBox)FormView1.FindControl("vdocklocTextBox");
        TextBox dockhour = (TextBox)FormView1.FindControl("vdockhourTextBox");
        TextBox locbin = (TextBox)FormView1.FindControl("vlocbinTextBox");

        TextBox carrier = (TextBox)FormView1.FindControl("vcarrierTextBox");
        TextBox pallet = (TextBox)FormView1.FindControl("vpalletTextBox");
        RadioButtonList shipmeth = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
        if (shipmeth.SelectedIndex == 0)
        {
            HiddenField3.Value = "yes";
        }
        if (shipmeth.SelectedIndex == 1)
        {
            HiddenField3.Value = "no";
        }
        TextBox delchg = (TextBox)FormView1.FindControl("vdelchgTextBox");
        TextBox deltime = (TextBox)FormView1.FindControl("vdeltimeTextBox");
        TextBox destcode = (TextBox)FormView1.FindControl("vdestcodeTextBox");
        TextBox notes1 = (TextBox)FormView1.FindControl("vnotes1TextBox");

        TextBox notes2 = (TextBox)FormView1.FindControl("vnotes2TextBox");
        TextBox notes3 = (TextBox)FormView1.FindControl("vnotes3TextBox");
        TextBox notes4 = (TextBox)FormView1.FindControl("vnotes4TextBox");

        TextBox faxAreaCode = (TextBox)FormView1.FindControl("vfaxAreaCodeTextBox");
        TextBox faxNumber = (TextBox)FormView1.FindControl("vFaxNumberTextBox");
        TextBox fi_jdedid = (TextBox)FormView1.FindControl("vfi_jdedidTextBox");
        TextBox tb_mandatorytax = (TextBox)FormView1.FindControl("vtb_mandatorytaxTextBox");
        TextBox loc = (TextBox)FormView1.FindControl("vlocTextBox");




        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmCustomer"].DefaultValue = Convert.ToString(Session["customer1_list_cust"]);


        ObjectDataSource1.SelectParameters["prmshipname"].DefaultValue = shipname.Text.Trim();
        ObjectDataSource1.SelectParameters["prmshipcity"].DefaultValue = shipcity.Text.Trim();
        ObjectDataSource1.SelectParameters["prmshipstate"].DefaultValue = shipstate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmshipzip"].DefaultValue = shipzip.Text.Trim();
        ObjectDataSource1.SelectParameters["prmshipaddr1"].DefaultValue = shipaddr1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmshipaddr2"].DefaultValue = shipaddr2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcontact"].DefaultValue = contact.Text.Trim();
        ObjectDataSource1.SelectParameters["prmareacode"].DefaultValue = areacode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmphone"].DefaultValue = phone.Text.Trim();

        ObjectDataSource1.SelectParameters["prmtaxcode"].DefaultValue = taxcode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbroker"].DefaultValue = HiddenField1.Value.Trim();
        ObjectDataSource1.SelectParameters["prmbill"].DefaultValue = HiddenField2.Value.Trim();
        ObjectDataSource1.SelectParameters["prmdockloc"].DefaultValue = dockloc.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdockhour"].DefaultValue = dockhour.Text.Trim();
        ObjectDataSource1.SelectParameters["prmlocbin"].DefaultValue = locbin.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcarrier"].DefaultValue = carrier.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpallet"].DefaultValue = pallet.Text.Trim();

        ObjectDataSource1.SelectParameters["prmshipmeth"].DefaultValue = HiddenField3.Value.Trim();

        ObjectDataSource1.SelectParameters["prmdelchg"].DefaultValue = delchg.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdeltime"].DefaultValue = deltime.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdestcode"].DefaultValue = destcode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmnotes1"].DefaultValue = notes1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmnotes2"].DefaultValue = notes2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmnotes3"].DefaultValue = notes3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmnotes4"].DefaultValue = notes4.Text.Trim();

        ObjectDataSource1.SelectParameters["prmfaxAreaCode"].DefaultValue = faxAreaCode.Text.Trim();

        ObjectDataSource1.SelectParameters["prmfaxNumber"].DefaultValue = faxNumber.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfi_jdedid"].DefaultValue = fi_jdedid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmtb_mandatorytax"].DefaultValue = tb_mandatorytax.Text.Trim();
        ObjectDataSource1.SelectParameters["prmloc"].DefaultValue = loc.Text.Trim();



    }
    protected void lnk_Listcustomers_Click(object Sender, EventArgs e)
    {
        Response.Redirect("customer_list.aspx");
    }
    protected void lnk_listship_Click(object sender, EventArgs e)
    {
        Response.Redirect("cust_list_ship.aspx");
    }
    protected void lnk_viewcustomers_Click(object Sender, EventArgs e)
    {
        Response.Redirect("cust_viewlist.aspx");
    }
    protected void lnk_viewship_Click(object Sender, EventArgs e)
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

    protected void Deletebutton_Click(object sender, EventArgs e)
    {

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";

    }


}

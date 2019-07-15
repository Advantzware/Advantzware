
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class cust_view_sold : System.Web.UI.Page
{


    protected void Page_Load(object sender, System.EventArgs e)
    {
        
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        Label cust1 = (Label)Form.FindControl("Customer");
        cust1.Text = Convert.ToString(Session["customer1_list_cust"]);
        Label cust1name = (Label)Form.FindControl("CustName");
        cust1name.Text = Convert.ToString(Session["customer1_list_cust_name"]);
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "cust_view_sold.aspx";
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
            if (Session["customer_list_soldto"] == null)
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
            TextBox code = (TextBox)FormView1.FindControl("vsoldidTextBox");
            code.Focus();

        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox code = (TextBox)FormView1.FindControl("vsoldnameTextBox");
            code.Focus();
            //TextBox textBox1 = (TextBox)FormView1.FindControl("vsoldcityTextBox");
            //this.Controls.Add(textBox1);
            //textBox1.Click += new EventHandler(textBox1_Click);         
            // protected  void textBox1_Click(object sender, EventArgs e)
            // {
            //        Response.Write("hello city");
            //    }


        }
    }


    protected void AddButton_Click(object sender, EventArgs e)
    {
        Session["customer_list_soldto"] = null;
        TextBox soldid = (TextBox)FormView1.FindControl("vsoldidTextBox");
        TextBox soldname = (TextBox)FormView1.FindControl("vsoldnameTextBox");
        TextBox soldaddr1 = (TextBox)FormView1.FindControl("vsoldaddr1TextBox");
        TextBox soldaddr2 = (TextBox)FormView1.FindControl("vsoldaddr2TextBox");
        TextBox soldcity = (TextBox)FormView1.FindControl("vsoldcityTextBox");
        TextBox soldstate = (TextBox)FormView1.FindControl("vsoldstateTextBox");
        TextBox soldzip = (TextBox)FormView1.FindControl("vsoldzipTextBox");



        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";

        ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_ShipCompany"]);
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        ObjectDataSource1.SelectParameters["prmCustomer"].DefaultValue = Convert.ToString(Session["customer1_list_cust"]);
        ObjectDataSource1.SelectParameters["prmsoldid"].DefaultValue = soldid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmsoldname"].DefaultValue = soldname.Text.Trim();
        ObjectDataSource1.SelectParameters["prmsoldaddr1"].DefaultValue = soldaddr1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmsoldaddr2"].DefaultValue = soldaddr2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmsoldcity"].DefaultValue = soldcity.Text.Trim();
        ObjectDataSource1.SelectParameters["prmsoldstate"].DefaultValue = soldstate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmsoldzip"].DefaultValue = soldzip.Text.Trim();
    }


    protected void UpdateButton_Click(object sender, EventArgs e)
    {

        TextBox soldname = (TextBox)FormView1.FindControl("vsoldnameTextBox");
        TextBox soldaddr1 = (TextBox)FormView1.FindControl("vsoldaddr1TextBox");
        TextBox soldaddr2 = (TextBox)FormView1.FindControl("vsoldaddr2TextBox");
        TextBox soldcity = (TextBox)FormView1.FindControl("vsoldcityTextBox");
        TextBox soldstate = (TextBox)FormView1.FindControl("vsoldstateTextBox");
        TextBox soldzip = (TextBox)FormView1.FindControl("vsoldzipTextBox");
        
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmCustomer"].DefaultValue = Convert.ToString(Session["customer1_list_cust"]);
        ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_ShipCompany"]);

        ObjectDataSource1.SelectParameters["prmsoldname"].DefaultValue = soldname.Text.Trim();
        ObjectDataSource1.SelectParameters["prmsoldaddr1"].DefaultValue = soldaddr1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmsoldaddr2"].DefaultValue = soldaddr2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmsoldcity"].DefaultValue = soldcity.Text.Trim();
        ObjectDataSource1.SelectParameters["prmsoldstate"].DefaultValue = soldstate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmsoldzip"].DefaultValue = soldzip.Text.Trim();
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

    protected void DeleteButton_Clock(object sender, EventArgs e)
    {

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
    }


}

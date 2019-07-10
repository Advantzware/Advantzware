
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class view_cust_plant : System.Web.UI.Page
{



    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmActPlant"].DefaultValue = "view";
        


        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_cust_plant.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            lblComp.Text = PrmComp;
            //Session["Customers_Company"] = labelcompany.Text;
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
            lblUser.Text = UserLogin.UserName;
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

    protected void LinkButton1_Click(object sender, EventArgs e)
    {

        Response.Redirect("menu.aspx");
    }
    protected void list_cust_click(object sender, EventArgs e)
    {
        Response.Redirect("cust_plant_list.aspx");
    }
    protected void view_cust_click(object sender, EventArgs e)
    {
        Response.Redirect("view_cust_plant.aspx");
    }

    protected void Insert_Button_Click(object sender, EventArgs e)
    {
        TextBox customer = (TextBox)FormView1.FindControl("vCustTextBox");        
        TextBox shipid = (TextBox)FormView1.FindControl("vShipidTextBox");
        TextBox vendor = (TextBox)FormView1.FindControl("vVendorCodeLabel");
        TextBox plantid = (TextBox)FormView1.FindControl("vPlantidTextBox");
        TextBox deptcode = (TextBox)FormView1.FindControl("vVenderDeptCodeTextBox");
        TextBox plantname = (TextBox)FormView1.FindControl("vPlantNameTextBox");
        

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmActPlant"].DefaultValue = "AddCustPlant";
        ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = customer.Text.Trim();
        ObjectDataSource1.SelectParameters["prmShipid"].DefaultValue = shipid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmVanderCode"].DefaultValue = vendor.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPlantid"].DefaultValue = plantid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDeptCode"].DefaultValue = deptcode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPlantName"].DefaultValue = plantname.Text.Trim();
        ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = Convert.ToString(Session["view_cust_plant_cust_reckey"]);
        FormView1.ChangeMode(FormViewMode.ReadOnly);
       
    }
    protected void Update_Button_Click(object sender, EventArgs e)
    {
        TextBox customer = (TextBox)FormView1.FindControl("vCustTextBox");
        TextBox shipid = (TextBox)FormView1.FindControl("vShipidTextBox");
        TextBox vendor = (TextBox)FormView1.FindControl("vVendorCodeLabel");
        TextBox plantid = (TextBox)FormView1.FindControl("vPlantidTextBox");
        TextBox deptcode = (TextBox)FormView1.FindControl("vVenderDeptCodeTextBox");
        TextBox plantname = (TextBox)FormView1.FindControl("vPlantNameTextBox");

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmActPlant"].DefaultValue = "UpdateCustPlant";

        ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = customer.Text.Trim();
        ObjectDataSource1.SelectParameters["prmShipid"].DefaultValue = shipid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmVanderCode"].DefaultValue = vendor.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPlantid"].DefaultValue = plantid.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDeptCode"].DefaultValue = deptcode.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPlantName"].DefaultValue = plantname.Text.Trim();
        ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = Convert.ToString(Session["view_cust_plant_cust_reckey"]);
        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }
    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmActPlant"].DefaultValue = "DeleteCustPlant";
        ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = Convert.ToString(Session["view_cust_plant_cust_reckey"]);
        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox arcode = (TextBox)FormView1.FindControl("vCustTextBox");
            arcode.Focus();
        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox arcode = (TextBox)FormView1.FindControl("vCustTextBox");
            arcode.Focus();
        }
    }
    protected void FormView1_Unload(object sender, EventArgs e)
    {
        try
        {
            Label reckey = (Label)FormView1.FindControl("vReckeyLabel");
            Session["view_cust_plant_cust_reckey"] = reckey.Text;
           
        }
        catch { }
    }
    protected void customer_txt_Click(object sender, EventArgs e)
    {

        try
        {
            TextBox customer = (TextBox)FormView1.FindControl("vCustTextBox");
            TextBox shipid = (TextBox)FormView1.FindControl("vShipidTextBox");
            TextBox vendor = (TextBox)FormView1.FindControl("vVendorCodeLabel");
            TextBox plantid = (TextBox)FormView1.FindControl("vPlantidTextBox");
            TextBox deptcode = (TextBox)FormView1.FindControl("vVenderDeptCodeTextBox");
            TextBox plantname = (TextBox)FormView1.FindControl("vPlantNameTextBox");

            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];

            LookUp look = new LookUp();
            DataSet ds = new DataSet();
            ds = look.Cross_Ref_Lookup("search", UserLogin.UserName, "custno", "EQUAL", customer.Text);

            customer.Text = ds.Tables[0].Rows[0][0].ToString();
            vendor.Text = ds.Tables[0].Rows[0][1].ToString();
            shipid.Focus();

        }
        catch
        {

            
        }
    }
}

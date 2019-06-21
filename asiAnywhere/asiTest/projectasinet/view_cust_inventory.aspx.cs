
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class view_cust_inventory : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmActCitem"].DefaultValue = "view";
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_cust_inventory.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            //labelcompany.Text = PrmComp;
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
        Response.Redirect("customer_inventory.aspx");
    }
    protected void view_cust_click(object sender, EventArgs e)
    {
        Response.Redirect("view_cust_inventory.aspx");
    }
    protected void save_click(object sender, EventArgs e)
    {               
        TextBox cust = (TextBox)FormView1.FindControl("vCustnoTextBox");
        Session["index_view_cust_inv_cust"] = cust.Text;
        TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
        Session["index_view_cust_inv_loc"] = loc.Text;
        TextBox qty = (TextBox)FormView1.FindControl("vQtyTextBox");
        TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
        Session["index_view_cust_inv_item"] = item.Text;
        TextBox cons = (TextBox)FormView1.FindControl("vConsumTextBox");
        //Response.Write(loc.Text);
        //Response.Write(Session["index_view_cust_inv"]);
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmActCitem"].DefaultValue = "AddCustItem";
        ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = Convert.ToString(Session["index_view_cust_inv_cust"]);
        ObjectDataSource1.SelectParameters["prmLoc"].DefaultValue = Convert.ToString(Session["index_view_cust_inv_loc"]);
        ObjectDataSource1.SelectParameters["prmItem"].DefaultValue = Convert.ToString(Session["index_view_cust_inv_item"]);
        ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();
        ObjectDataSource1.SelectParameters["prmConsum"].DefaultValue = cons.Text.Trim();
        
    }
    protected void update_click(object sender, EventArgs e)
    {
        TextBox cust = (TextBox)FormView1.FindControl("vCustnoTextBox");
        Session["index_view_cust_inv_cust"] = cust.Text;
        TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
        Session["index_view_cust_inv_loc"] = loc.Text;
        TextBox qty = (TextBox)FormView1.FindControl("vQtyTextBox");
        TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
        Session["index_view_cust_inv_item"] = item.Text;
        TextBox cons = (TextBox)FormView1.FindControl("vConsumTextBox");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmActCitem"].DefaultValue = "UpdateCustItem";
        ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = Convert.ToString(Session["index_view_cust_inv_cust"]);
        ObjectDataSource1.SelectParameters["prmLoc"].DefaultValue = Convert.ToString(Session["index_view_cust_inv_loc"]);
        ObjectDataSource1.SelectParameters["prmItem"].DefaultValue = Convert.ToString(Session["index_view_cust_inv_item"]);
        ObjectDataSource1.SelectParameters["prmQty"].DefaultValue = qty.Text.Trim();
        ObjectDataSource1.SelectParameters["prmConsum"].DefaultValue = cons.Text.Trim();

        ObjectDataSource1.SelectParameters["prmUpdateCust"].DefaultValue = Convert.ToString(Session["index_view_cust_inv_cust_update"]);
        ObjectDataSource1.SelectParameters["prmUpdateloc"].DefaultValue = Convert.ToString(Session["index_view_cust_inv_loc_update"]);
        ObjectDataSource1.SelectParameters["prmUpdateItem"].DefaultValue = Convert.ToString(Session["index_view_cust_inv_item_update"]);
    }
    protected void delete_Click(object sender, EventArgs e)
    {
        Label reckey = (Label)FormView1.FindControl("reckeyLabel");
        Label cust = (Label)FormView1.FindControl("vCustnoLabel");
        Label loc = (Label)FormView1.FindControl("vLocLabel");
        Label item = (Label)FormView1.FindControl("vItemLabel");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmActCitem"].DefaultValue = "deletecustitem";
        ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = cust.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLoc"].DefaultValue = loc.Text.Trim();
        ObjectDataSource1.SelectParameters["prmItem"].DefaultValue = item.Text.Trim();
        ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

    }
    protected void FormView1_DataBound(object sender, EventArgs e)
    {

        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox code = (TextBox)FormView1.FindControl("vCustnoTextBox");
            code.Focus();

        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox code = (TextBox)FormView1.FindControl("vCustNoTextBox");
            code.Focus();

        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {

            }
            catch { }
        }
    }

    protected void formview_prerender_click(object sender, EventArgs e)
    {
        try
        {
            Label cust = (Label)FormView1.FindControl("vCustnoLabel");
            Label loc = (Label)FormView1.FindControl("vLocLabel");
            Label item = (Label)FormView1.FindControl("vItemLabel");

            Session["index_view_cust_inv_cust_update"] = cust.Text.Trim();
            Session["index_view_cust_inv_loc_update"] = loc.Text.Trim();
            Session["index_view_cust_inv_item_update"] = item.Text.Trim();
        }
        catch { }
    }
}

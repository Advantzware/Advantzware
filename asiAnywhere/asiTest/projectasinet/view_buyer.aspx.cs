
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class view_buyer : System.Web.UI.Page
{
    
    protected void Page_Load(object sender, System.EventArgs e)
    {        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "buyer_list.aspx";
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

           
          
        }
        if (Convert.ToString(Session["buyer_list_new_buyer"]) == "addnewbuyer")
        {
            FormView1.ChangeMode(FormViewMode.Insert);
            Session["buyer_list_new_buyer"] = null;
        }
        
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            TextBox type = (TextBox)FormView1.FindControl("vbuyerTextBox");
            
            type.Focus();
            
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox dscr = (TextBox)FormView1.FindControl("vdscrTextBox");

            dscr.Focus();           


        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {

            try
            {

                Label reckey = (Label)FormView1.FindControl("reckeyLabel");
                Session["buyer_list_reckey_buyer"] = reckey.Text.Trim();
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
        Response.Redirect("buyer_list.aspx");
    }
    protected void lnk_viewvend_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_buyer.aspx");
    }

    protected void addButton_Click(object sender, EventArgs e)
    {

        TextBox buyer = (TextBox)FormView1.FindControl("vbuyerTextBox");
        TextBox dscr = (TextBox)FormView1.FindControl("vdscrTextBox");
               


        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        ObjectDataSource1.SelectParameters["prmvbuyer"].DefaultValue = buyer.Text.Trim();
        ObjectDataSource1.SelectParameters["prmvnum"].DefaultValue = dscr.Text.Trim();
        
        FormView1.ChangeMode(FormViewMode.ReadOnly);

    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {

        Label buyer = (Label)FormView1.FindControl("vbuyerLabel");
        TextBox dscr = (TextBox)FormView1.FindControl("vdscrTextBox");

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        ObjectDataSource1.SelectParameters["prmvbuyer"].DefaultValue = buyer.Text.Trim();
        ObjectDataSource1.SelectParameters["prmvnum"].DefaultValue = dscr.Text.Trim();

        FormView1.ChangeMode(FormViewMode.ReadOnly);
                     
    }


    protected void Deletebutton_Click(object sender, EventArgs e)
    {

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        //Response.Write("<script>window.location.href = 'customer_list.aspx';</script>");
    }
    
    
    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
    }
    protected void Formview_Unload(object sender, EventArgs e)
    {

    }
    



}

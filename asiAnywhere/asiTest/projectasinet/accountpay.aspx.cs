
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
#endregion

public partial class accountpay : System.Web.UI.Page
{
        
    protected void Page_Load(object sender, System.EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "accountpay.aspx";
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
       

    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox payable = (TextBox)FormView1.FindControl("payableTextBox");

            payable.Focus();


        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {

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




    

    protected void UpdateButton_Click(object sender, EventArgs e)
    {

        TextBox payable = (TextBox)FormView1.FindControl("payableTextBox");
        TextBox purc = (TextBox)FormView1.FindControl("purchTextBox");
        TextBox casea = (TextBox)FormView1.FindControl("cashactTextBox");
        TextBox disc = (TextBox)FormView1.FindControl("discTextBox");

        TextBox stax = (TextBox)FormView1.FindControl("staxTextBox");
        TextBox fright = (TextBox)FormView1.FindControl("freightTextBox");


        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = Convert.ToString(Session["customer_user_id"]);
        ObjectDataSource1.SelectParameters["prmpayable"].DefaultValue = payable.Text.Trim();
        ObjectDataSource1.SelectParameters["prmpurch"].DefaultValue = purc.Text.Trim();
        ObjectDataSource1.SelectParameters["prmcashact"].DefaultValue = casea.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdisc"].DefaultValue = disc.Text.Trim();
        ObjectDataSource1.SelectParameters["prmstax"].DefaultValue = stax.Text.Trim();
        ObjectDataSource1.SelectParameters["prmfreight"].DefaultValue = fright.Text.Trim();

        FormView1.ChangeMode(FormViewMode.ReadOnly);

    }



   
    protected void Formview_Unload(object sender, EventArgs e)
    {

    }




}
